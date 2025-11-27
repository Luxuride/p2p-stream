use crate::P2PSwarm;
use crate::protocol::VideoStreamChunk;
use anyhow::Result;
use async_channel::{Receiver, Sender};
use async_trait::async_trait;
use futures::{AsyncReadExt, AsyncWriteExt, StreamExt};
use libp2p::gossipsub::{
    self, Event as GossipsubEvent, IdentTopic, MessageAuthenticity, PublishError,
};
use libp2p::mdns;
use libp2p::swarm::{NetworkBehaviour, Swarm, SwarmEvent};
use libp2p::{Multiaddr, PeerId, StreamProtocol, identity};
use libp2p_stream as stream;
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use serde_cbor;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::atomic::AtomicU64;
use std::sync::{Arc, RwLock};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

const STREAM_PROTOCOL: StreamProtocol = StreamProtocol::new("/stream");

fn compute_sha256(data: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(data);
    let digest = hasher.finalize();
    let mut out = [0u8; 32];
    out.copy_from_slice(&digest[..]);
    out
}

#[derive(NetworkBehaviour)]
pub struct HybridBehaviour {
    mdns: mdns::tokio::Behaviour,
    gossipsub: gossipsub::Behaviour,
    stream: stream::Behaviour,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GossipVideoMeta {
    pub timestamp: u128,
    pub sha256: [u8; 32],
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct StreamChunkRequest {
    pub sha256: [u8; 32],
}

/// In-memory cache for chunks keyed by sha256.
pub struct VideoChunkCache {
    store: HashMap<[u8; 32], VideoStreamChunk>,
}

impl VideoChunkCache {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    /// Insert or replace the chunk for the given key.
    pub fn insert(&mut self, key: [u8; 32], chunk: VideoStreamChunk) {
        self.store.insert(key, chunk);
    }

    /// Get a cloned chunk by key.
    pub fn get_clone(&self, key: &[u8; 32]) -> Option<VideoStreamChunk> {
        self.store.get(key).cloned()
    }
}

#[derive(Clone, Debug)]
pub struct RemoteAnnouncement {
    pub from: PeerId,
    pub announcement: GossipVideoMeta,
}

/// Hybrid networking:
/// - Uses gossipsub to broadcast ChunkAnnouncement
/// - Uses libp2p-stream to request/serve VideoStreamChunk by sha256
pub struct HybridP2P {
    inbound_ann_rx: Receiver<RemoteAnnouncement>,
    announce_tx: Sender<(GossipVideoMeta, VideoStreamChunk)>,
    // Minimal inbound channel for chunks (not used by hybrid announce path, but required by P2PSwarm trait)
    chunk_inbound_rx: Receiver<VideoStreamChunk>,
    chunk_outbound_tx: Sender<VideoStreamChunk>,
    peers: Arc<RwLock<HashSet<PeerId>>>,
    recent_announcements: Arc<RwLock<VecDeque<RemoteAnnouncement>>>,
    control: stream::Control,
    topic: IdentTopic,
}

impl HybridP2P {
    pub async fn run(topic: &str) -> Result<Self> {
        // Identity
        let local_key = identity::Keypair::generate_ed25519();
        let local_peer_id = PeerId::from(local_key.public());
        info!("Local peer id: {local_peer_id}");

        // GossipSub
        let gs_config = gossipsub::ConfigBuilder::default()
            .validation_mode(gossipsub::ValidationMode::Strict)
            .build()
            .expect("valid gossipsub config");

        let mut gsb =
            gossipsub::Behaviour::new(MessageAuthenticity::Signed(local_key.clone()), gs_config)
                .unwrap();

        let topic = IdentTopic::new(topic.to_string());
        gsb.subscribe(&topic)?;

        // Behaviour (mDNS + GS + Stream)
        let mut behaviour = HybridBehaviour {
            mdns: mdns::tokio::Behaviour::new(mdns::Config::default(), local_peer_id)?,
            gossipsub: gsb,
            stream: stream::Behaviour::new(),
        };

        // Controls
        let control = behaviour.stream.new_control();
        let mut control_accept = control.clone();
        let request_control = control.clone();

        // Build swarm
        let mut swarm: Swarm<HybridBehaviour> =
            libp2p::SwarmBuilder::with_existing_identity(local_key)
                .with_tokio()
                .with_quic()
                .with_behaviour(|_| Ok(behaviour))?
                .build();

        // Listen on all interfaces
        let addr: Multiaddr = "/ip4/0.0.0.0/udp/0/quic-v1"
            .parse()
            .expect("valid listen addr");
        swarm.listen_on(addr)?;

        let (inbound_ann_tx, inbound_ann_rx) = async_channel::unbounded::<RemoteAnnouncement>();
        let (announce_tx, announce_rx) =
            async_channel::unbounded::<(GossipVideoMeta, VideoStreamChunk)>();
        // Simple chunk channel to satisfy trait
        let (chunk_outbound_tx, chunk_inbound_rx) = async_channel::unbounded::<VideoStreamChunk>();

        // Peers and chunk cache (sha256 -> chunk)
        let peers: Arc<RwLock<HashSet<PeerId>>> = Arc::new(RwLock::new(HashSet::new()));
        let cache: Arc<RwLock<VideoChunkCache>> = Arc::new(RwLock::new(VideoChunkCache::new()));
        // Recent announcements buffer (trimmed by time in the gossip loop)
        let recent: Arc<RwLock<VecDeque<RemoteAnnouncement>>> =
            Arc::new(RwLock::new(VecDeque::new()));
        // In-flight sha256s to avoid duplicate concurrent fetches
        let inflight: Arc<RwLock<HashSet<[u8; 32]>>> = Arc::new(RwLock::new(HashSet::new()));

        // Serve incoming stream requests
        {
            let cache_for_server = cache.clone();
            tokio::spawn(async move {
                match control_accept.accept(STREAM_PROTOCOL) {
                    Ok(mut incoming) => {
                        while let Some((peer, mut st)) = incoming.next().await {
                            debug!("Incoming stream request from peer {peer}");
                            let mut req_bytes = Vec::new();
                            if let Err(e) = st.read_to_end(&mut req_bytes).await {
                                warn!("Failed to read request from {peer}: {e:?}");
                                continue;
                            }
                            match serde_cbor::from_slice::<StreamChunkRequest>(&req_bytes) {
                                Ok(req) => {
                                    debug!(
                                        "Received chunk request from {peer} for sha256 {:?}",
                                        req.sha256
                                    );
                                    // Lookup chunk
                                    let maybe_chunk = {
                                        let cache = cache_for_server.read().unwrap();
                                        cache.get_clone(&req.sha256)
                                    };
                                    if let Some(chunk) = maybe_chunk {
                                        match serde_cbor::to_vec(&chunk) {
                                            Ok(resp_bytes) => {
                                                if let Err(e) = st.write_all(&resp_bytes).await {
                                                    warn!(
                                                        "Failed to write response to {peer}: {e:?}"
                                                    );
                                                } else {
                                                    debug!(
                                                        "Sent {} bytes of chunk response to {peer}",
                                                        resp_bytes.len()
                                                    );
                                                }
                                                let _ = st.close().await;
                                            }
                                            Err(e) => {
                                                warn!("Failed to encode chunk for {peer}: {e:?}");
                                                let _ = st.close().await;
                                            }
                                        }
                                    } else {
                                        debug!(
                                            "Requested chunk not found for {:?} from {peer}",
                                            req.sha256
                                        );
                                        let _ = st.close().await;
                                    }
                                }
                                Err(e) => {
                                    warn!("Failed to decode chunk request from {peer}: {e:?}");
                                    let _ = st.close().await;
                                }
                            }
                        }
                    }
                    Err(e) => {
                        warn!("Failed to accept incoming stream requests: {e:?}");
                    }
                }
            });
        }

        // Drive swarm and handle announcements
        {
            let peers_inner = peers.clone();
            let topic_for_loop = topic.clone();
            let cache_for_loop = cache.clone();
            let recent_for_loop = recent.clone();
            let inflight_for_loop = inflight.clone();
            let chunk_tx_for_loop = chunk_outbound_tx.clone();
            let control_for_loop = request_control.clone();
            let local_peer_id_for_loop = local_peer_id;

            tokio::spawn(async move {
                let mut swarm = swarm;

                loop {
                    tokio::select! {
                        maybe_event = swarm.next() => {
                            match maybe_event {
                                Some(SwarmEvent::Behaviour(HybridBehaviourEvent::Gossipsub(
                                    GossipsubEvent::Message { propagation_source, message_id: _, message }
                                ))) => {
                                    match serde_cbor::from_slice::<GossipVideoMeta>(&message.data) {
                                        Ok(announcement) => {
                                            debug!("Received announcement from {propagation_source} ts={} sha256={:?}", announcement.timestamp, announcement.sha256);
                                            // Track in recent announcements buffer (trim to ~60s)
                                            {
                                                let now_ms = UNIX_EPOCH.elapsed().unwrap().as_millis();
                                                let mut dq = recent_for_loop.write().unwrap();
                                                dq.push_back(RemoteAnnouncement {
                                                    from: propagation_source,
                                                    announcement: announcement.clone(),
                                                });
                                                while let Some(front) = dq.front() {
                                                    if now_ms.saturating_sub(front.announcement.timestamp) > 60_000 {
                                                        dq.pop_front();
                                                    } else {
                                                        break;
                                                    }
                                                }
                                            }

                                            // Forward to external consumers
                                            let _ = inbound_ann_tx.send(RemoteAnnouncement {
                                                from: propagation_source,
                                                announcement: announcement.clone(),
                                            }).await;

                                            // Automatically request recent chunks (≤ 10 seconds), if not cached or already in-flight, and not from self.
                                            let now_ms = UNIX_EPOCH.elapsed().unwrap().as_millis();
                                            let is_recent = now_ms.saturating_sub(announcement.timestamp) <= 10_000;
                                            if is_recent && propagation_source != local_peer_id_for_loop {
                                                let sha = announcement.sha256;
                                                // Skip if already cached
                                                let have_already = {
                                                    let cg = cache_for_loop.read().unwrap();
                                                    cg.get_clone(&sha).is_some()
                                                };
                                                if !have_already {
                                                    // Deduplicate concurrent requests
                                                    let should_request = {
                                                        let mut set = inflight_for_loop.write().unwrap();
                                                        if set.contains(&sha) {
                                                            false
                                                        } else {
                                                            set.insert(sha);
                                                            true
                                                        }
                                                    };
                                                    if should_request {
                                                        let mut control = control_for_loop.clone();
                                                        let chunk_tx = chunk_tx_for_loop.clone();
                                                        let cache_for_req = cache_for_loop.clone();
                                                        let inflight_done = inflight_for_loop.clone();
                                                        let target_peer = propagation_source;
                                                        tokio::spawn(async move {
                                                            debug!("Requesting chunk {:?} from peer {}", sha, target_peer);
                                                            match control.open_stream(target_peer, STREAM_PROTOCOL).await {
                                                                Ok(mut st) => {
                                                                    let req = StreamChunkRequest { sha256: sha };
                                                                    match serde_cbor::to_vec(&req) {
                                                                        Ok(bytes) => {
                                                                            if let Err(e) = st.write_all(&bytes).await {
                                                                                warn!("Failed to write request to {} for {:?}: {e:?}", target_peer, sha);
                                                                            } else {
                                                                                let _ = st.close().await;
                                                                                let mut resp = Vec::new();
                                                                                match st.read_to_end(&mut resp).await {
                                                                                    Ok(_) => {
                                                                                        match serde_cbor::from_slice::<VideoStreamChunk>(&resp) {
                                                                                            Ok(chunk) => {
                                                                                                // Cache and forward
                                                                                                {
                                                                                                    let mut cg = cache_for_req.write().unwrap();
                                                                                                    cg.insert(sha, chunk.clone());
                                                                                                }
                                                                                                let _ = chunk_tx.send(chunk).await;
                                                                                                debug!("Fetched and cached chunk {:?}", sha);
                                                                                            }
                                                                                            Err(e) => {
                                                                                                warn!("Failed to decode chunk response for {:?}: {e:?}", sha);
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                    Err(e) => {
                                                                                        warn!("Failed reading response from {} for {:?}: {e:?}", target_peer, sha);
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                        Err(e) => {
                                                                            warn!("Failed encoding request for {:?}: {e:?}", sha);
                                                                        }
                                                                    }
                                                                }
                                                                Err(e) => {
                                                                    warn!("Open stream failed to {} for {:?}: {e:?}", target_peer, sha);
                                                                }
                                                            }
                                                            // Clear in-flight
                                                            let mut set = inflight_done.write().unwrap();
                                                            set.remove(&sha);
                                                        });
                                                    } else {
                                                    }
                                                } else {
                                                }
                                            } else {
                                            }
                                        }
                                        Err(e) => {
                                            warn!("Failed to decode announcement: {e:?}");
                                        }
                                    }
                                }
                                Some(SwarmEvent::Behaviour(HybridBehaviourEvent::Mdns(
                                    mdns::Event::Discovered(list)
                                ))) => {
                                    for (peer, addr) in list {
                                        peers_inner.write().unwrap().insert(peer);
                                        if let Err(e) = swarm.dial(addr.clone()) {
                                            warn!("Dial error to discovered peer {peer}: {e:?}");
                                        }
                                        swarm.behaviour_mut().gossipsub.add_explicit_peer(&peer);
                                    }
                                }
                                Some(SwarmEvent::Behaviour(HybridBehaviourEvent::Mdns(
                                    mdns::Event::Expired(list)
                                ))) => {
                                    for (peer, _addr) in list {
                                        peers_inner.write().unwrap().remove(&peer);
                                        swarm.behaviour_mut().gossipsub.remove_explicit_peer(&peer);
                                    }
                                }
                                Some(SwarmEvent::ConnectionEstablished { peer_id, .. }) => {
                                    peers_inner.write().unwrap().insert(peer_id);
                                }
                                Some(SwarmEvent::ConnectionClosed { peer_id, .. }) => {
                                    peers_inner.write().unwrap().remove(&peer_id);
                                }
                                Some(_other) => {}
                                None => break,
                            }
                        }

                        // Outbound share: publish metadata and update cache
                        maybe_item = announce_rx.recv() => {
                            if let Ok((meta, chunk)) = maybe_item {
                                // Update cache with capacity keyed by sha256 from metadata
                                {
                                    let mut guard = cache_for_loop.write().unwrap();
                                    guard.insert(meta.sha256, chunk.clone());
                                }

                                // Publish metadata via gossipsub
                                match serde_cbor::to_vec(&meta) {
                                    Ok(bytes) => {
                                        if let Err(e) = swarm.behaviour_mut().gossipsub.publish(
                                            topic_for_loop.clone(),
                                            bytes,
                                        ) {
                                            warn!("Publish announcement failed: {e:?}");
                                        }
                                    }
                                    Err(e) => warn!("Encode announcement failed: {e:?}"),
                                }
                            }
                        }
                    }
                }
            });
        }

        Ok(Self {
            inbound_ann_rx,
            announce_tx,
            chunk_inbound_rx,
            chunk_outbound_tx,
            peers,
            recent_announcements: recent,
            control: request_control,
            topic,
        })
    }

    /// Announcements receiver (who announced what).
    pub fn inbound_announcements(&self) -> &Receiver<RemoteAnnouncement> {
        &self.inbound_ann_rx
    }

    /// Share a chunk: store locally and broadcast its metadata via gossipsub.
    pub async fn share(&self, meta: GossipVideoMeta, chunk: VideoStreamChunk) {
        let _ = self.announce_tx.send((meta, chunk)).await;
    }

    /// Request a chunk by sha256 from a specific peer over stream.
    pub async fn request(
        &mut self,
        peer: PeerId,
        sha256: [u8; 32],
    ) -> Result<Option<VideoStreamChunk>> {
        match self.control.open_stream(peer, STREAM_PROTOCOL).await {
            Ok(mut st) => {
                debug!("Opened stream to peer {peer} for sha256 {:?}", sha256);
                let req = StreamChunkRequest { sha256 };
                let bytes = serde_cbor::to_vec(&req)?;
                st.write_all(&bytes).await?;
                debug!("Sent {} bytes of request to {peer}", bytes.len());
                // Close our write side so the server read_to_end can complete.
                let _ = st.close().await;

                let mut resp = Vec::new();
                st.read_to_end(&mut resp).await?;
                debug!("Received {} bytes response from {peer}", resp.len());
                match serde_cbor::from_slice::<VideoStreamChunk>(&resp) {
                    Ok(chunk) => {
                        debug!("Decoded chunk from {peer} for sha256 {:?}", sha256);
                        Ok(Some(chunk))
                    }
                    Err(e) => {
                        warn!(
                            "Failed to decode chunk from response (len={}): {e:?}",
                            resp.len()
                        );
                        Ok(None)
                    }
                }
            }
            Err(e) => {
                warn!("Failed to open stream to peer {peer}: {e:?}");
                Ok(None)
            }
        }
    }

    pub fn topic(&self) -> IdentTopic {
        self.topic.clone()
    }

    pub fn peers(&self) -> Arc<RwLock<HashSet<PeerId>>> {
        self.peers.clone()
    }

    /// Return a snapshot of announcements whose timestamps are within max_age from now.
    pub fn recent_announcements_within(&self, max_age: Duration) -> Vec<RemoteAnnouncement> {
        let now_ms: u128 = UNIX_EPOCH.elapsed().unwrap().as_millis();
        let cutoff: u128 = max_age.as_millis();
        let guard = self.recent_announcements.read().unwrap();
        // Iterate newest-first and clone
        guard
            .iter()
            .rev()
            .filter(|ra| now_ms.saturating_sub(ra.announcement.timestamp) <= cutoff)
            .cloned()
            .collect()
    }

    /// Request all unique chunks announced within max_age.
    pub async fn request_recent_chunks_within(
        &mut self,
        max_age: Duration,
    ) -> Result<Vec<VideoStreamChunk>> {
        let anns = self.recent_announcements_within(max_age);
        let mut seen: HashSet<[u8; 32]> = HashSet::new();
        let mut out: Vec<VideoStreamChunk> = Vec::new();

        for ra in anns {
            if seen.insert(ra.announcement.sha256) {
                if let Ok(Some(chunk)) = self.request(ra.from, ra.announcement.sha256).await {
                    out.push(chunk);
                }
            }
        }

        Ok(out)
    }

    /// Convenience: request all unique chunks announced in the last 10 seconds.
    pub async fn request_recent_chunks_last_10s(&mut self) -> Result<Vec<VideoStreamChunk>> {
        self.request_recent_chunks_within(Duration::from_secs(10))
            .await
    }
}

#[async_trait]
impl P2PSwarm for HybridP2P {
    fn inbound_rx(&self) -> &Receiver<VideoStreamChunk> {
        &self.chunk_inbound_rx
    }

    async fn send_message(&mut self, chunk: VideoStreamChunk) {
        // Prepare metadata from environment/defaults
        let announce_tx = self.announce_tx.clone();
        let sha256 = compute_sha256(&chunk.chunk);
        let meta = GossipVideoMeta {
            timestamp: UNIX_EPOCH.elapsed().unwrap().as_millis(),
            sha256,
        };
        debug!(
            "Sharing/announcing chunk with sha256 {:?} at ts {}",
            sha256, meta.timestamp
        );
        // Broadcast meta via gossip loop and cache the chunk
        let _ = announce_tx.send((meta, chunk)).await;
    }
}
