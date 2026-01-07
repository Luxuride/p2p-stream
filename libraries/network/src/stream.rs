use crate::P2PSwarm;
use crate::protocol::VideoStreamChunk;
use anyhow::Result;
use async_channel::{Receiver, Sender};
use async_trait::async_trait;
use futures::{AsyncReadExt, AsyncWriteExt, StreamExt};
use libp2p::mdns;
use libp2p::swarm::{NetworkBehaviour, SwarmEvent};
use libp2p::{Multiaddr, PeerId, StreamProtocol, identity};
use libp2p_stream as stream;
use log::{error, info, warn};
use serde_cbor;
use std::collections::HashSet;
use std::sync::{Arc, RwLock};

const STREAM_PROTOCOL: StreamProtocol = StreamProtocol::new("/stream");

/// Stream-based P2P networking over QUIC + mDNS.
pub struct P2PStreamSwarm {
    inbound_rx: Receiver<VideoStreamChunk>,
    outbound_tx: Sender<VideoStreamChunk>,
}

#[derive(NetworkBehaviour)]
pub struct StreamBehaviour {
    mdns: mdns::tokio::Behaviour,
    stream: stream::Behaviour,
}

impl P2PStreamSwarm {
    pub async fn run(_topic: &str) -> Result<Self> {
        // Identity
        let local_key = identity::Keypair::generate_ed25519();
        let local_peer_id = PeerId::from(local_key.public());
        info!("Local peer id: {local_peer_id}");

        // Behaviour
        let behaviour = StreamBehaviour {
            mdns: mdns::tokio::Behaviour::new(mdns::Config::default(), local_peer_id)?,
            stream: stream::Behaviour::new(),
        };

        // Control for libp2p-stream
        let control = behaviour.stream.new_control();
        let mut control_accept = control.clone();
        let mut control_for_send = control.clone();

        // Build Swarm (Tokio + QUIC)
        let mut swarm = libp2p::SwarmBuilder::with_existing_identity(local_key)
            .with_tokio()
            .with_quic()
            .with_behaviour(|_| Ok(behaviour))?
            .build();

        // Listen on all interfaces, random UDP port via QUIC v1
        let addr: Multiaddr = "/ip4/0.0.0.0/udp/0/quic-v1"
            .parse()
            .expect("valid listen addr");
        swarm.listen_on(addr)?;

        // Channels for inbound/outbound chunks (bounded; capacity from ENV)
        let (inbound_tx, inbound_rx) = async_channel::unbounded::<VideoStreamChunk>();
        let (outbound_tx, outbound_rx) = async_channel::unbounded::<VideoStreamChunk>();

        // Shared set of connected peers for broadcasting
        let peers: Arc<RwLock<HashSet<PeerId>>> = Arc::new(RwLock::new(HashSet::new()));

        // Task: Accept incoming streams and forward payloads to inbound channel
        {
            let inbound_tx = inbound_tx.clone();
            tokio::spawn(async move {
                match control_accept.accept(STREAM_PROTOCOL) {
                    Ok(mut incoming) => {
                        while let Some((_peer, mut st)) = incoming.next().await {
                            let mut data = Vec::new();
                            if let Err(e) = st.read_to_end(&mut data).await {
                                warn!("Failed reading incoming stream: {e:?}");
                                continue;
                            }
                            let inbound_tx = inbound_tx.clone();
                            tokio::spawn(async move {
                                match serde_cbor::from_slice::<VideoStreamChunk>(&data) {
                                    Ok(stream_chunk) => {
                                        let _ = inbound_tx.send(stream_chunk).await;
                                    }
                                    Err(e) => {
                                        warn!("Failed to decode inbound chunk: {e:?}");
                                    }
                                }
                            });
                        }
                    }
                    Err(e) => {
                        error!("Failed to accept incoming streams: {e:?}");
                    }
                }
            });
        }

        // Task: Drive the Swarm and maintain peer set
        {
            let peers_inner = peers.clone();
            tokio::spawn(async move {
                let mut swarm = swarm;
                loop {
                    match swarm.next().await {
                        Some(SwarmEvent::Behaviour(StreamBehaviourEvent::Mdns(
                            mdns::Event::Discovered(list),
                        ))) => {
                            for (pid, _addr) in list {
                                peers_inner.write().unwrap().insert(pid);
                            }
                        }
                        Some(SwarmEvent::Behaviour(StreamBehaviourEvent::Mdns(
                            mdns::Event::Expired(list),
                        ))) => {
                            for (pid, _addr) in list {
                                peers_inner.write().unwrap().remove(&pid);
                            }
                        }
                        Some(SwarmEvent::ConnectionEstablished { peer_id, .. }) => {
                            info!("Connection established with {peer_id}");
                            peers_inner.write().unwrap().insert(peer_id);
                        }
                        Some(SwarmEvent::ConnectionClosed { peer_id, .. }) => {
                            info!("Connection closed with {peer_id}");
                            peers_inner.write().unwrap().remove(&peer_id);
                        }
                        Some(_other) => {
                            // Ignore other events for now
                        }
                        None => break,
                    }
                }
            });
        }

        // Task: Broadcast outbound chunks to all known peers
        {
            let peers_for_send = peers.clone();
            tokio::spawn(async move {
                while let Ok(chunk) = outbound_rx.recv().await {
                    let targets: Vec<PeerId> =
                        { peers_for_send.read().unwrap().iter().cloned().collect() };

                    // Serialize once per broadcast
                    let payload = match serde_cbor::to_vec(&chunk) {
                        Ok(b) => b,
                        Err(e) => {
                            warn!("Failed to encode outbound chunk: {e:?}");
                            continue;
                        }
                    };

                    for pid in targets {
                        log::trace!("Sending to {pid}");
                        match control_for_send.open_stream(pid, STREAM_PROTOCOL).await {
                            Ok(mut st) => {
                                if let Err(e) = st.write_all(&payload).await {
                                    warn!("Failed to write to stream: {e:?}");
                                }
                            }
                            Err(stream::OpenStreamError::UnsupportedProtocol(_)) => {
                                warn!("Unsupported protocol when opening stream");
                            }
                            Err(e) => {
                                warn!("Failed to open stream: {e:?}");
                            }
                        }
                    }
                }
            });
        }

        Ok(Self {
            inbound_rx,
            outbound_tx,
        })
    }
}

#[async_trait]
impl P2PSwarm for P2PStreamSwarm {
    fn inbound_rx(&self) -> &Receiver<VideoStreamChunk> {
        &self.inbound_rx
    }

    async fn send_message(&mut self, chunk: VideoStreamChunk) {
        let _ = self.outbound_tx.send(chunk).await;
    }
}
