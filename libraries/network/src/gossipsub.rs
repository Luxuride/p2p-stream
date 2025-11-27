use crate::P2PSwarm;
use crate::protocol::VideoStreamChunk;
use anyhow::Result;
use async_channel::{Receiver, Sender};
use async_trait::async_trait;
use futures::StreamExt;
use libp2p::gossipsub::{self, Event as GossipsubEvent, IdentTopic, MessageAuthenticity};
use libp2p::mdns;
use libp2p::swarm::{NetworkBehaviour, Swarm, SwarmEvent};
use libp2p::{Multiaddr, PeerId, identity};
use log::{info, warn};
use std::collections::HashSet;
use std::env;
use std::sync::{Arc, RwLock};

/// Resolve channel capacity from ENV.
/// Uses VIDEO_STREAM_CHUNKS if set; defaults to 128.
fn channel_capacity_from_env() -> usize {
    env::var("VIDEO_STREAM_CHUNKS")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(128)
}

/// Gossipsub-based P2P networking over QUIC + mDNS.
pub struct GossipP2P {
    inbound_rx: Receiver<VideoStreamChunk>,
    outbound_tx: Sender<VideoStreamChunk>,
}

#[derive(NetworkBehaviour)]
pub struct GossipBehaviour {
    mdns: mdns::tokio::Behaviour,
    gossipsub: gossipsub::Behaviour,
}

impl GossipP2P {
    /// Run a gossipsub swarm subscribed to the given topic.
    pub async fn run(topic: &str) -> Result<Self> {
        // Identity
        let local_key = identity::Keypair::generate_ed25519();
        let local_peer_id = PeerId::from(local_key.public());
        info!("Local peer id: {local_peer_id}");

        // Configure gossipsub
        let gossipsub_config = gossipsub::ConfigBuilder::default()
            .validation_mode(gossipsub::ValidationMode::Strict)
            .build()
            .expect("valid gossipsub config");

        let mut gs = gossipsub::Behaviour::new(
            MessageAuthenticity::Signed(local_key.clone()),
            gossipsub_config,
        )
        .unwrap();

        // Topic subscription
        let topic = IdentTopic::new(topic.to_string());
        gs.subscribe(&topic)?;

        // Behaviour (mDNS + Gossipsub)
        let behaviour = GossipBehaviour {
            mdns: mdns::tokio::Behaviour::new(mdns::Config::default(), local_peer_id)?,
            gossipsub: gs,
        };

        // Build Swarm (Tokio + QUIC)
        let mut swarm: Swarm<GossipBehaviour> =
            libp2p::SwarmBuilder::with_existing_identity(local_key)
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
        let cap = channel_capacity_from_env();
        let (inbound_tx, inbound_rx) = async_channel::bounded::<VideoStreamChunk>(cap);
        let (outbound_tx, outbound_rx) = async_channel::bounded::<VideoStreamChunk>(cap);

        // Shared set of connected peers for bookkeeping
        let peers: Arc<RwLock<HashSet<PeerId>>> = Arc::new(RwLock::new(HashSet::new()));

        // Drive the Swarm and handle outbound publishes in one task
        {
            let peers_inner = peers.clone();
            let topic_for_loop = topic.clone();

            tokio::spawn(async move {
                let mut swarm = swarm;

                loop {
                    tokio::select! {
                        // Drive swarm events
                        maybe_event = swarm.next() => {
                            match maybe_event {
                                Some(SwarmEvent::Behaviour(GossipBehaviourEvent::Gossipsub(
                                    GossipsubEvent::Message {
                                        propagation_source,
                                        message_id: _,
                                        message,
                                    },
                                ))) => {
                                    // Track who we heard from and forward payload
                                    peers_inner.write().unwrap().insert(propagation_source);
                                    match serde_cbor::from_slice::<VideoStreamChunk>(&message.data) {
                                        Ok(chunk) => {
                                            let _ = inbound_tx.send(chunk).await;
                                        }
                                        Err(e) => {
                                            warn!("Failed to decode inbound chunk: {e:?}");
                                        }
                                    }
                                }
                                Some(SwarmEvent::Behaviour(GossipBehaviourEvent::Mdns(
                                    mdns::Event::Discovered(list),
                                ))) => {
                                    for (peer, addr) in list {
                                        peers_inner.write().unwrap().insert(peer);
                                        // Attempt to connect to discovered peer
                                        if let Err(e) = swarm.dial(addr.clone()) {
                                            warn!("Failed to dial discovered peer {peer}: {e:?}");
                                        }
                                        // Help gossipsub form/maintain mesh
                                        swarm.behaviour_mut().gossipsub.add_explicit_peer(&peer);
                                    }
                                }
                                Some(SwarmEvent::Behaviour(GossipBehaviourEvent::Mdns(
                                    mdns::Event::Expired(list),
                                ))) => {
                                    for (peer, _addr) in list {
                                        peers_inner.write().unwrap().remove(&peer);
                                        swarm.behaviour_mut().gossipsub.remove_explicit_peer(&peer);
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
                                    // Ignore other events
                                }
                                None => break,
                            }
                        }

                        // Publish outbound messages
                        msg = outbound_rx.recv() => {
                            match msg {
                                Ok(chunk) => {
                                    match serde_cbor::to_vec(&chunk) {
                                        Ok(bytes) => {
                                            if let Err(e) = swarm.behaviour_mut().gossipsub.publish(
                                                topic_for_loop.clone(),
                                                bytes,
                                            ) {
                                                warn!("Failed to publish message: {e:?}");
                                            }
                                        }
                                        Err(e) => {
                                            warn!("Failed to encode outbound chunk: {e:?}");
                                        }
                                    }
                                }
                                Err(_closed) => {
                                    // Outbound channel closed; continue driving swarm
                                }
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
impl P2PSwarm for GossipP2P {
    fn inbound_rx(&self) -> &Receiver<VideoStreamChunk> {
        &self.inbound_rx
    }

    async fn send_message(&mut self, chunk: VideoStreamChunk) {
        let _ = self.outbound_tx.send(chunk).await;
    }
}
