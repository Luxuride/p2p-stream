use crate::Protocol;
use network::P2PSwarm;
use renderer::GstRenderer;
use std::sync::Arc;
use tokio::sync::Mutex;

pub struct Render {}

impl Render {
    pub async fn new(render: GstRenderer, swarm: Arc<Mutex<dyn P2PSwarm>>) -> Self {
        let p2p_swarm_clone = swarm.clone();
        tokio::spawn(async move {
            // Get a clone of the inbound receiver once
            let rx = {
                let guard = p2p_swarm_clone.lock().await;
                guard.inbound_rx().clone()
            };
            let renderer = render;
            while let Ok(stream_chunk) = rx.recv().await {
                renderer.push_ts_chunk(&stream_chunk.chunk).ok();
            }
        });
        Self {}
    }
}
