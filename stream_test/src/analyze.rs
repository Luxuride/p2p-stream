use crate::Protocol;
use network::P2PSwarm;
use serde::Serialize;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};
use tokio::sync::Mutex;
use tokio::task::JoinHandle;

#[derive(Serialize)]
pub struct AnalysisItem {
    creation_time: u128,
    arrival_time: u128,
    index: u64,
    size: u32,
}

pub struct Analyze {
    pub result: Arc<Mutex<Vec<AnalysisItem>>>,
    pub analysis_join_handle: JoinHandle<()>,
}

impl Analyze {
    pub fn new(swarm: Arc<Mutex<dyn P2PSwarm>>) -> Self {
        let p2p_swarm_clone = swarm.clone();
        let result = Arc::new(Mutex::new(Vec::new()));
        let result_clone = Arc::clone(&result);
        let analysis_join_handle = tokio::spawn(async move {
            let rx = {
                let guard = p2p_swarm_clone.lock().await;
                guard.inbound_rx().clone()
            };
            loop {
                tokio::select! {
                        _ = tokio::time::sleep(std::time::Duration::from_secs(5)) => {
                            break;
                        },
                        stream_chunk = rx.recv() => {
                            if let Ok(stream_chunk) = stream_chunk {
                                let creation_time = stream_chunk.timestamp.duration_since(UNIX_EPOCH).unwrap().as_millis();
                                let arrival_time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
                                let index = stream_chunk.index;
                                result_clone.lock().await.push(AnalysisItem {
                                    creation_time,
                                    arrival_time,
                                    index,
                                    size: stream_chunk.chunk.len() as u32 + 16 + 8,
                                });
                            } else {
                                break;
                            }
                    }
                }
            }
        });
        Self {
            result,
            analysis_join_handle,
        }
    }
}
