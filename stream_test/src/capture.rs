use anyhow::Result;
use log::{error, trace};
use network::P2PSwarm;
use network::protocol::VideoStreamChunk;
use std::sync::Arc;
use std::time::SystemTime;
use tokio::sync::Mutex;
use video_reader::VideoReader;

pub struct Capture {
    pub video_reader: VideoReader,
}
impl Capture {
    pub async fn new(swarm: Arc<Mutex<dyn P2PSwarm>>) -> Result<Self> {
        let (video_tx, mut video_rx) = tokio::sync::mpsc::unbounded_channel::<Vec<u8>>();

        // Spawn a task to wrap raw buffers into VideoStreamChunk and forward to P2P
        {
            let p2p_swarm = swarm.clone();
            tokio::spawn(async move {
                let mut index = 0;
                while let Some(chunk) = video_rx.recv().await {
                    let vsc = VideoStreamChunk {
                        chunk,
                        timestamp: SystemTime::now(),
                        index,
                    };
                    index = index.checked_add(1).unwrap();
                    p2p_swarm.lock().await.send_message(vsc).await;
                }
            });
        }

        let mut video_reader = VideoReader::default();
        trace!("Starting screen capture...");
        video_reader.start().await?;
        trace!("Setting up callback...");
        video_reader.set_new_sample_callback(move |app_sink| {
            if let Ok(sample) = app_sink.pull_sample() {
                if let Some(map) = sample.buffer().and_then(|b| b.map_readable().ok()) {
                    trace!("Pushing {} bytes", map.len());
                    // Non-blocking send via channel
                    let _ = video_tx.send(map.to_vec());
                } else {
                    error!("Failed to get buffer map");
                }
            }
            Ok(gstreamer::FlowSuccess::Ok)
        });
        Ok(Self { video_reader })
    }
}
