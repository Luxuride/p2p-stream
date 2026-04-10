use anyhow::Result;
use log::{info, trace, warn};
use network::protocol::VideoStreamChunk;
use network::P2PSwarm;
use recorder::ScreenCapture;
use std::sync::Arc;
use std::time::SystemTime;
use tokio::sync::Mutex;

pub struct ScreenProducer<'a> {
    screen_capture: ScreenCapture<'a>,
}

impl<'a> ScreenProducer<'a> {
    pub async fn new(swarm: Arc<Mutex<dyn P2PSwarm>>, bitrate: u32, mtu: u32) -> Result<Self> {
        let (video_tx, mut video_rx) = tokio::sync::mpsc::unbounded_channel::<Vec<u8>>();

        tokio::spawn(async move {
            let mut index = 0;
            while let Some(chunk) = video_rx.recv().await {
                let vsc = VideoStreamChunk {
                    chunk,
                    timestamp: SystemTime::now(),
                    index,
                };
                index = index.checked_add(1).unwrap();
                swarm.lock().await.send_message(vsc).await;
            }
        });

        let mut screen_capture = ScreenCapture::new(bitrate, mtu);
        trace!("Starting screen capture...");
        screen_capture.start().await?;
        trace!("Setting up callback...");
        screen_capture.set_new_sample_callback(move |app_sink| {
            if let Ok(sample) = app_sink.pull_sample() {
                if let Some(map) = sample.buffer().and_then(|b| b.map_readable().ok()) {
                    trace!("Pushing {} bytes", map.len());
                    let _ = video_tx.send(map.to_vec());
                } else {
                    warn!("Failed to get buffer map");
                }
            }
            Ok(gstreamer::FlowSuccess::Ok)
        });

        Ok(Self { screen_capture })
    }

    pub async fn stop(&mut self) -> Result<()> {
        info!("Stopping screen capture");
        self.screen_capture.stop().await
    }
}
