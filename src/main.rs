use anyhow::Result;
use log::{error, trace};
use network::stream::P2PSwarm;
use recorder::ScreenCapture;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;

use network::protocol::VideoStreamChunk;

#[tokio::main]
async fn main() -> Result<()> {
    gstreamer::init()?;
    env_logger::init();

    let renderer = renderer::GstRenderer::new()?;
    let p2p_swarm = Arc::new(Mutex::new(P2PSwarm::run("test").await?));
    {
        let p2p_swarm_clone = p2p_swarm.clone();
        tokio::spawn(async move {
            // Get a clone of the inbound receiver once
            let rx = {
                let guard = p2p_swarm_clone.lock().await;
                guard.inbound_rx().clone()
            };
            while let Ok(stream_chunk) = rx.recv().await {
                renderer.push_ts_chunk(&stream_chunk.chunk).ok();
            }
        });
    }

    if std::env::var("STOP") == Ok("true".to_string()) {
        tokio::time::sleep(Duration::MAX).await;
    }

    // Create a channel for raw video buffers
    let (video_tx, mut video_rx) = tokio::sync::mpsc::unbounded_channel::<Vec<u8>>();

    // Spawn a task to wrap raw buffers into VideoStreamChunk and forward to P2P
    {
        let p2p_swarm = p2p_swarm.clone();
        tokio::spawn(async move {
            while let Some(chunk) = video_rx.recv().await {
                let vsc = VideoStreamChunk { chunk };
                p2p_swarm.lock().await.send_message(vsc).await;
            }
        });
    }

    let mut screen_capture = ScreenCapture::new();
    trace!("Starting screen capture...");
    screen_capture.start().await?;
    trace!("Setting up callback...");
    screen_capture.set_new_sample_callback(move |app_sink| {
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

    tokio::time::sleep(Duration::MAX).await;
    Ok(())
}
