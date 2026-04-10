mod analyze;
mod capture;
mod screen_producer;
mod render;

use crate::analyze::Analyze;
use crate::capture::Capture;
use crate::screen_producer::ScreenProducer;
use anyhow::Result;
use clap::{Parser, ValueEnum};
use log::{info, warn};
use std::env;
use std::fmt::Display;
use std::sync::Arc;
use std::time::Duration;
use tokio::process::Command;
use tokio::signal::ctrl_c;
use tokio::sync::Mutex;

#[derive(ValueEnum, Debug, Clone)]
enum Protocol {
    Gossipsub,
    Stream,
    Hybrid,
}

impl Display for Protocol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Protocol::Gossipsub => write!(f, "gossipsub"),
            Protocol::Stream => write!(f, "stream"),
            Protocol::Hybrid => write!(f, "hybrid"),
        }
    }
}

#[derive(ValueEnum, Debug, Clone)]
enum Role {
    Manager,
    Analyzer,
    Renderer,
    Producer,
    ScreenProducer,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long, default_value = "gossipsub")]
    protocol: Protocol,

    #[arg(short, long, default_value = "manager")]
    role: Role,

    #[arg(long, default_value_t = 500)]
    bitrate: u32,

    #[arg(long, default_value_t = 4096)]
    chunk_size: u32,

    #[arg(long)]
    output: Option<String>,

    #[arg(long)]
    video_path: Option<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    gstreamer::init()?;
    env_logger::init();
    let args = Args::parse();
    match args.role {
        Role::Manager => {
            let exe = match env::current_exe() {
                Ok(exe) => exe,
                Err(error) => {
                    panic!("Failed to get exe: {}", error);
                }
            };
            let output_path = args.output.clone().unwrap_or_else(|| {
                format!(
                    "analysis_{}_{}kbps_{}mtu.csv",
                    args.protocol, args.bitrate, args.chunk_size
                )
            });
            let mut analyzer_cmd = Command::new(&exe);
            analyzer_cmd
                .arg("--protocol")
                .arg(args.protocol.to_string())
                .arg("--role")
                .arg("analyzer")
                .arg("--bitrate")
                .arg(args.bitrate.to_string())
                .arg("--chunk-size")
                .arg(args.chunk_size.to_string())
                .arg("--output")
                .arg(output_path.clone());
            if let Some(video_path) = &args.video_path {
                analyzer_cmd.arg("--video-path").arg(video_path);
            }
            let mut client_child = analyzer_cmd.spawn()?;
            let client_pid = client_child.id().unwrap();
            let mut producer_cmd = Command::new(&exe);
            producer_cmd
                .arg("--protocol")
                .arg(args.protocol.to_string())
                .arg("--role")
                .arg("producer")
                .arg("--bitrate")
                .arg(args.bitrate.to_string())
                .arg("--chunk-size")
                .arg(args.chunk_size.to_string());
            if let Some(video_path) = &args.video_path {
                producer_cmd.arg("--video-path").arg(video_path);
            }
            let mut producer_child = producer_cmd.spawn()?;
            let producer_pid = producer_child.id().unwrap();
            tokio::select! {
                producer_status = producer_child.wait() => {
                    info!("Producer exited: {:?}. Stopping analyzer...", producer_status);
                    unsafe {
                        libc::kill(client_pid as i32, libc::SIGINT);
                    }
                    let _ = client_child.wait().await;
                }
                analyzer_status = client_child.wait() => {
                    info!("Analyzer exited: {:?}. Stopping producer...", analyzer_status);
                    unsafe {
                        libc::kill(producer_pid as i32, libc::SIGINT);
                    }
                    let _ = producer_child.wait().await;
                }
                _ = tokio::signal::ctrl_c() => {
                    info!("Manager received Ctrl+C, stopping producer and analyzer...");
                    unsafe {
                        libc::kill(client_pid as i32, libc::SIGINT);
                        libc::kill(producer_pid as i32, libc::SIGINT);
                    }
                    let _ = futures_util::future::join_all([
                        client_child.wait(),
                        producer_child.wait()
                    ]).await;
                }
            }
        }
        Role::Analyzer => {
            println!("Starting Analyzer");
            let analyze: Analyze = match args.protocol {
                Protocol::Gossipsub => Analyze::new(Arc::new(Mutex::new(
                    network::gossipsub::GossipP2P::run("gossipsub").await?,
                ))),
                Protocol::Hybrid => Analyze::new(Arc::new(Mutex::new(
                    network::hybrid::HybridP2P::run("hybrid").await?,
                ))),
                Protocol::Stream => Analyze::new(Arc::new(Mutex::new(
                    network::stream::P2PStreamSwarm::run("stream").await?,
                ))),
            };
            tokio::select! {
                _ = ctrl_c() => {}
                _ = analyze.analysis_join_handle => {}
            };
            let output_path = args.output.clone().unwrap_or_else(|| {
                format!(
                    "analysis_{}_{}kbps_{}mtu.csv",
                    args.protocol, args.bitrate, args.chunk_size
                )
            });
            let mut wtr = csv::WriterBuilder::new().from_path(output_path)?;
            let result = analyze.result.lock().await;
            for item in result.as_slice() {
                wtr.serialize(item)?;
            }
            wtr.flush()?;
            std::process::exit(0);
        }
        Role::Renderer => {
            println!("Starting Renderer");
            let renderer = renderer::GstRenderer::new()?;
            match args.protocol {
                Protocol::Gossipsub => render::Render::new(
                    renderer,
                    Arc::new(Mutex::new(
                        network::gossipsub::GossipP2P::run("gossipsub").await?,
                    )),
                ),
                Protocol::Hybrid => render::Render::new(
                    renderer,
                    Arc::new(Mutex::new(network::hybrid::HybridP2P::run("hybrid").await?)),
                ),
                Protocol::Stream => render::Render::new(
                    renderer,
                    Arc::new(Mutex::new(
                        network::stream::P2PStreamSwarm::run("stream").await?,
                    )),
                ),
            }
            .await;
            tokio::select! {
                _ = ctrl_c() => {}
                _ = tokio::time::sleep(std::time::Duration::MAX) => {}
            };
        }
        Role::Producer => {
            println!("Starting producer");
            let mut capture = match args.protocol {
                Protocol::Gossipsub => Capture::new(
                    Arc::new(Mutex::new(
                        network::gossipsub::GossipP2P::run("gossipsub").await?,
                    )),
                    args.bitrate,
                    args.chunk_size,
                    args.video_path.clone(),
                ),
                Protocol::Hybrid => Capture::new(
                    Arc::new(Mutex::new(network::hybrid::HybridP2P::run("hybrid").await?)),
                    args.bitrate,
                    args.chunk_size,
                    args.video_path.clone(),
                ),
                Protocol::Stream => Capture::new(
                    Arc::new(Mutex::new(
                        network::stream::P2PStreamSwarm::run("stream").await?,
                    )),
                    args.bitrate,
                    args.chunk_size,
                    args.video_path.clone(),
                ),
            }
            .await?;
            tokio::select! {
                _ = ctrl_c() => {
                    info!("Producer received Ctrl+C, starting graceful GStreamer shutdown");
                    if let Err(err) = capture.video_reader.shutdown_graceful(Duration::from_secs(2)).await {
                        warn!("Graceful shutdown failed, forcing stop: {:?}", err);
                        capture.video_reader.stop().await?;
                    }
                    info!("Producer shutdown complete");
                }
                eos_result = capture.video_reader.wait_for_eos() => {
                    match eos_result {
                        Ok(()) => info!("Producer reached EOS, stopping pipeline"),
                        Err(err) => warn!("Producer EOS wait returned error, forcing stop: {:?}", err),
                    }
                    capture.video_reader.stop().await?;
                }
            }
            std::process::exit(0);
        }
        Role::ScreenProducer => {
            println!("Starting screen producer");
            let mut screen_producer = ScreenProducer::new(match args.protocol {
                Protocol::Gossipsub => Arc::new(Mutex::new(
                    network::gossipsub::GossipP2P::run("gossipsub").await?,
                )),
                Protocol::Hybrid => Arc::new(Mutex::new(
                    network::hybrid::HybridP2P::run("hybrid").await?,
                )),
                Protocol::Stream => Arc::new(Mutex::new(
                    network::stream::P2PStreamSwarm::run("stream").await?,
                )),
            })
            .await?;

            tokio::select! {
                _ = ctrl_c() => {
                    info!("Screen producer received Ctrl+C, stopping capture");
                    screen_producer.stop().await?;
                }
            }

            std::process::exit(0);
        }
    }
    Ok(())
}
