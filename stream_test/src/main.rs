mod analyze;
mod capture;
mod render;

use crate::analyze::Analyze;
use crate::capture::Capture;
use anyhow::Result;
use clap::{Parser, ValueEnum};
use log::{info, warn};
use std::env;
use std::fmt::Display;
use std::time::Duration;
use std::sync::Arc;
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
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long, default_value = "gossipsub")]
    protocol: Protocol,

    #[arg(short, long, default_value = "manager")]
    role: Role,
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
            let mut client_child = Command::new(&exe)
                .arg("--protocol")
                .arg(&args.protocol.to_string())
                .arg("--role")
                .arg("analyzer")
                .spawn()?;
            let client_pid = client_child.id().unwrap();
            let mut producer_child = Command::new(&exe)
                .arg("--protocol")
                .arg(&args.protocol.to_string())
                .arg("--role")
                .arg("producer")
                .spawn()?;
            let producer_pid = producer_child.id().unwrap();
            let client_child_wait = client_child.wait();
            let producer_wait = producer_child.wait();
            tokio::select! {
                _ = futures_util::future::join_all([client_child_wait, producer_wait]) => {}
                _ = tokio::signal::ctrl_c() => {
                    unsafe {
                        libc::kill(client_pid as i32, libc::SIGINT);
                        libc::kill(producer_pid as i32, libc::SIGINT);
                    }
                    futures_util::future::join_all([
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
            let mut wtr = csv::WriterBuilder::new().from_path("Output.csv")?;
            let result = analyze.result.lock().await;
            for item in result.as_slice() {
                wtr.serialize(item)?;
            }
            wtr.flush()?;
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
                Protocol::Gossipsub => Capture::new(Arc::new(Mutex::new(
                    network::gossipsub::GossipP2P::run("gossipsub").await?,
                ))),
                Protocol::Hybrid => Capture::new(Arc::new(Mutex::new(
                    network::hybrid::HybridP2P::run("hybrid").await?,
                ))),
                Protocol::Stream => Capture::new(Arc::new(Mutex::new(
                    network::stream::P2PStreamSwarm::run("stream").await?,
                ))),
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
    }
    Ok(())
}
