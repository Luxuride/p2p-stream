mod analyze;
mod capture;
mod render;

use crate::analyze::Analyze;
use crate::capture::Capture;
use anyhow::Result;
use clap::{Parser, ValueEnum};
use std::env;
use std::fmt::Display;
use std::sync::Arc;
use tokio::process::Command;
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
    Client,
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
            let exe = env::current_exe()?;
            let mut client_child = Command::new(&exe)
                .arg("--protocol")
                .arg(&args.protocol.to_string())
                .arg("--role")
                .arg("client")
                .spawn()?;
            let mut producer_child = Command::new(&exe)
                .arg("--protocol")
                .arg(&args.protocol.to_string())
                .arg("--role")
                .arg("producer")
                .spawn()?;
            let client_child_wait = client_child.wait();
            let producer_wait = producer_child.wait();
            futures_util::future::join_all([client_child_wait, producer_wait]).await;
        }
        Role::Client => {
            println!("Starting Client");
            let analyze = match args.protocol {
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
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
            let mut wtr = csv::Writer::from_path("out.csv")?;
            let result = analyze.result.lock().await;
            for item in result.as_slice() {
                wtr.serialize(item)?;
            }
            wtr.flush()?;
        }
        Role::Producer => {
            println!("Starting producer");
            let capture = match args.protocol {
                Protocol::Gossipsub => Capture::new(Arc::new(Mutex::new(
                    network::gossipsub::GossipP2P::run("gossipsub").await?,
                ))),
                Protocol::Hybrid => Capture::new(Arc::new(Mutex::new(
                    network::hybrid::HybridP2P::run("hybrid").await?,
                ))),
                Protocol::Stream => Capture::new(Arc::new(Mutex::new(
                    network::stream::P2PStreamSwarm::run("stream").await?,
                ))),
            };
            capture.await?;
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
        }
    }
    Ok(())
}
