use serde::{Deserialize, Serialize};
use std::time::SystemTime;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct VideoStreamChunk {
    #[serde(with = "serde_bytes")]
    pub chunk: Vec<u8>,
    pub timestamp: SystemTime,
    pub index: u64,
}
