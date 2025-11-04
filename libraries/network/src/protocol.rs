use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct VideoStreamChunk {
    #[serde(with = "serde_bytes")]
    pub chunk: Vec<u8>,
}
