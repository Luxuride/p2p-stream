use crate::protocol::VideoStreamChunk;
use async_channel::Receiver;
use async_trait::async_trait;

pub mod gossipsub;
pub mod hybrid;
pub mod protocol;
pub mod stream;

#[async_trait]
pub trait P2PSwarm
where
    Self: Send + Sync,
{
    /// Access the receiver for inbound chunks.
    fn inbound_rx(&self) -> &Receiver<VideoStreamChunk>;

    /// Queue a chunk to be sent to all connected peers.
    async fn send_message(&mut self, chunk: VideoStreamChunk);
}
