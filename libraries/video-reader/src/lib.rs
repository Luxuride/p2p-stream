use anyhow::Result;
use gst::Pipeline;
use gst::prelude::{Cast, ElementExt, ElementExtManual, GstBinExtManual, ObjectExt, PadExt};
use gst_app::AppSink;
use log::{error, info, trace, warn};
use std::time::Duration;

#[derive(Default)]
pub struct VideoReader {
    pipeline: Option<Pipeline>,
    app_sink: Option<AppSink>,
}

// Helper function to create a software decoder when hardware acceleration fails
fn fallback_to_software_decoder() -> Result<gst::Element> {
    warn!("Falling back to software H.264 decoder (avdec_h264)");
    gst::ElementFactory::make("avdec_h264")
        .build()
        .map_err(|e| anyhow::anyhow!("Failed to create software H.264 decoder: {:?}", e))
}

// Helper function to create a software encoder when hardware acceleration fails
fn fallback_to_software_encoder() -> Result<gst::Element> {
    warn!("Falling back to software x264 encoder");
    gst::ElementFactory::make("x264enc")
        .property("tune", "zerolatency")
        .property("speed-preset", "ultrafast")
        .property("bitrate", 500u32)
        .property("keyframe-period", 30u32)
        .property("rate-control", 2u32)
        .build()
        .map_err(|e| anyhow::anyhow!("Failed to create software encoder: {:?}", e))
}

impl VideoReader {
    fn build_pipeline(&mut self) -> Result<Pipeline> {
        let file_src = gst::ElementFactory::make("filesrc")
            .property(
                "location",
                std::env::var("VIDEO_SOURCE").expect("VIDEO_SOURCE env var not set"),
            )
            .build()?;
        let demux = gst::ElementFactory::make("qtdemux").build()?;
        let start_parse = gst::ElementFactory::make("h264parse").build()?;

        // Try hardware acceleration first, fall back to software decoding
        let dec = if let Ok(vaapi_dec) = gst::ElementFactory::make("vaapih264dec").build() {
            trace!("Using VA-API H.264 decoder");
            vaapi_dec
        } else {
            fallback_to_software_decoder()?
        };

        let time_overlay = gst::ElementFactory::make("timeoverlay").build()?;

        // Try hardware acceleration first, fall back to software encoding
        let encoder = if let Ok(vaapi_enc) = gst::ElementFactory::make("vaapih264enc")
            .property("quality-level", 6u32)
            .property("bitrate", 500u32)
            .property("keyframe-period", 0u32)
            .build()
        {
            vaapi_enc
        } else {
            fallback_to_software_encoder()?
        };
        let end_parse = gst::ElementFactory::make("h264parse")
            .property("config-interval", 1i32)
            .build()?;
        let payload_queue = gst::ElementFactory::make("queue").build()?;
        let pay = gst::ElementFactory::make("rtph264pay")
            .property("mtu", 2u32.pow(12))
            .property("pt", 96u32)
            .build()?;

        // Create AppSink for RTP H.264 stream
        let app_sink = AppSink::builder().build();

        // Set properties after building
        app_sink.set_property("emit-signals", true);
        app_sink.set_property("max-buffers", 1u32);
        app_sink.set_property("drop", true);

        let pipeline = gst::Pipeline::default();
        pipeline.add_many([
            &file_src,
            &demux,
            &start_parse,
            &dec,
            &time_overlay,
            &encoder,
            &end_parse,
            &payload_queue,
            &pay,
            app_sink.upcast_ref(),
        ])?;

        // Link elements
        gst::Element::link_many([&file_src, &demux])?;
        let start_parse_clone = start_parse.clone();
        demux.connect_pad_added(move |_demux, src_pad| {
            let sink_pad = start_parse_clone
                .static_pad("sink")
                .expect("No sink pad on parse");

            if sink_pad.is_linked() {
                return;
            }

            let _ = src_pad.link(&sink_pad).ok();
        });
        gst::Element::link_many([
            &start_parse,
            &dec,
            &time_overlay,
            &encoder,
            &end_parse,
            &payload_queue,
            &pay,
            app_sink.upcast_ref(),
        ])?;

        self.app_sink = Some(app_sink);
        pipeline.set_state(gst::State::Playing)?;

        Ok(pipeline)
    }

    pub fn pull_sample(&self) -> Option<gst::Sample> {
        self.app_sink
            .as_ref()?
            .try_pull_sample(gst::ClockTime::ZERO)
    }

    pub fn set_new_sample_callback<F>(&self, callback: F)
    where
        F: Fn(&AppSink) -> Result<gst::FlowSuccess, gst::FlowError> + Send + Sync + 'static,
    {
        if let Some(app_sink) = &self.app_sink {
            trace!("Setting up AppSink callback...");
            app_sink.set_callbacks(
                gst_app::AppSinkCallbacks::builder()
                    .new_sample(callback)
                    .build(),
            );
        }
    }

    pub async fn start(&mut self) -> Result<()> {
        self.stop().await?;
        self.pipeline = Some(self.build_pipeline()?);
        Ok(())
    }

    pub async fn stop(&mut self) -> Result<()> {
        if let Some(pipeline) = self.pipeline.take() {
            info!("Stopping video reader pipeline (forcing NULL)");
            let _ = pipeline.set_state(gst::State::Null);
        }
        self.app_sink = None;
        Ok(())
    }

    pub fn request_eos(&self) -> Result<bool> {
        let Some(pipeline) = &self.pipeline else {
            return Ok(false);
        };
        info!("Requesting EOS on video reader pipeline");
        Ok(pipeline.send_event(gst::event::Eos::new()))
    }

    pub async fn wait_for_eos_timeout(&self, timeout: Duration) -> Result<bool> {
        use anyhow::anyhow;
        use gst::MessageView;

        let pipeline = match &self.pipeline {
            Some(p) => p.clone(),
            None => return Ok(true),
        };

        let bus = pipeline
            .bus()
            .ok_or(anyhow!("Pipeline has no bus to wait on"))?;

        let wait_time = gst::ClockTime::from_nseconds(timeout.as_nanos() as u64);
        let bus_cloned = bus.clone();
        let msg_opt = tokio::task::spawn_blocking(move || {
            bus_cloned.timed_pop_filtered(
                wait_time,
                &[gst::MessageType::Eos, gst::MessageType::Error],
            )
        })
        .await
        .map_err(|e| anyhow!("Failed to join EOS wait task: {:?}", e))?;

        let Some(msg) = msg_opt else {
            return Ok(false);
        };

        match msg.view() {
            MessageView::Eos(_) => {
                info!("Received EOS from video reader pipeline");
                Ok(true)
            }
            MessageView::Error(err) => {
                error!(
                    "Pipeline error while waiting for EOS: {} ({:?})",
                    err.error(),
                    err.debug()
                );
                Err(anyhow!(
                    "Pipeline error: {} ({:?})",
                    err.error(),
                    err.debug()
                ))
            }
            _ => Ok(false),
        }
    }

    pub async fn shutdown_graceful(&mut self, timeout: Duration) -> Result<()> {
        let eos_sent = self.request_eos()?;
        if eos_sent {
            let reached_eos = self.wait_for_eos_timeout(timeout).await?;
            if !reached_eos {
                warn!("Timed out waiting for EOS after {:?}, forcing NULL", timeout);
            }
        } else {
            warn!("No active pipeline found while requesting EOS");
        }
        self.stop().await
    }

    pub async fn wait_for_eos(&self) -> Result<()> {
        let wait_step = Duration::from_millis(250);
        loop {
            if self.pipeline.is_none() {
                return Ok(());
            }

            let reached_eos = self.wait_for_eos_timeout(wait_step).await?;
            if reached_eos {
                return Ok(());
            }
        }
    }
}
