use anyhow::Result;
use gst::Pipeline;
use gst::prelude::{Cast, ElementExt, GstBinExtManual, ObjectExt, PadExt};
use gst_app::AppSink;
use log::{trace, warn};

#[derive(Default)]
pub struct VideoReader {
    pipeline: Option<Pipeline>,
    app_sink: Option<AppSink>,
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
                "/home/lux/Documents/Rust/p2p-stream/big_buck_bunny_1080p_h264.mov",
            )
            .build()?;
        let demux = gst::ElementFactory::make("qtdemux").build()?;
        let start_parse = gst::ElementFactory::make("h264parse").build()?;
        let dec = gst::ElementFactory::make("vaapih264dec").build()?;
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
            let _ = pipeline.set_state(gst::State::Null);
        }
        self.app_sink = None;
        Ok(())
    }

    pub async fn wait_for_eos(&self) -> Result<()> {
        use anyhow::anyhow;
        use gst::MessageView;

        // If there's no pipeline, nothing to wait for.
        let pipeline = match &self.pipeline {
            Some(p) => p.clone(),
            None => return Ok(()),
        };

        let bus = pipeline
            .bus()
            .ok_or(anyhow!("Pipeline has no bus to wait on"))?;

        // Clone the bus for moving into the blocking task.
        let bus_cloned = bus.clone();

        // Block in a dedicated thread until EOS or ERROR arrives.
        let msg_opt = tokio::task::spawn_blocking(move || {
            // Wait forever for EOS or ERROR
            bus_cloned.timed_pop_filtered(
                gst::ClockTime::NONE,
                &[gst::MessageType::Eos, gst::MessageType::Error],
            )
        })
        .await
        .map_err(|e| anyhow!("Failed to join blocking task: {:?}", e))?
        .unwrap();

        match msg_opt.view() {
            MessageView::Eos(_) => Ok(()),
            MessageView::Error(err) => Err(anyhow!(
                "Pipeline error: {} ({:?})",
                err.error(),
                err.debug()
            )),
            _ => Ok(()),
        }
    }
}
