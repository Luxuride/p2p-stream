use anyhow::Result;
use ashpd::desktop::Session;
use ashpd::desktop::screencast::{CursorMode, Screencast, SourceType, Stream};
use gst::Pipeline;
use gst::prelude::{Cast, ElementExt, ElementExtManual, GstBinExtManual, ObjectExt};
use gst_app::AppSink;
use log::{trace, warn};
use std::os::fd::{AsRawFd, OwnedFd};

pub struct ActiveCapture<'a> {
    stream: Stream,
    stream_fd: OwnedFd,
    session: Session<'a, Screencast<'a>>,
}

#[derive(Default)]
pub struct ScreenCapture<'a> {
    active_capture: Option<ActiveCapture<'a>>,
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

impl<'a> ScreenCapture<'_> {
    async fn open_portal() -> Result<ActiveCapture<'a>> {
        let proxy = Screencast::new().await?;
        let session = proxy.create_session().await?;
        proxy
            .select_sources(
                &session,
                CursorMode::Embedded,
                SourceType::Monitor | SourceType::Window,
                false,
                None,
                ashpd::desktop::PersistMode::ExplicitlyRevoked,
            )
            .await?;

        let response = proxy.start(&session, None).await?.response()?;
        let stream = response
            .streams()
            .first()
            .expect("No stream found or selected")
            .to_owned();

        let fd = proxy.open_pipe_wire_remote(&session).await?;

        Ok(ActiveCapture {
            stream,
            stream_fd: fd,
            session,
        })
    }

    fn build_pipeline(&mut self) -> Result<Pipeline> {
        let Some(active_capture) = &self.active_capture else {
            return Err(anyhow::anyhow!("No active capture"));
        };
        let pipewire_node_id = &active_capture.stream.pipe_wire_node_id();
        let stream_raw_fd = &active_capture.stream_fd.as_raw_fd();

        trace!(
            "Building pipeline with PipeWire node ID: {}, FD: {}",
            pipewire_node_id, stream_raw_fd
        );

        let pipewire_element = gst::ElementFactory::make("pipewiresrc")
            .property("fd", stream_raw_fd)
            .property("path", pipewire_node_id.to_string())
            .build()?;
        let src_queue = gst::ElementFactory::make("queue")
            .property_from_str("leaky", "downstream")
            .build()?;
        let videorate = gst::ElementFactory::make("videorate").build()?;
        let convert = gst::ElementFactory::make("videoconvert").build()?;
        let time_overlay = gst::ElementFactory::make("timeoverlay").build()?;

        // Try hardware acceleration first, fall back to software encoding
        let (vaapi_post_proc, encoder) = if let (Ok(vaapi_post_proc), Ok(vaapi_enc)) = (
            gst::ElementFactory::make("vaapipostproc").build(),
            gst::ElementFactory::make("vaapih264enc")
                .property("quality-level", 6u32)
                .property("bitrate", 1000u32)
                .property("keyframe-period", 0u32)
                .build(),
        ) {
            (vaapi_post_proc, vaapi_enc)
        } else {
            (
                gst::ElementFactory::make("identity").build()?,
                fallback_to_software_encoder()?,
            )
        };
        let parse = gst::ElementFactory::make("h264parse")
            .property("config-interval", 1i32)
            .build()?;
        let mux = gst::ElementFactory::make("mpegtsmux")
            .property("alignment", 7i32)
            .build()?;
        let pay = gst::ElementFactory::make("rtpmp2tpay")
            .property("pt", 33u32)
            .build()?;

        let payload_queue = gst::ElementFactory::make("queue").build()?;
        // Create AppSink for RTP H.264 stream
        let app_sink = AppSink::builder().build();

        // Set properties after building
        app_sink.set_property("emit-signals", true);
        app_sink.set_property("max-buffers", 1u32);
        app_sink.set_property("drop", true);

        let pipeline = gst::Pipeline::default();
        pipeline.add_many([
            &pipewire_element,
            &src_queue,
            &videorate,
            &convert,
            &time_overlay,
            &vaapi_post_proc,
            &encoder,
            &parse,
            &mux,
            &pay,
            &payload_queue,
            app_sink.upcast_ref(),
        ])?;

        // Link elements
        gst::Element::link_many([
            &pipewire_element,
            &src_queue,
            &videorate,
            &convert,
            &time_overlay,
            &vaapi_post_proc,
            &encoder,
            &parse,
            &mux,
            &pay,
            &payload_queue,
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
        self.active_capture = Some(Self::open_portal().await?);
        self.pipeline = Some(self.build_pipeline()?);
        Ok(())
    }

    pub async fn stop(&mut self) -> Result<()> {
        if let Some(pipeline) = self.pipeline.take() {
            pipeline.send_event(gst::event::Eos::new());
            let _ = pipeline.set_state(gst::State::Null);
        }
        if let Some(active_capture) = self.active_capture.take() {
            active_capture.session.close().await?;
        }
        self.app_sink = None;
        Ok(())
    }
}
