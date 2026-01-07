use anyhow::Result;
use gst::glib::clone::Downgrade;
use gst::prelude::{Cast, ElementExt, GstBinExtManual, PadExt};
use gst_app::AppSrc;
use log::{error, info};

pub struct GstRenderer {
    pipeline: gst::Pipeline,
    appsrc: AppSrc,
}

impl GstRenderer {
    pub fn new() -> Result<Self> {
        let caps = gst::Caps::builder("application/x-rtp")
            .field("media", "video")
            .field("payload", 96i32)
            .field("encoding-name", "H264")
            .field("clock-rate", 90000i32)
            .build();
        let appsrc = AppSrc::builder()
            .caps(&caps)
            .is_live(true)
            .do_timestamp(false)
            .format(gst::Format::Time)
            .build();
        let buffer = gst::ElementFactory::make("rtpjitterbuffer")
            .property("latency", 5000u32)
            .build()?;
        let depay = gst::ElementFactory::make("rtph264depay").build()?;
        let parse = gst::ElementFactory::make("h264parse").build()?;
        let decoder = gst::ElementFactory::make("vaapih264dec")
            .property("low-latency", true)
            .build()
            .or_else(|_| gst::ElementFactory::make("v4l2h264dec").build())
            .or_else(|_| gst::ElementFactory::make("avdec_h264").build())
            .map_err(|e| {
                anyhow::anyhow!(
                    "No suitable H.264 decoder found (vaapi/v4l2/avdec/openh264): {:?}",
                    e
                )
            })?;
        let post_proc = gst::ElementFactory::make("vaapipostproc").build()?;
        let convert = gst::ElementFactory::make("videoconvert").build()?;
        let dec_queue = gst::ElementFactory::make("queue").build()?;
        let sink = gst::ElementFactory::make("autovideosink")
            .property("sync", false)
            .build()?;

        // Pipeline
        let pipeline = gst::Pipeline::new();
        pipeline.add_many([
            appsrc.upcast_ref(),
            &buffer,
            &depay,
            &parse,
            &decoder,
            &post_proc,
            &convert,
            &dec_queue,
            &sink,
        ])?;

        // Link the rest of the chain (parse -> sink) statically
        gst::Element::link_many([appsrc.upcast_ref(), &buffer, &depay, &parse, &decoder, &post_proc, &convert, &dec_queue, &sink])?;

        pipeline.set_state(gst::State::Playing)?;

        Ok(Self { pipeline, appsrc })
    }

    pub fn push_sample(&self, sample: &gst::Sample) -> Result<gst::FlowSuccess> {
        let res = self.appsrc.push_sample(sample)?;
        Ok(res)
    }

    pub fn push_ts_chunk(&self, data: &[u8]) -> Result<gst::FlowSuccess> {
        let mut buffer = gst::Buffer::with_size(data.len())?;
        {
            let mut map = buffer
                .get_mut()
                .unwrap()
                .map_writable()
                .map_err(|_| anyhow::anyhow!("Failed to map writable buffer"))?;
            map.as_mut_slice().copy_from_slice(data);
        }
        let res = self.appsrc.push_buffer(buffer)?;
        Ok(res)
    }
}

impl Drop for GstRenderer {
    fn drop(&mut self) {
        info!("Dropping GstRenderer");
        if let Err(error) = self.pipeline.set_state(gst::State::Null) {
            error!("Failed to set pipeline to NULL state: {:?}", error);
        }
    }
}
