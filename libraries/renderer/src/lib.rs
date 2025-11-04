use anyhow::Result;
use gst::glib::clone::Downgrade;
use gst::prelude::{Cast, ElementExt, ElementExtManual, GstBinExtManual, PadExt};
use gst_app;
use gst_app::AppSrc;
use log::{error, info};

pub struct GstRenderer {
    pipeline: gst::Pipeline,
    appsrc: gst_app::AppSrc,
}

impl GstRenderer {
    pub fn new() -> Result<Self> {
        let caps = gst::Caps::builder("application/x-rtp")
            .field("media", &"video")
            .field("payload", &33i32)
            .field("encoding-name", &"MP2T")
            .field("clock-rate", &90000i32)
            .build();
        let appsrc = AppSrc::builder()
            .caps(&caps)
            .is_live(true)
            .do_timestamp(false)
            .format(gst::Format::Time)
            .build();
        let buffer = gst::ElementFactory::make("rtpjitterbuffer")
            .property("latency", 2000u32)
            .build()?;
        let depay = gst::ElementFactory::make("rtpmp2tdepay").build()?;
        let demux = gst::ElementFactory::make("tsdemux").build()?;
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
        let convert = gst::ElementFactory::make("videoconvert").build()?;
        let dec_queue = gst::ElementFactory::make("queue").build()?;
        let sink = gst::ElementFactory::make("autovideosink")
            .property("sync", false)
            .build()?;

        // Pipeline
        let pipeline = gst::Pipeline::new();
        pipeline.add_many(&[
            appsrc.upcast_ref(),
            &buffer,
            &depay,
            &demux,
            &parse,
            &decoder,
            &convert,
            &dec_queue,
            &sink,
        ])?;

        // Link up to the demux statically
        gst::Element::link_many(&[appsrc.upcast_ref(), &buffer, &depay, &demux])?;

        // Link the rest of the chain (parse -> sink) statically
        gst::Element::link_many(&[&parse, &decoder, &convert, &dec_queue, &sink])?;

        // Dynamically link tsdemux to h264parse when the correct pad appears
        let parse_weak = parse.downgrade();
        demux.connect_pad_added(move |_demux, src_pad| {
            let Some(parse) = parse_weak.upgrade() else {
                return;
            };

            // Only accept H.264 video pads
            if let Some(caps) = src_pad.current_caps() {
                if let Some(structure) = caps.structure(0) {
                    let name = structure.name().as_str();
                    if name != "video/x-h264" {
                        return; // Ignore non-H264 streams
                    }
                }
            }

            if let Some(sink_pad) = parse.static_pad("sink") {
                if sink_pad.is_linked() {
                    return;
                }
                if let Err(e) = src_pad.link(&sink_pad) {
                    error!("Failed to link tsdemux to h264parse: {:?}", e);
                }
            }
        });

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
