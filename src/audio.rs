use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
    time::Duration,
};

use rodio::{OutputStream, OutputStreamHandle, Sink, Source};

use crate::{apu::Sample, gb::CYCLES_PER_SEC};

pub trait Audio {
    fn update(&mut self, samples: Vec<Sample>);
    fn is_full(&self) -> bool;
}

pub struct NoAudio;

impl Audio for NoAudio {
    fn update(&mut self, _samples: Vec<Sample>) {}

    fn is_full(&self) -> bool {
        false
    }
}

pub struct Rodio {
    _audio_stream: OutputStream,
    _audio_stream_handle: OutputStreamHandle,
    _audio_sink: Sink,
    samples_buffer: GbSamplesBuffer,
}

impl Rodio {
    pub fn new() -> Self {
        let samples_buffer = GbSamplesBuffer::new(CYCLES_PER_SEC as u32, vec![]);
        let (_audio_stream, _audio_stream_handle) = OutputStream::try_default().unwrap();

        let _audio_sink = Sink::try_new(&_audio_stream_handle).unwrap();

        _audio_sink.append(samples_buffer.clone().amplify(0.1));

        _audio_sink.play();

        Self {
            _audio_stream,
            _audio_stream_handle,
            _audio_sink,
            samples_buffer,
        }
    }
}

impl Audio for Rodio {
    fn update(&mut self, samples: Vec<Sample>) {
        self.samples_buffer.update(samples);
    }

    fn is_full(&self) -> bool {
        self.samples_buffer.data.lock().unwrap().len() > CYCLES_PER_SEC / 20
    }
}

#[derive(Clone)]
pub struct GbSamplesBuffer {
    data: Arc<Mutex<VecDeque<Sample>>>,
    sample_rate: u32,
}

impl GbSamplesBuffer {
    pub fn new(sample_rate: u32, samples: Vec<Sample>) -> Self {
        let mut data = VecDeque::new();

        data.extend(samples);

        Self {
            data: Arc::new(Mutex::new(data)),
            sample_rate,
        }
    }

    pub fn update(&mut self, samples: Vec<Sample>) {
        let mut data = self.data.lock().unwrap();
        data.extend(samples);
    }
}

impl Iterator for GbSamplesBuffer {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let mut data = self.data.lock().unwrap();
        data.pop_front().or(Some(0.0.into())).map(Into::into)
    }
}

impl Source for GbSamplesBuffer {
    fn current_frame_len(&self) -> Option<usize> {
        Some(1024)
    }

    fn channels(&self) -> u16 {
        1
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn total_duration(&self) -> Option<Duration> {
        None
    }
}
