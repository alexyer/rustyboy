use std::{
    collections::VecDeque,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex,
    },
    time::Duration,
};

use rodio::{OutputStream, OutputStreamHandle, Sink, Source};

use crate::apu::Sample;

pub const SAMPLE_RATE: u32 = 44100;

pub trait Audio {
    fn update(&mut self, samples: Vec<Sample>);
    fn len(&self) -> usize;
}

pub struct NoAudio;

impl Audio for NoAudio {
    fn update(&mut self, _samples: Vec<Sample>) {}

    fn len(&self) -> usize {
        42
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
        let samples_buffer = GbSamplesBuffer::new(SAMPLE_RATE, vec![]);
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

    fn len(&self) -> usize {
        self.samples_buffer.len.load(Ordering::Acquire)
    }
}

#[derive(Clone)]
pub struct GbSamplesBuffer {
    data: Arc<Mutex<VecDeque<Sample>>>,
    played_samples: Arc<Mutex<Vec<f32>>>,
    sample_rate: u32,
    len: Arc<AtomicUsize>,
}

impl GbSamplesBuffer {
    pub fn new(sample_rate: u32, samples: Vec<Sample>) -> Self {
        let mut data = VecDeque::new();

        data.extend(samples);

        Self {
            data: Arc::new(Mutex::new(data)),
            played_samples: Arc::new(Mutex::new(vec![])),
            sample_rate,
            len: Arc::new(AtomicUsize::new(0)),
        }
    }

    pub fn update(&mut self, samples: Vec<Sample>) {
        self.len.fetch_add(samples.len(), Ordering::Release);

        let mut data = self.data.lock().unwrap();
        data.extend(samples);
    }
}

impl Iterator for GbSamplesBuffer {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let mut data = self.data.lock().unwrap();
        let sample = data.pop_front().or(Some(0.0.into())).map(Into::into);

        self.played_samples
            .lock()
            .unwrap()
            .push(sample.unwrap_or(0.0) * 0.1);

        self.len
            .fetch_update(Ordering::Release, Ordering::Acquire, |x| {
                Some(x.saturating_sub(1))
            })
            .unwrap();

        sample
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
