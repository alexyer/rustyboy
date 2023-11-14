use std::ops::{Add, Div};

use crate::{gb::CYCLES_PER_SEC, mmu::Mmu};

const NR14_ADDRESS: usize = 0xff14;
const NR24_ADDRESS: usize = 0xff19;
const NR34_ADDRESS: usize = 0xff1e;
const NR44_ADDRESS: usize = 0xff23;
const NR52_ADDRESS: usize = 0xff26;

const WAVE_RAM_ADDRESS: usize = 0xff30;
const WAVE_RAM_SIZE: usize = 0x10;
const WAVE_SAMPLES_PER_BYTE: usize = 2;
const WAVE_SAMPLES_COUNT: usize = WAVE_RAM_SIZE * WAVE_SAMPLES_PER_BYTE;

const CYCLES_PER_64HZ: usize = CYCLES_PER_SEC / 64;
const CYCLES_PER_128HZ: usize = CYCLES_PER_SEC / 128;
const CYCLES_PER_256HZ: usize = CYCLES_PER_SEC / 256;
const CYCLES_PER_PWM_DIV_TICK: usize = 4;
const CYCLES_PER_WAVE_DIV_TICK: usize = 2;

/// NR XX channel register addresses.
/// Initialize with NR X4 address.
#[derive(Clone, Copy)]
pub struct NrXxAddress(usize);

impl NrXxAddress {
    pub fn nrx0_address(self) -> usize {
        self.0 - 4
    }

    pub fn nrx1_address(self) -> usize {
        self.0 - 3
    }

    pub fn nrx2_address(self) -> usize {
        self.0 - 2
    }

    pub fn nrx3_address(self) -> usize {
        self.0 - 1
    }

    pub fn nrx4_address(self) -> usize {
        self.0
    }
}

impl From<usize> for NrXxAddress {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Default)]
pub enum PwmWaveDuty {
    #[default]
    Duty0,
    Duty1,
    Duty2,
    Duty3,
}

impl PwmWaveDuty {
    pub fn as_slice(&self) -> &[u8] {
        match self {
            PwmWaveDuty::Duty0 => &[0xf, 0, 0, 0, 0, 0, 0, 0],
            PwmWaveDuty::Duty1 => &[0xf, 0xf, 0, 0, 0, 0, 0, 0],
            PwmWaveDuty::Duty2 => &[0xf, 0xf, 0xf, 0xf, 0, 0, 0, 0],
            PwmWaveDuty::Duty3 => &[0, 0, 0, 0, 0, 0, 0xf, 0xf],
        }
    }
}

impl From<&u8> for PwmWaveDuty {
    fn from(value: &u8) -> Self {
        match value {
            0x00 => PwmWaveDuty::Duty0,
            0x01 => PwmWaveDuty::Duty1,
            0x02 => PwmWaveDuty::Duty2,
            0x03 => PwmWaveDuty::Duty3,
            _ => panic!("invalid pwm duty: {value}"),
        }
    }
}

pub struct ChannelState {
    on: bool,
    samples: Vec<Sample>,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
pub struct Sample {
    value: f32,
}

impl Sample {
    pub fn as_f32(&self) -> f32 {
        self.value
    }
}

impl From<f32> for Sample {
    fn from(value: f32) -> Self {
        Self { value }
    }
}

impl From<Sample> for f32 {
    fn from(sample: Sample) -> Self {
        sample.value / 100.0
    }
}

impl From<u8> for Sample {
    fn from(value: u8) -> Self {
        let value = f32::try_from(value / 0xf).unwrap() / 2.0;

        Sample::from(value)
    }
}

impl Add for Sample {
    type Output = Sample;

    fn add(self, rhs: Self) -> Self::Output {
        Sample {
            value: self.value + rhs.value,
        }
    }
}

impl Div<f32> for Sample {
    type Output = Sample;

    fn div(self, rhs: f32) -> Self::Output {
        Sample {
            value: self.value / rhs,
        }
    }
}

pub struct PwmChannel {
    on: bool,

    duty_counter: u8,
    envelope_counter: usize,
    length_timer_counter: usize,
    period_counter: usize,
    sweep_counter: usize,

    duty: PwmWaveDuty,
    length_timer: u8,
    period: u16,
    volume: u8,

    sweep_available: bool,
    nrxx: NrXxAddress,
}

impl PwmChannel {
    pub fn new(nrx4_address: usize, sweep_available: bool) -> Self {
        Self {
            on: false,

            duty_counter: 0,
            envelope_counter: 0,
            length_timer_counter: 0,
            period_counter: 0,
            sweep_counter: 0,

            duty: PwmWaveDuty::Duty0,
            length_timer: 0,
            period: 0,
            volume: 0,
            sweep_available,
            nrxx: nrx4_address.into(),
        }
    }

    pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu) -> ChannelState {
        // Channel triggered
        if mmu.check_bit(self.nrxx.nrx4_address(), 7) {
            self.trigger(mmu);
        }

        if !self.on {
            return ChannelState {
                on: self.on,
                samples: vec![Sample { value: 0.0 }; cycles],
            };
        }

        self.update_duty(mmu);

        let mut samples = vec![];

        for _ in 0..cycles {
            self.update_duty_counter(1, mmu);
            self.update_envelope(1, mmu);

            if self.sweep_available {
                self.update_sweep(1, mmu);
            }

            samples.push(self.get_current_sample());
        }

        if mmu.check_bit(self.nrxx.nrx4_address(), 6) {
            self.update_length_timer(cycles);
        }

        ChannelState {
            on: self.on,
            samples,
        }
    }

    fn get_current_sample(&self) -> Sample {
        let sample = self.duty.as_slice()[self.duty_counter as usize];

        let amplified_sample = sample * self.volume;
        let normalized_sample = amplified_sample as f32 / 2.0;

        Sample {
            value: normalized_sample,
        }
    }

    fn update_period(&mut self, mmu: &Mmu) {
        self.period = ((mmu.read_byte(self.nrxx.nrx4_address()) as u16 & 0x7) << 8)
            | mmu.read_byte(self.nrxx.nrx3_address()) as u16;
    }

    fn trigger(&mut self, mmu: &mut Mmu) {
        self.on = true;
        self.duty_counter = 0;
        self.length_timer = mmu.read_byte(self.nrxx.nrx1_address()) & 0x3f;
        self.update_period(mmu);

        self.volume = (mmu.read_byte(self.nrxx.nrx2_address()) & 0xf0) >> 4;
        self.envelope_counter = 0;

        mmu.set_bit_to(self.nrxx.nrx4_address(), 7, false);
    }

    fn update_duty(&mut self, mmu: &Mmu) {
        let duty = mmu.read_byte(self.nrxx.nrx1_address()) >> 6;
        self.duty = PwmWaveDuty::from(&duty);
    }

    fn update_duty_counter(&mut self, cycles: usize, mmu: &Mmu) {
        self.period_counter += cycles;

        if self.period_counter >= CYCLES_PER_PWM_DIV_TICK {
            self.period_counter %= CYCLES_PER_PWM_DIV_TICK;
            self.period += 1;

            if self.period > 0x7ff {
                self.update_period(mmu);
                self.duty_counter += 1;
                self.duty_counter %= 8;
            }
        }
    }

    fn update_envelope(&mut self, cycles: usize, mmu: &Mmu) {
        let sweep_pace = (mmu.read_byte(self.nrxx.nrx2_address()) & 0x7) as usize;

        if sweep_pace == 0 {
            return;
        }

        self.envelope_counter += cycles;

        if self.volume == 0 {
            self.on = false;
            return;
        }

        if self.envelope_counter >= sweep_pace * CYCLES_PER_64HZ {
            self.envelope_counter %= sweep_pace * CYCLES_PER_64HZ;

            let env_increase = mmu.check_bit(self.nrxx.nrx2_address(), 3);

            if !env_increase {
                self.volume -= 1;
            } else {
                self.volume += 1;
                self.volume %= 0xf;
            }
        }
    }

    fn update_length_timer(&mut self, cycles: usize) {
        self.length_timer_counter += cycles;

        if self.length_timer_counter >= CYCLES_PER_256HZ {
            self.length_timer_counter %= CYCLES_PER_256HZ;
            self.length_timer += 1;

            if self.length_timer == 64 {
                self.on = false;
            }
        }
    }

    fn update_sweep(&mut self, cycles: usize, mmu: &mut Mmu) {
        let pace = ((mmu.read_byte(self.nrxx.nrx0_address()) & 0xf0) >> 4) as usize;

        if pace == 0 {
            return;
        }

        self.sweep_counter += cycles;

        if self.sweep_counter >= pace * CYCLES_PER_128HZ {
            self.sweep_counter %= pace * CYCLES_PER_128HZ;

            let period_decrease = mmu.check_bit(self.nrxx.nrx0_address(), 3);
            let step = (mmu.read_byte(self.nrxx.nrx0_address()) & 0x7) as u32;

            let nr14 = mmu.read_byte(self.nrxx.nrx4_address()) as u16;
            let period = ((nr14 & 0x7) << 8) | mmu.read_byte(self.nrxx.nrx3_address()) as u16;

            let new_period = if period_decrease {
                period - period / 2u16.pow(step)
            } else {
                period + period / 2u16.pow(step)
            };

            if new_period > 0x7ff {
                self.on = false;
            }

            let high_period = ((new_period & 0x300) >> 8) as u8;

            mmu.write_byte(self.nrxx.nrx4_address(), (nr14 & 0xf8) as u8 | high_period);

            mmu.write_byte(self.nrxx.nrx3_address(), new_period as u8);
        }
    }
}

pub struct NoiseChannel {
    on: bool,
    nrx4_address: usize,
}

impl NoiseChannel {
    pub fn new(nrx4_address: usize) -> Self {
        Self {
            on: false,
            nrx4_address,
        }
    }
}

pub struct WaveChannel {
    on: bool,
    dac_on: bool,

    length_timer_counter: usize,
    sample_counter: usize,

    nrxx: NrXxAddress,

    current_sample: usize,
    length_timer: u8,
    period: u16,
    volume: u8,
}

impl WaveChannel {
    pub fn new(nrx4_address: usize) -> Self {
        Self {
            on: false,
            dac_on: false,
            length_timer_counter: 0,
            sample_counter: 0,
            nrxx: NrXxAddress(nrx4_address),
            current_sample: 0,
            length_timer: 0,
            period: 0,
            volume: 0,
        }
    }

    pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu) -> ChannelState {
        let dac_on = mmu.check_bit(self.nrxx.nrx0_address(), 7);

        if !self.dac_on && dac_on {
            self.dac_on = true;
        }

        if mmu.check_bit(self.nrxx.nrx4_address(), 7) {
            self.trigger(mmu);
        }

        if !self.on {
            return ChannelState {
                on: self.on,
                samples: vec![Default::default(); cycles],
            };
        }

        let mut samples = vec![];

        for _ in 0..cycles {
            self.update_sample_counter(1, mmu);
            samples.push(self.get_current_sample(&mmu));
        }

        if mmu.check_bit(self.nrxx.nrx4_address(), 6) {
            self.update_length_timer(cycles);
        }

        ChannelState {
            on: self.on,
            samples,
        }
    }

    fn get_current_sample(&self, mmu: &Mmu) -> Sample {
        let sample_byte_index = self.current_sample / WAVE_SAMPLES_PER_BYTE;
        let shift = self.current_sample % WAVE_SAMPLES_PER_BYTE;

        let sample_byte = mmu.read_byte(WAVE_RAM_ADDRESS + sample_byte_index);
        let sample = sample_byte >> shift;

        match self.volume {
            0x0 => Sample::default(),
            0x1 => Sample::from(sample),
            0x2 => Sample::from(sample),
            0x3 => Sample::from(sample),
            _ => unreachable!(),
        }
    }

    fn update_period(&mut self, mmu: &Mmu) {
        self.period = ((mmu.read_byte(self.nrxx.nrx4_address()) as u16 & 0x7) << 8)
            | mmu.read_byte(self.nrxx.nrx3_address()) as u16;
    }

    fn update_length_timer(&mut self, cycles: usize) {
        self.length_timer_counter += cycles;

        if self.length_timer_counter >= CYCLES_PER_256HZ {
            self.length_timer_counter %= CYCLES_PER_256HZ;
            self.length_timer += 1;

            if self.length_timer == 64 {
                self.on = false;
            }
        }
    }

    fn update_sample_counter(&mut self, cycles: usize, mmu: &Mmu) {
        self.sample_counter += cycles;

        if self.sample_counter >= CYCLES_PER_WAVE_DIV_TICK {
            self.sample_counter %= CYCLES_PER_WAVE_DIV_TICK;
            self.period += 1;

            if self.period > 0x7ff {
                self.update_period(mmu);
                self.current_sample += 1;
                self.current_sample %= WAVE_SAMPLES_COUNT;
            }
        }
    }

    fn trigger(&mut self, mmu: &mut Mmu) {
        self.on = true;
        self.current_sample = 1;

        self.length_timer = mmu.read_byte(self.nrxx.nrx1_address());
        self.update_period(mmu);

        self.volume = (mmu.read_byte(self.nrxx.nrx2_address()) & 0x60) >> 5;

        mmu.set_bit_to(self.nrxx.nrx4_address(), 7, false);
    }
}

pub struct Apu {
    channel1: PwmChannel,
    channel2: PwmChannel,
    channel3: WaveChannel,
    channel4: NoiseChannel,
}

macro_rules! check_nr52 {
    ($name:ident, $bit:expr) => {
        fn $name(&self, mmu: &Mmu) -> bool {
            mmu.check_bit(NR52_ADDRESS, $bit)
        }
    };
}

macro_rules! set_nr52 {
    ($name:ident, $bit:expr) => {
        fn $name(&self, value: bool, mmu: &mut Mmu) {
            mmu.set_bit_to(NR52_ADDRESS, $bit, value);
        }
    };
}

impl Default for Apu {
    fn default() -> Self {
        Self {
            channel1: PwmChannel::new(NR14_ADDRESS, true),
            channel2: PwmChannel::new(NR24_ADDRESS, false),
            channel3: WaveChannel::new(NR34_ADDRESS),
            channel4: NoiseChannel::new(NR24_ADDRESS),
        }
    }
}

impl Apu {
    check_nr52!(channel1_on, 0);
    check_nr52!(channel2_on, 1);
    check_nr52!(channel3_on, 2);
    check_nr52!(channel4_on, 3);
    check_nr52!(on, 7);

    set_nr52!(set_channel1_to, 0);
    set_nr52!(set_channel2_to, 1);
    set_nr52!(set_channel3_to, 2);
    set_nr52!(set_channel4_to, 3);

    pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu) -> Vec<Sample> {
        if !self.on(mmu) {
            return vec![Default::default(); cycles];
        }

        let channel1_state = self.channel1.tick(cycles, mmu);
        self.set_channel1_to(channel1_state.on, mmu);

        let channel2_state = self.channel2.tick(cycles, mmu);
        self.set_channel2_to(channel2_state.on, mmu);

        let channel3_state = self.channel3.tick(cycles, mmu);
        self.set_channel3_to(channel3_state.on, mmu);

        self.mix(
            channel1_state.samples,
            channel2_state.samples,
            channel3_state.samples,
        )
    }

    pub fn mix(
        &self,
        channel1: Vec<Sample>,
        channel2: Vec<Sample>,
        channel3: Vec<Sample>,
    ) -> Vec<Sample> {
        itertools::izip!(channel1, channel2, channel3)
            .map(|(sample1, sample2, sample3)| (sample1 + sample2 + sample3) / 3.0)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_on() {
        let mut mmu = Mmu::default();
        let apu = Apu::default();

        assert!(!apu.on(&mmu));

        mmu.write_byte(NR52_ADDRESS, 1 << 7);

        assert!(apu.on(&mmu));
    }

    #[test]
    fn test_is_channel1_on() {
        let mut mmu = Mmu::default();
        let apu = Apu::default();

        assert!(!apu.channel1_on(&mmu));

        mmu.write_byte(NR52_ADDRESS, 1);

        assert!(apu.channel1_on(&mmu));
    }
    #[test]
    fn test_is_channel2_on() {
        let mut mmu = Mmu::default();
        let apu = Apu::default();

        assert!(!apu.channel2_on(&mmu));

        mmu.write_byte(NR52_ADDRESS, 1 << 1);

        assert!(apu.channel2_on(&mmu));
    }
    #[test]
    fn test_is_channel3_on() {
        let mut mmu = Mmu::default();
        let apu = Apu::default();

        assert!(!apu.channel3_on(&mmu));

        mmu.write_byte(NR52_ADDRESS, 1 << 2);

        assert!(apu.channel3_on(&mmu));
    }
    #[test]
    fn test_is_channel4_on() {
        let mut mmu = Mmu::default();
        let apu = Apu::default();

        assert!(!apu.channel4_on(&mmu));

        mmu.write_byte(NR52_ADDRESS, 1 << 3);

        assert!(apu.channel4_on(&mmu));
    }

    #[test]
    fn test_pwm_channel_length_timer() {
        let mut mmu = Mmu::default();
        let mut channel = PwmChannel::new(NR14_ADDRESS, true);

        mmu.write_byte(NR14_ADDRESS, 0xff);
        mmu.write_byte(NR14_ADDRESS - 3, 0x3f);

        channel.tick(CYCLES_PER_256HZ / 2, &mut mmu);

        assert!(channel.on);

        channel.tick(CYCLES_PER_256HZ / 2, &mut mmu);

        assert!(!channel.on);
    }
}
