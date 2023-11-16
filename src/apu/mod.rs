mod channel;
mod noise_channel;
mod pwm_channel;
mod sample;
mod wave_channel;

pub use sample::Sample;

use crate::{audio::SAMPLE_RATE, gb::CYCLES_PER_SEC, mmu::Mmu};

use self::{
    channel::{Dac, LengthModChannel, SweepModChannel, VolumeModChannel},
    noise_channel::NoiseChannel,
    pwm_channel::PwmChannel,
    wave_channel::WaveChannel,
};

const NR14_ADDRESS: usize = 0xff14;
const NR24_ADDRESS: usize = 0xff19;
const NR34_ADDRESS: usize = 0xff1e;
const NR44_ADDRESS: usize = 0xff23;
const NR52_ADDRESS: usize = 0xff26;

const CYCLES_PER_512HZ: usize = CYCLES_PER_SEC / 512;
const CYCLES_PER_SAMPLE_RATE: usize = CYCLES_PER_SEC / SAMPLE_RATE as usize;

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

pub struct Apu {
    channel1: Dac<LengthModChannel<VolumeModChannel<SweepModChannel<PwmChannel>>, 64>>,
    channel2: Dac<LengthModChannel<VolumeModChannel<PwmChannel>, 64>>,
    channel3: Dac<LengthModChannel<WaveChannel, 256>>,
    channel4: Dac<LengthModChannel<VolumeModChannel<NoiseChannel>, 256>>,

    sample_rate_counter: usize,
    pub sample_ready: bool,

    frame_seq_step: u8,

    dac_seq_counter: usize,
    frame_seq_counter: usize,

    frame_seq_clocked: bool,
    length_ctr_clocked: bool,
    vol_env_clocked: bool,
    sweep_clocked: bool,

    noise_clocked: bool,
    pwm_clocked: bool,
    wave_clocked: bool,
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
            channel1: Dac::new(NR14_ADDRESS),
            channel2: Dac::new(NR24_ADDRESS),
            channel3: Dac::new(NR34_ADDRESS),
            channel4: Dac::new(NR44_ADDRESS),

            sample_rate_counter: 0,
            sample_ready: false,

            frame_seq_step: 0,

            dac_seq_counter: 0,
            frame_seq_counter: 0,

            frame_seq_clocked: false,
            length_ctr_clocked: false,
            vol_env_clocked: false,
            sweep_clocked: false,

            noise_clocked: false,
            pwm_clocked: false,
            wave_clocked: false,
        }
    }
}

#[allow(unused)]
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

    pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu) {
        if !self.on(mmu) {
            return;
        }

        for _ in 0..cycles {
            self.sample_rate_counter += 1;

            if self.sample_rate_counter > CYCLES_PER_SAMPLE_RATE {
                self.sample_rate_counter -= CYCLES_PER_SAMPLE_RATE;
                self.sample_ready = true;
            }

            self.clock_timers();

            if self.pwm_clocked {
                let channel1_state = self.channel1.tick(
                    mmu,
                    self.length_ctr_clocked,
                    self.vol_env_clocked,
                    self.sweep_clocked,
                );

                self.set_channel1_to(channel1_state.on, mmu);

                let channel2_state = self.channel2.tick(
                    mmu,
                    self.length_ctr_clocked,
                    self.vol_env_clocked,
                    self.sweep_clocked,
                );

                self.set_channel2_to(channel2_state.on, mmu);
            }

            if self.wave_clocked {
                let channel3_state = self.channel3.tick(
                    mmu,
                    self.length_ctr_clocked,
                    self.vol_env_clocked,
                    self.sweep_clocked,
                );

                self.set_channel3_to(channel3_state.on, mmu);
            }

            if self.noise_clocked {
                let channel4_state = self.channel4.tick(
                    mmu,
                    self.length_ctr_clocked,
                    self.vol_env_clocked,
                    self.sweep_clocked,
                );

                self.set_channel4_to(channel4_state.on, mmu);
            }

            self.reset_clocked();
        }
    }

    pub fn get_sample(&mut self, mmu: &Mmu) -> Sample {
        (self.channel1.current_sample(mmu)
            + self.channel2.current_sample(mmu)
            + self.channel3.current_sample(mmu)
            + self.channel4.current_sample(mmu))
            / 4.0
    }

    fn clock_timers(&mut self) {
        self.clock_frame_seq();
        self.clock_dac_seq();
    }

    fn clock_dac_seq(&mut self) {
        self.dac_seq_counter = (self.dac_seq_counter + 1) % 4;
        self.noise_clocked = true;

        match self.dac_seq_counter {
            0 => {
                self.pwm_clocked = true;
                self.wave_clocked = true;
            }
            2 => self.wave_clocked = true,
            _ => (),
        }
    }

    fn clock_frame_seq(&mut self) {
        self.frame_seq_counter += 1;

        if self.frame_seq_counter >= CYCLES_PER_512HZ {
            self.frame_seq_counter %= CYCLES_PER_512HZ;

            self.frame_seq_step = (self.frame_seq_step + 1) % 8;
            self.frame_seq_clocked = true;

            match self.frame_seq_step {
                0 => self.length_ctr_clocked = true,
                2 => {
                    self.length_ctr_clocked = true;
                    self.sweep_clocked = true;
                }
                4 => self.length_ctr_clocked = true,
                6 => {
                    self.length_ctr_clocked = true;
                    self.sweep_clocked = true;
                }
                7 => self.vol_env_clocked = true,
                _ => (),
            }
        }
    }

    fn reset_clocked(&mut self) {
        self.frame_seq_clocked = false;
        self.length_ctr_clocked = false;
        self.sweep_clocked = false;
        self.vol_env_clocked = false;
        self.pwm_clocked = false;
        self.wave_clocked = false;
        self.noise_clocked = false;
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
}
