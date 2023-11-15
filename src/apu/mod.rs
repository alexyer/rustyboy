mod channel;
mod pwm_channel;
mod sample;
mod wave_channel;

pub use sample::Sample;

use crate::{gb::CYCLES_PER_SEC, mmu::Mmu};

use self::{
    channel::{Dac, LengthModChannel, SweepModChannel, VolumeModChannel},
    pwm_channel::PwmChannel,
    wave_channel::WaveChannel,
};

const NR14_ADDRESS: usize = 0xff14;
const NR24_ADDRESS: usize = 0xff19;
const NR34_ADDRESS: usize = 0xff1e;
const NR44_ADDRESS: usize = 0xff23;
const NR52_ADDRESS: usize = 0xff26;

const CYCLES_PER_512HZ: usize = CYCLES_PER_SEC / 512;

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

// pub struct NoiseChannel {
//     on: bool,
//     nrxx: NrXxAddress,

//     current_sample: Sample,

//     length_timer: u8,
//     lfsr: u16,
//     volume: u8,

//     shift: u8,
//     short: bool,
//     divider: u8,

//     envelope_counter: usize,
//     length_timer_counter: usize,
//     lfsr_counter: usize,
// }

// impl NoiseChannel {
//     pub fn new(nrx4_address: usize) -> Self {
//         Self {
//             on: false,
//             nrxx: NrXxAddress(nrx4_address),
//             current_sample: Default::default(),
//             lfsr: 0x7fff,
//             length_timer: 0,
//             length_timer_counter: 0,
//             envelope_counter: 0,
//             lfsr_counter: 0,
//             volume: 0,
//             shift: 0,
//             short: false,
//             divider: 0,
//         }
//     }

//     pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu) -> ChannelState {
//         // Channel triggered
//         if mmu.check_bit(self.nrxx.nrx4_address(), 7) {
//             self.trigger(mmu);
//         }

//         if !self.on {
//             return ChannelState {
//                 on: self.on,
//                 samples: vec![Sample { value: 0.0 }; cycles],
//             };
//         }

//         let mut samples = vec![];

//         for _ in 0..cycles {
//             self.update_envelope(1, mmu);
//             self.update_lfsr(1, &mmu);

//             samples.push(self.current_sample);

//             // if !self.on {
//             //     samples.push(Sample::from_raw_sample(0));
//             // }
//         }

//         if mmu.check_bit(self.nrxx.nrx4_address(), 6) {
//             self.update_length_timer(cycles);
//         }

//         ChannelState {
//             on: self.on,
//             samples,
//         }
//     }

//     fn update_lfsr(&mut self, cycles: usize, mmu: &Mmu) {
//         let divided_freq = CYCLES_PER_NOISE_DIV_TICK / (self.divider as usize + 1);

//         let freq = divided_freq / (2 << self.shift);

//         self.lfsr_counter += cycles;

//         if self.lfsr_counter >= freq {
//             let nr43 = mmu.read_byte(self.nrxx.nrx3_address());

//             self.shift = (nr43 & 0xf0) >> 4;
//             self.short = check_bit!(nr43, 3);
//             self.divider = nr43 & 0x7;

//             // println!("{} {}", self.shift, self.divider);
//             self.lfsr_counter %= freq;

//             let tap = self.lfsr;
//             self.lfsr >>= 1;

//             let tap = (tap ^ self.lfsr) & 1;

//             let mask = if self.short { 0x4040 } else { 0x4000 };

//             if tap == 0 {
//                 self.lfsr &= !mask;
//                 self.current_sample = Sample::from_raw_sample(0xf & self.volume);
//             } else {
//                 self.current_sample = Sample::from_raw_sample(0);
//                 self.lfsr |= mask;
//             }
//         }
//     }

//     fn trigger(&mut self, mmu: &mut Mmu) {
//         self.on = true;
//         self.lfsr = 0xff;
//         self.length_timer = mmu.read_byte(self.nrxx.nrx1_address()) & 0x3f;
//         // println!(
//         //     "{:x}, {}",
//         //     self.nrxx.nrx1_address(),
//         //     mmu.read_byte(self.nrxx.nrx1_address())
//         // );
//         // println!("{}", mmu.check_bit(self.nrxx.nrx4_address(), 6));

//         self.volume = (mmu.read_byte(self.nrxx.nrx2_address()) & 0xf0) >> 4;
//         self.envelope_counter = 0;

//         let nr43 = mmu.read_byte(self.nrxx.nrx3_address());

//         self.shift = (nr43 & 0xf0) >> 4;
//         self.short = check_bit!(nr43, 3);
//         self.divider = nr43 & 0x7;

//         mmu.set_bit_to(self.nrxx.nrx4_address(), 7, false);
//     }

//     fn update_length_timer(&mut self, cycles: usize) {
//         self.length_timer_counter += cycles;

//         if self.length_timer_counter >= CYCLES_PER_256HZ {
//             self.length_timer_counter %= CYCLES_PER_256HZ;
//             self.length_timer += 1;

//             if self.length_timer == 64 {
//                 self.on = false;
//             }
//         }
//     }

//     fn update_envelope(&mut self, cycles: usize, mmu: &Mmu) {
//         let sweep_pace = (mmu.read_byte(self.nrxx.nrx2_address()) & 0x7) as usize;

//         if sweep_pace == 0 {
//             return;
//         }

//         self.envelope_counter += cycles;

//         if self.volume == 0 {
//             self.on = false;
//             return;
//         }

//         if self.envelope_counter >= sweep_pace * CYCLES_PER_64HZ {
//             self.envelope_counter %= sweep_pace * CYCLES_PER_64HZ;

//             let env_increase = mmu.check_bit(self.nrxx.nrx2_address(), 3);

//             if !env_increase {
//                 self.volume -= 1;
//             } else {
//                 self.volume += 1;
//                 self.volume %= 0xf;
//             }
//         }
//     }
// }

// pub struct WaveChannel {
//     on: bool,
//     dac_on: bool,

//     length_timer_counter: usize,
//     sample_counter: usize,

//     nrxx: NrXxAddress,

//     current_sample: usize,
//     length_timer: u8,
//     period: u16,
//     volume: u8,
// }

pub struct Apu {
    // channel1: PwmChannel,
    // channel2: PwmChannel,
    // channel3: WaveChannel,
    // channel4: NoiseChannel,
    channel1: Dac<LengthModChannel<VolumeModChannel<SweepModChannel<PwmChannel>>, 64>>,
    channel2: Dac<LengthModChannel<VolumeModChannel<PwmChannel>, 64>>,
    channel3: Dac<LengthModChannel<WaveChannel, 256>>,
    channel4: Dac<LengthModChannel<PwmChannel, 64>>,

    frame_seq_step: u8,

    dac_seq_counter: usize,
    frame_seq_counter: usize,

    frame_seq_clocked: bool,
    length_ctr_clocked: bool,
    vol_env_clocked: bool,
    sweep_clocked: bool,
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

            frame_seq_step: 0,

            dac_seq_counter: 0,
            frame_seq_counter: 0,

            frame_seq_clocked: false,
            length_ctr_clocked: false,
            vol_env_clocked: false,
            sweep_clocked: false,
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

    pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu) -> Vec<Sample> {
        if !self.on(mmu) {
            return vec![Default::default(); cycles];
        }

        let mut channel1_samples = vec![];
        let mut channel2_samples = vec![];
        let mut channel3_samples = vec![];

        for _ in 0..cycles {
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

            channel1_samples.push(Sample::from_raw_sample(self.channel1.current_sample(mmu)));
            channel2_samples.push(Sample::from_raw_sample(self.channel2.current_sample(mmu)));
            channel3_samples.push(Sample::from_raw_sample(self.channel3.current_sample(mmu)));

            self.reset_clocked();
        }

        self.mix(
            channel1_samples,
            channel2_samples,
            channel3_samples,
            // channel4_state.samples,
        )
    }

    fn clock_timers(&mut self) {
        self.clock_frame_seq();
        self.clock_dac_seq()
    }

    fn clock_dac_seq(&mut self) {
        self.dac_seq_counter = (self.dac_seq_counter + 1) % 4;

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
    }

    pub fn mix(
        &self,
        channel1: Vec<Sample>,
        channel2: Vec<Sample>,
        channel3: Vec<Sample>,
        // channel4: Vec<Sample>,
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
}
