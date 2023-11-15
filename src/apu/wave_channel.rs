use crate::mmu::Mmu;

use super::{
    channel::{Channel, ChannelState},
    sample::RawSample,
    NrXxAddress,
};

const WAVE_RAM_ADDRESS: usize = 0xff30;
const WAVE_RAM_SIZE: u8 = 0x10;
const WAVE_SAMPLES_PER_BYTE: u8 = 2;
const WAVE_SAMPLES_COUNT: u8 = WAVE_RAM_SIZE * WAVE_SAMPLES_PER_BYTE;

pub struct WaveChannel {
    on: bool,
    nrxx: NrXxAddress,

    sample: u8,
    freq_timer: u16,
    volume: u8,
}

impl Channel for WaveChannel {
    fn new(control_register_address: usize) -> Self {
        Self {
            on: false,
            nrxx: NrXxAddress(control_register_address),
            sample: 0,
            freq_timer: 0,
            volume: 0,
        }
    }

    fn nrxx(&self) -> &NrXxAddress {
        &self.nrxx
    }

    fn trigger(&mut self, mmu: &mut Mmu) {
        self.on = true;
        self.update_period(mmu);
        self.sample = 1;
        self.volume = mmu.read_byte(self.nrxx().nrx2_address()) >> 5;
    }

    fn on(&self) -> bool {
        self.on
    }

    fn current_sample(&self, mmu: &Mmu) -> RawSample {
        let sample_byte_index = self.sample / WAVE_SAMPLES_PER_BYTE;
        let shift = self.sample % WAVE_SAMPLES_PER_BYTE;

        let sample_byte = mmu.read_byte(WAVE_RAM_ADDRESS + sample_byte_index as usize);
        let raw_sample = (sample_byte >> shift) & !0xfc;

        match self.volume {
            0x0 => 0,
            0x1 => raw_sample,
            0x2 => raw_sample >> 1,
            0x3 => raw_sample >> 2,
            _ => unreachable!(),
        }
    }

    fn do_update(
        &mut self,
        mmu: &mut crate::mmu::Mmu,
        _length_ctr_clocked: bool,
        _vol_env_clocked: bool,
        _sweep_clocked: bool,
    ) -> super::channel::ChannelState {
        self.freq_timer -= 1;

        if self.freq_timer == 0 {
            self.sample = (self.sample + 1) % WAVE_SAMPLES_COUNT;
            self.update_period(mmu);
        }

        ChannelState {
            on: self.on(),
            dac_on: true,
        }
    }
}

impl WaveChannel {
    fn update_period(&mut self, mmu: &mut Mmu) {
        let nrx4 = mmu.read_byte(self.nrxx().nrx4_address()) as u16;
        let period = ((nrx4 & 0x7) << 8) | mmu.read_byte(self.nrxx().nrx3_address()) as u16;
        self.freq_timer = 2048 - period;
    }
}
