use crate::mmu::Mmu;

use super::{
    channel::{Channel, ChannelState},
    NrXxAddress,
};

pub struct NoiseChannel {
    on: bool,
    nrxx: NrXxAddress,

    lfsr: u16,

    freq: u16,
    short: bool,
}

impl Channel for NoiseChannel {
    fn new(control_register_address: usize) -> Self {
        Self {
            on: false,
            nrxx: NrXxAddress(control_register_address),
            lfsr: 0,
            freq: 0,
            short: false,
        }
    }

    fn nrxx(&self) -> &NrXxAddress {
        &self.nrxx
    }

    fn trigger(&mut self, mmu: &mut Mmu) {
        self.on = true;

        self.lfsr = 0x7fff;

        let shift = self.get_shift(mmu);
        self.short = mmu.check_bit(self.nrxx().nrx3_address(), 3);

        let divisor = self.get_divisor(mmu);

        self.freq = divisor << shift;
    }

    fn on(&self) -> bool {
        self.on
    }

    fn current_sample(&self, _mmu: &Mmu) -> super::sample::RawSample {
        0xf * (!self.lfsr & 0x1) as u8
    }

    fn do_update(
        &mut self,
        mmu: &mut crate::mmu::Mmu,
        _length_ctr_clocked: bool,
        _vol_env_clocked: bool,
        _sweep_clocked: bool,
    ) -> ChannelState {
        self.freq -= 1;

        if self.freq == 0 {
            let divisor = self.get_divisor(mmu);
            let shift = self.get_shift(mmu);

            self.freq = divisor << shift;

            let xor = (self.lfsr & 1) ^ ((self.lfsr & 2) >> 1);
            self.lfsr = (self.lfsr >> 1) | (xor << 14);

            if self.short {
                self.lfsr &= !(1 << 6);
                self.lfsr |= xor << 6;
            }
        }

        ChannelState {
            on: self.on(),
            dac_on: true,
        }
    }
}

impl NoiseChannel {
    fn get_divisor(&self, mmu: &Mmu) -> u16 {
        let divider = mmu.read_byte(self.nrxx().nrx3_address()) & 0x7;
        [8, 16, 32, 48, 64, 80, 96, 112][divider as usize]
    }

    fn get_shift(&self, mmu: &Mmu) -> u16 {
        (mmu.read_byte(self.nrxx().nrx3_address()) >> 4) as u16
    }
}
