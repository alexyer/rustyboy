use crate::mmu::Mmu;

use super::{
    channel::{Channel, ChannelState},
    NrXxAddress,
};

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

impl From<u8> for PwmWaveDuty {
    fn from(value: u8) -> Self {
        match value {
            0x00 => PwmWaveDuty::Duty0,
            0x01 => PwmWaveDuty::Duty1,
            0x02 => PwmWaveDuty::Duty2,
            0x03 => PwmWaveDuty::Duty3,
            _ => panic!("invalid pwm duty: {value}"),
        }
    }
}

pub struct PwmChannel {
    on: bool,
    nrxx: NrXxAddress,

    duty: PwmWaveDuty,
    wave_duty_position: u8,
    freq_timer: u16,
}

impl Channel for PwmChannel {
    fn new(control_register_address: usize) -> Self {
        Self {
            on: false,
            nrxx: NrXxAddress(control_register_address),
            duty: Default::default(),
            wave_duty_position: 0,
            freq_timer: 0,
        }
    }

    fn nrxx(&self) -> &NrXxAddress {
        &self.nrxx
    }

    fn trigger(&mut self, mmu: &mut Mmu) {
        self.on = true;
        self.update_period(mmu);
        self.update_duty(mmu);
    }

    fn on(&self) -> bool {
        self.on
    }

    fn current_sample(&self, _mmu: &Mmu) -> super::sample::RawSample {
        if self.on {
            self.duty.as_slice()[self.wave_duty_position as usize]
        } else {
            0
        }
    }

    fn do_update(
        &mut self,
        mmu: &mut Mmu,
        _length_ctr_clocked: bool,
        _vol_env_clocked: bool,
        _sweep_clocked: bool,
    ) -> ChannelState {
        self.freq_timer -= 1;

        if self.freq_timer == 0 {
            self.wave_duty_position = (self.wave_duty_position + 1) % 8;
            self.update_duty(mmu);
            self.update_period(mmu);
        }

        ChannelState {
            on: self.on(),
            dac_on: true,
        }
    }
}

impl PwmChannel {
    fn update_period(&mut self, mmu: &mut Mmu) {
        let nrx4 = mmu.read_byte(self.nrxx().nrx4_address()) as u16;
        let period = ((nrx4 & 0x7) << 8) | mmu.read_byte(self.nrxx().nrx3_address()) as u16;
        self.freq_timer = 2048 - period;
    }

    fn update_duty(&mut self, mmu: &mut Mmu) {
        self.duty = PwmWaveDuty::from((mmu.read_byte(self.nrxx().nrx1_address()) & 0xc0) >> 6);
    }
}
