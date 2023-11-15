use crate::{check_bit, mmu::Mmu};

use super::{sample::RawSample, NrXxAddress};

pub trait Channel {
    fn new(control_register_address: usize) -> Self;
    fn nrxx(&self) -> &NrXxAddress;
    fn trigger(&mut self, mmu: &mut Mmu);
    fn on(&self) -> bool;
    fn current_sample(&self, mmu: &Mmu) -> RawSample;

    fn tick(
        &mut self,
        mmu: &mut Mmu,
        length_ctr_clocked: bool,
        vol_env_clocked: bool,
        sweep_clocked: bool,
    ) -> ChannelState {
        if self.is_triggered(mmu) {
            self.trigger(mmu);
        }

        if !self.on() {
            return ChannelState {
                on: false,
                dac_on: true,
            };
        }

        let state = self.do_update(mmu, length_ctr_clocked, vol_env_clocked, sweep_clocked);

        self.clear_trigger(mmu);

        state
    }

    fn is_triggered(&self, mmu: &mut Mmu) -> bool {
        mmu.check_bit(self.nrxx().nrx4_address(), 7)
    }

    fn clear_trigger(&self, mmu: &mut Mmu) {
        mmu.set_bit_to(self.nrxx().nrx4_address(), 7, false);
    }

    fn do_update(
        &mut self,
        mmu: &mut Mmu,
        length_ctr_clocked: bool,
        vol_env_clocked: bool,
        sweep_clocked: bool,
    ) -> ChannelState;
}

#[derive(Debug)]
pub struct ChannelState {
    pub on: bool,
    pub dac_on: bool,
}

pub struct Dac<C: Channel> {
    on: bool,
    channel: C,
}

impl<C> Dac<C>
where
    C: Channel,
{
    pub fn new(control_register_address: usize) -> Self {
        Self {
            on: false,
            channel: C::new(control_register_address),
        }
    }

    pub fn tick(
        &mut self,
        mmu: &mut Mmu,
        length_ctr_clocked: bool,
        vol_env_clocked: bool,
        sweep_clocked: bool,
    ) -> ChannelState {
        let state = self
            .channel
            .tick(mmu, length_ctr_clocked, vol_env_clocked, sweep_clocked);

        self.on = state.dac_on;

        state
    }

    pub fn current_sample(&self, mmu: &Mmu) -> RawSample {
        self.channel.current_sample(mmu)
    }
}

pub struct LengthModChannel<C: Channel, const L: u16> {
    on: bool,
    channel: C,

    length_enabled: bool,
    length_timer: u16,

    nrxx: NrXxAddress,
}

impl<C, const L: u16> LengthModChannel<C, L> where C: Channel {}

impl<C, const L: u16> Channel for LengthModChannel<C, L>
where
    C: Channel,
{
    fn new(control_register_address: usize) -> Self {
        Self {
            on: false,
            channel: C::new(control_register_address),
            length_enabled: false,
            length_timer: 0,
            nrxx: NrXxAddress(control_register_address),
        }
    }

    fn nrxx(&self) -> &NrXxAddress {
        &self.nrxx
    }

    fn trigger(&mut self, mmu: &mut Mmu) {
        self.on = true;
        self.length_enabled = mmu.check_bit(self.nrxx().nrx4_address(), 6);

        if !self.length_enabled {
            return;
        }

        self.length_timer = L - (mmu.read_byte(self.nrxx().nrx1_address()) & 0x3f) as u16;
    }

    fn on(&self) -> bool {
        self.on
    }

    fn current_sample(&self, mmu: &Mmu) -> RawSample {
        self.channel.current_sample(mmu)
    }

    fn do_update(
        &mut self,
        mmu: &mut Mmu,
        length_ctr_clocked: bool,
        vol_env_clocked: bool,
        sweep_clocked: bool,
    ) -> ChannelState {
        if self.length_enabled {
            if length_ctr_clocked {
                self.length_timer -= 1;
            }

            if self.length_timer == 0 {
                self.on = false;
            }
        }

        self.channel
            .tick(mmu, length_ctr_clocked, vol_env_clocked, sweep_clocked)
    }
}

pub struct VolumeModChannel<C: Channel> {
    on: bool,
    nrxx: NrXxAddress,

    volume: u8,
    increase: bool,
    sweep_pace: u8,

    sweep_pace_timer: u8,

    channel: C,
}

impl<C> Channel for VolumeModChannel<C>
where
    C: Channel,
{
    fn new(control_register_address: usize) -> Self {
        Self {
            on: false,
            nrxx: NrXxAddress(control_register_address),
            channel: C::new(control_register_address),
            volume: 0,
            increase: false,
            sweep_pace: 0,
            sweep_pace_timer: 0,
        }
    }

    fn nrxx(&self) -> &NrXxAddress {
        &self.nrxx
    }

    fn trigger(&mut self, mmu: &mut Mmu) {
        let nrx2 = mmu.read_byte(self.nrxx().nrx2_address());

        self.on = true;
        self.volume = nrx2 >> 4;
        self.increase = check_bit!(nrx2, 3);
        self.sweep_pace = nrx2 & 0x7;

        self.channel.trigger(mmu);
    }

    fn on(&self) -> bool {
        self.on
    }

    fn current_sample(&self, mmu: &Mmu) -> RawSample {
        self.channel.current_sample(mmu) & self.volume
    }

    fn do_update(
        &mut self,
        mmu: &mut Mmu,
        length_ctr_clocked: bool,
        vol_env_clocked: bool,
        sweep_clocked: bool,
    ) -> ChannelState {
        if self.sweep_pace == 0 {
            return self
                .channel
                .tick(mmu, length_ctr_clocked, vol_env_clocked, sweep_clocked);
        }

        if vol_env_clocked {
            self.sweep_pace_timer = (self.sweep_pace_timer + 1) % self.sweep_pace;

            if self.sweep_pace_timer == 0 {
                if self.increase {
                    self.volume = (self.volume + 1) % 0xf;
                } else {
                    self.volume -= 1;
                }

                if self.volume == 0 {
                    self.on = false;
                }
            }
        }

        self.channel
            .tick(mmu, length_ctr_clocked, vol_env_clocked, sweep_clocked)
    }
}

pub struct SweepModChannel<C: Channel> {
    on: bool,
    nrxx: NrXxAddress,
    channel: C,

    pace: u8,
    decreases: bool,
    step: u8,

    counter: u8,
}

impl<C> Channel for SweepModChannel<C>
where
    C: Channel,
{
    fn new(control_register_address: usize) -> Self {
        Self {
            on: false,
            nrxx: NrXxAddress(control_register_address),
            channel: C::new(control_register_address),
            pace: 0,
            decreases: false,
            step: 0,
            counter: 0,
        }
    }

    fn nrxx(&self) -> &NrXxAddress {
        &self.nrxx
    }

    fn trigger(&mut self, mmu: &mut Mmu) {
        let nrx0 = mmu.read_byte(self.nrxx().nrx0_address());

        self.on = true;
        self.pace = (nrx0 & 0xf0) >> 4;
        self.decreases = check_bit!(nrx0, 3);
        self.step = nrx0 & 0x7;
    }

    fn on(&self) -> bool {
        self.on
    }

    fn current_sample(&self, mmu: &Mmu) -> RawSample {
        self.channel.current_sample(mmu)
    }

    fn do_update(
        &mut self,
        mmu: &mut Mmu,
        length_ctr_clocked: bool,
        vol_env_clocked: bool,
        sweep_clocked: bool,
    ) -> ChannelState {
        if sweep_clocked {
            self.counter += 1;

            if self.counter == 0 {
                return self
                    .channel
                    .tick(mmu, length_ctr_clocked, vol_env_clocked, sweep_clocked);
            }

            if self.counter == self.pace {
                let nrx4 = mmu.read_byte(self.nrxx().nrx4_address()) as u16;
                let period = ((nrx4 & 0x7) << 8) | mmu.read_byte(self.nrxx().nrx3_address()) as u16;

                let shift_freq = period / (1 << self.step);

                let new_period = if self.decreases {
                    period - shift_freq
                } else {
                    let new_period = period + shift_freq;

                    if new_period > 0x7ff {
                        self.on = false;
                        return ChannelState {
                            on: false,
                            dac_on: true,
                        };
                    }

                    new_period
                };

                let nrx4 = nrx4 & !0x7;

                mmu.write_byte(
                    self.nrxx().nrx4_address(),
                    (nrx4 | ((new_period & 0xf0) >> 8)) as u8,
                );

                mmu.write_byte(self.nrxx().nrx3_address(), new_period as u8);
            }
        }

        self.channel
            .tick(mmu, length_ctr_clocked, vol_env_clocked, sweep_clocked)
    }
}
