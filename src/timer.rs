use crate::{
    check_bit,
    gb::CYCLES_PER_SEC,
    mmu::{Mmu, INT_FLAG_ADDRESS},
};

const T_CYCLES_PER_DIV: usize = 256;
const DIV_ADDRESS: usize = 0xff04;
const TIMA_ADDRESS: usize = 0xff05;
const TMA_ADDRESS: usize = 0xff06;
const TAC_ADDRESS: usize = 0xff07;

#[derive(Debug, Default)]
enum Divider {
    #[default]
    By1024,
    By16,
    By64,
    By256,
}

impl From<u8> for Divider {
    fn from(value: u8) -> Self {
        match value {
            0 => Divider::By1024,
            1 => Divider::By16,
            2 => Divider::By64,
            3 => Divider::By256,
            _ => panic!("invalid input clock select: {value}"),
        }
    }
}

#[derive(Debug, Default)]
struct Tac {
    timer_enabled: bool,
    divider: Divider,
}

impl Tac {
    pub fn divider_value(&self) -> usize {
        match self.divider {
            Divider::By1024 => 1024,
            Divider::By16 => 16,
            Divider::By64 => 64,
            Divider::By256 => 256,
        }
    }
}

impl From<u8> for Tac {
    fn from(value: u8) -> Self {
        Self {
            timer_enabled: check_bit!(value, 2),
            divider: Divider::from(value & 3),
        }
    }
}

#[derive(Default)]
pub struct Timer {
    div: u8,
    div_cycle_counter: usize,
    tima_cycle_counter: usize,
    stopped: bool,
    tac: Tac,
}

impl Timer {
    pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu) {
        self.tick_div(cycles, mmu);
        self.tick_tima(cycles, mmu);
    }

    fn tick_tima(&mut self, cycles: usize, mmu: &mut Mmu) {
        self.tac = Tac::from(mmu.read_byte(TAC_ADDRESS));

        if !self.tac.timer_enabled {
            return;
        }

        self.tima_cycle_counter += cycles;

        if self.tima_cycle_counter >= CYCLES_PER_SEC / self.tac.divider_value() {
            self.tima_cycle_counter %= CYCLES_PER_SEC / self.tac.divider_value();
            let (res, overflow) = mmu.read_byte(TIMA_ADDRESS).overflowing_add(1);

            if overflow {
                let tma = self.get_tma(mmu);
                mmu.write_byte(TIMA_ADDRESS, tma);
                mmu.set_bit_to(INT_FLAG_ADDRESS, 2, true);
            } else {
                mmu.write_byte(TIMA_ADDRESS, res);
            }

            println!("{}", mmu.read_byte(TIMA_ADDRESS));
        }
    }

    fn tick_div(&mut self, cycles: usize, mmu: &mut Mmu) {
        if self.stopped {
            return;
        }

        if self.div != mmu.read_byte(DIV_ADDRESS) {
            self.div = 0;
            mmu.write_byte(DIV_ADDRESS, self.div);
        }

        self.div_cycle_counter += cycles;

        if self.div_cycle_counter >= T_CYCLES_PER_DIV {
            self.div_cycle_counter %= T_CYCLES_PER_DIV;
            self.div = self.div.wrapping_add(1);

            mmu.write_byte(DIV_ADDRESS, self.div);
        }
    }

    fn get_tma(&self, mmu: &Mmu) -> u8 {
        mmu.read_byte(TMA_ADDRESS)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tick_div() {
        let mut mmu = Mmu::default();
        let mut timer = Timer::default();

        timer.tick(257, &mut mmu);

        assert_eq!(timer.div, 1);
        assert_eq!(mmu.read_byte(DIV_ADDRESS), 1);

        mmu.write_byte(DIV_ADDRESS, 3);
        timer.tick(10, &mut mmu);

        assert_eq!(timer.div, 0);
        assert_eq!(mmu.read_byte(DIV_ADDRESS), 0);
    }

    #[test]
    fn test_tma() {
        let mut mmu = Mmu::default();
        let timer = Timer::default();

        mmu.write_byte(0xff06, 42);

        assert_eq!(timer.get_tma(&mmu), 42);
    }

    #[test]
    fn test_tick_tima() {
        let mut mmu = Mmu::default();
        let mut timer = Timer::default();
        mmu.write_byte(TAC_ADDRESS, 6);
        mmu.write_byte(TMA_ADDRESS, 0xde);

        for _ in 0..256 {
            timer.tick(CYCLES_PER_SEC / 64, &mut mmu);
        }

        assert_eq!(mmu.read_byte(TIMA_ADDRESS), 0xde);
        assert!(mmu.check_bit(INT_FLAG_ADDRESS, 2));
    }
}
