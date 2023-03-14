use std::ffi::CString;

use crate::{
    cpu::Cpu,
    mmu::{Mmu, INT_ENABLE_ADDRESS},
    ppu::Ppu,
};

const CYCLES_PER_SEC: usize = 4194304;

pub struct GameBoy {
    cpu: Cpu,
    mmu: Mmu,
    ppu: Ppu,
}

impl GameBoy {
    pub fn new(boot_rom: &[u8], rom: &[u8]) -> Self {
        let mut gb = Self {
            cpu: Cpu::default(),
            mmu: Mmu::default(),
            ppu: Ppu::default(),
        };

        // TODO(alexyer): proper loading
        gb.mmu.write_slice(&rom[..0x4000], 0);
        gb.mmu.write_slice(&boot_rom, 0);

        let name_bytes = gb.mmu.read_slice(0x134, 15);
        let name = name_bytes.split(|c| *c == 0).next().unwrap();

        println!(
            "ROM name: {}",
            CString::new(name).unwrap().into_string().unwrap()
        );

        gb
    }

    pub fn run(&mut self) {
        loop {
            let start = std::time::Instant::now();

            let mut executed_cycles = 0;
            while executed_cycles <= CYCLES_PER_SEC {
                executed_cycles += self.step();
            }
            println!("{}", self.cpu);
            println!("IE: 0b{:08b}", self.mmu.read_byte(INT_ENABLE_ADDRESS));
            std::thread::sleep(
                std::time::Duration::from_secs(1) - std::time::Instant::now().duration_since(start),
            );
        }
    }

    pub fn step(&mut self) -> usize {
        let cycles = self.cpu.tick(&mut self.mmu);
        self.ppu.tick(cycles, &mut self.mmu);

        cycles
    }
}
