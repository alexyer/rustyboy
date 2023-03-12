use crate::{cpu::Cpu, mmu::Mmu};

const CYCLES_PER_SEC: usize = 4194304;

pub struct GameBoy {
    cpu: Cpu,
    mmu: Mmu,
}

impl GameBoy {
    pub fn new(boot_rom: &[u8], rom: &[u8]) -> Self {
        let mut gb = Self {
            cpu: Cpu::default(),
            mmu: Mmu::default(),
        };

        // TODO(alexyer): proper loading
        gb.mmu.write_slice(&rom[..0x4000], 0);
        gb.mmu.write_slice(&boot_rom, 0);

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

            std::thread::sleep(
                std::time::Duration::from_secs(1) - std::time::Instant::now().duration_since(start),
            );
        }
    }

    pub fn step(&mut self) -> usize {
        self.cpu.exec_instruction(&mut self.mmu)
    }
}
