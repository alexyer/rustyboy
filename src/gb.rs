use crate::{
    cartridge::load_cartridge,
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
        let cartridge = load_cartridge(rom);

        println!("ROM name: {}", cartridge.name());
        println!("Cartridge type: {:?}", cartridge.cartridge_type());

        let gb = Self {
            cpu: Cpu::default(),
            mmu: Mmu::new(Some(boot_rom.to_vec()), cartridge),
            ppu: Ppu::default(),
        };

        gb
    }

    pub fn run(&mut self) {
        loop {
            let start = std::time::Instant::now();

            let mut executed_cycles = 0;
            while executed_cycles <= CYCLES_PER_SEC {
                executed_cycles += self.step();
            }
            // println!("{}", self.cpu);
            // println!("IE: 0b{:08b}", self.mmu.read_byte(INT_ENABLE_ADDRESS));
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
