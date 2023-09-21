use crate::{
    cartridge::load_cartridge,
    cpu::Cpu,
    mmu::Mmu,
    ppu::Ppu,
    screen::{Headless, Screen, Sdl},
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

    pub fn run(&mut self, headless: bool) {
        if headless {
            self._run(&mut Headless::default());
        } else {
            self._run(&mut Sdl::default());
        };
    }

    pub fn _run(&mut self, screen: &mut impl Screen) {
        loop {
            let start = std::time::Instant::now();

            let mut executed_cycles = 0;
            while executed_cycles <= CYCLES_PER_SEC {
                let (cycles, should_quit) = self.step(screen);
                executed_cycles += cycles;

                if should_quit {
                    return;
                }
            }

            std::thread::sleep(
                std::time::Duration::from_secs(1) - std::time::Instant::now().duration_since(start),
            );
        }
    }

    pub fn step(&mut self, screen: &mut impl Screen) -> (usize, bool) {
        let mut should_quit = false;

        let cycles = self.cpu.tick(&mut self.mmu);
        self.ppu.tick(cycles, &mut self.mmu);

        if self.ppu.should_draw {
            should_quit = screen.poll_events();

            screen.update(self.ppu.buffer());

            self.ppu.should_draw = false;
        }

        (cycles, should_quit)
    }
}
