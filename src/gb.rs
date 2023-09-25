use std::{
    fs::File,
    io::Write,
    time::{Duration, Instant},
};

use crate::{
    cartridge::load_cartridge,
    cpu::{Cpu, Reg, Reg16},
    input::{Button, Input},
    mmu::Mmu,
    ppu::Ppu,
    screen::{Headless, Screen, Sdl},
    timer::Timer,
};

pub const CYCLES_PER_SEC: usize = 4194304;

pub enum GameBoyEvent {
    Quit,
    ButtonPressed(Button),
    ButtonReleased(Button),
}

pub struct GameBoy {
    cpu: Cpu,
    input: Input,
    mmu: Mmu,
    ppu: Ppu,
    timer: Timer,
}

impl GameBoy {
    pub fn new(boot_rom: Option<&[u8]>, rom: &[u8]) -> Self {
        let cartridge = load_cartridge(rom);

        println!("ROM name: {}", cartridge.name());
        println!("Cartridge type: {:?}", cartridge.cartridge_type());
        println!("Ram size: {:?}", cartridge.ram_size());
        println!("CGB flag: {:?}", cartridge.cgb_flag());

        if let Some(boot_rom) = boot_rom {
            Self {
                cpu: Cpu::default(),
                input: Input::default(),
                mmu: Mmu::new(Some(boot_rom.to_vec()), cartridge),
                ppu: Ppu::default(),
                timer: Timer::default(),
            }
        } else {
            let mut gb = Self {
                cpu: Cpu::default(),
                input: Input::default(),
                mmu: Mmu::new(None, cartridge),
                ppu: Ppu::default(),
                timer: Timer::default(),
            };

            gb.cpu.regs.write_reg(Reg::A, 0x01);
            gb.cpu.regs.write_reg(Reg::C, 0x13);
            gb.cpu.regs.write_reg(Reg::E, 0xd8);
            gb.cpu.regs.write_reg(Reg::H, 0x01);
            gb.cpu.regs.write_reg(Reg::L, 0x4d);
            gb.cpu.regs.write_reg16(Reg16::SP, 0xfffe);
            gb.cpu.regs.write_reg16(Reg16::PC, 0x0100);

            gb.cpu.set_c();
            gb.cpu.set_h();
            gb.cpu.set_z();

            gb
        }
    }

    pub fn run(&mut self, headless: bool, debug_disable_sprites: bool, log_file: Option<String>) {
        if headless {
            self._run(&mut Headless, debug_disable_sprites, log_file);
        } else {
            self._run(&mut Sdl::default(), debug_disable_sprites, log_file);
        };
    }

    pub fn _run(
        &mut self,
        screen: &mut impl Screen,
        debug_disable_sprites: bool,
        log_file_path: Option<String>,
    ) {
        let log_file = log_file_path.map(|path| File::create(path).unwrap());

        loop {
            let start = std::time::Instant::now();

            let mut executed_cycles = 0;
            while executed_cycles <= 10000 {
                if let Some(mut log_file) = log_file.as_ref() {
                    log_file
                        .write_all(self.cpu.log_state(&self.mmu).as_bytes())
                        .unwrap();
                }

                let (cycles, should_quit) = self.step(screen, debug_disable_sprites);
                executed_cycles += cycles;

                if should_quit {
                    return;
                }
            }

            let exec_time = Instant::now().duration_since(start);
            let expected_time = Duration::from_nanos(200) * executed_cycles as u32;

            if exec_time < expected_time {
                std::thread::sleep(expected_time - exec_time);
            }
        }
    }

    pub fn step(&mut self, screen: &mut impl Screen, debug_disable_sprites: bool) -> (usize, bool) {
        let mut should_quit = false;

        self.input.tick(&mut self.mmu);
        let cycles = self.cpu.tick(&mut self.mmu);
        self.ppu.tick(cycles, &mut self.mmu, debug_disable_sprites);
        self.timer.tick(cycles, &mut self.mmu);

        if self.ppu.should_draw {
            match screen.poll_events() {
                Some(GameBoyEvent::Quit) => should_quit = true,
                Some(GameBoyEvent::ButtonPressed(button)) => self.input.button_pressed(&button),
                Some(GameBoyEvent::ButtonReleased(button)) => self.input.button_released(&button),
                None => (),
            }

            screen.update(self.ppu.buffer());
            self.ppu.buffer_mut().reset();

            self.ppu.should_draw = false;
        }

        (cycles, should_quit)
    }
}
