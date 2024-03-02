use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use crate::{
    apu::Apu,
    audio::{Audio, NoAudio, Rodio},
    cartridge::load_cartridge,
    cpu::{Cpu, Reg, Reg16},
    input::{Button, Input},
    mmu::Mmu,
    ppu::Ppu,
    renderer::{Headless, Renderer, Sdl},
    timer::Timer,
};

pub const CYCLES_PER_SEC: usize = 4194304;

pub enum GameBoyEvent {
    Quit,
    ButtonPressed(Button),
    ButtonReleased(Button),
}

pub struct GameBoy {
    apu: Apu,
    cpu: Cpu,
    input: Input,
    mmu: Mmu,
    ppu: Ppu,
    timer: Timer,
}

impl GameBoy {
    pub fn new(boot_rom_path: Option<String>, rom_path: String, ram_dir: Option<String>) -> Self {
        let boot_rom = if let Some(boot_rom_path) = boot_rom_path {
            let mut boot_rom = Vec::new();

            File::open(boot_rom_path)
                .expect("boot ROM")
                .read_to_end(&mut boot_rom)
                .unwrap();

            Some(boot_rom)
        } else {
            None
        };

        let rom = {
            let mut rom = Vec::new();

            File::open(&rom_path)
                .expect("ROM")
                .read_to_end(&mut rom)
                .unwrap();

            rom
        };

        let cartridge = load_cartridge(&rom, Path::new(&rom_path), ram_dir.as_ref().map(Path::new));

        println!("ROM name: {}", cartridge.name());
        println!("Cartridge type: {:?}", cartridge.cartridge_type());
        println!("Ram size: {:?}", cartridge.ram_size());
        println!("CGB flag: {:?}", cartridge.cgb_flag());

        if let Some(boot_rom) = boot_rom {
            Self {
                apu: Apu::default(),
                cpu: Cpu::default(),
                input: Input::default(),
                mmu: Mmu::new(Some(boot_rom.to_vec()), cartridge),
                ppu: Ppu::default(),
                timer: Timer::default(),
            }
        } else {
            let mut gb = Self {
                apu: Apu::default(),
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
            self._run(&mut Headless, &mut NoAudio, debug_disable_sprites, log_file);
        } else {
            self._run(
                &mut Sdl::default(),
                &mut Rodio::new(),
                debug_disable_sprites,
                log_file,
            );
        };
    }

    pub fn _run(
        &mut self,
        renderer: &mut impl Renderer,
        audio: &mut impl Audio,
        debug_disable_sprites: bool,
        log_file_path: Option<String>,
    ) {
        let log_file = log_file_path.map(|path| File::create(path).unwrap());

        loop {
            if audio.len() > 1500 {
                continue;
            }

            if let Some(mut log_file) = log_file.as_ref() {
                log_file
                    .write_all(self.cpu.log_state(&self.mmu).as_bytes())
                    .unwrap();
            }

            let (_, should_quit) = self.step(renderer, audio, debug_disable_sprites);

            if should_quit {
                return;
            }
        }
    }

    pub fn step(
        &mut self,
        renderer: &mut impl Renderer,
        audio: &mut impl Audio,
        debug_disable_sprites: bool,
    ) -> (usize, bool) {
        let mut should_quit = false;

        self.input.tick(&mut self.mmu);
        let cycles = self.cpu.tick(&mut self.mmu);
        self.ppu.tick(cycles, &mut self.mmu, debug_disable_sprites);
        self.timer.tick(cycles, &mut self.mmu);
        self.apu.tick(cycles, &mut self.mmu);

        if self.apu.sample_ready {
            audio.update(vec![self.apu.get_sample(&self.mmu)]);
            self.apu.sample_ready = false;
        }

        if self.ppu.should_draw {
            match renderer.poll_events() {
                Some(GameBoyEvent::Quit) => should_quit = true,
                Some(GameBoyEvent::ButtonPressed(button)) => self.input.button_pressed(&button),
                Some(GameBoyEvent::ButtonReleased(button)) => self.input.button_released(&button),
                None => (),
            }

            renderer.update(self.ppu.buffer());
            self.ppu.buffer_mut().reset();

            self.ppu.should_draw = false;
        }

        (cycles, should_quit)
    }
}
