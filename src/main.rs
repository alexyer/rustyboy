extern crate rustyboy_instruction_derive;

mod apu;
mod audio;
mod cartridge;
mod cpu;
mod errors;
mod frame_buffer;
mod gb;
mod input;
mod instruction;
mod mmu;
mod ppu;
mod renderer;
mod timer;
mod utils;

use std::{fs::File, io::Read};

use gb::GameBoy;

fn main() {
    let boot_rom_path = std::env::var("BOOT_ROM").ok();
    let rom_path = std::env::var("ROM").expect("ROM path");

    let headless = {
        std::env::var("HEADLESS")
            .unwrap_or("0".to_string())
            .parse::<usize>()
            .expect("HEADLESS must be 1 or 0")
            != 0
    };

    let disable_spites = {
        std::env::var("DISABLE_SPRITES")
            .unwrap_or("0".to_string())
            .parse::<usize>()
            .expect("HEADLESS must be 1 or 0")
            != 0
    };

    let log_file = std::env::var("LOG_FILE").ok();

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

        File::open(rom_path)
            .expect("ROM")
            .read_to_end(&mut rom)
            .unwrap();

        rom
    };

    let mut gb = GameBoy::new(boot_rom.as_deref(), &rom);
    gb.run(headless, disable_spites, log_file);
}
