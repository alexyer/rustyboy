extern crate rustyboy_instruction_derive;

mod cartridge;
mod cpu;
mod errors;
mod frame_buffer;
mod gb;
mod instruction;
mod mmu;
mod ppu;
mod screen;

use std::{fs::File, io::Read};

use gb::GameBoy;

fn main() {
    let boot_rom_path = std::env::var("BOOT_ROM").expect("boot ROM path");
    let rom_path = std::env::var("ROM").expect("ROM path");

    let headless = {
        let headless: usize = std::env::var("HEADLESS")
            .unwrap_or("0".to_string())
            .parse()
            .expect("HEADLESS must be 1 or 0");

        headless != 0
    };

    let boot_rom = {
        let mut boot_rom = Vec::new();

        File::open(boot_rom_path)
            .expect("boot ROM")
            .read_to_end(&mut boot_rom)
            .unwrap();

        boot_rom
    };

    let rom = {
        let mut rom = Vec::new();

        File::open(rom_path)
            .expect("ROM")
            .read_to_end(&mut rom)
            .unwrap();

        rom
    };

    let mut gb = GameBoy::new(&boot_rom, &rom);
    gb.run(headless);
}
