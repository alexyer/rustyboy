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

use gb::GameBoy;

fn main() {
    let boot_rom_path = std::env::var("BOOT_ROM").ok();
    let rom_path = std::env::var("ROM").expect("ROM path");
    let ram_dir = std::env::var("RAM_DIR").ok();

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

    let mut gb = GameBoy::new(boot_rom_path, rom_path, ram_dir);
    gb.run(headless, disable_spites, log_file);
}
