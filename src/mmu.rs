use std::vec;

use crate::cartridge::{Cartridge, EmptyCartridge};

const KIB_8: usize = 8 * 1024;

pub const INT_FLAG_ADDRESS: usize = 0xff0f;
pub const INT_ENABLE_ADDRESS: usize = 0xffff;

/// Memory management unit.
pub struct Mmu {
    /// I/O Registers
    io: Vec<u8>,

    /// 8 KiB Video RAM
    vram: Vec<u8>,

    /// 4 KiB Work RAM
    wram: Vec<u8>,

    /// High RAM
    hram: Vec<u8>,

    /// Sprite attribute table
    oam: Vec<u8>,

    /// Interrupt enable
    ie: u8,

    boot_rom: Option<Vec<u8>>,
    cartridge: Box<dyn Cartridge>,
}

impl Default for Mmu {
    fn default() -> Self {
        Self {
            io: vec![0; 128],
            vram: vec![0; KIB_8],
            wram: vec![0; KIB_8],
            hram: vec![0; 127],
            oam: vec![0; 160],
            ie: 0,
            boot_rom: None,
            cartridge: Box::new(EmptyCartridge::default()),
        }
    }
}

impl Mmu {
    pub fn new(boot_rom: Option<Vec<u8>>, cartridge: Box<dyn Cartridge>) -> Self {
        Self {
            boot_rom,
            cartridge,
            ..Default::default()
        }
    }

    pub fn write_slice(&mut self, data: &[u8], addr: usize) {
        for (i, &data) in data.iter().enumerate() {
            self.write_byte(addr + i, data)
        }
    }

    pub fn read_slice(&self, addr: usize, len: usize) -> Vec<u8> {
        let mut data = Vec::with_capacity(len);

        for i in 0..len {
            data.push(self.read_byte(addr + i));
        }

        data
    }

    pub fn write_byte(&mut self, addr: usize, data: u8) {
        match addr {
            addr if addr < 0x8000 => self.cartridge_write_byte(addr, data),
            addr if addr >= 0x8000 && addr < 0xa000 => self.vram_write_byte(addr - 0x8000, data),
            addr if addr >= 0xa000 && addr < 0xc000 => self.cartridge_write_byte(addr, data),
            addr if addr >= 0xc000 && addr < 0xe000 => self.wram_write_byte(addr - 0xc000, data),
            addr if addr >= 0xe000 && addr < 0xfe00 => self.wram_write_byte(addr - 0xe000, data),
            addr if addr >= 0xfe00 && addr < 0xfea0 => self.oam_write_byte(addr - 0xfe00, data),
            addr if addr >= 0xff00 && addr < 0xff80 => self.io_write_byte(addr - 0xff00, data),
            addr if addr >= 0xff80 && addr < 0xffff => self.hram_write_byte(addr - 0xff80, data),
            addr if addr == 0xffff => self.ie_write(data),
            _ => panic!("invalid memory write: {:X}", addr),
        }
    }

    pub fn read_byte(&self, addr: usize) -> u8 {
        match addr {
            addr if addr < 0x8000 => self.cartridge_read_byte(addr),
            addr if addr >= 0x8000 && addr < 0xa000 => self.vram_read_byte(addr - 0x8000),
            addr if addr >= 0xa000 && addr < 0xc000 => self.cartridge_read_byte(addr),
            addr if addr >= 0xc000 && addr < 0xe000 => self.wram_read_byte(addr - 0xc000),
            addr if addr >= 0xe000 && addr < 0xfe00 => self.wram_read_byte(addr - 0xe000),
            addr if addr >= 0xfe00 && addr < 0xfea0 => self.oam_read_byte(addr - 0xfe00),
            addr if addr >= 0xff00 && addr < 0xff80 => self.io_read_byte(addr - 0xff00),
            addr if addr >= 0xff80 && addr < 0xffff => self.hram_read_byte(addr - 0xff80),
            addr if addr == 0xffff => self.ie_read(),
            _ => panic!("invalid memory read: {:X}", addr),
        }
    }

    /// Check bit of the byte located at `addr` in memory.
    pub fn check_bit(&self, addr: usize, bit: usize) -> bool {
        if bit > 7 {
            panic!("invalid bit");
        }

        self.read_byte(addr) & 1 << bit != 0
    }

    pub fn set_bit_to(&mut self, addr: usize, bit: usize, value: bool) {
        if bit > 7 {
            panic!("invalid bit");
        }

        if value {
            self.write_byte(addr, self.read_byte(addr) | 1 << bit);
        } else {
            self.write_byte(addr, self.read_byte(addr) & !(1 << bit));
        }
    }

    pub fn inc(&mut self, addr: usize) {
        self.write_byte(addr, self.read_byte(addr).wrapping_add(1));
    }

    fn cartridge_write_byte(&mut self, addr: usize, data: u8) {
        self.cartridge.write(addr, data)
    }

    fn cartridge_read_byte(&self, addr: usize) -> u8 {
        if addr <= 0xff && self.boot_rom.is_some() {
            return self.boot_rom.clone().unwrap()[addr];
        }

        self.cartridge.read(addr)
    }

    fn io_write_byte(&mut self, addr: usize, data: u8) {
        if addr == 0x01 || addr == 0x02 {
            print!("{}", data as char);
        }

        match addr {
            // TODO(alexyer): Remove later. Currently it's used to log test rom output.
            addr if addr <= 0x02 => print!("{}", data as char),
            // DIV
            0x04 => self.io[addr] = data,
            // TIMA
            0x05 => self.io[addr] = data,
            // TMA
            0x06 => self.io[addr] = data,
            // TAC
            0x07 => self.io[addr] = data,
            // TODO(alexyer): implement sound.
            addr if addr >= 0x10 && addr <= 0x26 => (),
            // LCDC
            0x40 => self.io[addr] = data,
            // STAT
            0x41 => self.io[addr] = data,
            // SCY
            0x42 => self.io[addr] = data,
            // SCX
            0x43 => self.io[addr] = data,
            // LY
            0x44 => self.io[addr] = data,
            // LYC
            0x45 => self.io[addr] = data,
            // DMA
            0x46 => self.dma_transfer(data),
            // BGP
            0x47 => self.io[addr] = data,
            // OBP0
            0x48 => self.io[addr] = data,
            // OBP1
            0x49 => self.io[addr] = data,
            // WY
            0x4a => self.io[addr] = data,
            // WX
            0x4b => self.io[addr] = data,
            // TODO(alexyer): Speed switch
            // KEY1
            0x4d => self.io[addr] = data,

            // ???
            0x50 => self.io[addr] = data,

            //IF
            0x0f => self.io[addr] = data,
            _ => panic!("unsupported IO write: 0x{addr:x}"),
        }
    }

    fn io_read_byte(&self, addr: usize) -> u8 {
        match addr {
            // DIV
            0x04 => self.io[addr],
            // TIMA
            0x05 => self.io[addr],
            // TMA
            0x06 => self.io[addr],
            // TAC
            0x07 => self.io[addr],
            // TODO(alexyer): implement sound.
            addr if addr >= 0x10 && addr <= 0x26 => self.io[addr],
            // LCDC
            0x40 => self.io[addr],
            // STAT
            0x41 => self.io[addr],
            // SCY
            0x42 => self.io[addr],
            // SCX
            0x43 => self.io[addr],
            // LY
            0x44 => self.io[addr],
            // LYC
            0x45 => self.io[addr],
            0x46 => 0xff,
            // BGP
            0x47 => self.io[addr],
            // OBP0
            0x48 => self.io[addr],
            // OBP1
            0x49 => self.io[addr],
            // WY
            0x4a => self.io[addr],
            // WX
            0x4b => self.io[addr],
            // TODO(alexyer): speed switch
            // KEY1
            0x4d => self.io[addr],
            // IF
            0x0f => self.io[addr],
            _ => panic!("unsupported IO read: 0x{addr:x}"),
        }
    }

    fn vram_write_byte(&mut self, addr: usize, data: u8) {
        self.vram[addr] = data
    }

    fn vram_read_byte(&self, addr: usize) -> u8 {
        self.vram[addr]
    }

    fn wram_write_byte(&mut self, addr: usize, data: u8) {
        self.wram[addr] = data
    }

    fn wram_read_byte(&self, addr: usize) -> u8 {
        self.wram[addr]
    }

    fn hram_write_byte(&mut self, addr: usize, data: u8) {
        self.hram[addr] = data
    }

    fn hram_read_byte(&self, addr: usize) -> u8 {
        self.hram[addr]
    }

    fn oam_write_byte(&mut self, addr: usize, data: u8) {
        self.oam[addr] = data
    }

    fn oam_read_byte(&self, addr: usize) -> u8 {
        self.oam[addr]
    }

    fn ie_write(&mut self, data: u8) {
        self.ie = data
    }

    fn ie_read(&self) -> u8 {
        self.ie
    }

    fn dma_transfer(&mut self, addr: u8) {
        let addr = addr as usize * 0x100;
        let data = self.read_slice(addr, 0xa0);

        self.oam.copy_from_slice(&data);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_slice() {
        let mut mmu = Mmu::default();

        assert_eq!(mmu.cartridge.read(42), 0);

        mmu.write_slice(&vec![1; 1], 42);

        assert_eq!(mmu.cartridge.read(42), 1);
    }

    #[test]
    #[should_panic]
    fn test_write_slice_panic() {
        let mut mmu = Mmu::default();
        mmu.write_slice(&vec![1; 20], 0xfea0);
    }

    #[test]
    fn test_read_byte_from_bank00() {
        let mut mmu = Mmu::default();
        mmu.write_slice(&[42], 0);

        assert_eq!(mmu.read_byte(0), 42);
    }

    #[test]
    fn test_read_byte_from_dma() {
        let mmu = Mmu::default();

        assert_eq!(mmu.read_byte(0xff46), 0xff);
    }

    #[test]
    fn test_write_to_dma() {
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xde, 0xad, 0xbe, 0xef], 0);

        assert_eq!(mmu.read_byte(0xfe00), 0);

        mmu.write_byte(0xff46, 0);

        assert_eq!(mmu.read_byte(0xfe00), 0xde);
    }
}
