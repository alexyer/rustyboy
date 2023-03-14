use std::vec;

const KIB_16: usize = 16 * 1024;
const KIB_8: usize = 8 * 1024;
const KIB_4: usize = 4 * 1024;

pub const INT_FLAG_ADDRESS: usize = 0xff0f;
pub const INT_ENABLE_ADDRESS: usize = 0xffff;

/// Memory management unit.
pub struct Mmu {
    /// 16 KiB ROM bank 00.
    /// From cartridge, usually a fixed bank.
    bank00: Vec<u8>,

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
}

impl Default for Mmu {
    fn default() -> Self {
        Self {
            bank00: vec![0; KIB_16],
            io: vec![0; 128],
            vram: vec![0; KIB_8],
            wram: vec![0; KIB_4],
            hram: vec![0; 127],
            oam: vec![0; 160],
            ie: 0,
        }
    }
}

impl Mmu {
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
            addr if addr < 0x4000 => self.bank00_write_byte(addr, data),
            addr if addr >= 0x8000 && addr < 0xa000 => self.vram_write_byte(addr - 0x8000, data),
            addr if addr >= 0xc000 && addr < 0xd000 => self.wram_write_byte(addr - 0xc000, data),
            addr if addr >= 0xfe00 && addr < 0xfea0 => self.oam_write_byte(addr - 0xfe00, data),
            addr if addr >= 0xff00 && addr < 0xff80 => self.io_write_byte(addr - 0xff00, data),
            addr if addr >= 0xff80 && addr < 0xffff => self.hram_write_byte(addr - 0xff80, data),
            addr if addr == 0xffff => self.ie_write(data),
            _ => panic!("invalid memory access: {:X}", addr),
        }
    }

    pub fn read_byte(&self, addr: usize) -> u8 {
        match addr {
            addr if addr < 0x4000 => self.bank00_read_byte(addr),
            addr if addr >= 0x8000 && addr < 0xa000 => self.vram_read_byte(addr - 0x8000),
            addr if addr >= 0xc000 && addr < 0xd000 => self.wram_read_byte(addr - 0xc000),
            addr if addr >= 0xfe00 && addr < 0xfea0 => self.oam_read_byte(addr - 0xfe00),
            addr if addr >= 0xff00 && addr < 0xff80 => self.io_read_byte(addr - 0xff00),
            addr if addr >= 0xff80 && addr < 0xffff => self.hram_read_byte(addr - 0xff80),
            addr if addr == 0xffff => self.ie_read(),
            _ => panic!("invalid memory access: {:X}", addr),
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

    fn bank00_write_byte(&mut self, addr: usize, data: u8) {
        self.bank00[addr] = data
    }

    fn bank00_read_byte(&self, addr: usize) -> u8 {
        self.bank00[addr]
    }

    fn io_write_byte(&mut self, addr: usize, data: u8) {
        self.io[addr] = data
    }

    fn io_read_byte(&self, addr: usize) -> u8 {
        self.io[addr]
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_slice() {
        let mut mmu = Mmu::default();

        assert_eq!(mmu.bank00[42], 0);

        mmu.write_slice(&vec![1; 1], 42);

        assert_eq!(mmu.bank00[42], 1);
    }

    #[test]
    #[should_panic]
    fn test_write_slice_panic() {
        let mut mmu = Mmu::default();
        mmu.write_slice(&vec![1; 20], KIB_16 - 10);
    }

    #[test]
    fn test_read_byte_from_bank00() {
        let mut mmu = Mmu::default();
        mmu.write_slice(&[42], 0);

        assert_eq!(mmu.read_byte(0), 42);
    }
}
