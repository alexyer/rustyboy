use std::ffi::CString;

#[derive(Debug)]
pub enum CartridgeType {
    RomOnly,
    Mbc1,
}

impl From<u8> for CartridgeType {
    fn from(value: u8) -> Self {
        match value {
            0x00 => CartridgeType::RomOnly,
            0x01 => CartridgeType::Mbc1,
            _ => panic!("unrecognized cartridge type: 0x{value:x}"),
        }
    }
}

pub trait Cartridge {
    fn read(&self, addr: usize) -> u8;
    fn write(&mut self, addr: usize, value: u8);

    fn name(&self) -> String;
    fn cartridge_type(&self) -> CartridgeType;
}

pub struct EmptyCartridge(Vec<u8>);

impl Default for EmptyCartridge {
    fn default() -> Self {
        Self(vec![0; 0x8000])
    }
}

impl Cartridge for EmptyCartridge {
    fn read(&self, addr: usize) -> u8 {
        self.0[addr]
    }

    fn write(&mut self, addr: usize, value: u8) {
        self.0[addr] = value;
    }
    fn name(&self) -> String {
        let name_bytes = &self.0[0x134..0x134 + 15];
        let name = name_bytes.split(|c| *c == 0).next().unwrap();

        CString::new(name).unwrap().into_string().unwrap()
    }

    fn cartridge_type(&self) -> CartridgeType {
        unimplemented!()
    }
}

pub struct Mbc1 {
    buffer: Vec<u8>,
    ram_enabled: bool,
    rom_bank: u16,
}

impl Mbc1 {
    pub fn new(rom: &[u8]) -> Self {
        Self {
            buffer: rom.to_vec(),
            ram_enabled: false,
            rom_bank: 0x01,
        }
    }
}

impl Cartridge for Mbc1 {
    fn read(&self, addr: usize) -> u8 {
        match addr {
            addr if addr < 0x4000 => self.buffer[addr],
            addr if addr >= 0x4000 && addr <= 0x7fff => {
                let address_into_bank = addr - 0x4000;
                let bank_offset = 0x4000 * self.rom_bank as usize;

                self.buffer[bank_offset + address_into_bank]
            }
            _ => panic!("invalid MBC1 memory read: 0x{addr:x}"),
        }
    }

    fn write(&mut self, addr: usize, data: u8) {
        match addr {
            addr if addr < 0x2000 => self.ram_enabled = data & 0xff == 0xa,
            addr if addr >= 0x2000 && addr < 0x4000 => match data {
                0x00 => self.rom_bank = 0x01,
                0x20 => self.rom_bank = 0x21,
                0x40 => self.rom_bank = 0x41,
                0x60 => self.rom_bank = 0x61,
                _ => self.rom_bank = data as u16 & 0x1f,
            },
            _ => panic!("invalid MBC1 memory write: 0x{addr:x}"),
        }
    }

    fn name(&self) -> String {
        let name_bytes = &self.buffer[0x134..0x134 + 15];
        let name = name_bytes.split(|c| *c == 0).next().unwrap();

        CString::new(name).unwrap().into_string().unwrap()
    }

    fn cartridge_type(&self) -> CartridgeType {
        CartridgeType::from(self.buffer[0x147])
    }
}

pub fn load_cartridge(rom: &[u8]) -> Box<dyn Cartridge> {
    match CartridgeType::from(rom[0x147]) {
        CartridgeType::RomOnly => todo!(),
        CartridgeType::Mbc1 => Box::new(Mbc1::new(rom)),
    }
}

// pub fn empty_cartridge() -> Bo
