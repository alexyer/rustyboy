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
            0x03 => CartridgeType::Mbc1,
            _ => panic!("unrecognized cartridge type: 0x{value:x}"),
        }
    }
}

#[derive(Debug)]
pub enum RamSize {
    NoRam,
    Unused,
    Kb8,
    Kb32,
    Kb128,
    Kb64,
}

impl RamSize {
    pub fn len(&self) -> usize {
        match self {
            RamSize::NoRam => 0,
            RamSize::Unused => 0,
            RamSize::Kb8 => 8 * 1024,
            RamSize::Kb32 => 32 * 1024,
            RamSize::Kb128 => 128 * 1024,
            RamSize::Kb64 => 64,
        }
    }
}

impl From<u8> for RamSize {
    fn from(value: u8) -> Self {
        match value {
            0x00 => RamSize::NoRam,
            0x01 => RamSize::Unused,
            0x02 => RamSize::Kb8,
            0x03 => RamSize::Kb32,
            0x04 => RamSize::Kb128,
            0x05 => RamSize::Kb64,
            _ => panic!("unrecognized RAM size: 0x{value:x}"),
        }
    }
}

#[derive(Debug)]
pub enum CgbFlag {
    GbCompatible,
    CgbOnly,
}

impl From<u8> for CgbFlag {
    fn from(value: u8) -> Self {
        match value {
            0x00 => CgbFlag::GbCompatible,
            0x80 => CgbFlag::GbCompatible,
            0xc0 => CgbFlag::CgbOnly,
            _ => panic!("unrecognized CGB flag: 0x{value:x}"),
        }
    }
}

pub trait Cartridge {
    fn read(&self, addr: usize) -> u8;
    fn write(&mut self, addr: usize, value: u8);

    fn name(&self) -> String;
    fn cartridge_type(&self) -> CartridgeType;
    fn ram_size(&self) -> RamSize;
    fn cgb_flag(&self) -> CgbFlag;
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

    fn ram_size(&self) -> RamSize {
        unimplemented!()
    }

    fn cgb_flag(&self) -> CgbFlag {
        unimplemented!()
    }
}

pub struct Mbc1 {
    buffer: Vec<u8>,
    ram: Vec<u8>,
    ram_enabled: bool,
    rom_bank: u16,
    ram_bank: u16,
}

impl Mbc1 {
    pub fn new(rom: &[u8]) -> Self {
        let mut cartridge = Self {
            buffer: rom.to_vec(),
            ram: vec![],
            ram_enabled: false,
            rom_bank: 0x01,
            ram_bank: 0x00,
        };

        cartridge.ram = vec![0; cartridge.ram_size().len()];

        cartridge
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
            addr if addr >= 0xa000 && addr <= 0xbfff => {
                if self.ram_enabled {
                    let offset_into_ram = 0x2000 * self.ram_bank as usize;
                    let address_in_ram = (addr - 0xa000) + offset_into_ram;

                    self.ram[address_in_ram]
                } else {
                    println!("MBC1: reading from disabled RAM");
                    0xff
                }
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
            addr if addr >= 0xa000 && addr <= 0xbfff => {
                if self.ram_enabled {
                    let offset_into_ram = 0x2000 * self.ram_bank as usize;
                    let address_in_ram = (addr - 0xa000) + offset_into_ram;

                    self.ram[address_in_ram] = data;
                } else {
                    println!("MBC1: writing to disabled RAM");
                }
            }
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

    fn ram_size(&self) -> RamSize {
        RamSize::from(self.buffer[0x149])
    }

    fn cgb_flag(&self) -> CgbFlag {
        CgbFlag::from(self.buffer[0x143])
    }
}

pub struct RomOnly {
    buffer: Vec<u8>,
}

impl RomOnly {
    pub fn new(rom: &[u8]) -> Self {
        Self {
            buffer: rom.to_vec(),
        }
    }
}

impl Cartridge for RomOnly {
    fn read(&self, addr: usize) -> u8 {
        self.buffer[addr]
    }

    fn write(&mut self, addr: usize, value: u8) {
        self.buffer[addr] = value;
    }

    fn name(&self) -> String {
        let name_bytes = &self.buffer[0x134..0x134 + 15];
        let name = name_bytes.split(|c| *c == 0).next().unwrap();

        CString::new(name).unwrap().into_string().unwrap()
    }

    fn cartridge_type(&self) -> CartridgeType {
        CartridgeType::from(self.buffer[0x147])
    }

    fn ram_size(&self) -> RamSize {
        RamSize::from(self.buffer[0x149])
    }

    fn cgb_flag(&self) -> CgbFlag {
        CgbFlag::from(self.buffer[0x143])
    }
}

pub fn load_cartridge(rom: &[u8]) -> Box<dyn Cartridge> {
    match CartridgeType::from(rom[0x147]) {
        CartridgeType::RomOnly => Box::new(RomOnly::new(rom)),
        CartridgeType::Mbc1 => Box::new(Mbc1::new(rom)),
    }
}
