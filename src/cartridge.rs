use std::{
    ffi::CString,
    fs::File,
    io::{Read, Write},
    os::unix::fs::FileExt,
    path::Path,
};

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
    ram_file: File,
}

impl Mbc1 {
    pub fn new(rom: &[u8], rom_path: &Path, ram_dir: Option<&Path>) -> Self {
        let rom_name = format!(
            "{}.ram",
            rom_path
                .file_name()
                .expect("valid ROM file name")
                .to_string_lossy()
        );
        let ram_dir = ram_dir.unwrap_or(rom_path.parent().unwrap());
        let ram_path = ram_dir.join(rom_name);

        let ram = {
            let mut ram_file = File::options()
                .read(true)
                .write(true)
                .create(true)
                .open(&ram_path)
                .unwrap();

            let mut ram = Vec::new();

            let read = ram_file.read_to_end(&mut ram).unwrap();

            if read != 0 {
                ram
            } else {
                vec![0; RamSize::from(rom[0x149]).len()]
            }
        };

        let ram_file = File::options().write(true).open(&ram_path).unwrap();

        Self {
            buffer: rom.to_vec(),
            ram,
            ram_enabled: false,
            rom_bank: 0x01,
            ram_bank: 0x00,
            ram_file,
        }
    }

    fn save_ram_state(&mut self) {
        self.ram_file.write_at(&self.ram, 0).unwrap();
        self.ram_file.flush().unwrap();
    }
}

impl Cartridge for Mbc1 {
    fn read(&self, addr: usize) -> u8 {
        match addr {
            addr if addr < 0x4000 => self.buffer[addr],
            addr if (0x4000..=0x7fff).contains(&addr) => {
                let address_into_bank = addr - 0x4000;
                let bank_offset = 0x4000 * self.rom_bank as usize;

                self.buffer[bank_offset + address_into_bank]
            }
            addr if (0xa000..=0xbfff).contains(&addr) => {
                if self.ram_enabled {
                    let offset_into_ram = 0x2000 * self.ram_bank as usize;
                    let address_in_ram = (addr - 0xa000) + offset_into_ram;

                    self.ram[address_in_ram]
                } else {
                    0xff
                }
            }
            _ => panic!("invalid MBC1 memory read: 0x{addr:x}"),
        }
    }

    fn write(&mut self, addr: usize, data: u8) {
        match addr {
            addr if addr < 0x2000 => self.ram_enabled = data == 0xa,
            addr if (0x2000..0x4000).contains(&addr) => match data {
                0x00 => self.rom_bank = 0x01,
                0x20 => self.rom_bank = 0x21,
                0x40 => self.rom_bank = 0x41,
                0x60 => self.rom_bank = 0x61,
                _ => self.rom_bank = data as u16 & 0x1f,
            },
            addr if (0xa000..=0xbfff).contains(&addr) => {
                if self.ram_enabled {
                    let offset_into_ram = 0x2000 * self.ram_bank as usize;
                    let address_in_ram = (addr - 0xa000) + offset_into_ram;

                    self.ram[address_in_ram] = data;
                    self.save_ram_state();
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

pub fn load_cartridge(rom: &[u8], rom_path: &Path, ram_dir: Option<&Path>) -> Box<dyn Cartridge> {
    match CartridgeType::from(rom[0x147]) {
        CartridgeType::RomOnly => Box::new(RomOnly::new(rom)),
        CartridgeType::Mbc1 => Box::new(Mbc1::new(rom, rom_path, ram_dir)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mbc1_ram_persistency() {
        let dir = tempfile::tempdir().unwrap();
        let rom_path = dir.path().join("rom.gb");

        {
            let mut cartridge = Mbc1::new(&vec![0x02; 0x150], &rom_path, None);

            // Enable RAM.
            cartridge.write(0x0, 0xa);

            assert_eq!(cartridge.read(0xa000), 0);

            cartridge.write(0xa000, 0xff);

            assert_eq!(cartridge.read(0xa000), 0xff);
        }

        {
            let mut cartridge = Mbc1::new(&vec![0x02; 0x150], &rom_path, None);

            // Enable RAM.
            cartridge.write(0x0, 0xa);

            assert_eq!(cartridge.read(0xa000), 0xff);
        }
    }
}
