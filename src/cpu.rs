use std::fmt::Display;

use crate::{
    errors::InstructionError,
    instruction::{Instruction, Opcode, PrefixedOpcode},
    mmu::{Mmu, INT_ENABLE_ADDRESS, INT_FLAG_ADDRESS},
};

macro_rules! prepare_data {
    ($instruction:ident, $len:expr) => {{
        let mut data = [0; $len];
        data.copy_from_slice($instruction.data());
        data
    }};
}

macro_rules! impl_flag {
    ($set_flag:ident, $clear_flag:ident, $set_flag_to:ident, $flag:ident, $i:expr) => {
        fn $set_flag(&mut self) {
            self.regs
                .write_reg(Reg::F, self.regs.read_reg(Reg::F) | 1 << $i)
        }
        fn $clear_flag(&mut self) {
            self.regs
                .write_reg(Reg::F, self.regs.read_reg(Reg::F) & !(1 << $i))
        }
        fn $set_flag_to(&mut self, value: bool) {
            if value {
                self.$set_flag()
            } else {
                self.$clear_flag()
            }
        }
        fn $flag(&self) -> bool {
            self.regs.read_reg(Reg::F) & 1 << $i != 0
        }
    };
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Reg {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug, Default)]
struct Regs {
    /// General purpose and flag registers.
    _regs: [u8; 8],

    /// Stack pointer.
    sp: u16,

    /// Program counter.
    pc: u16,
}

impl Regs {
    pub fn read_reg(&self, reg: Reg) -> u8 {
        self._regs[reg as usize]
    }

    pub fn write_reg(&mut self, reg: Reg, val: u8) {
        self._regs[reg as usize] = val;
    }

    pub fn read_reg16(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => {
                u16::from_le_bytes([self.read_reg(Reg::F) & 0b11110000, self.read_reg(Reg::A)])
            }
            Reg16::BC => u16::from_le_bytes([self.read_reg(Reg::C), self.read_reg(Reg::B)]),
            Reg16::DE => u16::from_le_bytes([self.read_reg(Reg::E), self.read_reg(Reg::D)]),
            Reg16::HL => u16::from_le_bytes([self.read_reg(Reg::L), self.read_reg(Reg::H)]),
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
        }
    }

    pub fn write_reg16(&mut self, reg: Reg16, val: u16) {
        match reg {
            Reg16::AF => {
                let bytes = val.to_le_bytes();

                self.write_reg(Reg::A, bytes[1]);
                self.write_reg(Reg::F, bytes[0]);
            }
            Reg16::BC => {
                let bytes = val.to_le_bytes();

                self.write_reg(Reg::B, bytes[1]);
                self.write_reg(Reg::C, bytes[0]);
            }
            Reg16::DE => {
                let bytes = val.to_le_bytes();

                self.write_reg(Reg::D, bytes[1]);
                self.write_reg(Reg::E, bytes[0]);
            }
            Reg16::HL => {
                let bytes = val.to_le_bytes();

                self.write_reg(Reg::H, bytes[1]);
                self.write_reg(Reg::L, bytes[0]);
            }
            Reg16::SP => self.sp = val,
            Reg16::PC => self.pc = val,
        }
    }

    pub fn inc_reg(&mut self, reg: Reg) -> u8 {
        match reg {
            Reg::F => unreachable!(),
            _ => {
                let val = self.read_reg(reg).wrapping_add(1);
                self.write_reg(reg, val);
                val
            }
        }
    }

    pub fn dec_reg(&mut self, reg: Reg) -> u8 {
        match reg {
            Reg::F => unreachable!(),
            _ => {
                let val = self.read_reg(reg).wrapping_sub(1);
                self.write_reg(reg, val);
                val
            }
        }
    }

    pub fn inc_reg16(&mut self, reg: Reg16) -> u16 {
        let val = self.read_reg16(reg).wrapping_add(1);
        self.write_reg16(reg, val);
        val
    }

    pub fn dec_reg16(&mut self, reg: Reg16) -> u16 {
        let val = self.read_reg16(reg).wrapping_sub(1);
        self.write_reg16(reg, val);
        val
    }
}

#[derive(Default, Debug)]
pub struct Cpu {
    regs: Regs,
    interrupts_enabled: bool,
}

impl Display for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "PC: 0x{:x} SP: 0x{:x}\n",
            self.regs.read_reg16(Reg16::PC),
            self.regs.read_reg16(Reg16::SP)
        ))
        .unwrap();

        f.write_str(&format!(
            "A: 0x{:x} z: {} n: {} h: {} c: {}\n",
            self.regs.read_reg(Reg::A),
            self.z(),
            self.n(),
            self.h(),
            self.c()
        ))
        .unwrap();

        f.write_str(&format!(
            "B: 0x{:x} C: 0x{:x}\n",
            self.regs.read_reg(Reg::B),
            self.regs.read_reg(Reg::C)
        ))
        .unwrap();

        f.write_str(&format!(
            "D: 0x{:x} E: 0x{:x}\n",
            self.regs.read_reg(Reg::D),
            self.regs.read_reg(Reg::E)
        ))
        .unwrap();

        f.write_str(&format!(
            "H: 0x{:x} L: 0x{:x}",
            self.regs.read_reg(Reg::H),
            self.regs.read_reg(Reg::L)
        ))
    }
}

impl Cpu {
    pub fn tick(&mut self, mmu: &mut Mmu) -> usize {
        self.handle_interrupts(mmu);
        self.exec_instruction(mmu)
    }

    fn handle_interrupts(&mut self, mmu: &mut Mmu) {
        if !self.interrupts_enabled {
            return;
        }

        let int_flag = mmu.read_byte(INT_FLAG_ADDRESS);
        let int_enabled = mmu.read_byte(INT_ENABLE_ADDRESS);

        let fired_interrupts = int_flag & int_enabled;

        if fired_interrupts == 0 {
            return;
        }

        let mut handled_interrupt;

        handled_interrupt = self.handle_interrupt(0, 0x40, fired_interrupts, mmu);

        if handled_interrupt {
            return;
        }

        handled_interrupt = self.handle_interrupt(1, 0x48, fired_interrupts, mmu);

        if handled_interrupt {
            return;
        }

        handled_interrupt = self.handle_interrupt(2, 0x50, fired_interrupts, mmu);

        if handled_interrupt {
            return;
        }

        handled_interrupt = self.handle_interrupt(3, 0x58, fired_interrupts, mmu);

        if handled_interrupt {
            return;
        }

        handled_interrupt = self.handle_interrupt(4, 0x60, fired_interrupts, mmu);

        if handled_interrupt {
            return;
        }
    }

    fn handle_interrupt(
        &mut self,
        interrupt_bit: u8,
        interrupt_vector: u16,
        fired_interrupts: u8,
        mmu: &mut Mmu,
    ) -> bool {
        if fired_interrupts & 1 << interrupt_bit == 0 {
            return false;
        }

        mmu.set_bit_to(INT_FLAG_ADDRESS, interrupt_bit as usize, false);
        self.regs.write_reg16(Reg16::PC, interrupt_vector);
        self.interrupts_enabled = false;

        true
    }

    /// Execute the next instruction and return the number of T-states.
    fn exec_instruction(&mut self, mmu: &mut Mmu) -> usize {
        match self.read_instruction(mmu) {
            Some(instruction) => {
                match instruction.opcode() {
                    Opcode::Nop => (),
                    Opcode::Di => self.di(),
                    Opcode::Ei => self.ei(),
                    Opcode::LdBCD16 => self.ld_rr_d16(Reg16::BC, &prepare_data!(instruction, 2)),
                    Opcode::LdDED16 => self.ld_rr_d16(Reg16::DE, &prepare_data!(instruction, 2)),
                    Opcode::LdSPD16 => self.ld_rr_d16(Reg16::SP, &prepare_data!(instruction, 2)),
                    Opcode::LdAD8 => self.ld_r_d8(Reg::A, &prepare_data!(instruction, 1)),
                    Opcode::LdAA16 => self.ld_a_a16(&prepare_data!(instruction, 2), &mmu),
                    Opcode::LdBD8 => self.ld_r_d8(Reg::B, &prepare_data!(instruction, 1)),
                    Opcode::LdCD8 => self.ld_r_d8(Reg::C, &prepare_data!(instruction, 1)),
                    Opcode::LdDD8 => self.ld_r_d8(Reg::D, &prepare_data!(instruction, 1)),
                    Opcode::LdED8 => self.ld_r_d8(Reg::E, &prepare_data!(instruction, 1)),
                    Opcode::LdHD8 => self.ld_r_d8(Reg::H, &prepare_data!(instruction, 1)),
                    Opcode::LdLD8 => self.ld_r_d8(Reg::L, &prepare_data!(instruction, 1)),
                    Opcode::LdHLSPE8 => self.ld_hl_sp_e8(&prepare_data!(instruction, 1)),
                    Opcode::AddSPE8 => self.add_sp_e8(&prepare_data!(instruction, 1)),
                    Opcode::LdAB => self.ld_r_r(Reg::A, Reg::B),
                    Opcode::LdAC => self.ld_r_r(Reg::A, Reg::C),
                    Opcode::LdAD => self.ld_r_r(Reg::A, Reg::D),
                    Opcode::LdAE => self.ld_r_r(Reg::A, Reg::E),
                    Opcode::LdAH => self.ld_r_r(Reg::A, Reg::H),
                    Opcode::LdAL => self.ld_r_r(Reg::A, Reg::L),
                    Opcode::LdBA => self.ld_r_r(Reg::B, Reg::A),
                    Opcode::LdBB => self.ld_r_r(Reg::B, Reg::B),
                    Opcode::LdBC => self.ld_r_r(Reg::B, Reg::C),
                    Opcode::LdBD => self.ld_r_r(Reg::B, Reg::D),
                    Opcode::LdBE => self.ld_r_r(Reg::B, Reg::E),
                    Opcode::LdBH => self.ld_r_r(Reg::B, Reg::H),
                    Opcode::LdBL => self.ld_r_r(Reg::B, Reg::L),
                    Opcode::LdCA => self.ld_r_r(Reg::C, Reg::A),
                    Opcode::LdCB => self.ld_r_r(Reg::C, Reg::B),
                    Opcode::LdDA => self.ld_r_r(Reg::D, Reg::A),
                    Opcode::LdDL => self.ld_r_r(Reg::D, Reg::L),
                    Opcode::LdEA => self.ld_r_r(Reg::E, Reg::A),
                    Opcode::LdEL => self.ld_r_r(Reg::E, Reg::L),
                    Opcode::LdHA => self.ld_r_r(Reg::H, Reg::A),
                    Opcode::LdHD => self.ld_r_r(Reg::H, Reg::D),
                    Opcode::LdLA => self.ld_r_r(Reg::L, Reg::A),
                    Opcode::LdLE => self.ld_r_r(Reg::L, Reg::E),
                    Opcode::PushAF => self.push_rr(Reg16::AF, mmu),
                    Opcode::PushBC => self.push_rr(Reg16::BC, mmu),
                    Opcode::PushDE => self.push_rr(Reg16::DE, mmu),
                    Opcode::PushHL => self.push_rr(Reg16::HL, mmu),
                    Opcode::PopAF => self.pop_rr(Reg16::AF, mmu),
                    Opcode::PopBC => self.pop_rr(Reg16::BC, mmu),
                    Opcode::PopDE => self.pop_rr(Reg16::DE, mmu),
                    Opcode::PopHL => self.pop_rr(Reg16::HL, mmu),
                    Opcode::LdhAA8 => self.ldh_a_a8(&prepare_data!(instruction, 1), mmu),
                    Opcode::LdhA8A => self.ldh_a8_a(&prepare_data!(instruction, 1), mmu),
                    Opcode::LdAIndHLInc => self.ld_a_ind_hl_inc(mmu),
                    Opcode::IncA => self.inc_r(Reg::A),
                    Opcode::IncB => self.inc_r(Reg::B),
                    Opcode::IncC => self.inc_r(Reg::C),
                    Opcode::IncD => self.inc_r(Reg::D),
                    Opcode::IncE => self.inc_r(Reg::E),
                    Opcode::IncH => self.inc_r(Reg::H),
                    Opcode::IncL => self.inc_r(Reg::L),
                    Opcode::IncBC => self.inc_rr(Reg16::BC),
                    Opcode::IncDE => self.inc_rr(Reg16::DE),
                    Opcode::IncHL => self.inc_rr(Reg16::HL),
                    Opcode::IncSP => self.inc_rr(Reg16::SP),
                    Opcode::Daa => self.daa(),
                    Opcode::DecA => self.dec_r(Reg::A),
                    Opcode::DecB => self.dec_r(Reg::B),
                    Opcode::DecC => self.dec_r(Reg::C),
                    Opcode::DecD => self.dec_r(Reg::D),
                    Opcode::DecE => self.dec_r(Reg::E),
                    Opcode::DecH => self.dec_r(Reg::H),
                    Opcode::DecL => self.dec_r(Reg::L),
                    Opcode::DecBC => self.dec_rr(Reg16::BC),
                    Opcode::DecDE => self.dec_rr(Reg16::DE),
                    Opcode::DecHL => self.dec_rr(Reg16::HL),
                    Opcode::DecSP => self.dec_rr(Reg16::SP),
                    Opcode::DecIndHL => self.dec_ind_hl(mmu),
                    Opcode::OrAIndHL => self.or_a_ind_hl(mmu),
                    Opcode::OrA => self.or_r(Reg::A),
                    Opcode::OrB => self.or_r(Reg::B),
                    Opcode::OrC => self.or_r(Reg::C),
                    Opcode::OrD8 => self.or_d8(&prepare_data!(instruction, 1)),
                    Opcode::XorA => self.xor_r(Reg::A),
                    Opcode::XorC => self.xor_r(Reg::C),
                    Opcode::XorL => self.xor_r(Reg::L),
                    Opcode::XorAD8 => self.xor_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::XorAIndHL => self.xor_a_ind_hl(mmu),
                    Opcode::AndAD8 => self.and_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::AdcAD8 => self.adc_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::AddAB => self.add_r(Reg::B),
                    Opcode::AddAIndHL => self.add_a_ind_hl(mmu),
                    Opcode::AddAD8 => self.add_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::AddHLBC => self.add_hl_rr(Reg16::BC),
                    Opcode::AddHLDE => self.add_hl_rr(Reg16::DE),
                    Opcode::AddHLHL => self.add_hl_rr(Reg16::HL),
                    Opcode::AddHLSP => self.add_hl_rr(Reg16::SP),
                    Opcode::SbcD8 => self.sbc_d8(&prepare_data!(instruction, 1)),
                    Opcode::SubD8 => self.sub_d8(&prepare_data!(instruction, 1)),
                    Opcode::SubAB => self.sub_r(Reg::B),
                    Opcode::CpAE => self.cp_r(Reg::E),
                    Opcode::CpD8 => self.cp_d8(&prepare_data!(instruction, 1)),
                    Opcode::CpAIndHL => self.cp_a_ind_hl(mmu),
                    Opcode::LdAIndC => self.ld_r_ind_r(Reg::A, Reg::C, mmu),
                    Opcode::LdAIndDE => self.ld_r_ind_rr(Reg::A, Reg16::DE, mmu),
                    Opcode::LdAIndHL => self.ld_r_ind_rr(Reg::A, Reg16::HL, mmu),
                    Opcode::LdBIndHL => self.ld_r_ind_rr(Reg::B, Reg16::HL, mmu),
                    Opcode::LdCIndHL => self.ld_r_ind_rr(Reg::C, Reg16::HL, mmu),
                    Opcode::LdDIndHL => self.ld_r_ind_rr(Reg::D, Reg16::HL, mmu),
                    Opcode::LdEIndHL => self.ld_r_ind_rr(Reg::E, Reg16::HL, mmu),
                    Opcode::LdHIndHL => self.ld_r_ind_rr(Reg::H, Reg16::HL, mmu),
                    Opcode::LdLIndHL => self.ld_r_ind_rr(Reg::L, Reg16::HL, mmu),
                    Opcode::LdHLD16 => self.ld_rr_d16(Reg16::HL, &prepare_data!(instruction, 2)),
                    Opcode::LdIndDEA => self.ld_ind_rr_r(Reg16::DE, Reg::A, mmu),
                    Opcode::LdIndHLA => self.ld_ind_rr_r(Reg16::HL, Reg::A, mmu),
                    Opcode::LdIndHLB => self.ld_ind_rr_r(Reg16::HL, Reg::B, mmu),
                    Opcode::LdIndHLC => self.ld_ind_rr_r(Reg16::HL, Reg::C, mmu),
                    Opcode::LdIndHLD => self.ld_ind_rr_r(Reg16::HL, Reg::D, mmu),
                    Opcode::LdIndHLE => self.ld_ind_rr_r(Reg16::HL, Reg::E, mmu),
                    Opcode::LdIndHLD8 => self.ld_ind_hl_d8(&prepare_data!(instruction, 1), mmu),
                    Opcode::LdIndHLDecA => self.ld_ind_hl_dec_a(mmu),
                    Opcode::LdIndHLIncA => self.ld_ind_hl_inc_a(mmu),
                    Opcode::LdIndCA => self.ld_ind_c_a(mmu),
                    Opcode::JrR8 => self.jr_r8(&prepare_data!(instruction, 1)),
                    Opcode::JrCR8 => self.jr_c_r8(&prepare_data!(instruction, 1)),
                    Opcode::JrNcR8 => self.jr_nc_r8(&prepare_data!(instruction, 1)),
                    Opcode::JrNzR8 => self.jr_nz_r8(&prepare_data!(instruction, 1)),
                    Opcode::JrZR8 => self.jr_z_r8(&prepare_data!(instruction, 1)),
                    Opcode::JpA16 => self.jp_a16(&prepare_data!(instruction, 2)),
                    Opcode::JpNzA16 => self.jp_nz_a16(&prepare_data!(instruction, 2)),
                    Opcode::JpHL => self.jp_hl(),
                    Opcode::CallA16 => self.call_a16(&prepare_data!(instruction, 2), mmu),
                    Opcode::CallNzA16 => self.call_nz_a16(&prepare_data!(instruction, 2), mmu),
                    Opcode::Ret => self.ret(mmu),
                    Opcode::RetC => self.ret_c(mmu),
                    Opcode::RetNc => self.ret_nc(mmu),
                    Opcode::RetZ => self.ret_z(mmu),
                    Opcode::RlA => self.rl_r(Reg::A),
                    Opcode::RrA => self.rr_r(Reg::A),
                    Opcode::Cpl => self.cpl(),
                    Opcode::Scf => self.scf(),
                    Opcode::LdA16A => self.ld_a16_a(&prepare_data!(instruction, 2), mmu),
                    Opcode::LdA16SP => self.ld_a16_sp(&prepare_data!(instruction, 2), mmu),
                    Opcode::LdSPHL => self.ld_sp_hl(),
                    Opcode::Prefix => match instruction.prefixed_opcode().unwrap() {
                        PrefixedOpcode::Bit0D => self.bit_r(Reg::D, 0),
                        PrefixedOpcode::Bit7H => self.bit_r(Reg::H, 7),
                        PrefixedOpcode::RlC => self.rl_r(Reg::C),
                        PrefixedOpcode::RrC => self.rr_r(Reg::C),
                        PrefixedOpcode::RrD => self.rr_r(Reg::D),
                        PrefixedOpcode::RrE => self.rr_r(Reg::E),
                        PrefixedOpcode::SrlB => self.srl_r(Reg::B),
                        PrefixedOpcode::SwapA => self.swap_r(Reg::A),
                    },
                };

                instruction.cycles()
            }
            None => 0,
        }
    }

    fn read_instruction<'a>(&mut self, mmu: &'a mut Mmu) -> Option<Instruction> {
        let opcode =
            match Opcode::try_from(&mmu.read_byte(self.regs.read_reg16(Reg16::PC) as usize)) {
                Ok(opcode) => opcode,
                Err(InstructionError::UnrecognizedOpcode(opcode)) => {
                    println!("{}", self);
                    println!("IE: 0b{:b}", mmu.read_byte(INT_ENABLE_ADDRESS));
                    panic!("unrecognized opcode: 0x{:x}", opcode);
                }
                _ => unreachable!(),
            };

        self.regs.inc_reg16(Reg16::PC);
        let pc = self.regs.read_reg16(Reg16::PC);

        match opcode {
            Opcode::Nop => Some(Instruction::nop()),
            Opcode::LdSPD16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::ld_sp_d16(&data))
            }
            Opcode::LdAD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_a_d8(data))
            }
            Opcode::LdAA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::ld_a_a16(&data))
            }
            Opcode::LdBD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_b_d8(data))
            }
            Opcode::LdCD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_c_d8(data))
            }
            Opcode::LdDD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_d_d8(data))
            }
            Opcode::LdHD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_h_d8(data))
            }
            Opcode::LdED8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_e_d8(data))
            }
            Opcode::LdLD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_l_d8(data))
            }
            Opcode::LdhAA8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ldh_a_a8(data))
            }
            Opcode::LdhA8A => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ldh_a8_a(data))
            }
            Opcode::LdHLSPE8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_hl_sp_e8(data))
            }
            Opcode::AddSPE8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::add_sp_e8(data))
            }
            Opcode::LdSPHL => Some(Instruction::ld_sp_hl(&[])),
            Opcode::LdAB => Some(Instruction::ld_a_b(&[])),
            Opcode::LdAC => Some(Instruction::ld_a_c(&[])),
            Opcode::LdAD => Some(Instruction::ld_a_d(&[])),
            Opcode::LdAE => Some(Instruction::ld_a_e(&[])),
            Opcode::LdAH => Some(Instruction::ld_a_h(&[])),
            Opcode::LdAL => Some(Instruction::ld_a_l(&[])),
            Opcode::LdBA => Some(Instruction::ld_b_a(&[])),
            Opcode::LdBB => Some(Instruction::ld_b_b(&[])),
            Opcode::LdBC => Some(Instruction::ld_b_c(&[])),
            Opcode::LdBD => Some(Instruction::ld_b_d(&[])),
            Opcode::LdBE => Some(Instruction::ld_b_e(&[])),
            Opcode::LdBH => Some(Instruction::ld_b_h(&[])),
            Opcode::LdBL => Some(Instruction::ld_b_l(&[])),
            Opcode::LdCA => Some(Instruction::ld_c_a(&[])),
            Opcode::LdCB => Some(Instruction::ld_c_b(&[])),
            Opcode::LdDA => Some(Instruction::ld_d_a(&[])),
            Opcode::LdDL => Some(Instruction::ld_d_l(&[])),
            Opcode::LdEA => Some(Instruction::ld_e_a(&[])),
            Opcode::LdEL => Some(Instruction::ld_e_l(&[])),
            Opcode::LdHA => Some(Instruction::ld_h_a(&[])),
            Opcode::LdHD => Some(Instruction::ld_h_d(&[])),
            Opcode::LdLA => Some(Instruction::ld_l_a(&[])),
            Opcode::LdLE => Some(Instruction::ld_l_e(&[])),
            Opcode::PushAF => Some(Instruction::push_af(&[])),
            Opcode::PushBC => Some(Instruction::push_bc(&[])),
            Opcode::PushDE => Some(Instruction::push_de(&[])),
            Opcode::PushHL => Some(Instruction::push_hl(&[])),
            Opcode::PopAF => Some(Instruction::pop_af(&[])),
            Opcode::PopBC => Some(Instruction::pop_bc(&[])),
            Opcode::PopDE => Some(Instruction::pop_de(&[])),
            Opcode::PopHL => Some(Instruction::pop_hl(&[])),
            Opcode::IncA => Some(Instruction::inc_a(&[])),
            Opcode::IncB => Some(Instruction::inc_b(&[])),
            Opcode::IncC => Some(Instruction::inc_c(&[])),
            Opcode::IncD => Some(Instruction::inc_d(&[])),
            Opcode::IncE => Some(Instruction::inc_e(&[])),
            Opcode::IncBC => Some(Instruction::inc_bc(&[])),
            Opcode::IncDE => Some(Instruction::inc_de(&[])),
            Opcode::IncHL => Some(Instruction::inc_hl(&[])),
            Opcode::IncSP => Some(Instruction::inc_sp(&[])),
            Opcode::IncH => Some(Instruction::inc_h(&[])),
            Opcode::IncL => Some(Instruction::inc_l(&[])),
            Opcode::Daa => Some(Instruction::daa(&[])),
            Opcode::DecA => Some(Instruction::dec_a(&[])),
            Opcode::DecB => Some(Instruction::dec_b(&[])),
            Opcode::DecC => Some(Instruction::dec_c(&[])),
            Opcode::DecD => Some(Instruction::dec_d(&[])),
            Opcode::DecE => Some(Instruction::dec_e(&[])),
            Opcode::DecH => Some(Instruction::dec_h(&[])),
            Opcode::DecL => Some(Instruction::dec_l(&[])),
            Opcode::DecBC => Some(Instruction::dec_bc(&[])),
            Opcode::DecDE => Some(Instruction::dec_de(&[])),
            Opcode::DecHL => Some(Instruction::dec_hl(&[])),
            Opcode::DecSP => Some(Instruction::dec_sp(&[])),
            Opcode::DecIndHL => Some(Instruction::dec_ind_hl(&[])),
            Opcode::OrA => Some(Instruction::or_a(&[])),
            Opcode::OrAIndHL => Some(Instruction::or_a_ind_hl(&[])),
            Opcode::OrB => Some(Instruction::or_b(&[])),
            Opcode::OrC => Some(Instruction::or_c(&[])),
            Opcode::OrD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::or_d8(data))
            }
            Opcode::XorA => Some(Instruction::xor_a(&[])),
            Opcode::XorC => Some(Instruction::xor_a_c(&[])),
            Opcode::XorL => Some(Instruction::xor_a_l(&[])),
            Opcode::XorAD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::xor_a_d8(data))
            }
            Opcode::XorAIndHL => Some(Instruction::xor_a_ind_hl(&[])),
            Opcode::AndAD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::and_a_d8(data))
            }
            Opcode::AdcAD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::adc_a_d8(data))
            }
            Opcode::AddAB => Some(Instruction::add_a_b(&[])),
            Opcode::AddHLBC => Some(Instruction::add_hl_bc(&[])),
            Opcode::AddHLDE => Some(Instruction::add_hl_de(&[])),
            Opcode::AddHLHL => Some(Instruction::add_hl_hl(&[])),
            Opcode::AddHLSP => Some(Instruction::add_hl_sp(&[])),
            Opcode::AddAIndHL => Some(Instruction::add_a_ind_hl(&[])),
            Opcode::AddAD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::add_a_d8(data))
            }
            Opcode::CpAE => Some(Instruction::cp_a_e(&[])),
            Opcode::CpD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::cp_d8(data))
            }
            Opcode::CpAIndHL => Some(Instruction::cp_a_ind_hl(&[])),
            Opcode::SbcD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::sbc_d8(data))
            }
            Opcode::SubD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::sub_d8(data))
            }
            Opcode::SubAB => Some(Instruction::sub_a_b(&[])),
            Opcode::LdHLD16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::ld_hl_d16(&data))
            }
            Opcode::LdBCD16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::ld_bc_d16(&data))
            }
            Opcode::LdDED16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::ld_de_d16(&data))
            }
            Opcode::LdIndDEA => Some(Instruction::ld_ind_de_a(&[])),
            Opcode::LdIndHLA => Some(Instruction::ld_ind_hl_a(&[])),
            Opcode::LdIndHLB => Some(Instruction::ld_ind_hl_b(&[])),
            Opcode::LdIndHLC => Some(Instruction::ld_ind_hl_c(&[])),
            Opcode::LdIndHLD => Some(Instruction::ld_ind_hl_d(&[])),
            Opcode::LdIndHLE => Some(Instruction::ld_ind_hl_e(&[])),
            Opcode::LdIndHLD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::ld_ind_hl_d8(data))
            }
            Opcode::LdIndHLDecA => Some(Instruction::ld_ind_hl_dec_a(&[])),
            Opcode::LdIndHLIncA => Some(Instruction::ld_ind_hl_inc_a(&[])),
            Opcode::LdIndCA => Some(Instruction::ld_ind_c_a(&[])),
            Opcode::LdAIndC => Some(Instruction::ld_a_ind_c(&[])),
            Opcode::LdAIndDE => Some(Instruction::ldh_a_ind_de(&[])),
            Opcode::LdAIndHL => Some(Instruction::ldh_a_ind_hl(&[])),
            Opcode::LdBIndHL => Some(Instruction::ldh_b_ind_hl(&[])),
            Opcode::LdCIndHL => Some(Instruction::ldh_c_ind_hl(&[])),
            Opcode::LdDIndHL => Some(Instruction::ldh_d_ind_hl(&[])),
            Opcode::LdEIndHL => Some(Instruction::ldh_e_ind_hl(&[])),
            Opcode::LdHIndHL => Some(Instruction::ldh_h_ind_hl(&[])),
            Opcode::LdLIndHL => Some(Instruction::ldh_l_ind_hl(&[])),
            Opcode::LdAIndHLInc => Some(Instruction::ld_a_ind_hl_inc(&[])),
            Opcode::JrR8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::jr_r8(data))
            }
            Opcode::JrCR8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::jr_c_r8(data))
            }
            Opcode::JrNcR8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::jr_nc_r8(data))
            }
            Opcode::JrNzR8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::jr_nz_r8(data))
            }
            Opcode::JrZR8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::jr_z_r8(data))
            }
            Opcode::JpA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::jp_a16(&data))
            }
            Opcode::JpNzA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::jp_nz_a16(&data))
            }
            Opcode::JpHL => Some(Instruction::jp_hl(&[])),
            Opcode::CallA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::call_a16(&data))
            }
            Opcode::CallNzA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::call_nz_a16(&data))
            }
            Opcode::Ret => Some(Instruction::ret(&[])),
            Opcode::RetC => Some(Instruction::ret_c(&[])),
            Opcode::RetNc => Some(Instruction::ret_nc(&[])),
            Opcode::RetZ => Some(Instruction::ret_z(&[])),
            Opcode::RlA => Some(Instruction::rl_a(&[])),
            Opcode::RrA => Some(Instruction::rr_a(&[])),
            Opcode::Cpl => Some(Instruction::cpl(&[])),
            Opcode::Scf => Some(Instruction::scf(&[])),
            Opcode::LdA16A => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::ld_a16_a(&data))
            }
            Opcode::LdA16SP => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::ld_a16_sp(&data))
            }
            Opcode::Prefix => {
                let prefixed_opcode = match PrefixedOpcode::try_from(&mmu.read_byte(pc as usize)) {
                    Ok(prefixed_opcode) => prefixed_opcode,
                    Err(InstructionError::UnrecognizedPrefixedOpcode(prefixed_opcode)) => {
                        println!("{}", self);
                        panic!("unrecognized prefixed opcode: 0x{:x}", prefixed_opcode);
                    }
                    _ => unreachable!(),
                };
                self.regs.write_reg16(Reg16::PC, pc + 1);

                match prefixed_opcode {
                    PrefixedOpcode::Bit7H => Some(Instruction::bit7h(&[])),
                    PrefixedOpcode::RlC => Some(Instruction::rl_c(&[])),
                    PrefixedOpcode::Bit0D => Some(Instruction::bit0d(&[])),
                    PrefixedOpcode::RrC => Some(Instruction::rr_c(&[])),
                    PrefixedOpcode::RrD => Some(Instruction::rr_d(&[])),
                    PrefixedOpcode::RrE => Some(Instruction::rr_e(&[])),
                    PrefixedOpcode::SrlB => Some(Instruction::srl_b(&[])),
                    PrefixedOpcode::SwapA => Some(Instruction::swap_a(&[])),
                }
            }
            Opcode::Di => Some(Instruction::di(&[])),
            Opcode::Ei => Some(Instruction::ei(&[])),
        }
    }

    impl_flag!(set_z, clear_z, set_z_to, z, 7);
    impl_flag!(set_n, clear_n, set_n_to, n, 6);
    impl_flag!(set_h, clear_h, set_h_to, h, 5);
    impl_flag!(set_c, clear_c, set_c_to, c, 4);

    fn check_z(&mut self, value: u8) {
        if value == 0 {
            self.set_z();
        } else {
            self.clear_z();
        }
    }

    fn call_a16(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        let pc = self.regs.read_reg16(Reg16::PC).to_le_bytes();

        let sp = self.regs.dec_reg16(Reg16::SP);
        mmu.write_byte(sp as usize, pc[1]);

        let sp = self.regs.dec_reg16(Reg16::SP);
        mmu.write_byte(sp as usize, pc[0]);

        self.regs.write_reg16(Reg16::PC, u16::from_le_bytes(*data));
    }

    fn call_nz_a16(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        if !self.z() {
            self.call_a16(data, mmu);
        }
    }

    fn ret(&mut self, mmu: &mut Mmu) {
        let sp = self.regs.read_reg16(Reg16::SP);

        let ll = mmu.read_byte(sp as usize);
        let sp = self.regs.inc_reg16(Reg16::SP);

        let hh = mmu.read_byte(sp as usize);
        let sp = self.regs.inc_reg16(Reg16::SP);

        self.regs
            .write_reg16(Reg16::PC, u16::from_le_bytes([ll, hh]));
    }

    fn ret_c(&mut self, mmu: &mut Mmu) {
        if self.c() {
            self.ret(mmu);
        }
    }

    fn ret_nc(&mut self, mmu: &mut Mmu) {
        if !self.c() {
            self.ret(mmu);
        }
    }

    fn ret_z(&mut self, mmu: &mut Mmu) {
        if self.z() {
            self.ret(mmu);
        }
    }

    fn di(&mut self) {
        self.interrupts_enabled = false;
    }

    fn ei(&mut self) {
        self.interrupts_enabled = true;
    }

    fn cpl(&mut self) {
        self.regs.write_reg(Reg::A, !self.regs.read_reg(Reg::A));

        self.set_n();
        self.set_h();
    }

    fn scf(&mut self) {
        self.clear_n();
        self.clear_h();
        self.set_c();
    }

    fn daa(&mut self) {
        if !self.n() {
            if self.c() || self.regs.read_reg(Reg::A) > 0x99 {
                self.regs
                    .write_reg(Reg::A, self.regs.read_reg(Reg::A).wrapping_add(0x60));
                self.set_c();
            }

            if self.h() || (self.regs.read_reg(Reg::A) & 0x0f) > 0x09 {
                self.regs
                    .write_reg(Reg::A, self.regs.read_reg(Reg::A).wrapping_add(0x6));
            }
        } else {
            if self.c() {
                self.regs
                    .write_reg(Reg::A, self.regs.read_reg(Reg::A).wrapping_sub(0x60));
            }

            if self.h() {
                self.regs
                    .write_reg(Reg::A, self.regs.read_reg(Reg::A).wrapping_sub(0x6));
            }
        }

        self.check_z(self.regs.read_reg(Reg::A));
        self.clear_h();
    }

    fn push_rr(&mut self, reg: Reg16, mmu: &mut Mmu) {
        let bytes = self.regs.read_reg16(reg).to_le_bytes();
        self.regs.dec_reg16(Reg16::SP);
        mmu.write_byte(self.regs.read_reg16(Reg16::SP) as usize, bytes[1]);

        self.regs.dec_reg16(Reg16::SP);
        mmu.write_byte(self.regs.read_reg16(Reg16::SP) as usize, bytes[0]);
    }

    fn pop_rr(&mut self, reg: Reg16, mmu: &mut Mmu) {
        let ll = mmu.read_byte(self.regs.read_reg16(Reg16::SP) as usize);
        self.regs.inc_reg16(Reg16::SP);

        let hh = mmu.read_byte(self.regs.read_reg16(Reg16::SP) as usize);
        self.regs.inc_reg16(Reg16::SP);

        let val = u16::from_le_bytes([ll, hh]);

        self.regs.write_reg16(reg, val);
    }

    fn ld_a_a16(&mut self, data: &[u8; 2], mmu: &Mmu) {
        self.regs
            .write_reg(Reg::A, mmu.read_byte(u16::from_le_bytes(*data) as usize));
    }

    fn ld_rr_d16(&mut self, reg: Reg16, data: &[u8; 2]) {
        self.regs.write_reg16(reg, u16::from_le_bytes(*data));
    }

    fn ld_r_d8(&mut self, reg: Reg, data: &[u8; 1]) {
        self.regs.write_reg(reg, data[0]);
    }

    fn ld_r_r(&mut self, dst: Reg, src: Reg) {
        self.regs.write_reg(dst, self.regs.read_reg(src));
    }

    fn ldh_a8_a(&self, data: &[u8; 1], mmu: &mut Mmu) {
        let addr = 0xff00 + data[0] as u16;
        mmu.write_byte(addr as usize, self.regs.read_reg(Reg::A))
    }

    fn ldh_a_a8(&mut self, data: &[u8; 1], mmu: &Mmu) {
        let addr = 0xff00 + data[0] as u16;
        self.regs.write_reg(Reg::A, mmu.read_byte(addr as usize));
    }

    fn ld_a16_a(&self, data: &[u8; 2], mmu: &mut Mmu) {
        let addr = u16::from_le_bytes(*data);
        mmu.write_byte(addr as usize, self.regs.read_reg(Reg::A));
    }

    fn ld_a16_sp(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        let addr = u16::from_le_bytes(*data) as usize;
        let bytes = self.regs.read_reg16(Reg16::SP).to_le_bytes();

        mmu.write_byte(addr, bytes[0]);
        mmu.write_byte(addr + 1, bytes[1]);
    }

    fn ld_hl_sp_e8(&mut self, data: &[u8; 1]) {
        let reg = self.regs.read_reg16(Reg16::SP);
        let e8: i8 = unsafe { std::mem::transmute(data[0]) };

        let val = if e8 >= 0 {
            reg.wrapping_add(e8 as u16)
        } else {
            reg.wrapping_sub(-e8 as u16)
        };

        self.regs.write_reg16(Reg16::HL, val);
        // self.h = ((val & 0xff00) >> 8) as u8;
        // self.l = (val & 0x00ff) as u8;

        self.clear_z();
        self.clear_n();
        self.set_h_to(((reg ^ e8 as u16 ^ (val & 0xffff)) & 0x10) == 0x10);
        self.set_c_to(((reg ^ e8 as u16 ^ (val & 0xffff)) & 0x100) == 0x100);
    }

    fn add_sp_e8(&mut self, data: &[u8; 1]) {
        let reg = self.regs.read_reg16(Reg16::SP);
        let e8: i8 = unsafe { std::mem::transmute(data[0]) };

        let val = reg.wrapping_add_signed(e8 as i16);

        self.clear_z();
        self.clear_n();
        self.set_h_to(((reg ^ e8 as u16 ^ (val & 0xffff)) & 0x10) == 0x10);
        self.set_c_to(((reg ^ e8 as u16 ^ (val & 0xffff)) & 0x100) == 0x100);

        self.regs.write_reg16(Reg16::SP, val);
    }

    fn ld_sp_hl(&mut self) {
        self.regs
            .write_reg16(Reg16::SP, self.regs.read_reg16(Reg16::HL));
    }

    fn and_a_d8(&mut self, data: &[u8; 1]) {
        let mut val = self.regs.read_reg(Reg::A);

        val &= data[0];
        self.set_z_to(val == 0);
        self.set_h();
        self.clear_n();
        self.clear_c();

        self.regs.write_reg(Reg::A, val);
    }

    fn xor_r(&mut self, reg: Reg) {
        let x = self.regs.read_reg(Reg::A);
        let y = self.regs.read_reg(reg);

        let val = self.xor(x, y);

        self.regs.write_reg(Reg::A, val);
    }

    fn xor_a_ind_hl(&mut self, mmu: &Mmu) {
        let val = self.xor(
            self.regs.read_reg(Reg::A),
            mmu.read_byte(self.regs.read_reg16(Reg16::HL) as usize),
        );

        self.regs.write_reg(Reg::A, val);
    }

    fn xor_a_d8(&mut self, data: &[u8; 1]) {
        let val = self.xor(self.regs.read_reg(Reg::A), data[0]);
        self.regs.write_reg(Reg::A, val);
    }

    fn xor(&mut self, x: u8, y: u8) -> u8 {
        let res = x ^ y;

        self.check_z(res);
        self.clear_n();
        self.clear_h();
        self.clear_c();

        res
    }

    fn or_a_ind_hl(&mut self, mmu: &Mmu) {
        let val = self.or(
            self.regs.read_reg(Reg::A),
            mmu.read_byte(self.regs.read_reg16(Reg16::HL) as usize),
        );

        self.regs.write_reg(Reg::A, val);
    }

    fn or_d8(&mut self, data: &[u8; 1]) {
        let val = self.or(self.regs.read_reg(Reg::A), data[0]);
        self.regs.write_reg(Reg::A, val);
    }

    fn or_r(&mut self, reg: Reg) {
        let x = self.regs.read_reg(Reg::A);
        let y = self.regs.read_reg(reg);

        let val = self.or(x, y);

        self.regs.write_reg(Reg::A, val);
    }

    fn or(&mut self, x: u8, y: u8) -> u8 {
        let res = x | y;
        self.set_z_to(res == 0);
        self.clear_c();
        self.clear_h();
        self.clear_n();

        res
    }

    fn inc_r(&mut self, reg: Reg) {
        let res = self.regs.inc_reg(reg);

        self.clear_n();
        self.check_z(res);
        self.set_h_to(res & 0xf == 0x0);
    }

    fn inc_rr(&mut self, reg: Reg16) {
        self.regs.inc_reg16(reg);
    }

    fn add_r(&mut self, reg: Reg) {
        let val = self.add8(self.regs.read_reg(Reg::A), self.regs.read_reg(reg));
        self.regs.write_reg(Reg::A, val);
    }

    fn add_a_ind_hl(&mut self, mmu: &Mmu) {
        let val = self.add8(
            self.regs.read_reg(Reg::A),
            mmu.read_byte(self.regs.read_reg16(Reg16::HL) as usize),
        );

        self.regs.write_reg(Reg::A, val);
    }

    fn add_a_d8(&mut self, data: &[u8; 1]) {
        let val = self.add8(self.regs.read_reg(Reg::A), data[0]);
        self.regs.write_reg(Reg::A, val);
    }

    fn add8(&mut self, x: u8, y: u8) -> u8 {
        let (res, c) = x.overflowing_add(y);

        self.clear_n();
        self.check_z(res);
        self.set_h_to((x & 0xf) + (y & 0xf) > 0xf);
        self.set_c_to(c);

        res
    }

    fn add16(&mut self, x: u16, y: u16) -> u16 {
        let (res, c) = x.overflowing_add(y);

        self.clear_n();
        self.set_h_to((x & 0xfff) + (y & 0xfff) > 0xfff);
        self.set_c_to(c);

        res
    }

    fn add_hl_rr(&mut self, reg: Reg16) {
        let val = self.add16(self.regs.read_reg16(Reg16::HL), self.regs.read_reg16(reg));
        self.regs.write_reg16(Reg16::HL, val);
    }

    fn adc_a_d8(&mut self, data: &[u8; 1]) {
        let reg = self.regs.read_reg(Reg::A);

        let mut res;
        let first_c;
        let second_c;

        (res, first_c) = reg.overflowing_add(self.c().into());
        (res, second_c) = res.overflowing_add(data[0]);

        let carry_u8: u8 = self.c().into();

        self.clear_n();
        self.check_z(res);
        self.set_c_to(first_c || second_c);
        self.set_h_to((reg & 0xf) + (data[0] & 0xf) + carry_u8 > 0xf);

        self.regs.write_reg(Reg::A, res);
    }

    fn sbc_d8(&mut self, data: &[u8; 1]) {
        let reg = self.regs.read_reg(Reg::A);

        let mut val;
        let first_c;
        let second_c;

        (val, first_c) = reg.overflowing_sub(self.c().into());
        (val, second_c) = val.overflowing_sub(data[0]);

        let carry_u8: u8 = self.c().into();

        let (r, first_h) = (reg & 0xf).overflowing_sub(data[0] & 0xf);
        let (_, second_h) = r.overflowing_sub(carry_u8);

        self.set_n();
        self.check_z(val);
        self.set_c_to(first_c || second_c);
        self.set_h_to(first_h || second_h);

        self.regs.write_reg(Reg::A, val);
    }

    fn sub8(&mut self, x: u8, y: u8) -> u8 {
        let (res, c) = x.overflowing_sub(y);

        let (_, h) = (x & 0xf).overflowing_sub(y & 0xf);

        self.set_n();
        self.check_z(res);
        self.set_h_to(h);
        self.set_c_to(c);

        res
    }

    fn sub_d8(&mut self, data: &[u8; 1]) {
        let val = self.sub8(self.regs.read_reg(Reg::A), data[0]);
        self.regs.write_reg(Reg::A, val);
    }

    fn sub_r(&mut self, reg: Reg) {
        let val = self.sub8(self.regs.read_reg(Reg::A), self.regs.read_reg(reg));
        self.regs.write_reg(Reg::A, val);
    }

    fn cp(&mut self, x: u8, y: u8) {
        self.sub8(x, y);
    }

    fn cp_r(&mut self, reg: Reg) {
        self.cp(self.regs.read_reg(Reg::A), self.regs.read_reg(reg));
    }

    fn cp_d8(&mut self, data: &[u8; 1]) {
        self.cp(self.regs.read_reg(Reg::A), data[0]);
    }

    fn cp_a_ind_hl(&mut self, mmu: &mut Mmu) {
        self.cp(
            self.regs.read_reg(Reg::A),
            mmu.read_byte(self.regs.read_reg16(Reg16::HL) as usize),
        );
    }

    fn dec_r(&mut self, reg: Reg) {
        let res = self.regs.dec_reg(reg);

        self.set_n();
        self.check_z(res);
        self.set_h_to(res & 0xf == 0xf);
    }

    fn dec_rr(&mut self, reg: Reg16) {
        self.regs.dec_reg16(reg);
    }

    fn dec_ind_hl(&mut self, mmu: &mut Mmu) {
        let addr = self.regs.read_reg16(Reg16::HL);
        let val = mmu
            .read_byte(self.regs.read_reg16(Reg16::HL) as usize)
            .wrapping_sub(1);

        mmu.write_byte(self.regs.read_reg16(Reg16::HL) as usize, val);

        self.check_z(val);
        self.set_n();
        self.set_h_to(val & 0xf == 0xf);
    }

    fn ld_a_ind_hl_inc(&mut self, mmu: &mut Mmu) {
        self.regs.write_reg(
            Reg::A,
            mmu.read_byte(self.regs.read_reg16(Reg16::HL) as usize),
        );
        self.regs.inc_reg16(Reg16::HL);
    }

    fn ld_r_ind_rr(&mut self, dst: Reg, src: Reg16, mmu: &mut Mmu) {
        self.regs
            .write_reg(dst, mmu.read_byte(self.regs.read_reg16(src) as usize));
    }

    fn ld_r_ind_r(&mut self, dst: Reg, src: Reg, mmu: &mut Mmu) {
        self.regs.write_reg(
            dst,
            mmu.read_byte(0xff00 + (self.regs.read_reg(src) as usize)),
        );
    }

    fn ld_ind_rr_r(&mut self, dst: Reg16, src: Reg, mmu: &mut Mmu) {
        mmu.write_byte(self.regs.read_reg16(dst) as usize, self.regs.read_reg(src));
    }

    fn ld_ind_hl_d8(&mut self, data: &[u8; 1], mmu: &mut Mmu) {
        mmu.write_byte(self.regs.read_reg16(Reg16::HL) as usize, data[0]);
    }

    fn ld_ind_hl_dec_a(&mut self, mmu: &mut Mmu) {
        self.ld_ind_rr_r(Reg16::HL, Reg::A, mmu);
        self.dec_rr(Reg16::HL);
    }

    fn ld_ind_hl_inc_a(&mut self, mmu: &mut Mmu) {
        self.ld_ind_rr_r(Reg16::HL, Reg::A, mmu);
        self.inc_rr(Reg16::HL);
    }

    fn ld_ind_c_a(&mut self, mmu: &mut Mmu) {
        mmu.write_byte(
            0xff00 + self.regs.read_reg(Reg::C) as usize,
            self.regs.read_reg(Reg::A),
        );
    }

    fn bit_r(&mut self, reg: Reg, n: u8) {
        self.clear_n();
        self.set_h();

        if self.regs.read_reg(reg) & 1 << n != 0 {
            self.clear_z();
        } else {
            self.set_z();
        }
    }

    fn rl_r(&mut self, reg: Reg) {
        let val = self.regs.read_reg(reg);
        let (res, c) = val.overflowing_shl(1);
        self.regs.write_reg(reg, res + val.wrapping_shr(7));

        self.clear_z();
        self.clear_n();
        self.clear_h();
        self.set_c_to(c);
    }

    fn rr_r(&mut self, reg: Reg) {
        let val = self.regs.read_reg(reg);
        let c: u8 = self.c().into();

        let will_carry = val & 0x1 != 0;

        let val = c.wrapping_shl(7) | val.wrapping_shr(1);

        self.clear_n();
        self.clear_h();
        self.check_z(val);
        self.set_c_to(will_carry);

        self.regs.write_reg(reg, val);
    }

    fn srl_r(&mut self, reg: Reg) {
        let (val, c) = self.regs.read_reg(reg).overflowing_shr(1);
        self.regs.write_reg(reg, val);

        self.clear_n();
        self.clear_h();
        self.check_z(val);
        self.set_c_to(c);
    }

    fn swap_r(&mut self, reg: Reg) {
        let r = self.regs.read_reg(reg);
        let val = (r << 4) | (r >> 4);

        self.check_z(val);
        self.clear_c();
        self.clear_h();
        self.clear_n();

        self.regs.write_reg(reg, val);
    }

    fn jr_nc_r8(&mut self, data: &[u8; 1]) {
        if !self.c() {
            self.jr_r8(data);
        }
    }

    fn jr_nz_r8(&mut self, data: &[u8; 1]) {
        if !self.z() {
            self.jr_r8(data);
        }
    }

    fn jr_z_r8(&mut self, data: &[u8; 1]) {
        if self.z() {
            self.jr_r8(data);
        }
    }

    fn jr_c_r8(&mut self, data: &[u8; 1]) {
        if self.c() {
            self.jr_r8(data);
        }
    }

    fn jr_r8(&mut self, data: &[u8; 1]) {
        let offset = data[0] as i8;
        self.regs.write_reg16(
            Reg16::PC,
            self.regs
                .read_reg16(Reg16::PC)
                .wrapping_add_signed(offset as i16),
        );
    }

    fn jp_a16(&mut self, data: &[u8; 2]) {
        self.regs.write_reg16(Reg16::PC, u16::from_le_bytes(*data));
    }

    fn jp_nz_a16(&mut self, data: &[u8; 2]) {
        if !self.z() {
            self.jp_a16(data);
        }
    }

    fn jp_hl(&mut self) {
        self.regs
            .write_reg16(Reg16::PC, self.regs.read_reg16(Reg16::HL));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_inc_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 41);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg($r), 42);
                assert!(!cpu.h());
            }
        };
    }

    macro_rules! test_dec_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 43);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg($r), 42);
                assert!(!cpu.h());
                assert!(cpu.n());
            }
        };
    }

    macro_rules! test_dec_rr {
        ($reg:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg16($reg, 43);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.regs.read_reg16($reg), 42);
            }
        };
    }

    macro_rules! test_ld_r_r {
        ($dst:expr, $src:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($src, 42);
                cpu.regs.write_reg($dst, 0);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg($dst), 42);
            }
        };
    }

    macro_rules! test_ld_r_ind_rr {
        ($src:expr, $dst:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($dst, 0);
                cpu.regs.write_reg16($src, 1);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg($dst), 0xff);
            }
        };
    }

    macro_rules! test_push_rr {
        ($reg:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);
                cpu.regs.write_reg16($reg, 0xfeff);
                cpu.regs.write_reg16(Reg16::SP, 10);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 16);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg16(Reg16::SP), 8);
                assert_eq!(mmu.read_byte(9), 0xfe);
                assert_eq!(mmu.read_byte(8), 0xff);
            }
        };
    }

    macro_rules! test_pop_rr {
        ($reg:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);
                cpu.regs.write_reg16(Reg16::SP, 1);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg16(Reg16::SP), 3);
                assert_eq!(cpu.regs.read_reg16($reg), 0xfffe);
            }
        };
    }

    macro_rules! test_ld_rr_d16 {
        ($reg:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 3);
                assert_eq!(cpu.regs.read_reg16($reg), 0xfffe);
            }
        };
    }

    macro_rules! test_inc_rr {
        ($reg:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg16($reg, 41);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg16($reg), 42);
            }
        };
    }

    macro_rules! test_or_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 0x0f);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 0x0f);
                assert!(!cpu.z());

                cpu.regs.write_reg16(Reg16::PC, 0);
                cpu.regs.write_reg(Reg::A, 0);
                cpu.regs.write_reg($r, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 0);
                assert!(cpu.z());
            }
        };
    }

    macro_rules! test_ld_r_a16 {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);
                mmu.write_byte(0x0005, 42);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 16);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 3);
                assert_eq!(cpu.regs.read_reg($r), 42);
            }
        };
    }

    macro_rules! test_ld_r_d8 {
        ($r: expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert_eq!(cpu.regs.read_reg($r), 0xff);
            }
        };
    }

    macro_rules! test_ld_ind_rr_r {
        ($src:expr, $dst:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($src, 42);
                cpu.regs.write_reg16($dst, 5);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(mmu.read_byte(5), 42);
            }
        };
    }

    macro_rules! test_ld_ind_rr_d8 {
        ($reg:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg16($reg, 5);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert_eq!(mmu.read_byte(5), 0xde);
            }
        };
    }

    macro_rules! test_ld_r_ind_r {
        ($dst:expr, $src:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($src, 0);
                cpu.regs.write_reg($dst, 5);
                mmu.write_slice($mnemonic, 0);
                mmu.write_byte(0xff05, 0xaa);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg($src), 0xaa);
            }
        };
    }

    macro_rules! test_rr_r {
        ($r:expr, $mnemonic:expr, $cycles:expr, $pc:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 2);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, $cycles);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), $pc);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
                assert_eq!(cpu.regs.read_reg($r), 1);
            }
        };
    }

    macro_rules! test_xor_r_r {
        ($x:expr, $y:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($x, 42);
                cpu.regs.write_reg($y, 42);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg($x), 0);
                assert!(cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
            }
        };
    }

    macro_rules! test_add_hl_rr {
        ($reg:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg16($reg, 0x00ad);
                cpu.regs.write_reg16(Reg16::HL, 0xde00);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg16(Reg16::HL), 0xdead);
                assert!(!cpu.z());
                assert!(!cpu.c());
                assert!(!cpu.n());
                assert!(!cpu.h());
            }
        };
    }

    #[test]
    fn test_nop() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x00], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
    }

    #[test]
    fn test_di() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xf3], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert!(!cpu.interrupts_enabled);
    }

    #[test]
    fn test_ei() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xfb], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert!(cpu.interrupts_enabled);
    }

    test_push_rr!(Reg16::BC, &[0xc5], test_push_bc);
    test_push_rr!(Reg16::DE, &[0xd5], test_push_de);
    test_push_rr!(Reg16::HL, &[0xe5], test_push_hl);

    #[test]
    fn test_push_af() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xf5], 0);
        cpu.regs.write_reg16(Reg16::AF, 0xfeff);
        cpu.regs.write_reg16(Reg16::SP, 10);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 8);
        assert_eq!(mmu.read_byte(9), 0xfe);
        assert_eq!(mmu.read_byte(8), 0xf0);
    }

    #[test]
    fn test_call_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xcd, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 10);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 24);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 5);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 8);
        assert_eq!(mmu.read_byte(9), 0x00);
        assert_eq!(mmu.read_byte(8), 0x03);
    }

    #[test]
    fn test_call_nz_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc4, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 10);
        cpu.clear_z();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 24);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 5);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 8);
        assert_eq!(mmu.read_byte(9), 0x00);
        assert_eq!(mmu.read_byte(8), 0x03);
    }

    #[test]
    fn test_ret() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc9, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 1);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 5);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 3);
    }

    #[test]
    fn test_ret_c() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xd8, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 1);
        cpu.set_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 20);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 5);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 3);
    }

    #[test]
    fn test_ret_nc() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xd0, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 1);
        cpu.clear_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 20);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 5);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 3);
    }

    #[test]
    fn test_ret_z() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc8, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 1);
        cpu.set_z();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 20);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 5);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 3);
    }

    #[test]
    fn test_pop_af() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xf1, 0xfe, 0xff], 0);
        cpu.regs.write_reg16(Reg16::SP, 1);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 3);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0xff);
    }

    test_pop_rr!(Reg16::BC, &[0xc1, 0xfe, 0xff], test_pop_bc);
    test_pop_rr!(Reg16::DE, &[0xd1, 0xfe, 0xff], test_pop_de);
    test_pop_rr!(Reg16::HL, &[0xe1, 0xfe, 0xff], test_pop_hl);

    #[test]
    fn test_ld_sp_d16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x31, 0xfe, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 3);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 0xfffe);
    }

    #[test]
    fn test_ld_hl_sp_e8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::SP, 0xdead);
        mmu.write_slice(&[0xf8, 0xfe], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg16(Reg16::HL), 0xdeab);
    }

    #[test]
    fn test_add_sp_e8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::SP, 0xdead);
        mmu.write_slice(&[0xe8, 0xfe], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 0xdeab);
    }

    test_ld_rr_d16!(Reg16::BC, &[0x01, 0xfe, 0xff], test_ld_bc_d16);
    test_ld_rr_d16!(Reg16::DE, &[0x11, 0xfe, 0xff], test_ld_de_d16);
    test_ld_rr_d16!(Reg16::HL, &[0x21, 0xfe, 0xff], test_ld_hl_d16);

    #[test]
    fn test_ld_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x3e, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0xff);
    }

    #[test]
    fn test_ld_b_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x06, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::B), 0xff);
    }

    test_ld_r_r!(Reg::A, Reg::B, &[0x78], test_ld_a_b);
    test_ld_r_r!(Reg::A, Reg::C, &[0x79], test_ld_a_c);
    test_ld_r_r!(Reg::A, Reg::D, &[0x7a], test_ld_a_d);
    test_ld_r_r!(Reg::A, Reg::E, &[0x7b], test_ld_a_e);
    test_ld_r_r!(Reg::A, Reg::H, &[0x7c], test_ld_a_h);
    test_ld_r_r!(Reg::A, Reg::L, &[0x7d], test_ld_a_l);
    test_ld_r_r!(Reg::B, Reg::A, &[0x47], test_ld_b_a);
    test_ld_r_r!(Reg::B, Reg::C, &[0x41], test_ld_b_c);
    test_ld_r_r!(Reg::B, Reg::D, &[0x42], test_ld_b_d);
    test_ld_r_r!(Reg::B, Reg::E, &[0x43], test_ld_b_e);
    test_ld_r_r!(Reg::B, Reg::H, &[0x44], test_ld_b_h);
    test_ld_r_r!(Reg::B, Reg::L, &[0x45], test_ld_b_l);
    test_ld_r_r!(Reg::C, Reg::A, &[0x4f], test_ld_c_a);
    test_ld_r_r!(Reg::C, Reg::B, &[0x48], test_ld_c_b);
    test_ld_r_r!(Reg::D, Reg::A, &[0x57], test_ld_d_a);
    test_ld_r_r!(Reg::D, Reg::L, &[0x55], test_ld_d_l);
    test_ld_r_r!(Reg::E, Reg::A, &[0x5f], test_ld_e_a);
    test_ld_r_r!(Reg::E, Reg::L, &[0x5d], test_ld_e_l);
    test_ld_r_r!(Reg::H, Reg::A, &[0x67], test_ld_h_a);
    test_ld_r_r!(Reg::H, Reg::D, &[0x62], test_ld_h_d);
    test_ld_r_r!(Reg::L, Reg::A, &[0x6f], test_ld_l_a);
    test_ld_r_r!(Reg::L, Reg::E, &[0x6b], test_ld_l_e);

    test_ld_r_d8!(Reg::C, &[0x0e, 0xff], test_ld_c_d8);
    test_ld_r_d8!(Reg::D, &[0x16, 0xff], test_ld_d_d8);
    test_ld_r_d8!(Reg::E, &[0x1e, 0xff], test_ld_e_d8);
    test_ld_r_d8!(Reg::H, &[0x26, 0xff], test_ld_h_d8);
    test_ld_r_d8!(Reg::L, &[0x2e, 0xff], test_ld_l_d8);

    #[test]
    fn test_ld_a16_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 42);
        mmu.write_slice(&[0xea, 0x05, 0x00], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 3);
        assert_eq!(mmu.read_byte(0x0005), 42);
    }

    #[test]
    fn test_ld_a16_sp() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::SP, 0xdead);
        mmu.write_slice(&[0x08, 0x05, 0x00], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 20);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 3);
        assert_eq!(mmu.read_byte(0x0005), 0xad);
        assert_eq!(mmu.read_byte(0x0006), 0xde);
    }

    #[test]
    fn test_ld_sp_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::HL, 0xdead);

        mmu.write_slice(&[0xf9], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 0xdead);
    }

    #[test]
    fn test_ldh_a8_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 42);
        mmu.write_slice(&[0xe0, 0x00], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(mmu.read_byte(0xff00), 42);
    }

    test_ld_r_a16!(Reg::A, &[0xfa, 0x05, 0x00], test_ld_a_a16);

    #[test]
    fn test_ldh_a_a8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 0);
        mmu.write_slice(&[0xf0, 0x00], 0);
        mmu.write_byte(0xff00, 42);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 42);
    }

    test_or_r!(Reg::A, &[0xb7], test_or_a);
    test_or_r!(Reg::B, &[0xb0], test_or_b);
    test_or_r!(Reg::C, &[0xb1], test_or_c);

    #[test]
    fn test_or_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 0xaa);
        mmu.write_slice(&[0xf6, 0x55], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0xff);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
    }

    #[test]
    fn test_or_a_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        cpu.regs.write_reg16(Reg16::HL, 1);
        mmu.write_slice(&[0xb6, 0x5], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 15);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
    }

    #[test]
    fn test_xor_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 42);
        mmu.write_slice(&[0xaf], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0);
        assert!(cpu.z());
    }

    test_xor_r_r!(Reg::A, Reg::C, &[0xa9], test_xor_a_c);
    test_xor_r_r!(Reg::A, Reg::L, &[0xad], test_xor_a_l);

    #[test]
    fn test_xor_a_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        cpu.regs.write_reg16(Reg16::HL, 1);
        mmu.write_slice(&[0xae, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0);
        assert!(cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
    }

    #[test]
    fn test_xor_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        mmu.write_slice(&[0xee, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0);
        assert!(cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
    }

    #[test]
    fn test_add_a_b() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        cpu.regs.write_reg(Reg::B, 10);
        mmu.write_slice(&[0x80], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 20);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    test_add_hl_rr!(Reg16::BC, &[0x09], test_add_hl_bc);
    test_add_hl_rr!(Reg16::DE, &[0x19], test_add_hl_de);
    test_add_hl_rr!(Reg16::SP, &[0x39], test_add_hl_sp);

    #[test]
    fn test_add_hl_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::HL, 0x00ff);
        mmu.write_slice(&[0x29], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg16(Reg16::HL), 0x1fe);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_add_a_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        cpu.regs.write_reg16(Reg16::HL, 1);

        mmu.write_slice(&[0x86, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 20);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_add_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        mmu.write_slice(&[0xc6, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 20);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_adc_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        cpu.set_c();
        mmu.write_slice(&[0xce, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 21);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_and_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 0xf0);
        mmu.write_slice(&[0xe6, 0x0f], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0);
        assert!(cpu.z());
        assert!(cpu.h());

        cpu.regs.write_reg(Reg::A, 0x0f);
        cpu.regs.write_reg16(Reg16::PC, 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0x0f);
        assert!(!cpu.z());
        assert!(cpu.h());
    }

    #[test]
    fn test_sub_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        mmu.write_slice(&[0xd6, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0);
        assert!(cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_sbc_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 20);
        cpu.set_c();
        mmu.write_slice(&[0xde, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 9);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_sub_a_b() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 11);
        cpu.regs.write_reg(Reg::B, 10);
        mmu.write_slice(&[0x90], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 1);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_cp_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        mmu.write_slice(&[0xfe, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(cpu.regs.read_reg(Reg::A), 10);
        assert!(cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_cp_a_e() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        cpu.regs.write_reg(Reg::E, 10);
        mmu.write_slice(&[0xbb], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 10);
        assert!(cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_cp_a_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 10);
        cpu.regs.write_reg16(Reg16::HL, 1);
        mmu.write_slice(&[0xbe, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 10);
        assert!(cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    test_inc_r!(Reg::A, &[0x3c], test_inc_a);
    test_inc_r!(Reg::B, &[0x04], test_inc_b);
    test_inc_r!(Reg::C, &[0x0c], test_inc_c);
    test_inc_r!(Reg::E, &[0x1c], test_inc_e);
    test_inc_r!(Reg::H, &[0x24], test_inc_h);
    test_inc_r!(Reg::L, &[0x2c], test_inc_l);

    test_inc_rr!(Reg16::BC, &[0x03], test_inc_bc);
    test_inc_rr!(Reg16::DE, &[0x13], test_inc_de);
    test_inc_rr!(Reg16::HL, &[0x23], test_inc_hl);

    #[test]
    fn test_inc_sp() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::SP, 0xdeac);
        mmu.write_slice(&[0x33], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 0xdead);
    }

    test_dec_r!(Reg::A, &[0x3d], test_dec_a);
    test_dec_r!(Reg::B, &[0x05], test_dec_b);
    test_dec_r!(Reg::C, &[0x0d], test_dec_c);
    test_dec_r!(Reg::D, &[0x15], test_dec_d);
    test_dec_r!(Reg::E, &[0x1d], test_dec_e);
    test_dec_r!(Reg::H, &[0x25], test_dec_h);
    test_dec_r!(Reg::L, &[0x2d], test_dec_l);

    test_dec_rr!(Reg16::BC, &[0x0b], test_dec_bc);
    test_dec_rr!(Reg16::DE, &[0x1b], test_dec_de);
    test_dec_rr!(Reg16::HL, &[0x2b], test_dec_hl);

    #[test]
    fn test_dec_sp() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::SP, 0xdeae);
        mmu.write_slice(&[0x3b], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 0xdead);
    }

    #[test]
    fn test_dec_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::HL, 1);
        mmu.write_slice(&[0x35, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(mmu.read_byte(1), 9);
        assert!(cpu.n());
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.h());
    }

    #[test]
    fn test_ld_ind_hl_dec_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 42);
        cpu.regs.write_reg16(Reg16::HL, 5);
        mmu.write_slice(&[0x32], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg16(Reg16::HL), 4);
        assert_eq!(mmu.read_byte(5), 42);
    }

    #[test]
    fn test_ld_ind_hl_inc_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 42);
        cpu.regs.write_reg16(Reg16::HL, 5);
        mmu.write_slice(&[0x22], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg16(Reg16::HL), 6);
        assert_eq!(mmu.read_byte(5), 42);
    }

    test_ld_ind_rr_r!(Reg::A, Reg16::DE, &[0x12], test_ld_ind_de_a);
    test_ld_ind_rr_r!(Reg::A, Reg16::HL, &[0x32], test_ld_ind_hl_a);
    test_ld_ind_rr_r!(Reg::B, Reg16::HL, &[0x70], test_ld_ind_hl_b);
    test_ld_ind_rr_r!(Reg::C, Reg16::HL, &[0x71], test_ld_ind_hl_c);
    test_ld_ind_rr_r!(Reg::D, Reg16::HL, &[0x72], test_ld_ind_hl_d);
    test_ld_ind_rr_r!(Reg::E, Reg16::HL, &[0x73], test_ld_ind_hl_e);

    test_ld_ind_rr_d8!(Reg16::HL, &[0x36, 0xde], test_ld_ind_hl_d8);

    #[test]
    fn test_ld_ind_c_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 42);
        cpu.regs.write_reg(Reg::C, 5);
        mmu.write_slice(&[0xe2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(mmu.read_byte(0xff05), 42);
    }

    test_ld_r_ind_rr!(Reg16::DE, Reg::A, &[0x1a, 0xff], test_ld_a_ind_de);
    test_ld_r_ind_rr!(Reg16::HL, Reg::A, &[0x7e, 0xff], test_ld_a_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::B, &[0x46, 0xff], test_ld_b_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::C, &[0x4e, 0xff], test_ld_c_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::D, &[0x56, 0xff], test_ld_d_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::E, &[0x5e, 0xff], test_ld_e_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::H, &[0x66, 0xff], test_ld_h_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::L, &[0x6e, 0xff], test_ld_l_ind_hl);

    test_ld_r_ind_r!(Reg::C, Reg::A, &[0xf2], test_ld_a_ind_c);

    #[test]
    fn test_ld_a_ind_hl_inc() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 0);
        cpu.regs.write_reg16(Reg16::HL, 1);
        mmu.write_slice(&[0x2a, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0xff);
        assert_eq!(cpu.regs.read_reg16(Reg16::HL), 2);
    }

    #[test]
    fn test_bit7h() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::H, 0b11111111);
        cpu.regs.write_reg(Reg::L, 0b11111111);
        mmu.write_slice(&[0xcb, 0x7c], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(cpu.h());

        cpu.regs.write_reg16(Reg16::PC, 0);
        cpu.regs.write_reg(Reg::H, 0b01111111);
        cpu.regs.write_reg(Reg::L, 0b11111111);
        mmu.write_slice(&[0xcb, 0x7c], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert!(cpu.z());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_bit0d() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::D, 0b11111111);
        mmu.write_slice(&[0xcb, 0x42], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(cpu.h());

        cpu.regs.write_reg16(Reg16::PC, 0);
        cpu.regs.write_reg(Reg::D, 0b11111110);
        mmu.write_slice(&[0xcb, 0x42], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert!(cpu.z());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_rl_c() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::C, 1);
        mmu.write_slice(&[0xcb, 0x11], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.regs.read_reg(Reg::C), 2);
    }

    test_rr_r!(Reg::A, &[0x1f], 4, 1, test_rr_a);
    test_rr_r!(Reg::C, &[0xcb, 0x19], 12, 2, test_rr_c);
    test_rr_r!(Reg::D, &[0xcb, 0x1a], 12, 2, test_rr_d);
    test_rr_r!(Reg::E, &[0xcb, 0x1b], 12, 2, test_rr_e);

    #[test]
    fn test_srl_b() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::B, 2);
        mmu.write_slice(&[0xcb, 0x38], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.regs.read_reg(Reg::B), 1);
    }

    #[test]
    fn test_swap_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 0xab);
        mmu.write_slice(&[0xcb, 0x37], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.regs.read_reg(Reg::A), 0xba);
    }

    #[test]
    fn test_rl_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 1);
        mmu.write_slice(&[0x17], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.regs.read_reg(Reg::A), 0x2);
    }

    #[test]
    fn test_jr_nz_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x20, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 4);

        cpu.regs.write_reg16(Reg16::PC, 0);
        cpu.set_z();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
    }

    #[test]
    fn test_jr_nc_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x30, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 4);

        cpu.regs.write_reg16(Reg16::PC, 0);
        cpu.set_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
    }

    #[test]
    fn test_jr_z_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x28, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);

        cpu.regs.write_reg16(Reg16::PC, 0);
        cpu.set_z();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 4);
    }

    #[test]
    fn test_jr_c_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x38, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);

        cpu.regs.write_reg16(Reg16::PC, 0);
        cpu.set_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 4);
    }

    #[test]
    fn test_jr_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x18, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 4);
    }

    #[test]
    fn test_jp_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc3, 0x05, 0x0a], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 0x0a05);
    }

    #[test]
    fn test_jp_nz_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.clear_z();
        mmu.write_slice(&[0xc2, 0x05, 0x0a], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 0x0a05);
    }

    #[test]
    fn test_jp_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::HL, 0x0a05);
        mmu.write_slice(&[0xe9], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 0x0a05);
    }

    #[test]
    fn test_cpl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 0xaa);

        mmu.write_slice(&[0x2f], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0x55);
    }

    #[test]
    fn test_scf() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.clear_c();

        mmu.write_slice(&[0x37], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert!(cpu.c());
    }

    #[test]
    fn test_integration_pop_af() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();

        mmu.write_slice(
            &[
                0x01, 0x00, 0x12, 0xc5, 0xf1, 0xf5, 0xd1, 0x79, 0xe6, 0xf0, 0xbb, 0xc2, 0xad, 0xde,
            ],
            0,
        );
        cpu.regs.write_reg16(Reg16::SP, 0xff);

        for _ in 0..9 {
            cpu.exec_instruction(&mut mmu);
        }

        assert_ne!(cpu.regs.read_reg16(Reg16::PC), 0xdead);
        assert_eq!(cpu.regs.read_reg(Reg::A), cpu.regs.read_reg(Reg::A));
    }
}
