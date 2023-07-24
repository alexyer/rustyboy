use std::fmt::Display;

use crate::{
    errors::InstructionError,
    instruction::{Instruction, InstructionType, Opcode, PrefixedOpcode},
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
                let regs = instruction.regs().clone().unwrap_or_default();

                match instruction.instruction_type() {
                    InstructionType::Nop => (),
                    InstructionType::Daa => self.daa(),
                    InstructionType::LdRrD16 => {
                        self.ld_rr_d16(regs[0].into(), &prepare_data!(instruction, 2))
                    }
                    InstructionType::LdRD8 => {
                        self.ld_r_d8(regs[0].into(), &prepare_data!(instruction, 1))
                    }
                    InstructionType::LdAA16 => self.ld_a_a16(&prepare_data!(instruction, 2), &mmu),
                    InstructionType::LdRR => self.ld_r_r(regs[0].into(), regs[1].into()),
                    InstructionType::LdAA8 => self.ldh_a_a8(&prepare_data!(instruction, 1), mmu),
                    InstructionType::LdA8A => self.ldh_a8_a(&prepare_data!(instruction, 1), mmu),
                    InstructionType::LdAIndC => self.ld_r_ind_r(Reg::A, Reg::C, mmu),
                    InstructionType::LdRIndRR => {
                        self.ld_r_ind_rr(regs[0].into(), regs[1].into(), mmu)
                    }
                    InstructionType::LdIndRrR => {
                        self.ld_ind_rr_r(regs[0].into(), regs[1].into(), mmu)
                    }
                    InstructionType::PushRr => self.push_rr(regs[0].into(), mmu),
                    InstructionType::PopRr => self.pop_rr(regs[0].into(), mmu),
                    InstructionType::LdA16A => self.ld_a16_a(&prepare_data!(instruction, 2), mmu),
                    InstructionType::LdA16Sp => self.ld_a16_sp(&prepare_data!(instruction, 2), mmu),
                    InstructionType::LdIndHlD8 => {
                        self.ld_ind_hl_d8(&prepare_data!(instruction, 1), mmu)
                    }
                    InstructionType::LdIndHlDecA => self.ld_ind_hl_dec_a(mmu),
                    InstructionType::LdIndHLIncA => self.ld_ind_hl_inc_a(mmu),
                    InstructionType::LdAIndHLInc => self.ld_a_ind_hl_inc(mmu),
                    InstructionType::LdIndCA => self.ld_ind_c_a(mmu),
                    InstructionType::LdHLSPE8 => self.ld_hl_sp_e8(&prepare_data!(instruction, 1)),
                    InstructionType::LdRrRr => match instruction.opcode() {
                        Opcode::LdSPHL => self.ld_sp_hl(),
                        _ => unreachable!(),
                    },
                    InstructionType::IncR => self.inc_r(regs[0].into()),
                    InstructionType::IncRr => self.inc_rr(regs[0].into()),
                    InstructionType::DecR => self.dec_r(regs[0].into()),
                    InstructionType::DecRr => self.dec_rr(regs[0].into()),
                    InstructionType::DecIndHl => self.dec_ind_hl(mmu),
                    InstructionType::AdcR => self.adc_r(regs[0].into()),
                    InstructionType::AdcD8 => self.adc_d8(&prepare_data!(instruction, 1)),
                    InstructionType::AddR => self.add_r(regs[0].into()),
                    InstructionType::AddD8 => self.add_d8(&prepare_data!(instruction, 1)),
                    InstructionType::AddRrRr => self.add_hl_rr(regs[1].into()),
                    InstructionType::AddSpE8 => self.add_sp_e8(&prepare_data!(instruction, 1)),
                    InstructionType::AddAIndHl => self.add_a_ind_hl(mmu),
                    InstructionType::AndR => self.and_r(regs[0].into()),
                    InstructionType::AndD8 => self.and_d8(&prepare_data!(instruction, 1)),
                    InstructionType::SbcR => self.sbc_r(regs[0].into()),
                    InstructionType::SbcD8 => self.sbc_d8(&prepare_data!(instruction, 1)),
                    InstructionType::SubD8 => self.sub_d8(&prepare_data!(instruction, 1)),
                    InstructionType::SubR => self.sub_r(regs[0].into()),
                    InstructionType::OrR => self.or_r(regs[0].into()),
                    InstructionType::OrAIndHl => self.or_a_ind_hl(mmu),
                    InstructionType::OrD8 => self.or_d8(&prepare_data!(instruction, 1)),
                    InstructionType::XorR => self.xor_r(regs[0].into()),
                    InstructionType::XorD8 => self.xor_d8(&prepare_data!(instruction, 1)),
                    InstructionType::XorAIndHl => self.xor_a_ind_hl(mmu),
                    InstructionType::CpR => self.cp_r(regs[0].into()),
                    InstructionType::CpD8 => self.cp_d8(&prepare_data!(instruction, 1)),
                    InstructionType::CpAIndHl => self.cp_a_ind_hl(mmu),
                    InstructionType::Cpl => self.cpl(),
                    InstructionType::Ccf => self.ccf(),
                    InstructionType::Scf => self.scf(),
                    InstructionType::Jr => match instruction.opcode() {
                        Opcode::JrR8 => self.jr_r8(&prepare_data!(instruction, 1)),
                        Opcode::JrCR8 => self.jr_c_r8(&prepare_data!(instruction, 1)),
                        Opcode::JrNcR8 => self.jr_nc_r8(&prepare_data!(instruction, 1)),
                        Opcode::JrNzR8 => self.jr_nz_r8(&prepare_data!(instruction, 1)),
                        Opcode::JrZR8 => self.jr_z_r8(&prepare_data!(instruction, 1)),
                        _ => unreachable!(),
                    },
                    InstructionType::Jp => match instruction.opcode() {
                        Opcode::JpA16 => self.jp_a16(&prepare_data!(instruction, 2)),
                        Opcode::JpHL => self.jp_hl(),
                        Opcode::JpNcA16 => self.jp_nc_a16(&prepare_data!(instruction, 2)),
                        Opcode::JpNzA16 => self.jp_nz_a16(&prepare_data!(instruction, 2)),
                        Opcode::JpCA16 => self.jp_c_a16(&prepare_data!(instruction, 2)),
                        Opcode::JpZA16 => self.jp_z_a16(&prepare_data!(instruction, 2)),
                        _ => unreachable!(),
                    },
                    InstructionType::Call => match instruction.opcode() {
                        Opcode::CallA16 => self.call_a16(&prepare_data!(instruction, 2), mmu),
                        Opcode::CallCA16 => self.call_c_a16(&prepare_data!(instruction, 2), mmu),
                        Opcode::CallZA16 => self.call_z_a16(&prepare_data!(instruction, 2), mmu),
                        Opcode::CallNcA16 => self.call_nc_a16(&prepare_data!(instruction, 2), mmu),
                        Opcode::CallNzA16 => self.call_nz_a16(&prepare_data!(instruction, 2), mmu),
                        _ => unreachable!(),
                    },
                    InstructionType::Ret => match instruction.opcode() {
                        Opcode::Ret => self.ret(mmu),
                        Opcode::Reti => self.reti(mmu),
                        Opcode::RetC => self.ret_c(mmu),
                        Opcode::RetNc => self.ret_nc(mmu),
                        Opcode::RetZ => self.ret_z(mmu),
                        Opcode::RetNz => self.ret_nz(mmu),
                        _ => unreachable!(),
                    },
                    InstructionType::Rst => match instruction.opcode() {
                        Opcode::Rst00 => self.call_a16(&[0x00, 0x00], mmu),
                        _ => unreachable!(),
                    },
                    InstructionType::Di => self.di(),
                    InstructionType::Ei => self.ei(),
                    InstructionType::Bit0R => self.bit_r(regs[0].into(), 0),
                    InstructionType::Bit7R => self.bit_r(regs[0].into(), 7),
                    InstructionType::RlR => self.rl_r(regs[0].into()),
                    InstructionType::RlcR => self.rlc_r(regs[0].into()),
                    InstructionType::RrR => self.rr_r(regs[0].into()),
                    InstructionType::RrcR => self.rrc_r(regs[0].into()),
                    InstructionType::SrlR => self.srl_r(regs[0].into()),
                    InstructionType::SwapR => self.swap_r(regs[0].into()),
                }

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
            Opcode::LdAA => Some(Instruction::ld_a_a(&[])),
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
            Opcode::LdCC => Some(Instruction::ld_c_c(&[])),
            Opcode::LdCD => Some(Instruction::ld_c_d(&[])),
            Opcode::LdCE => Some(Instruction::ld_c_e(&[])),
            Opcode::LdCH => Some(Instruction::ld_c_h(&[])),
            Opcode::LdCL => Some(Instruction::ld_c_l(&[])),
            Opcode::LdDA => Some(Instruction::ld_d_a(&[])),
            Opcode::LdDB => Some(Instruction::ld_d_b(&[])),
            Opcode::LdDC => Some(Instruction::ld_d_c(&[])),
            Opcode::LdDD => Some(Instruction::ld_d_d(&[])),
            Opcode::LdDE => Some(Instruction::ld_d_e(&[])),
            Opcode::LdDH => Some(Instruction::ld_d_h(&[])),
            Opcode::LdDL => Some(Instruction::ld_d_l(&[])),
            Opcode::LdEA => Some(Instruction::ld_e_a(&[])),
            Opcode::LdEB => Some(Instruction::ld_e_b(&[])),
            Opcode::LdEC => Some(Instruction::ld_e_c(&[])),
            Opcode::LdED => Some(Instruction::ld_e_d(&[])),
            Opcode::LdEE => Some(Instruction::ld_e_e(&[])),
            Opcode::LdEH => Some(Instruction::ld_e_h(&[])),
            Opcode::LdEL => Some(Instruction::ld_e_l(&[])),
            Opcode::LdHA => Some(Instruction::ld_h_a(&[])),
            Opcode::LdHB => Some(Instruction::ld_h_b(&[])),
            Opcode::LdHC => Some(Instruction::ld_h_c(&[])),
            Opcode::LdHD => Some(Instruction::ld_h_d(&[])),
            Opcode::LdHE => Some(Instruction::ld_h_e(&[])),
            Opcode::LdHH => Some(Instruction::ld_h_h(&[])),
            Opcode::LdHL => Some(Instruction::ld_h_l(&[])),
            Opcode::LdLA => Some(Instruction::ld_l_a(&[])),
            Opcode::LdLB => Some(Instruction::ld_l_b(&[])),
            Opcode::LdLC => Some(Instruction::ld_l_c(&[])),
            Opcode::LdLD => Some(Instruction::ld_l_d(&[])),
            Opcode::LdLE => Some(Instruction::ld_l_e(&[])),
            Opcode::LdLH => Some(Instruction::ld_l_h(&[])),
            Opcode::LdLL => Some(Instruction::ld_l_l(&[])),
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
            Opcode::OrD => Some(Instruction::or_d(&[])),
            Opcode::OrE => Some(Instruction::or_e(&[])),
            Opcode::OrH => Some(Instruction::or_h(&[])),
            Opcode::OrL => Some(Instruction::or_l(&[])),
            Opcode::OrD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::or_d8(data))
            }
            Opcode::XorA => Some(Instruction::xor_a(&[])),
            Opcode::XorB => Some(Instruction::xor_b(&[])),
            Opcode::XorC => Some(Instruction::xor_c(&[])),
            Opcode::XorD => Some(Instruction::xor_d(&[])),
            Opcode::XorE => Some(Instruction::xor_e(&[])),
            Opcode::XorH => Some(Instruction::xor_h(&[])),
            Opcode::XorL => Some(Instruction::xor_l(&[])),
            Opcode::XorD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::xor_d8(data))
            }
            Opcode::XorAIndHL => Some(Instruction::xor_a_ind_hl(&[])),
            Opcode::AndA => Some(Instruction::and_a(&[])),
            Opcode::AndB => Some(Instruction::and_b(&[])),
            Opcode::AndC => Some(Instruction::and_c(&[])),
            Opcode::AndD => Some(Instruction::and_d(&[])),
            Opcode::AndE => Some(Instruction::and_e(&[])),
            Opcode::AndH => Some(Instruction::and_h(&[])),
            Opcode::AndL => Some(Instruction::and_l(&[])),
            Opcode::AndD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::and_d8(data))
            }
            Opcode::AdcA => Some(Instruction::adc_a(&[])),
            Opcode::AdcB => Some(Instruction::adc_b(&[])),
            Opcode::AdcC => Some(Instruction::adc_c(&[])),
            Opcode::AdcD => Some(Instruction::adc_d(&[])),
            Opcode::AdcE => Some(Instruction::adc_e(&[])),
            Opcode::AdcH => Some(Instruction::adc_h(&[])),
            Opcode::AdcL => Some(Instruction::adc_l(&[])),
            Opcode::AdcD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::adc_d8(data))
            }
            Opcode::AddA => Some(Instruction::add_a(&[])),
            Opcode::AddB => Some(Instruction::add_b(&[])),
            Opcode::AddC => Some(Instruction::add_c(&[])),
            Opcode::AddD => Some(Instruction::add_d(&[])),
            Opcode::AddE => Some(Instruction::add_e(&[])),
            Opcode::AddH => Some(Instruction::add_h(&[])),
            Opcode::AddL => Some(Instruction::add_l(&[])),
            Opcode::AddHLBC => Some(Instruction::add_hl_bc(&[])),
            Opcode::AddHLDE => Some(Instruction::add_hl_de(&[])),
            Opcode::AddHLHL => Some(Instruction::add_hl_hl(&[])),
            Opcode::AddHLSP => Some(Instruction::add_hl_sp(&[])),
            Opcode::AddAIndHL => Some(Instruction::add_a_ind_hl(&[])),
            Opcode::AddD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::add_d8(data))
            }
            Opcode::CpA => Some(Instruction::cp_a(&[])),
            Opcode::CpB => Some(Instruction::cp_b(&[])),
            Opcode::CpC => Some(Instruction::cp_c(&[])),
            Opcode::CpD => Some(Instruction::cp_d(&[])),
            Opcode::CpE => Some(Instruction::cp_e(&[])),
            Opcode::CpH => Some(Instruction::cp_h(&[])),
            Opcode::CpL => Some(Instruction::cp_l(&[])),
            Opcode::CpD8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(Instruction::cp_d8(data))
            }
            Opcode::CpAIndHL => Some(Instruction::cp_a_ind_hl(&[])),
            Opcode::SbcA => Some(Instruction::sbc_a(&[])),
            Opcode::SbcB => Some(Instruction::sbc_b(&[])),
            Opcode::SbcC => Some(Instruction::sbc_c(&[])),
            Opcode::SbcD => Some(Instruction::sbc_d(&[])),
            Opcode::SbcE => Some(Instruction::sbc_e(&[])),
            Opcode::SbcH => Some(Instruction::sbc_h(&[])),
            Opcode::SbcL => Some(Instruction::sbc_l(&[])),
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
            Opcode::SubA => Some(Instruction::sub_a(&[])),
            Opcode::SubB => Some(Instruction::sub_b(&[])),
            Opcode::SubC => Some(Instruction::sub_c(&[])),
            Opcode::SubD => Some(Instruction::sub_d(&[])),
            Opcode::SubE => Some(Instruction::sub_e(&[])),
            Opcode::SubH => Some(Instruction::sub_h(&[])),
            Opcode::SubL => Some(Instruction::sub_l(&[])),
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
            Opcode::LdIndHLH => Some(Instruction::ld_ind_hl_h(&[])),
            Opcode::LdIndHLL => Some(Instruction::ld_ind_hl_l(&[])),
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
            Opcode::JpNcA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::jp_nc_a16(&data))
            }
            Opcode::JpNzA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::jp_nz_a16(&data))
            }
            Opcode::JpCA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::jp_c_a16(&data))
            }
            Opcode::JpZA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::jp_z_a16(&data))
            }
            Opcode::JpHL => Some(Instruction::jp_hl(&[])),
            Opcode::CallA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::call_a16(&data))
            }
            Opcode::CallCA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::call_c_a16(&data))
            }
            Opcode::CallZA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::call_z_a16(&data))
            }
            Opcode::CallNcA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::call_nc_a16(&data))
            }
            Opcode::CallNzA16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(Instruction::call_nz_a16(&data))
            }
            Opcode::Ret => Some(Instruction::ret(&[])),
            Opcode::Reti => Some(Instruction::reti(&[])),
            Opcode::RetC => Some(Instruction::ret_c(&[])),
            Opcode::RetNc => Some(Instruction::ret_nc(&[])),
            Opcode::RetZ => Some(Instruction::ret_z(&[])),
            Opcode::RetNz => Some(Instruction::ret_nz(&[])),
            Opcode::Rst00 => Some(Instruction::rst00(&[])),
            Opcode::RlA => Some(Instruction::rl_a(&[])),
            Opcode::RrA => Some(Instruction::rr_a(&[])),
            Opcode::Cpl => Some(Instruction::cpl(&[])),
            Opcode::Ccf => Some(Instruction::ccf(&[])),
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
            Opcode::RlcA => Some(Instruction::rlc_a(&[])),
            Opcode::RrcA => Some(Instruction::rrc_a(&[])),
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
    impl_flag!(set_n, clear_n, _set_n_to, n, 6);
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

    fn call_c_a16(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        if self.c() {
            self.call_a16(data, mmu);
        }
    }

    fn call_z_a16(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        if self.z() {
            self.call_a16(data, mmu);
        }
    }

    fn call_nc_a16(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        if !self.c() {
            self.call_a16(data, mmu);
        }
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
        self.regs.inc_reg16(Reg16::SP);

        self.regs
            .write_reg16(Reg16::PC, u16::from_le_bytes([ll, hh]));
    }

    fn reti(&mut self, mmu: &mut Mmu) {
        self.interrupts_enabled = true;
        self.ret(mmu)
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

    fn ret_nz(&mut self, mmu: &mut Mmu) {
        if !self.z() {
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

    fn ccf(&mut self) {
        self.set_c_to(!self.c());
        self.clear_n();
        self.clear_h();
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

    fn and_r(&mut self, reg: Reg) {
        let val = self.and8(self.regs.read_reg(Reg::A), self.regs.read_reg(reg));
        self.regs.write_reg(Reg::A, val);
    }

    fn and_d8(&mut self, data: &[u8; 1]) {
        let val = self.and8(self.regs.read_reg(Reg::A), data[0]);
        self.regs.write_reg(Reg::A, val);
    }

    fn and8(&mut self, x: u8, y: u8) -> u8 {
        let val = x & y;

        self.set_z_to(val == 0);
        self.set_h();
        self.clear_n();
        self.clear_c();

        val
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

    fn xor_d8(&mut self, data: &[u8; 1]) {
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

    fn add_d8(&mut self, data: &[u8; 1]) {
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

    fn adc_d8(&mut self, data: &[u8; 1]) {
        let res = self.adc8(self.regs.read_reg(Reg::A), data[0]);
        self.regs.write_reg(Reg::A, res);
    }

    fn adc_r(&mut self, reg: Reg) {
        let res = self.adc8(self.regs.read_reg(Reg::A), self.regs.read_reg(reg));
        self.regs.write_reg(Reg::A, res);
    }

    fn adc8(&mut self, x: u8, y: u8) -> u8 {
        let mut res;
        let first_c;
        let second_c;

        (res, first_c) = x.overflowing_add(self.c().into());
        (res, second_c) = res.overflowing_add(y);

        let carry_u8: u8 = self.c().into();

        self.clear_n();
        self.check_z(res);
        self.set_c_to(first_c || second_c);
        self.set_h_to((x & 0xf) + (y & 0xf) + carry_u8 > 0xf);

        res
    }

    fn sbc_r(&mut self, reg: Reg) {
        let val = self.sbc8(self.regs.read_reg(Reg::A), self.regs.read_reg(reg));
        self.regs.write_reg(Reg::A, val);
    }

    fn sbc_d8(&mut self, data: &[u8; 1]) {
        let val = self.sbc8(self.regs.read_reg(Reg::A), data[0]);
        self.regs.write_reg(Reg::A, val);
    }

    fn sbc8(&mut self, x: u8, y: u8) -> u8 {
        let mut val;
        let first_c;
        let second_c;

        (val, first_c) = x.overflowing_sub(self.c().into());
        (val, second_c) = val.overflowing_sub(y);

        let carry_u8: u8 = self.c().into();

        let (r, first_h) = (x & 0xf).overflowing_sub(y & 0xf);
        let (_, second_h) = r.overflowing_sub(carry_u8);

        self.set_n();
        self.check_z(val);
        self.set_c_to(first_c || second_c);
        self.set_h_to(first_h || second_h);

        val
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
        let addr = self.regs.read_reg16(Reg16::HL) as usize;
        let val = mmu.read_byte(addr).wrapping_sub(1);

        mmu.write_byte(addr, val);

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
        let carry: u8 = self.c().into();
        let will_carry = val & 0b10000000 != 0;

        let val = val.wrapping_shl(1) | carry;
        self.regs.write_reg(reg, val);

        match reg {
            Reg::A => self.clear_z(),
            _ => self.check_z(val),
        }

        self.clear_n();
        self.clear_h();
        self.set_c_to(will_carry);
    }

    fn rlc_r(&mut self, reg: Reg) {
        let val = self.rlc8(self.regs.read_reg(reg));
        self.regs.write_reg(reg, val);

        match reg {
            Reg::A => self.clear_z(),
            _ => (),
        }
    }

    fn rlc8(&mut self, x: u8) -> u8 {
        let carry = x & 0b10000000 != 0;
        let truncated_bit = (x & 0b10000000) >> 7;
        let res = x.wrapping_shl(1) | truncated_bit;

        self.set_c_to(carry);
        self.check_z(res);
        self.clear_h();
        self.clear_n();

        res
    }

    fn rr_r(&mut self, reg: Reg) {
        let val = self.regs.read_reg(reg);
        let c: u8 = self.c().into();

        let will_carry = val & 0x1 != 0;

        let val = c.wrapping_shl(7) | val.wrapping_shr(1);

        match reg {
            Reg::A => self.clear_z(),
            _ => self.check_z(val),
        }

        self.clear_n();
        self.clear_h();
        self.set_c_to(will_carry);

        self.regs.write_reg(reg, val);
    }

    fn rrc_r(&mut self, reg: Reg) {
        let val = self.rrc8(self.regs.read_reg(reg));
        self.regs.write_reg(reg, val);

        match reg {
            Reg::A => self.clear_z(),
            _ => (),
        }
    }

    fn rrc8(&mut self, x: u8) -> u8 {
        let carry = x & 0b00000001 != 0;
        let truncated_bit = x & 0b00000001;
        let res = (x >> 1) | (truncated_bit << 7);

        self.set_c_to(carry);
        self.check_z(res);
        self.clear_h();
        self.clear_n();

        res
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

    fn jp_nc_a16(&mut self, data: &[u8; 2]) {
        if !self.c() {
            self.jp_a16(data);
        }
    }

    fn jp_nz_a16(&mut self, data: &[u8; 2]) {
        if !self.z() {
            self.jp_a16(data);
        }
    }

    fn jp_c_a16(&mut self, data: &[u8; 2]) {
        if self.c() {
            self.jp_a16(data);
        }
    }

    fn jp_z_a16(&mut self, data: &[u8; 2]) {
        if self.z() {
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

    macro_rules! test_rrc_r {
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

    macro_rules! test_xor_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg(Reg::A, 42);
                cpu.regs.write_reg($r, 42);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 0);
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

    macro_rules! test_sub_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg(Reg::A, 11);
                cpu.regs.write_reg($r, 10);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 1);
                assert!(!cpu.z());
                assert!(!cpu.c());
                assert!(cpu.n());
                assert!(!cpu.h());
            }
        };
    }

    macro_rules! test_adc_r {
        ($r:expr, $mnemonic:expr,  $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg(Reg::A, 10);
                cpu.regs.write_reg($r, 10);
                cpu.set_c();
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 21);
                assert!(!cpu.z());
                assert!(!cpu.c());
                assert!(!cpu.n());
                assert!(cpu.h());
            }
        };
    }

    macro_rules! test_rlc_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 1);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
                assert_eq!(cpu.regs.read_reg($r), 2);
            }
        };
    }

    macro_rules! test_and_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg(Reg::A, 0xf0);
                cpu.regs.write_reg($r, 0x0f);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 0);
                assert!(cpu.z());
                assert!(cpu.h());

                cpu.regs.write_reg(Reg::A, 0x0f);
                cpu.regs.write_reg16(Reg16::PC, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 0x0f);
                assert!(!cpu.z());
                assert!(cpu.h());
            }
        };
    }

    macro_rules! test_sbc_r {
        ($r:expr, $mnemonic:expr,  $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg(Reg::A, 11);
                cpu.regs.write_reg($r, 10);
                cpu.set_c();
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 0);
                assert!(cpu.z());
                assert!(!cpu.c());
                assert!(cpu.n());
                assert!(!cpu.h());
            }
        };
    }

    macro_rules! test_add_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg(Reg::A, 10);
                cpu.regs.write_reg($r, 10);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 20);
                assert!(!cpu.z());
                assert!(!cpu.c());
                assert!(!cpu.n());
                assert!(cpu.h());
            }
        };
    }

    macro_rules! test_cp_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg(Reg::A, 10);
                cpu.regs.write_reg($r, 10);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
                assert_eq!(cpu.regs.read_reg(Reg::A), 10);
                assert!(cpu.z());
                assert!(!cpu.c());
                assert!(cpu.n());
                assert!(!cpu.h());
            }
        };
    }

    macro_rules! test_rst_d8 {
        ($d8:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 16);
                assert_eq!(
                    cpu.regs.read_reg16(Reg16::PC),
                    u16::from_le_bytes([$d8, 0x00])
                );
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
    fn test_call_nc_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xd4, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 10);
        cpu.clear_c();

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
    fn test_call_c_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xdc, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 10);
        cpu.set_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 24);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 5);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 8);
        assert_eq!(mmu.read_byte(9), 0x00);
        assert_eq!(mmu.read_byte(8), 0x03);
    }

    #[test]
    fn test_call_z_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xcc, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 10);
        cpu.set_z();

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
    fn test_reti() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xd9, 0x05, 0x00], 0);
        cpu.interrupts_enabled = false;
        cpu.regs.write_reg16(Reg16::SP, 1);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 5);
        assert_eq!(cpu.regs.read_reg16(Reg16::SP), 3);
        assert!(cpu.interrupts_enabled);
    }

    test_rst_d8!(0x00, &[0xc7], test_rst_00);

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
    fn test_ret_nz() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc0, 0x05, 0x00], 0);
        cpu.regs.write_reg16(Reg16::SP, 1);
        cpu.clear_z();

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
    test_ld_r_r!(Reg::C, Reg::D, &[0x4a], test_ld_c_d);
    test_ld_r_r!(Reg::C, Reg::E, &[0x4b], test_ld_c_e);
    test_ld_r_r!(Reg::C, Reg::H, &[0x4c], test_ld_c_h);
    test_ld_r_r!(Reg::C, Reg::L, &[0x4d], test_ld_c_l);
    test_ld_r_r!(Reg::D, Reg::A, &[0x57], test_ld_d_a);
    test_ld_r_r!(Reg::D, Reg::B, &[0x50], test_ld_d_b);
    test_ld_r_r!(Reg::D, Reg::C, &[0x51], test_ld_d_c);
    test_ld_r_r!(Reg::D, Reg::E, &[0x53], test_ld_d_e);
    test_ld_r_r!(Reg::D, Reg::H, &[0x54], test_ld_d_h);
    test_ld_r_r!(Reg::D, Reg::L, &[0x55], test_ld_d_l);
    test_ld_r_r!(Reg::E, Reg::A, &[0x5f], test_ld_e_a);
    test_ld_r_r!(Reg::E, Reg::B, &[0x58], test_ld_e_b);
    test_ld_r_r!(Reg::E, Reg::C, &[0x59], test_ld_e_c);
    test_ld_r_r!(Reg::E, Reg::D, &[0x5a], test_ld_e_d);
    test_ld_r_r!(Reg::E, Reg::H, &[0x5c], test_ld_e_h);
    test_ld_r_r!(Reg::E, Reg::L, &[0x5d], test_ld_e_l);
    test_ld_r_r!(Reg::H, Reg::A, &[0x67], test_ld_h_a);
    test_ld_r_r!(Reg::H, Reg::B, &[0x60], test_ld_h_b);
    test_ld_r_r!(Reg::H, Reg::C, &[0x61], test_ld_h_c);
    test_ld_r_r!(Reg::H, Reg::D, &[0x62], test_ld_h_d);
    test_ld_r_r!(Reg::H, Reg::E, &[0x63], test_ld_h_e);
    test_ld_r_r!(Reg::H, Reg::L, &[0x65], test_ld_h_l);
    test_ld_r_r!(Reg::L, Reg::A, &[0x6f], test_ld_l_a);
    test_ld_r_r!(Reg::L, Reg::B, &[0x68], test_ld_l_b);
    test_ld_r_r!(Reg::L, Reg::C, &[0x69], test_ld_l_c);
    test_ld_r_r!(Reg::L, Reg::D, &[0x6a], test_ld_l_d);
    test_ld_r_r!(Reg::L, Reg::E, &[0x6b], test_ld_l_e);
    test_ld_r_r!(Reg::L, Reg::H, &[0x6c], test_ld_l_h);

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
    test_or_r!(Reg::D, &[0xb2], test_or_d);
    test_or_r!(Reg::E, &[0xb3], test_or_e);
    test_or_r!(Reg::H, &[0xb4], test_or_h);
    test_or_r!(Reg::L, &[0xb5], test_or_l);

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

    test_xor_r!(Reg::B, &[0xa8], test_xor_b);
    test_xor_r!(Reg::C, &[0xa9], test_xor_c);
    test_xor_r!(Reg::D, &[0xaa], test_xor_d);
    test_xor_r!(Reg::E, &[0xab], test_xor_e);
    test_xor_r!(Reg::H, &[0xac], test_xor_h);
    test_xor_r!(Reg::L, &[0xad], test_xor_l);

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

    test_add_r!(Reg::B, &[0x80], test_add_b);
    test_add_r!(Reg::C, &[0x81], test_add_c);
    test_add_r!(Reg::D, &[0x82], test_add_d);
    test_add_r!(Reg::E, &[0x83], test_add_e);
    test_add_r!(Reg::H, &[0x84], test_add_h);
    test_add_r!(Reg::L, &[0x85], test_add_l);

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

    test_adc_r!(Reg::B, &[0x88], test_adc_b);
    test_adc_r!(Reg::C, &[0x89], test_adc_c);
    test_adc_r!(Reg::D, &[0x8a], test_adc_d);
    test_adc_r!(Reg::E, &[0x8b], test_adc_e);
    test_adc_r!(Reg::H, &[0x8c], test_adc_h);
    test_adc_r!(Reg::L, &[0x8d], test_adc_l);

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

    test_and_r!(Reg::B, &[0xa0], test_and_b);
    test_and_r!(Reg::C, &[0xa1], test_and_c);
    test_and_r!(Reg::D, &[0xa2], test_and_d);
    test_and_r!(Reg::E, &[0xa3], test_and_e);
    test_and_r!(Reg::H, &[0xa4], test_and_h);
    test_and_r!(Reg::L, &[0xa5], test_and_l);

    #[test]
    fn test_and_d8() {
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

    test_sbc_r!(Reg::B, &[0x98], test_sbc_b);
    test_sbc_r!(Reg::C, &[0x99], test_sbc_c);
    test_sbc_r!(Reg::D, &[0x9a], test_sbc_d);
    test_sbc_r!(Reg::E, &[0x9b], test_sbc_e);
    test_sbc_r!(Reg::H, &[0x9c], test_sbc_h);
    test_sbc_r!(Reg::L, &[0x9d], test_sbc_l);

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

    test_sub_r!(Reg::B, &[0x90], test_sub_b);
    test_sub_r!(Reg::C, &[0x91], test_sub_c);
    test_sub_r!(Reg::D, &[0x92], test_sub_d);
    test_sub_r!(Reg::E, &[0x93], test_sub_e);
    test_sub_r!(Reg::H, &[0x94], test_sub_h);
    test_sub_r!(Reg::L, &[0x95], test_sub_l);

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

    test_cp_r!(Reg::A, &[0xbf], test_cp_a);
    test_cp_r!(Reg::B, &[0xb8], test_cp_b);
    test_cp_r!(Reg::C, &[0xb9], test_cp_c);
    test_cp_r!(Reg::D, &[0xba], test_cp_d);
    test_cp_r!(Reg::E, &[0xbb], test_cp_e);
    test_cp_r!(Reg::H, &[0xbc], test_cp_h);
    test_cp_r!(Reg::L, &[0xbd], test_cp_l);

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

    test_rlc_r!(Reg::A, &[0x07], test_rlc_a);

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

    test_rrc_r!(Reg::A, &[0x0f], 4, 1, test_rrc_a);

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
    fn test_jp_c_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.set_c();
        mmu.write_slice(&[0xda, 0x05, 0x0a], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 0x0a05);
    }

    #[test]
    fn test_jp_z_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.set_z();
        mmu.write_slice(&[0xca, 0x05, 0x0a], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 0x0a05);
    }

    #[test]
    fn test_jp_nc_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.clear_c();
        mmu.write_slice(&[0xd2, 0x05, 0x0a], 0);

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
    fn test_ccf() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.clear_c();
        cpu.set_n();
        cpu.set_h();

        mmu.write_slice(&[0x3f], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert!(cpu.c());
        assert!(!cpu.n());
        assert!(!cpu.h());
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
