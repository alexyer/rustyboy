use std::fmt::Display;

use crate::{
    check_bit,
    errors::InstructionError,
    instruction::{
        Instruction, InstructionType, NormalInstruction, Opcode, PrefixedInstruction,
        PrefixedOpcode,
    },
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
        pub fn $set_flag(&mut self) {
            self.regs
                .write_reg(Reg::F, self.regs.read_reg(Reg::F) | 1 << $i)
        }
        pub fn $clear_flag(&mut self) {
            self.regs
                .write_reg(Reg::F, self.regs.read_reg(Reg::F) & !(1 << $i))
        }
        pub fn $set_flag_to(&mut self, value: bool) {
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

impl From<&str> for Reg {
    fn from(v: &str) -> Self {
        match v {
            "A" => Reg::A,
            "B" => Reg::B,
            "C" => Reg::C,
            "D" => Reg::D,
            "E" => Reg::E,
            "F" => Reg::F,
            "H" => Reg::H,
            "L" => Reg::L,
            _ => panic!("unrecognized reg: {v}"),
        }
    }
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

impl From<&str> for Reg16 {
    fn from(v: &str) -> Self {
        match v {
            "AF" => Reg16::AF,
            "BC" => Reg16::BC,
            "DE" => Reg16::DE,
            "HL" => Reg16::HL,
            "SP" => Reg16::SP,
            "PC" => Reg16::PC,
            _ => panic!("unrecognized reg: {v}"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Regs {
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
    pub regs: Regs,
    interrupts_enabled: bool,

    /// Set if instruction branched during exectuion.
    /// Reset every cycle.
    branched: bool,

    halted: bool,
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
    pub fn log_state(&self, mmu: &Mmu) -> String {
        format!(
            "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}\n",
            self.regs.read_reg(Reg::A),
            self.regs.read_reg(Reg::F),
            self.regs.read_reg(Reg::B),
            self.regs.read_reg(Reg::C),
            self.regs.read_reg(Reg::D),
            self.regs.read_reg(Reg::E),
            self.regs.read_reg(Reg::H),
            self.regs.read_reg(Reg::L),
            self.regs.read_reg16(Reg16::SP),
            self.regs.read_reg16(Reg16::PC),
            mmu.read_byte(self.regs.read_reg16(Reg16::PC) as usize),
            mmu.read_byte(self.regs.read_reg16(Reg16::PC) as usize + 1),
            mmu.read_byte(self.regs.read_reg16(Reg16::PC) as usize + 2),
            mmu.read_byte(self.regs.read_reg16(Reg16::PC) as usize + 3),
        )
    }

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

        self.halted = false;
        self.push_rr(Reg16::PC, mmu);

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

        self.handle_interrupt(4, 0x60, fired_interrupts, mmu);
    }

    fn handle_interrupt(
        &mut self,
        interrupt_bit: u8,
        interrupt_vector: u16,
        fired_interrupts: u8,
        mmu: &mut Mmu,
    ) -> bool {
        if !check_bit!(fired_interrupts, interrupt_bit) {
            return false;
        }

        mmu.set_bit_to(INT_FLAG_ADDRESS, interrupt_bit as usize, false);
        self.regs.write_reg16(Reg16::PC, interrupt_vector);
        self.interrupts_enabled = false;

        true
    }

    /// Execute the next instruction and return the number of T-states.
    fn exec_instruction(&mut self, mmu: &mut Mmu) -> usize {
        if self.halted {
            return 1;
        }

        match self.read_instruction(mmu) {
            Some(Instruction::Normal(instruction)) => {
                let regs: Vec<&str> = instruction.regs().iter().map(String::as_str).collect();

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
                    InstructionType::LdRIndHlDec => self.ld_r_ind_hl_dec(regs[0].into(), mmu),
                    InstructionType::LdRIndRr => {
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
                        Opcode::LD_SP_HL => self.ld_sp_hl(),
                        _ => unreachable!(),
                    },
                    InstructionType::IncR => self.inc_r(regs[0].into()),
                    InstructionType::IncRr => self.inc_rr(regs[0].into()),
                    InstructionType::IncIndHl => self.inc_ind_hl(mmu),
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
                    InstructionType::SubIndHl => self.sub_ind_hl(mmu),
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
                        Opcode::JR_R8 => self.jr_r8(&prepare_data!(instruction, 1)),
                        Opcode::JR_C_R8 => self.jr_c_r8(&prepare_data!(instruction, 1)),
                        Opcode::JR_NC_R8 => self.jr_nc_r8(&prepare_data!(instruction, 1)),
                        Opcode::JR_NZ_R8 => self.jr_nz_r8(&prepare_data!(instruction, 1)),
                        Opcode::JR_Z_R8 => self.jr_z_r8(&prepare_data!(instruction, 1)),
                        _ => unreachable!(),
                    },
                    InstructionType::Jp => match instruction.opcode() {
                        Opcode::JP_A16 => self.jp_a16(&prepare_data!(instruction, 2)),
                        Opcode::JP_HL => self.jp_hl(),
                        Opcode::JP_NC_A16 => self.jp_nc_a16(&prepare_data!(instruction, 2)),
                        Opcode::JP_NZ_A16 => self.jp_nz_a16(&prepare_data!(instruction, 2)),
                        Opcode::JP_C_A16 => self.jp_c_a16(&prepare_data!(instruction, 2)),
                        Opcode::JP_Z_A16 => self.jp_z_a16(&prepare_data!(instruction, 2)),
                        _ => unreachable!(),
                    },
                    InstructionType::Call => match instruction.opcode() {
                        Opcode::CALL_A16 => self.call_a16(&prepare_data!(instruction, 2), mmu),
                        Opcode::CALL_C_A16 => self.call_c_a16(&prepare_data!(instruction, 2), mmu),
                        Opcode::CALL_Z_A16 => self.call_z_a16(&prepare_data!(instruction, 2), mmu),
                        Opcode::CALL_NC_A16 => {
                            self.call_nc_a16(&prepare_data!(instruction, 2), mmu)
                        }
                        Opcode::CALL_NZ_A16 => {
                            self.call_nz_a16(&prepare_data!(instruction, 2), mmu)
                        }
                        _ => unreachable!(),
                    },
                    InstructionType::Ret => match instruction.opcode() {
                        Opcode::RET => self.ret(mmu),
                        Opcode::RETI => self.reti(mmu),
                        Opcode::RET_C => self.ret_c(mmu),
                        Opcode::RET_NC => self.ret_nc(mmu),
                        Opcode::RET_Z => self.ret_z(mmu),
                        Opcode::RET_NZ => self.ret_nz(mmu),
                        _ => unreachable!(),
                    },
                    InstructionType::Rst => match instruction.opcode() {
                        Opcode::RST00 => self.call_a16(&[0x00, 0x00], mmu),
                        Opcode::RST08 => self.call_a16(&[0x08, 0x00], mmu),
                        Opcode::RST10 => self.call_a16(&[0x10, 0x00], mmu),
                        Opcode::RST18 => self.call_a16(&[0x18, 0x00], mmu),
                        Opcode::RST20 => self.call_a16(&[0x20, 0x00], mmu),
                        Opcode::RST28 => self.call_a16(&[0x28, 0x00], mmu),
                        Opcode::RST30 => self.call_a16(&[0x30, 0x00], mmu),
                        Opcode::RST38 => self.call_a16(&[0x38, 0x00], mmu),
                        _ => unreachable!(),
                    },
                    InstructionType::Di => self.di(),
                    InstructionType::Ei => self.ei(),
                    InstructionType::Rlca => self.rlca(),
                    InstructionType::Rrca => self.rrca(),
                    InstructionType::Rla => self.rla(),
                    InstructionType::Rra => self.rra(),
                    InstructionType::Halt => self.halt(),
                    _ => unreachable!(),
                }

                let cycles = instruction.cycles(self.branched);
                self.branched = false;

                cycles
            }
            Some(Instruction::Prefixed(instruction)) => {
                let regs: Vec<&str> = instruction.regs().iter().map(String::as_str).collect();

                match instruction.instruction_type() {
                    InstructionType::Bit0R => self.bit_r(regs[0].into(), 0),
                    InstructionType::Bit1R => self.bit_r(regs[0].into(), 1),
                    InstructionType::Bit2R => self.bit_r(regs[0].into(), 2),
                    InstructionType::Bit3R => self.bit_r(regs[0].into(), 3),
                    InstructionType::Bit4R => self.bit_r(regs[0].into(), 4),
                    InstructionType::Bit5R => self.bit_r(regs[0].into(), 5),
                    InstructionType::Bit6R => self.bit_r(regs[0].into(), 6),
                    InstructionType::Bit7R => self.bit_r(regs[0].into(), 7),
                    InstructionType::BitIndHl => match instruction.opcode() {
                        PrefixedOpcode::BIT7_IND_HL => self.bit_ind_hl(7, mmu),
                        _ => unreachable!(),
                    },
                    InstructionType::Res0R => self.res_r(regs[0].into(), 0),
                    InstructionType::Res1R => self.res_r(regs[0].into(), 1),
                    InstructionType::Res2R => self.res_r(regs[0].into(), 2),
                    InstructionType::Res3R => self.res_r(regs[0].into(), 3),
                    InstructionType::Res4R => self.res_r(regs[0].into(), 4),
                    InstructionType::Res5R => self.res_r(regs[0].into(), 5),
                    InstructionType::Res6R => self.res_r(regs[0].into(), 6),
                    InstructionType::Res7R => self.res_r(regs[0].into(), 7),
                    InstructionType::Set0R => self.set_r(regs[0].into(), 0),
                    InstructionType::Set1R => self.set_r(regs[0].into(), 1),
                    InstructionType::Set2R => self.set_r(regs[0].into(), 2),
                    InstructionType::Set3R => self.set_r(regs[0].into(), 3),
                    InstructionType::Set4R => self.set_r(regs[0].into(), 4),
                    InstructionType::Set5R => self.set_r(regs[0].into(), 5),
                    InstructionType::Set6R => self.set_r(regs[0].into(), 6),
                    InstructionType::Set7R => self.set_r(regs[0].into(), 7),
                    InstructionType::RlcR => self.rlc_r(regs[0].into()),
                    InstructionType::RrcR => self.rrc_r(regs[0].into()),
                    InstructionType::SlaR => self.sla_r(regs[0].into()),
                    InstructionType::SraR => self.sra_r(regs[0].into()),
                    InstructionType::SrlR => self.srl_r(regs[0].into()),
                    InstructionType::SwapR => self.swap_r(regs[0].into()),
                    InstructionType::RlR => self.rl_r(regs[0].into()),
                    InstructionType::RrR => self.rr_r(regs[0].into()),
                    _ => unreachable!(),
                }

                let cycles = instruction.cycles(self.branched);
                self.branched = false;

                cycles
            }
            None => 0,
        }
    }

    fn read_instruction<'a>(&mut self, mmu: &'a mut Mmu) -> Option<Instruction> {
        let opcode = match Opcode::try_from(mmu.read_byte(self.regs.read_reg16(Reg16::PC) as usize))
        {
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
            Opcode::NOP => Some(NormalInstruction::nop(&[]).into()),
            Opcode::LD_SP_D16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::ld_sp_d16(&data).into())
            }
            Opcode::LD_A_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_a_d8(data).into())
            }
            Opcode::LD_A_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::ld_a_a16(&data).into())
            }
            Opcode::LD_B_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_b_d8(data).into())
            }
            Opcode::LD_C_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_c_d8(data).into())
            }
            Opcode::LD_D_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_d_d8(data).into())
            }
            Opcode::LD_H_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_h_d8(data).into())
            }
            Opcode::LD_E_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_e_d8(data).into())
            }
            Opcode::LD_L_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_l_d8(data).into())
            }
            Opcode::LDH_A_A8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ldh_a_a8(data).into())
            }
            Opcode::LDH_A8_A => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ldh_a8_a(data).into())
            }
            Opcode::LD_HL_SP_E8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_hl_sp_e8(data).into())
            }
            Opcode::ADD_SP_E8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::add_sp_e8(data).into())
            }
            Opcode::LD_SP_HL => Some(NormalInstruction::ld_sp_hl(&[]).into()),
            Opcode::LD_A_A => Some(NormalInstruction::ld_a_a(&[]).into()),
            Opcode::LD_A_B => Some(NormalInstruction::ld_a_b(&[]).into()),
            Opcode::LD_A_C => Some(NormalInstruction::ld_a_c(&[]).into()),
            Opcode::LD_A_D => Some(NormalInstruction::ld_a_d(&[]).into()),
            Opcode::LD_A_E => Some(NormalInstruction::ld_a_e(&[]).into()),
            Opcode::LD_A_H => Some(NormalInstruction::ld_a_h(&[]).into()),
            Opcode::LD_A_L => Some(NormalInstruction::ld_a_l(&[]).into()),
            Opcode::LD_B_A => Some(NormalInstruction::ld_b_a(&[]).into()),
            Opcode::LD_B_B => Some(NormalInstruction::ld_b_b(&[]).into()),
            Opcode::LD_B_C => Some(NormalInstruction::ld_b_c(&[]).into()),
            Opcode::LD_B_D => Some(NormalInstruction::ld_b_d(&[]).into()),
            Opcode::LD_B_E => Some(NormalInstruction::ld_b_e(&[]).into()),
            Opcode::LD_B_H => Some(NormalInstruction::ld_b_h(&[]).into()),
            Opcode::LD_B_L => Some(NormalInstruction::ld_b_l(&[]).into()),
            Opcode::LD_C_A => Some(NormalInstruction::ld_c_a(&[]).into()),
            Opcode::LD_C_B => Some(NormalInstruction::ld_c_b(&[]).into()),
            Opcode::LD_C_C => Some(NormalInstruction::ld_c_c(&[]).into()),
            Opcode::LD_C_D => Some(NormalInstruction::ld_c_d(&[]).into()),
            Opcode::LD_C_E => Some(NormalInstruction::ld_c_e(&[]).into()),
            Opcode::LD_C_H => Some(NormalInstruction::ld_c_h(&[]).into()),
            Opcode::LD_C_L => Some(NormalInstruction::ld_c_l(&[]).into()),
            Opcode::LD_D_A => Some(NormalInstruction::ld_d_a(&[]).into()),
            Opcode::LD_D_B => Some(NormalInstruction::ld_d_b(&[]).into()),
            Opcode::LD_D_C => Some(NormalInstruction::ld_d_c(&[]).into()),
            Opcode::LD_D_D => Some(NormalInstruction::ld_d_d(&[]).into()),
            Opcode::LD_D_E => Some(NormalInstruction::ld_d_e(&[]).into()),
            Opcode::LD_D_H => Some(NormalInstruction::ld_d_h(&[]).into()),
            Opcode::LD_D_L => Some(NormalInstruction::ld_d_l(&[]).into()),
            Opcode::LD_E_A => Some(NormalInstruction::ld_e_a(&[]).into()),
            Opcode::LD_E_B => Some(NormalInstruction::ld_e_b(&[]).into()),
            Opcode::LD_E_C => Some(NormalInstruction::ld_e_c(&[]).into()),
            Opcode::LD_E_D => Some(NormalInstruction::ld_e_d(&[]).into()),
            Opcode::LD_E_E => Some(NormalInstruction::ld_e_e(&[]).into()),
            Opcode::LD_E_H => Some(NormalInstruction::ld_e_h(&[]).into()),
            Opcode::LD_E_L => Some(NormalInstruction::ld_e_l(&[]).into()),
            Opcode::LD_H_A => Some(NormalInstruction::ld_h_a(&[]).into()),
            Opcode::LD_H_B => Some(NormalInstruction::ld_h_b(&[]).into()),
            Opcode::LD_H_C => Some(NormalInstruction::ld_h_c(&[]).into()),
            Opcode::LD_H_D => Some(NormalInstruction::ld_h_d(&[]).into()),
            Opcode::LD_H_E => Some(NormalInstruction::ld_h_e(&[]).into()),
            Opcode::LD_H_H => Some(NormalInstruction::ld_h_h(&[]).into()),
            Opcode::LD_H_L => Some(NormalInstruction::ld_h_l(&[]).into()),
            Opcode::LD_L_A => Some(NormalInstruction::ld_l_a(&[]).into()),
            Opcode::LD_L_B => Some(NormalInstruction::ld_l_b(&[]).into()),
            Opcode::LD_L_C => Some(NormalInstruction::ld_l_c(&[]).into()),
            Opcode::LD_L_D => Some(NormalInstruction::ld_l_d(&[]).into()),
            Opcode::LD_L_E => Some(NormalInstruction::ld_l_e(&[]).into()),
            Opcode::LD_L_H => Some(NormalInstruction::ld_l_h(&[]).into()),
            Opcode::LD_L_L => Some(NormalInstruction::ld_l_l(&[]).into()),
            Opcode::LD_A_IND_HL_INC => Some(NormalInstruction::ld_a_ind_hl_inc(&[]).into()),
            Opcode::PUSH_AF => Some(NormalInstruction::push_af(&[]).into()),
            Opcode::PUSH_BC => Some(NormalInstruction::push_bc(&[]).into()),
            Opcode::PUSH_DE => Some(NormalInstruction::push_de(&[]).into()),
            Opcode::PUSH_HL => Some(NormalInstruction::push_hl(&[]).into()),
            Opcode::POP_AF => Some(NormalInstruction::pop_af(&[]).into()),
            Opcode::POP_BC => Some(NormalInstruction::pop_bc(&[]).into()),
            Opcode::POP_DE => Some(NormalInstruction::pop_de(&[]).into()),
            Opcode::POP_HL => Some(NormalInstruction::pop_hl(&[]).into()),
            Opcode::INC_A => Some(NormalInstruction::inc_a(&[]).into()),
            Opcode::INC_B => Some(NormalInstruction::inc_b(&[]).into()),
            Opcode::INC_C => Some(NormalInstruction::inc_c(&[]).into()),
            Opcode::INC_D => Some(NormalInstruction::inc_d(&[]).into()),
            Opcode::INC_E => Some(NormalInstruction::inc_e(&[]).into()),
            Opcode::INC_BC => Some(NormalInstruction::inc_bc(&[]).into()),
            Opcode::INC_DE => Some(NormalInstruction::inc_de(&[]).into()),
            Opcode::INC_HL => Some(NormalInstruction::inc_hl(&[]).into()),
            Opcode::INC_SP => Some(NormalInstruction::inc_sp(&[]).into()),
            Opcode::INC_H => Some(NormalInstruction::inc_h(&[]).into()),
            Opcode::INC_L => Some(NormalInstruction::inc_l(&[]).into()),
            Opcode::INC_IND_HL => Some(NormalInstruction::inc_ind_hl(&[]).into()),
            Opcode::DAA => Some(NormalInstruction::daa(&[]).into()),
            Opcode::DEC_A => Some(NormalInstruction::dec_a(&[]).into()),
            Opcode::DEC_B => Some(NormalInstruction::dec_b(&[]).into()),
            Opcode::DEC_C => Some(NormalInstruction::dec_c(&[]).into()),
            Opcode::DEC_D => Some(NormalInstruction::dec_d(&[]).into()),
            Opcode::DEC_E => Some(NormalInstruction::dec_e(&[]).into()),
            Opcode::DEC_H => Some(NormalInstruction::dec_h(&[]).into()),
            Opcode::DEC_L => Some(NormalInstruction::dec_l(&[]).into()),
            Opcode::DEC_BC => Some(NormalInstruction::dec_bc(&[]).into()),
            Opcode::DEC_DE => Some(NormalInstruction::dec_de(&[]).into()),
            Opcode::DEC_HL => Some(NormalInstruction::dec_hl(&[]).into()),
            Opcode::DEC_SP => Some(NormalInstruction::dec_sp(&[]).into()),
            Opcode::DEC_IND_HL => Some(NormalInstruction::dec_ind_hl(&[]).into()),
            Opcode::OR_A => Some(NormalInstruction::or_a(&[]).into()),
            Opcode::OR_A_IND_HL => Some(NormalInstruction::or_a_ind_hl(&[]).into()),
            Opcode::OR_B => Some(NormalInstruction::or_b(&[]).into()),
            Opcode::OR_C => Some(NormalInstruction::or_c(&[]).into()),
            Opcode::OR_D => Some(NormalInstruction::or_d(&[]).into()),
            Opcode::OR_E => Some(NormalInstruction::or_e(&[]).into()),
            Opcode::OR_H => Some(NormalInstruction::or_h(&[]).into()),
            Opcode::OR_L => Some(NormalInstruction::or_l(&[]).into()),
            Opcode::OR_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::or_d8(data).into())
            }
            Opcode::XOR_A => Some(NormalInstruction::xor_a(&[]).into()),
            Opcode::XOR_B => Some(NormalInstruction::xor_b(&[]).into()),
            Opcode::XOR_C => Some(NormalInstruction::xor_c(&[]).into()),
            Opcode::XOR_D => Some(NormalInstruction::xor_d(&[]).into()),
            Opcode::XOR_E => Some(NormalInstruction::xor_e(&[]).into()),
            Opcode::XOR_H => Some(NormalInstruction::xor_h(&[]).into()),
            Opcode::XOR_L => Some(NormalInstruction::xor_l(&[]).into()),
            Opcode::XOR_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::xor_d8(data).into())
            }
            Opcode::XOR_A_IND_HL => Some(NormalInstruction::xor_a_ind_hl(&[]).into()),
            Opcode::AND_A => Some(NormalInstruction::and_a(&[]).into()),
            Opcode::AND_B => Some(NormalInstruction::and_b(&[]).into()),
            Opcode::AND_C => Some(NormalInstruction::and_c(&[]).into()),
            Opcode::AND_D => Some(NormalInstruction::and_d(&[]).into()),
            Opcode::AND_E => Some(NormalInstruction::and_e(&[]).into()),
            Opcode::AND_H => Some(NormalInstruction::and_h(&[]).into()),
            Opcode::AND_L => Some(NormalInstruction::and_l(&[]).into()),
            Opcode::AND_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::and_d8(data).into())
            }
            Opcode::ADC_A => Some(NormalInstruction::adc_a(&[]).into()),
            Opcode::ADC_B => Some(NormalInstruction::adc_b(&[]).into()),
            Opcode::ADC_C => Some(NormalInstruction::adc_c(&[]).into()),
            Opcode::ADC_D => Some(NormalInstruction::adc_d(&[]).into()),
            Opcode::ADC_E => Some(NormalInstruction::adc_e(&[]).into()),
            Opcode::ADC_H => Some(NormalInstruction::adc_h(&[]).into()),
            Opcode::ADC_L => Some(NormalInstruction::adc_l(&[]).into()),
            Opcode::ADC_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::adc_d8(data).into())
            }
            Opcode::ADD_A => Some(NormalInstruction::add_a(&[]).into()),
            Opcode::ADD_B => Some(NormalInstruction::add_b(&[]).into()),
            Opcode::ADD_C => Some(NormalInstruction::add_c(&[]).into()),
            Opcode::ADD_D => Some(NormalInstruction::add_d(&[]).into()),
            Opcode::ADD_E => Some(NormalInstruction::add_e(&[]).into()),
            Opcode::ADD_H => Some(NormalInstruction::add_h(&[]).into()),
            Opcode::ADD_L => Some(NormalInstruction::add_l(&[]).into()),
            Opcode::ADD_HL_BC => Some(NormalInstruction::add_hl_bc(&[]).into()),
            Opcode::ADD_HL_DE => Some(NormalInstruction::add_hl_de(&[]).into()),
            Opcode::ADD_HL_HL => Some(NormalInstruction::add_hl_hl(&[]).into()),
            Opcode::ADD_HL_SP => Some(NormalInstruction::add_hl_sp(&[]).into()),
            Opcode::ADD_A_IND_HL => Some(NormalInstruction::add_a_ind_hl(&[]).into()),
            Opcode::ADD_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::add_d8(data).into())
            }
            Opcode::CP_A => Some(NormalInstruction::cp_a(&[]).into()),
            Opcode::CP_B => Some(NormalInstruction::cp_b(&[]).into()),
            Opcode::CP_C => Some(NormalInstruction::cp_c(&[]).into()),
            Opcode::CP_D => Some(NormalInstruction::cp_d(&[]).into()),
            Opcode::CP_E => Some(NormalInstruction::cp_e(&[]).into()),
            Opcode::CP_H => Some(NormalInstruction::cp_h(&[]).into()),
            Opcode::CP_L => Some(NormalInstruction::cp_l(&[]).into()),
            Opcode::CP_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::cp_d8(data).into())
            }
            Opcode::CP_A_IND_HL => Some(NormalInstruction::cp_a_ind_hl(&[]).into()),
            Opcode::SBC_A => Some(NormalInstruction::sbc_a(&[]).into()),
            Opcode::SBC_B => Some(NormalInstruction::sbc_b(&[]).into()),
            Opcode::SBC_C => Some(NormalInstruction::sbc_c(&[]).into()),
            Opcode::SBC_D => Some(NormalInstruction::sbc_d(&[]).into()),
            Opcode::SBC_E => Some(NormalInstruction::sbc_e(&[]).into()),
            Opcode::SBC_H => Some(NormalInstruction::sbc_h(&[]).into()),
            Opcode::SBC_L => Some(NormalInstruction::sbc_l(&[]).into()),
            Opcode::SBC_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::sbc_d8(data).into())
            }
            Opcode::SUB_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::sub_d8(data).into())
            }
            Opcode::SUB_A => Some(NormalInstruction::sub_a(&[]).into()),
            Opcode::SUB_B => Some(NormalInstruction::sub_b(&[]).into()),
            Opcode::SUB_C => Some(NormalInstruction::sub_c(&[]).into()),
            Opcode::SUB_D => Some(NormalInstruction::sub_d(&[]).into()),
            Opcode::SUB_E => Some(NormalInstruction::sub_e(&[]).into()),
            Opcode::SUB_H => Some(NormalInstruction::sub_h(&[]).into()),
            Opcode::SUB_L => Some(NormalInstruction::sub_l(&[]).into()),
            Opcode::SUB_IND_HL => Some(NormalInstruction::sub_ind_hl(&[]).into()),
            Opcode::LD_HL_D16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::ld_hl_d16(&data).into())
            }
            Opcode::LD_BC_D16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::ld_bc_d16(&data).into())
            }
            Opcode::LD_DE_D16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::ld_de_d16(&data).into())
            }
            Opcode::LD_IND_BC_A => Some(NormalInstruction::ld_ind_bc_a(&[]).into()),
            Opcode::LD_IND_DE_A => Some(NormalInstruction::ld_ind_de_a(&[]).into()),
            Opcode::LD_IND_HL_A => Some(NormalInstruction::ld_ind_hl_a(&[]).into()),
            Opcode::LD_IND_HL_B => Some(NormalInstruction::ld_ind_hl_b(&[]).into()),
            Opcode::LD_IND_HL_C => Some(NormalInstruction::ld_ind_hl_c(&[]).into()),
            Opcode::LD_IND_HL_D => Some(NormalInstruction::ld_ind_hl_d(&[]).into()),
            Opcode::LD_IND_HL_E => Some(NormalInstruction::ld_ind_hl_e(&[]).into()),
            Opcode::LD_IND_HL_H => Some(NormalInstruction::ld_ind_hl_h(&[]).into()),
            Opcode::LD_IND_HL_L => Some(NormalInstruction::ld_ind_hl_l(&[]).into()),
            Opcode::LD_IND_HL_D8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::ld_ind_hl_d8(data).into())
            }
            Opcode::LD_IND_HL_DEC_A => Some(NormalInstruction::ld_ind_hl_dec_a(&[]).into()),
            Opcode::LD_IND_HL_INC_A => Some(NormalInstruction::ld_ind_hl_inc_a(&[]).into()),
            Opcode::LD_IND_C_A => Some(NormalInstruction::ld_ind_c_a(&[]).into()),
            Opcode::LD_A_IND_C => Some(NormalInstruction::ld_a_ind_c(&[]).into()),
            Opcode::LD_A_IND_DE => Some(NormalInstruction::ld_a_ind_de(&[]).into()),
            Opcode::LD_A_IND_HL => Some(NormalInstruction::ld_a_ind_hl(&[]).into()),
            Opcode::LD_B_IND_HL => Some(NormalInstruction::ld_b_ind_hl(&[]).into()),
            Opcode::LD_C_IND_HL => Some(NormalInstruction::ld_c_ind_hl(&[]).into()),
            Opcode::LD_D_IND_HL => Some(NormalInstruction::ld_d_ind_hl(&[]).into()),
            Opcode::LD_E_IND_HL => Some(NormalInstruction::ld_e_ind_hl(&[]).into()),
            Opcode::LD_H_IND_HL => Some(NormalInstruction::ld_h_ind_hl(&[]).into()),
            Opcode::LD_L_IND_HL => Some(NormalInstruction::ld_l_ind_hl(&[]).into()),
            Opcode::LD_A_IND_HL_DEC => Some(NormalInstruction::ld_a_ind_hl_dec(&[]).into()),
            Opcode::LD_A_IND_BC => Some(NormalInstruction::ld_a_ind_bc(&[]).into()),
            Opcode::JR_R8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::jr_r8(data).into())
            }
            Opcode::JR_C_R8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::jr_c_r8(data).into())
            }
            Opcode::JR_NC_R8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::jr_nc_r8(data).into())
            }
            Opcode::JR_NZ_R8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::jr_nz_r8(data).into())
            }
            Opcode::JR_Z_R8 => {
                let data = &[mmu.read_byte(pc as usize)];
                self.regs.write_reg16(Reg16::PC, pc + 1);

                Some(NormalInstruction::jr_z_r8(data).into())
            }
            Opcode::JP_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::jp_a16(&data).into())
            }
            Opcode::JP_NC_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::jp_nc_a16(&data).into())
            }
            Opcode::JP_NZ_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::jp_nz_a16(&data).into())
            }
            Opcode::JP_C_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::jp_c_a16(&data).into())
            }
            Opcode::JP_Z_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::jp_z_a16(&data).into())
            }
            Opcode::JP_HL => Some(NormalInstruction::jp_hl(&[]).into()),
            Opcode::CALL_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::call_a16(&data).into())
            }
            Opcode::CALL_C_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::call_c_a16(&data).into())
            }
            Opcode::CALL_Z_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::call_z_a16(&data).into())
            }
            Opcode::CALL_NC_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::call_nc_a16(&data).into())
            }
            Opcode::CALL_NZ_A16 => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::call_nz_a16(&data).into())
            }
            Opcode::HALT => Some(NormalInstruction::halt(&[]).into()),
            Opcode::STOP => Some(NormalInstruction::halt(&[]).into()),
            Opcode::RET => Some(NormalInstruction::ret(&[]).into()),
            Opcode::RETI => Some(NormalInstruction::reti(&[]).into()),
            Opcode::RET_C => Some(NormalInstruction::ret_c(&[]).into()),
            Opcode::RET_NC => Some(NormalInstruction::ret_nc(&[]).into()),
            Opcode::RET_Z => Some(NormalInstruction::ret_z(&[]).into()),
            Opcode::RET_NZ => Some(NormalInstruction::ret_nz(&[]).into()),
            Opcode::RST00 => Some(NormalInstruction::rst00(&[]).into()),
            Opcode::RST08 => Some(NormalInstruction::rst08(&[]).into()),
            Opcode::RST10 => Some(NormalInstruction::rst10(&[]).into()),
            Opcode::RST18 => Some(NormalInstruction::rst18(&[]).into()),
            Opcode::RST20 => Some(NormalInstruction::rst20(&[]).into()),
            Opcode::RST28 => Some(NormalInstruction::rst28(&[]).into()),
            Opcode::RST30 => Some(NormalInstruction::rst30(&[]).into()),
            Opcode::RST38 => Some(NormalInstruction::rst38(&[]).into()),
            Opcode::RLA => Some(NormalInstruction::rla(&[]).into()),
            Opcode::RRA => Some(NormalInstruction::rra(&[]).into()),
            Opcode::CPL => Some(NormalInstruction::cpl(&[]).into()),
            Opcode::CCF => Some(NormalInstruction::ccf(&[]).into()),
            Opcode::SCF => Some(NormalInstruction::scf(&[]).into()),
            Opcode::LD_A16_A => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::ld_a16_a(&data).into())
            }
            Opcode::LD_A16_SP => {
                let data = mmu.read_slice(pc as usize, 2);
                self.regs.write_reg16(Reg16::PC, pc + 2);

                Some(NormalInstruction::ld_a16_sp(&data).into())
            }
            Opcode::RLCA => Some(NormalInstruction::rlca(&[]).into()),
            Opcode::RRCA => Some(NormalInstruction::rrca(&[]).into()),
            Opcode::PREFIX => {
                let prefixed_opcode = match PrefixedOpcode::try_from(mmu.read_byte(pc as usize)) {
                    Ok(prefixed_opcode) => prefixed_opcode,
                    Err(InstructionError::UnrecognizedPrefixedOpcode(prefixed_opcode)) => {
                        println!("{}", self);
                        panic!("unrecognized prefixed opcode: 0x{:x}", prefixed_opcode);
                    }
                    _ => unreachable!(),
                };
                self.regs.write_reg16(Reg16::PC, pc + 1);

                match prefixed_opcode {
                    PrefixedOpcode::RL_B => Some(PrefixedInstruction::rl_b(&[]).into()),
                    PrefixedOpcode::RL_C => Some(PrefixedInstruction::rl_c(&[]).into()),
                    PrefixedOpcode::RL_D => Some(PrefixedInstruction::rl_d(&[]).into()),
                    PrefixedOpcode::RL_E => Some(PrefixedInstruction::rl_e(&[]).into()),
                    PrefixedOpcode::RL_H => Some(PrefixedInstruction::rl_h(&[]).into()),
                    PrefixedOpcode::RL_L => Some(PrefixedInstruction::rl_l(&[]).into()),
                    PrefixedOpcode::RL_A => Some(PrefixedInstruction::rl_a(&[]).into()),
                    PrefixedOpcode::RLC_A => Some(PrefixedInstruction::rlc_a(&[]).into()),
                    PrefixedOpcode::RLC_B => Some(PrefixedInstruction::rlc_b(&[]).into()),
                    PrefixedOpcode::RLC_C => Some(PrefixedInstruction::rlc_c(&[]).into()),
                    PrefixedOpcode::RLC_D => Some(PrefixedInstruction::rlc_d(&[]).into()),
                    PrefixedOpcode::RLC_E => Some(PrefixedInstruction::rlc_e(&[]).into()),
                    PrefixedOpcode::RLC_H => Some(PrefixedInstruction::rlc_h(&[]).into()),
                    PrefixedOpcode::RLC_L => Some(PrefixedInstruction::rlc_l(&[]).into()),
                    PrefixedOpcode::BIT0B => Some(PrefixedInstruction::bit0b(&[]).into()),
                    PrefixedOpcode::BIT0C => Some(PrefixedInstruction::bit0c(&[]).into()),
                    PrefixedOpcode::BIT0D => Some(PrefixedInstruction::bit0d(&[]).into()),
                    PrefixedOpcode::BIT0E => Some(PrefixedInstruction::bit0e(&[]).into()),
                    PrefixedOpcode::BIT0H => Some(PrefixedInstruction::bit0h(&[]).into()),
                    PrefixedOpcode::BIT0L => Some(PrefixedInstruction::bit0l(&[]).into()),
                    PrefixedOpcode::BIT0A => Some(PrefixedInstruction::bit0a(&[]).into()),
                    PrefixedOpcode::BIT1B => Some(PrefixedInstruction::bit1b(&[]).into()),
                    PrefixedOpcode::BIT1C => Some(PrefixedInstruction::bit1c(&[]).into()),
                    PrefixedOpcode::BIT1D => Some(PrefixedInstruction::bit1d(&[]).into()),
                    PrefixedOpcode::BIT1E => Some(PrefixedInstruction::bit1e(&[]).into()),
                    PrefixedOpcode::BIT1H => Some(PrefixedInstruction::bit1h(&[]).into()),
                    PrefixedOpcode::BIT1L => Some(PrefixedInstruction::bit1l(&[]).into()),
                    PrefixedOpcode::BIT1A => Some(PrefixedInstruction::bit1a(&[]).into()),
                    PrefixedOpcode::BIT2B => Some(PrefixedInstruction::bit2b(&[]).into()),
                    PrefixedOpcode::BIT2C => Some(PrefixedInstruction::bit2c(&[]).into()),
                    PrefixedOpcode::BIT2D => Some(PrefixedInstruction::bit2d(&[]).into()),
                    PrefixedOpcode::BIT2E => Some(PrefixedInstruction::bit2e(&[]).into()),
                    PrefixedOpcode::BIT2H => Some(PrefixedInstruction::bit2h(&[]).into()),
                    PrefixedOpcode::BIT2L => Some(PrefixedInstruction::bit2l(&[]).into()),
                    PrefixedOpcode::BIT2A => Some(PrefixedInstruction::bit2a(&[]).into()),
                    PrefixedOpcode::BIT3B => Some(PrefixedInstruction::bit3b(&[]).into()),
                    PrefixedOpcode::BIT3C => Some(PrefixedInstruction::bit3c(&[]).into()),
                    PrefixedOpcode::BIT3D => Some(PrefixedInstruction::bit3d(&[]).into()),
                    PrefixedOpcode::BIT3E => Some(PrefixedInstruction::bit3e(&[]).into()),
                    PrefixedOpcode::BIT3H => Some(PrefixedInstruction::bit3h(&[]).into()),
                    PrefixedOpcode::BIT3L => Some(PrefixedInstruction::bit3l(&[]).into()),
                    PrefixedOpcode::BIT3A => Some(PrefixedInstruction::bit3a(&[]).into()),
                    PrefixedOpcode::BIT4B => Some(PrefixedInstruction::bit4b(&[]).into()),
                    PrefixedOpcode::BIT4C => Some(PrefixedInstruction::bit4c(&[]).into()),
                    PrefixedOpcode::BIT4D => Some(PrefixedInstruction::bit4d(&[]).into()),
                    PrefixedOpcode::BIT4E => Some(PrefixedInstruction::bit4e(&[]).into()),
                    PrefixedOpcode::BIT4H => Some(PrefixedInstruction::bit4h(&[]).into()),
                    PrefixedOpcode::BIT4L => Some(PrefixedInstruction::bit4l(&[]).into()),
                    PrefixedOpcode::BIT4A => Some(PrefixedInstruction::bit4a(&[]).into()),
                    PrefixedOpcode::BIT5B => Some(PrefixedInstruction::bit5b(&[]).into()),
                    PrefixedOpcode::BIT5C => Some(PrefixedInstruction::bit5c(&[]).into()),
                    PrefixedOpcode::BIT5D => Some(PrefixedInstruction::bit5d(&[]).into()),
                    PrefixedOpcode::BIT5E => Some(PrefixedInstruction::bit5e(&[]).into()),
                    PrefixedOpcode::BIT5H => Some(PrefixedInstruction::bit5h(&[]).into()),
                    PrefixedOpcode::BIT5L => Some(PrefixedInstruction::bit5l(&[]).into()),
                    PrefixedOpcode::BIT5A => Some(PrefixedInstruction::bit5a(&[]).into()),
                    PrefixedOpcode::BIT6B => Some(PrefixedInstruction::bit6b(&[]).into()),
                    PrefixedOpcode::BIT6C => Some(PrefixedInstruction::bit6c(&[]).into()),
                    PrefixedOpcode::BIT6D => Some(PrefixedInstruction::bit6d(&[]).into()),
                    PrefixedOpcode::BIT6E => Some(PrefixedInstruction::bit6e(&[]).into()),
                    PrefixedOpcode::BIT6H => Some(PrefixedInstruction::bit6h(&[]).into()),
                    PrefixedOpcode::BIT6L => Some(PrefixedInstruction::bit6l(&[]).into()),
                    PrefixedOpcode::BIT6A => Some(PrefixedInstruction::bit6a(&[]).into()),
                    PrefixedOpcode::BIT7B => Some(PrefixedInstruction::bit7b(&[]).into()),
                    PrefixedOpcode::BIT7C => Some(PrefixedInstruction::bit7c(&[]).into()),
                    PrefixedOpcode::BIT7D => Some(PrefixedInstruction::bit7d(&[]).into()),
                    PrefixedOpcode::BIT7E => Some(PrefixedInstruction::bit7e(&[]).into()),
                    PrefixedOpcode::BIT7H => Some(PrefixedInstruction::bit7h(&[]).into()),
                    PrefixedOpcode::BIT7L => Some(PrefixedInstruction::bit7l(&[]).into()),
                    PrefixedOpcode::BIT7A => Some(PrefixedInstruction::bit7a(&[]).into()),
                    PrefixedOpcode::BIT7_IND_HL => {
                        Some(PrefixedInstruction::bit7_ind_hl(&[]).into())
                    }
                    PrefixedOpcode::RES0B => Some(PrefixedInstruction::res0b(&[]).into()),
                    PrefixedOpcode::RES0C => Some(PrefixedInstruction::res0c(&[]).into()),
                    PrefixedOpcode::RES0D => Some(PrefixedInstruction::res0d(&[]).into()),
                    PrefixedOpcode::RES0E => Some(PrefixedInstruction::res0e(&[]).into()),
                    PrefixedOpcode::RES0H => Some(PrefixedInstruction::res0h(&[]).into()),
                    PrefixedOpcode::RES0L => Some(PrefixedInstruction::res0l(&[]).into()),
                    PrefixedOpcode::RES0A => Some(PrefixedInstruction::res0a(&[]).into()),
                    PrefixedOpcode::RES1B => Some(PrefixedInstruction::res1b(&[]).into()),
                    PrefixedOpcode::RES1C => Some(PrefixedInstruction::res1c(&[]).into()),
                    PrefixedOpcode::RES1D => Some(PrefixedInstruction::res1d(&[]).into()),
                    PrefixedOpcode::RES1E => Some(PrefixedInstruction::res1e(&[]).into()),
                    PrefixedOpcode::RES1H => Some(PrefixedInstruction::res1h(&[]).into()),
                    PrefixedOpcode::RES1L => Some(PrefixedInstruction::res1l(&[]).into()),
                    PrefixedOpcode::RES1A => Some(PrefixedInstruction::res1a(&[]).into()),
                    PrefixedOpcode::RES2B => Some(PrefixedInstruction::res2b(&[]).into()),
                    PrefixedOpcode::RES2C => Some(PrefixedInstruction::res2c(&[]).into()),
                    PrefixedOpcode::RES2D => Some(PrefixedInstruction::res2d(&[]).into()),
                    PrefixedOpcode::RES2E => Some(PrefixedInstruction::res2e(&[]).into()),
                    PrefixedOpcode::RES2H => Some(PrefixedInstruction::res2h(&[]).into()),
                    PrefixedOpcode::RES2L => Some(PrefixedInstruction::res2l(&[]).into()),
                    PrefixedOpcode::RES2A => Some(PrefixedInstruction::res2a(&[]).into()),
                    PrefixedOpcode::RES3B => Some(PrefixedInstruction::res3b(&[]).into()),
                    PrefixedOpcode::RES3C => Some(PrefixedInstruction::res3c(&[]).into()),
                    PrefixedOpcode::RES3D => Some(PrefixedInstruction::res3d(&[]).into()),
                    PrefixedOpcode::RES3E => Some(PrefixedInstruction::res3e(&[]).into()),
                    PrefixedOpcode::RES3H => Some(PrefixedInstruction::res3h(&[]).into()),
                    PrefixedOpcode::RES3L => Some(PrefixedInstruction::res3l(&[]).into()),
                    PrefixedOpcode::RES3A => Some(PrefixedInstruction::res3a(&[]).into()),
                    PrefixedOpcode::RES4B => Some(PrefixedInstruction::res4b(&[]).into()),
                    PrefixedOpcode::RES4C => Some(PrefixedInstruction::res4c(&[]).into()),
                    PrefixedOpcode::RES4D => Some(PrefixedInstruction::res4d(&[]).into()),
                    PrefixedOpcode::RES4E => Some(PrefixedInstruction::res4e(&[]).into()),
                    PrefixedOpcode::RES4H => Some(PrefixedInstruction::res4h(&[]).into()),
                    PrefixedOpcode::RES4L => Some(PrefixedInstruction::res4l(&[]).into()),
                    PrefixedOpcode::RES4A => Some(PrefixedInstruction::res4a(&[]).into()),
                    PrefixedOpcode::RES5B => Some(PrefixedInstruction::res5b(&[]).into()),
                    PrefixedOpcode::RES5C => Some(PrefixedInstruction::res5c(&[]).into()),
                    PrefixedOpcode::RES5D => Some(PrefixedInstruction::res5d(&[]).into()),
                    PrefixedOpcode::RES5E => Some(PrefixedInstruction::res5e(&[]).into()),
                    PrefixedOpcode::RES5H => Some(PrefixedInstruction::res5h(&[]).into()),
                    PrefixedOpcode::RES5L => Some(PrefixedInstruction::res5l(&[]).into()),
                    PrefixedOpcode::RES5A => Some(PrefixedInstruction::res5a(&[]).into()),
                    PrefixedOpcode::RES6B => Some(PrefixedInstruction::res6b(&[]).into()),
                    PrefixedOpcode::RES6C => Some(PrefixedInstruction::res6c(&[]).into()),
                    PrefixedOpcode::RES6D => Some(PrefixedInstruction::res6d(&[]).into()),
                    PrefixedOpcode::RES6E => Some(PrefixedInstruction::res6e(&[]).into()),
                    PrefixedOpcode::RES6H => Some(PrefixedInstruction::res6h(&[]).into()),
                    PrefixedOpcode::RES6L => Some(PrefixedInstruction::res6l(&[]).into()),
                    PrefixedOpcode::RES6A => Some(PrefixedInstruction::res6a(&[]).into()),
                    PrefixedOpcode::RES7B => Some(PrefixedInstruction::res7b(&[]).into()),
                    PrefixedOpcode::RES7C => Some(PrefixedInstruction::res7c(&[]).into()),
                    PrefixedOpcode::RES7D => Some(PrefixedInstruction::res7d(&[]).into()),
                    PrefixedOpcode::RES7E => Some(PrefixedInstruction::res7e(&[]).into()),
                    PrefixedOpcode::RES7H => Some(PrefixedInstruction::res7h(&[]).into()),
                    PrefixedOpcode::RES7L => Some(PrefixedInstruction::res7l(&[]).into()),
                    PrefixedOpcode::RES7A => Some(PrefixedInstruction::res7a(&[]).into()),
                    PrefixedOpcode::SET0B => Some(PrefixedInstruction::set0b(&[]).into()),
                    PrefixedOpcode::SET0C => Some(PrefixedInstruction::set0c(&[]).into()),
                    PrefixedOpcode::SET0D => Some(PrefixedInstruction::set0d(&[]).into()),
                    PrefixedOpcode::SET0E => Some(PrefixedInstruction::set0e(&[]).into()),
                    PrefixedOpcode::SET0H => Some(PrefixedInstruction::set0h(&[]).into()),
                    PrefixedOpcode::SET0L => Some(PrefixedInstruction::set0l(&[]).into()),
                    PrefixedOpcode::SET0A => Some(PrefixedInstruction::set0a(&[]).into()),
                    PrefixedOpcode::SET1B => Some(PrefixedInstruction::set1b(&[]).into()),
                    PrefixedOpcode::SET1C => Some(PrefixedInstruction::set1c(&[]).into()),
                    PrefixedOpcode::SET1D => Some(PrefixedInstruction::set1d(&[]).into()),
                    PrefixedOpcode::SET1E => Some(PrefixedInstruction::set1e(&[]).into()),
                    PrefixedOpcode::SET1H => Some(PrefixedInstruction::set1h(&[]).into()),
                    PrefixedOpcode::SET1L => Some(PrefixedInstruction::set1l(&[]).into()),
                    PrefixedOpcode::SET1A => Some(PrefixedInstruction::set1a(&[]).into()),
                    PrefixedOpcode::SET2B => Some(PrefixedInstruction::set2b(&[]).into()),
                    PrefixedOpcode::SET2C => Some(PrefixedInstruction::set2c(&[]).into()),
                    PrefixedOpcode::SET2D => Some(PrefixedInstruction::set2d(&[]).into()),
                    PrefixedOpcode::SET2E => Some(PrefixedInstruction::set2e(&[]).into()),
                    PrefixedOpcode::SET2H => Some(PrefixedInstruction::set2h(&[]).into()),
                    PrefixedOpcode::SET2L => Some(PrefixedInstruction::set2l(&[]).into()),
                    PrefixedOpcode::SET2A => Some(PrefixedInstruction::set2a(&[]).into()),
                    PrefixedOpcode::SET3B => Some(PrefixedInstruction::set3b(&[]).into()),
                    PrefixedOpcode::SET3C => Some(PrefixedInstruction::set3c(&[]).into()),
                    PrefixedOpcode::SET3D => Some(PrefixedInstruction::set3d(&[]).into()),
                    PrefixedOpcode::SET3E => Some(PrefixedInstruction::set3e(&[]).into()),
                    PrefixedOpcode::SET3H => Some(PrefixedInstruction::set3h(&[]).into()),
                    PrefixedOpcode::SET3L => Some(PrefixedInstruction::set3l(&[]).into()),
                    PrefixedOpcode::SET3A => Some(PrefixedInstruction::set3a(&[]).into()),
                    PrefixedOpcode::SET4B => Some(PrefixedInstruction::set4b(&[]).into()),
                    PrefixedOpcode::SET4C => Some(PrefixedInstruction::set4c(&[]).into()),
                    PrefixedOpcode::SET4D => Some(PrefixedInstruction::set4d(&[]).into()),
                    PrefixedOpcode::SET4E => Some(PrefixedInstruction::set4e(&[]).into()),
                    PrefixedOpcode::SET4H => Some(PrefixedInstruction::set4h(&[]).into()),
                    PrefixedOpcode::SET4L => Some(PrefixedInstruction::set4l(&[]).into()),
                    PrefixedOpcode::SET4A => Some(PrefixedInstruction::set4a(&[]).into()),
                    PrefixedOpcode::SET5B => Some(PrefixedInstruction::set5b(&[]).into()),
                    PrefixedOpcode::SET5C => Some(PrefixedInstruction::set5c(&[]).into()),
                    PrefixedOpcode::SET5D => Some(PrefixedInstruction::set5d(&[]).into()),
                    PrefixedOpcode::SET5E => Some(PrefixedInstruction::set5e(&[]).into()),
                    PrefixedOpcode::SET5H => Some(PrefixedInstruction::set5h(&[]).into()),
                    PrefixedOpcode::SET5L => Some(PrefixedInstruction::set5l(&[]).into()),
                    PrefixedOpcode::SET5A => Some(PrefixedInstruction::set5a(&[]).into()),
                    PrefixedOpcode::SET6B => Some(PrefixedInstruction::set6b(&[]).into()),
                    PrefixedOpcode::SET6C => Some(PrefixedInstruction::set6c(&[]).into()),
                    PrefixedOpcode::SET6D => Some(PrefixedInstruction::set6d(&[]).into()),
                    PrefixedOpcode::SET6E => Some(PrefixedInstruction::set6e(&[]).into()),
                    PrefixedOpcode::SET6H => Some(PrefixedInstruction::set6h(&[]).into()),
                    PrefixedOpcode::SET6L => Some(PrefixedInstruction::set6l(&[]).into()),
                    PrefixedOpcode::SET6A => Some(PrefixedInstruction::set6a(&[]).into()),
                    PrefixedOpcode::SET7B => Some(PrefixedInstruction::set7b(&[]).into()),
                    PrefixedOpcode::SET7C => Some(PrefixedInstruction::set7c(&[]).into()),
                    PrefixedOpcode::SET7D => Some(PrefixedInstruction::set7d(&[]).into()),
                    PrefixedOpcode::SET7E => Some(PrefixedInstruction::set7e(&[]).into()),
                    PrefixedOpcode::SET7H => Some(PrefixedInstruction::set7h(&[]).into()),
                    PrefixedOpcode::SET7L => Some(PrefixedInstruction::set7l(&[]).into()),
                    PrefixedOpcode::SET7A => Some(PrefixedInstruction::set7a(&[]).into()),
                    PrefixedOpcode::RR_B => Some(PrefixedInstruction::rr_b(&[]).into()),
                    PrefixedOpcode::RR_C => Some(PrefixedInstruction::rr_c(&[]).into()),
                    PrefixedOpcode::RR_D => Some(PrefixedInstruction::rr_d(&[]).into()),
                    PrefixedOpcode::RR_E => Some(PrefixedInstruction::rr_e(&[]).into()),
                    PrefixedOpcode::RR_H => Some(PrefixedInstruction::rr_h(&[]).into()),
                    PrefixedOpcode::RR_L => Some(PrefixedInstruction::rr_l(&[]).into()),
                    PrefixedOpcode::RR_A => Some(PrefixedInstruction::rr_a(&[]).into()),
                    PrefixedOpcode::RRC_A => Some(PrefixedInstruction::rrc_a(&[]).into()),
                    PrefixedOpcode::RRC_B => Some(PrefixedInstruction::rrc_b(&[]).into()),
                    PrefixedOpcode::RRC_C => Some(PrefixedInstruction::rrc_c(&[]).into()),
                    PrefixedOpcode::RRC_D => Some(PrefixedInstruction::rrc_d(&[]).into()),
                    PrefixedOpcode::RRC_E => Some(PrefixedInstruction::rrc_e(&[]).into()),
                    PrefixedOpcode::RRC_H => Some(PrefixedInstruction::rrc_h(&[]).into()),
                    PrefixedOpcode::RRC_L => Some(PrefixedInstruction::rrc_l(&[]).into()),
                    PrefixedOpcode::SLA_B => Some(PrefixedInstruction::sla_b(&[]).into()),
                    PrefixedOpcode::SLA_C => Some(PrefixedInstruction::sla_c(&[]).into()),
                    PrefixedOpcode::SLA_D => Some(PrefixedInstruction::sla_d(&[]).into()),
                    PrefixedOpcode::SLA_E => Some(PrefixedInstruction::sla_e(&[]).into()),
                    PrefixedOpcode::SLA_H => Some(PrefixedInstruction::sla_h(&[]).into()),
                    PrefixedOpcode::SLA_L => Some(PrefixedInstruction::sla_l(&[]).into()),
                    PrefixedOpcode::SLA_A => Some(PrefixedInstruction::sla_a(&[]).into()),
                    PrefixedOpcode::SRA_B => Some(PrefixedInstruction::sra_b(&[]).into()),
                    PrefixedOpcode::SRA_C => Some(PrefixedInstruction::sra_c(&[]).into()),
                    PrefixedOpcode::SRA_D => Some(PrefixedInstruction::sra_d(&[]).into()),
                    PrefixedOpcode::SRA_E => Some(PrefixedInstruction::sra_e(&[]).into()),
                    PrefixedOpcode::SRA_H => Some(PrefixedInstruction::sra_h(&[]).into()),
                    PrefixedOpcode::SRA_L => Some(PrefixedInstruction::sra_l(&[]).into()),
                    PrefixedOpcode::SRA_A => Some(PrefixedInstruction::sra_a(&[]).into()),
                    PrefixedOpcode::SRL_B => Some(PrefixedInstruction::srl_b(&[]).into()),
                    PrefixedOpcode::SRL_C => Some(PrefixedInstruction::srl_c(&[]).into()),
                    PrefixedOpcode::SRL_D => Some(PrefixedInstruction::srl_d(&[]).into()),
                    PrefixedOpcode::SRL_E => Some(PrefixedInstruction::srl_e(&[]).into()),
                    PrefixedOpcode::SRL_H => Some(PrefixedInstruction::srl_h(&[]).into()),
                    PrefixedOpcode::SRL_L => Some(PrefixedInstruction::srl_l(&[]).into()),
                    PrefixedOpcode::SRL_A => Some(PrefixedInstruction::srl_a(&[]).into()),
                    PrefixedOpcode::SWAP_B => Some(PrefixedInstruction::swap_b(&[]).into()),
                    PrefixedOpcode::SWAP_C => Some(PrefixedInstruction::swap_c(&[]).into()),
                    PrefixedOpcode::SWAP_D => Some(PrefixedInstruction::swap_d(&[]).into()),
                    PrefixedOpcode::SWAP_E => Some(PrefixedInstruction::swap_e(&[]).into()),
                    PrefixedOpcode::SWAP_H => Some(PrefixedInstruction::swap_h(&[]).into()),
                    PrefixedOpcode::SWAP_L => Some(PrefixedInstruction::swap_l(&[]).into()),
                    PrefixedOpcode::SWAP_A => Some(PrefixedInstruction::swap_a(&[]).into()),
                }
            }
            Opcode::DI => Some(NormalInstruction::di(&[]).into()),
            Opcode::EI => Some(NormalInstruction::ei(&[]).into()),
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

    fn halt(&mut self) {
        self.halted = true;
    }

    fn call_a16(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        self.branched = true;

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
        self.branched = true;

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

    fn sub_ind_hl(&mut self, mmu: &Mmu) {
        let val = self.sub8(
            self.regs.read_reg(Reg::A),
            mmu.read_byte(self.regs.read_reg16(Reg16::HL) as usize),
        );

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

    fn inc_ind_hl(&mut self, mmu: &mut Mmu) {
        let addr = self.regs.read_reg16(Reg16::HL) as usize;
        let val = mmu.read_byte(addr).wrapping_add(1);

        mmu.write_byte(addr, val);

        self.check_z(val);
        self.clear_n();
        self.set_h_to(val & 0xf == 0xf);
    }

    fn dec_ind_hl(&mut self, mmu: &mut Mmu) {
        let addr = self.regs.read_reg16(Reg16::HL) as usize;
        let val = mmu.read_byte(addr).wrapping_sub(1);

        mmu.write_byte(addr, val);

        self.check_z(val);
        self.set_n();
        self.set_h_to(val & 0xf == 0xf);
    }

    fn ld_r_ind_hl_dec(&mut self, reg: Reg, mmu: &mut Mmu) {
        self.regs
            .write_reg(reg, mmu.read_byte(self.regs.read_reg16(Reg16::HL) as usize));
        self.regs.dec_reg16(Reg16::HL);
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

    fn bit_ind_hl(&mut self, n: u8, mmu: &Mmu) {
        self.clear_n();
        self.set_h();

        if mmu.read_byte(self.regs.read_reg16(Reg16::HL) as usize) & 1 << n != 0 {
            self.clear_z();
        } else {
            self.set_z();
        }
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

    fn res_r(&mut self, reg: Reg, n: u8) {
        let val = self.regs.read_reg(reg) & !(1 << n);
        self.regs.write_reg(reg, val);
    }

    fn set_r(&mut self, reg: Reg, n: u8) {
        let val = self.regs.read_reg(reg) | (1 << n);
        self.regs.write_reg(reg, val);
    }

    fn rl_r(&mut self, reg: Reg) {
        let val = self.regs.read_reg(reg);
        let carry: u8 = self.c().into();
        let will_carry = val & 0b10000000 != 0;

        let val = val.wrapping_shl(1) | carry;
        self.regs.write_reg(reg, val);

        self.check_z(val);
        self.clear_n();
        self.clear_h();
        self.set_c_to(will_carry);
    }

    fn rla(&mut self) {
        self.rl_r(Reg::A);
        self.clear_z();
    }

    fn rlca(&mut self) {
        let val = self.rlc8(self.regs.read_reg(Reg::A));
        self.regs.write_reg(Reg::A, val);
        self.clear_z()
    }

    fn rlc_r(&mut self, reg: Reg) {
        let val = self.rlc8(self.regs.read_reg(reg));
        self.regs.write_reg(reg, val);
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

        self.check_z(val);
        self.clear_n();
        self.clear_h();
        self.set_c_to(will_carry);

        self.regs.write_reg(reg, val);
    }

    fn rra(&mut self) {
        self.rr_r(Reg::A);
        self.clear_z();
    }

    fn rrca(&mut self) {
        let val = self.rrc8(self.regs.read_reg(Reg::A));
        self.regs.write_reg(Reg::A, val);
        self.clear_z();
    }

    fn rrc_r(&mut self, reg: Reg) {
        let val = self.rrc8(self.regs.read_reg(reg));
        self.regs.write_reg(reg, val);
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

    fn sla8(&mut self, x: u8) -> u8 {
        let carry = x & 0b10000000 != 0;
        let res = x.wrapping_shl(1);

        self.set_c_to(carry);
        self.check_z(res);
        self.clear_h();
        self.clear_n();

        res
    }

    fn sla_r(&mut self, reg: Reg) {
        let val = self.sla8(self.regs.read_reg(reg));
        self.regs.write_reg(reg, val);
    }

    fn sra8(&mut self, x: u8) -> u8 {
        let carry = x & 0b00000001 != 0;
        let truncated_bit = (x & 0b10000000) >> 7;

        let res = (x >> 1) | (truncated_bit << 7);

        self.set_c_to(carry);
        self.check_z(res);
        self.clear_h();
        self.clear_n();

        res
    }

    fn sra_r(&mut self, reg: Reg) {
        let val = self.sra8(self.regs.read_reg(reg));
        self.regs.write_reg(reg, val);
    }

    fn srl8(&mut self, x: u8) -> u8 {
        let carry = x & 0b00000001 != 0;

        let res = x >> 1;
        self.clear_n();
        self.clear_h();
        self.check_z(res);
        self.set_c_to(carry);

        res
    }

    fn srl_r(&mut self, reg: Reg) {
        let val = self.srl8(self.regs.read_reg(reg));
        self.regs.write_reg(reg, val);
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
        self.branched = true;

        let offset = data[0] as i8;
        self.regs.write_reg16(
            Reg16::PC,
            self.regs
                .read_reg16(Reg16::PC)
                .wrapping_add_signed(offset as i16),
        );
    }

    fn jp_a16(&mut self, data: &[u8; 2]) {
        self.branched = true;

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
                cpu.regs.write_reg($dst, 0x40);
                mmu.write_slice($mnemonic, 0);
                mmu.write_byte(0xff40, 0xaa);

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
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 2);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
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

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
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

    macro_rules! test_rl_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 1);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
                assert_eq!(cpu.regs.read_reg($r), 2);
            }
        };
    }

    macro_rules! test_sla_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 1);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
                assert_eq!(cpu.regs.read_reg($r), 2);
            }
        };
    }

    macro_rules! test_sra_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 2);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
                assert_eq!(cpu.regs.read_reg($r), 1);
            }
        };
    }

    macro_rules! test_swap_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 0xab);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
                assert_eq!(cpu.regs.read_reg($r), 0xba);
            }
        };
    }

    macro_rules! test_srl_r {
        ($r:expr, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 2);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
                assert_eq!(cpu.regs.read_reg($r), 1);
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

    macro_rules! test_bit {
        ($r:expr, $mnemonic:expr, $i:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 1 | 1 << $i);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(cpu.h());

                cpu.regs.write_reg16(Reg16::PC, 0);
                cpu.regs.write_reg($r, 1 & !(1 << $i));
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(cpu.z());
                assert!(!cpu.n());
                assert!(cpu.h());
            }
        };
    }

    macro_rules! test_bit_ind_hl {
        ($mnemonic:expr, $i:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg16(Reg16::HL, 0xdead);
                mmu.write_byte(0xdead, 1 | 1 << $i);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(cpu.h());

                cpu.regs.write_reg16(Reg16::PC, 0);
                mmu.write_byte(0xdead, 1 & !(1 << $i));
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert!(cpu.z());
                assert!(!cpu.n());
                assert!(cpu.h());
            }
        };
    }

    macro_rules! test_res {
        ($r:expr, $mnemonic:expr, $i:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 0 | 1 << $i);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert_eq!(cpu.regs.read_reg($r), 0);
            }
        };
    }

    macro_rules! test_set {
        ($r:expr, $mnemonic:expr, $i:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.regs.write_reg($r, 0);
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
                assert_eq!(cpu.regs.read_reg($r), 0 | 1 << $i);
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
    test_rst_d8!(0x08, &[0xcf], test_rst_08);
    test_rst_d8!(0x10, &[0xd7], test_rst_10);
    test_rst_d8!(0x18, &[0xdf], test_rst_18);
    test_rst_d8!(0x20, &[0xe7], test_rst_20);
    test_rst_d8!(0x28, &[0xef], test_rst_28);
    test_rst_d8!(0x30, &[0xf7], test_rst_30);
    test_rst_d8!(0x38, &[0xff], test_rst_38);

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
        mmu.write_slice(&[0xe0, 0x40], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
        assert_eq!(mmu.read_byte(0xff40), 42);
    }

    test_ld_r_a16!(Reg::A, &[0xfa, 0x05, 0x00], test_ld_a_a16);

    #[test]
    fn test_ldh_a_a8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 0);
        mmu.write_slice(&[0xf0, 0x40], 0);
        mmu.write_byte(0xff40, 42);

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
    fn test_subw_ind_hl() {
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
    fn test_inc_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg16(Reg16::HL, 1);
        mmu.write_slice(&[0x34, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(mmu.read_byte(1), 11);
        assert!(!cpu.n());
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

    test_ld_ind_rr_r!(Reg::A, Reg16::BC, &[0x02], test_ld_ind_bc_a);
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
        cpu.regs.write_reg(Reg::C, 0x40);
        mmu.write_slice(&[0xe2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(mmu.read_byte(0xff40), 42);
    }

    test_ld_r_ind_rr!(Reg16::DE, Reg::A, &[0x1a, 0xff], test_ld_a_ind_de);
    test_ld_r_ind_rr!(Reg16::HL, Reg::A, &[0x7e, 0xff], test_ld_a_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::B, &[0x46, 0xff], test_ld_b_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::C, &[0x4e, 0xff], test_ld_c_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::D, &[0x56, 0xff], test_ld_d_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::E, &[0x5e, 0xff], test_ld_e_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::H, &[0x66, 0xff], test_ld_h_ind_hl);
    test_ld_r_ind_rr!(Reg16::HL, Reg::L, &[0x6e, 0xff], test_ld_l_ind_hl);

    test_ld_r_ind_rr!(Reg16::BC, Reg::A, &[0x0a, 0xff], test_ld_a_ind_bc);

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
    fn test_ld_a_ind_hl_dec() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.regs.write_reg(Reg::A, 0);
        cpu.regs.write_reg16(Reg16::HL, 1);
        mmu.write_slice(&[0x3a, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 1);
        assert_eq!(cpu.regs.read_reg(Reg::A), 0xff);
        assert_eq!(cpu.regs.read_reg16(Reg16::HL), 0);
    }

    test_bit!(Reg::B, &[0xcb, 0x40], 0, test_bit0b);
    test_bit!(Reg::C, &[0xcb, 0x41], 0, test_bit0c);
    test_bit!(Reg::D, &[0xcb, 0x42], 0, test_bit0d);
    test_bit!(Reg::E, &[0xcb, 0x43], 0, test_bit0e);
    test_bit!(Reg::H, &[0xcb, 0x44], 0, test_bit0h);
    test_bit!(Reg::L, &[0xcb, 0x45], 0, test_bit0l);
    test_bit!(Reg::A, &[0xcb, 0x47], 0, test_bit0a);

    test_bit!(Reg::B, &[0xcb, 0x48], 1, test_bit1b);
    test_bit!(Reg::C, &[0xcb, 0x49], 1, test_bit1c);
    test_bit!(Reg::D, &[0xcb, 0x4a], 1, test_bit1d);
    test_bit!(Reg::E, &[0xcb, 0x4b], 1, test_bit1e);
    test_bit!(Reg::H, &[0xcb, 0x4c], 1, test_bit1h);
    test_bit!(Reg::L, &[0xcb, 0x4d], 1, test_bit1l);
    test_bit!(Reg::A, &[0xcb, 0x4f], 1, test_bit1a);

    test_bit!(Reg::B, &[0xcb, 0x50], 2, test_bit2b);
    test_bit!(Reg::C, &[0xcb, 0x51], 2, test_bit2c);
    test_bit!(Reg::D, &[0xcb, 0x52], 2, test_bit2d);
    test_bit!(Reg::E, &[0xcb, 0x53], 2, test_bit2e);
    test_bit!(Reg::H, &[0xcb, 0x54], 2, test_bit2h);
    test_bit!(Reg::L, &[0xcb, 0x55], 2, test_bit2l);
    test_bit!(Reg::A, &[0xcb, 0x57], 2, test_bit2a);

    test_bit!(Reg::B, &[0xcb, 0x58], 3, test_bit3b);
    test_bit!(Reg::C, &[0xcb, 0x59], 3, test_bit3c);
    test_bit!(Reg::D, &[0xcb, 0x5a], 3, test_bit3d);
    test_bit!(Reg::E, &[0xcb, 0x5b], 3, test_bit3e);
    test_bit!(Reg::H, &[0xcb, 0x5c], 3, test_bit3h);
    test_bit!(Reg::L, &[0xcb, 0x5d], 3, test_bit3l);
    test_bit!(Reg::A, &[0xcb, 0x5e], 3, test_bit3a);

    test_bit!(Reg::B, &[0xcb, 0x60], 4, test_bit4b);
    test_bit!(Reg::C, &[0xcb, 0x61], 4, test_bit4c);
    test_bit!(Reg::D, &[0xcb, 0x62], 4, test_bit4d);
    test_bit!(Reg::E, &[0xcb, 0x63], 4, test_bit4e);
    test_bit!(Reg::H, &[0xcb, 0x64], 4, test_bit4h);
    test_bit!(Reg::L, &[0xcb, 0x65], 4, test_bit4l);
    test_bit!(Reg::A, &[0xcb, 0x67], 4, test_bit4a);

    test_bit!(Reg::B, &[0xcb, 0x68], 5, test_bit5b);
    test_bit!(Reg::C, &[0xcb, 0x69], 5, test_bit5c);
    test_bit!(Reg::D, &[0xcb, 0x6a], 5, test_bit5d);
    test_bit!(Reg::E, &[0xcb, 0x6b], 5, test_bit5e);
    test_bit!(Reg::H, &[0xcb, 0x6c], 5, test_bit5h);
    test_bit!(Reg::L, &[0xcb, 0x6d], 5, test_bit5l);
    test_bit!(Reg::A, &[0xcb, 0x6f], 5, test_bit5a);

    test_bit!(Reg::B, &[0xcb, 0x70], 6, test_bit6b);
    test_bit!(Reg::C, &[0xcb, 0x71], 6, test_bit6c);
    test_bit!(Reg::D, &[0xcb, 0x72], 6, test_bit6d);
    test_bit!(Reg::E, &[0xcb, 0x73], 6, test_bit6e);
    test_bit!(Reg::H, &[0xcb, 0x74], 6, test_bit6h);
    test_bit!(Reg::L, &[0xcb, 0x75], 6, test_bit6l);
    test_bit!(Reg::A, &[0xcb, 0x77], 6, test_bit6a);

    test_bit!(Reg::B, &[0xcb, 0x78], 7, test_bit7b);
    test_bit!(Reg::C, &[0xcb, 0x79], 7, test_bit7c);
    test_bit!(Reg::D, &[0xcb, 0x7a], 7, test_bit7d);
    test_bit!(Reg::E, &[0xcb, 0x7b], 7, test_bit7e);
    test_bit!(Reg::H, &[0xcb, 0x7c], 7, test_bit7h);
    test_bit!(Reg::L, &[0xcb, 0x7d], 7, test_bit7l);
    test_bit!(Reg::A, &[0xcb, 0x7f], 7, test_bit7a);

    test_bit_ind_hl!(&[0xcb, 0x7e], 7, test_bit7_ind_hl);

    test_res!(Reg::B, &[0xcb, 0x80], 0, test_res0b);
    test_res!(Reg::C, &[0xcb, 0x81], 0, test_res0c);
    test_res!(Reg::D, &[0xcb, 0x82], 0, test_res0d);
    test_res!(Reg::E, &[0xcb, 0x83], 0, test_res0e);
    test_res!(Reg::H, &[0xcb, 0x84], 0, test_res0h);
    test_res!(Reg::L, &[0xcb, 0x85], 0, test_res0l);
    test_res!(Reg::A, &[0xcb, 0x87], 0, test_res0a);

    test_res!(Reg::B, &[0xcb, 0x88], 1, test_res1b);
    test_res!(Reg::C, &[0xcb, 0x89], 1, test_res1c);
    test_res!(Reg::D, &[0xcb, 0x8a], 1, test_res1d);
    test_res!(Reg::E, &[0xcb, 0x8b], 1, test_res1e);
    test_res!(Reg::H, &[0xcb, 0x8c], 1, test_res1h);
    test_res!(Reg::L, &[0xcb, 0x8d], 1, test_res1l);
    test_res!(Reg::A, &[0xcb, 0x8f], 1, test_res1a);

    test_res!(Reg::B, &[0xcb, 0x90], 2, test_res2b);
    test_res!(Reg::C, &[0xcb, 0x91], 2, test_res2c);
    test_res!(Reg::D, &[0xcb, 0x92], 2, test_res2d);
    test_res!(Reg::E, &[0xcb, 0x93], 2, test_res2e);
    test_res!(Reg::H, &[0xcb, 0x94], 2, test_res2h);
    test_res!(Reg::L, &[0xcb, 0x95], 2, test_res2l);
    test_res!(Reg::A, &[0xcb, 0x97], 2, test_res2a);

    test_res!(Reg::B, &[0xcb, 0x98], 3, test_res3b);
    test_res!(Reg::C, &[0xcb, 0x99], 3, test_res3c);
    test_res!(Reg::D, &[0xcb, 0x9a], 3, test_res3d);
    test_res!(Reg::E, &[0xcb, 0x9b], 3, test_res3e);
    test_res!(Reg::H, &[0xcb, 0x9c], 3, test_res3h);
    test_res!(Reg::L, &[0xcb, 0x9d], 3, test_res3l);
    test_res!(Reg::A, &[0xcb, 0x9e], 3, test_res3a);

    test_res!(Reg::B, &[0xcb, 0xa0], 4, test_res4b);
    test_res!(Reg::C, &[0xcb, 0xa1], 4, test_res4c);
    test_res!(Reg::D, &[0xcb, 0xa2], 4, test_res4d);
    test_res!(Reg::E, &[0xcb, 0xa3], 4, test_res4e);
    test_res!(Reg::H, &[0xcb, 0xa4], 4, test_res4h);
    test_res!(Reg::L, &[0xcb, 0xa5], 4, test_res4l);
    test_res!(Reg::A, &[0xcb, 0xa7], 4, test_res4a);

    test_res!(Reg::B, &[0xcb, 0xa8], 5, test_res5b);
    test_res!(Reg::C, &[0xcb, 0xa9], 5, test_res5c);
    test_res!(Reg::D, &[0xcb, 0xaa], 5, test_res5d);
    test_res!(Reg::E, &[0xcb, 0xab], 5, test_res5e);
    test_res!(Reg::H, &[0xcb, 0xac], 5, test_res5h);
    test_res!(Reg::L, &[0xcb, 0xad], 5, test_res5l);
    test_res!(Reg::A, &[0xcb, 0xaf], 5, test_res5a);

    test_res!(Reg::B, &[0xcb, 0xb0], 6, test_res6b);
    test_res!(Reg::C, &[0xcb, 0xb1], 6, test_res6c);
    test_res!(Reg::D, &[0xcb, 0xb2], 6, test_res6d);
    test_res!(Reg::E, &[0xcb, 0xb3], 6, test_res6e);
    test_res!(Reg::H, &[0xcb, 0xb4], 6, test_res6h);
    test_res!(Reg::L, &[0xcb, 0xb5], 6, test_res6l);
    test_res!(Reg::A, &[0xcb, 0xb7], 6, test_res6a);

    test_res!(Reg::B, &[0xcb, 0xb8], 7, test_res7b);
    test_res!(Reg::C, &[0xcb, 0xb9], 7, test_res7c);
    test_res!(Reg::D, &[0xcb, 0xba], 7, test_res7d);
    test_res!(Reg::E, &[0xcb, 0xbb], 7, test_res7e);
    test_res!(Reg::H, &[0xcb, 0xbc], 7, test_res7h);
    test_res!(Reg::L, &[0xcb, 0xbd], 7, test_res7l);
    test_res!(Reg::A, &[0xcb, 0xbf], 7, test_res7a);

    test_set!(Reg::B, &[0xcb, 0xc0], 0, test_set0b);
    test_set!(Reg::C, &[0xcb, 0xc1], 0, test_set0c);
    test_set!(Reg::D, &[0xcb, 0xc2], 0, test_set0d);
    test_set!(Reg::E, &[0xcb, 0xc3], 0, test_set0e);
    test_set!(Reg::H, &[0xcb, 0xc4], 0, test_set0h);
    test_set!(Reg::L, &[0xcb, 0xc5], 0, test_set0l);
    test_set!(Reg::A, &[0xcb, 0xc7], 0, test_set0a);

    test_set!(Reg::B, &[0xcb, 0xc8], 1, test_set1b);
    test_set!(Reg::C, &[0xcb, 0xc9], 1, test_set1c);
    test_set!(Reg::D, &[0xcb, 0xca], 1, test_set1d);
    test_set!(Reg::E, &[0xcb, 0xcb], 1, test_set1e);
    test_set!(Reg::H, &[0xcb, 0xcc], 1, test_set1h);
    test_set!(Reg::L, &[0xcb, 0xcd], 1, test_set1l);
    test_set!(Reg::A, &[0xcb, 0xcf], 1, test_set1a);

    test_set!(Reg::B, &[0xcb, 0xd0], 2, test_set2b);
    test_set!(Reg::C, &[0xcb, 0xd1], 2, test_set2c);
    test_set!(Reg::D, &[0xcb, 0xd2], 2, test_set2d);
    test_set!(Reg::E, &[0xcb, 0xd3], 2, test_set2e);
    test_set!(Reg::H, &[0xcb, 0xd4], 2, test_set2h);
    test_set!(Reg::L, &[0xcb, 0xd5], 2, test_set2l);
    test_set!(Reg::A, &[0xcb, 0xd7], 2, test_set2a);

    test_set!(Reg::B, &[0xcb, 0xd8], 3, test_set3b);
    test_set!(Reg::C, &[0xcb, 0xd9], 3, test_set3c);
    test_set!(Reg::D, &[0xcb, 0xda], 3, test_set3d);
    test_set!(Reg::E, &[0xcb, 0xdb], 3, test_set3e);
    test_set!(Reg::H, &[0xcb, 0xdc], 3, test_set3h);
    test_set!(Reg::L, &[0xcb, 0xdd], 3, test_set3l);
    test_set!(Reg::A, &[0xcb, 0xde], 3, test_set3a);

    test_set!(Reg::B, &[0xcb, 0xe0], 4, test_set4b);
    test_set!(Reg::C, &[0xcb, 0xe1], 4, test_set4c);
    test_set!(Reg::D, &[0xcb, 0xe2], 4, test_set4d);
    test_set!(Reg::E, &[0xcb, 0xe3], 4, test_set4e);
    test_set!(Reg::H, &[0xcb, 0xe4], 4, test_set4h);
    test_set!(Reg::L, &[0xcb, 0xe5], 4, test_set4l);
    test_set!(Reg::A, &[0xcb, 0xe7], 4, test_set4a);

    test_set!(Reg::B, &[0xcb, 0xe8], 5, test_set5b);
    test_set!(Reg::C, &[0xcb, 0xe9], 5, test_set5c);
    test_set!(Reg::D, &[0xcb, 0xea], 5, test_set5d);
    test_set!(Reg::E, &[0xcb, 0xeb], 5, test_set5e);
    test_set!(Reg::H, &[0xcb, 0xec], 5, test_set5h);
    test_set!(Reg::L, &[0xcb, 0xed], 5, test_set5l);
    test_set!(Reg::A, &[0xcb, 0xef], 5, test_set5a);

    test_set!(Reg::B, &[0xcb, 0xf0], 6, test_set6b);
    test_set!(Reg::C, &[0xcb, 0xf1], 6, test_set6c);
    test_set!(Reg::D, &[0xcb, 0xf2], 6, test_set6d);
    test_set!(Reg::E, &[0xcb, 0xf3], 6, test_set6e);
    test_set!(Reg::H, &[0xcb, 0xf4], 6, test_set6h);
    test_set!(Reg::L, &[0xcb, 0xf5], 6, test_set6l);
    test_set!(Reg::A, &[0xcb, 0xf7], 6, test_set6a);

    test_set!(Reg::B, &[0xcb, 0xf8], 7, test_set7b);
    test_set!(Reg::C, &[0xcb, 0xf9], 7, test_set7c);
    test_set!(Reg::D, &[0xcb, 0xfa], 7, test_set7d);
    test_set!(Reg::E, &[0xcb, 0xfb], 7, test_set7e);
    test_set!(Reg::H, &[0xcb, 0xfc], 7, test_set7h);
    test_set!(Reg::L, &[0xcb, 0xfd], 7, test_set7l);
    test_set!(Reg::A, &[0xcb, 0xff], 7, test_set7a);

    test_rlc_r!(Reg::A, &[0xcb, 0x07], test_rlc_a);
    test_rlc_r!(Reg::B, &[0xcb, 0x00], test_rlc_b);
    test_rlc_r!(Reg::C, &[0xcb, 0x01], test_rlc_c);
    test_rlc_r!(Reg::D, &[0xcb, 0x02], test_rlc_d);
    test_rlc_r!(Reg::E, &[0xcb, 0x03], test_rlc_e);
    test_rlc_r!(Reg::H, &[0xcb, 0x04], test_rlc_h);
    test_rlc_r!(Reg::L, &[0xcb, 0x05], test_rlc_hl);

    test_rl_r!(Reg::B, &[0xcb, 0x10], test_rl_b);
    test_rl_r!(Reg::C, &[0xcb, 0x11], test_rl_c);
    test_rl_r!(Reg::D, &[0xcb, 0x12], test_rl_d);
    test_rl_r!(Reg::E, &[0xcb, 0x13], test_rl_e);
    test_rl_r!(Reg::H, &[0xcb, 0x14], test_rl_h);
    test_rl_r!(Reg::L, &[0xcb, 0x15], test_rl_l);
    test_rl_r!(Reg::A, &[0xcb, 0x17], test_rl_a);

    test_rr_r!(Reg::A, &[0xcb, 0x1f], 12, 2, test_rr_a);
    test_rr_r!(Reg::B, &[0xcb, 0x18], 12, 2, test_rr_b);
    test_rr_r!(Reg::C, &[0xcb, 0x19], 12, 2, test_rr_c);
    test_rr_r!(Reg::D, &[0xcb, 0x1a], 12, 2, test_rr_d);
    test_rr_r!(Reg::E, &[0xcb, 0x1b], 12, 2, test_rr_e);
    test_rr_r!(Reg::H, &[0xcb, 0x1c], 12, 2, test_rr_h);
    test_rr_r!(Reg::L, &[0xcb, 0x1d], 12, 2, test_rr_l);

    test_rrc_r!(Reg::A, &[0xcb, 0x0f], test_rrc_a);
    test_rrc_r!(Reg::B, &[0xcb, 0x08], test_rrc_b);
    test_rrc_r!(Reg::C, &[0xcb, 0x09], test_rrc_c);
    test_rrc_r!(Reg::D, &[0xcb, 0x0a], test_rrc_d);
    test_rrc_r!(Reg::E, &[0xcb, 0x0b], test_rrc_e);
    test_rrc_r!(Reg::H, &[0xcb, 0x0c], test_rrc_h);
    test_rrc_r!(Reg::L, &[0xcb, 0x0d], test_rrc_l);

    test_srl_r!(Reg::B, &[0xcb, 0x38], test_srl_b);
    test_srl_r!(Reg::C, &[0xcb, 0x39], test_srl_c);
    test_srl_r!(Reg::D, &[0xcb, 0x3a], test_srl_d);
    test_srl_r!(Reg::E, &[0xcb, 0x3b], test_srl_e);
    test_srl_r!(Reg::H, &[0xcb, 0x3c], test_srl_h);
    test_srl_r!(Reg::L, &[0xcb, 0x3d], test_srl_l);
    test_srl_r!(Reg::A, &[0xcb, 0x3f], test_srl_a);

    test_sla_r!(Reg::B, &[0xcb, 0x20], test_sla_b);
    test_sla_r!(Reg::C, &[0xcb, 0x21], test_sla_c);
    test_sla_r!(Reg::D, &[0xcb, 0x22], test_sla_d);
    test_sla_r!(Reg::E, &[0xcb, 0x23], test_sla_e);
    test_sla_r!(Reg::H, &[0xcb, 0x24], test_sla_h);
    test_sla_r!(Reg::L, &[0xcb, 0x25], test_sla_l);
    test_sla_r!(Reg::A, &[0xcb, 0x27], test_sla_a);

    test_sra_r!(Reg::B, &[0xcb, 0x28], test_sa_b);
    test_sra_r!(Reg::C, &[0xcb, 0x29], test_sa_c);
    test_sra_r!(Reg::D, &[0xcb, 0x2a], test_sa_d);
    test_sra_r!(Reg::E, &[0xcb, 0x2b], test_sa_e);
    test_sra_r!(Reg::H, &[0xcb, 0x2c], test_sa_h);
    test_sra_r!(Reg::L, &[0xcb, 0x2d], test_sa_l);
    test_sra_r!(Reg::A, &[0xcb, 0x2f], test_sa_a);

    test_swap_r!(Reg::B, &[0xcb, 0x30], test_swap_b);
    test_swap_r!(Reg::C, &[0xcb, 0x31], test_swap_c);
    test_swap_r!(Reg::D, &[0xcb, 0x32], test_swap_d);
    test_swap_r!(Reg::E, &[0xcb, 0x33], test_swap_e);
    test_swap_r!(Reg::H, &[0xcb, 0x34], test_swap_h);
    test_swap_r!(Reg::L, &[0xcb, 0x35], test_swap_l);
    test_swap_r!(Reg::A, &[0xcb, 0x37], test_swap_a);

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

        assert_eq!(cycles, 8);
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

        assert_eq!(cycles, 8);
        assert_eq!(cpu.regs.read_reg16(Reg16::PC), 2);
    }

    #[test]
    fn test_jr_z_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x28, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
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

        assert_eq!(cycles, 8);
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
