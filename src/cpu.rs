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

macro_rules! little_endian {
    ($ll:expr, $hh:expr) => {
        ((($hh as u16) << 8) | (($ll as u16) & 0x00ff))
    };
}

macro_rules! impl_flag {
    ($set_flag:ident, $clear_flag:ident, $set_flag_to:ident, $flag:ident, $i:expr) => {
        fn $set_flag(&mut self) {
            self.f |= 1 << $i
        }
        fn $clear_flag(&mut self) {
            self.f &= !(1 << $i)
        }
        fn $set_flag_to(&mut self, value: bool) {
            if value {
                self.$set_flag()
            } else {
                self.$clear_flag()
            }
        }
        fn $flag(&self) -> bool {
            self.f & 1 << $i != 0
        }
    };
}

macro_rules! ld_rr_d16 {
    ($ll:ident, $hh:ident, $name:ident) => {
        fn $name(&mut self, data: &[u8; 2]) {
            self.$ll = data[0];
            self.$hh = data[1];
        }
    };
    ($rr:ident, $name:ident) => {
        fn $name(&mut self, data: &[u8; 2]) {
            self.$rr = u16::from_le_bytes(*data);
        }
    };
}

macro_rules! ld_ind_rr_r {
    ($ll: ident, $hh: ident, $r:ident, $name:ident) => {
        fn $name(&mut self, mmu: &mut Mmu) {
            mmu.write_byte(little_endian!(self.$ll, self.$hh) as usize, self.$r);
        }
    };
}

macro_rules! ld_ind_rr_d8 {
    ($ll: ident, $hh: ident, $name:ident) => {
        fn $name(&mut self, data: &[u8; 1], mmu: &mut Mmu) {
            mmu.write_byte(little_endian!(self.$ll, self.$hh) as usize, data[0]);
        }
    };
}

macro_rules! ld_r_ind_rr {
    ($ll: ident, $hh: ident, $r:ident, $name:ident) => {
        fn $name(&mut self, mmu: &mut Mmu) {
            self.$r = mmu.read_byte(little_endian!(self.$ll, self.$hh) as usize);
        }
    };
}

macro_rules! ld_r_ind_r {
    ($dst: ident, $r:ident, $name:ident) => {
        fn $name(&mut self, mmu: &mut Mmu) {
            self.$r = mmu.read_byte(0xff00 + (self.$dst as usize));
        }
    };
}

macro_rules! ld_r_d8 {
    ($rr:ident, $name:ident) => {
        fn $name(&mut self, data: &[u8; 1]) {
            self.$rr = data[0];
        }
    };
}

macro_rules! ld_r_r {
    ($dst:ident, $src:ident, $name:ident) => {
        fn $name(&mut self) {
            self.$dst = self.$src;
        }
    };
}

macro_rules! push_rr {
    ($hh:ident, $ll:ident, $name:ident) => {
        fn $name(&mut self, mmu: &mut Mmu) {
            self.sp -= 1;
            mmu.write_byte(self.sp as usize, self.$hh);

            self.sp -= 1;
            mmu.write_byte(self.sp as usize, self.$ll);
        }
    };
}

macro_rules! pop_rr {
    ($hh:ident, $ll:ident, $name:ident) => {
        fn $name(&mut self, mmu: &Mmu) {
            self.$ll = mmu.read_byte(self.sp as usize);
            self.sp += 1;

            self.$hh = mmu.read_byte(self.sp as usize);
            self.sp += 1;
        }
    };
}

macro_rules! dec_r {
    ($r:ident, $name:ident) => {
        fn $name(&mut self) {
            self.$r = self.$r.wrapping_sub(1);

            self.set_n();
            self.check_z(self.$r);
            self.set_h_to(self.$r & 0xf == 0xf);
        }
    };
}

macro_rules! inc_rr {
    ($ll:ident, $hh:ident, $name:ident) => {
        fn $name(&mut self) {
            let (res, c) = self.$ll.overflowing_add(1);
            self.$ll = res;

            if c {
                self.$hh = self.$hh.wrapping_add(1);
            }
        }
    };
}

macro_rules! dec_rr {
    ($ll:ident, $hh:ident, $name:ident) => {
        fn $name(&mut self) {
            let (res, c) = self.$ll.overflowing_sub(1);
            self.$ll = res;

            if c {
                self.$hh = self.$hh.wrapping_sub(1);
            }
        }
    };
}

macro_rules! bit {
    ($r:ident, $n:expr, $name:ident) => {
        fn $name(&mut self) {
            self.clear_n();
            self.set_h();

            if self.$r & 1 << $n != 0 {
                self.clear_z();
            } else {
                self.set_z();
            }
        }
    };
}

macro_rules! inc_r {
    ($r:ident, $name:ident) => {
        fn $name(&mut self) {
            self.$r = self.$r.wrapping_add(1);

            self.clear_n();
            self.check_z(self.$r);
            self.set_h_to(self.$r & 0xf == 0x0);
        }
    };
}

macro_rules! or_r {
    ($r: ident, $name:ident) => {
        fn $name(&mut self) {
            self.a = self.or(self.a, self.$r);
        }
    };
}

macro_rules! xor_r_r {
    ($x:ident, $y:ident, $name:ident) => {
        fn $name(&mut self) {
            self.$x = self.xor(self.$x, self.$y);
        }
    };
}

macro_rules! rl_r {
    ($r:ident, $name:ident) => {
        fn $name(&mut self) {
            let val = self.$r;
            let (res, c) = val.overflowing_shl(1);
            self.$r = res + val.wrapping_shr(7);

            self.clear_z();
            self.clear_n();
            self.clear_h();
            self.set_c_to(c);
        }
    };
}

macro_rules! rr_r {
    ($r:ident, $name:ident) => {
        fn $name(&mut self) {
            let val = self.$r;
            let c: u8 = self.c().into();

            let will_carry = val & 0x1 != 0;

            self.$r = c.wrapping_shl(7) | val.wrapping_shr(1);

            self.clear_n();
            self.clear_h();
            self.check_z(self.$r);
            self.set_c_to(will_carry);
        }
    };
}

macro_rules! ld_r_a16 {
    ($r:ident, $name:ident) => {
        fn $name(&mut self, data: &[u8; 2], mmu: &Mmu) {
            self.$r = mmu.read_byte(little_endian!(data[0], data[1]) as usize);
        }
    };
}

macro_rules! add_hl_rr {
    ($rr:ident, $name:ident) => {
        fn $name(&mut self) {
            let res = self.add16(self.hl(), self.$rr());
            self.set_hl(res);
        }
    };
}

#[derive(Default, Debug)]
pub struct Cpu {
    /// Program Counter/Pointer
    pc: u16,

    /// Stack pointer
    sp: u16,

    /// Accumulator
    a: u8,

    b: u8,
    c: u8,
    d: u8,
    e: u8,

    /// Flags
    f: u8,

    h: u8,
    l: u8,

    // TODO(alexyer): is it needed or just check mem(INT_ENABLE_ADDRESS)???
    interrupts_enabled: bool,
}

impl Display for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("PC: 0x{:x} SP: 0x{:x}\n", self.pc, self.sp))
            .unwrap();
        f.write_str(&format!(
            "A: 0x{:x} z: {} n: {} h: {} c: {}\n",
            self.a,
            self.z(),
            self.n(),
            self.h(),
            self.c()
        ))
        .unwrap();
        f.write_str(&format!("B: 0x{:x} C: 0x{:x}\n", self.b, self.c))
            .unwrap();
        f.write_str(&format!("D: 0x{:x} E: 0x{:x}\n", self.d, self.e))
            .unwrap();
        f.write_str(&format!("H: 0x{:x} L: 0x{:x}", self.h, self.l))
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
        self.pc = interrupt_vector;
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
                    Opcode::LdBCD16 => self.ld_bc_d16(&prepare_data!(instruction, 2)),
                    Opcode::LdDED16 => self.ld_de_d16(&prepare_data!(instruction, 2)),
                    Opcode::LdSPD16 => self.ld_sp_d16(&prepare_data!(instruction, 2)),
                    Opcode::LdAD8 => self.ld_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdAA16 => self.ld_a_a16(&prepare_data!(instruction, 2), &mmu),
                    Opcode::LdBD8 => self.ld_b_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdCD8 => self.ld_c_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdDD8 => self.ld_d_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdED8 => self.ld_e_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdHD8 => self.ld_h_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdLD8 => self.ld_l_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdHLSPE8 => self.ld_hl_sp_e8(&prepare_data!(instruction, 1)),
                    Opcode::AddSPE8 => self.add_sp_e8(&prepare_data!(instruction, 1)),
                    Opcode::LdAB => self.ld_a_b(),
                    Opcode::LdAC => self.ld_a_c(),
                    Opcode::LdAD => self.ld_a_d(),
                    Opcode::LdAE => self.ld_a_e(),
                    Opcode::LdAH => self.ld_a_h(),
                    Opcode::LdAL => self.ld_a_l(),
                    Opcode::LdBA => self.ld_b_a(),
                    Opcode::LdBB => self.ld_b_b(),
                    Opcode::LdBC => self.ld_b_c(),
                    Opcode::LdBD => self.ld_b_d(),
                    Opcode::LdBE => self.ld_b_e(),
                    Opcode::LdBH => self.ld_b_h(),
                    Opcode::LdBL => self.ld_b_l(),
                    Opcode::LdCA => self.ld_c_a(),
                    Opcode::LdCB => self.ld_c_b(),
                    Opcode::LdDA => self.ld_d_a(),
                    Opcode::LdDL => self.ld_d_l(),
                    Opcode::LdEA => self.ld_e_a(),
                    Opcode::LdEL => self.ld_e_l(),
                    Opcode::LdHA => self.ld_h_a(),
                    Opcode::LdHD => self.ld_h_d(),
                    Opcode::LdLA => self.ld_l_a(),
                    Opcode::LdLE => self.ld_l_e(),
                    Opcode::PushAF => self.push_af(mmu),
                    Opcode::PushBC => self.push_bc(mmu),
                    Opcode::PushDE => self.push_de(mmu),
                    Opcode::PushHL => self.push_hl(mmu),
                    Opcode::PopAF => self.pop_af(mmu),
                    Opcode::PopBC => self.pop_bc(mmu),
                    Opcode::PopDE => self.pop_de(mmu),
                    Opcode::PopHL => self.pop_hl(mmu),
                    Opcode::LdhAA8 => self.ldh_a_a8(&prepare_data!(instruction, 1), mmu),
                    Opcode::LdhA8A => self.ldh_a8_a(&prepare_data!(instruction, 1), mmu),
                    Opcode::LdAIndHLInc => self.ld_a_ind_hl_inc(mmu),
                    Opcode::IncA => self.inc_a(),
                    Opcode::IncB => self.inc_b(),
                    Opcode::IncC => self.inc_c(),
                    Opcode::IncD => self.inc_d(),
                    Opcode::IncE => self.inc_e(),
                    Opcode::IncBC => self.inc_bc(),
                    Opcode::IncDE => self.inc_de(),
                    Opcode::IncHL => self.inc_hl(),
                    Opcode::IncSP => self.inc_sp(),
                    Opcode::IncH => self.inc_h(),
                    Opcode::IncL => self.inc_l(),
                    Opcode::Daa => self.daa(),
                    Opcode::DecA => self.dec_a(),
                    Opcode::DecB => self.dec_b(),
                    Opcode::DecC => self.dec_c(),
                    Opcode::DecD => self.dec_d(),
                    Opcode::DecE => self.dec_e(),
                    Opcode::DecH => self.dec_h(),
                    Opcode::DecL => self.dec_l(),
                    Opcode::DecBC => self.dec_bc(),
                    Opcode::DecDE => self.dec_de(),
                    Opcode::DecHL => self.dec_hl(),
                    Opcode::DecSP => self.dec_sp(),
                    Opcode::DecIndHL => self.dec_ind_hl(mmu),
                    Opcode::OrA => self.or_a(),
                    Opcode::OrAIndHL => self.or_a_ind_hl(mmu),
                    Opcode::OrB => self.or_b(),
                    Opcode::OrC => self.or_c(),
                    Opcode::OrD8 => self.or_d8(&prepare_data!(instruction, 1)),
                    Opcode::XorA => self.xor_a(),
                    Opcode::XorAC => self.xor_a_c(),
                    Opcode::XorAL => self.xor_a_l(),
                    Opcode::XorAD8 => self.xor_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::XorAIndHL => self.xor_a_ind_hl(mmu),
                    Opcode::AndAD8 => self.and_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::AdcAD8 => self.adc_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::AddAB => self.add_a_b(),
                    Opcode::AddAIndHL => self.add_a_ind_hl(mmu),
                    Opcode::AddAD8 => self.add_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::AddHLBC => self.add_hl_bc(),
                    Opcode::AddHLDE => self.add_hl_de(),
                    Opcode::AddHLHL => self.add_hl_hl(),
                    Opcode::AddHLSP => self.add_hl_sp(),
                    Opcode::SbcD8 => self.sbc_d8(&prepare_data!(instruction, 1)),
                    Opcode::SubD8 => self.sub_d8(&prepare_data!(instruction, 1)),
                    Opcode::SubAB => self.sub_a_b(),
                    Opcode::CpAE => self.cp_a_e(),
                    Opcode::CpD8 => self.cp_d8(&prepare_data!(instruction, 1)),
                    Opcode::CpAIndHL => self.cp_a_ind_hl(mmu),
                    Opcode::LdAIndC => self.ld_a_ind_c(mmu),
                    Opcode::LdAIndDE => self.ld_a_ind_de(mmu),
                    Opcode::LdAIndHL => self.ld_a_ind_hl(mmu),
                    Opcode::LdBIndHL => self.ld_b_ind_hl(mmu),
                    Opcode::LdCIndHL => self.ld_c_ind_hl(mmu),
                    Opcode::LdDIndHL => self.ld_d_ind_hl(mmu),
                    Opcode::LdEIndHL => self.ld_e_ind_hl(mmu),
                    Opcode::LdHIndHL => self.ld_h_ind_hl(mmu),
                    Opcode::LdLIndHL => self.ld_l_ind_hl(mmu),
                    Opcode::LdHLD16 => self.ld_hl_d16(&prepare_data!(instruction, 2)),
                    Opcode::LdIndDEA => self.ld_ind_de_a(mmu),
                    Opcode::LdIndHLA => self.ld_ind_hl_a(mmu),
                    Opcode::LdIndHLB => self.ld_ind_hl_b(mmu),
                    Opcode::LdIndHLC => self.ld_ind_hl_c(mmu),
                    Opcode::LdIndHLD => self.ld_ind_hl_d(mmu),
                    Opcode::LdIndHLE => self.ld_ind_hl_e(mmu),
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
                    Opcode::RlA => self.rl_a(),
                    Opcode::RrA => self.rr_a(),
                    Opcode::Cpl => self.cpl(),
                    Opcode::Scf => self.scf(),
                    Opcode::LdA16A => self.ld_a16_a(&prepare_data!(instruction, 2), mmu),
                    Opcode::LdA16SP => self.ld_a16_sp(&prepare_data!(instruction, 2), mmu),
                    Opcode::LdSPHL => self.ld_sp_hl(),
                    Opcode::Prefix => match instruction.prefixed_opcode().unwrap() {
                        PrefixedOpcode::Bit0D => self.bit0d(),
                        PrefixedOpcode::Bit7H => self.bit7h(),
                        PrefixedOpcode::RlC => self.rl_c(),
                        PrefixedOpcode::RrC => self.rr_c(),
                        PrefixedOpcode::RrD => self.rr_d(),
                        PrefixedOpcode::RrE => self.rr_e(),
                        PrefixedOpcode::SrlB => self.srl_b(),
                        PrefixedOpcode::SwapA => self.swap_a(),
                    },
                };

                instruction.cycles()
            }
            None => 0,
        }
    }

    fn read_instruction<'a>(&mut self, mmu: &'a mut Mmu) -> Option<Instruction> {
        let opcode = match Opcode::try_from(&mmu.read_byte(self.pc as usize)) {
            Ok(opcode) => opcode,
            Err(InstructionError::UnrecognizedOpcode(opcode)) => {
                println!("{}", self);
                println!("IE: 0b{:b}", mmu.read_byte(INT_ENABLE_ADDRESS));
                panic!("unrecognized opcode: 0x{:x}", opcode);
            }
            _ => unreachable!(),
        };
        self.pc += 1;

        match opcode {
            Opcode::Nop => Some(Instruction::nop()),
            Opcode::LdSPD16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_sp_d16(&data))
            }
            Opcode::LdAD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_a_d8(data))
            }
            Opcode::LdAA16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_a_a16(&data))
            }
            Opcode::LdBD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_b_d8(data))
            }
            Opcode::LdCD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_c_d8(data))
            }
            Opcode::LdDD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_d_d8(data))
            }
            Opcode::LdHD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_h_d8(data))
            }
            Opcode::LdED8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_e_d8(data))
            }
            Opcode::LdLD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_l_d8(data))
            }
            Opcode::LdhAA8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ldh_a_a8(data))
            }
            Opcode::LdhA8A => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ldh_a8_a(data))
            }
            Opcode::LdHLSPE8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_hl_sp_e8(data))
            }
            Opcode::AddSPE8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

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
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::or_d8(data))
            }
            Opcode::XorA => Some(Instruction::xor_a(&[])),
            Opcode::XorAC => Some(Instruction::xor_a_c(&[])),
            Opcode::XorAL => Some(Instruction::xor_a_l(&[])),
            Opcode::XorAD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::xor_a_d8(data))
            }
            Opcode::XorAIndHL => Some(Instruction::xor_a_ind_hl(&[])),
            Opcode::AndAD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::and_a_d8(data))
            }
            Opcode::AdcAD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::adc_a_d8(data))
            }
            Opcode::AddAB => Some(Instruction::add_a_b(&[])),
            Opcode::AddHLBC => Some(Instruction::add_hl_bc(&[])),
            Opcode::AddHLDE => Some(Instruction::add_hl_de(&[])),
            Opcode::AddHLHL => Some(Instruction::add_hl_hl(&[])),
            Opcode::AddHLSP => Some(Instruction::add_hl_sp(&[])),
            Opcode::AddAIndHL => Some(Instruction::add_a_ind_hl(&[])),
            Opcode::AddAD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::add_a_d8(data))
            }
            Opcode::CpAE => Some(Instruction::cp_a_e(&[])),
            Opcode::CpD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::cp_d8(data))
            }
            Opcode::CpAIndHL => Some(Instruction::cp_a_ind_hl(&[])),
            Opcode::SbcD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::sbc_d8(data))
            }
            Opcode::SubD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::sub_d8(data))
            }
            Opcode::SubAB => Some(Instruction::sub_a_b(&[])),
            Opcode::LdHLD16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_hl_d16(&data))
            }
            Opcode::LdBCD16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_bc_d16(&data))
            }
            Opcode::LdDED16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_de_d16(&data))
            }
            Opcode::LdIndDEA => Some(Instruction::ld_ind_de_a(&[])),
            Opcode::LdIndHLA => Some(Instruction::ld_ind_hl_a(&[])),
            Opcode::LdIndHLB => Some(Instruction::ld_ind_hl_b(&[])),
            Opcode::LdIndHLC => Some(Instruction::ld_ind_hl_c(&[])),
            Opcode::LdIndHLD => Some(Instruction::ld_ind_hl_d(&[])),
            Opcode::LdIndHLE => Some(Instruction::ld_ind_hl_e(&[])),
            Opcode::LdIndHLD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

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
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::jr_r8(data))
            }
            Opcode::JrCR8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::jr_c_r8(data))
            }
            Opcode::JrNcR8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::jr_nc_r8(data))
            }
            Opcode::JrNzR8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::jr_nz_r8(data))
            }
            Opcode::JrZR8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::jr_z_r8(data))
            }
            Opcode::JpA16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::jp_a16(&data))
            }
            Opcode::JpNzA16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::jp_nz_a16(&data))
            }
            Opcode::JpHL => Some(Instruction::jp_hl(&[])),
            Opcode::CallA16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::call_a16(&data))
            }
            Opcode::CallNzA16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

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
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_a16_a(&data))
            }
            Opcode::LdA16SP => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_a16_sp(&data))
            }
            Opcode::Prefix => {
                let prefixed_opcode =
                    match PrefixedOpcode::try_from(&mmu.read_byte(self.pc as usize)) {
                        Ok(prefixed_opcode) => prefixed_opcode,
                        Err(InstructionError::UnrecognizedPrefixedOpcode(prefixed_opcode)) => {
                            println!("{}", self);
                            panic!("unrecognized prefixed opcode: 0x{:x}", prefixed_opcode);
                        }
                        _ => unreachable!(),
                    };
                self.pc += 1;

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

    fn bc(&self) -> u16 {
        little_endian!(self.c, self.b)
    }

    fn de(&self) -> u16 {
        little_endian!(self.e, self.d)
    }

    fn hl(&self) -> u16 {
        little_endian!(self.l, self.h)
    }

    fn set_hl(&mut self, val: u16) {
        self.h = ((val & 0xff00) >> 8) as u8;
        self.l = (val & 0x00ff) as u8;
    }

    fn check_z(&mut self, value: u8) {
        if value == 0 {
            self.set_z();
        } else {
            self.clear_z();
        }
    }

    fn call_a16(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        let pc = self.pc.to_le_bytes();

        self.sp -= 1;
        mmu.write_byte(self.sp as usize, pc[1]);

        self.sp -= 1;
        mmu.write_byte(self.sp as usize, pc[0]);

        self.pc = little_endian!(data[0], data[1]);
    }

    fn call_nz_a16(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        if !self.z() {
            self.call_a16(data, mmu);
        }
    }

    fn ret(&mut self, mmu: &mut Mmu) {
        let ll = mmu.read_byte(self.sp as usize);
        self.sp += 1;

        let hh = mmu.read_byte(self.sp as usize);
        self.sp += 1;

        self.pc = little_endian!(ll, hh);
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
        self.a = !self.a;

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
            if self.c() || self.a > 0x99 {
                self.a = self.a.wrapping_add(0x60);
                self.set_c();
            }

            if self.h() || (self.a & 0x0f) > 0x09 {
                self.a = self.a.wrapping_add(0x6);
            }
        } else {
            if self.c() {
                self.a = self.a.wrapping_sub(0x60);
            }

            if self.h() {
                self.a = self.a.wrapping_sub(0x6);
            }
        }

        self.check_z(self.a);
        self.clear_h();
    }

    push_rr!(a, f, push_af);
    push_rr!(b, c, push_bc);
    push_rr!(d, e, push_de);
    push_rr!(h, l, push_hl);

    // Special case. Since lower bits in flags register always zero.
    fn pop_af(&mut self, mmu: &Mmu) {
        self.f = mmu.read_byte(self.sp as usize) & 0b11110000;
        self.sp += 1;

        self.a = mmu.read_byte(self.sp as usize);
        self.sp += 1;
    }

    pop_rr!(b, c, pop_bc);
    pop_rr!(d, e, pop_de);
    pop_rr!(h, l, pop_hl);

    ld_r_a16!(a, ld_a_a16);

    ld_rr_d16!(sp, ld_sp_d16);
    ld_rr_d16!(c, b, ld_bc_d16);
    ld_rr_d16!(e, d, ld_de_d16);
    ld_rr_d16!(l, h, ld_hl_d16);

    ld_r_d8!(a, ld_a_d8);
    ld_r_d8!(b, ld_b_d8);
    ld_r_d8!(c, ld_c_d8);
    ld_r_d8!(d, ld_d_d8);
    ld_r_d8!(e, ld_e_d8);
    ld_r_d8!(h, ld_h_d8);
    ld_r_d8!(l, ld_l_d8);

    ld_r_r!(a, b, ld_a_b);
    ld_r_r!(a, c, ld_a_c);
    ld_r_r!(a, d, ld_a_d);
    ld_r_r!(a, e, ld_a_e);
    ld_r_r!(a, h, ld_a_h);
    ld_r_r!(a, l, ld_a_l);
    ld_r_r!(b, a, ld_b_a);
    ld_r_r!(b, b, ld_b_b);
    ld_r_r!(c, b, ld_b_c);
    ld_r_r!(d, b, ld_b_d);
    ld_r_r!(e, b, ld_b_e);
    ld_r_r!(h, b, ld_b_h);
    ld_r_r!(l, b, ld_b_l);
    ld_r_r!(c, a, ld_c_a);
    ld_r_r!(b, a, ld_c_b);
    ld_r_r!(d, a, ld_d_a);
    ld_r_r!(d, l, ld_d_l);
    ld_r_r!(e, a, ld_e_a);
    ld_r_r!(e, l, ld_e_l);
    ld_r_r!(h, a, ld_h_a);
    ld_r_r!(h, d, ld_h_d);
    ld_r_r!(l, a, ld_l_a);
    ld_r_r!(l, e, ld_l_e);

    fn ldh_a8_a(&self, data: &[u8; 1], mmu: &mut Mmu) {
        let addr = 0xff00 + data[0] as u16;
        mmu.write_byte(addr as usize, self.a)
    }

    fn ldh_a_a8(&mut self, data: &[u8; 1], mmu: &Mmu) {
        let addr = 0xff00 + data[0] as u16;
        self.a = mmu.read_byte(addr as usize);
    }

    fn ld_a16_a(&self, data: &[u8; 2], mmu: &mut Mmu) {
        let addr = u16::from_le_bytes(*data);
        mmu.write_byte(addr as usize, self.a);
    }

    fn ld_a16_sp(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        let addr = u16::from_le_bytes(*data) as usize;
        let bytes = self.sp.to_le_bytes();

        mmu.write_byte(addr, bytes[0]);
        mmu.write_byte(addr + 1, bytes[1]);
    }

    fn ld_hl_sp_e8(&mut self, data: &[u8; 1]) {
        let e8: i8 = unsafe { std::mem::transmute(data[0]) };

        let val = if e8 >= 0 {
            self.sp.wrapping_add(e8 as u16)
        } else {
            self.sp.wrapping_sub(-e8 as u16)
        };

        self.h = ((val & 0xff00) >> 8) as u8;
        self.l = (val & 0x00ff) as u8;

        self.clear_z();
        self.clear_n();
        self.set_h_to(((self.sp ^ e8 as u16 ^ (val & 0xffff)) & 0x10) == 0x10);
        self.set_c_to(((self.sp ^ e8 as u16 ^ (val & 0xffff)) & 0x100) == 0x100);
    }

    fn add_sp_e8(&mut self, data: &[u8; 1]) {
        let e8: i8 = unsafe { std::mem::transmute(data[0]) };

        let val = if e8 >= 0 {
            self.sp.wrapping_add(e8 as u16)
        } else {
            self.sp.wrapping_sub(-e8 as u16)
        };

        self.clear_z();
        self.clear_n();
        self.set_h_to(((self.sp ^ e8 as u16 ^ (val & 0xffff)) & 0x10) == 0x10);
        self.set_c_to(((self.sp ^ e8 as u16 ^ (val & 0xffff)) & 0x100) == 0x100);

        self.sp = val;
    }

    fn ld_sp_hl(&mut self) {
        self.sp = self.hl();
    }

    fn and_a_d8(&mut self, data: &[u8; 1]) {
        self.a &= data[0];
        self.set_z_to(self.a == 0);
        self.set_h();
        self.clear_n();
        self.clear_c();
    }

    or_r!(a, or_a);
    or_r!(b, or_b);
    or_r!(c, or_c);

    fn xor_a(&mut self) {
        self.a = self.xor(self.a, self.a);
    }

    xor_r_r!(a, c, xor_a_c);
    xor_r_r!(a, l, xor_a_l);

    fn xor_a_ind_hl(&mut self, mmu: &Mmu) {
        self.a = self.xor(self.a, mmu.read_byte(self.hl() as usize));
    }

    fn xor_a_d8(&mut self, data: &[u8; 1]) {
        self.a = self.xor(self.a, data[0]);
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
        self.a = self.or(self.a, mmu.read_byte(self.hl() as usize));
    }

    fn or_d8(&mut self, data: &[u8; 1]) {
        self.a = self.or(self.a, data[0]);
    }

    fn or(&mut self, x: u8, y: u8) -> u8 {
        let res = x | y;
        self.set_z_to(res == 0);
        self.clear_c();
        self.clear_h();
        self.clear_n();

        res
    }

    inc_r!(a, inc_a);
    inc_r!(b, inc_b);
    inc_r!(c, inc_c);
    inc_r!(d, inc_d);
    inc_r!(e, inc_e);
    inc_r!(h, inc_h);
    inc_r!(l, inc_l);

    inc_rr!(c, b, inc_bc);
    inc_rr!(e, d, inc_de);
    inc_rr!(l, h, inc_hl);

    fn inc_sp(&mut self) {
        self.sp = self.sp.wrapping_add(1);
    }

    fn add_a_b(&mut self) {
        self.a = self.add8(self.a, self.b);
    }

    fn add_a_ind_hl(&mut self, mmu: &Mmu) {
        self.a = self.add8(self.a, mmu.read_byte(self.hl() as usize));
    }

    fn add_a_d8(&mut self, data: &[u8; 1]) {
        self.a = self.add8(self.a, data[0]);
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

    add_hl_rr!(bc, add_hl_bc);
    add_hl_rr!(de, add_hl_de);
    add_hl_rr!(hl, add_hl_hl);

    fn add_hl_sp(&mut self) {
        let res = self.add16(self.hl(), self.sp);
        self.h = ((res & 0xff00) >> 8) as u8;
        self.l = (res & 0x00ff) as u8;
    }

    fn adc_a_d8(&mut self, data: &[u8; 1]) {
        let mut res;
        let first_c;
        let second_c;

        (res, first_c) = self.a.overflowing_add(self.c().into());
        (res, second_c) = res.overflowing_add(data[0]);

        let carry_u8: u8 = self.c().into();

        self.clear_n();
        self.check_z(res);
        self.set_c_to(first_c || second_c);
        self.set_h_to((self.a & 0xf) + (data[0] & 0xf) + carry_u8 > 0xf);

        self.a = res;
    }

    fn sbc_d8(&mut self, data: &[u8; 1]) {
        let mut res;
        let first_c;
        let second_c;

        (res, first_c) = self.a.overflowing_sub(self.c().into());
        (res, second_c) = res.overflowing_sub(data[0]);

        let carry_u8: u8 = self.c().into();

        let (r, first_h) = (self.a & 0xf).overflowing_sub(data[0] & 0xf);
        let (_, second_h) = r.overflowing_sub(carry_u8);

        self.set_n();
        self.check_z(res);
        self.set_c_to(first_c || second_c);
        self.set_h_to(first_h || second_h);

        self.a = res;
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
        self.a = self.sub8(self.a, data[0]);
    }

    fn sub_a_b(&mut self) {
        self.a = self.sub8(self.a, self.b);
    }

    fn cp(&mut self, x: u8, y: u8) {
        self.sub8(x, y);
    }

    fn cp_a_e(&mut self) {
        self.cp(self.a, self.e);
    }

    fn cp_d8(&mut self, data: &[u8; 1]) {
        self.cp(self.a, data[0]);
    }

    fn cp_a_ind_hl(&mut self, mmu: &mut Mmu) {
        self.cp(self.a, mmu.read_byte(self.hl() as usize));
    }

    dec_r!(a, dec_a);
    dec_r!(b, dec_b);
    dec_r!(c, dec_c);
    dec_r!(d, dec_d);
    dec_r!(e, dec_e);
    dec_r!(h, dec_h);
    dec_r!(l, dec_l);
    dec_rr!(c, b, dec_bc);
    dec_rr!(e, d, dec_de);
    dec_rr!(l, h, dec_hl);

    fn dec_sp(&mut self) {
        self.sp = self.sp.wrapping_sub(1);
    }

    fn dec_ind_hl(&mut self, mmu: &mut Mmu) {
        let val = mmu.read_byte(self.hl() as usize).wrapping_sub(1);

        mmu.write_byte(self.hl() as usize, val);

        self.check_z(val);
        self.set_n();
        self.set_h_to(val & 0xf == 0xf);
    }

    fn ld_a_ind_hl_inc(&mut self, mmu: &mut Mmu) {
        self.a = mmu.read_byte(self.hl() as usize);
        self.inc_hl();
    }

    ld_r_ind_rr!(e, d, a, ld_a_ind_de);
    ld_r_ind_rr!(l, h, a, ld_a_ind_hl);
    ld_r_ind_rr!(l, h, b, ld_b_ind_hl);
    ld_r_ind_rr!(l, h, c, ld_c_ind_hl);
    ld_r_ind_rr!(l, h, d, ld_d_ind_hl);
    ld_r_ind_rr!(l, h, e, ld_e_ind_hl);
    ld_r_ind_rr!(l, h, h, ld_h_ind_hl);
    ld_r_ind_rr!(l, h, l, ld_l_ind_hl);

    ld_r_ind_r!(c, a, ld_a_ind_c);

    ld_ind_rr_r!(l, h, a, ld_ind_hl_a);
    ld_ind_rr_r!(l, h, b, ld_ind_hl_b);
    ld_ind_rr_r!(l, h, c, ld_ind_hl_c);
    ld_ind_rr_r!(l, h, d, ld_ind_hl_d);
    ld_ind_rr_r!(l, h, e, ld_ind_hl_e);
    ld_ind_rr_r!(e, d, a, ld_ind_de_a);

    ld_ind_rr_d8!(l, h, ld_ind_hl_d8);

    fn ld_ind_hl_dec_a(&mut self, mmu: &mut Mmu) {
        self.ld_ind_hl_a(mmu);
        self.dec_hl();
    }

    fn ld_ind_hl_inc_a(&mut self, mmu: &mut Mmu) {
        self.ld_ind_hl_a(mmu);
        self.inc_hl();
    }

    fn ld_ind_c_a(&mut self, mmu: &mut Mmu) {
        mmu.write_byte(0xff00 + self.c as usize, self.a);
    }

    bit!(d, 0, bit0d);
    bit!(h, 7, bit7h);

    rl_r!(a, rl_a);
    rl_r!(c, rl_c);

    rr_r!(a, rr_a);
    rr_r!(c, rr_c);
    rr_r!(d, rr_d);
    rr_r!(e, rr_e);

    fn srl_b(&mut self) {
        let (res, c) = self.b.overflowing_shr(1);
        self.b = res;

        self.clear_n();
        self.clear_h();
        self.check_z(res);
        self.set_c_to(c);
    }

    fn swap_a(&mut self) {
        self.a = (self.a << 4) | (self.a >> 4);

        self.check_z(self.a);
        self.clear_c();
        self.clear_h();
        self.clear_n();
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
        self.pc = self.pc.wrapping_add_signed(offset as i16);
    }

    fn jp_a16(&mut self, data: &[u8; 2]) {
        self.pc = little_endian!(data[0], data[1]);
    }

    fn jp_nz_a16(&mut self, data: &[u8; 2]) {
        if !self.z() {
            self.jp_a16(data);
        }
    }

    fn jp_hl(&mut self) {
        self.pc = self.hl();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_inc_r {
        ($r:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$r = 41;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.$r, 42);
                assert!(!cpu.h());
            }
        };
    }

    macro_rules! test_dec_r {
        ($r:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$r = 43;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.$r, 42);
                assert!(!cpu.h());
                assert!(cpu.n());
            }
        };
    }

    macro_rules! test_dec_rr {
        ($ll:ident, $hh:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$ll = 43;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.$ll, 42);
            }
        };
    }

    macro_rules! test_ld_r_r {
        ($dst:ident, $src:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$src = 42;
                cpu.$dst = 0;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.$dst, 42);
            }
        };
    }

    macro_rules! test_ld_r_ind_rr {
        ($ll:ident, $hh:ident, $r:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$r = 0;
                cpu.$hh = 0;
                cpu.$ll = 1;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.$r, 0xff);
            }
        };
    }

    macro_rules! test_push_rr {
        ($hh: ident, $ll:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);
                cpu.$hh = 0xfe;
                cpu.$ll = 0xff;
                cpu.sp = 10;

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 16);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.sp, 8);
                assert_eq!(mmu.read_byte(9), 0xfe);
                assert_eq!(mmu.read_byte(8), 0xff);
            }
        };
    }

    macro_rules! test_pop_rr {
        ($hh: ident, $ll:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);
                cpu.sp = 1;

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.sp, 3);
                assert_eq!(cpu.$hh, 0xff);
                assert_eq!(cpu.$ll, 0xfe);
            }
        };
    }

    macro_rules! test_ld_rr_d16 {
        ($ll:ident, $hh:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.pc, 3);
                assert_eq!(cpu.$hh, 0xff);
                assert_eq!(cpu.$ll, 0xfe);
            }
        };
    }

    macro_rules! test_inc_rr {
        ($ll:ident, $hh:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$ll = 41;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.$ll, 42);
            }
        };
    }

    macro_rules! test_or_r {
        ($r:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$r = 0x0f;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.a, 0x0f);
                assert!(!cpu.z());

                cpu.pc = 0;
                cpu.a = 0;
                cpu.$r = 0;

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.a, 0);
                assert!(cpu.z());
            }
        };
    }

    macro_rules! test_ld_r_a16 {
        ($r:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);
                mmu.write_byte(0x0005, 42);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 16);
                assert_eq!(cpu.pc, 3);
                assert_eq!(cpu.$r, 42);
            }
        };
    }

    macro_rules! test_ld_r_d8 {
        ($r: ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.pc, 2);
                assert_eq!(cpu.$r, 0xff);
            }
        };
    }

    macro_rules! test_ld_ind_rr_r {
        ($r:ident, $ll:ident, $hh:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$r = 42;
                cpu.$hh = 0;
                cpu.$ll = 5;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.pc, 1);
                assert_eq!(mmu.read_byte(5), 42);
            }
        };
    }

    macro_rules! test_ld_ind_rr_d8 {
        ($ll:ident, $hh:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$hh = 0;
                cpu.$ll = 5;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 12);
                assert_eq!(cpu.pc, 2);
                assert_eq!(mmu.read_byte(5), 0xde);
            }
        };
    }

    macro_rules! test_ld_r_ind_r {
        ($dst:ident, $r:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$r = 0;
                cpu.$dst = 5;
                mmu.write_slice($mnemonic, 0);
                mmu.write_byte(0xff05, 0xaa);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.$r, 0xaa);
            }
        };
    }

    macro_rules! test_rr_r {
        ($r:ident, $mnemonic:expr, $cycles:expr, $pc:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$r = 2;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, $cycles);
                assert_eq!(cpu.pc, $pc);
                assert!(!cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
                assert_eq!(cpu.$r, 1);
            }
        };
    }

    macro_rules! test_xor_r_r {
        ($x: ident, $y: ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$x = 42;
                cpu.$y = 42;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 4);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.$x, 0);
                assert!(cpu.z());
                assert!(!cpu.n());
                assert!(!cpu.h());
                assert!(!cpu.c());
            }
        };
    }

    macro_rules! test_add_hl_rr {
        ($ll:ident, $hh:ident, $mnemonic:expr, $name:ident) => {
            #[test]
            fn $name() {
                let mut cpu = Cpu::default();
                let mut mmu = Mmu::default();
                cpu.$hh = 0;
                cpu.$ll = 0xad;
                cpu.h = 0xde;
                cpu.l = 0;
                mmu.write_slice($mnemonic, 0);

                let cycles = cpu.exec_instruction(&mut mmu);

                assert_eq!(cycles, 8);
                assert_eq!(cpu.pc, 1);
                assert_eq!(cpu.hl(), 0xdead);
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
        assert_eq!(cpu.pc, 1);
    }

    #[test]
    fn test_di() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xf3], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert!(!cpu.interrupts_enabled);
    }

    #[test]
    fn test_ei() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xfb], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert!(cpu.interrupts_enabled);
    }

    test_push_rr!(a, f, &[0xf5], test_push_af);
    test_push_rr!(b, c, &[0xc5], test_push_bc);
    test_push_rr!(d, e, &[0xd5], test_push_de);
    test_push_rr!(h, l, &[0xe5], test_push_hl);

    #[test]
    fn test_call_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xcd, 0x05, 0x00], 0);
        cpu.sp = 10;

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 24);
        assert_eq!(cpu.pc, 5);
        assert_eq!(cpu.sp, 8);
        assert_eq!(mmu.read_byte(9), 0x00);
        assert_eq!(mmu.read_byte(8), 0x03);
    }

    #[test]
    fn test_call_nz_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc4, 0x05, 0x00], 0);
        cpu.sp = 10;
        cpu.clear_z();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 24);
        assert_eq!(cpu.pc, 5);
        assert_eq!(cpu.sp, 8);
        assert_eq!(mmu.read_byte(9), 0x00);
        assert_eq!(mmu.read_byte(8), 0x03);
    }

    #[test]
    fn test_ret() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc9, 0x05, 0x00], 0);
        cpu.sp = 1;

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.pc, 5);
        assert_eq!(cpu.sp, 3);
    }

    #[test]
    fn test_ret_c() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xd8, 0x05, 0x00], 0);
        cpu.sp = 1;
        cpu.set_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 20);
        assert_eq!(cpu.pc, 5);
        assert_eq!(cpu.sp, 3);
    }

    #[test]
    fn test_ret_nc() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xd0, 0x05, 0x00], 0);
        cpu.sp = 1;
        cpu.clear_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 20);
        assert_eq!(cpu.pc, 5);
        assert_eq!(cpu.sp, 3);
    }

    #[test]
    fn test_ret_z() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc8, 0x05, 0x00], 0);
        cpu.sp = 1;
        cpu.set_z();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 20);
        assert_eq!(cpu.pc, 5);
        assert_eq!(cpu.sp, 3);
    }

    #[test]
    fn test_pop_af() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xf1, 0xfe, 0xff], 0);
        cpu.sp = 1;

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.sp, 3);
        assert_eq!(cpu.a, 0xff);
        assert_eq!(cpu.f, 0xf0);
    }
    test_pop_rr!(b, c, &[0xc1, 0xfe, 0xff], test_pop_bc);
    test_pop_rr!(d, e, &[0xd1, 0xfe, 0xff], test_pop_de);
    test_pop_rr!(h, l, &[0xe1, 0xfe, 0xff], test_pop_hl);

    #[test]
    fn test_ld_sp_d16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x31, 0xfe, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 3);
        assert_eq!(cpu.sp, 0xfffe);
    }

    #[test]
    fn test_ld_hl_sp_e8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.sp = 0xdead;
        mmu.write_slice(&[0xf8, 0xfe], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.hl(), 0xdeab);
    }

    #[test]
    fn test_add_sp_e8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.sp = 0xdead;
        mmu.write_slice(&[0xe8, 0xfe], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.sp, 0xdeab);
    }

    test_ld_rr_d16!(c, b, &[0x01, 0xfe, 0xff], test_ld_bc_d16);
    test_ld_rr_d16!(e, d, &[0x11, 0xfe, 0xff], test_ld_de_d16);
    test_ld_rr_d16!(l, h, &[0x21, 0xfe, 0xff], test_ld_hl_d16);

    #[test]
    fn test_ld_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x3e, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 0xff);
    }

    #[test]
    fn test_ld_b_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x06, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.b, 0xff);
    }

    test_ld_r_r!(a, b, &[0x78], test_ld_a_b);
    test_ld_r_r!(a, c, &[0x79], test_ld_a_c);
    test_ld_r_r!(a, d, &[0x7a], test_ld_a_d);
    test_ld_r_r!(a, e, &[0x7b], test_ld_a_e);
    test_ld_r_r!(a, h, &[0x7c], test_ld_a_h);
    test_ld_r_r!(a, l, &[0x7d], test_ld_a_l);
    test_ld_r_r!(b, a, &[0x47], test_ld_b_a);
    test_ld_r_r!(c, b, &[0x41], test_ld_b_c);
    test_ld_r_r!(d, b, &[0x42], test_ld_b_d);
    test_ld_r_r!(e, b, &[0x43], test_ld_b_e);
    test_ld_r_r!(h, b, &[0x44], test_ld_b_h);
    test_ld_r_r!(l, b, &[0x45], test_ld_b_l);
    test_ld_r_r!(c, a, &[0x4f], test_ld_c_a);
    test_ld_r_r!(c, b, &[0x48], test_ld_c_b);
    test_ld_r_r!(d, a, &[0x57], test_ld_d_a);
    test_ld_r_r!(d, l, &[0x55], test_ld_d_l);
    test_ld_r_r!(e, a, &[0x5f], test_ld_e_a);
    test_ld_r_r!(e, l, &[0x5d], test_ld_e_l);
    test_ld_r_r!(h, a, &[0x67], test_ld_h_a);
    test_ld_r_r!(h, d, &[0x62], test_ld_h_d);
    test_ld_r_r!(l, a, &[0x6f], test_ld_l_a);
    test_ld_r_r!(l, e, &[0x6b], test_ld_l_e);

    test_ld_r_d8!(c, &[0x0e, 0xff], test_ld_c_d8);
    test_ld_r_d8!(d, &[0x16, 0xff], test_ld_d_d8);
    test_ld_r_d8!(e, &[0x1e, 0xff], test_ld_e_d8);
    test_ld_r_d8!(h, &[0x26, 0xff], test_ld_h_d8);
    test_ld_r_d8!(l, &[0x2e, 0xff], test_ld_l_d8);

    #[test]
    fn test_ld_a16_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        mmu.write_slice(&[0xea, 0x05, 0x00], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.pc, 3);
        assert_eq!(mmu.read_byte(0x0005), 42);
    }

    #[test]
    fn test_ld_a16_sp() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.sp = 0xdead;
        mmu.write_slice(&[0x08, 0x05, 0x00], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 20);
        assert_eq!(cpu.pc, 3);
        assert_eq!(mmu.read_byte(0x0005), 0xad);
        assert_eq!(mmu.read_byte(0x0006), 0xde);
    }

    #[test]
    fn test_ld_sp_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.h = 0xde;
        cpu.l = 0xad;

        mmu.write_slice(&[0xf9], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.sp, 0xdead);
    }

    #[test]
    fn test_ldh_a8_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        mmu.write_slice(&[0xe0, 0x00], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert_eq!(mmu.read_byte(0xff00), 42);
    }

    test_ld_r_a16!(a, &[0xfa, 0x05, 0x00], test_ld_a_a16);

    #[test]
    fn test_ldh_a_a8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 0;
        mmu.write_slice(&[0xf0, 0x00], 0);
        mmu.write_byte(0xff00, 42);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 42);
    }

    test_or_r!(a, &[0xb7], test_or_a);
    test_or_r!(b, &[0xb0], test_or_b);
    test_or_r!(c, &[0xb1], test_or_c);

    #[test]
    fn test_or_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 0xaa;
        mmu.write_slice(&[0xf6, 0x55], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 0xff);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
    }

    #[test]
    fn test_or_a_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        cpu.h = 0;
        cpu.l = 1;
        mmu.write_slice(&[0xb6, 0x5], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 15);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
    }

    #[test]
    fn test_xor_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        mmu.write_slice(&[0xaf], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 0);
        assert!(cpu.z());
    }

    test_xor_r_r!(a, c, &[0xa9], test_xor_a_c);
    test_xor_r_r!(a, l, &[0xad], test_xor_a_l);

    #[test]
    fn test_xor_a_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        cpu.h = 0;
        cpu.l = 1;
        mmu.write_slice(&[0xae, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 0);
        assert!(cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
    }

    #[test]
    fn test_xor_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        mmu.write_slice(&[0xee, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 0);
        assert!(cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
    }

    #[test]
    fn test_add_a_b() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        cpu.b = 10;
        mmu.write_slice(&[0x80], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 20);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    test_add_hl_rr!(c, b, &[0x09], test_add_hl_bc);
    test_add_hl_rr!(e, d, &[0x19], test_add_hl_de);

    #[test]
    fn test_add_hl_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.h = 0;
        cpu.l = 0xff;
        mmu.write_slice(&[0x29], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.hl(), 0x1fe);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_add_hl_sp() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.h = 0xde;
        cpu.l = 0xab;
        cpu.sp = 2;
        mmu.write_slice(&[0x39], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.hl(), 0xdead);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_add_a_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        cpu.h = 0;
        cpu.l = 1;

        mmu.write_slice(&[0x86, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 20);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_add_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        mmu.write_slice(&[0xc6, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 20);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_adc_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        cpu.set_c();
        mmu.write_slice(&[0xce, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 21);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_and_a_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 0xf0;
        mmu.write_slice(&[0xe6, 0x0f], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 0);
        assert!(cpu.z());
        assert!(cpu.h());

        cpu.a = 0x0f;
        cpu.pc = 0;

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 0x0f);
        assert!(!cpu.z());
        assert!(cpu.h());
    }

    #[test]
    fn test_sub_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        mmu.write_slice(&[0xd6, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 0);
        assert!(cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_sbc_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 20;
        cpu.set_c();
        mmu.write_slice(&[0xde, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 9);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_sub_a_b() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 11;
        cpu.b = 10;
        mmu.write_slice(&[0x90], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 1);
        assert!(!cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_cp_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        mmu.write_slice(&[0xfe, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.a, 10);
        assert!(cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_cp_a_e() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        cpu.e = 10;
        mmu.write_slice(&[0xbb], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 10);
        assert!(cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    #[test]
    fn test_cp_a_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 10;
        cpu.h = 0;
        cpu.l = 1;
        mmu.write_slice(&[0xbe, 0xa], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 10);
        assert!(cpu.z());
        assert!(!cpu.c());
        assert!(cpu.n());
        assert!(!cpu.h());
    }

    test_inc_r!(a, &[0x3c], test_inc_a);
    test_inc_r!(b, &[0x04], test_inc_b);
    test_inc_r!(c, &[0x0c], test_inc_c);
    test_inc_r!(e, &[0x1c], test_inc_e);
    test_inc_r!(h, &[0x24], test_inc_h);
    test_inc_r!(l, &[0x2c], test_inc_l);

    test_inc_rr!(c, b, &[0x03], test_inc_bc);
    test_inc_rr!(e, d, &[0x13], test_inc_de);
    test_inc_rr!(l, h, &[0x23], test_inc_hl);

    #[test]
    fn test_inc_sp() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.sp = 0xdeac;
        mmu.write_slice(&[0x33], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.sp, 0xdead);
    }

    test_dec_r!(a, &[0x3d], test_dec_a);
    test_dec_r!(b, &[0x05], test_dec_b);
    test_dec_r!(c, &[0x0d], test_dec_c);
    test_dec_r!(d, &[0x15], test_dec_d);
    test_dec_r!(e, &[0x1d], test_dec_e);
    test_dec_r!(h, &[0x25], test_dec_h);
    test_dec_r!(l, &[0x2d], test_dec_l);

    test_dec_rr!(c, b, &[0x0b], test_dec_bc);
    test_dec_rr!(e, d, &[0x1b], test_dec_de);
    test_dec_rr!(l, h, &[0x2b], test_dec_hl);

    #[test]
    fn test_dec_sp() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.sp = 0xdeae;
        mmu.write_slice(&[0x3b], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.sp, 0xdead);
    }

    #[test]
    fn test_dec_ind_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.h = 0;
        cpu.l = 1;
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
        cpu.a = 42;
        cpu.l = 5;
        mmu.write_slice(&[0x32], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.l, 4);
        assert_eq!(mmu.read_byte(5), 42);
    }

    #[test]
    fn test_ld_ind_hl_inc_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        cpu.l = 5;
        mmu.write_slice(&[0x22], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.l, 6);
        assert_eq!(mmu.read_byte(5), 42);
    }

    test_ld_ind_rr_r!(a, e, d, &[0x12], test_ld_ind_de_a);
    test_ld_ind_rr_r!(a, l, h, &[0x32], test_ld_ind_hl_a);
    test_ld_ind_rr_r!(b, l, h, &[0x70], test_ld_ind_hl_b);
    test_ld_ind_rr_r!(c, l, h, &[0x71], test_ld_ind_hl_c);
    test_ld_ind_rr_r!(d, l, h, &[0x72], test_ld_ind_hl_d);
    test_ld_ind_rr_r!(e, l, h, &[0x73], test_ld_ind_hl_e);

    test_ld_ind_rr_d8!(l, h, &[0x36, 0xde], test_ld_ind_hl_d8);

    #[test]
    fn test_ld_ind_c_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        cpu.c = 5;
        mmu.write_slice(&[0xe2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(mmu.read_byte(0xff05), 42);
    }

    test_ld_r_ind_rr!(e, d, a, &[0x1a, 0xff], test_ld_a_ind_de);
    test_ld_r_ind_rr!(l, h, a, &[0x7e, 0xff], test_ld_a_ind_hl);
    test_ld_r_ind_rr!(l, h, b, &[0x46, 0xff], test_ld_b_ind_hl);
    test_ld_r_ind_rr!(l, h, c, &[0x4e, 0xff], test_ld_c_ind_hl);
    test_ld_r_ind_rr!(l, h, d, &[0x56, 0xff], test_ld_d_ind_hl);
    test_ld_r_ind_rr!(l, h, e, &[0x5e, 0xff], test_ld_e_ind_hl);
    test_ld_r_ind_rr!(l, h, h, &[0x66, 0xff], test_ld_h_ind_hl);
    test_ld_r_ind_rr!(l, h, l, &[0x6e, 0xff], test_ld_l_ind_hl);

    test_ld_r_ind_r!(c, a, &[0xf2], test_ld_a_ind_c);

    #[test]
    fn test_ld_a_ind_hl_inc() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 0;
        cpu.h = 0;
        cpu.l = 1;
        mmu.write_slice(&[0x2a, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 0xff);
        assert_eq!(cpu.l, 2);
    }

    #[test]
    fn test_bit7h() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.h = 0b11111111;
        cpu.l = 0b11111111;
        mmu.write_slice(&[0xcb, 0x7c], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(cpu.h());

        cpu.pc = 0;
        cpu.h = 0b01111111;
        cpu.l = 0b11111111;
        mmu.write_slice(&[0xcb, 0x7c], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert!(cpu.z());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_bit0d() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.d = 0b11111111;
        mmu.write_slice(&[0xcb, 0x42], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(cpu.h());

        cpu.pc = 0;
        cpu.d = 0b11111110;
        mmu.write_slice(&[0xcb, 0x42], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert!(cpu.z());
        assert!(!cpu.n());
        assert!(cpu.h());
    }

    #[test]
    fn test_rl_c() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.c = 1;
        mmu.write_slice(&[0xcb, 0x11], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.c, 2);
    }

    test_rr_r!(a, &[0x1f], 4, 1, test_rr_a);
    test_rr_r!(c, &[0xcb, 0x19], 12, 2, test_rr_c);
    test_rr_r!(d, &[0xcb, 0x1a], 12, 2, test_rr_d);
    test_rr_r!(e, &[0xcb, 0x1b], 12, 2, test_rr_e);

    #[test]
    fn test_srl_b() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.b = 2;
        mmu.write_slice(&[0xcb, 0x38], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.b, 1);
    }

    #[test]
    fn test_swap_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 0xab;
        mmu.write_slice(&[0xcb, 0x37], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.a, 0xba);
    }

    #[test]
    fn test_rl_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 1;
        mmu.write_slice(&[0x17], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.a, 2);
    }

    #[test]
    fn test_jr_nz_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x20, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 4);

        cpu.pc = 0;
        cpu.set_z();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
    }

    #[test]
    fn test_jr_nc_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x30, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 4);

        cpu.pc = 0;
        cpu.set_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
    }

    #[test]
    fn test_jr_z_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x28, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);

        cpu.pc = 0;
        cpu.set_z();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 4);
    }

    #[test]
    fn test_jr_c_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x38, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);

        cpu.pc = 0;
        cpu.set_c();

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 4);
    }

    #[test]
    fn test_jr_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x18, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 4);
    }

    #[test]
    fn test_jp_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc3, 0x05, 0x0a], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.pc, 0x0a05);
    }

    #[test]
    fn test_jp_nz_a16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.clear_z();
        mmu.write_slice(&[0xc2, 0x05, 0x0a], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.pc, 0x0a05);
    }

    #[test]
    fn test_jp_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.h = 0xa;
        cpu.l = 0x5;
        mmu.write_slice(&[0xe9], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x0a05);
    }

    #[test]
    fn test_cpl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 0xaa;

        mmu.write_slice(&[0x2f], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.a, 0x55);
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
        cpu.sp = 0xff;

        for _ in 0..9 {
            cpu.exec_instruction(&mut mmu);
        }

        assert_ne!(cpu.pc, 0xdead);
        assert_eq!(cpu.a, cpu.e);
    }
}
