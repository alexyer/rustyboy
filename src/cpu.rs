use std::fmt::Display;

use crate::{
    errors::InstructionError,
    instruction::{Instruction, Opcode, PrefixedOpcode},
    mmu::{Mmu, INT_ENABLE_ADDRESS},
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

macro_rules! push_r {
    ($hh:ident, $ll:ident, $name:ident) => {
        fn $name(&mut self, mmu: &mut Mmu) {
            self.sp -= 1;

            mmu.write_byte(self.sp as usize, self.$hh);
            self.sp -= 1;

            mmu.write_byte(self.sp as usize, self.$ll);
        }
    };
}

macro_rules! pop_r {
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
            self.check_h(self.$r);
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
            self.check_h(self.$r);
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

    fn handle_interrupts(&mut self, _mmu: &mut Mmu) {
        if self.interrupts_enabled {
            todo!()
        }
    }

    /// Execute the next instruction and return the number of T-states.
    fn exec_instruction(&mut self, mmu: &mut Mmu) -> usize {
        match self.read_instruction(mmu) {
            Some(instruction) => {
                match instruction.opcode() {
                    Opcode::Nop => (),
                    Opcode::LdDeD16 => self.ld_de_d16(&prepare_data!(instruction, 2)),
                    Opcode::LdSpD16 => self.ld_sp_d16(&prepare_data!(instruction, 2)),
                    Opcode::LdAD8 => self.ld_a_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdBD8 => self.ld_b_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdCD8 => self.ld_c_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdDD8 => self.ld_d_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdED8 => self.ld_e_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdLD8 => self.ld_l_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdAE => self.ld_a_e(),
                    Opcode::LdAL => self.ld_a_l(),
                    Opcode::LdBA => self.ld_b_a(),
                    Opcode::LdCA => self.ld_c_a(),
                    Opcode::LdDA => self.ld_d_a(),
                    Opcode::LdDL => self.ld_d_l(),
                    Opcode::LdHA => self.ld_h_a(),
                    Opcode::PushAF => self.push_af(mmu),
                    Opcode::PushBC => self.push_bc(mmu),
                    Opcode::PopAF => self.pop_af(mmu),
                    Opcode::PopBC => self.pop_bc(mmu),
                    Opcode::LdhAA8 => self.ldh_a_a8(&prepare_data!(instruction, 1), mmu),
                    Opcode::LdhA8A => self.ldh_a8_a(&prepare_data!(instruction, 1), mmu),
                    Opcode::LdAIndHLInc => self.ld_a_ind_hl_inc(mmu),
                    Opcode::IncB => self.inc_b(),
                    Opcode::IncC => self.inc_c(),
                    Opcode::IncDE => self.inc_de(),
                    Opcode::IncHL => self.inc_hl(),
                    Opcode::DecA => self.dec_a(),
                    Opcode::DecB => self.dec_b(),
                    Opcode::DecC => self.dec_c(),
                    Opcode::DecD => self.dec_d(),
                    Opcode::DecE => self.dec_e(),
                    Opcode::DecDE => self.dec_de(),
                    Opcode::XorA => self.xor_a(),
                    Opcode::AddAB => self.add_a_b(),
                    Opcode::SubD8 => self.sub_d8(&prepare_data!(instruction, 1)),
                    Opcode::CpD8 => self.cp_d8(&prepare_data!(instruction, 1)),
                    Opcode::LdAIndDE => self.ld_a_ind_de(mmu),
                    Opcode::LdHlD16 => self.ld_hl_d16(&prepare_data!(instruction, 2)),
                    Opcode::LdIndHLA => self.ld_ind_hl_a(mmu),
                    Opcode::LdIndHLDecA => self.ld_ind_hl_dec_a(mmu),
                    Opcode::LdIndHLIncA => self.ld_ind_hl_inc_a(mmu),
                    Opcode::LdIndCA => self.ld_ind_c_a(mmu),
                    Opcode::JrR8 => self.jr_r8(&prepare_data!(instruction, 1)),
                    Opcode::JrNzR8 => self.jr_nz_r8(&prepare_data!(instruction, 1)),
                    Opcode::JrZR8 => self.jr_z_r8(&prepare_data!(instruction, 1)),
                    Opcode::CallA16 => self.call_a16(&prepare_data!(instruction, 2), mmu),
                    Opcode::Ret => self.ret(mmu),
                    Opcode::RlA => self.rl_a(),
                    Opcode::LdA16A => self.ld_a16_a(&prepare_data!(instruction, 2), mmu),
                    Opcode::Prefix => match instruction.prefixed_opcode().unwrap() {
                        PrefixedOpcode::Bit0D => self.bit0d(),
                        PrefixedOpcode::Bit7H => self.bit7h(),
                        PrefixedOpcode::RlC => self.rl_c(),
                        PrefixedOpcode::RrD => self.rr_d(),
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
            Opcode::LdSpD16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_sp_d16(&data))
            }
            Opcode::LdAD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::ld_a_d8(data))
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
            Opcode::LdAE => Some(Instruction::ld_a_e(&[])),
            Opcode::LdAL => Some(Instruction::ld_a_l(&[])),
            Opcode::LdBA => Some(Instruction::ld_b_a(&[])),
            Opcode::LdCA => Some(Instruction::ld_c_a(&[])),
            Opcode::LdDA => Some(Instruction::ld_d_a(&[])),
            Opcode::LdDL => Some(Instruction::ld_d_l(&[])),
            Opcode::LdHA => Some(Instruction::ld_h_a(&[])),
            Opcode::PushAF => Some(Instruction::push_af(&[])),
            Opcode::PushBC => Some(Instruction::push_bc(&[])),
            Opcode::PopAF => Some(Instruction::pop_af(&[])),
            Opcode::PopBC => Some(Instruction::pop_bc(&[])),
            Opcode::IncB => Some(Instruction::inc_b(&[])),
            Opcode::IncC => Some(Instruction::inc_c(&[])),
            Opcode::IncDE => Some(Instruction::inc_de(&[])),
            Opcode::IncHL => Some(Instruction::inc_hl(&[])),
            Opcode::DecA => Some(Instruction::dec_a(&[])),
            Opcode::DecB => Some(Instruction::dec_b(&[])),
            Opcode::DecC => Some(Instruction::dec_c(&[])),
            Opcode::DecD => Some(Instruction::dec_d(&[])),
            Opcode::DecE => Some(Instruction::dec_e(&[])),
            Opcode::DecDE => Some(Instruction::dec_de(&[])),
            Opcode::XorA => Some(Instruction::xor_a(&[])),
            Opcode::AddAB => Some(Instruction::add_a_b(&[])),
            Opcode::CpD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::cp_d8(data))
            }
            Opcode::SubD8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::sub_d8(data))
            }
            Opcode::LdHlD16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_hl_d16(&data))
            }
            Opcode::LdDeD16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_de_d16(&data))
            }
            Opcode::LdIndHLA => Some(Instruction::ld_ind_hl_a(&[])),
            Opcode::LdIndHLDecA => Some(Instruction::ld_ind_hl_dec_a(&[])),
            Opcode::LdIndHLIncA => Some(Instruction::ld_ind_hl_inc_a(&[])),
            Opcode::LdIndCA => Some(Instruction::ld_ind_c_a(&[])),
            Opcode::LdAIndDE => Some(Instruction::ldh_a_ind_de(&[])),
            Opcode::LdAIndHLInc => Some(Instruction::ld_a_ind_hl_inc(&[])),
            Opcode::JrR8 => {
                let data = &[mmu.read_byte(self.pc as usize)];
                self.pc += 1;

                Some(Instruction::jr_r8(data))
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
            Opcode::CallA16 => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::call_a16(&data))
            }
            Opcode::Ret => Some(Instruction::ret(&[])),
            Opcode::RlA => Some(Instruction::rl_a(&[])),
            Opcode::LdA16A => {
                let data = mmu.read_slice(self.pc as usize, 2);
                self.pc += 2;

                Some(Instruction::ld_a16_a(&data))
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
                    PrefixedOpcode::RrD => Some(Instruction::rr_d(&[])),
                }
            }
        }
    }

    impl_flag!(set_z, clear_z, set_z_to, z, 6);
    impl_flag!(set_n, clear_n, set_n_to, n, 5);
    impl_flag!(set_h, clear_h, set_h_to, h, 4);
    impl_flag!(set_c, clear_c, set_c_to, c, 3);

    fn hl(&self) -> u16 {
        little_endian!(self.l, self.h)
    }

    fn check_z(&mut self, value: u8) {
        if value == 0 {
            self.set_z();
        } else {
            self.clear_z();
        }
    }

    fn check_h(&mut self, value: u8) {
        if value & 0x10 == 0x10 {
            self.set_h()
        } else {
            self.clear_h()
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

    fn ret(&mut self, mmu: &mut Mmu) {
        let ll = mmu.read_byte(self.sp as usize);
        self.sp += 1;

        let hh = mmu.read_byte(self.sp as usize);
        self.sp += 1;

        self.pc = little_endian!(ll, hh);
    }

    push_r!(a, f, push_af);
    push_r!(b, c, push_bc);

    pop_r!(a, f, pop_af);
    pop_r!(b, c, pop_bc);

    ld_rr_d16!(sp, ld_sp_d16);
    ld_rr_d16!(e, d, ld_de_d16);
    ld_rr_d16!(l, h, ld_hl_d16);

    ld_r_d8!(a, ld_a_d8);
    ld_r_d8!(b, ld_b_d8);
    ld_r_d8!(c, ld_c_d8);
    ld_r_d8!(d, ld_d_d8);
    ld_r_d8!(e, ld_e_d8);
    ld_r_d8!(l, ld_l_d8);

    ld_r_r!(a, e, ld_a_e);
    ld_r_r!(a, l, ld_a_l);
    ld_r_r!(b, a, ld_b_a);
    ld_r_r!(c, a, ld_c_a);
    ld_r_r!(d, a, ld_d_a);
    ld_r_r!(d, l, ld_d_l);
    ld_r_r!(h, a, ld_h_a);

    fn ldh_a8_a(&mut self, data: &[u8; 1], mmu: &mut Mmu) {
        let addr = 0xff00 + data[0] as u16;
        mmu.write_byte(addr as usize, self.a)
    }

    fn ldh_a_a8(&mut self, data: &[u8; 1], mmu: &Mmu) {
        let addr = 0xff00 + data[0] as u16;
        self.a = mmu.read_byte(addr as usize);
    }

    fn ld_a16_a(&mut self, data: &[u8; 2], mmu: &mut Mmu) {
        let addr = u16::from_le_bytes(*data);
        mmu.write_byte(addr as usize, self.a);
    }

    fn xor_a(&mut self) {
        self.a = 0;
        self.set_z();
    }

    inc_r!(b, inc_b);
    inc_r!(c, inc_c);

    inc_rr!(l, h, inc_hl);
    inc_rr!(e, d, inc_de);

    fn add_a_b(&mut self) {
        let (res, c) = self.a.overflowing_add(self.b);
        self.a = res;

        self.clear_n();
        self.check_z(self.a);
        self.check_h(self.a);
        self.set_c_to(c);
    }

    fn sub_d8(&mut self, data: &[u8; 1]) {
        let (res, c) = self.a.overflowing_sub(data[0]);
        self.a = res;

        self.set_n();
        self.check_z(self.a);
        self.check_h(self.a);
        self.set_c_to(c);
    }

    fn cp_d8(&mut self, data: &[u8; 1]) {
        let (res, c) = self.a.overflowing_sub(data[0]);

        self.set_n();
        self.check_z(res);
        self.check_h(res);
        self.set_c_to(c);
    }

    dec_r!(a, dec_a);
    dec_r!(b, dec_b);
    dec_r!(c, dec_c);
    dec_r!(d, dec_d);
    dec_r!(e, dec_e);
    dec_rr!(e, d, dec_de);
    dec_rr!(l, h, dec_hl);

    fn ld_a_ind_hl_inc(&mut self, mmu: &mut Mmu) {
        self.a = mmu.read_byte(self.hl() as usize);
        self.inc_hl();
    }

    fn ld_a_ind_de(&mut self, mmu: &mut Mmu) {
        self.a = mmu.read_byte(little_endian!(self.e, self.d) as usize);
    }

    fn ld_ind_hl_a(&mut self, mmu: &mut Mmu) {
        mmu.write_byte(little_endian!(self.l, self.h) as usize, self.a);
    }

    fn ld_ind_hl_dec_a(&mut self, mmu: &mut Mmu) {
        self.ld_ind_hl_a(mmu);
        self.dec_hl();
    }

    fn ld_ind_hl_inc_a(&mut self, mmu: &mut Mmu) {
        self.ld_ind_hl_a(mmu);
        self.inc_hl();
    }

    fn ld_ind_c_a(&mut self, mmu: &mut Mmu) {
        mmu.write_byte(self.c as usize, self.a);
    }

    bit!(d, 0, bit0d);
    bit!(h, 7, bit7h);

    fn rl_c(&mut self) {
        let (res, c) = self.c.overflowing_shl(1);
        self.c = res;

        self.clear_n();
        self.clear_h();
        self.check_z(res);
        self.set_c_to(c);
    }

    fn rl_a(&mut self) {
        let (res, c) = self.a.overflowing_shl(1);
        self.a = res;

        self.clear_z();
        self.clear_n();
        self.clear_h();
        self.set_c_to(c);
    }

    fn rr_d(&mut self) {
        let (res, c) = self.d.overflowing_shr(1);
        self.d = res;

        self.clear_n();
        self.clear_h();
        self.check_z(res);
        self.set_c_to(c);
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

    fn jr_r8(&mut self, data: &[u8; 1]) {
        let offset = data[0] as i8;
        self.pc = self.pc.wrapping_add_signed(offset as i16);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_push_af() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xf5], 0);
        cpu.a = 42;
        cpu.set_z();
        cpu.sp = 10;

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.sp, 8);
        assert_eq!(mmu.read_byte(9), 42);
        assert_eq!(mmu.read_byte(8), 0b01000000);
    }

    #[test]
    fn test_push_bc() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc5], 0);
        cpu.b = 0xfe;
        cpu.c = 0xff;
        cpu.sp = 10;

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 16);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.sp, 8);
        assert_eq!(mmu.read_byte(9), 0xfe);
        assert_eq!(mmu.read_byte(8), 0xff);
    }

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
        assert_eq!(cpu.f, 0xfe);
    }

    #[test]
    fn test_pop_bc() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0xc1, 0xfe, 0xff], 0);
        cpu.sp = 1;

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.sp, 3);
        assert_eq!(cpu.b, 0xff);
        assert_eq!(cpu.c, 0xfe);
    }

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
    fn test_ld_de_d16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x11, 0xfe, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 3);
        assert_eq!(cpu.d, 0xff);
        assert_eq!(cpu.e, 0xfe);
    }

    #[test]
    fn test_ld_hl_d16() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x21, 0xfe, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 3);
        assert_eq!(cpu.h, 0xff);
        assert_eq!(cpu.l, 0xfe);
    }

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

    #[test]
    fn test_ld_a_l() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.l = 42;
        cpu.a = 0;
        mmu.write_slice(&[0x7d], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 42);
    }

    #[test]
    fn test_ld_b_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        cpu.b = 0;
        mmu.write_slice(&[0x47], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.b, 42);
    }

    #[test]
    fn test_ld_c_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        cpu.c = 0;
        mmu.write_slice(&[0x4f], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.c, 42);
    }

    #[test]
    fn test_ld_d_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        cpu.d = 0;
        mmu.write_slice(&[0x57], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.d, 42);
    }

    #[test]
    fn test_ld_d_l() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.l = 42;
        cpu.d = 0;
        mmu.write_slice(&[0x55], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.d, 42);
    }

    #[test]
    fn test_ld_h_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        cpu.h = 0;
        mmu.write_slice(&[0x67], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.h, 42);
    }

    #[test]
    fn test_ld_c_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x0e, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.c, 0xff);
    }

    #[test]
    fn test_ld_d_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x16, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.d, 0xff);
    }

    #[test]
    fn test_ld_e_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x1e, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.e, 0xff);
    }

    #[test]
    fn test_ld_l_d8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x2e, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 2);
        assert_eq!(cpu.l, 0xff);
    }

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
    fn test_inc_b() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.b = 41;
        mmu.write_slice(&[0x04], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.b, 42);
        assert!(!cpu.h());
    }

    #[test]
    fn test_inc_c() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.c = 41;
        mmu.write_slice(&[0x0c], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.c, 42);
        assert!(!cpu.h());
    }

    #[test]
    fn test_inc_hl() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.l = 41;
        mmu.write_slice(&[0x23], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.l, 42);
    }

    #[test]
    fn test_inc_de() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.e = 41;
        mmu.write_slice(&[0x13], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.e, 42);
    }

    #[test]
    fn test_dec_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 43;
        mmu.write_slice(&[0x3d], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.a, 42);
        assert!(!cpu.h());
        assert!(cpu.n());
    }

    #[test]
    fn test_dec_b() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.b = 43;
        mmu.write_slice(&[0x05], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.b, 42);
        assert!(!cpu.h());
        assert!(cpu.n());
    }

    #[test]
    fn test_dec_c() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.c = 43;
        mmu.write_slice(&[0x0d], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.c, 42);
        assert!(!cpu.h());
        assert!(cpu.n());
    }

    #[test]
    fn test_dec_d() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.d = 43;
        mmu.write_slice(&[0x15], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.d, 42);
        assert!(!cpu.h());
        assert!(cpu.n());
    }

    #[test]
    fn test_dec_e() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.e = 43;
        mmu.write_slice(&[0x1d], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 4);
        assert_eq!(cpu.e, 42);
        assert!(!cpu.h());
        assert!(cpu.n());
    }

    #[test]
    fn test_dec_de() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.e = 43;
        mmu.write_slice(&[0x1b], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.e, 42);
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

    #[test]
    fn test_ld_ind_hl_a() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 42;
        cpu.l = 5;
        mmu.write_slice(&[0x32], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(mmu.read_byte(5), 42);
    }

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
        assert_eq!(mmu.read_byte(5), 42);
    }

    #[test]
    fn test_ld_a_ind_de() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.a = 0;
        cpu.d = 0;
        cpu.e = 1;
        mmu.write_slice(&[0x1a, 0xff], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 8);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.a, 0xff);
    }

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

    #[test]
    fn test_rr_d() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        cpu.d = 2;
        mmu.write_slice(&[0xcb, 0x1a], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 2);
        assert!(!cpu.z());
        assert!(!cpu.n());
        assert!(!cpu.h());
        assert!(!cpu.c());
        assert_eq!(cpu.d, 1);
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
    fn test_jr_r8() {
        let mut cpu = Cpu::default();
        let mut mmu = Mmu::default();
        mmu.write_slice(&[0x18, 2], 0);

        let cycles = cpu.exec_instruction(&mut mmu);

        assert_eq!(cycles, 12);
        assert_eq!(cpu.pc, 4);
    }
}
