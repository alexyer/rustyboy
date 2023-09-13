use num_enum::TryFromPrimitive;

use crate::{
    cpu::{Reg, Reg16},
    errors::InstructionError,
};

macro_rules! check_data {
    ($data:ident, $opcode:expr, $prefixed_opcode:expr) => {
        if let Some(prefixed_opcode) = $prefixed_opcode {
            if $data.len() != prefixed_opcode.data_len() {
                panic!(
                    "invalid data provided for {:?}. expected: {}, got: {}",
                    $prefixed_opcode,
                    prefixed_opcode.data_len(),
                    $data.len()
                );
            }
        } else {
            if $data.len() != $opcode.data_len() {
                panic!(
                    "invalid data provided for {:?}. expected: {}, got: {}",
                    $opcode,
                    $opcode.data_len(),
                    $data.len()
                );
            }
        }
    };
}

macro_rules! impl_instruction_constructor {
    ($fun:ident, $type:expr, $opcode:expr, $prefixed_opcode:expr, $regs:expr) => {
        pub fn $fun(data: &[u8]) -> Self {
            let opcode = $opcode;
            let prefixed_opcode = $prefixed_opcode;
            let instruction_type = $type;
            let regs = $regs;

            check_data!(data, opcode, prefixed_opcode);
            Self {
                opcode,
                prefixed_opcode,
                instruction_type,
                regs,
                data: data.into(),
            }
        }
    };
}

#[derive(Debug)]
pub enum InstructionType {
    Nop,
    Daa,
    LdRrD16,
    LdRD8,
    LdAA16,
    LdRR,
    LdAA8,
    LdA8A,
    LdAIndC,
    LdRIndRR,
    LdIndRrR,
    PushRr,
    PopRr,
    LdA16A,
    LdA16Sp,
    LdIndHlD8,
    LdIndHlDecA,
    LdIndHLIncA,
    LdAIndHLInc,
    LdIndCA,
    LdHLSPE8,
    LdRrRr,
    IncR,
    IncRr,
    DecR,
    DecRr,
    DecIndHl,
    AdcR,
    AdcD8,
    AddR,
    AddD8,
    AddRrRr,
    AddSpE8,
    AddAIndHl,
    AndR,
    AndD8,
    SbcR,
    SbcD8,
    SubD8,
    SubR,
    OrR,
    OrAIndHl,
    OrD8,
    XorR,
    XorD8,
    XorAIndHl,
    CpR,
    CpD8,
    CpAIndHl,
    Cpl,
    Ccf,
    Scf,
    Jr,
    Jp,
    Call,
    Ret,
    Di,
    Ei,
    Bit0R,
    Bit7R,
    RlR,
    Rlca,
    RlcR,
    RrR,
    Rrca,
    RrcR,
    Rst,
    SrlR,
    SwapR,
}

#[derive(Debug, Clone, Copy)]
pub enum InstructionReg {
    Reg(Reg),
    Reg16(Reg16),
}

impl Into<Reg> for InstructionReg {
    fn into(self) -> Reg {
        match self {
            InstructionReg::Reg(reg) => reg,
            InstructionReg::Reg16(_) => unreachable!(),
        }
    }
}

impl Into<Reg16> for InstructionReg {
    fn into(self) -> Reg16 {
        match self {
            InstructionReg::Reg(_) => unreachable!(),
            InstructionReg::Reg16(reg) => reg,
        }
    }
}

const INSTRUCTION_CYCLES: [usize; 256] = [
    1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1, 1, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1,
    2, 3, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1, 2, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,
    2, 3, 3, 4, 3, 4, 2, 4, 2, 4, 3, 0, 3, 6, 2, 4, 2, 3, 3, 0, 3, 4, 2, 4, 2, 4, 3, 0, 3, 0, 2, 4,
    3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4, 3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4,
];

const INSTRUCTION_CYCLES_BRANCHED: [usize; 256] = [
    1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1, 1, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1,
    3, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1, 3, 3, 2, 2, 3, 3, 3, 1, 3, 2, 2, 2, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,
    5, 3, 4, 4, 6, 4, 2, 4, 5, 4, 4, 0, 6, 6, 2, 4, 5, 3, 4, 0, 6, 4, 2, 4, 5, 4, 4, 0, 6, 0, 2, 4,
    3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4, 3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4,
];

const INSTRUCTION_CYCLES_CB: [usize; 256] = [
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2,
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2,
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2,
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2,
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2,
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2,
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2,
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2,
];

#[derive(Debug)]
pub struct Instruction {
    opcode: Opcode,
    prefixed_opcode: Option<PrefixedOpcode>,
    instruction_type: InstructionType,
    regs: Option<Vec<InstructionReg>>,
    data: Vec<u8>,
}

impl Instruction {
    pub fn opcode(&self) -> &Opcode {
        &self.opcode
    }

    pub fn instruction_type(&self) -> &InstructionType {
        &self.instruction_type
    }

    pub fn cycles(&self, branched: bool) -> usize {
        if let Some(prefixed_opcode) = self.prefixed_opcode {
            INSTRUCTION_CYCLES_CB[prefixed_opcode as usize] * 4 + 4
        } else {
            if branched {
                INSTRUCTION_CYCLES_BRANCHED[self.opcode as usize] * 4
            } else {
                INSTRUCTION_CYCLES[self.opcode as usize] * 4
            }
        }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn regs(&self) -> &Option<Vec<InstructionReg>> {
        &self.regs
    }

    pub fn nop() -> Self {
        Self {
            opcode: Opcode::Nop,
            prefixed_opcode: None,
            instruction_type: InstructionType::Nop,
            data: vec![],
            regs: None,
        }
    }

    impl_instruction_constructor!(
        ld_bc_d16,
        InstructionType::LdRrD16,
        Opcode::LdBCD16,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::BC)])
    );
    impl_instruction_constructor!(
        ld_de_d16,
        InstructionType::LdRrD16,
        Opcode::LdDED16,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::DE)])
    );
    impl_instruction_constructor!(
        ld_sp_d16,
        InstructionType::LdRrD16,
        Opcode::LdSPD16,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::SP)])
    );
    impl_instruction_constructor!(
        ld_hl_d16,
        InstructionType::LdRrD16,
        Opcode::LdHLD16,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::HL)])
    );
    impl_instruction_constructor!(
        ld_a_d8,
        InstructionType::LdRD8,
        Opcode::LdAD8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        ld_b_d8,
        InstructionType::LdRD8,
        Opcode::LdBD8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        ld_c_d8,
        InstructionType::LdRD8,
        Opcode::LdCD8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        ld_d_d8,
        InstructionType::LdRD8,
        Opcode::LdDD8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        ld_e_d8,
        InstructionType::LdRD8,
        Opcode::LdED8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        ld_h_d8,
        InstructionType::LdRD8,
        Opcode::LdHD8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        ld_l_d8,
        InstructionType::LdRD8,
        Opcode::LdLD8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        ld_a_a16,
        InstructionType::LdAA16,
        Opcode::LdAA16,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        ld_a_a,
        InstructionType::LdRR,
        Opcode::LdAA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_a_b,
        InstructionType::LdRR,
        Opcode::LdAB,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::B)
        ])
    );
    impl_instruction_constructor!(
        ld_a_c,
        InstructionType::LdRR,
        Opcode::LdAC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_a_d,
        InstructionType::LdRR,
        Opcode::LdAD,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::D)
        ])
    );
    impl_instruction_constructor!(
        ld_a_e,
        InstructionType::LdRR,
        Opcode::LdAE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::E)
        ])
    );
    impl_instruction_constructor!(
        ld_a_h,
        InstructionType::LdRR,
        Opcode::LdAH,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::H)
        ])
    );
    impl_instruction_constructor!(
        ld_a_l,
        InstructionType::LdRR,
        Opcode::LdAL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::L)
        ])
    );
    impl_instruction_constructor!(
        ld_b_a,
        InstructionType::LdRR,
        Opcode::LdBA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::B),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_b_b,
        InstructionType::LdRR,
        Opcode::LdBB,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::B),
            InstructionReg::Reg(Reg::B)
        ])
    );
    impl_instruction_constructor!(
        ld_b_c,
        InstructionType::LdRR,
        Opcode::LdBC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::B),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_b_d,
        InstructionType::LdRR,
        Opcode::LdBD,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::B),
            InstructionReg::Reg(Reg::D)
        ])
    );
    impl_instruction_constructor!(
        ld_b_e,
        InstructionType::LdRR,
        Opcode::LdBE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::B),
            InstructionReg::Reg(Reg::E)
        ])
    );
    impl_instruction_constructor!(
        ld_b_h,
        InstructionType::LdRR,
        Opcode::LdBH,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::B),
            InstructionReg::Reg(Reg::H)
        ])
    );
    impl_instruction_constructor!(
        ld_b_l,
        InstructionType::LdRR,
        Opcode::LdBL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::B),
            InstructionReg::Reg(Reg::L)
        ])
    );
    impl_instruction_constructor!(
        ld_c_a,
        InstructionType::LdRR,
        Opcode::LdCA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::C),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_c_b,
        InstructionType::LdRR,
        Opcode::LdCB,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::C),
            InstructionReg::Reg(Reg::B)
        ])
    );
    impl_instruction_constructor!(
        ld_c_c,
        InstructionType::LdRR,
        Opcode::LdCC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::C),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_c_d,
        InstructionType::LdRR,
        Opcode::LdCD,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::C),
            InstructionReg::Reg(Reg::D)
        ])
    );
    impl_instruction_constructor!(
        ld_c_e,
        InstructionType::LdRR,
        Opcode::LdCE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::C),
            InstructionReg::Reg(Reg::E)
        ])
    );
    impl_instruction_constructor!(
        ld_c_h,
        InstructionType::LdRR,
        Opcode::LdCH,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::C),
            InstructionReg::Reg(Reg::H)
        ])
    );
    impl_instruction_constructor!(
        ld_c_l,
        InstructionType::LdRR,
        Opcode::LdCL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::C),
            InstructionReg::Reg(Reg::L)
        ])
    );
    impl_instruction_constructor!(
        ld_d_a,
        InstructionType::LdRR,
        Opcode::LdDA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::D),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_d_b,
        InstructionType::LdRR,
        Opcode::LdDB,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::D),
            InstructionReg::Reg(Reg::B)
        ])
    );
    impl_instruction_constructor!(
        ld_d_c,
        InstructionType::LdRR,
        Opcode::LdDC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::D),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_d_d,
        InstructionType::LdRR,
        Opcode::LdDD,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::D),
            InstructionReg::Reg(Reg::D)
        ])
    );
    impl_instruction_constructor!(
        ld_d_e,
        InstructionType::LdRR,
        Opcode::LdDE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::D),
            InstructionReg::Reg(Reg::E)
        ])
    );
    impl_instruction_constructor!(
        ld_d_h,
        InstructionType::LdRR,
        Opcode::LdDH,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::D),
            InstructionReg::Reg(Reg::H)
        ])
    );
    impl_instruction_constructor!(
        ld_d_l,
        InstructionType::LdRR,
        Opcode::LdDL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::D),
            InstructionReg::Reg(Reg::L)
        ])
    );
    impl_instruction_constructor!(
        ld_e_a,
        InstructionType::LdRR,
        Opcode::LdEA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::E),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_e_b,
        InstructionType::LdRR,
        Opcode::LdDB,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::E),
            InstructionReg::Reg(Reg::B)
        ])
    );
    impl_instruction_constructor!(
        ld_e_c,
        InstructionType::LdRR,
        Opcode::LdDC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::E),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_e_d,
        InstructionType::LdRR,
        Opcode::LdDD,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::E),
            InstructionReg::Reg(Reg::D)
        ])
    );
    impl_instruction_constructor!(
        ld_e_e,
        InstructionType::LdRR,
        Opcode::LdDE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::E),
            InstructionReg::Reg(Reg::E)
        ])
    );
    impl_instruction_constructor!(
        ld_e_h,
        InstructionType::LdRR,
        Opcode::LdDH,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::E),
            InstructionReg::Reg(Reg::H)
        ])
    );
    impl_instruction_constructor!(
        ld_e_l,
        InstructionType::LdRR,
        Opcode::LdEL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::E),
            InstructionReg::Reg(Reg::L)
        ])
    );
    impl_instruction_constructor!(
        ld_h_a,
        InstructionType::LdRR,
        Opcode::LdHA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::H),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_h_b,
        InstructionType::LdRR,
        Opcode::LdHB,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::H),
            InstructionReg::Reg(Reg::B)
        ])
    );
    impl_instruction_constructor!(
        ld_h_c,
        InstructionType::LdRR,
        Opcode::LdHC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::H),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_h_d,
        InstructionType::LdRR,
        Opcode::LdHD,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::H),
            InstructionReg::Reg(Reg::D)
        ])
    );
    impl_instruction_constructor!(
        ld_h_e,
        InstructionType::LdRR,
        Opcode::LdHE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::H),
            InstructionReg::Reg(Reg::E)
        ])
    );
    impl_instruction_constructor!(
        ld_h_h,
        InstructionType::LdRR,
        Opcode::LdHH,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::H),
            InstructionReg::Reg(Reg::H)
        ])
    );
    impl_instruction_constructor!(
        ld_h_l,
        InstructionType::LdRR,
        Opcode::LdHA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::H),
            InstructionReg::Reg(Reg::L)
        ])
    );
    impl_instruction_constructor!(
        ld_l_a,
        InstructionType::LdRR,
        Opcode::LdLA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::L),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_l_b,
        InstructionType::LdRR,
        Opcode::LdLB,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::L),
            InstructionReg::Reg(Reg::B)
        ])
    );
    impl_instruction_constructor!(
        ld_l_c,
        InstructionType::LdRR,
        Opcode::LdLC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::L),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_l_d,
        InstructionType::LdRR,
        Opcode::LdLD,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::L),
            InstructionReg::Reg(Reg::D)
        ])
    );
    impl_instruction_constructor!(
        ld_l_e,
        InstructionType::LdRR,
        Opcode::LdLE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::L),
            InstructionReg::Reg(Reg::E)
        ])
    );
    impl_instruction_constructor!(
        ld_l_h,
        InstructionType::LdRR,
        Opcode::LdLH,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::L),
            InstructionReg::Reg(Reg::H)
        ])
    );
    impl_instruction_constructor!(
        ld_l_l,
        InstructionType::LdRR,
        Opcode::LdLL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::L),
            InstructionReg::Reg(Reg::L)
        ])
    );
    impl_instruction_constructor!(
        ldh_a_a8,
        InstructionType::LdAA8,
        Opcode::LdhAA8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        ldh_a8_a,
        InstructionType::LdA8A,
        Opcode::LdhA8A,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        ld_a_ind_c,
        InstructionType::LdAIndC,
        Opcode::LdAIndC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ldh_a_ind_de,
        InstructionType::LdRIndRR,
        Opcode::LdAIndDE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg16(Reg16::DE)
        ])
    );
    impl_instruction_constructor!(
        ldh_a_ind_hl,
        InstructionType::LdRIndRR,
        Opcode::LdAIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        ldh_b_ind_hl,
        InstructionType::LdRIndRR,
        Opcode::LdBIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::B),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        ldh_c_ind_hl,
        InstructionType::LdRIndRR,
        Opcode::LdCIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::C),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        ldh_d_ind_hl,
        InstructionType::LdRIndRR,
        Opcode::LdDIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::D),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        ldh_e_ind_hl,
        InstructionType::LdRIndRR,
        Opcode::LdEIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::E),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        ldh_h_ind_hl,
        InstructionType::LdRIndRR,
        Opcode::LdHIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::H),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        ldh_l_ind_hl,
        InstructionType::LdRIndRR,
        Opcode::LdLIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::L),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        push_af,
        InstructionType::PushRr,
        Opcode::PushAF,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::AF)])
    );
    impl_instruction_constructor!(
        push_bc,
        InstructionType::PushRr,
        Opcode::PushBC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::BC)])
    );
    impl_instruction_constructor!(
        push_de,
        InstructionType::PushRr,
        Opcode::PushDE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::DE)])
    );
    impl_instruction_constructor!(
        push_hl,
        InstructionType::PushRr,
        Opcode::PushHL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::HL)])
    );
    impl_instruction_constructor!(
        pop_af,
        InstructionType::PopRr,
        Opcode::PopAF,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::AF)])
    );
    impl_instruction_constructor!(
        pop_bc,
        InstructionType::PopRr,
        Opcode::PopBC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::BC)])
    );
    impl_instruction_constructor!(
        pop_de,
        InstructionType::PopRr,
        Opcode::PopDE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::DE)])
    );
    impl_instruction_constructor!(
        pop_hl,
        InstructionType::PopRr,
        Opcode::PopHL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::HL)])
    );
    impl_instruction_constructor!(
        ld_ind_de_a,
        InstructionType::LdIndRrR,
        Opcode::LdIndDEA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::DE),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_a,
        InstructionType::LdIndRrR,
        Opcode::LdIndHLA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_b,
        InstructionType::LdIndRrR,
        Opcode::LdIndHLB,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::B)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_c,
        InstructionType::LdIndRrR,
        Opcode::LdIndHLC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_d,
        InstructionType::LdIndRrR,
        Opcode::LdIndHLD,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::D)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_e,
        InstructionType::LdIndRrR,
        Opcode::LdIndHLE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::E)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_h,
        InstructionType::LdIndRrR,
        Opcode::LdIndHLH,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::H)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_l,
        InstructionType::LdIndRrR,
        Opcode::LdIndHLL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::L)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_d8,
        InstructionType::LdIndHlD8,
        Opcode::LdIndHLD8,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::HL)])
    );
    impl_instruction_constructor!(
        ld_ind_hl_dec_a,
        InstructionType::LdIndHlDecA,
        Opcode::LdIndHLDecA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_hl_inc_a,
        InstructionType::LdIndHLIncA,
        Opcode::LdIndHLIncA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg(Reg::A)
        ])
    );
    impl_instruction_constructor!(
        ld_a_ind_hl_inc,
        InstructionType::LdAIndHLInc,
        Opcode::LdAIndHLInc,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        ld_ind_c_a,
        InstructionType::LdIndCA,
        Opcode::LdIndCA,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg(Reg::C)
        ])
    );
    impl_instruction_constructor!(
        ld_hl_sp_e8,
        InstructionType::LdHLSPE8,
        Opcode::LdHLSPE8,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg16(Reg16::SP)
        ])
    );
    impl_instruction_constructor!(
        ld_sp_hl,
        InstructionType::LdRrRr,
        Opcode::LdSPHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::SP),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        inc_a,
        InstructionType::IncR,
        Opcode::IncA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        inc_b,
        InstructionType::IncR,
        Opcode::IncB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        inc_c,
        InstructionType::IncR,
        Opcode::IncC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        inc_d,
        InstructionType::IncR,
        Opcode::IncD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        inc_e,
        InstructionType::IncR,
        Opcode::IncE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        inc_h,
        InstructionType::IncR,
        Opcode::IncH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        inc_l,
        InstructionType::IncR,
        Opcode::IncL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        inc_bc,
        InstructionType::IncRr,
        Opcode::IncBC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::BC)])
    );
    impl_instruction_constructor!(
        inc_de,
        InstructionType::IncRr,
        Opcode::IncDE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::DE)])
    );
    impl_instruction_constructor!(
        inc_hl,
        InstructionType::IncRr,
        Opcode::IncHL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::HL)])
    );
    impl_instruction_constructor!(
        inc_sp,
        InstructionType::IncRr,
        Opcode::IncSP,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::SP)])
    );
    impl_instruction_constructor!(
        daa,
        InstructionType::Daa,
        Opcode::Daa,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        dec_a,
        InstructionType::DecR,
        Opcode::DecA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        dec_b,
        InstructionType::DecR,
        Opcode::DecB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        dec_c,
        InstructionType::DecR,
        Opcode::DecC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        dec_d,
        InstructionType::DecR,
        Opcode::DecD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        dec_e,
        InstructionType::DecR,
        Opcode::DecE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        dec_h,
        InstructionType::DecR,
        Opcode::DecH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        dec_l,
        InstructionType::DecR,
        Opcode::DecL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        dec_bc,
        InstructionType::DecRr,
        Opcode::DecBC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::BC)])
    );
    impl_instruction_constructor!(
        dec_de,
        InstructionType::DecRr,
        Opcode::DecDE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::DE)])
    );
    impl_instruction_constructor!(
        dec_hl,
        InstructionType::DecRr,
        Opcode::DecHL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::HL)])
    );
    impl_instruction_constructor!(
        dec_sp,
        InstructionType::DecRr,
        Opcode::DecSP,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::SP)])
    );
    impl_instruction_constructor!(
        dec_ind_hl,
        InstructionType::DecIndHl,
        Opcode::DecIndHL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::HL)])
    );
    impl_instruction_constructor!(
        adc_a,
        InstructionType::AdcR,
        Opcode::AdcA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        adc_b,
        InstructionType::AdcR,
        Opcode::AdcB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        adc_c,
        InstructionType::AdcR,
        Opcode::AdcC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        adc_d,
        InstructionType::AdcR,
        Opcode::AdcD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        adc_e,
        InstructionType::AdcR,
        Opcode::AdcE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        adc_h,
        InstructionType::AdcR,
        Opcode::AdcH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        adc_l,
        InstructionType::AdcR,
        Opcode::AdcL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        adc_d8,
        InstructionType::AdcD8,
        Opcode::AdcD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        add_a,
        InstructionType::AddR,
        Opcode::AddA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        add_b,
        InstructionType::AddR,
        Opcode::AddB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        add_c,
        InstructionType::AddR,
        Opcode::AddC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        add_d,
        InstructionType::AddR,
        Opcode::AddD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        add_e,
        InstructionType::AddR,
        Opcode::AddE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        add_h,
        InstructionType::AddR,
        Opcode::AddH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        add_l,
        InstructionType::AddR,
        Opcode::AddL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        add_d8,
        InstructionType::AddD8,
        Opcode::AddD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        add_hl_bc,
        InstructionType::AddRrRr,
        Opcode::AddHLBC,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg16(Reg16::BC)
        ])
    );
    impl_instruction_constructor!(
        add_hl_de,
        InstructionType::AddRrRr,
        Opcode::AddHLDE,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg16(Reg16::DE)
        ])
    );
    impl_instruction_constructor!(
        add_hl_hl,
        InstructionType::AddRrRr,
        Opcode::AddHLHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        add_hl_sp,
        InstructionType::AddRrRr,
        Opcode::AddHLSP,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg16(Reg16::HL),
            InstructionReg::Reg16(Reg16::SP)
        ])
    );
    impl_instruction_constructor!(
        add_sp_e8,
        InstructionType::AddSpE8,
        Opcode::AddSPE8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        add_a_ind_hl,
        InstructionType::AddAIndHl,
        Opcode::AddAIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        and_a,
        InstructionType::AndR,
        Opcode::AndA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        and_b,
        InstructionType::AndR,
        Opcode::AndB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        and_c,
        InstructionType::AndR,
        Opcode::AndC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        and_d,
        InstructionType::AndR,
        Opcode::AndD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        and_e,
        InstructionType::AndR,
        Opcode::AndE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        and_h,
        InstructionType::AndR,
        Opcode::AndH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        and_l,
        InstructionType::AndR,
        Opcode::AndL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        and_d8,
        InstructionType::AndD8,
        Opcode::AndD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        sbc_a,
        InstructionType::SbcR,
        Opcode::SbcA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        sbc_b,
        InstructionType::SbcR,
        Opcode::SbcB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        sbc_c,
        InstructionType::SbcR,
        Opcode::SbcC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        sbc_d,
        InstructionType::SbcR,
        Opcode::SbcD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        sbc_e,
        InstructionType::SbcR,
        Opcode::SbcE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        sbc_h,
        InstructionType::SbcR,
        Opcode::SbcH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        sbc_l,
        InstructionType::SbcR,
        Opcode::SbcL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        sbc_d8,
        InstructionType::SbcD8,
        Opcode::SbcD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        sub_d8,
        InstructionType::SubD8,
        Opcode::SubD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        sub_a,
        InstructionType::SubR,
        Opcode::SubA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        sub_b,
        InstructionType::SubR,
        Opcode::SubB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        sub_c,
        InstructionType::SubR,
        Opcode::SubC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        sub_d,
        InstructionType::SubR,
        Opcode::SubD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        sub_e,
        InstructionType::SubR,
        Opcode::SubE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        sub_h,
        InstructionType::SubR,
        Opcode::SubH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        sub_l,
        InstructionType::SubR,
        Opcode::SubL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        or_a,
        InstructionType::OrR,
        Opcode::OrA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        or_b,
        InstructionType::OrR,
        Opcode::OrB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        or_c,
        InstructionType::OrR,
        Opcode::OrC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        or_d,
        InstructionType::OrR,
        Opcode::OrD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        or_e,
        InstructionType::OrR,
        Opcode::OrE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        or_h,
        InstructionType::OrR,
        Opcode::OrH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        or_l,
        InstructionType::OrR,
        Opcode::OrL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        or_a_ind_hl,
        InstructionType::OrAIndHl,
        Opcode::OrAIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        or_d8,
        InstructionType::OrD8,
        Opcode::OrD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        xor_a,
        InstructionType::XorR,
        Opcode::XorA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        xor_b,
        InstructionType::XorR,
        Opcode::XorB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        xor_c,
        InstructionType::XorR,
        Opcode::XorC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        xor_d,
        InstructionType::XorR,
        Opcode::XorD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        xor_e,
        InstructionType::XorR,
        Opcode::XorE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        xor_h,
        InstructionType::XorR,
        Opcode::XorH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        xor_l,
        InstructionType::XorR,
        Opcode::XorL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        xor_d8,
        InstructionType::XorD8,
        Opcode::XorD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        xor_a_ind_hl,
        InstructionType::XorAIndHl,
        Opcode::XorAIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        cp_a,
        InstructionType::CpR,
        Opcode::CpA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        cp_b,
        InstructionType::CpR,
        Opcode::CpB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        cp_c,
        InstructionType::CpR,
        Opcode::CpC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        cp_d,
        InstructionType::CpR,
        Opcode::CpD,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        cp_e,
        InstructionType::CpR,
        Opcode::CpE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        cp_h,
        InstructionType::CpR,
        Opcode::CpH,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        cp_l,
        InstructionType::CpR,
        Opcode::CpL,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        cp_d8,
        InstructionType::CpD8,
        Opcode::CpD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        cp_a_ind_hl,
        InstructionType::CpAIndHl,
        Opcode::CpAIndHL,
        None::<PrefixedOpcode>,
        Some(vec![
            InstructionReg::Reg(Reg::A),
            InstructionReg::Reg16(Reg16::HL)
        ])
    );
    impl_instruction_constructor!(
        cpl,
        InstructionType::Cpl,
        Opcode::Cpl,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        ccf,
        InstructionType::Ccf,
        Opcode::Ccf,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        scf,
        InstructionType::Scf,
        Opcode::Scf,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jr_r8,
        InstructionType::Jr,
        Opcode::JrR8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jr_c_r8,
        InstructionType::Jr,
        Opcode::JrCR8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jr_nc_r8,
        InstructionType::Jr,
        Opcode::JrNcR8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jr_nz_r8,
        InstructionType::Jr,
        Opcode::JrNzR8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jr_z_r8,
        InstructionType::Jr,
        Opcode::JrZR8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jp_a16,
        InstructionType::Jp,
        Opcode::JpA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jp_nc_a16,
        InstructionType::Jp,
        Opcode::JpNcA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jp_nz_a16,
        InstructionType::Jp,
        Opcode::JpNzA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jp_c_a16,
        InstructionType::Jp,
        Opcode::JpCA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jp_z_a16,
        InstructionType::Jp,
        Opcode::JpZA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        jp_hl,
        InstructionType::Jp,
        Opcode::JpHL,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        call_a16,
        InstructionType::Call,
        Opcode::CallA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        call_c_a16,
        InstructionType::Call,
        Opcode::CallCA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        call_z_a16,
        InstructionType::Call,
        Opcode::CallZA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        call_nc_a16,
        InstructionType::Call,
        Opcode::CallNcA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        call_nz_a16,
        InstructionType::Call,
        Opcode::CallNzA16,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        di,
        InstructionType::Di,
        Opcode::Di,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        ei,
        InstructionType::Ei,
        Opcode::Ei,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        bit0d,
        InstructionType::Bit0R,
        Opcode::Prefix,
        Some(PrefixedOpcode::Bit0D),
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        bit7h,
        InstructionType::Bit7R,
        Opcode::Prefix,
        Some(PrefixedOpcode::Bit7H),
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        rl_a,
        InstructionType::RlR,
        Opcode::RlA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        rl_c,
        InstructionType::RlR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RlC),
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        rlca,
        InstructionType::Rlca,
        Opcode::Rlca,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        rlc_a,
        InstructionType::RlcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RlcA),
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        rlc_b,
        InstructionType::RlcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RlcB),
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        rlc_c,
        InstructionType::RlcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RlcC),
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        rlc_d,
        InstructionType::RlcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RlcD),
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        rlc_e,
        InstructionType::RlcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RlcE),
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        rlc_h,
        InstructionType::RlcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RlcH),
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        rlc_l,
        InstructionType::RlcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RlcL),
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        rr_a,
        InstructionType::RrR,
        Opcode::RrA,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        rr_c,
        InstructionType::RrR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrC),
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        rr_d,
        InstructionType::RrR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrD),
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        rr_e,
        InstructionType::RrR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrE),
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        rrca,
        InstructionType::Rrca,
        Opcode::Rrca,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        rrc_a,
        InstructionType::RrcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrcA),
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        rrc_b,
        InstructionType::RrcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrcB),
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        rrc_c,
        InstructionType::RrcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrcC),
        Some(vec![InstructionReg::Reg(Reg::C)])
    );
    impl_instruction_constructor!(
        rrc_d,
        InstructionType::RrcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrcD),
        Some(vec![InstructionReg::Reg(Reg::D)])
    );
    impl_instruction_constructor!(
        rrc_e,
        InstructionType::RrcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrcE),
        Some(vec![InstructionReg::Reg(Reg::E)])
    );
    impl_instruction_constructor!(
        rrc_h,
        InstructionType::RrcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrcH),
        Some(vec![InstructionReg::Reg(Reg::H)])
    );
    impl_instruction_constructor!(
        rrc_l,
        InstructionType::RrcR,
        Opcode::Prefix,
        Some(PrefixedOpcode::RrcL),
        Some(vec![InstructionReg::Reg(Reg::L)])
    );
    impl_instruction_constructor!(
        srl_b,
        InstructionType::SrlR,
        Opcode::Prefix,
        Some(PrefixedOpcode::SrlB),
        Some(vec![InstructionReg::Reg(Reg::B)])
    );
    impl_instruction_constructor!(
        swap_a,
        InstructionType::SwapR,
        Opcode::Prefix,
        Some(PrefixedOpcode::SwapA),
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        ret,
        InstructionType::Ret,
        Opcode::Ret,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        reti,
        InstructionType::Ret,
        Opcode::Reti,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        ret_c,
        InstructionType::Ret,
        Opcode::RetC,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        ret_nc,
        InstructionType::Ret,
        Opcode::RetNc,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        ret_nz,
        InstructionType::Ret,
        Opcode::RetNz,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        ret_z,
        InstructionType::Ret,
        Opcode::RetZ,
        None::<PrefixedOpcode>,
        None
    );

    impl_instruction_constructor!(
        rst00,
        InstructionType::Rst,
        Opcode::Rst00,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        ld_a16_a,
        InstructionType::LdA16A,
        Opcode::LdA16A,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::A)])
    );
    impl_instruction_constructor!(
        ld_a16_sp,
        InstructionType::LdA16Sp,
        Opcode::LdA16SP,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg16(Reg16::SP)])
    );
}

#[derive(Debug, Clone, Copy, TryFromPrimitive)]
#[num_enum(error_type(name = InstructionError, constructor = InstructionError::UnrecognizedOpcode))]
#[repr(u8)]
pub enum Opcode {
    Nop = 0x00,
    LdBCD16 = 0x01,
    IncBC = 0x03,
    IncB = 0x04,
    DecB = 0x05,
    LdBD8 = 0x06,
    Rlca = 0x07,
    LdA16SP = 0x08,
    AddHLBC = 0x09,
    DecBC = 0x0b,
    IncC = 0x0c,
    DecC = 0x0d,
    LdCD8 = 0x0e,
    Rrca = 0x0f,

    LdDED16 = 0x11,
    LdIndDEA = 0x12,
    IncDE = 0x13,
    IncD = 0x14,
    DecD = 0x15,
    LdDD8 = 0x16,
    RlA = 0x17,
    JrR8 = 0x18,
    AddHLDE = 0x19,
    LdAIndDE = 0x1a,
    DecDE = 0x1b,
    IncE = 0x1c,
    DecE = 0x1d,
    LdED8 = 0x1e,
    RrA = 0x1f,

    JrNzR8 = 0x20,
    LdHLD16 = 0x21,
    LdIndHLIncA = 0x22,
    IncHL = 0x23,
    IncH = 0x24,
    DecH = 0x25,
    LdHD8 = 0x26,
    Daa = 0x27,
    JrZR8 = 0x28,
    AddHLHL = 0x29,
    LdAIndHLInc = 0x2a,
    DecHL = 0x2b,
    IncL = 0x2c,
    DecL = 0x2d,
    LdLD8 = 0x2e,
    Cpl = 0x2f,

    JrNcR8 = 0x30,
    LdSPD16 = 0x31,
    LdIndHLDecA = 0x32,
    IncSP = 0x33,
    DecIndHL = 0x35,
    LdIndHLD8 = 0x36,
    Scf = 0x37,
    JrCR8 = 0x38,
    AddHLSP = 0x39,
    DecSP = 0x3b,
    IncA = 0x3c,
    DecA = 0x3d,
    LdAD8 = 0x3e,
    Ccf = 0x3f,

    LdBB = 0x40,
    LdBC = 0x41,
    LdBD = 0x42,
    LdBE = 0x43,
    LdBH = 0x44,
    LdBL = 0x45,
    LdBIndHL = 0x46,
    LdBA = 0x47,
    LdCB = 0x48,
    LdCC = 0x49,
    LdCD = 0x4a,
    LdCE = 0x4b,
    LdCH = 0x4c,
    LdCL = 0x4d,
    LdCIndHL = 0x4e,
    LdCA = 0x4f,

    LdDB = 0x50,
    LdDC = 0x51,
    LdDD = 0x52,
    LdDE = 0x53,
    LdDH = 0x54,
    LdDL = 0x55,
    LdDIndHL = 0x56,
    LdDA = 0x57,
    LdEB = 0x58,
    LdEC = 0x59,
    LdED = 0x5a,
    LdEE = 0x5b,
    LdEH = 0x5c,
    LdEL = 0x5d,
    LdEIndHL = 0x5e,
    LdEA = 0x5f,

    LdHB = 0x60,
    LdHC = 0x61,
    LdHD = 0x62,
    LdHE = 0x63,
    LdHH = 0x64,
    LdHL = 0x65,
    LdHIndHL = 0x66,
    LdHA = 0x67,
    LdLB = 0x68,
    LdLC = 0x69,
    LdLD = 0x6a,
    LdLE = 0x6b,
    LdLH = 0x6c,
    LdLL = 0x6d,
    LdLIndHL = 0x6e,
    LdLA = 0x6f,

    LdIndHLB = 0x70,
    LdIndHLC = 0x71,
    LdIndHLD = 0x72,
    LdIndHLE = 0x73,
    LdIndHLH = 0x74,
    LdIndHLL = 0x75,
    LdIndHLA = 0x77,
    LdAB = 0x78,
    LdAC = 0x79,
    LdAD = 0x7a,
    LdAE = 0x7b,
    LdAH = 0x7c,
    LdAL = 0x7d,
    LdAIndHL = 0x7e,
    LdAA = 0x7f,

    AddB = 0x80,
    AddC = 0x81,
    AddD = 0x82,
    AddE = 0x83,
    AddH = 0x84,
    AddL = 0x85,
    AddAIndHL = 0x86,
    AddA = 0x87,
    AdcB = 0x88,
    AdcC = 0x89,
    AdcD = 0x8a,
    AdcE = 0x8b,
    AdcH = 0x8c,
    AdcL = 0x8d,
    AdcA = 0x8f,

    SubB = 0x90,
    SubC = 0x91,
    SubD = 0x92,
    SubE = 0x93,
    SubH = 0x94,
    SubL = 0x95,
    SubA = 0x97,
    SbcB = 0x98,
    SbcC = 0x99,
    SbcD = 0x9a,
    SbcE = 0x9b,
    SbcH = 0x9c,
    SbcL = 0x9d,
    SbcA = 0x9f,

    AndB = 0xa0,
    AndC = 0xa1,
    AndD = 0xa2,
    AndE = 0xa3,
    AndH = 0xa4,
    AndL = 0xa5,
    AndA = 0xa7,
    XorB = 0xa8,
    XorC = 0xa9,
    XorD = 0xaa,
    XorE = 0xab,
    XorH = 0xac,
    XorL = 0xad,
    XorAIndHL = 0xae,
    XorA = 0xaf,

    OrB = 0xb0,
    OrC = 0xb1,
    OrD = 0xb2,
    OrE = 0xb3,
    OrH = 0xb4,
    OrL = 0xb5,
    OrAIndHL = 0xb6,
    OrA = 0xb7,
    CpB = 0xb8,
    CpC = 0xb9,
    CpD = 0xba,
    CpE = 0xbb,
    CpH = 0xbc,
    CpL = 0xbd,
    CpAIndHL = 0xbe,
    CpA = 0xbf,

    RetNz = 0xc0,
    PopBC = 0xc1,
    JpNzA16 = 0xc2,
    JpA16 = 0xc3,
    CallNzA16 = 0xc4,
    PushBC = 0xc5,
    AddD8 = 0xc6,
    Rst00 = 0xc7,
    RetZ = 0xc8,
    Ret = 0xc9,
    JpZA16 = 0xca,
    Prefix = 0xcb,
    CallZA16 = 0xcc,
    AdcD8 = 0xce,
    CallA16 = 0xcd,

    RetNc = 0xd0,
    PopDE = 0xd1,
    JpNcA16 = 0xd2,
    CallNcA16 = 0xd4,
    PushDE = 0xd5,
    SubD8 = 0xd6,
    RetC = 0xd8,
    Reti = 0xd9,
    JpCA16 = 0xda,
    CallCA16 = 0xdc,
    SbcD8 = 0xde,

    LdhA8A = 0xe0,
    PopHL = 0xe1,
    LdIndCA = 0xe2,
    PushHL = 0xe5,
    JpHL = 0xe9,
    AndD8 = 0xe6,
    AddSPE8 = 0xe8,
    LdA16A = 0xea,

    LdhAA8 = 0xf0,
    PopAF = 0xf1,
    LdAIndC = 0xf2,
    Di = 0xf3,
    PushAF = 0xf5,
    OrD8 = 0xf6,
    LdHLSPE8 = 0xf8,
    LdSPHL = 0xf9,
    LdAA16 = 0xfa,
    Ei = 0xfb,
    CpD8 = 0xfe,

    XorD8 = 0xee,
}

impl Opcode {
    /// Instruction data length in bytes.
    pub fn data_len(&self) -> usize {
        match self {
            Opcode::Nop => 0,
            Opcode::Prefix => 0,
            Opcode::Di => 0,
            Opcode::Ei => 0,
            Opcode::JrR8 => 1,
            Opcode::JrNcR8 => 1,
            Opcode::JrNzR8 => 1,
            Opcode::JrCR8 => 1,
            Opcode::JrZR8 => 1,
            Opcode::JpA16 => 2,
            Opcode::JpCA16 => 2,
            Opcode::JpNcA16 => 2,
            Opcode::JpNzA16 => 2,
            Opcode::JpZA16 => 2,
            Opcode::JpHL => 0,
            Opcode::LdAD8 => 1,
            Opcode::LdBD8 => 1,
            Opcode::LdCD8 => 1,
            Opcode::LdDD8 => 1,
            Opcode::LdED8 => 1,
            Opcode::LdHD8 => 1,
            Opcode::LdLD8 => 1,
            Opcode::LdAA => 0,
            Opcode::LdAB => 0,
            Opcode::LdAC => 0,
            Opcode::LdAD => 0,
            Opcode::LdAE => 0,
            Opcode::LdAH => 0,
            Opcode::LdAL => 0,
            Opcode::LdBA => 0,
            Opcode::LdBB => 0,
            Opcode::LdBC => 0,
            Opcode::LdBD => 0,
            Opcode::LdBE => 0,
            Opcode::LdBH => 0,
            Opcode::LdBL => 0,
            Opcode::LdCA => 0,
            Opcode::LdCB => 0,
            Opcode::LdCC => 0,
            Opcode::LdCD => 0,
            Opcode::LdCE => 0,
            Opcode::LdCH => 0,
            Opcode::LdCL => 0,
            Opcode::LdDA => 0,
            Opcode::LdDB => 0,
            Opcode::LdDC => 0,
            Opcode::LdDD => 0,
            Opcode::LdDE => 0,
            Opcode::LdDH => 0,
            Opcode::LdDL => 0,
            Opcode::LdEA => 0,
            Opcode::LdEB => 0,
            Opcode::LdEC => 0,
            Opcode::LdED => 0,
            Opcode::LdEE => 0,
            Opcode::LdEH => 0,
            Opcode::LdEL => 0,
            Opcode::LdHA => 0,
            Opcode::LdHB => 0,
            Opcode::LdHC => 0,
            Opcode::LdHD => 0,
            Opcode::LdHE => 0,
            Opcode::LdHH => 0,
            Opcode::LdHL => 0,
            Opcode::LdLA => 0,
            Opcode::LdLB => 0,
            Opcode::LdLC => 0,
            Opcode::LdLD => 0,
            Opcode::LdLE => 0,
            Opcode::LdLH => 0,
            Opcode::LdLL => 0,
            Opcode::LdhAA8 => 1,
            Opcode::LdhA8A => 1,
            Opcode::PushAF => 0,
            Opcode::PushBC => 0,
            Opcode::PushDE => 0,
            Opcode::PushHL => 0,
            Opcode::PopAF => 0,
            Opcode::PopBC => 0,
            Opcode::PopDE => 0,
            Opcode::PopHL => 0,
            Opcode::LdAIndC => 0,
            Opcode::LdAIndDE => 0,
            Opcode::LdAIndHL => 0,
            Opcode::LdBIndHL => 0,
            Opcode::LdCIndHL => 0,
            Opcode::LdDIndHL => 0,
            Opcode::LdEIndHL => 0,
            Opcode::LdHIndHL => 0,
            Opcode::LdLIndHL => 0,
            Opcode::LdIndDEA => 0,
            Opcode::LdIndHLA => 0,
            Opcode::LdIndHLB => 0,
            Opcode::LdIndHLC => 0,
            Opcode::LdIndHLD => 0,
            Opcode::LdIndHLE => 0,
            Opcode::LdIndHLH => 0,
            Opcode::LdIndHLL => 0,
            Opcode::LdIndHLD8 => 1,
            Opcode::LdIndHLDecA => 0,
            Opcode::LdIndHLIncA => 0,
            Opcode::LdBCD16 => 2,
            Opcode::LdDED16 => 2,
            Opcode::LdAA16 => 2,
            Opcode::LdA16SP => 2,
            Opcode::LdSPD16 => 2,
            Opcode::LdHLD16 => 2,
            Opcode::LdHLSPE8 => 1,
            Opcode::LdSPHL => 0,
            Opcode::IncA => 0,
            Opcode::IncB => 0,
            Opcode::IncC => 0,
            Opcode::IncD => 0,
            Opcode::IncE => 0,
            Opcode::IncBC => 0,
            Opcode::IncDE => 0,
            Opcode::IncHL => 0,
            Opcode::IncSP => 0,
            Opcode::IncH => 0,
            Opcode::IncL => 0,
            Opcode::Daa => 0,
            Opcode::DecA => 0,
            Opcode::DecB => 0,
            Opcode::DecC => 0,
            Opcode::DecD => 0,
            Opcode::DecE => 0,
            Opcode::DecH => 0,
            Opcode::DecL => 0,
            Opcode::DecBC => 0,
            Opcode::DecDE => 0,
            Opcode::DecHL => 0,
            Opcode::DecSP => 0,
            Opcode::DecIndHL => 0,
            Opcode::OrA => 0,
            Opcode::OrAIndHL => 0,
            Opcode::OrB => 0,
            Opcode::OrC => 0,
            Opcode::OrD => 0,
            Opcode::OrE => 0,
            Opcode::OrH => 0,
            Opcode::OrL => 0,
            Opcode::OrD8 => 1,
            Opcode::XorA => 0,
            Opcode::XorB => 0,
            Opcode::XorC => 0,
            Opcode::XorD => 0,
            Opcode::XorE => 0,
            Opcode::XorH => 0,
            Opcode::XorL => 0,
            Opcode::XorD8 => 1,
            Opcode::XorAIndHL => 0,
            Opcode::LdIndCA => 0,
            Opcode::AdcA => 0,
            Opcode::AdcB => 0,
            Opcode::AdcC => 0,
            Opcode::AdcD => 0,
            Opcode::AdcE => 0,
            Opcode::AdcH => 0,
            Opcode::AdcL => 0,
            Opcode::AdcD8 => 1,
            Opcode::AddA => 0,
            Opcode::AddB => 0,
            Opcode::AddC => 0,
            Opcode::AddD => 0,
            Opcode::AddE => 0,
            Opcode::AddH => 0,
            Opcode::AddL => 0,
            Opcode::AddD8 => 1,
            Opcode::AddHLBC => 0,
            Opcode::AddHLDE => 0,
            Opcode::AddHLHL => 0,
            Opcode::AddHLSP => 0,
            Opcode::AddSPE8 => 1,
            Opcode::AddAIndHL => 0,
            Opcode::AndA => 0,
            Opcode::AndB => 0,
            Opcode::AndC => 0,
            Opcode::AndD => 0,
            Opcode::AndE => 0,
            Opcode::AndH => 0,
            Opcode::AndL => 0,
            Opcode::AndD8 => 1,
            Opcode::SbcA => 0,
            Opcode::SbcB => 0,
            Opcode::SbcC => 0,
            Opcode::SbcD => 0,
            Opcode::SbcE => 0,
            Opcode::SbcH => 0,
            Opcode::SbcL => 0,
            Opcode::SbcD8 => 1,
            Opcode::SubD8 => 1,
            Opcode::SubA => 0,
            Opcode::SubB => 0,
            Opcode::SubC => 0,
            Opcode::SubD => 0,
            Opcode::SubE => 0,
            Opcode::SubH => 0,
            Opcode::SubL => 0,
            Opcode::CpA => 0,
            Opcode::CpB => 0,
            Opcode::CpC => 0,
            Opcode::CpD => 0,
            Opcode::CpE => 0,
            Opcode::CpH => 0,
            Opcode::CpL => 0,
            Opcode::CpD8 => 1,
            Opcode::CpAIndHL => 0,
            Opcode::Cpl => 0,
            Opcode::Ccf => 0,
            Opcode::Scf => 0,
            Opcode::CallA16 => 2,
            Opcode::CallCA16 => 2,
            Opcode::CallZA16 => 2,
            Opcode::CallNcA16 => 2,
            Opcode::CallNzA16 => 2,
            Opcode::RlA => 0,
            Opcode::Rlca => 0,
            Opcode::RrA => 0,
            Opcode::Rrca => 0,
            Opcode::Ret => 0,
            Opcode::Reti => 0,
            Opcode::RetC => 0,
            Opcode::RetNc => 0,
            Opcode::RetZ => 0,
            Opcode::RetNz => 0,
            Opcode::Rst00 => 0,
            Opcode::LdA16A => 2,
            Opcode::LdAIndHLInc => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, TryFromPrimitive)]
#[num_enum(error_type(name = InstructionError, constructor = InstructionError::UnrecognizedPrefixedOpcode))]
#[repr(u8)]
pub enum PrefixedOpcode {
    RlcB = 0x00,
    RlcC = 0x01,
    RlcD = 0x02,
    RlcE = 0x03,
    RlcH = 0x04,
    RlcL = 0x05,
    RlcA = 0x07,
    RrcB = 0x08,
    RrcC = 0x09,
    RrcD = 0x0a,
    RrcE = 0x0b,
    RrcH = 0x0c,
    RrcL = 0x0d,
    RrcA = 0x0f,
    RlC = 0x11,
    RrC = 0x19,
    RrD = 0x1a,
    RrE = 0x1b,

    SwapA = 0x37,
    SrlB = 0x38,

    Bit0D = 0x42,

    Bit7H = 0x7c,
}

impl PrefixedOpcode {
    pub fn data_len(&self) -> usize {
        0
    }
}
