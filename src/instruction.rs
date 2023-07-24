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
    AdcD8,
    AddR,
    AddD8,
    AddRrRr,
    AddSpE8,
    AddAIndHl,
    AndD8,
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
    RrR,
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

    pub fn prefixed_opcode(&self) -> &Option<PrefixedOpcode> {
        &self.prefixed_opcode
    }

    pub fn cycles(&self) -> usize {
        if let Some(prefixed_opcode) = self.prefixed_opcode {
            self.opcode.cycles() + prefixed_opcode.cycles()
        } else {
            self.opcode.cycles()
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
        adc_d8,
        InstructionType::AdcD8,
        Opcode::AdcD8,
        None::<PrefixedOpcode>,
        None
    );
    impl_instruction_constructor!(
        add_b,
        InstructionType::AddR,
        Opcode::AddB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
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
        and_d8,
        InstructionType::AndD8,
        Opcode::AndD8,
        None::<PrefixedOpcode>,
        None
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
        sub_b,
        InstructionType::SubR,
        Opcode::SubB,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::B)])
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
        xor_c,
        InstructionType::XorR,
        Opcode::XorC,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::C)])
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
        cp_e,
        InstructionType::CpR,
        Opcode::CpE,
        None::<PrefixedOpcode>,
        Some(vec![InstructionReg::Reg(Reg::E)])
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
        jp_nz_a16,
        InstructionType::Jp,
        Opcode::JpNzA16,
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
        ret_z,
        InstructionType::Ret,
        Opcode::RetZ,
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

#[derive(Debug)]
pub enum Opcode {
    // Misc / control instructions
    Nop,
    Prefix,
    Di,
    Ei,

    // Jumps / Calls
    JrR8,
    JrNcR8,
    JrNzR8,
    JrCR8,
    JrZR8,
    JpA16,
    JpNzA16,
    JpHL,
    CallA16,
    CallNzA16,
    Ret,
    RetC,
    RetNc,
    RetZ,

    // 8-bit load instructions
    LdAD8,
    LdBD8,
    LdCD8,
    LdDD8,
    LdED8,
    LdHD8,
    LdLD8,
    LdAA,
    LdAB,
    LdAC,
    LdAD,
    LdAE,
    LdAL,
    LdAH,
    LdBA,
    LdBB,
    LdBC,
    LdBD,
    LdBE,
    LdBH,
    LdBL,
    LdCA,
    LdCB,
    LdCC,
    LdCD,
    LdCE,
    LdCH,
    LdCL,
    LdDA,
    LdDB,
    LdDC,
    LdDD,
    LdDE,
    LdDH,
    LdDL,
    LdEA,
    LdEB,
    LdEC,
    LdED,
    LdEE,
    LdEH,
    LdEL,
    LdHA,
    LdHB,
    LdHC,
    LdHD,
    LdHE,
    LdHH,
    LdHL,
    LdLA,
    LdLB,
    LdLC,
    LdLD,
    LdLE,
    LdLH,
    LdLL,
    LdhA8A,
    LdhAA8,
    LdAIndC,
    LdAIndDE,
    LdAIndHL,
    LdBIndHL,
    LdCIndHL,
    LdDIndHL,
    LdEIndHL,
    LdHIndHL,
    LdLIndHL,
    LdIndDEA,
    LdIndHLA,
    LdIndHLB,
    LdIndHLC,
    LdIndHLD,
    LdIndHLE,
    LdIndHLH,
    LdIndHLL,
    LdAIndHLInc,
    LdIndHLDecA,
    LdIndHLIncA,
    LdIndHLD8,
    LdIndCA,
    PushAF,
    PushBC,
    PushDE,
    PushHL,
    PopAF,
    PopBC,
    PopDE,
    PopHL,

    // 16-bit load instructions
    LdA16A,
    LdAA16,
    LdBCD16,
    LdDED16,
    LdHLD16,
    LdHLSPE8,
    LdSPD16,
    LdSPHL,
    LdA16SP,

    // 8-bit arithmetic and logical instructions
    AdcD8,
    AddB,
    AddAIndHL,
    AddD8,
    AddHLBC,
    AddHLDE,
    AddHLHL,
    AddHLSP,
    AddSPE8,
    AndD8,
    SbcD8,
    SubD8,
    SubB,
    IncA,
    IncB,
    IncC,
    IncD,
    IncE,
    IncBC,
    IncDE,
    IncHL,
    IncSP,
    IncH,
    IncL,
    Daa,
    DecA,
    DecB,
    DecC,
    DecD,
    DecE,
    DecH,
    DecL,
    DecBC,
    DecDE,
    DecHL,
    DecSP,
    DecIndHL,
    OrA,
    OrAIndHL,
    OrB,
    OrC,
    OrD8,
    XorA,
    XorC,
    XorL,
    XorD8,
    XorAIndHL,
    CpE,
    CpD8,
    CpAIndHL,
    Cpl,
    Scf,
    RlA,
    RrA,
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
            Opcode::JpNzA16 => 2,
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
            Opcode::OrD8 => 1,
            Opcode::XorA => 0,
            Opcode::XorC => 0,
            Opcode::XorL => 0,
            Opcode::XorD8 => 1,
            Opcode::XorAIndHL => 0,
            Opcode::LdIndCA => 0,
            Opcode::AdcD8 => 1,
            Opcode::AddB => 0,
            Opcode::AddD8 => 1,
            Opcode::AddHLBC => 0,
            Opcode::AddHLDE => 0,
            Opcode::AddHLHL => 0,
            Opcode::AddHLSP => 0,
            Opcode::AddSPE8 => 1,
            Opcode::AddAIndHL => 0,
            Opcode::AndD8 => 1,
            Opcode::SbcD8 => 1,
            Opcode::SubD8 => 1,
            Opcode::SubB => 0,
            Opcode::CpE => 0,
            Opcode::CpD8 => 1,
            Opcode::CpAIndHL => 0,
            Opcode::Cpl => 0,
            Opcode::Scf => 0,
            Opcode::CallA16 => 2,
            Opcode::CallNzA16 => 2,
            Opcode::RlA => 0,
            Opcode::RrA => 0,
            Opcode::Ret => 0,
            Opcode::RetC => 0,
            Opcode::RetNc => 0,
            Opcode::RetZ => 0,
            Opcode::LdA16A => 2,
            Opcode::LdAIndHLInc => 0,
        }
    }

    /// Instruction duration in T-states.
    pub fn cycles(&self) -> usize {
        match self {
            Opcode::Nop => 4,
            Opcode::Prefix => 4,
            Opcode::Di => 4,
            Opcode::Ei => 4,
            Opcode::JrR8 => 12,
            Opcode::JrCR8 => 12,
            // FIXME(alexyer): Dynamic cycles
            Opcode::JrZR8 => 12,
            Opcode::JrNcR8 => 12,
            // FIXME(alexyer): Dynamic cycles
            Opcode::JrNzR8 => 12,
            Opcode::JpA16 => 16,
            Opcode::JpNzA16 => 16,
            Opcode::JpHL => 4,
            Opcode::LdAD8 => 8,
            Opcode::LdBD8 => 8,
            Opcode::LdCD8 => 8,
            Opcode::LdDD8 => 8,
            Opcode::LdED8 => 8,
            Opcode::LdHD8 => 8,
            Opcode::LdLD8 => 8,
            Opcode::LdAA => 4,
            Opcode::LdAB => 4,
            Opcode::LdAC => 4,
            Opcode::LdAD => 4,
            Opcode::LdAE => 4,
            Opcode::LdAH => 4,
            Opcode::LdAL => 4,
            Opcode::LdBA => 4,
            Opcode::LdBB => 4,
            Opcode::LdBC => 4,
            Opcode::LdBD => 4,
            Opcode::LdBE => 4,
            Opcode::LdBH => 4,
            Opcode::LdBL => 4,
            Opcode::LdCA => 4,
            Opcode::LdCB => 4,
            Opcode::LdCC => 4,
            Opcode::LdCD => 4,
            Opcode::LdCE => 4,
            Opcode::LdCH => 4,
            Opcode::LdCL => 4,
            Opcode::LdDA => 4,
            Opcode::LdDB => 4,
            Opcode::LdDC => 4,
            Opcode::LdDD => 4,
            Opcode::LdDE => 4,
            Opcode::LdDH => 4,
            Opcode::LdDL => 4,
            Opcode::LdEA => 4,
            Opcode::LdEB => 4,
            Opcode::LdEC => 4,
            Opcode::LdED => 4,
            Opcode::LdEE => 4,
            Opcode::LdEH => 4,
            Opcode::LdEL => 4,
            Opcode::LdHA => 4,
            Opcode::LdHB => 4,
            Opcode::LdHC => 4,
            Opcode::LdHD => 4,
            Opcode::LdHE => 4,
            Opcode::LdHH => 4,
            Opcode::LdHL => 4,
            Opcode::LdLA => 4,
            Opcode::LdLB => 4,
            Opcode::LdLC => 4,
            Opcode::LdLD => 4,
            Opcode::LdLE => 4,
            Opcode::LdLH => 4,
            Opcode::LdLL => 4,
            Opcode::LdhAA8 => 12,
            Opcode::LdhA8A => 12,
            Opcode::PushAF => 16,
            Opcode::PushBC => 16,
            Opcode::PushDE => 16,
            Opcode::PushHL => 16,
            Opcode::PopAF => 12,
            Opcode::PopBC => 12,
            Opcode::PopDE => 12,
            Opcode::PopHL => 12,
            Opcode::LdIndDEA => 8,
            Opcode::LdIndHLA => 8,
            Opcode::LdIndHLB => 8,
            Opcode::LdIndHLC => 8,
            Opcode::LdIndHLD => 8,
            Opcode::LdIndHLE => 8,
            Opcode::LdIndHLH => 8,
            Opcode::LdIndHLL => 8,
            Opcode::LdIndHLD8 => 12,
            Opcode::LdIndHLDecA => 8,
            Opcode::LdIndHLIncA => 8,
            Opcode::LdHLSPE8 => 12,
            Opcode::LdSPHL => 8,
            Opcode::LdBCD16 => 12,
            Opcode::LdAA16 => 16,
            Opcode::LdA16SP => 20,
            Opcode::LdDED16 => 12,
            Opcode::LdSPD16 => 12,
            Opcode::LdHLD16 => 12,
            Opcode::IncA => 4,
            Opcode::IncB => 4,
            Opcode::IncC => 4,
            Opcode::IncD => 4,
            Opcode::IncE => 4,
            Opcode::IncBC => 8,
            Opcode::IncDE => 8,
            Opcode::IncHL => 8,
            Opcode::IncSP => 8,
            Opcode::IncH => 4,
            Opcode::IncL => 4,
            Opcode::Daa => 4,
            Opcode::DecA => 4,
            Opcode::DecB => 4,
            Opcode::DecC => 4,
            Opcode::DecD => 4,
            Opcode::DecE => 4,
            Opcode::DecH => 4,
            Opcode::DecL => 4,
            Opcode::DecBC => 8,
            Opcode::DecDE => 8,
            Opcode::DecHL => 8,
            Opcode::DecSP => 8,
            Opcode::DecIndHL => 12,
            Opcode::OrA => 4,
            Opcode::OrAIndHL => 8,
            Opcode::OrB => 4,
            Opcode::OrC => 4,
            Opcode::OrD8 => 8,
            Opcode::XorA => 4,
            Opcode::XorC => 4,
            Opcode::XorL => 4,
            Opcode::XorD8 => 8,
            Opcode::XorAIndHL => 8,
            Opcode::LdIndCA => 8,
            Opcode::LdAIndC => 8,
            Opcode::LdAIndDE => 8,
            Opcode::LdAIndHL => 8,
            Opcode::LdBIndHL => 8,
            Opcode::LdCIndHL => 8,
            Opcode::LdDIndHL => 8,
            Opcode::LdEIndHL => 8,
            Opcode::LdHIndHL => 8,
            Opcode::LdLIndHL => 8,
            Opcode::AdcD8 => 8,
            Opcode::AddB => 4,
            Opcode::AddD8 => 8,
            Opcode::AddHLBC => 8,
            Opcode::AddHLDE => 8,
            Opcode::AddHLHL => 8,
            Opcode::AddSPE8 => 16,
            Opcode::AddHLSP => 8,
            Opcode::AddAIndHL => 8,
            Opcode::AndD8 => 8,
            Opcode::SbcD8 => 8,
            Opcode::SubD8 => 8,
            Opcode::SubB => 4,
            Opcode::CpE => 4,
            Opcode::CpD8 => 8,
            Opcode::Cpl => 4,
            Opcode::Scf => 4,
            Opcode::CpAIndHL => 8,
            Opcode::CallA16 => 24,
            Opcode::CallNzA16 => 24,
            Opcode::RlA => 4,
            Opcode::RrA => 4,
            Opcode::Ret => 16,
            Opcode::RetC => 20,
            Opcode::RetNc => 20,
            Opcode::RetZ => 20,
            Opcode::LdA16A => 16,
            Opcode::LdAIndHLInc => 8,
        }
    }
}

impl TryFrom<&u8> for Opcode {
    type Error = InstructionError;

    fn try_from(value: &u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Opcode::Nop),
            0x01 => Ok(Opcode::LdBCD16),
            0x03 => Ok(Opcode::IncBC),
            0x04 => Ok(Opcode::IncB),
            0x05 => Ok(Opcode::DecB),
            0x06 => Ok(Opcode::LdBD8),
            0x08 => Ok(Opcode::LdA16SP),
            0x09 => Ok(Opcode::AddHLBC),
            0x0b => Ok(Opcode::DecBC),
            0x0c => Ok(Opcode::IncC),
            0x0d => Ok(Opcode::DecC),
            0x0e => Ok(Opcode::LdCD8),

            0x11 => Ok(Opcode::LdDED16),
            0x12 => Ok(Opcode::LdIndDEA),
            0x13 => Ok(Opcode::IncDE),
            0x14 => Ok(Opcode::IncD),
            0x15 => Ok(Opcode::DecD),
            0x16 => Ok(Opcode::LdDD8),
            0x17 => Ok(Opcode::RlA),
            0x18 => Ok(Opcode::JrR8),
            0x19 => Ok(Opcode::AddHLDE),
            0x1a => Ok(Opcode::LdAIndDE),
            0x1b => Ok(Opcode::DecDE),
            0x1c => Ok(Opcode::IncE),
            0x1d => Ok(Opcode::DecE),
            0x1e => Ok(Opcode::LdED8),
            0x1f => Ok(Opcode::RrA),

            0x20 => Ok(Opcode::JrNzR8),
            0x21 => Ok(Opcode::LdHLD16),
            0x22 => Ok(Opcode::LdIndHLIncA),
            0x23 => Ok(Opcode::IncHL),
            0x24 => Ok(Opcode::IncH),
            0x25 => Ok(Opcode::DecH),
            0x26 => Ok(Opcode::LdHD8),
            0x27 => Ok(Opcode::Daa),
            0x28 => Ok(Opcode::JrZR8),
            0x29 => Ok(Opcode::AddHLHL),
            0x2a => Ok(Opcode::LdAIndHLInc),
            0x2b => Ok(Opcode::DecHL),
            0x2c => Ok(Opcode::IncL),
            0x2d => Ok(Opcode::DecL),
            0x2e => Ok(Opcode::LdLD8),
            0x2f => Ok(Opcode::Cpl),

            0x30 => Ok(Opcode::JrNcR8),
            0x31 => Ok(Opcode::LdSPD16),
            0x32 => Ok(Opcode::LdIndHLDecA),
            0x33 => Ok(Opcode::IncSP),
            0x35 => Ok(Opcode::DecIndHL),
            0x36 => Ok(Opcode::LdIndHLD8),
            0x37 => Ok(Opcode::Scf),
            0x38 => Ok(Opcode::JrCR8),
            0x39 => Ok(Opcode::AddHLSP),
            0x3b => Ok(Opcode::DecSP),
            0x3c => Ok(Opcode::IncA),
            0x3d => Ok(Opcode::DecA),
            0x3e => Ok(Opcode::LdAD8),

            0x40 => Ok(Opcode::LdBB),
            0x41 => Ok(Opcode::LdBC),
            0x42 => Ok(Opcode::LdBD),
            0x43 => Ok(Opcode::LdBE),
            0x44 => Ok(Opcode::LdBH),
            0x45 => Ok(Opcode::LdBL),
            0x46 => Ok(Opcode::LdBIndHL),
            0x47 => Ok(Opcode::LdBA),
            0x48 => Ok(Opcode::LdCB),
            0x49 => Ok(Opcode::LdCC),
            0x4a => Ok(Opcode::LdCD),
            0x4b => Ok(Opcode::LdCE),
            0x4c => Ok(Opcode::LdCH),
            0x4d => Ok(Opcode::LdCL),
            0x4e => Ok(Opcode::LdCIndHL),
            0x4f => Ok(Opcode::LdCA),

            0x50 => Ok(Opcode::LdDB),
            0x51 => Ok(Opcode::LdDC),
            0x52 => Ok(Opcode::LdDD),
            0x53 => Ok(Opcode::LdDE),
            0x54 => Ok(Opcode::LdDH),
            0x55 => Ok(Opcode::LdDL),
            0x56 => Ok(Opcode::LdDIndHL),
            0x57 => Ok(Opcode::LdDA),
            0x58 => Ok(Opcode::LdEB),
            0x59 => Ok(Opcode::LdEC),
            0x5a => Ok(Opcode::LdED),
            0x5b => Ok(Opcode::LdEE),
            0x5c => Ok(Opcode::LdEH),
            0x5d => Ok(Opcode::LdEL),
            0x5e => Ok(Opcode::LdEIndHL),
            0x5f => Ok(Opcode::LdEA),

            0x60 => Ok(Opcode::LdHB),
            0x61 => Ok(Opcode::LdHC),
            0x62 => Ok(Opcode::LdHD),
            0x63 => Ok(Opcode::LdHE),
            0x64 => Ok(Opcode::LdHH),
            0x65 => Ok(Opcode::LdHL),
            0x66 => Ok(Opcode::LdHIndHL),
            0x67 => Ok(Opcode::LdHA),
            0x68 => Ok(Opcode::LdLB),
            0x69 => Ok(Opcode::LdLC),
            0x6a => Ok(Opcode::LdLD),
            0x6b => Ok(Opcode::LdLE),
            0x6c => Ok(Opcode::LdLH),
            0x6d => Ok(Opcode::LdLL),
            0x6e => Ok(Opcode::LdLIndHL),
            0x6f => Ok(Opcode::LdLA),

            0x70 => Ok(Opcode::LdIndHLB),
            0x71 => Ok(Opcode::LdIndHLC),
            0x72 => Ok(Opcode::LdIndHLD),
            0x73 => Ok(Opcode::LdIndHLE),
            0x74 => Ok(Opcode::LdIndHLH),
            0x75 => Ok(Opcode::LdIndHLL),
            0x77 => Ok(Opcode::LdIndHLA),
            0x78 => Ok(Opcode::LdAB),
            0x79 => Ok(Opcode::LdAC),
            0x7a => Ok(Opcode::LdAD),
            0x7b => Ok(Opcode::LdAE),
            0x7c => Ok(Opcode::LdAH),
            0x7d => Ok(Opcode::LdAL),
            0x7e => Ok(Opcode::LdAIndHL),
            0x7f => Ok(Opcode::LdAA),

            0x80 => Ok(Opcode::AddB),
            0x86 => Ok(Opcode::AddAIndHL),

            0x90 => Ok(Opcode::SubB),

            0xad => Ok(Opcode::XorL),
            0xa9 => Ok(Opcode::XorC),
            0xae => Ok(Opcode::XorAIndHL),
            0xaf => Ok(Opcode::XorA),

            0xb0 => Ok(Opcode::OrB),
            0xb1 => Ok(Opcode::OrC),
            0xb6 => Ok(Opcode::OrAIndHL),
            0xb7 => Ok(Opcode::OrA),
            0xbb => Ok(Opcode::CpE),
            0xbe => Ok(Opcode::CpAIndHL),

            0xc1 => Ok(Opcode::PopBC),
            0xc2 => Ok(Opcode::JpNzA16),
            0xc3 => Ok(Opcode::JpA16),
            0xc4 => Ok(Opcode::CallNzA16),
            0xc5 => Ok(Opcode::PushBC),
            0xc6 => Ok(Opcode::AddD8),
            0xc8 => Ok(Opcode::RetZ),
            0xc9 => Ok(Opcode::Ret),
            0xcb => Ok(Opcode::Prefix),
            0xce => Ok(Opcode::AdcD8),
            0xcd => Ok(Opcode::CallA16),

            0xd0 => Ok(Opcode::RetNc),
            0xd1 => Ok(Opcode::PopDE),
            0xd5 => Ok(Opcode::PushDE),
            0xd6 => Ok(Opcode::SubD8),
            0xd8 => Ok(Opcode::RetC),
            0xde => Ok(Opcode::SbcD8),

            0xe0 => Ok(Opcode::LdhA8A),
            0xe1 => Ok(Opcode::PopHL),
            0xe2 => Ok(Opcode::LdIndCA),
            0xe5 => Ok(Opcode::PushHL),
            0xe9 => Ok(Opcode::JpHL),
            0xe6 => Ok(Opcode::AndD8),
            0xe8 => Ok(Opcode::AddSPE8),
            0xea => Ok(Opcode::LdA16A),

            0xf0 => Ok(Opcode::LdhAA8),
            0xf1 => Ok(Opcode::PopAF),
            0xf2 => Ok(Opcode::LdAIndC),
            0xf3 => Ok(Opcode::Di),
            0xf5 => Ok(Opcode::PushAF),
            0xf6 => Ok(Opcode::OrD8),
            0xf8 => Ok(Opcode::LdHLSPE8),
            0xf9 => Ok(Opcode::LdSPHL),
            0xfa => Ok(Opcode::LdAA16),
            0xfb => Ok(Opcode::Ei),
            0xfe => Ok(Opcode::CpD8),

            0xee => Ok(Opcode::XorD8),
            _ => Err(InstructionError::UnrecognizedOpcode(*value)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixedOpcode {
    Bit0D,
    Bit7H,
    RlC,
    RrC,
    RrD,
    RrE,
    SrlB,
    SwapA,
}

impl PrefixedOpcode {
    pub fn data_len(&self) -> usize {
        match self {
            PrefixedOpcode::Bit0D => 0,
            PrefixedOpcode::Bit7H => 0,
            PrefixedOpcode::RlC => 0,
            PrefixedOpcode::RrC => 0,
            PrefixedOpcode::RrD => 0,
            PrefixedOpcode::RrE => 0,
            PrefixedOpcode::SrlB => 0,
            PrefixedOpcode::SwapA => 0,
        }
    }

    pub fn cycles(&self) -> usize {
        match self {
            PrefixedOpcode::Bit0D => 8,
            PrefixedOpcode::Bit7H => 8,
            PrefixedOpcode::RlC => 8,
            PrefixedOpcode::RrC => 8,
            PrefixedOpcode::RrD => 8,
            PrefixedOpcode::RrE => 8,
            PrefixedOpcode::SrlB => 8,
            PrefixedOpcode::SwapA => 8,
        }
    }
}

impl TryFrom<&u8> for PrefixedOpcode {
    type Error = InstructionError;
    fn try_from(value: &u8) -> Result<Self, Self::Error> {
        match value {
            0x11 => Ok(PrefixedOpcode::RlC),
            0x19 => Ok(PrefixedOpcode::RrC),
            0x1a => Ok(PrefixedOpcode::RrD),
            0x1b => Ok(PrefixedOpcode::RrE),

            0x37 => Ok(PrefixedOpcode::SwapA),
            0x38 => Ok(PrefixedOpcode::SrlB),

            0x42 => Ok(PrefixedOpcode::Bit0D),

            0x7c => Ok(PrefixedOpcode::Bit7H),

            _ => Err(InstructionError::UnrecognizedPrefixedOpcode(*value)),
        }
    }
}
