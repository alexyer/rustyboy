use crate::errors::InstructionError;

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
    ($fun:ident, $opcode:expr, $prefixed_opcode:expr) => {
        pub fn $fun(data: &[u8]) -> Self {
            let opcode = $opcode;
            let prefixed_opcode = $prefixed_opcode;

            check_data!(data, opcode, prefixed_opcode);
            Self {
                opcode,
                prefixed_opcode,
                data: data.into(),
            }
        }
    };
}

#[derive(Debug)]
pub struct Instruction {
    opcode: Opcode,
    prefixed_opcode: Option<PrefixedOpcode>,
    data: Vec<u8>,
}

impl Instruction {
    pub fn opcode(&self) -> &Opcode {
        &self.opcode
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

    pub fn nop() -> Self {
        Self {
            opcode: Opcode::Nop,
            prefixed_opcode: None,
            data: vec![],
        }
    }

    impl_instruction_constructor!(ld_de_d16, Opcode::LdDeD16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_sp_d16, Opcode::LdSpD16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_hl_d16, Opcode::LdHlD16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_d8, Opcode::LdAD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_b_d8, Opcode::LdBD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_c_d8, Opcode::LdCD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_d_d8, Opcode::LdDD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_e_d8, Opcode::LdED8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_l_d8, Opcode::LdLD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_e, Opcode::LdAE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_l, Opcode::LdAL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_b_a, Opcode::LdBA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_c_a, Opcode::LdCA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_d_a, Opcode::LdDA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_d_l, Opcode::LdDL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_h_a, Opcode::LdHA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_a_a8, Opcode::LdhAA8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_a8_a, Opcode::LdhA8A, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_a_ind_de, Opcode::LdAIndDE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(push_af, Opcode::PushAF, None::<PrefixedOpcode>);
    impl_instruction_constructor!(push_bc, Opcode::PushBC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(pop_af, Opcode::PopAF, None::<PrefixedOpcode>);
    impl_instruction_constructor!(pop_bc, Opcode::PopBC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_a, Opcode::LdIndHLA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_dec_a, Opcode::LdIndHLDecA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_inc_a, Opcode::LdIndHLIncA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_ind_hl_inc, Opcode::LdAIndHLInc, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_c_a, Opcode::LdIndCA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_b, Opcode::IncB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_c, Opcode::IncC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_de, Opcode::IncDE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_hl, Opcode::IncHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_a, Opcode::DecA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_b, Opcode::DecB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_c, Opcode::DecC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_d, Opcode::DecD, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_e, Opcode::DecE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_de, Opcode::DecDE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(add_a_b, Opcode::AddAB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(sub_d8, Opcode::SubD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(xor_a, Opcode::XorA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(cp_d8, Opcode::CpD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jr_r8, Opcode::JrR8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jr_nz_r8, Opcode::JrNzR8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jr_z_r8, Opcode::JrZR8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(call_a16, Opcode::CallA16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(bit0d, Opcode::Prefix, Some(PrefixedOpcode::Bit0D));
    impl_instruction_constructor!(bit7h, Opcode::Prefix, Some(PrefixedOpcode::Bit7H));
    impl_instruction_constructor!(rl_c, Opcode::Prefix, Some(PrefixedOpcode::RlC));
    impl_instruction_constructor!(rr_d, Opcode::Prefix, Some(PrefixedOpcode::RrD));
    impl_instruction_constructor!(rl_a, Opcode::RlA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ret, Opcode::Ret, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a16_a, Opcode::LdA16A, None::<PrefixedOpcode>);
}

#[derive(Debug)]
pub enum Opcode {
    // Misc / control instructions
    Nop,
    Prefix,

    // Jumps / Calls
    JrR8,
    JrNzR8,
    JrZR8,
    CallA16,
    Ret,

    // 8-bit load instructions
    LdAD8,
    LdBD8,
    LdCD8,
    LdDD8,
    LdED8,
    LdLD8,
    LdAE,
    LdAL,
    LdBA,
    LdCA,
    LdDA,
    LdDL,
    LdHA,
    LdhA8A,
    LdhAA8,
    LdAIndDE,
    LdIndHLA,
    LdAIndHLInc,
    LdIndHLDecA,
    LdIndHLIncA,
    LdIndCA,
    PushAF,
    PushBC,
    PopAF,
    PopBC,
    LdA16A,

    // 16-bit load instructions
    LdDeD16,
    LdHlD16,
    LdSpD16,

    // 8-bit arithmetic and logical instructions
    AddAB,
    SubD8,
    IncB,
    IncC,
    IncDE,
    IncHL,
    DecA,
    DecB,
    DecC,
    DecD,
    DecE,
    DecDE,
    XorA,
    CpD8,
    RlA,
}

impl Opcode {
    /// Instruction data length in bytes.
    pub fn data_len(&self) -> usize {
        match self {
            Opcode::Nop => 0,
            Opcode::Prefix => 0,
            Opcode::JrR8 => 1,
            Opcode::JrNzR8 => 1,
            Opcode::JrZR8 => 1,
            Opcode::LdAD8 => 1,
            Opcode::LdBD8 => 1,
            Opcode::LdCD8 => 1,
            Opcode::LdDD8 => 1,
            Opcode::LdED8 => 1,
            Opcode::LdLD8 => 1,
            Opcode::LdAE => 0,
            Opcode::LdAL => 0,
            Opcode::LdBA => 0,
            Opcode::LdCA => 0,
            Opcode::LdDA => 0,
            Opcode::LdDL => 0,
            Opcode::LdHA => 0,
            Opcode::LdhAA8 => 1,
            Opcode::LdhA8A => 1,
            Opcode::PushAF => 0,
            Opcode::PushBC => 0,
            Opcode::PopAF => 0,
            Opcode::PopBC => 0,
            Opcode::LdAIndDE => 0,
            Opcode::LdIndHLA => 0,
            Opcode::LdIndHLDecA => 0,
            Opcode::LdIndHLIncA => 0,
            Opcode::LdDeD16 => 2,
            Opcode::LdSpD16 => 2,
            Opcode::LdHlD16 => 2,
            Opcode::IncB => 0,
            Opcode::IncC => 0,
            Opcode::IncDE => 0,
            Opcode::IncHL => 0,
            Opcode::DecA => 0,
            Opcode::DecB => 0,
            Opcode::DecC => 0,
            Opcode::DecD => 0,
            Opcode::DecE => 0,
            Opcode::DecDE => 0,
            Opcode::XorA => 0,
            Opcode::LdIndCA => 0,
            Opcode::AddAB => 0,
            Opcode::SubD8 => 1,
            Opcode::CpD8 => 1,
            Opcode::CallA16 => 2,
            Opcode::RlA => 0,
            Opcode::Ret => 0,
            Opcode::LdA16A => 2,
            Opcode::LdAIndHLInc => 0,
        }
    }

    /// Instruction duration in T-states.
    pub fn cycles(&self) -> usize {
        match self {
            Opcode::Nop => 4,
            Opcode::Prefix => 4,
            Opcode::JrR8 => 12,
            // FIXME(alexyer): Dynamic cycles
            Opcode::JrZR8 => 12,
            // FIXME(alexyer): Dynamic cycles
            Opcode::JrNzR8 => 12,
            Opcode::LdAD8 => 8,
            Opcode::LdBD8 => 8,
            Opcode::LdCD8 => 8,
            Opcode::LdDD8 => 8,
            Opcode::LdED8 => 8,
            Opcode::LdLD8 => 8,
            Opcode::LdAE => 4,
            Opcode::LdAL => 4,
            Opcode::LdBA => 4,
            Opcode::LdCA => 4,
            Opcode::LdDA => 4,
            Opcode::LdDL => 4,
            Opcode::LdHA => 4,
            Opcode::LdhAA8 => 12,
            Opcode::LdhA8A => 12,
            Opcode::PushAF => 16,
            Opcode::PushBC => 16,
            Opcode::PopAF => 12,
            Opcode::PopBC => 12,
            Opcode::LdIndHLA => 8,
            Opcode::LdIndHLDecA => 8,
            Opcode::LdIndHLIncA => 8,
            Opcode::LdDeD16 => 12,
            Opcode::LdSpD16 => 12,
            Opcode::LdHlD16 => 12,
            Opcode::IncB => 4,
            Opcode::IncC => 4,
            Opcode::IncDE => 8,
            Opcode::IncHL => 8,
            Opcode::DecA => 4,
            Opcode::DecB => 4,
            Opcode::DecC => 4,
            Opcode::DecD => 4,
            Opcode::DecE => 4,
            Opcode::DecDE => 8,
            Opcode::XorA => 4,
            Opcode::LdIndCA => 8,
            Opcode::LdAIndDE => 8,
            Opcode::AddAB => 4,
            Opcode::SubD8 => 8,
            Opcode::CpD8 => 8,
            Opcode::CallA16 => 24,
            Opcode::RlA => 4,
            Opcode::Ret => 16,
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
            0x04 => Ok(Opcode::IncB),
            0x05 => Ok(Opcode::DecB),
            0x06 => Ok(Opcode::LdBD8),
            0x0c => Ok(Opcode::IncC),
            0x0d => Ok(Opcode::DecC),
            0x0e => Ok(Opcode::LdCD8),
            0x11 => Ok(Opcode::LdDeD16),
            0x13 => Ok(Opcode::IncDE),
            0x15 => Ok(Opcode::DecD),
            0x16 => Ok(Opcode::LdDD8),
            0x17 => Ok(Opcode::RlA),
            0x18 => Ok(Opcode::JrR8),
            0x1a => Ok(Opcode::LdAIndDE),
            0x1b => Ok(Opcode::DecDE),
            0x1d => Ok(Opcode::DecE),
            0x1e => Ok(Opcode::LdED8),
            0x20 => Ok(Opcode::JrNzR8),
            0x21 => Ok(Opcode::LdHlD16),
            0x22 => Ok(Opcode::LdIndHLIncA),
            0x23 => Ok(Opcode::IncHL),
            0x28 => Ok(Opcode::JrZR8),
            0x2a => Ok(Opcode::LdAIndHLInc),
            0x2e => Ok(Opcode::LdLD8),
            0x31 => Ok(Opcode::LdSpD16),
            0x32 => Ok(Opcode::LdIndHLDecA),
            0x3d => Ok(Opcode::DecA),
            0x3e => Ok(Opcode::LdAD8),
            0x47 => Ok(Opcode::LdBA),
            0x55 => Ok(Opcode::LdDL),
            0x57 => Ok(Opcode::LdDA),
            0x4f => Ok(Opcode::LdCA),
            0x67 => Ok(Opcode::LdHA),
            0x7b => Ok(Opcode::LdAE),
            0x77 => Ok(Opcode::LdIndHLA),
            0x7d => Ok(Opcode::LdAL),
            0x80 => Ok(Opcode::AddAB),
            0xaf => Ok(Opcode::XorA),
            0xc1 => Ok(Opcode::PopBC),
            0xc5 => Ok(Opcode::PushBC),
            0xc9 => Ok(Opcode::Ret),
            0xcb => Ok(Opcode::Prefix),
            0xcd => Ok(Opcode::CallA16),
            0xd6 => Ok(Opcode::SubD8),
            0xe0 => Ok(Opcode::LdhA8A),
            0xe2 => Ok(Opcode::LdIndCA),
            0xea => Ok(Opcode::LdA16A),
            0xf0 => Ok(Opcode::LdhAA8),
            0xf1 => Ok(Opcode::PopAF),
            0xf5 => Ok(Opcode::PushAF),
            0xfe => Ok(Opcode::CpD8),
            _ => Err(InstructionError::UnrecognizedOpcode(*value)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixedOpcode {
    Bit0D,
    Bit7H,
    RlC,
    RrD,
}

impl PrefixedOpcode {
    pub fn data_len(&self) -> usize {
        match self {
            PrefixedOpcode::Bit0D => 0,
            PrefixedOpcode::Bit7H => 0,
            PrefixedOpcode::RlC => 0,
            PrefixedOpcode::RrD => 0,
        }
    }

    pub fn cycles(&self) -> usize {
        match self {
            PrefixedOpcode::Bit0D => 8,
            PrefixedOpcode::Bit7H => 8,
            PrefixedOpcode::RlC => 8,
            PrefixedOpcode::RrD => 8,
        }
    }
}

impl TryFrom<&u8> for PrefixedOpcode {
    type Error = InstructionError;
    fn try_from(value: &u8) -> Result<Self, Self::Error> {
        match value {
            0x11 => Ok(PrefixedOpcode::RlC),
            0x1a => Ok(PrefixedOpcode::RrD),
            0x42 => Ok(PrefixedOpcode::Bit0D),
            0x7c => Ok(PrefixedOpcode::Bit7H),
            _ => Err(InstructionError::UnrecognizedPrefixedOpcode(*value)),
        }
    }
}
