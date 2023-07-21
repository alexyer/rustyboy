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

    impl_instruction_constructor!(ld_bc_d16, Opcode::LdBCD16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_de_d16, Opcode::LdDED16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_sp_d16, Opcode::LdSPD16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_hl_d16, Opcode::LdHLD16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_d8, Opcode::LdAD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_a16, Opcode::LdAA16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_b_d8, Opcode::LdBD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_c_d8, Opcode::LdCD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_d_d8, Opcode::LdDD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_e_d8, Opcode::LdED8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_h_d8, Opcode::LdHD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_l_d8, Opcode::LdLD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_b, Opcode::LdAB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_c, Opcode::LdAC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_d, Opcode::LdAD, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_e, Opcode::LdAE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_h, Opcode::LdAH, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_l, Opcode::LdAL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_b_a, Opcode::LdBA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_c_a, Opcode::LdCA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_d_a, Opcode::LdDA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_d_l, Opcode::LdDL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_e_a, Opcode::LdEA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_e_l, Opcode::LdEL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_h_a, Opcode::LdHA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_h_d, Opcode::LdHD, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_l_a, Opcode::LdLA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_l_e, Opcode::LdLE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_a_a8, Opcode::LdhAA8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_a8_a, Opcode::LdhA8A, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_ind_c, Opcode::LdAIndC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_a_ind_de, Opcode::LdAIndDE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_a_ind_hl, Opcode::LdAIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_b_ind_hl, Opcode::LdBIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_c_ind_hl, Opcode::LdCIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_d_ind_hl, Opcode::LdDIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_e_ind_hl, Opcode::LdEIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_h_ind_hl, Opcode::LdHIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ldh_l_ind_hl, Opcode::LdLIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(push_af, Opcode::PushAF, None::<PrefixedOpcode>);
    impl_instruction_constructor!(push_bc, Opcode::PushBC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(push_de, Opcode::PushDE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(push_hl, Opcode::PushHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(pop_af, Opcode::PopAF, None::<PrefixedOpcode>);
    impl_instruction_constructor!(pop_bc, Opcode::PopBC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(pop_de, Opcode::PopDE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(pop_hl, Opcode::PopHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_de_a, Opcode::LdIndDEA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_a, Opcode::LdIndHLA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_b, Opcode::LdIndHLB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_c, Opcode::LdIndHLC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_d, Opcode::LdIndHLD, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_e, Opcode::LdIndHLE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_d8, Opcode::LdIndHLD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_dec_a, Opcode::LdIndHLDecA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_hl_inc_a, Opcode::LdIndHLIncA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a_ind_hl_inc, Opcode::LdAIndHLInc, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_ind_c_a, Opcode::LdIndCA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_hl_sp_e8, Opcode::LdHLSPE8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_sp_hl, Opcode::LdSPHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_a, Opcode::IncA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_b, Opcode::IncB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_c, Opcode::IncC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_d, Opcode::IncD, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_e, Opcode::IncE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_bc, Opcode::IncBC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_de, Opcode::IncDE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_hl, Opcode::IncHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_sp, Opcode::IncSP, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_h, Opcode::IncH, None::<PrefixedOpcode>);
    impl_instruction_constructor!(inc_l, Opcode::IncL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(daa, Opcode::Daa, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_a, Opcode::DecA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_b, Opcode::DecB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_c, Opcode::DecC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_d, Opcode::DecD, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_e, Opcode::DecE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_h, Opcode::DecH, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_l, Opcode::DecL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_de, Opcode::DecDE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_sp, Opcode::DecSP, None::<PrefixedOpcode>);
    impl_instruction_constructor!(dec_ind_hl, Opcode::DecIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(adc_a_d8, Opcode::AdcAD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(add_a_b, Opcode::AddAB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(add_a_d8, Opcode::AddAD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(add_hl_hl, Opcode::AddHLHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(add_hl_sp, Opcode::AddHLSP, None::<PrefixedOpcode>);
    impl_instruction_constructor!(add_sp_e8, Opcode::AddSPE8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(add_a_ind_hl, Opcode::AddAIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(and_a_d8, Opcode::AndAD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(sbc_d8, Opcode::SbcD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(sub_d8, Opcode::SubD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(sub_a_b, Opcode::SubAB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(or_a, Opcode::OrA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(or_b, Opcode::OrB, None::<PrefixedOpcode>);
    impl_instruction_constructor!(or_a_ind_hl, Opcode::OrAIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(or_c, Opcode::OrC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(or_d8, Opcode::OrD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(xor_a, Opcode::XorA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(xor_a_c, Opcode::XorAC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(xor_a_l, Opcode::XorAL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(xor_a_d8, Opcode::XorAD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(xor_a_ind_hl, Opcode::XorAIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(cp_a_e, Opcode::CpAE, None::<PrefixedOpcode>);
    impl_instruction_constructor!(cp_d8, Opcode::CpD8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(cp_a_ind_hl, Opcode::CpAIndHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(cpl, Opcode::Cpl, None::<PrefixedOpcode>);
    impl_instruction_constructor!(scf, Opcode::Scf, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jr_r8, Opcode::JrR8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jr_c_r8, Opcode::JrCR8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jr_nc_r8, Opcode::JrNcR8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jr_nz_r8, Opcode::JrNzR8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jr_z_r8, Opcode::JrZR8, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jp_a16, Opcode::JpA16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jp_nz_a16, Opcode::JpNzA16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(jp_hl, Opcode::JpHL, None::<PrefixedOpcode>);
    impl_instruction_constructor!(call_a16, Opcode::CallA16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(call_nz_a16, Opcode::CallNzA16, None::<PrefixedOpcode>);
    impl_instruction_constructor!(di, Opcode::Di, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ei, Opcode::Ei, None::<PrefixedOpcode>);
    impl_instruction_constructor!(bit0d, Opcode::Prefix, Some(PrefixedOpcode::Bit0D));
    impl_instruction_constructor!(bit7h, Opcode::Prefix, Some(PrefixedOpcode::Bit7H));
    impl_instruction_constructor!(rl_c, Opcode::Prefix, Some(PrefixedOpcode::RlC));
    impl_instruction_constructor!(rr_c, Opcode::Prefix, Some(PrefixedOpcode::RrC));
    impl_instruction_constructor!(rr_d, Opcode::Prefix, Some(PrefixedOpcode::RrD));
    impl_instruction_constructor!(rr_e, Opcode::Prefix, Some(PrefixedOpcode::RrE));
    impl_instruction_constructor!(srl_b, Opcode::Prefix, Some(PrefixedOpcode::SrlB));
    impl_instruction_constructor!(swap_a, Opcode::Prefix, Some(PrefixedOpcode::SwapA));
    impl_instruction_constructor!(rl_a, Opcode::RlA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(rr_a, Opcode::RrA, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ret, Opcode::Ret, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ret_c, Opcode::RetC, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ret_nc, Opcode::RetNc, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ret_z, Opcode::RetZ, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a16_a, Opcode::LdA16A, None::<PrefixedOpcode>);
    impl_instruction_constructor!(ld_a16_sp, Opcode::LdA16SP, None::<PrefixedOpcode>);
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
    LdAB,
    LdAC,
    LdAD,
    LdAE,
    LdAL,
    LdAH,
    LdBA,
    LdCA,
    LdDA,
    LdDL,
    LdEA,
    LdEL,
    LdHA,
    LdHD,
    LdLA,
    LdLE,
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
    AdcAD8,
    AddAB,
    AddAIndHL,
    AddAD8,
    AddHLHL,
    AddHLSP,
    AddSPE8,
    AndAD8,
    SbcD8,
    SubD8,
    SubAB,
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
    DecDE,
    DecSP,
    DecIndHL,
    OrA,
    OrAIndHL,
    OrB,
    OrC,
    OrD8,
    XorA,
    XorAC,
    XorAL,
    XorAD8,
    XorAIndHL,
    CpAE,
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
            Opcode::LdAB => 0,
            Opcode::LdAC => 0,
            Opcode::LdAD => 0,
            Opcode::LdAE => 0,
            Opcode::LdAH => 0,
            Opcode::LdAL => 0,
            Opcode::LdBA => 0,
            Opcode::LdCA => 0,
            Opcode::LdDA => 0,
            Opcode::LdDL => 0,
            Opcode::LdEA => 0,
            Opcode::LdEL => 0,
            Opcode::LdHA => 0,
            Opcode::LdHD => 0,
            Opcode::LdLA => 0,
            Opcode::LdLE => 0,
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
            Opcode::LdIndHLD8 => 1,
            Opcode::LdIndHLDecA => 0,
            Opcode::LdIndHLIncA => 0,
            Opcode::LdBCD16 => 2,
            Opcode::LdAA16 => 2,
            Opcode::LdA16SP => 2,
            Opcode::LdDED16 => 2,
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
            Opcode::DecDE => 0,
            Opcode::DecSP => 0,
            Opcode::DecIndHL => 0,
            Opcode::OrA => 0,
            Opcode::OrAIndHL => 0,
            Opcode::OrB => 0,
            Opcode::OrC => 0,
            Opcode::OrD8 => 1,
            Opcode::XorA => 0,
            Opcode::XorAC => 0,
            Opcode::XorAL => 0,
            Opcode::XorAD8 => 1,
            Opcode::XorAIndHL => 0,
            Opcode::LdIndCA => 0,
            Opcode::AdcAD8 => 1,
            Opcode::AddAB => 0,
            Opcode::AddAD8 => 1,
            Opcode::AddHLHL => 0,
            Opcode::AddHLSP => 0,
            Opcode::AddSPE8 => 1,
            Opcode::AddAIndHL => 0,
            Opcode::AndAD8 => 1,
            Opcode::SbcD8 => 1,
            Opcode::SubD8 => 1,
            Opcode::SubAB => 0,
            Opcode::CpAE => 0,
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
            Opcode::LdAB => 4,
            Opcode::LdAC => 4,
            Opcode::LdAD => 4,
            Opcode::LdAE => 4,
            Opcode::LdAH => 4,
            Opcode::LdAL => 4,
            Opcode::LdBA => 4,
            Opcode::LdCA => 4,
            Opcode::LdDA => 4,
            Opcode::LdDL => 4,
            Opcode::LdEA => 4,
            Opcode::LdEL => 4,
            Opcode::LdHA => 4,
            Opcode::LdHD => 4,
            Opcode::LdLA => 4,
            Opcode::LdLE => 4,
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
            Opcode::DecDE => 8,
            Opcode::DecSP => 8,
            Opcode::DecIndHL => 12,
            Opcode::OrA => 4,
            Opcode::OrAIndHL => 8,
            Opcode::OrB => 4,
            Opcode::OrC => 4,
            Opcode::OrD8 => 8,
            Opcode::XorA => 4,
            Opcode::XorAC => 4,
            Opcode::XorAL => 4,
            Opcode::XorAD8 => 8,
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
            Opcode::AdcAD8 => 8,
            Opcode::AddAB => 4,
            Opcode::AddAD8 => 8,
            Opcode::AddHLHL => 8,
            Opcode::AddSPE8 => 16,
            Opcode::AddHLSP => 8,
            Opcode::AddAIndHL => 8,
            Opcode::AndAD8 => 8,
            Opcode::SbcD8 => 8,
            Opcode::SubD8 => 8,
            Opcode::SubAB => 4,
            Opcode::CpAE => 4,
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

            0x46 => Ok(Opcode::LdBIndHL),
            0x47 => Ok(Opcode::LdBA),
            0x4e => Ok(Opcode::LdCIndHL),
            0x4f => Ok(Opcode::LdCA),

            0x5d => Ok(Opcode::LdEL),
            0x5e => Ok(Opcode::LdEIndHL),
            0x55 => Ok(Opcode::LdDL),
            0x56 => Ok(Opcode::LdDIndHL),
            0x57 => Ok(Opcode::LdDA),
            0x5f => Ok(Opcode::LdEA),

            0x62 => Ok(Opcode::LdHD),
            0x66 => Ok(Opcode::LdHIndHL),
            0x67 => Ok(Opcode::LdHA),
            0x6b => Ok(Opcode::LdLE),
            0x6e => Ok(Opcode::LdLIndHL),
            0x6f => Ok(Opcode::LdLA),

            0x70 => Ok(Opcode::LdIndHLB),
            0x71 => Ok(Opcode::LdIndHLC),
            0x72 => Ok(Opcode::LdIndHLD),
            0x73 => Ok(Opcode::LdIndHLE),
            0x77 => Ok(Opcode::LdIndHLA),
            0x78 => Ok(Opcode::LdAB),
            0x79 => Ok(Opcode::LdAC),
            0x7a => Ok(Opcode::LdAD),
            0x7b => Ok(Opcode::LdAE),
            0x7c => Ok(Opcode::LdAH),
            0x7d => Ok(Opcode::LdAL),
            0x7e => Ok(Opcode::LdAIndHL),

            0x80 => Ok(Opcode::AddAB),
            0x86 => Ok(Opcode::AddAIndHL),

            0x90 => Ok(Opcode::SubAB),

            0xad => Ok(Opcode::XorAL),
            0xa9 => Ok(Opcode::XorAC),
            0xae => Ok(Opcode::XorAIndHL),
            0xaf => Ok(Opcode::XorA),

            0xb0 => Ok(Opcode::OrB),
            0xb1 => Ok(Opcode::OrC),
            0xb6 => Ok(Opcode::OrAIndHL),
            0xb7 => Ok(Opcode::OrA),
            0xbb => Ok(Opcode::CpAE),
            0xbe => Ok(Opcode::CpAIndHL),

            0xc1 => Ok(Opcode::PopBC),
            0xc2 => Ok(Opcode::JpNzA16),
            0xc3 => Ok(Opcode::JpA16),
            0xc4 => Ok(Opcode::CallNzA16),
            0xc5 => Ok(Opcode::PushBC),
            0xc6 => Ok(Opcode::AddAD8),
            0xc8 => Ok(Opcode::RetZ),
            0xc9 => Ok(Opcode::Ret),
            0xcb => Ok(Opcode::Prefix),
            0xce => Ok(Opcode::AdcAD8),
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
            0xe6 => Ok(Opcode::AndAD8),
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

            0xee => Ok(Opcode::XorAD8),
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
