use num_enum::TryFromPrimitive;
use rustyboy_instruction_derive::InstructionImpl;

use crate::errors::InstructionError;

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
    LdRIndRr,
    LdIndRrR,
    LdRIndHlDec,
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
    IncIndHl,
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
    Bit1R,
    Bit2R,
    Bit3R,
    Bit4R,
    Bit5R,
    Bit6R,
    Bit7R,
    Res0R,
    Res1R,
    Res2R,
    Res3R,
    Res4R,
    Res5R,
    Res6R,
    Res7R,
    RlR,
    Rlca,
    RlcR,
    RrR,
    Rrca,
    RrcR,
    Rla,
    Rra,
    Rst,
    SlaR,
    SraR,
    SrlR,
    SwapR,
    Halt,
}

pub enum Instruction {
    Normal(NormalInstruction),
    Prefixed(PrefixedInstruction),
}

impl From<NormalInstruction> for Instruction {
    fn from(value: NormalInstruction) -> Self {
        Instruction::Normal(value)
    }
}

impl From<PrefixedInstruction> for Instruction {
    fn from(value: PrefixedInstruction) -> Self {
        Instruction::Prefixed(value)
    }
}

#[derive(Debug, Clone, Copy, InstructionImpl, TryFromPrimitive)]
#[num_enum(error_type(name = InstructionError, constructor = InstructionError::UnrecognizedOpcode))]
#[instruction_struct_name(NormalInstruction)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum Opcode {
    #[instruction(instruction_type = "Nop", len = 0, cycles = [4])]
    NOP = 0x00,

    #[instruction(regs = ["BC"], instruction_type = "LdRrD16", len = 2, cycles = [12])]
    LD_BC_D16 = 0x01,

    #[instruction(regs = ["BC", "A"], instruction_type = "LdIndRrR", len = 0, cycles = [8])]
    LD_IND_BC_A = 0x02,

    #[instruction(regs = ["BC"], instruction_type = "IncRr", len = 0, cycles = [8])]
    INC_BC = 0x03,

    #[instruction(regs = ["B"], instruction_type = "IncR", len = 0, cycles = [4])]
    INC_B = 0x04,

    #[instruction(regs = ["B"], instruction_type = "DecR", len = 0, cycles = [4])]
    DEC_B = 0x05,

    #[instruction(regs = ["B"], instruction_type = "LdRD8", len = 1, cycles = [8])]
    LD_B_D8 = 0x06,

    #[instruction(regs = ["A"], instruction_type = "Rlca", len = 0, cycles = [4])]
    RLCA = 0x07,

    #[instruction(instruction_type = "LdA16Sp", len = 2, cycles = [20])]
    LD_A16_SP = 0x08,

    #[instruction(regs = ["HL", "BC"], instruction_type = "AddRrRr", len = 0, cycles = [8])]
    ADD_HL_BC = 0x09,

    #[instruction(regs = ["A", "BC"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_A_IND_BC = 0x0a,

    // TODO(alexyer): Proper stop implementation
    #[instruction(instruction_type = "Halt", len = 0, cycles = [4])]
    STOP = 0x10,

    #[instruction(regs = ["BC"], instruction_type = "DecRr", len = 0, cycles = [8])]
    DEC_BC = 0x0b,

    #[instruction(regs = ["C"], instruction_type = "IncR", len = 0, cycles = [4])]
    INC_C = 0x0c,

    #[instruction(regs = ["C"], instruction_type = "DecR", len = 0, cycles = [4])]
    DEC_C = 0x0d,

    #[instruction(regs = ["C"], instruction_type = "LdRD8", len = 1, cycles = [8])]
    LD_C_D8 = 0x0e,

    #[instruction(regs = ["A"], instruction_type = "Rrca", len = 0, cycles = [4])]
    RRCA = 0x0f,

    #[instruction(regs = ["DE"], instruction_type = "LdRrD16", len = 2, cycles = [12])]
    LD_DE_D16 = 0x11,

    #[instruction(regs = ["DE", "A"], instruction_type = "LdIndRrR", len = 0, cycles = [8])]
    LD_IND_DE_A = 0x12,

    #[instruction(regs = ["DE"], instruction_type = "IncRr", len = 0, cycles = [8])]
    INC_DE = 0x13,

    #[instruction(regs = ["D"], instruction_type = "IncR", len = 0, cycles = [4])]
    INC_D = 0x14,

    #[instruction(regs = ["D"], instruction_type = "DecR", len = 0, cycles = [4])]
    DEC_D = 0x15,

    #[instruction(regs = ["D"], instruction_type = "LdRD8", len = 1, cycles = [8])]
    LD_D_D8 = 0x16,

    #[instruction(regs = ["A"], instruction_type = "Rla", len = 0, cycles = [4])]
    RLA = 0x17,

    #[instruction(regs = [], instruction_type = "Jr", len = 1, cycles = [12])]
    JR_R8 = 0x18,

    #[instruction(regs = ["HL", "DE"], instruction_type = "AddRrRr", len = 0, cycles = [8])]
    ADD_HL_DE = 0x19,

    #[instruction(regs = ["A", "DE"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_A_IND_DE = 0x1a,

    #[instruction(regs = ["DE"], instruction_type = "DecRr", len = 0, cycles = [8])]
    DEC_DE = 0x1b,

    #[instruction(regs = ["E"], instruction_type = "IncR", len = 0, cycles = [4])]
    INC_E = 0x1c,

    #[instruction(regs = ["E"], instruction_type = "DecR", len = 0, cycles = [4])]
    DEC_E = 0x1d,

    #[instruction(regs = ["E"], instruction_type = "LdRD8", len = 1, cycles = [8])]
    LD_E_D8 = 0x1e,

    #[instruction(regs = ["A"], instruction_type = "Rra", len = 0, cycles = [4])]
    RRA = 0x1f,

    #[instruction(instruction_type = "Jr", len = 1, cycles = [12, 8])]
    JR_NZ_R8 = 0x20,

    #[instruction(regs = ["HL"], instruction_type = "LdRrD16", len = 2, cycles = [12])]
    LD_HL_D16 = 0x21,

    #[instruction(regs = ["HL", "A"], instruction_type = "LdIndHLIncA", len = 0, cycles = [8])]
    LD_IND_HL_INC_A = 0x22,

    #[instruction(regs = ["HL"], instruction_type = "IncRr", len = 0, cycles = [8])]
    INC_HL = 0x23,

    #[instruction(regs = ["H"], instruction_type = "IncR", len = 0, cycles = [4])]
    INC_H = 0x24,

    #[instruction(regs = ["H"], instruction_type = "DecR", len = 0, cycles = [4])]
    DEC_H = 0x25,

    #[instruction(regs = ["H"], instruction_type = "LdRD8", len = 1, cycles = [8])]
    LD_H_D8 = 0x26,

    #[instruction(instruction_type = "Daa", len = 0, cycles = [4])]
    DAA = 0x27,

    #[instruction(instruction_type = "Jr", len = 1, cycles = [12, 8])]
    JR_Z_R8 = 0x28,

    #[instruction(regs = ["HL", "HL"], instruction_type = "AddRrRr", len = 0, cycles = [8])]
    ADD_HL_HL = 0x29,

    #[instruction(regs = ["A", "HL"], instruction_type = "LdAIndHLInc", len = 0, cycles = [8])]
    LD_A_IND_HL_INC = 0x2a,

    #[instruction(regs = ["HL"], instruction_type = "DecRr", len = 0, cycles = [8])]
    DEC_HL = 0x2b,

    #[instruction(regs = ["L"], instruction_type = "IncR", len = 0, cycles = [4])]
    INC_L = 0x2c,

    #[instruction(regs = ["L"], instruction_type = "DecR", len = 0, cycles = [4])]
    DEC_L = 0x2d,

    #[instruction(regs = ["L"], instruction_type = "LdRD8", len = 1, cycles = [8])]
    LD_L_D8 = 0x2e,

    #[instruction(instruction_type = "Cpl", len = 0, cycles = [4])]
    CPL = 0x2f,

    #[instruction(instruction_type = "Jr", len = 1, cycles = [12, 8])]
    JR_NC_R8 = 0x30,

    #[instruction(regs = ["SP"], instruction_type = "LdRrD16", len = 2, cycles = [12])]
    LD_SP_D16 = 0x31,

    #[instruction(regs = ["HL", "A"], instruction_type = "LdIndHlDecA", len = 0, cycles = [8])]
    LD_IND_HL_DEC_A = 0x32,

    #[instruction(regs = ["SP"], instruction_type = "IncRr", len = 0, cycles = [8])]
    INC_SP = 0x33,

    #[instruction(regs = ["HL"], instruction_type = "IncIndHl", len = 0, cycles = [12])]
    INC_IND_HL = 0x34,

    #[instruction(regs = ["HL"], instruction_type = "DecIndHl", len = 0, cycles = [12])]
    DEC_IND_HL = 0x35,

    #[instruction(regs = ["HL"], instruction_type = "LdIndHlD8", len = 1, cycles = [12])]
    LD_IND_HL_D8 = 0x36,

    #[instruction(instruction_type = "Scf", len = 0, cycles = [4])]
    SCF = 0x37,

    #[instruction(instruction_type = "Jr", len = 1, cycles = [12, 8])]
    JR_C_R8 = 0x38,

    #[instruction(regs = ["HL", "SP"], instruction_type = "AddRrRr", len = 0, cycles = [8])]
    ADD_HL_SP = 0x39,

    #[instruction(regs = ["A", "HL"], instruction_type = "LdRIndHlDec", len = 0, cycles = [8])]
    LD_A_IND_HL_DEC = 0x3a,

    #[instruction(regs = ["SP"], instruction_type = "DecRr", len = 0, cycles = [8])]
    DEC_SP = 0x3b,

    #[instruction(regs = ["A"], instruction_type = "IncR", len = 0, cycles = [4])]
    INC_A = 0x3c,

    #[instruction(regs = ["A"], instruction_type = "DecR", len = 0, cycles = [4])]
    DEC_A = 0x3d,

    #[instruction(regs = ["A"], instruction_type = "LdRD8", len = 1, cycles = [8])]
    LD_A_D8 = 0x3e,

    #[instruction(instruction_type = "Ccf", len = 0, cycles = [4])]
    CCF = 0x3f,

    #[instruction(regs = ["B", "B"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_B_B = 0x40,

    #[instruction(regs = ["B", "C"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_B_C = 0x41,

    #[instruction(regs = ["B", "D"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_B_D = 0x42,

    #[instruction(regs = ["B", "E"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_B_E = 0x43,

    #[instruction(regs = ["B", "H"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_B_H = 0x44,

    #[instruction(regs = ["B", "L"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_B_L = 0x45,

    #[instruction(regs = ["B", "HL"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_B_IND_HL = 0x46,

    #[instruction(regs = ["B", "A"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_B_A = 0x47,

    #[instruction(regs = ["C", "B"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_C_B = 0x48,

    #[instruction(regs = ["C", "C"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_C_C = 0x49,

    #[instruction(regs = ["C", "D"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_C_D = 0x4a,

    #[instruction(regs = ["C", "E"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_C_E = 0x4b,

    #[instruction(regs = ["C", "H"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_C_H = 0x4c,

    #[instruction(regs = ["C", "L"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_C_L = 0x4d,

    #[instruction(regs = ["C", "HL"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_C_IND_HL = 0x4e,

    #[instruction(regs = ["C", "A"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_C_A = 0x4f,

    #[instruction(regs = ["D", "B"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_D_B = 0x50,

    #[instruction(regs = ["D", "C"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_D_C = 0x51,

    #[instruction(regs = ["D", "D"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_D_D = 0x52,

    #[instruction(regs = ["D", "E"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_D_E = 0x53,

    #[instruction(regs = ["D", "H"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_D_H = 0x54,

    #[instruction(regs = ["D", "L"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_D_L = 0x55,

    #[instruction(regs = ["D", "HL"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_D_IND_HL = 0x56,

    #[instruction(regs = ["D", "A"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_D_A = 0x57,

    #[instruction(regs = ["E", "B"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_E_B = 0x58,

    #[instruction(regs = ["E", "C"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_E_C = 0x59,

    #[instruction(regs = ["E", "D"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_E_D = 0x5a,

    #[instruction(regs = ["E", "E"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_E_E = 0x5b,

    #[instruction(regs = ["E", "H"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_E_H = 0x5c,

    #[instruction(regs = ["E", "L"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_E_L = 0x5d,

    #[instruction(regs = ["E", "HL"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_E_IND_HL = 0x5e,

    #[instruction(regs = ["E", "A"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_E_A = 0x5f,

    #[instruction(regs = ["H", "B"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_H_B = 0x60,

    #[instruction(regs = ["H", "C"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_H_C = 0x61,

    #[instruction(regs = ["H", "D"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_H_D = 0x62,

    #[instruction(regs = ["H", "E"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_H_E = 0x63,

    #[instruction(regs = ["H", "H"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_H_H = 0x64,

    #[instruction(regs = ["H", "L"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_H_L = 0x65,

    #[instruction(regs = ["H", "HL"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_H_IND_HL = 0x66,

    #[instruction(regs = ["H", "A"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_H_A = 0x67,

    #[instruction(regs = ["L", "B"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_L_B = 0x68,

    #[instruction(regs = ["L", "C"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_L_C = 0x69,

    #[instruction(regs = ["L", "D"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_L_D = 0x6a,

    #[instruction(regs = ["L", "E"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_L_E = 0x6b,

    #[instruction(regs = ["L", "H"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_L_H = 0x6c,

    #[instruction(regs = ["L", "L"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_L_L = 0x6d,

    #[instruction(regs = ["L", "HL"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_L_IND_HL = 0x6e,

    #[instruction(regs = ["L", "A"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_L_A = 0x6f,

    #[instruction(regs = ["HL", "B"], instruction_type = "LdIndRrR", len = 0, cycles = [8])]
    LD_IND_HL_B = 0x70,

    #[instruction(regs = ["HL", "C"], instruction_type = "LdIndRrR", len = 0, cycles = [8])]
    LD_IND_HL_C = 0x71,

    #[instruction(regs = ["HL", "D"], instruction_type = "LdIndRrR", len = 0, cycles = [8])]
    LD_IND_HL_D = 0x72,

    #[instruction(regs = ["HL", "E"], instruction_type = "LdIndRrR", len = 0, cycles = [8])]
    LD_IND_HL_E = 0x73,

    #[instruction(regs = ["HL", "H"], instruction_type = "LdIndRrR", len = 0, cycles = [4])]
    LD_IND_HL_H = 0x74,

    #[instruction(regs = ["HL", "L"], instruction_type = "LdIndRrR", len = 0, cycles = [8])]
    LD_IND_HL_L = 0x75,

    #[instruction(instruction_type = "Halt", len = 0, cycles = [4])]
    HALT = 0x76,

    #[instruction(regs = ["HL", "A"], instruction_type = "LdIndRrR", len = 0, cycles = [8])]
    LD_IND_HL_A = 0x77,

    #[instruction(regs = ["A", "B"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_A_B = 0x78,

    #[instruction(regs = ["A", "C"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_A_C = 0x79,

    #[instruction(regs = ["A", "D"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_A_D = 0x7a,

    #[instruction(regs = ["A", "E"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_A_E = 0x7b,

    #[instruction(regs = ["A", "H"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_A_H = 0x7c,

    #[instruction(regs = ["A", "L"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_A_L = 0x7d,

    #[instruction(regs = ["A", "HL"], instruction_type = "LdRIndRr", len = 0, cycles = [8])]
    LD_A_IND_HL = 0x7e,

    #[instruction(regs = ["A", "A"], instruction_type = "LdRR", len = 0, cycles = [4])]
    LD_A_A = 0x7f,

    #[instruction(regs = ["B"], instruction_type = "AddR", len = 0, cycles = [4])]
    ADD_B = 0x80,

    #[instruction(regs = ["C"], instruction_type = "AddR", len = 0, cycles = [4])]
    ADD_C = 0x81,

    #[instruction(regs = ["D"], instruction_type = "AddR", len = 0, cycles = [4])]
    ADD_D = 0x82,

    #[instruction(regs = ["E"], instruction_type = "AddR", len = 0, cycles = [4])]
    ADD_E = 0x83,

    #[instruction(regs = ["H"], instruction_type = "AddR", len = 0, cycles = [4])]
    ADD_H = 0x84,

    #[instruction(regs = ["L"], instruction_type = "AddR", len = 0, cycles = [4])]
    ADD_L = 0x85,

    #[instruction(regs = ["A", "HL"], instruction_type = "AddAIndHl", len = 0, cycles = [8])]
    ADD_A_IND_HL = 0x86,

    #[instruction(regs = ["A"], instruction_type = "AddR", len = 0, cycles = [4])]
    ADD_A = 0x87,

    #[instruction(regs = ["B"], instruction_type = "AdcR", len = 0, cycles = [4])]
    ADC_B = 0x88,

    #[instruction(regs = ["C"], instruction_type = "AdcR", len = 0, cycles = [4])]
    ADC_C = 0x89,

    #[instruction(regs = ["D"], instruction_type = "AdcR", len = 0, cycles = [4])]
    ADC_D = 0x8a,

    #[instruction(regs = ["E"], instruction_type = "AdcR", len = 0, cycles = [4])]
    ADC_E = 0x8b,

    #[instruction(regs = ["H"], instruction_type = "AdcR", len = 0, cycles = [4])]
    ADC_H = 0x8c,

    #[instruction(regs = ["L"], instruction_type = "AdcR", len = 0, cycles = [4])]
    ADC_L = 0x8d,

    #[instruction(regs = ["A"], instruction_type = "AdcR", len = 0, cycles = [4])]
    ADC_A = 0x8f,

    #[instruction(regs = ["B"], instruction_type = "SubR", len = 0, cycles = [4])]
    SUB_B = 0x90,

    #[instruction(regs = ["C"], instruction_type = "SubR", len = 0, cycles = [4])]
    SUB_C = 0x91,

    #[instruction(regs = ["D"], instruction_type = "SubR", len = 0, cycles = [4])]
    SUB_D = 0x92,

    #[instruction(regs = ["E"], instruction_type = "SubR", len = 0, cycles = [4])]
    SUB_E = 0x93,

    #[instruction(regs = ["H"], instruction_type = "SubR", len = 0, cycles = [4])]
    SUB_H = 0x94,

    #[instruction(regs = ["L"], instruction_type = "SubR", len = 0, cycles = [4])]
    SUB_L = 0x95,

    #[instruction(regs = ["A"], instruction_type = "SubR", len = 0, cycles = [4])]
    SUB_A = 0x97,

    #[instruction(regs = ["B"], instruction_type = "SbcR", len = 0, cycles = [4])]
    SBC_B = 0x98,

    #[instruction(regs = ["C"], instruction_type = "SbcR", len = 0, cycles = [4])]
    SBC_C = 0x99,

    #[instruction(regs = ["D"], instruction_type = "SbcR", len = 0, cycles = [4])]
    SBC_D = 0x9a,

    #[instruction(regs = ["E"], instruction_type = "SbcR", len = 0, cycles = [4])]
    SBC_E = 0x9b,

    #[instruction(regs = ["H"], instruction_type = "SbcR", len = 0, cycles = [4])]
    SBC_H = 0x9c,

    #[instruction(regs = ["L"], instruction_type = "SbcR", len = 0, cycles = [4])]
    SBC_L = 0x9d,

    #[instruction(regs = ["A"], instruction_type = "SbcR", len = 0, cycles = [4])]
    SBC_A = 0x9f,

    #[instruction(regs = ["B"], instruction_type = "AndR", len = 0, cycles = [4])]
    AND_B = 0xa0,

    #[instruction(regs = ["C"], instruction_type = "AndR", len = 0, cycles = [4])]
    AND_C = 0xa1,

    #[instruction(regs = ["D"], instruction_type = "AndR", len = 0, cycles = [4])]
    AND_D = 0xa2,

    #[instruction(regs = ["E"], instruction_type = "AndR", len = 0, cycles = [4])]
    AND_E = 0xa3,

    #[instruction(regs = ["H"], instruction_type = "AndR", len = 0, cycles = [4])]
    AND_H = 0xa4,

    #[instruction(regs = ["L"], instruction_type = "AndR", len = 0, cycles = [4])]
    AND_L = 0xa5,

    #[instruction(regs = ["A"], instruction_type = "AndR", len = 0, cycles = [4])]
    AND_A = 0xa7,

    #[instruction(regs = ["B"], instruction_type = "XorR", len = 0, cycles = [4])]
    XOR_B = 0xa8,

    #[instruction(regs = ["C"], instruction_type = "XorR", len = 0, cycles = [4])]
    XOR_C = 0xa9,

    #[instruction(regs = ["D"], instruction_type = "XorR", len = 0, cycles = [4])]
    XOR_D = 0xaa,

    #[instruction(regs = ["E"], instruction_type = "XorR", len = 0, cycles = [4])]
    XOR_E = 0xab,

    #[instruction(regs = ["H"], instruction_type = "XorR", len = 0, cycles = [4])]
    XOR_H = 0xac,

    #[instruction(regs = ["L"], instruction_type = "XorR", len = 0, cycles = [4])]
    XOR_L = 0xad,

    #[instruction(regs = ["A", "HL"], instruction_type = "XorAIndHl", len = 0, cycles = [8])]
    XOR_A_IND_HL = 0xae,

    #[instruction(regs = ["A"], instruction_type = "XorR", len = 0, cycles = [4])]
    XOR_A = 0xaf,

    #[instruction(regs = ["B"], instruction_type = "OrR", len = 0, cycles = [4])]
    OR_B = 0xb0,

    #[instruction(regs = ["C"], instruction_type = "OrR", len = 0, cycles = [4])]
    OR_C = 0xb1,

    #[instruction(regs = ["D"], instruction_type = "OrR", len = 0, cycles = [4])]
    OR_D = 0xb2,

    #[instruction(regs = ["E"], instruction_type = "OrR", len = 0, cycles = [4])]
    OR_E = 0xb3,

    #[instruction(regs = ["H"], instruction_type = "OrR", len = 0, cycles = [4])]
    OR_H = 0xb4,

    #[instruction(regs = ["L"], instruction_type = "OrR", len = 0, cycles = [4])]
    OR_L = 0xb5,

    #[instruction(regs = ["A", "HL"], instruction_type = "OrAIndHl", len = 0, cycles = [8])]
    OR_A_IND_HL = 0xb6,

    #[instruction(regs = ["A"], instruction_type = "OrR", len = 0, cycles = [4])]
    OR_A = 0xb7,

    #[instruction(regs = ["B"], instruction_type = "CpR", len = 0, cycles = [4])]
    CP_B = 0xb8,

    #[instruction(regs = ["C"], instruction_type = "CpR", len = 0, cycles = [4])]
    CP_C = 0xb9,

    #[instruction(regs = ["D"], instruction_type = "CpR", len = 0, cycles = [4])]
    CP_D = 0xba,

    #[instruction(regs = ["E"], instruction_type = "CpR", len = 0, cycles = [4])]
    CP_E = 0xbb,

    #[instruction(regs = ["H"], instruction_type = "CpR", len = 0, cycles = [4])]
    CP_H = 0xbc,

    #[instruction(regs = ["L"], instruction_type = "CpR", len = 0, cycles = [4])]
    CP_L = 0xbd,

    #[instruction(regs = ["A", "HL"], instruction_type = "CpAIndHl", len = 0, cycles = [8])]
    CP_A_IND_HL = 0xbe,

    #[instruction(regs = ["A"], instruction_type = "CpR", len = 0, cycles = [4])]
    CP_A = 0xbf,

    #[instruction(instruction_type = "Ret", len = 0, cycles = [20, 8])]
    RET_NZ = 0xc0,

    #[instruction(regs = ["BC"], instruction_type = "PopRr", len = 0, cycles = [12])]
    POP_BC = 0xc1,

    #[instruction(instruction_type = "Jp", len = 2, cycles = [16, 12])]
    JP_NZ_A16 = 0xc2,

    #[instruction(instruction_type = "Jp", len = 2, cycles = [16])]
    JP_A16 = 0xc3,

    #[instruction(instruction_type = "Call", len = 2, cycles = [24, 12])]
    CALL_NZ_A16 = 0xc4,

    #[instruction(regs = ["BC"], instruction_type = "PushRr", len = 0, cycles = [16])]
    PUSH_BC = 0xc5,

    #[instruction(instruction_type = "AddD8", len = 1, cycles = [8])]
    ADD_D8 = 0xc6,

    #[instruction(instruction_type = "Rst", len = 0, cycles = [16])]
    RST00 = 0xc7,

    #[instruction(instruction_type = "Ret", len = 0, cycles = [20, 8])]
    RET_Z = 0xc8,

    #[instruction(instruction_type = "Ret", len = 0, cycles = [16])]
    RET = 0xc9,

    #[instruction(instruction_type = "Jp", len = 2, cycles = [16, 12])]
    JP_Z_A16 = 0xca,

    PREFIX = 0xcb,

    #[instruction(instruction_type = "Call", len = 2, cycles = [24, 12])]
    CALL_Z_A16 = 0xcc,

    #[instruction(instruction_type = "Call", len = 2, cycles = [24])]
    CALL_A16 = 0xcd,

    #[instruction(instruction_type = "AdcD8", len = 1, cycles = [8])]
    ADC_D8 = 0xce,

    #[instruction(instruction_type = "Rst", len = 0, cycles = [16])]
    RST08 = 0xcf,

    #[instruction(instruction_type = "Ret", len = 0, cycles = [20, 8])]
    RET_NC = 0xd0,

    #[instruction(regs = ["DE"], instruction_type = "PopRr", len = 0, cycles = [12])]
    POP_DE = 0xd1,

    #[instruction(instruction_type = "Jp", len = 2, cycles = [16, 12])]
    JP_NC_A16 = 0xd2,

    #[instruction(instruction_type = "Call", len = 2, cycles = [24, 12])]
    CALL_NC_A16 = 0xd4,

    #[instruction(regs = ["DE"], instruction_type = "PushRr", len = 0, cycles = [16])]
    PUSH_DE = 0xd5,

    #[instruction(instruction_type = "SubD8", len = 1, cycles = [8])]
    SUB_D8 = 0xd6,

    #[instruction(instruction_type = "Rst", len = 0, cycles = [16])]
    RST10 = 0xd7,

    #[instruction(instruction_type = "Ret", len = 0, cycles = [20, 8])]
    RET_C = 0xd8,

    #[instruction(instruction_type = "Ret", len = 0, cycles = [16])]
    RETI = 0xd9,

    #[instruction(instruction_type = "Jp", len = 2, cycles = [16, 12])]
    JP_C_A16 = 0xda,

    #[instruction(instruction_type = "Call", len = 2, cycles = [24, 12])]
    CALL_C_A16 = 0xdc,

    #[instruction(instruction_type = "SbcD8", len = 1, cycles = [8])]
    SBC_D8 = 0xde,

    #[instruction(instruction_type = "Rst", len = 0, cycles = [16])]
    RST18 = 0xdf,

    #[instruction(regs = ["A"], instruction_type = "LdA8A", len = 1, cycles = [12])]
    LDH_A8_A = 0xe0,

    #[instruction(regs = ["HL"], instruction_type = "PopRr", len = 0, cycles = [12])]
    POP_HL = 0xe1,

    #[instruction(regs = ["C", "A"], instruction_type = "LdIndCA", len = 0, cycles = [8])]
    LD_IND_C_A = 0xe2,

    #[instruction(regs = ["HL"], instruction_type = "PushRr", len = 0, cycles = [16])]
    PUSH_HL = 0xe5,

    #[instruction(instruction_type = "AndD8", len = 1, cycles = [8])]
    AND_D8 = 0xe6,

    #[instruction(instruction_type = "Rst", len = 0, cycles = [16])]
    RST20 = 0xe7,

    #[instruction(instruction_type = "AddSpE8", len = 1, cycles = [16])]
    ADD_SP_E8 = 0xe8,

    #[instruction(instruction_type = "Jp", len = 0, cycles = [4])]
    JP_HL = 0xe9,

    #[instruction(regs = ["A"], instruction_type = "LdA16A", len = 2, cycles = [16])]
    LD_A16_A = 0xea,

    #[instruction(instruction_type = "XorD8", len = 1, cycles = [8])]
    XOR_D8 = 0xee,

    #[instruction(instruction_type = "Rst", len = 0, cycles = [16])]
    RST28 = 0xef,

    #[instruction(regs = ["A"], instruction_type = "LdAA8", len = 1, cycles = [12])]
    LDH_A_A8 = 0xf0,

    #[instruction(regs = ["AF"], instruction_type = "PopRr", len = 0, cycles = [12])]
    POP_AF = 0xf1,

    #[instruction(regs = ["A", "C"], instruction_type = "LdAIndC", len = 0, cycles = [8])]
    LD_A_IND_C = 0xf2,

    #[instruction(instruction_type = "Di", len = 0, cycles = [4])]
    DI = 0xf3,

    #[instruction(regs = ["AF"], instruction_type = "PushRr", len = 0, cycles = [16])]
    PUSH_AF = 0xf5,

    #[instruction(instruction_type = "OrD8", len = 1, cycles = [8])]
    OR_D8 = 0xf6,

    #[instruction(instruction_type = "Rst", len = 0, cycles = [16])]
    RST30 = 0xf7,

    #[instruction(regs = ["HL", "SP"], instruction_type = "LdHLSPE8", len = 1, cycles = [12])]
    LD_HL_SP_E8 = 0xf8,

    #[instruction(regs = ["SP", "HL"], instruction_type = "LdRrRr", len = 0, cycles = [8])]
    LD_SP_HL = 0xf9,

    #[instruction(regs = ["A"], instruction_type = "LdAA16", len = 2, cycles = [16])]
    LD_A_A16 = 0xfa,

    #[instruction(instruction_type = "Ei", len = 0, cycles = [4])]
    EI = 0xfb,

    #[instruction(instruction_type = "CpD8", len = 1, cycles = [8])]
    CP_D8 = 0xfe,

    #[instruction(instruction_type = "Rst", len = 0, cycles = [16])]
    RST38 = 0xff,
}

#[derive(Debug, Clone, Copy, TryFromPrimitive, InstructionImpl)]
#[num_enum(error_type(name = InstructionError, constructor = InstructionError::UnrecognizedPrefixedOpcode))]
#[instruction_struct_name(PrefixedInstruction)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum PrefixedOpcode {
    #[instruction(regs = ["B"], instruction_type = "RlcR", len = 0, cycles = [12])]
    RLC_B = 0x00,

    #[instruction(regs = ["C"], instruction_type = "RlcR", len = 0, cycles = [12])]
    RLC_C = 0x01,

    #[instruction(regs = ["D"], instruction_type = "RlcR", len = 0, cycles = [12])]
    RLC_D = 0x02,

    #[instruction(regs = ["E"], instruction_type = "RlcR", len = 0, cycles = [12])]
    RLC_E = 0x03,

    #[instruction(regs = ["H"], instruction_type = "RlcR", len = 0, cycles = [12])]
    RLC_H = 0x04,

    #[instruction(regs = ["L"], instruction_type = "RlcR", len = 0, cycles = [12])]
    RLC_L = 0x05,

    #[instruction(regs = ["A"], instruction_type = "RlcR", len = 0, cycles = [12])]
    RLC_A = 0x07,

    #[instruction(regs = ["B"], instruction_type = "RrcR", len = 0, cycles = [12])]
    RRC_B = 0x08,

    #[instruction(regs = ["C"], instruction_type = "RrcR", len = 0, cycles = [12])]
    RRC_C = 0x09,

    #[instruction(regs = ["D"], instruction_type = "RrcR", len = 0, cycles = [12])]
    RRC_D = 0x0a,

    #[instruction(regs = ["E"], instruction_type = "RrcR", len = 0, cycles = [12])]
    RRC_E = 0x0b,

    #[instruction(regs = ["H"], instruction_type = "RrcR", len = 0, cycles = [12])]
    RRC_H = 0x0c,

    #[instruction(regs = ["L"], instruction_type = "RrcR", len = 0, cycles = [12])]
    RRC_L = 0x0d,

    #[instruction(regs = ["A"], instruction_type = "RrcR", len = 0, cycles = [12])]
    RRC_A = 0x0f,

    #[instruction(regs = ["B"], instruction_type = "RlR", len = 0, cycles = [12])]
    RL_B = 0x10,

    #[instruction(regs = ["C"], instruction_type = "RlR", len = 0, cycles = [12])]
    RL_C = 0x11,

    #[instruction(regs = ["D"], instruction_type = "RlR", len = 0, cycles = [12])]
    RL_D = 0x12,

    #[instruction(regs = ["E"], instruction_type = "RlR", len = 0, cycles = [12])]
    RL_E = 0x13,

    #[instruction(regs = ["H"], instruction_type = "RlR", len = 0, cycles = [12])]
    RL_H = 0x14,

    #[instruction(regs = ["L"], instruction_type = "RlR", len = 0, cycles = [12])]
    RL_L = 0x15,

    #[instruction(regs = ["A"], instruction_type = "RlR", len = 0, cycles = [12])]
    RL_A = 0x17,

    #[instruction(regs = ["B"], instruction_type = "RrR", len = 0, cycles = [12])]
    RR_B = 0x18,

    #[instruction(regs = ["C"], instruction_type = "RrR", len = 0, cycles = [12])]
    RR_C = 0x19,

    #[instruction(regs = ["D"], instruction_type = "RrR", len = 0, cycles = [12])]
    RR_D = 0x1a,

    #[instruction(regs = ["E"], instruction_type = "RrR", len = 0, cycles = [12])]
    RR_E = 0x1b,

    #[instruction(regs = ["H"], instruction_type = "RrR", len = 0, cycles = [12])]
    RR_H = 0x1c,

    #[instruction(regs = ["L"], instruction_type = "RrR", len = 0, cycles = [12])]
    RR_L = 0x1d,

    #[instruction(regs = ["A"], instruction_type = "RrR", len = 0, cycles = [12])]
    RR_A = 0x1f,

    #[instruction(regs = ["B"], instruction_type = "SlaR", len = 0, cycles = [12])]
    SLA_B = 0x20,

    #[instruction(regs = ["C"], instruction_type = "SlaR", len = 0, cycles = [12])]
    SLA_C = 0x21,

    #[instruction(regs = ["D"], instruction_type = "SlaR", len = 0, cycles = [12])]
    SLA_D = 0x22,

    #[instruction(regs = ["E"], instruction_type = "SlaR", len = 0, cycles = [12])]
    SLA_E = 0x23,

    #[instruction(regs = ["H"], instruction_type = "SlaR", len = 0, cycles = [12])]
    SLA_H = 0x24,

    #[instruction(regs = ["L"], instruction_type = "SlaR", len = 0, cycles = [12])]
    SLA_L = 0x25,

    #[instruction(regs = ["A"], instruction_type = "SlaR", len = 0, cycles = [12])]
    SLA_A = 0x27,

    #[instruction(regs = ["B"], instruction_type = "SraR", len = 0, cycles = [12])]
    SRA_B = 0x28,

    #[instruction(regs = ["C"], instruction_type = "SraR", len = 0, cycles = [12])]
    SRA_C = 0x29,

    #[instruction(regs = ["D"], instruction_type = "SraR", len = 0, cycles = [12])]
    SRA_D = 0x2a,

    #[instruction(regs = ["E"], instruction_type = "SraR", len = 0, cycles = [12])]
    SRA_E = 0x2b,

    #[instruction(regs = ["H"], instruction_type = "SraR", len = 0, cycles = [12])]
    SRA_H = 0x2c,

    #[instruction(regs = ["L"], instruction_type = "SraR", len = 0, cycles = [12])]
    SRA_L = 0x2d,

    #[instruction(regs = ["A"], instruction_type = "SraR", len = 0, cycles = [12])]
    SRA_A = 0x2f,

    #[instruction(regs = ["B"], instruction_type = "SwapR", len = 0, cycles = [12])]
    SWAP_B = 0x30,

    #[instruction(regs = ["C"], instruction_type = "SwapR", len = 0, cycles = [12])]
    SWAP_C = 0x31,

    #[instruction(regs = ["D"], instruction_type = "SwapR", len = 0, cycles = [12])]
    SWAP_D = 0x32,

    #[instruction(regs = ["E"], instruction_type = "SwapR", len = 0, cycles = [12])]
    SWAP_E = 0x33,

    #[instruction(regs = ["H"], instruction_type = "SwapR", len = 0, cycles = [12])]
    SWAP_H = 0x34,

    #[instruction(regs = ["L"], instruction_type = "SwapR", len = 0, cycles = [12])]
    SWAP_L = 0x35,

    #[instruction(regs = ["A"], instruction_type = "SwapR", len = 0, cycles = [12])]
    SWAP_A = 0x37,

    #[instruction(regs = ["B"], instruction_type = "SrlR", len = 0, cycles = [12])]
    SRL_B = 0x38,

    #[instruction(regs = ["C"], instruction_type = "SrlR", len = 0, cycles = [12])]
    SRL_C = 0x39,

    #[instruction(regs = ["D"], instruction_type = "SrlR", len = 0, cycles = [12])]
    SRL_D = 0x3a,

    #[instruction(regs = ["E"], instruction_type = "SrlR", len = 0, cycles = [12])]
    SRL_E = 0x3b,

    #[instruction(regs = ["H"], instruction_type = "SrlR", len = 0, cycles = [12])]
    SRL_H = 0x3c,

    #[instruction(regs = ["L"], instruction_type = "SrlR", len = 0, cycles = [12])]
    SRL_L = 0x3d,

    #[instruction(regs = ["A"], instruction_type = "SrlR", len = 0, cycles = [12])]
    SRL_A = 0x3f,

    #[instruction(regs = ["B"], instruction_type = "Bit0R", len = 0, cycles = [12])]
    BIT0B = 0x40,

    #[instruction(regs = ["C"], instruction_type = "Bit0R", len = 0, cycles = [12])]
    BIT0C = 0x41,

    #[instruction(regs = ["D"], instruction_type = "Bit0R", len = 0, cycles = [12])]
    BIT0D = 0x42,

    #[instruction(regs = ["E"], instruction_type = "Bit0R", len = 0, cycles = [12])]
    BIT0E = 0x43,

    #[instruction(regs = ["H"], instruction_type = "Bit0R", len = 0, cycles = [12])]
    BIT0H = 0x44,

    #[instruction(regs = ["L"], instruction_type = "Bit0R", len = 0, cycles = [12])]
    BIT0L = 0x45,

    #[instruction(regs = ["A"], instruction_type = "Bit0R", len = 0, cycles = [12])]
    BIT0A = 0x47,

    #[instruction(regs = ["B"], instruction_type = "Bit1R", len = 0, cycles = [12])]
    BIT1B = 0x48,

    #[instruction(regs = ["C"], instruction_type = "Bit1R", len = 0, cycles = [12])]
    BIT1C = 0x49,

    #[instruction(regs = ["D"], instruction_type = "Bit1R", len = 0, cycles = [12])]
    BIT1D = 0x4a,

    #[instruction(regs = ["E"], instruction_type = "Bit1R", len = 0, cycles = [12])]
    BIT1E = 0x4b,

    #[instruction(regs = ["H"], instruction_type = "Bit1R", len = 0, cycles = [12])]
    BIT1H = 0x4c,

    #[instruction(regs = ["L"], instruction_type = "Bit1R", len = 0, cycles = [12])]
    BIT1L = 0x4d,

    #[instruction(regs = ["A"], instruction_type = "Bit1R", len = 0, cycles = [12])]
    BIT1A = 0x4f,

    #[instruction(regs = ["B"], instruction_type = "Bit2R", len = 0, cycles = [12])]
    BIT2B = 0x50,

    #[instruction(regs = ["C"], instruction_type = "Bit2R", len = 0, cycles = [12])]
    BIT2C = 0x51,

    #[instruction(regs = ["D"], instruction_type = "Bit2R", len = 0, cycles = [12])]
    BIT2D = 0x52,

    #[instruction(regs = ["E"], instruction_type = "Bit2R", len = 0, cycles = [12])]
    BIT2E = 0x53,

    #[instruction(regs = ["H"], instruction_type = "Bit2R", len = 0, cycles = [12])]
    BIT2H = 0x54,

    #[instruction(regs = ["L"], instruction_type = "Bit2R", len = 0, cycles = [12])]
    BIT2L = 0x55,

    #[instruction(regs = ["A"], instruction_type = "Bit2R", len = 0, cycles = [12])]
    BIT2A = 0x57,

    #[instruction(regs = ["B"], instruction_type = "Bit3R", len = 0, cycles = [12])]
    BIT3B = 0x58,

    #[instruction(regs = ["C"], instruction_type = "Bit3R", len = 0, cycles = [12])]
    BIT3C = 0x59,

    #[instruction(regs = ["D"], instruction_type = "Bit3R", len = 0, cycles = [12])]
    BIT3D = 0x5a,

    #[instruction(regs = ["E"], instruction_type = "Bit3R", len = 0, cycles = [12])]
    BIT3E = 0x5b,

    #[instruction(regs = ["H"], instruction_type = "Bit3R", len = 0, cycles = [12])]
    BIT3H = 0x5c,

    #[instruction(regs = ["L"], instruction_type = "Bit3R", len = 0, cycles = [12])]
    BIT3L = 0x5d,

    #[instruction(regs = ["A"], instruction_type = "Bit3R", len = 0, cycles = [12])]
    BIT3A = 0x5e,

    #[instruction(regs = ["B"], instruction_type = "Bit4R", len = 0, cycles = [12])]
    BIT4B = 0x60,

    #[instruction(regs = ["C"], instruction_type = "Bit4R", len = 0, cycles = [12])]
    BIT4C = 0x61,

    #[instruction(regs = ["D"], instruction_type = "Bit4R", len = 0, cycles = [12])]
    BIT4D = 0x62,

    #[instruction(regs = ["E"], instruction_type = "Bit4R", len = 0, cycles = [12])]
    BIT4E = 0x63,

    #[instruction(regs = ["H"], instruction_type = "Bit4R", len = 0, cycles = [12])]
    BIT4H = 0x64,

    #[instruction(regs = ["L"], instruction_type = "Bit4R", len = 0, cycles = [12])]
    BIT4L = 0x65,

    #[instruction(regs = ["A"], instruction_type = "Bit4R", len = 0, cycles = [12])]
    BIT4A = 0x67,

    #[instruction(regs = ["B"], instruction_type = "Bit5R", len = 0, cycles = [12])]
    BIT5B = 0x68,

    #[instruction(regs = ["C"], instruction_type = "Bit5R", len = 0, cycles = [12])]
    BIT5C = 0x69,

    #[instruction(regs = ["D"], instruction_type = "Bit5R", len = 0, cycles = [12])]
    BIT5D = 0x6a,

    #[instruction(regs = ["E"], instruction_type = "Bit5R", len = 0, cycles = [12])]
    BIT5E = 0x6b,

    #[instruction(regs = ["H"], instruction_type = "Bit5R", len = 0, cycles = [12])]
    BIT5H = 0x6c,

    #[instruction(regs = ["L"], instruction_type = "Bit5R", len = 0, cycles = [12])]
    BIT5L = 0x6d,

    #[instruction(regs = ["A"], instruction_type = "Bit5R", len = 0, cycles = [12])]
    BIT5A = 0x6f,

    #[instruction(regs = ["B"], instruction_type = "Bit6R", len = 0, cycles = [12])]
    BIT6B = 0x70,

    #[instruction(regs = ["C"], instruction_type = "Bit6R", len = 0, cycles = [12])]
    BIT6C = 0x71,

    #[instruction(regs = ["D"], instruction_type = "Bit6R", len = 0, cycles = [12])]
    BIT6D = 0x72,

    #[instruction(regs = ["E"], instruction_type = "Bit6R", len = 0, cycles = [12])]
    BIT6E = 0x73,

    #[instruction(regs = ["H"], instruction_type = "Bit6R", len = 0, cycles = [12])]
    BIT6H = 0x74,

    #[instruction(regs = ["L"], instruction_type = "Bit6R", len = 0, cycles = [12])]
    BIT6L = 0x75,

    #[instruction(regs = ["A"], instruction_type = "Bit6R", len = 0, cycles = [12])]
    BIT6A = 0x77,

    #[instruction(regs = ["B"], instruction_type = "Bit7R", len = 0, cycles = [12])]
    BIT7B = 0x78,

    #[instruction(regs = ["C"], instruction_type = "Bit7R", len = 0, cycles = [12])]
    BIT7C = 0x79,

    #[instruction(regs = ["D"], instruction_type = "Bit7R", len = 0, cycles = [12])]
    BIT7D = 0x7a,

    #[instruction(regs = ["E"], instruction_type = "Bit7R", len = 0, cycles = [12])]
    BIT7E = 0x7b,

    #[instruction(regs = ["H"], instruction_type = "Bit7R", len = 0, cycles = [12])]
    BIT7H = 0x7c,

    #[instruction(regs = ["L"], instruction_type = "Bit7R", len = 0, cycles = [12])]
    BIT7L = 0x7d,

    #[instruction(regs = ["A"], instruction_type = "Bit7R", len = 0, cycles = [12])]
    BIT7A = 0x7f,

    #[instruction(regs = ["B"], instruction_type = "Res0R", len = 0, cycles = [12])]
    RES0B = 0x80,

    #[instruction(regs = ["C"], instruction_type = "Res0R", len = 0, cycles = [12])]
    RES0C = 0x81,

    #[instruction(regs = ["D"], instruction_type = "Res0R", len = 0, cycles = [12])]
    RES0D = 0x82,

    #[instruction(regs = ["E"], instruction_type = "Res0R", len = 0, cycles = [12])]
    RES0E = 0x83,

    #[instruction(regs = ["H"], instruction_type = "Res0R", len = 0, cycles = [12])]
    RES0H = 0x84,

    #[instruction(regs = ["L"], instruction_type = "Res0R", len = 0, cycles = [12])]
    RES0L = 0x85,

    #[instruction(regs = ["A"], instruction_type = "Res0R", len = 0, cycles = [12])]
    RES0A = 0x87,

    #[instruction(regs = ["B"], instruction_type = "Res1R", len = 0, cycles = [12])]
    RES1B = 0x88,

    #[instruction(regs = ["C"], instruction_type = "Res1R", len = 0, cycles = [12])]
    RES1C = 0x89,

    #[instruction(regs = ["D"], instruction_type = "Res1R", len = 0, cycles = [12])]
    RES1D = 0x8a,

    #[instruction(regs = ["E"], instruction_type = "Res1R", len = 0, cycles = [12])]
    RES1E = 0x8b,

    #[instruction(regs = ["H"], instruction_type = "Res1R", len = 0, cycles = [12])]
    RES1H = 0x8c,

    #[instruction(regs = ["L"], instruction_type = "Res1R", len = 0, cycles = [12])]
    RES1L = 0x8d,

    #[instruction(regs = ["A"], instruction_type = "Res1R", len = 0, cycles = [12])]
    RES1A = 0x8f,

    #[instruction(regs = ["B"], instruction_type = "Res2R", len = 0, cycles = [12])]
    RES2B = 0x90,

    #[instruction(regs = ["C"], instruction_type = "Res2R", len = 0, cycles = [12])]
    RES2C = 0x91,

    #[instruction(regs = ["D"], instruction_type = "Res2R", len = 0, cycles = [12])]
    RES2D = 0x92,

    #[instruction(regs = ["E"], instruction_type = "Res2R", len = 0, cycles = [12])]
    RES2E = 0x93,

    #[instruction(regs = ["H"], instruction_type = "Res2R", len = 0, cycles = [12])]
    RES2H = 0x94,

    #[instruction(regs = ["L"], instruction_type = "Res2R", len = 0, cycles = [12])]
    RES2L = 0x95,

    #[instruction(regs = ["A"], instruction_type = "Res2R", len = 0, cycles = [12])]
    RES2A = 0x97,

    #[instruction(regs = ["B"], instruction_type = "Res3R", len = 0, cycles = [12])]
    RES3B = 0x98,

    #[instruction(regs = ["C"], instruction_type = "Res3R", len = 0, cycles = [12])]
    RES3C = 0x99,

    #[instruction(regs = ["D"], instruction_type = "Res3R", len = 0, cycles = [12])]
    RES3D = 0x9a,

    #[instruction(regs = ["E"], instruction_type = "Res3R", len = 0, cycles = [12])]
    RES3E = 0x9b,

    #[instruction(regs = ["H"], instruction_type = "Res3R", len = 0, cycles = [12])]
    RES3H = 0x9c,

    #[instruction(regs = ["L"], instruction_type = "Res3R", len = 0, cycles = [12])]
    RES3L = 0x9d,

    #[instruction(regs = ["A"], instruction_type = "Res3R", len = 0, cycles = [12])]
    RES3A = 0x9e,

    #[instruction(regs = ["B"], instruction_type = "Res4R", len = 0, cycles = [12])]
    RES4B = 0xa0,

    #[instruction(regs = ["C"], instruction_type = "Res4R", len = 0, cycles = [12])]
    RES4C = 0xa1,

    #[instruction(regs = ["D"], instruction_type = "Res4R", len = 0, cycles = [12])]
    RES4D = 0xa2,

    #[instruction(regs = ["E"], instruction_type = "Res4R", len = 0, cycles = [12])]
    RES4E = 0xa3,

    #[instruction(regs = ["H"], instruction_type = "Res4R", len = 0, cycles = [12])]
    RES4H = 0xa4,

    #[instruction(regs = ["L"], instruction_type = "Res4R", len = 0, cycles = [12])]
    RES4L = 0xa5,

    #[instruction(regs = ["A"], instruction_type = "Res4R", len = 0, cycles = [12])]
    RES4A = 0xa7,

    #[instruction(regs = ["B"], instruction_type = "Res5R", len = 0, cycles = [12])]
    RES5B = 0xa8,

    #[instruction(regs = ["C"], instruction_type = "Res5R", len = 0, cycles = [12])]
    RES5C = 0xa9,

    #[instruction(regs = ["D"], instruction_type = "Res5R", len = 0, cycles = [12])]
    RES5D = 0xaa,

    #[instruction(regs = ["E"], instruction_type = "Res5R", len = 0, cycles = [12])]
    RES5E = 0xab,

    #[instruction(regs = ["H"], instruction_type = "Res5R", len = 0, cycles = [12])]
    RES5H = 0xac,

    #[instruction(regs = ["L"], instruction_type = "Res5R", len = 0, cycles = [12])]
    RES5L = 0xad,

    #[instruction(regs = ["A"], instruction_type = "Res5R", len = 0, cycles = [12])]
    RES5A = 0xaf,

    #[instruction(regs = ["B"], instruction_type = "Res6R", len = 0, cycles = [12])]
    RES6B = 0xb0,

    #[instruction(regs = ["C"], instruction_type = "Res6R", len = 0, cycles = [12])]
    RES6C = 0xb1,

    #[instruction(regs = ["D"], instruction_type = "Res6R", len = 0, cycles = [12])]
    RES6D = 0xb2,

    #[instruction(regs = ["E"], instruction_type = "Res6R", len = 0, cycles = [12])]
    RES6E = 0xb3,

    #[instruction(regs = ["H"], instruction_type = "Res6R", len = 0, cycles = [12])]
    RES6H = 0xb4,

    #[instruction(regs = ["L"], instruction_type = "Res6R", len = 0, cycles = [12])]
    RES6L = 0xb5,

    #[instruction(regs = ["A"], instruction_type = "Res6R", len = 0, cycles = [12])]
    RES6A = 0xb7,

    #[instruction(regs = ["B"], instruction_type = "Res7R", len = 0, cycles = [12])]
    RES7B = 0xb8,

    #[instruction(regs = ["C"], instruction_type = "Res7R", len = 0, cycles = [12])]
    RES7C = 0xb9,

    #[instruction(regs = ["D"], instruction_type = "Res7R", len = 0, cycles = [12])]
    RES7D = 0xba,

    #[instruction(regs = ["E"], instruction_type = "Res7R", len = 0, cycles = [12])]
    RES7E = 0xbb,

    #[instruction(regs = ["H"], instruction_type = "Res7R", len = 0, cycles = [12])]
    RES7H = 0xbc,

    #[instruction(regs = ["L"], instruction_type = "Res7R", len = 0, cycles = [12])]
    RES7L = 0xbd,

    #[instruction(regs = ["A"], instruction_type = "Res7R", len = 0, cycles = [12])]
    RES7A = 0xbf,
}
