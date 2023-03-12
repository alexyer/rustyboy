pub enum InstructionError {
    UnrecognizedOpcode(u8),
    UnrecognizedPrefixedOpcode(u8),
}
