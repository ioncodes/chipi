//! The `isa!` macro expands a `.chipi` spec into a decoder module.

chipi_macros::isa!("../../examples/mips.chipi");
chipi_macros::isa!("../../examples/gb.chipi");
chipi_macros::isa!("../../examples/snes_disasm.chipi", style = enum);

#[test]
fn mips_module_decodes() {
    let (inst, len) = Mips::decode(0x0085_1020);
    assert_eq!(inst.opcode_name(), "add");
    assert_eq!(len, 4);
    assert_eq!(inst.rd(), 2);
    assert_eq!(inst.rs(), 4);
    assert_eq!(inst.rt(), 5);
    assert_eq!(Mips::decode(0xFFFF_FFFF).0.opcode_name(), "Invalid");
    assert_eq!(Mips::OPCODE_COUNT, 12);
}

#[test]
fn gb_module_is_independent() {
    assert_eq!(Gb::decode(0x00).0.opcode_name(), "nop");
    assert_eq!(Gb::decode(0x76).0.opcode_name(), "halt");
    // 0x76 is HALT (a specific leaf) inside the general LD r,r' block; specificity wins.
    assert_eq!(Gb::decode(0x41).0.opcode_name(), "ld");
}

#[test]
fn enum_module_decodes_and_renders() {
    struct Mem;
    impl SnesD::DisasmCtx for Mem {
        fn read_u8(&self, addr: u64) -> u8 {
            [0xEAu8, 0xA9, 0x34, 0x12, 0x4C, 0x00, 0x90]
                .get(addr as usize)
                .copied()
                .unwrap_or(0)
        }
    }

    let (nop, nop_len) = SnesD::decode(0, &Mem);
    assert_eq!(nop, SnesD::Instruction::Nop);
    assert_eq!(nop_len, 1);
    assert_eq!(nop.name(), "nop");

    // Operands are pre-extracted into the variant at decode time.
    let (lda, lda_len) = SnesD::decode(1, &Mem);
    assert_eq!(lda, SnesD::Instruction::LdaImm { imm: 0x1234 });
    assert_eq!(lda_len, 3);

    let (jmp, _) = SnesD::decode(4, &Mem);
    assert!(matches!(jmp, SnesD::Instruction::JmpAbs { target: 0x9000 }));
}
