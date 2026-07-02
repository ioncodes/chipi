//! The `isa!` macro expands a `.chipi` spec into a decoder module.

chipi_macros::isa!("../../examples/mips.chipi");
chipi_macros::isa!("../../examples/gb.chipi");
chipi_macros::isa!("../../examples/snes_disasm.chipi", style = enum);
chipi_macros::isa!("../../examples/cond_demo.chipi", style = enum);
chipi_macros::isa!("../../examples/modes_demo.chipi");
chipi_macros::isa!("../../examples/axes_demo.chipi");

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

/// A modal spec expands too: classification takes the packed mode combination, and the same
/// opcode byte resolves to a different leaf per combo. The `disasm_in` renderer is also emitted,
/// but it sits behind `feature = "disasm"`, which this crate does not define; its behavior is
/// covered against the oracle in `chipi-backend-rust/tests/generated.rs`.
#[test]
fn modal_module_decodes_per_combo() {
    assert_eq!(MX::MODE_COMBOS, 2);
    let m8 = MX::pack_modes(1);
    let m16 = MX::pack_modes(0);

    let lda = MX::Instruction(0xA9);
    assert_eq!(lda.opcode_name_in(m8), "lda8");
    assert_eq!(lda.opcode_name_in(m16), "lda16");

    assert_eq!(MX::Instruction(0xEA).opcode_name_in(m8), "nop");
    assert_eq!(MX::Instruction(0xFF).opcode_name_in(m8), "Invalid");
}

/// Dotted leaf names derive the identity axes: `Mnemonic` and `Form` enums, name tables and
/// accessors, and `dispatch load { lda.*, ldx.* }` expands over the mnemonic axis. This is the
/// consumer story the axes exist for: one match over `Form` handles every mnemonic, because
/// leaves of one form are checked to bind the same operand shape.
#[test]
fn axes_module_exposes_identity() {
    use Axes::{Form, Instruction, Mnemonic};

    let lda_imm = Instruction(0xA9B7);
    assert_eq!(lda_imm.mnemonic(), Mnemonic::Lda);
    assert_eq!(lda_imm.form(), Form::Imm);
    assert_eq!(lda_imm.mnemonic().name(), "lda");
    assert_eq!(lda_imm.form().name(), "imm");
    assert_eq!(lda_imm.opcode_name(), "lda.imm");

    let sta_dp = Instruction(0x85C0);
    assert_eq!(sta_dp.mnemonic(), Mnemonic::Sta);
    assert_eq!(sta_dp.form(), Form::Dp);

    let nop = Instruction(0xEA00);
    assert_eq!(nop.mnemonic(), Mnemonic::Nop);
    assert_eq!(nop.form(), Form::None);
    assert_eq!(Instruction(0x0000).mnemonic(), Mnemonic::Invalid);

    // Effective-address style consumption: one match over the form axis, using the form's
    // uniform accessors, regardless of mnemonic.
    fn operand_of(inst: Instruction) -> Option<u64> {
        match inst.form() {
            Form::Imm => Some(inst.v() as u64),
            Form::Dp => Some(inst.dp() as u64),
            Form::None => None,
        }
    }
    assert_eq!(operand_of(lda_imm), Some(0xB7));
    assert_eq!(operand_of(sta_dp), Some(0xC0));
    assert_eq!(operand_of(Instruction(0xA2AA)), Some(0xAA));
    assert_eq!(operand_of(nop), None);

    // The `lda.*` / `ldx.*` dispatch pattern expanded to every leaf of those mnemonics.
    assert!(Instruction(0xA9B7).tags().contains(&"load"));
    assert!(Instruction(0xA2AA).tags().contains(&"load"));
    assert!(!Instruction(0x85C0).tags().contains(&"load"));
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

/// A spec with in-template display conditionals expands in enum style: the operands feeding the
/// conditions (`oe`, `rc`, `s`) are pre-extracted into the variant. The conditional `render`
/// output itself sits behind `feature = "disasm"` and is checked against the oracle in
/// `chipi-backend-rust/tests/generated.rs`.
#[test]
fn cond_enum_module_decodes() {
    struct Mem([u8; 4]);
    impl Cond::DisasmCtx for Mem {
        fn read_u8(&self, addr: u64) -> u8 {
            self.0[(addr & 3) as usize]
        }
    }

    // add: rc=1 oe=1 rd=3 ra=1 rb=2 (renders as "addo. r3, r1, r2").
    let (add, len) = Cond::decode(0, &Mem(0x0031_2401u32.to_le_bytes()));
    assert_eq!(len, 4);
    assert_eq!(
        add,
        Cond::Instruction::Add {
            rc: 1,
            oe: 1,
            rd: 3,
            ra: 1,
            rb: 2
        }
    );

    let (mov, _) = Cond::decode(0, &Mem(0x1050_0001u32.to_le_bytes()));
    assert_eq!(mov, Cond::Instruction::Mov { s: 1, rd: 5 });
    assert_eq!(mov.name(), "mov");
}
