//! Enum-style expansions of specs whose decoder names are already taken by the newtype
//! instances in `isa.rs`. The `isa!` module is named after the decoder, so the same spec in
//! both styles needs a second test binary.

chipi_macros::isa!("../../examples/axes_demo.chipi", style = enum);

/// The identity axes work through the enum backend too: the decoded variant fixes the leaf, so
/// `mnemonic()` and `form()` are direct table lookups through `opcode_id()`, with no mode combo.
#[test]
fn enum_axes_expose_identity() {
    use Axes::{Form, Instruction, Load, Mnemonic};

    struct Mem([u8; 2]);
    impl Axes::DisasmCtx for Mem {
        fn read_u8(&self, addr: u64) -> u8 {
            self.0[(addr & 1) as usize]
        }
    }

    // Word 0xA9B7 (little endian): lda.imm with v = 0xB7, grouped under `dispatch load`.
    let (lda, len) = Axes::decode(0, &Mem(0xA9B7u16.to_le_bytes()));
    assert_eq!(len, 2);
    assert_eq!(lda, Instruction::Load(Load::LdaImm { v: 0xB7 }));
    assert_eq!(lda.mnemonic(), Mnemonic::Lda);
    assert_eq!(lda.form(), Form::Imm);
    assert_eq!(lda.mnemonic().name(), "lda");
    assert_eq!(lda.form().name(), "imm");
    assert_eq!(lda.name(), "lda.imm");
    assert!(lda.tags().contains(&"load"));

    let (sta, _) = Axes::decode(0, &Mem(0x85C0u16.to_le_bytes()));
    assert_eq!(sta, Instruction::StaDp { dp: 0xC0 });
    assert_eq!(sta.mnemonic(), Mnemonic::Sta);
    assert_eq!(sta.form(), Form::Dp);
    assert!(!sta.tags().contains(&"load"));

    let (nop, _) = Axes::decode(0, &Mem(0xEA00u16.to_le_bytes()));
    assert_eq!(nop.mnemonic(), Mnemonic::Nop);
    assert_eq!(nop.form(), Form::None);

    let (bad, _) = Axes::decode(0, &Mem([0x00, 0x00]));
    assert_eq!(bad.mnemonic(), Mnemonic::Invalid);
    assert_eq!(bad.form(), Form::None);
}
