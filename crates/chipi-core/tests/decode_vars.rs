//! Decode variables: host modes and prefix-assigned context fields are readable by guards
//! (and `length` arms), so a prefix can actually change what decodes. Word-level `decode`
//! uses every variable's declared default.

mod common;

use chipi_core::compile;
use chipi_core::interp::{decode, decode_mode, decode_stream};
use common::StreamCtx;

macro_rules! example {
    ($name:literal) => {
        include_str!(concat!("../../../examples/", $name, ".chipi"))
    };
}

#[test]
fn prefix_context_changes_decode() {
    let isa = compile(example!("x86_prefix")).expect("x86_prefix compiles");

    // No prefix: 0x90 is nop, 0x50 pushes rax.
    let d = decode_stream(&isa, &[0x90]);
    assert_eq!(d.opcode_name, "nop");
    let d = decode_stream(&isa, &[0x50]);
    assert_eq!(d.opcode_name, "push");

    // REX with B set (0x49): the same 0x90 byte is now the r8 exchange.
    let d = decode_stream(&isa, &[0x49, 0x90]);
    assert_eq!(d.opcode_name, "xchg_r8");
    assert_eq!(d.prefix_len, 1);

    // REX without B (0x48) leaves rex_b at 0, so 0x90 stays nop.
    let d = decode_stream(&isa, &[0x48, 0x90]);
    assert_eq!(d.opcode_name, "nop");

    // The 0x66 operand-size override flips the push width.
    let d = decode_stream(&isa, &[0x66, 0x50]);
    assert_eq!(d.opcode_name, "push16");
    assert_eq!(d.disasm.as_deref(), Some("push ax"));

    // Word-level decode uses the context defaults (no prefix seen).
    assert_eq!(decode(&isa, 0x90).opcode_name, "nop");
    assert_eq!(decode(&isa, 0x50).opcode_name, "push");
}

#[test]
fn mode_reads_in_guards_follow_the_combo() {
    let isa = compile(example!("mode_guard")).expect("mode_guard compiles");

    // combo 0: m = 0.
    let d = decode_mode(&isa, 0, 0x10ab);
    assert_eq!(d.opcode_name, "wide");
    // combo 1: m = 1.
    let d = decode_mode(&isa, 1, 0x10ab);
    assert_eq!(d.opcode_name, "narrow");

    // `add` mixes a mode read and a field read: valid everywhere at m = 0, only for
    // matching registers at m = 1.
    assert_eq!(decode_mode(&isa, 0, 0x2012).opcode_name, "add");
    assert_eq!(decode_mode(&isa, 1, 0x2012).opcode_name, "Invalid");
    assert_eq!(decode_mode(&isa, 1, 0x2033).opcode_name, "add");

    // Word-level decode uses the mode default (m = 0).
    assert_eq!(decode(&isa, 0x10ab).opcode_name, "wide");
}

/// The collapsed one-leaf `fetch(m ? 8 : 16)` form decodes exactly like the split
/// two-leaf `m=1 fetch(8)` / `m=0 fetch(16)` form it replaces, for both mode values.
#[test]
fn fetch_expr_matches_split_leaves() {
    let collapsed = compile(example!("fetch_expr")).expect("fetch_expr compiles");
    let split_src = "\
decoder MiniM {
    width = 8
    bit_order = lsb0
    endian = little
    mode m: bool = 1
}

selector op [0:7]

nop         op=0xEA                       | \"nop\"
lda_imm_m8  m=1 op=0xA9 imm:u8 = fetch(8) | \"lda #${imm:02x}\"
lda_imm_m16 m=0 op=0xA9 imm:u16 = fetch(16) | \"lda #${imm:04x}\"
ldx_imm     op=0xA2 imm2:u16 = fetch(16)  | \"ldx #${imm2:04x}\"
";
    let split = compile(split_src).expect("split spec compiles");

    let streams: &[&[u8]] = &[
        &[0xA9, 0x42, 0x99],
        &[0xA9, 0x34, 0x12],
        &[0xA2, 0xCD, 0xAB],
        &[0xEA, 0x00, 0x00],
    ];
    for m in [0u64, 1] {
        let modes = [("m".to_string(), m)];
        for bytes in streams {
            let ctx = StreamCtx {
                bytes,
                modes: &modes,
            };
            let got = chipi_core::interp::disasm_ctx(&collapsed, 0, &ctx);
            let want = chipi_core::interp::disasm_ctx(&split, 0, &ctx);
            assert_eq!(got, want, "m={m} bytes={bytes:02x?}");
        }
    }

    // Spot checks: the collapsed leaf really narrows under m = 1.
    let m1 = [("m".to_string(), 1u64)];
    let ctx = StreamCtx {
        bytes: &[0xA9, 0x42, 0x99],
        modes: &m1,
    };
    assert_eq!(
        chipi_core::interp::disasm_ctx(&collapsed, 0, &ctx),
        ("lda #$42".to_string(), 2)
    );
    let m0 = [("m".to_string(), 0u64)];
    let ctx = StreamCtx {
        bytes: &[0xA9, 0x34, 0x12],
        modes: &m0,
    };
    assert_eq!(
        chipi_core::interp::disasm_ctx(&collapsed, 0, &ctx),
        ("lda #$1234".to_string(), 3)
    );
}

#[test]
fn fetch_width_must_read_modes_only() {
    let bad_field = "decoder T { width = 8 bit_order = lsb0 endian = little }\n\
                     selector op [0:7]\n\
                     a op=1 sz:u1[0] x:u16 = fetch(sz ? 8 : 16) | \"a\"\n";
    let errs = compile(bad_field).expect_err("field-reading fetch width should be rejected");
    assert!(
        errs.iter().any(|d| d.code == "BadFetch"),
        "expected BadFetch, got {:?}",
        errs.iter().map(|d| d.code).collect::<Vec<_>>()
    );

    let bad_range = "decoder T { width = 8 bit_order = lsb0 endian = little mode m: bool = 0 }\n\
                     selector op [0:7]\n\
                     a op=1 x:u16 = fetch(m ? 8 : 0) | \"a\"\n";
    let errs = compile(bad_range).expect_err("zero-width fetch should be rejected");
    assert!(
        errs.iter().any(|d| d.code == "BadFetch"),
        "expected BadFetch, got {:?}",
        errs.iter().map(|d| d.code).collect::<Vec<_>>()
    );
}

/// A `for` expansion is byte-identical to the hand-written fan-out it replaces.
#[test]
fn for_expansion_matches_handwritten() {
    let expanded = compile(example!("for_demo")).expect("for_demo compiles");

    let mut hand = String::from(
        "decoder ForDemo { width = 16 bit_order = lsb0 endian = little }\nselector op [8:15]\n",
    );
    for n in 0..8 {
        hand.push_str(&format!(
            "bbs_b{n} op={:#04x} dp:u8[0:7] | \"bbs ${{dp:02x}}.{n}\"\n",
            0x03 + n * 0x20
        ));
    }
    for n in 0..16 {
        hand.push_str(&format!(
            "tcall_{n} op={:#04x} | \"tcall {n}\"\n",
            0x01 + n * 0x10
        ));
    }
    hand.push_str("nop op=0xEA | \"nop\"\n");
    let handwritten = compile(&hand).expect("handwritten spec compiles");

    for w in 0..0x10000u64 {
        let a = decode(&expanded, w);
        let b = decode(&handwritten, w);
        assert_eq!(
            (a.opcode_name.clone(), a.disasm.clone()),
            (b.opcode_name.clone(), b.disasm.clone()),
            "word {w:#06x}"
        );
    }
}

#[test]
fn operand_shadowing_a_variable_is_rejected() {
    let src = "decoder T { width = 16 bit_order = lsb0 endian = little mode m: bool = 0 }\n\
               selector op [12:15]\n\
               a op=1 m:u4[3:0] | \"a {m}\"\n";
    let errs = compile(src).expect_err("shadowing operand should be rejected");
    assert!(
        errs.iter().any(|d| d.code == "DuplicateName"),
        "expected DuplicateName, got {:?}",
        errs.iter().map(|d| d.code).collect::<Vec<_>>()
    );
}
