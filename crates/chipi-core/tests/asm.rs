//! Tests for the text assembler: multibyte emission for `fetch(N)` operands, width-based candidate
//! disambiguation and a round-trip sweep over the window-word ISAs.

use chipi_core::assemble::{assemble_inst, roundtrip_asm};
use chipi_core::compile;
use chipi_core::interp::{self, DisasmCtx};

/// A `DisasmCtx` that reads the instruction back out of a freshly assembled byte buffer.
struct Bytes<'a>(&'a [u8]);
impl DisasmCtx for Bytes<'_> {
    fn read_u8(&self, addr: u64) -> u8 {
        self.0.get(addr as usize).copied().unwrap_or(0)
    }
}

// A small 65816-style slice: 8bit opcode window, operands fetched little-endian after it.
// Covers u8/u16/u24/i8 fetch widths, a two-operand instruction and width disambiguation.
const SNES: &str = r#"
decoder T { width = 8 bit_order = lsb0 endian = little }
selector op [0:7]
operand b8  = u8  fetch(8)
operand w16 = u16 fetch(16)
operand l24 = u24 fetch(24)
operand r8  = i8  fetch(8)
nop     op=0xEA | "nop"
lda_dp  op=0xA5 addr:b8  | "lda ${addr:02x}"
lda_abs op=0xAD addr:w16 | "lda ${addr:04x}"
lda_abl op=0xAF addr:l24 | "lda ${addr:06x}"
mvn     op=0x54 dst:b8 src:b8 | "mvn ${src:02x},${dst:02x}"
brr     op=0x90 disp:r8  | "brr ${disp:02x}"
"#;

#[test]
fn snes_fetch_byte_emission() {
    let isa = compile(SNES).expect("spec compiles");

    // (line, expected full byte stream). The operand order is little-endian and `lda $1234`
    // selects the 16bit `lda_abs`, not the 8bit `lda_dp`, because the value does not fit a byte.
    let cases: &[(&str, &[u8])] = &[
        ("nop", &[0xEA]),
        ("lda $12", &[0xA5, 0x12]),
        ("lda $1234", &[0xAD, 0x34, 0x12]),
        ("lda $abcdef", &[0xAF, 0xEF, 0xCD, 0xAB]),
        ("mvn $12,$34", &[0x54, 0x34, 0x12]),
        ("brr $f0", &[0x90, 0xF0]),
    ];

    for (line, want) in cases {
        let a = assemble_inst(&isa, line).unwrap_or_else(|e| panic!("`{line}`: {e}"));
        assert_eq!(&a.bytes[..], *want, "byte stream for `{line}`");

        // Round-trip: assemble bytes, disassemble them with context and get the original line back.
        let (text, len) = interp::disasm_ctx(&isa, 0, &Bytes(&a.bytes));
        assert_eq!(len as usize, a.bytes.len(), "length for `{line}`");
        assert_eq!(&text, line, "disasm round-trip for `{line}`");
    }
}

#[test]
fn snes_rejects_unassemblable() {
    let isa = compile(SNES).expect("spec compiles");
    // Wider than any matching form (max is u24).
    assert!(assemble_inst(&isa, "lda $12345678").is_err());
    // Unknown mnemonic.
    assert!(assemble_inst(&isa, "zzz $1").is_err());
    // A `:rel` or `:sym` operand needs decode context and here there is none. So a bare relative
    // branch template would not match. `brr` above uses plain hex on purpose so it can assemble.
}

/// Big-endian fetch emission: the operand bytes go out most-significant first.
#[test]
fn fetch_bytes_respect_big_endian() {
    let be = r#"
        decoder B { width = 8 bit_order = lsb0 endian = big }
        selector op [0:7]
        operand w16 = u16 fetch(16)
        jmp op=0x4C addr:w16 | "jmp ${addr:04x}"
    "#;
    let isa = compile(be).expect("spec compiles");
    let a = assemble_inst(&isa, "jmp $1234").expect("assembles");
    assert_eq!(&a.bytes[..], &[0x4C, 0x12, 0x34]); // big-endian: high byte first
}

/// For window-word ISAs (the whole instruction is the decode window), `assemble` must invert
/// disassembly exactly. Sweep a fixed sample and assert that no valid, renderable word ever
/// re-assembles to a different word.
fn sweep(name: &str, src: &str, samples: u64) {
    let isa =
        compile(src).unwrap_or_else(|e| panic!("`{name}` failed to compile: {} err", e.len()));
    let bits = isa.window_bits();
    let exhaustive = bits <= 16;
    let lim = if exhaustive { 1u64 << bits } else { samples };

    let mut w = 0u64;
    let mut checked = 0u64;
    for i in 0..lim {
        let word = if exhaustive {
            i
        } else {
            w = w
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            w & (((1u128 << bits) - 1) as u64)
        };
        if let Some(ok) = roundtrip_asm(&isa, word) {
            checked += 1;
            assert!(ok, "`{name}`: asm did not invert disasm for word {word:#x}");
        }
    }
    assert!(checked > 0, "`{name}`: nothing round-tripped");
}

#[test]
fn window_asm_round_trips() {
    sweep(
        "mips",
        include_str!("../../../examples/mips.chipi"),
        100_000,
    );
    sweep(
        "rv32i",
        include_str!("../../../examples/rv32i.chipi"),
        100_000,
    );
    sweep(
        "riscv",
        include_str!("../../../examples/riscv.chipi"),
        100_000,
    );
    sweep("gb", include_str!("../../../examples/gb.chipi"), 0);
    sweep("gc_dsp", include_str!("../../../examples/gc_dsp.chipi"), 0);
}
