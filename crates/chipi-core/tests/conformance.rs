//! Conformance for the whole pipeline. Every tracked example compiles and its
//! `encode(decode(w)) == w` round-trip count matches the reference oracle exactly, using the
//! same sampling (full or LCG). The two negative fixtures must be rejected with a
//! `FieldOverlap` error.

mod common;

use chipi_core::{compile, inverse, Isa};

/// Reproduce the oracle's `check --roundtrip` count. Use a full sweep for windows of 16 bits or
/// fewer, otherwise a 200k-word LCG sample. Returns `(valid, ok)`.
fn roundtrip_count(isa: &Isa) -> (u64, u64) {
    let (mut valid, mut ok) = (0u64, 0u64);
    for w in common::sample_words(isa.window_bits(), 16, 200_000) {
        if let Some(b) = inverse::roundtrip(isa, w) {
            valid += 1;
            if b {
                ok += 1;
            }
        }
    }
    (valid, ok)
}

macro_rules! example {
    ($name:literal) => {
        include_str!(concat!("../../../examples/", $name, ".chipi"))
    };
}

fn isa_of(name: &str, src: &str) -> Isa {
    compile(src).unwrap_or_else(|e| panic!("`{name}` failed to compile: {} error(s)", e.len()))
}

#[test]
fn examples_roundtrip_matches_oracle() {
    // (name, src, expected valid==ok count): golden values captured from the reference binary.
    let cases: &[(&str, &str, u64)] = &[
        ("mips", example!("mips"), 19219),
        ("gekko", example!("gekko"), 3123),
        ("gc_dsp", example!("gc_dsp"), 1027),
        ("gb", example!("gb"), 129),
        ("sparse_demo", example!("sparse_demo"), 3072),
        ("tags_demo", example!("tags_demo"), 3243),
        ("cond_demo", example!("cond_demo"), 25178),
        ("names_demo", example!("names_demo"), 1060),
        ("subdecoder_demo", example!("subdecoder_demo"), 512),
        ("modes_demo", example!("modes_demo"), 3),
        ("rv32i", example!("rv32i"), 8813),
        ("riscv", example!("riscv"), 390),
        ("riscv_rvc", example!("riscv_rvc"), 199),
        ("aarch64", example!("aarch64"), 747),
        ("gba_arm", example!("gba_arm"), 32714),
        ("x86_prefix", example!("x86_prefix"), 4),
        ("snes_disasm", example!("snes_disasm"), 3),
        ("fn_let_width", example!("fn_let_width"), 4096),
        ("guard_chain", example!("guard_chain"), 7168),
        ("mode_guard", example!("mode_guard"), 8192),
        ("fetch_expr", example!("fetch_expr"), 3),
        ("axes_demo", example!("axes_demo"), 1280),
        ("for_demo", example!("for_demo"), 6400),
    ];
    for &(name, src, expected) in cases {
        let isa = isa_of(name, src);
        let (valid, ok) = roundtrip_count(&isa);
        assert_eq!(
            ok,
            valid,
            "`{name}`: {} word(s) failed to round-trip",
            valid - ok
        );
        assert_eq!(
            valid, expected,
            "`{name}`: roundtrip count {valid} != oracle {expected}"
        );
    }
}

#[test]
fn negative_fixtures_are_rejected() {
    for (name, src) in [
        ("broken_overlap", example!("broken_overlap")),
        ("broken_gekko_xo", example!("broken_gekko_xo")),
    ] {
        let errs = compile(src).expect_err(&format!("`{name}` should not compile"));
        assert!(
            errs.iter().any(|d| d.code == "FieldOverlap"),
            "`{name}`: expected a FieldOverlap error, got {:?}",
            errs.iter().map(|d| d.code).collect::<Vec<_>>()
        );
    }
}
