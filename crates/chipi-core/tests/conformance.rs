//! Conformance for the whole pipeline. Every tracked example compiles and its
//! `encode(decode(w)) == w` round-trip count matches the reference oracle exactly, using the
//! same sampling (full or LCG). The two negative fixtures must be rejected with a
//! `FieldOverlap` error.

use chipi_core::{compile, inverse, Isa};

/// Reproduce the oracle's `check --roundtrip` count. Use a full sweep for windows of 16 bits or
/// fewer, otherwise a 200k-word LCG sample. Returns `(valid, ok)`.
fn roundtrip_count(isa: &Isa) -> (u64, u64) {
    let bits = isa.window_bits();
    let (mut valid, mut ok) = (0u64, 0u64);
    let tally = |word: u64, valid: &mut u64, ok: &mut u64| {
        if let Some(b) = inverse::roundtrip(isa, word) {
            *valid += 1;
            if b {
                *ok += 1;
            }
        }
    };
    if bits <= 16 {
        for w in 0..(1u64 << bits) {
            tally(w, &mut valid, &mut ok);
        }
    } else {
        let mask = ((1u128 << bits) - 1) as u64;
        let mut w = 0u64;
        for _ in 0..200_000u64 {
            w = w
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            tally(w & mask, &mut valid, &mut ok);
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
