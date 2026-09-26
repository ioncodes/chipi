//! The production-spec gate. The real bar for language and backend changes is the ISA corpus
//! vendored under `corpus/` at the repo root (upstream: the chipi-spec repository), not just
//! examples/. This suite compiles each production spec, pins its leaf count, and diffs a
//! word-sample decode transcript against a checked-in golden file, so an unintended behavior
//! change to any production decoder fails CI. Regenerate goldens after an INTENDED change with
//! CHIPI_BLESS=1.

mod common;

use chipi_core::interp::{decode, decode_mode};
use chipi_core::{compile, Isa};
use std::path::PathBuf;

fn golden_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/corpus_golden")
        .join(format!("{name}.txt"))
}

/// A deterministic word sample: every word for windows up to 12 bits, else a 4096-word LCG.
fn sample_words(isa: &Isa) -> Vec<u64> {
    common::sample_words(isa.window_bits(), 12, 4096)
}

/// One line per (combo, word): `combo word opcode|disasm`.
fn transcript(isa: &Isa) -> String {
    let combos = if isa.modes.is_empty() {
        1
    } else {
        isa.mode_combos() as usize
    };
    let words = sample_words(isa);

    let mut out = String::new();
    for combo in 0..combos {
        for &w in &words {
            let d = if isa.modes.is_empty() {
                decode(isa, w)
            } else {
                decode_mode(isa, combo, w)
            };
            let disasm = d.disasm.unwrap_or_else(|| "(invalid)".to_string());
            out.push_str(&format!("{combo} {w:#x} {}|{}\n", d.opcode_name, disasm));
        }
    }
    out
}

fn check_spec(src: &str, name: &str, leaves: usize) {
    let isa = compile(src).unwrap_or_else(|errs| {
        panic!("`{name}` failed to compile: {} error(s)", errs.len());
    });
    assert_eq!(isa.instrs.len(), leaves, "`{name}` leaf count changed");

    let got = transcript(&isa);
    let gpath = golden_path(name);
    if std::env::var("CHIPI_BLESS").is_ok() {
        std::fs::create_dir_all(gpath.parent().unwrap()).unwrap();
        std::fs::write(&gpath, &got).unwrap();
        eprintln!("blessed {}", gpath.display());
        return;
    }
    let Ok(want) = std::fs::read_to_string(&gpath) else {
        panic!(
            "`{name}` has no golden transcript at {}; run with CHIPI_BLESS=1 to create it",
            gpath.display()
        );
    };
    if got != want {
        let diff = got
            .lines()
            .zip(want.lines())
            .enumerate()
            .find(|(_, (g, w))| g != w);
        match diff {
            Some((i, (g, w))) => panic!(
                "`{name}` decode transcript changed at line {}: got `{g}`, golden `{w}`",
                i + 1
            ),
            None => panic!("`{name}` decode transcript changed in length"),
        }
    }
}

macro_rules! corpus {
    ($name:literal) => {
        include_str!(concat!("../../../corpus/", $name, ".chipi"))
    };
}

#[test]
fn corpus_ricoh_5a22() {
    check_spec(corpus!("ricoh_5a22"), "ricoh_5a22", 256);
}

#[test]
fn corpus_spc700() {
    check_spec(corpus!("spc700"), "spc700", 256);
}

#[test]
fn corpus_gekko() {
    check_spec(corpus!("gekko"), "gekko", 228);
}

#[test]
fn corpus_dsp() {
    check_spec(corpus!("dsp"), "dsp", 126);
}
