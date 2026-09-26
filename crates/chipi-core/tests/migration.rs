//! Proof that the 1.0 corpus migration preserved behavior where it meant to.
//!
//! The SNES specs were rewritten for 1.0: dotted identity axes replaced the underscore
//! naming, ricoh's m8/m16 and x8/x16 immediate pairs collapsed into single `fetch(expr)`
//! leaves, and spc700's bbs/bbc/set1/clr1/tcall fan-outs became `for` blocks. The
//! pre-migration spec text is checked in under `tests/migration_old/`; this suite compiles
//! both versions and compares them differentially instead of against goldens:
//!
//! - stream level: `disasm_ctx` through a mode-carrying context must produce the same
//!   text and total length for every opcode, mode combination and operand-byte sample.
//!   This is the check that proves a collapsed `fetch(m ? 8 : 16)` leaf behaves exactly
//!   like the old split m8/m16 pair, since leaf names do not appear in the output.
//! - word level: `decode_mode` must agree on validity and on the leaf identity, modulo
//!   the intended renames (dots for underscores; the collapsed immediates drop their
//!   `_m8`/`_m16`/`_x8`/`_x16` suffix).
//!
//! The GameCube specs migrated differently: gekko factored its recurring operand
//! triples into `form`/`uses` declarations (no renames, no leaf changes intended), and
//! dsp was left as-is after review: its families vary by lettered suffix (lrr/lrrd/
//! lrri/lrrn), not by index, so a `for` block collapses nothing but the two-leaf
//! nx_0/nx_1 pair, which it would not shorten. Both are fixed-width and mode-free
//! (no fetch, no modes), so the proof is a
//! word-level sweep: validity, leaf name, disassembly text and decoded length must be
//! identical for every sampled window value.
//!
//! Both spec versions are checked in, so the suite is fully self-contained: the migrated
//! specs are the vendored corpus under `corpus/`, the pre-1.0 text sits in
//! `tests/migration_old/`.

mod common;

use chipi_core::interp::{decode, decode_mode, disasm_ctx};
use chipi_core::{compile, Isa};
use common::StreamCtx;

const RICOH_OLD: &str = include_str!("migration_old/ricoh_5a22_pre1.chipi");
const SPC700_OLD: &str = include_str!("migration_old/spc700_pre1.chipi");
const GEKKO_OLD: &str = include_str!("migration_old/gekko_pre1.chipi");
const DSP_OLD: &str = include_str!("migration_old/dsp_pre1.chipi");

const RICOH_NEW: &str = include_str!("../../../corpus/ricoh_5a22.chipi");
const SPC700_NEW: &str = include_str!("../../../corpus/spc700.chipi");
const GEKKO_NEW: &str = include_str!("../../../corpus/gekko.chipi");
const DSP_NEW: &str = include_str!("../../../corpus/dsp.chipi");

/// Every assignment of values to the spec's modes (one empty assignment when there are none).
fn mode_assignments(isa: &Isa) -> Vec<Vec<(String, u64)>> {
    (0..isa.mode_combos())
        .map(|combo| {
            isa.mode_subst(combo)
                .into_iter()
                .map(|(n, v, _)| (n, v))
                .collect()
        })
        .collect()
}

/// The old spec's leaf identity, normalized to the new naming: dots become underscores
/// on the new side; the collapsed per-mode immediate suffixes drop on the old side, with the
/// index-register family mapping to the distinct `immx` form (`ldx_imm_x8` -> `ldx_immx`).
fn normalize_old(name: &str) -> String {
    for suffix in ["_m8", "_m16"] {
        if let Some(base) = name.strip_suffix(suffix) {
            return base.to_string();
        }
    }
    for suffix in ["_x8", "_x16"] {
        if let Some(base) = name.strip_suffix(suffix) {
            return format!("{base}x");
        }
    }
    name.to_string()
}

fn compare(old: &Isa, new: &Isa, spec: &str) {
    assert_eq!(
        old.window_bits(),
        new.window_bits(),
        "{spec}: window width changed"
    );

    let assignments = mode_assignments(new);

    // Word level: same validity, same leaf modulo the intended renames. The mode combos
    // of old and new agree because the mode declarations are unchanged.
    for assign in &assignments {
        let vals: Vec<u64> = assign.iter().map(|(_, v)| *v).collect();
        let combo = new.pack_modes(&vals) as usize;
        for w in 0..(1u64 << new.window_bits()) {
            let od = decode_mode(old, combo, w);
            let nd = decode_mode(new, combo, w);
            assert_eq!(
                od.is_valid(),
                nd.is_valid(),
                "{spec}: validity differs at combo {combo} word {w:#04x}"
            );
            if !od.is_valid() {
                continue;
            }
            let old_name = normalize_old(&od.opcode_name);
            let new_name = nd.opcode_name.replace('.', "_");
            assert_eq!(
                old_name, new_name,
                "{spec}: leaf identity differs at combo {combo} word {w:#04x}"
            );
        }
    }

    // Stream level: identical disassembly text and total length for every opcode byte,
    // mode assignment and a deterministic sample of trailing operand bytes. Collapsed
    // fetch(expr) leaves must reproduce the old split leaves' text and length exactly.
    for assign in &assignments {
        for op in 0..=255u8 {
            let mut lcg: u64 = 0x9E3779B97F4A7C15 ^ (op as u64);
            for _ in 0..8 {
                let mut bytes = [0u8; 8];
                bytes[0] = op;
                for b in bytes[1..].iter_mut() {
                    lcg = common::lcg_step(lcg);
                    *b = (lcg >> 33) as u8;
                }
                let ctx = StreamCtx {
                    bytes: &bytes,
                    modes: assign,
                };
                let (otext, olen) = disasm_ctx(old, 0, &ctx);
                let (ntext, nlen) = disasm_ctx(new, 0, &ctx);
                assert_eq!(
                    (otext.as_str(), olen),
                    (ntext.as_str(), nlen),
                    "{spec}: disasm_ctx differs at op {op:#04x} modes {assign:?} bytes {bytes:02x?}"
                );
            }
        }
    }
}

/// Word-level comparison for the fixed-width, mode-free GameCube specs. The forms
/// adoption in gekko may reorder field declarations within a leaf, which must not be
/// observable: names, disassembly and decoded length are compared verbatim (dots
/// normalized to underscores, though neither GC migration renames anything).
fn compare_words(old: &Isa, new: &Isa, words: &[u64], spec: &str) {
    assert_eq!(
        old.window_bits(),
        new.window_bits(),
        "{spec}: window width changed"
    );

    for &w in words {
        let od = decode(old, w);
        let nd = decode(new, w);
        assert_eq!(
            od.is_valid(),
            nd.is_valid(),
            "{spec}: validity differs at word {w:#010x}"
        );
        if !od.is_valid() {
            continue;
        }
        assert_eq!(
            od.opcode_name,
            nd.opcode_name.replace('.', "_"),
            "{spec}: leaf identity differs at word {w:#010x}"
        );
        assert_eq!(
            od.len_bytes, nd.len_bytes,
            "{spec}: decoded length differs at word {w:#010x}"
        );
        assert_eq!(
            od.disasm, nd.disasm,
            "{spec}: disassembly differs at word {w:#010x}"
        );
    }
}

fn compile_pair(old_src: &str, new_src: &str, spec: &str) -> (Isa, Isa) {
    let old = compile(old_src)
        .unwrap_or_else(|errs| panic!("pre-1.0 `{spec}` failed to compile: {errs:?}"));
    let new = compile(new_src)
        .unwrap_or_else(|errs| panic!("migrated `{spec}` failed to compile: {errs:?}"));

    (old, new)
}

#[test]
fn ricoh_5a22_migration_is_behavior_preserving() {
    let (old, new) = compile_pair(RICOH_OLD, RICOH_NEW, "ricoh_5a22");
    compare(&old, &new, "ricoh_5a22");
}

#[test]
fn spc700_migration_is_behavior_preserving() {
    let (old, new) = compile_pair(SPC700_OLD, SPC700_NEW, "spc700");
    compare(&old, &new, "spc700");
}

#[test]
fn gekko_migration_is_behavior_preserving() {
    let (old, new) = compile_pair(GEKKO_OLD, GEKKO_NEW, "gekko");

    // A 200k-word LCG sample over the 32-bit window, the conformance suite's sampling.
    let words = common::lcg_words(0, 200_000, 32);

    compare_words(&old, &new, &words, "gekko");
}

#[test]
fn dsp_migration_is_behavior_preserving() {
    let (old, new) = compile_pair(DSP_OLD, DSP_NEW, "dsp");

    // The dsp window is 32 bits, but only the high half is the opcode word; the low
    // half is a second immediate/address word for the two-word instructions. Sweep
    // every opcode word with three low-half samples each. This also pins the length
    // table (len_bytes) through `compare_words`.
    let mut words = Vec::with_capacity(3 << 16);
    let mut lcg = 0x243F_6A88_85A3_08D3u64;
    for hi in 0..=0xFFFFu64 {
        lcg = common::lcg_step(lcg);
        words.push(hi << 16);
        words.push(hi << 16 | 0xFFFF);
        words.push(hi << 16 | (lcg >> 33) & 0xFFFF);
    }

    compare_words(&old, &new, &words, "dsp");
}
