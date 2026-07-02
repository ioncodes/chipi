//! Direct `fn` inversion. The structural inverse must recover field bits without enumerating
//! the input domain, unrecognized shapes must still fall back to enumeration, and an
//! over-cap domain must report the offending fn and its size.
//!
//! Whether an encode took the enumeration fallback is observed through the
//! `inverse::encode_probe` test hook, which reports it per call.

mod common;

use chipi_core::inverse::EncodeError;
use chipi_core::{compile, inverse, Isa};

const SPEC: &str = r#"
decoder Inv {
    width = 16
    bit_order = lsb0
    endian = little
}

selector op [15:12]

fn affine(n: u4) -> u8 {
    return n * 4 + 2
}

fn shifted(n: u4) -> u8 {
    return (n << 3) | 0b101
}

fn hi_nibble(x: u8) -> u4 {
    return x[7:4]
}

fn square(n: u4) -> u8 {
    return n * n
}

aff op=1 n:u4[3:0] v:u8 = affine(n)    | "aff {v}"
shf op=2 n:u4[3:0] v:u8 = shifted(n)   | "shf {v}"
nib op=3 x:u8[7:0] v:u4 = hi_nibble(x) | "nib {v}"
sqr op=4 n:u4[3:0] v:u8 = square(n)    | "sqr {v}"
"#;

const BIG: &str = r#"
decoder Big {
    width = 32
    bit_order = lsb0
    endian = little
}

selector op [31:28]

fn scramble(x: u25) -> u25 {
    return x ^ (x >> 1)
}

scr op=1 x:u25[24:0] v:u25 = scramble(x) | "scr {v}"
"#;

fn instr(isa: &Isa, name: &str) -> usize {
    isa.instrs
        .iter()
        .position(|i| i.name == name)
        .unwrap_or_else(|| panic!("no instruction `{name}`"))
}

/// Encode operand `v`, also reporting whether the enumeration fallback ran.
fn encode_v(isa: &Isa, name: &str, v: i128) -> (Result<u64, EncodeError>, bool) {
    inverse::encode_probe(isa, instr(isa, name), &[("v".to_string(), v)])
}

#[test]
fn direct_inversion_skips_enumeration() {
    let isa = compile(SPEC).unwrap_or_else(|e| panic!("spec failed to compile: {e:?}"));

    // affine: v = n * 4 + 2 recovers n exactly.
    for n in 0..16u64 {
        let (word, enumerated) = encode_v(&isa, "aff", (n * 4 + 2) as i128);
        assert!(!enumerated, "affine n={n} must invert structurally");
        assert_eq!(word.unwrap() & 0xF, n, "affine n={n}");
    }

    // shift/or: v = (n << 3) | 0b101.
    for n in 0..16u64 {
        let (word, enumerated) = encode_v(&isa, "shf", ((n << 3) | 0b101) as i128);
        assert!(!enumerated, "shifted n={n} must invert structurally");
        assert_eq!(word.unwrap() & 0xF, n, "shifted n={n}");
    }

    // slice: v = x[7:4]; the dropped low nibble comes back as the smallest preimage, zero.
    for v in 0..16u64 {
        let (word, enumerated) = encode_v(&isa, "nib", v as i128);
        assert!(!enumerated, "hi_nibble v={v} must invert structurally");
        assert_eq!(word.unwrap() & 0xFF, v << 4, "hi_nibble v={v}");
    }

    // rv32i has no fn operands at all; a full encoder round-trip sweep must stay off the
    // enumeration path too.
    let rv32i = compile(include_str!("../../../examples/rv32i.chipi"))
        .unwrap_or_else(|e| panic!("rv32i failed to compile: {e:?}"));
    for w in common::lcg_words(0, 50_000, 32) {
        let d = chipi_core::interp::decode(&rv32i, w);
        let Some(idx) = d.instr_index else { continue };
        let values: Vec<(String, i128)> =
            d.fields.iter().map(|f| (f.name.clone(), f.value)).collect();
        let (re, enumerated) = inverse::encode_probe(&rv32i, idx, &values);
        assert!(!enumerated, "rv32i word {w:#x} must not enumerate");
        if let Ok(re) = re {
            let care = inverse::care_mask(&rv32i, idx);
            assert!(
                (w & care) == (re & care),
                "rv32i word {w:#x} failed to round-trip"
            );
        }
    }

    // A value the chain cannot produce is rejected; the fallback confirms it is unencodable.
    assert!(matches!(
        encode_v(&isa, "aff", 3).0,
        Err(EncodeError::NotEncodable(_))
    ));

    // square is not structurally invertible; the enumeration fallback still recovers n.
    let (word, enumerated) = encode_v(&isa, "sqr", 81);
    assert_eq!(word.unwrap() & 0xF, 9);
    assert!(
        enumerated,
        "unrecognized shapes must fall back to enumeration"
    );
}

#[test]
fn domain_too_large_names_the_fn() {
    let isa = compile(BIG).unwrap_or_else(|e| panic!("spec failed to compile: {e:?}"));
    let err = encode_v(&isa, "scr", 1).0.unwrap_err();
    assert_eq!(
        err,
        EncodeError::DomainTooLarge {
            func: "scramble".to_string(),
            operand: "v".to_string(),
            domain: 1 << 25,
        }
    );

    let msg = err.to_string();
    assert!(msg.contains("scramble"), "message names the fn: {msg}");
    assert!(msg.contains("33554432"), "message carries the size: {msg}");
}
