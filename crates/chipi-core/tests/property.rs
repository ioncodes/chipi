//! Property / fuzz suite. Throw a large stream of random words at each ISA and check that
//! the reference evaluator never panics and the encoder is the exact inverse of decode.
//! No dependencies: an LCG generates the words.

use chipi_core::{compile, interp, inverse, Isa};

macro_rules! ex {
    ($n:literal) => {
        (
            $n,
            include_str!(concat!("../../../examples/", $n, ".chipi")),
        )
    };
}

const EXAMPLES: &[(&str, &str)] = &[
    ex!("mips"),
    ex!("rv32i"),
    ex!("riscv"),
    ex!("gekko"),
    ex!("aarch64"),
    ex!("gba_arm"),
    ex!("gb"),
    ex!("gc_dsp"),
    ex!("sparse_demo"),
    ex!("tags_demo"),
    ex!("cond_demo"),
    ex!("modes_demo"),
    ex!("riscv_rvc"),
    ex!("x86_prefix"),
    ex!("snes_disasm"),
];

/// Visit `n` words drawn from the window's value space.
fn sweep(isa: &Isa, n: u64, mut f: impl FnMut(u64)) {
    let bits = isa.window_bits();
    if bits <= 16 {
        let total = 1u64 << bits;
        for w in 0..total.min(n.max(total)) {
            f(w);
        }
    } else {
        let mask = ((1u128 << bits) - 1) as u64;
        let mut w = 0x9e37_79b9_7f4a_7c15u64;
        for _ in 0..n {
            w = w
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            f(w & mask);
        }
    }
}

const N: u64 = 1_000_000;

#[test]
fn oracle_never_panics() {
    for &(name, src) in EXAMPLES {
        let isa = compile(src).unwrap_or_else(|_| panic!("`{name}` compile"));
        let combos = isa.mode_combos() as usize;
        sweep(&isa, N, |w| {
            let d = interp::decode(&isa, w);
            // touch every reported operand + the disasm so rendering paths run too
            let _ = d.disasm;
            let _ = d.fields.len();
            for c in 0..combos {
                let _ = interp::decode_mode(&isa, c, w);
            }
        });
    }
}

#[test]
fn encoder_is_exact_inverse() {
    for &(name, src) in EXAMPLES {
        let isa = compile(src).unwrap_or_else(|_| panic!("`{name}` compile"));
        sweep(&isa, N, |w| {
            if let Some(ok) = inverse::roundtrip(&isa, w) {
                assert!(ok, "`{name}`: word {w:#x} did not round-trip");
            }
        });
    }
}

#[test]
fn stream_decode_never_panics() {
    // Feed arbitrary byte slices through the prefix/stream path of the stream-y ISAs.
    for &(name, src) in &[
        (
            "x86_prefix",
            include_str!("../../../examples/x86_prefix.chipi"),
        ),
        (
            "snes_disasm",
            include_str!("../../../examples/snes_disasm.chipi"),
        ),
        ("gb", include_str!("../../../examples/gb.chipi")),
    ] {
        let isa = compile(src).unwrap_or_else(|_| panic!("`{name}` compile"));
        let mut w = 1u64;
        for _ in 0..100_000u64 {
            w = w
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            let bytes = w.to_le_bytes();
            let _ = interp::decode_stream(&isa, &bytes);
            let _ = interp::decode_stream(&isa, &bytes[..1]);
        }
    }
}
