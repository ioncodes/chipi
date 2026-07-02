//! Helpers shared by the chipi-core integration tests: the deterministic LCG word sampler and
//! a `DisasmCtx` test double. Each test binary uses a subset, hence the dead_code allowance.

#![allow(dead_code)]

use chipi_core::interp::DisasmCtx;

/// One step of the shared LCG (Knuth's MMIX constants). Every sampling loop in the suite draws
/// from this stream so golden counts stay pinned to one generator.
pub fn lcg_step(w: u64) -> u64 {
    w.wrapping_mul(6364136223846793005)
        .wrapping_add(1442695040888963407)
}

/// `count` LCG words from `seed`, each masked to the low `bits`.
pub fn lcg_words(seed: u64, count: usize, bits: u16) -> Vec<u64> {
    let mask = ((1u128 << bits) - 1) as u64;
    let mut w = seed;
    let mut out = Vec::with_capacity(count);
    for _ in 0..count {
        w = lcg_step(w);
        out.push(w & mask);
    }
    out
}

/// The standard word sample: a full sweep for windows up to `full_bits`, otherwise `count`
/// LCG words from seed 0.
pub fn sample_words(bits: u16, full_bits: u16, count: usize) -> Vec<u64> {
    if bits <= full_bits {
        (0..(1u64 << bits)).collect()
    } else {
        lcg_words(0, count, bits)
    }
}

/// A `DisasmCtx` test double: a fixed byte image plus host-reported mode values. Symbols stay
/// unresolved.
pub struct StreamCtx<'a> {
    pub bytes: &'a [u8],
    pub modes: &'a [(String, u64)],
}

impl DisasmCtx for StreamCtx<'_> {
    fn read_u8(&self, addr: u64) -> u8 {
        self.bytes.get(addr as usize).copied().unwrap_or(0)
    }

    fn mode(&self, name: &str) -> u64 {
        self.modes
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| *v)
            .unwrap_or(0)
    }
}
