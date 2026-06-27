//! Differential conformance: for each tracked example, emit the C++17 decoder, compile it with
//! `g++ -std=c++17`, run it over a fixed word sample and check it produces the same
//! `<opcode-name>|<disasm>` for every word as the `chipi_core` oracle (`decode`/`decode_mode`).

use chipi_backend_cpp::emit_cpp;
use chipi_core::interp::{decode, decode_mode};
use chipi_core::{compile, Isa};
use std::path::{Path, PathBuf};
use std::process::Command;

macro_rules! example {
    ($name:literal) => {
        include_str!(concat!("../../../examples/", $name, ".chipi"))
    };
}

/// Regression: the C++ backend has no stream context, so a `fetch(N)` operand must produce a clear
/// `#error` rather than a call to an undefined `fn_fetch`.
#[test]
fn cpp_refuses_fetch_operands() {
    let src = "decoder T { width = 8 bit_order = lsb0 endian = little }\n\
               selector op [0:7]\n\
               a op=0x01 x:u16 = fetch(16) | \"a ${x:04x}\"\n";
    let isa = compile(src).expect("spec compiles");
    let out = emit_cpp(&isa);
    assert!(
        out.contains("#error"),
        "cpp should refuse fetch specs:\n{out}"
    );
    assert!(
        !out.contains("fn_fetch"),
        "cpp must not emit an undefined fn_fetch:\n{out}"
    );
}

/// Fixed word sample, matching `crates/chipi-core/tests/conformance.rs`:
/// every word for windows of 16bit or fewer, else a 200k-word LCG (seed 0).
fn sample_words(isa: &Isa) -> Vec<u64> {
    let bits = isa.window_bits();
    if bits <= 16 {
        (0..(1u64 << bits)).collect()
    } else {
        let mask = ((1u128 << bits) - 1) as u64;
        let mut w = 0u64;
        let mut out = Vec::with_capacity(200_000);
        for _ in 0..200_000u64 {
            w = w
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            out.push(w & mask);
        }
        out
    }
}

/// The oracle's `<opcode-name>|<disasm>` line for a word at mode combination `combo`.
fn oracle_line(isa: &Isa, combo: usize, word: u64) -> String {
    let d = if isa.modes.is_empty() {
        decode(isa, word)
    } else {
        decode_mode(isa, combo, word)
    };
    let disasm = if d.is_valid() {
        d.disasm.clone().unwrap_or_default()
    } else {
        "(invalid)".to_string()
    };
    format!("{}|{}", d.opcode_name, disasm)
}

fn target_dir() -> PathBuf {
    // A per-test scratch dir that survives the run for debugging on failure.
    std::env::var("CARGO_TARGET_TMPDIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| std::env::temp_dir())
}

fn write_harness(dir: &Path, isa: &Isa, header: &str, words: &[u64], combos: usize) {
    std::fs::create_dir_all(dir).unwrap();
    std::fs::write(dir.join("dec.hpp"), header).unwrap();

    let modal = !isa.modes.is_empty();

    // The word literals, comma-separated.
    let mut words_src = String::new();
    for w in words {
        words_src.push_str(&format!("{w}ull,"));
    }

    let (name_call, call) = if modal {
        (
            "chipi::opcode_name((unsigned)c, (HANDLE)w)".to_string(),
            "chipi::disasm((unsigned)c, (HANDLE)w)".to_string(),
        )
    } else {
        (
            "chipi::opcode_name((HANDLE)w)".to_string(),
            "chipi::disasm((HANDLE)w)".to_string(),
        )
    };

    let handle = match isa.handle_bits() {
        8 => "uint8_t",
        16 => "uint16_t",
        32 => "uint32_t",
        _ => "uint64_t",
    };

    // Outer loop over mode combinations (1 for non-modal), inner over words. Lines are emitted
    // combo-major to match the Rust comparison order.
    let main = format!(
        r#"#include "dec.hpp"
#include <cstdio>
#include <cstdint>
#include <string>
using HANDLE = {handle};
static const unsigned long long WORDS[] = {{ {words_src} }};
int main() {{
    std::string out;
    out.reserve(1u << 20);
    for (unsigned c = 0; c < {combos}u; c++) {{
        for (size_t i = 0; i < sizeof(WORDS)/sizeof(WORDS[0]); i++) {{
            unsigned long long w = WORDS[i];
            (void)c;
            out += {name_call};
            out += '|';
            out += {call};
            out += '\n';
        }}
    }}
    fwrite(out.data(), 1, out.size(), stdout);
    return 0;
}}
"#
    );
    std::fs::write(dir.join("main.cpp"), main).unwrap();
}

fn run_case(tag: &str, src: &str) {
    let isa =
        compile(src).unwrap_or_else(|e| panic!("`{tag}` failed to compile: {} error(s)", e.len()));
    let header = emit_cpp(&isa);
    let words = sample_words(&isa);
    let combos = if isa.modes.is_empty() {
        1
    } else {
        isa.mode_combos() as usize
    };

    let dir = target_dir().join(format!("cpp_{tag}_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    write_harness(&dir, &isa, &header, &words, combos);

    let out = Command::new("g++")
        .args(["-std=c++17", "-O1", "-w", "-o"])
        .arg(dir.join("a.out"))
        .arg(dir.join("main.cpp"))
        .output()
        .expect("failed to invoke g++");
    assert!(
        out.status.success(),
        "`{tag}`: g++ failed:\n{}\n(scratch: {})",
        String::from_utf8_lossy(&out.stderr),
        dir.display()
    );

    let run = Command::new(dir.join("a.out"))
        .output()
        .expect("failed to run compiled harness");
    assert!(
        run.status.success(),
        "`{tag}`: harness crashed (scratch: {})",
        dir.display()
    );

    let got = String::from_utf8(run.stdout).expect("non-utf8 harness output");
    let got_lines: Vec<&str> = got.lines().collect();
    let expected = words.len() * combos;
    assert_eq!(
        got_lines.len(),
        expected,
        "`{tag}`: harness emitted {} lines, expected {expected}",
        got_lines.len(),
    );

    let mut mismatches = 0usize;
    let mut first: Option<String> = None;
    let mut idx = 0usize;
    for combo in 0..combos {
        for &w in &words {
            let want = oracle_line(&isa, combo, w);
            let have = got_lines[idx];
            if want != have {
                mismatches += 1;
                if first.is_none() {
                    first = Some(format!(
                        "combo {combo} word {w:#x}: oracle `{want}` != cpp `{have}`"
                    ));
                }
            }
            idx += 1;
        }
    }
    assert_eq!(
        mismatches,
        0,
        "`{tag}`: {mismatches}/{expected} lines differ. First: {}\n(scratch: {})",
        first.unwrap_or_default(),
        dir.display()
    );

    // Clean up only on success.
    let _ = std::fs::remove_dir_all(&dir);
}

macro_rules! conformance_tests {
    ($($fn_name:ident => $ex:literal),* $(,)?) => {
        $(
            #[test]
            fn $fn_name() {
                run_case($ex, example!($ex));
            }
        )*
    };
}

conformance_tests! {
    cpp_mips => "mips",
    cpp_rv32i => "rv32i",
    cpp_gekko => "gekko",
    cpp_gb => "gb",
    cpp_gc_dsp => "gc_dsp",
    cpp_gba_arm => "gba_arm",
    cpp_aarch64 => "aarch64",
    cpp_riscv => "riscv",
    cpp_sparse_demo => "sparse_demo",
    cpp_tags_demo => "tags_demo",
    cpp_cond_demo => "cond_demo",
    cpp_names_demo => "names_demo",
    cpp_subdecoder_demo => "subdecoder_demo",
    cpp_riscv_rvc => "riscv_rvc",
    cpp_modes_demo => "modes_demo",
    // `snes_disasm` is left out because it uses `fetch(N)` stream operands, which the
    // C++17 backend does not support (no contextual-disasm / stream plumbing; see spec section 12).
}

// ---- inline-spec regressions (16bit windows, every word) ----

/// Guard-only spec: a `when` guard with no computed/fns/length still needs the 128bit preamble.
#[test]
fn cpp_guard_only_spec() {
    let src = r#"
decoder Guard {
    width = 16
    bit_order = lsb0
    endian = little
}

selector op [12:15]

operand reg = u4 { display("r{}") }

g0 op=0 lo:reg[3:0] hi:reg[7:4] when lo != hi | "g0"
g1 op=1 lo:reg[3:0] hi:reg[7:4]               | "g1"
"#;
    run_case("guard_only", src);
}

/// Negative computed value rendered as hex masked to the value width (not 64bit sign-extended).
#[test]
fn cpp_negative_hex_masked() {
    let src = r#"
decoder Neg {
    width = 16
    bit_order = lsb0
    endian = little
}

selector op [8:15]

m op=0 da:u8[7:0] off:i16 = sign_extend(da, 8) | "off {off:#x}"
"#;
    run_case("negative_hex", src);
}

/// Display-arm condition exercising a `word[hi:lo]` slice and an alternative arm fallthrough.
#[test]
fn cpp_slice_condition() {
    let src = r#"
decoder Slice {
    width = 16
    bit_order = lsb0
    endian = little
}

selector op [12:15]

m op=2 when word[15:8] == 0xab | "x" | "y"
"#;
    run_case("slice_cond", src);
}
