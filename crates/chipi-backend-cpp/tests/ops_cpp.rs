//! Differential conformance for the `Ops` dispatch layer.
//!
//! For each tracked example we emit the C++ decoder, build a harness that subclasses `chipi::Ops`
//! (every handler records its own name), run `run_ops` over a fixed word sample and assert
//! that for every word the handler that fires matches the oracle's classified opcode and that
//! `run_ops` returns the oracle's instruction length. This proves dispatch routes correctly
//! (including `dispatch`-group folding through the default group handler) and that `run_ops` reports
//! the right length, mirroring the Rust backend's `Ops`/`dispatch_ops`/`run_ops`.

use chipi_backend_cpp::{emit_cpp, ident};
use chipi_core::interp::{decode, decode_mode};
use chipi_core::{compile, Isa};
use std::path::{Path, PathBuf};
use std::process::Command;

macro_rules! example {
    ($name:literal) => {
        include_str!(concat!("../../../examples/", $name, ".chipi"))
    };
}

/// Fixed word sample, matching `conformance_cpp.rs`: all words for windows <= 16 bits,
/// else a 200k-word LCG (seed 0).
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

/// The oracle's `<opcode-name>|<len-bytes>` line for a word at mode combination `combo`.
fn oracle_line(isa: &Isa, combo: usize, word: u64) -> String {
    let d = if isa.modes.is_empty() {
        decode(isa, word)
    } else {
        decode_mode(isa, combo, word)
    };
    format!("{}|{}", d.opcode_name, d.len_bytes)
}

fn handle_ty(isa: &Isa) -> &'static str {
    match isa.handle_bits() {
        8 => "uint8_t",
        16 => "uint16_t",
        32 => "uint32_t",
        _ => "uint64_t",
    }
}

fn target_dir() -> PathBuf {
    std::env::var("CARGO_TARGET_TMPDIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| std::env::temp_dir())
}

/// The recording `Ops` subclass: one override per instruction that stores its own name, plus an
/// `on_invalid` that stores the id-0 opcode name (what the oracle reports for unmapped words). Group
/// members are overridden too so the default group handler's routing is exercised; the group handler
/// itself is left as the generated default.
fn recorder_class(isa: &Isa) -> String {
    let mut s = String::from("struct Rec : chipi::Ops {\n    std::string fired;\n");
    for inst in &isa.instrs {
        s.push_str(&format!(
            "    void {}(chipi::Instruction) override {{ fired = {:?}; }}\n",
            ident(&inst.name),
            inst.name
        ));
    }
    s.push_str(
        "    void on_invalid(chipi::Instruction) override { fired = chipi::OPCODE_NAMES[0]; }\n};\n",
    );
    s
}

fn write_harness(dir: &Path, isa: &Isa, header: &str, words: &[u64], combos: usize) {
    std::fs::create_dir_all(dir).unwrap();
    std::fs::write(dir.join("dec.hpp"), header).unwrap();

    let modal = !isa.modes.is_empty();

    let mut words_src = String::new();
    for w in words {
        words_src.push_str(&format!("{w}ull,"));
    }

    let run_call = if modal {
        "chipi::run_ops((unsigned)c, r, (HANDLE)w)"
    } else {
        "chipi::run_ops(r, (HANDLE)w)"
    };

    let handle = handle_ty(isa);
    let recorder = recorder_class(isa);

    let main = format!(
        r#"#include "dec.hpp"
#include <cstdio>
#include <cstdint>
#include <string>
{recorder}
using HANDLE = {handle};
static const unsigned long long WORDS[] = {{ {words_src} }};
int main() {{
    std::string out;
    out.reserve(1u << 20);
    for (unsigned c = 0; c < {combos}u; c++) {{
        for (size_t i = 0; i < sizeof(WORDS)/sizeof(WORDS[0]); i++) {{
            unsigned long long w = WORDS[i];
            (void)c;
            Rec r;
            unsigned len = (unsigned){run_call};
            out += r.fired;
            out += '|';
            out += std::to_string(len);
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

    let dir = target_dir().join(format!("cpp_ops_{tag}_{}", std::process::id()));
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
        "`{tag}`: {mismatches}/{expected} dispatch lines differ. First: {}\n(scratch: {})",
        first.unwrap_or_default(),
        dir.display()
    );

    let _ = std::fs::remove_dir_all(&dir);
}

macro_rules! ops_tests {
    ($($fn_name:ident => $ex:literal),* $(,)?) => {
        $(
            #[test]
            fn $fn_name() {
                run_case($ex, example!($ex));
            }
        )*
    };
}

ops_tests! {
    cpp_ops_mips => "mips",
    cpp_ops_rv32i => "rv32i",
    cpp_ops_gekko => "gekko",
    cpp_ops_gb => "gb",
    cpp_ops_gc_dsp => "gc_dsp",
    cpp_ops_gba_arm => "gba_arm",
    cpp_ops_aarch64 => "aarch64",
    cpp_ops_riscv => "riscv",
    cpp_ops_sparse_demo => "sparse_demo",
    cpp_ops_tags_demo => "tags_demo",
    cpp_ops_cond_demo => "cond_demo",
    cpp_ops_riscv_rvc => "riscv_rvc",
    cpp_ops_modes_demo => "modes_demo",
}
