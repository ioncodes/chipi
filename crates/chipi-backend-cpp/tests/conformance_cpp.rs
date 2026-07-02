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

/// `fetch(N)` operands emit the contextual-disassembly surface: a `DisasmCtx` interface,
/// stream accessors, `stream_len` and `disasm_ctx`.
#[test]
fn cpp_supports_fetch_operands() {
    let src = "decoder T { width = 8 bit_order = lsb0 endian = little }\n\
               selector op [0:7]\n\
               a op=0x01 x:u16 = fetch(16) | \"a ${x:04x}\"\n";
    let isa = compile(src).expect("spec compiles");
    let out = emit_cpp(&isa);
    assert!(!out.contains("#error"), "fetch specs must emit:\n{out}");
    assert!(
        !out.contains("fn_fetch"),
        "cpp must not emit an undefined fn_fetch:\n{out}"
    );
    for needle in ["struct DisasmCtx", "stream_len", "disasm_ctx"] {
        assert!(out.contains(needle), "`{needle}` missing:\n{out}");
    }
}

/// The one shape still refused: `length` arms reading decode variables (no runtime source for
/// them in the emitted word-level `inst_len`). Must be a loud `#error`, never silent divergence.
#[test]
fn cpp_refuses_var_reading_length_arms() {
    let src = "decoder T { width = 8 bit_order = lsb0 endian = little mode m: bool = 0 }\n\
               length =\n\
               \x20 | m == 1 : 16\n\
               \x20 | else : 8\n\
               selector op [0:7]\n\
               a op=0x01 | \"a\"\n";
    let isa = compile(src).expect("spec compiles");
    let out = emit_cpp(&isa);
    assert!(
        out.contains("#error") && out.contains("length"),
        "cpp should refuse var-reading length arms:\n{out}"
    );
}

/// Tags and identity-axis metadata mirror the Rust backend: OPCODE_TAGS for tagged specs,
/// Mnemonic/Form enums and tables for dotted leaf names. (Their decode behavior is covered by
/// the conformance runs; this pins the emitted surface.)
#[test]
fn cpp_emits_tags_and_axes() {
    let isa = compile(example!("tags_demo")).expect("tags_demo compiles");
    let out = emit_cpp(&isa);
    assert!(out.contains("OPCODE_TAGS"), "tags table missing:\n{out}");
    assert!(out.contains("\"arith\""), "tag name missing");

    let isa = compile(example!("axes_demo")).expect("axes_demo compiles");
    let out = emit_cpp(&isa);
    for needle in [
        "enum Mnemonic",
        "MN_LDA",
        "enum Form",
        "FORM_IMM",
        "OPCODE_MNEMONIC",
        "OPCODE_FORM",
    ] {
        assert!(out.contains(needle), "`{needle}` missing:\n{out}");
    }
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
    cpp_fn_let_width => "fn_let_width",
    cpp_guard_chain => "guard_chain",
    cpp_mode_guard => "mode_guard",
    cpp_axes_demo => "axes_demo",
    cpp_for_demo => "for_demo",
    cpp_x86_prefix => "x86_prefix",
    // `snes_disasm` and `fetch_expr` use `fetch(N)` stream operands, so they have no word-level
    // `disasm`; they are covered by the contextual-harness tests below instead.
}

// ---- contextual disassembly (fetch/:sym) and prefix streams ----

/// Compile `header` plus `main_src` with g++ and return the harness stdout lines.
fn compile_and_run(tag: &str, header: &str, main_src: &str) -> Vec<String> {
    let dir = target_dir().join(format!("cpp_{tag}_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("dec.hpp"), header).unwrap();
    std::fs::write(dir.join("main.cpp"), main_src).unwrap();

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

    let got: Vec<String> = String::from_utf8(run.stdout)
        .expect("non-utf8 harness output")
        .lines()
        .map(str::to_string)
        .collect();

    let _ = std::fs::remove_dir_all(&dir);
    got
}

/// The generated contextual disassembler (`fetch(N)` operands + `:sym`) must match the oracle's
/// `interp::disasm_ctx`, mirroring the Rust backend's `generated_disasm_ctx_matches_oracle`.
#[test]
fn cpp_disasm_ctx_matches_oracle() {
    let isa = compile(example!("snes_disasm")).expect("snes_disasm compiles");
    let pcs = [0u64, 1, 4];

    // A small program image: nop; lda #$1234; jmp $9000.
    struct Mem;
    impl chipi_core::interp::DisasmCtx for Mem {
        fn read_u8(&self, addr: u64) -> u8 {
            [0xEAu8, 0xA9, 0x34, 0x12, 0x4C, 0x00, 0x90]
                .get(addr as usize)
                .copied()
                .unwrap_or(0)
        }
    }

    let expected: Vec<String> = pcs
        .iter()
        .map(|&pc| {
            let (text, len) = chipi_core::interp::disasm_ctx(&isa, pc, &Mem);
            format!("{text}|{len}")
        })
        .collect();

    let main_src = r#"#include "dec.hpp"
#include <cstdio>
struct Mem : chipi::DisasmCtx {
    uint8_t read_u8(uint64_t addr) const override {
        static const uint8_t img[7] = {0xEA, 0xA9, 0x34, 0x12, 0x4C, 0x00, 0x90};
        return addr < 7 ? img[addr] : 0;
    }
};
int main() {
    Mem mem;
    for (uint64_t pc : {0ull, 1ull, 4ull}) {
        std::pair<std::string, uint8_t> r = chipi::disasm_ctx(pc, mem);
        printf("%s|%d\n", r.first.c_str(), (int)r.second);
    }
    return 0;
}
"#;

    let got = compile_and_run("snes_ctx", &emit_cpp(&isa), main_src);
    assert_eq!(got, expected, "cpp disasm_ctx mismatch vs oracle");
}

/// Expression fetch widths: `disasm_ctx` and `stream_len` must follow the host-supplied mode
/// exactly like the oracle, mirroring the Rust backend's `generated_fetch_expr_matches_oracle`.
#[test]
fn cpp_fetch_expr_matches_oracle() {
    let isa = compile(example!("fetch_expr")).expect("fetch_expr compiles");

    // lda #imm; ldx #imm; nop, disassembled under both accumulator widths.
    let image: [u8; 7] = [0xA9, 0x42, 0x99, 0xA2, 0xCD, 0xAB, 0xEA];
    let pcs = [0u64, 3, 6];

    struct Mem(u64);
    impl chipi_core::interp::DisasmCtx for Mem {
        fn read_u8(&self, addr: u64) -> u8 {
            [0xA9u8, 0x42, 0x99, 0xA2, 0xCD, 0xAB, 0xEA]
                .get(addr as usize)
                .copied()
                .unwrap_or(0)
        }
        fn mode(&self, _name: &str) -> u64 {
            self.0
        }
    }

    let mut expected = Vec::new();
    for m in [0u64, 1] {
        for &pc in &pcs {
            let (text, len) = chipi_core::interp::disasm_ctx(&isa, pc, &Mem(m));
            expected.push(format!("{text}|{len}"));
        }
        // stream_len for the modal classify at each pc.
        for &pc in &pcs {
            let combo = m as usize;
            let word = image[pc as usize];
            let d = chipi_core::interp::decode_mode(&isa, combo, word as u64);
            let inst = &isa.instrs[d.instr_index.unwrap()];
            let extra = chipi_core::interp::fetched_bytes_combo(&isa, inst, m);
            expected.push(format!("len {}", 1 + extra));
        }
    }

    let main_src = r#"#include "dec.hpp"
#include <cstdio>
struct Mem : chipi::DisasmCtx {
    uint64_t m;
    uint8_t read_u8(uint64_t addr) const override {
        static const uint8_t img[7] = {0xA9, 0x42, 0x99, 0xA2, 0xCD, 0xAB, 0xEA};
        return addr < 7 ? img[addr] : 0;
    }
    uint64_t mode(const char*) const override { return m; }
};
int main() {
    static const uint8_t img[7] = {0xA9, 0x42, 0x99, 0xA2, 0xCD, 0xAB, 0xEA};
    Mem mem;
    for (uint64_t m : {0ull, 1ull}) {
        mem.m = m;
        for (uint64_t pc : {0ull, 3ull, 6ull}) {
            std::pair<std::string, uint8_t> r = chipi::disasm_ctx(pc, mem);
            printf("%s|%d\n", r.first.c_str(), (int)r.second);
        }
        for (uint64_t pc : {0ull, 3ull, 6ull}) {
            unsigned combo = chipi::pack_modes(m);
            printf("len %d\n", (int)chipi::stream_len(combo, img[pc]));
        }
    }
    return 0;
}
"#;

    let got = compile_and_run("fetch_expr_ctx", &emit_cpp(&isa), main_src);
    assert_eq!(got, expected, "cpp fetch(expr) mismatch vs oracle");
}

/// Prefix streams: `decode_stream` + `classify_with` must agree with the oracle's
/// `interp::decode_stream` on opcode name and total length for every probe stream.
#[test]
fn cpp_decode_stream_matches_oracle() {
    let isa = compile(example!("x86_prefix")).expect("x86_prefix compiles");

    let streams: &[&[u8]] = &[
        &[0x90],
        &[0x49, 0x90],
        &[0x48, 0x90],
        &[0x66, 0x50],
        &[0x50],
        &[0x66, 0x49, 0x90],
    ];

    let expected: Vec<String> = streams
        .iter()
        .map(|bytes| {
            let d = chipi_core::interp::decode_stream(&isa, bytes);
            format!("{}|{}", d.opcode_name, d.len_bytes)
        })
        .collect();

    let header = emit_cpp(&isa);
    assert!(
        header.contains("classify_with"),
        "context-reading guards should emit classify_with:\n{header}"
    );

    let mut main_src = String::from(
        "#include \"dec.hpp\"\n#include <cstdio>\n#include <vector>\nint main() {\n    std::vector<std::vector<uint8_t>> streams = {\n",
    );
    for bytes in streams {
        let lit: Vec<String> = bytes.iter().map(|b| format!("{b:#04x}")).collect();
        main_src.push_str(&format!("        {{{}}},\n", lit.join(", ")));
    }
    main_src.push_str(
        "    };\n\
         \x20   for (const std::vector<uint8_t>& s : streams) {\n\
         \x20       chipi::StreamInsn r = chipi::decode_stream(s.data(), s.size());\n\
         \x20       printf(\"%s|%d\\n\", chipi::OPCODE_NAMES[chipi::classify_with(r.inst, r.ctx)], (int)r.len);\n\
         \x20   }\n\
         \x20   return 0;\n}\n",
    );

    let got = compile_and_run("x86_stream", &header, &main_src);
    assert_eq!(got, expected, "cpp decode_stream mismatch vs oracle");
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
