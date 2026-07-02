//! Cross-check the emitted Python decoder against the in-repo `chipi_core::interp` oracle.
//!
//! For each tracked example this compiles the spec, emits a Python module and runs it under
//! `python3` over the same word sample as `chipi-core`'s conformance test. That sample is all
//! words for windows up to 16bit, otherwise a 200k-word LCG sample. It asserts the generated decoder
//! produces the same `<opcode-name>|<disasm>` for every sampled word as the oracle. Modal specs are
//! swept over every mode combination.

use chipi_backend_python::emit_python;
use chipi_core::interp::{decode, decode_mode};
use chipi_core::{compile, Isa};
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

/// The python interpreter to use (overridable via `PYTHON`).
fn python_bin() -> String {
    std::env::var("PYTHON").unwrap_or_else(|_| "python3".to_string())
}

/// `fetch(N)` operands emit the contextual-disassembly surface: a duck-typed `ctx` object,
/// stream accessors, `stream_len` and `disasm_ctx`.
#[test]
fn python_supports_fetch_operands() {
    let src = "decoder T { width = 8 bit_order = lsb0 endian = little }\n\
               selector op [0:7]\n\
               a op=0x01 x:u16 = fetch(16) | \"a ${x:04x}\"\n";
    let isa = compile(src).expect("spec compiles");
    let out = emit_python(&isa);
    assert!(
        !out.contains("NotImplementedError"),
        "fetch specs must emit:\n{out}"
    );
    assert!(
        !out.contains("fn_fetch"),
        "python must not emit an undefined fn_fetch:\n{out}"
    );
    for needle in ["def stream_len", "def disasm_ctx"] {
        assert!(out.contains(needle), "`{needle}` missing:\n{out}");
    }
}

/// The one shape still refused: `length` arms reading decode variables (no runtime source for
/// them in the emitted word-level `inst_len`). Must raise on import, never diverge silently.
#[test]
fn python_refuses_var_reading_length_arms() {
    let src = "decoder T { width = 8 bit_order = lsb0 endian = little mode m: bool = 0 }\n\
               length =\n\
               \x20 | m == 1 : 16\n\
               \x20 | else : 8\n\
               selector op [0:7]\n\
               a op=0x01 | \"a\"\n";
    let isa = compile(src).expect("spec compiles");
    let out = emit_python(&isa);
    assert!(
        out.contains("NotImplementedError") && out.contains("length"),
        "python should refuse var-reading length arms:\n{out}"
    );
}

/// Tags and identity-axis metadata mirror the Rust backend (see the C++ twin test).
#[test]
fn python_emits_tags_and_axes() {
    let isa =
        compile(include_str!("../../../examples/tags_demo.chipi")).expect("tags_demo compiles");
    let out = emit_python(&isa);
    assert!(out.contains("OPCODE_TAGS"), "tags table missing:\n{out}");
    assert!(out.contains("\"arith\""), "tag name missing");

    let isa =
        compile(include_str!("../../../examples/axes_demo.chipi")).expect("axes_demo compiles");
    let out = emit_python(&isa);
    for needle in [
        "MNEMONIC_NAMES",
        "FORM_NAMES",
        "OPCODE_MNEMONIC",
        "OPCODE_FORM",
        "def mnemonic",
        "def form",
    ] {
        assert!(out.contains(needle), "`{needle}` missing:\n{out}");
    }
}

/// Word sample for a window: all words up to 16bit, else a 200k LCG sweep.
fn sample_words(bits: u16) -> Vec<u64> {
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

/// The oracle line for a non-modal decode: `<opcode-name>|<disasm>`. An invalid encoding has no
/// oracle disasm; the standalone Python module renders it as `"(invalid)"`, so we normalise to that.
fn oracle_line(isa: &Isa, word: u64) -> String {
    let d = decode(isa, word);
    let disasm = d.disasm.unwrap_or_else(|| "(invalid)".to_string());

    format!("{}|{}", d.opcode_name, disasm)
}

/// The oracle line for a modal decode.
fn oracle_line_mode(isa: &Isa, combo: usize, word: u64) -> String {
    let d = decode_mode(isa, combo, word);
    let disasm = d.disasm.unwrap_or_else(|| "(invalid)".to_string());

    format!("{}|{}", d.opcode_name, disasm)
}

/// A unique scratch directory for one test run.
fn scratch_dir(test: &str) -> PathBuf {
    let mut dir = std::env::temp_dir();
    dir.push(format!("chipi_py_{test}_{}", std::process::id()));

    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create scratch dir");

    dir
}

/// Run the emitted module + harness over `words`, returning one output line per word.
fn run_harness(test: &str, module: &str, harness: &str, words: &[u64]) -> Vec<String> {
    let dir = scratch_dir(test);
    std::fs::write(dir.join("dec.py"), module).unwrap();
    std::fs::write(dir.join("main.py"), harness).unwrap();

    let mut wf = std::fs::File::create(dir.join("words.txt")).unwrap();
    let mut buf = String::with_capacity(words.len() * 9);
    for w in words {
        buf.push_str(&format!("{w:x}\n"));
    }
    wf.write_all(buf.as_bytes()).unwrap();
    drop(wf);

    let out = Command::new(python_bin())
        .arg("main.py")
        .current_dir(&dir)
        .output()
        .unwrap_or_else(|e| panic!("`{test}`: failed to spawn python: {e}"));

    assert!(
        out.status.success(),
        "`{test}`: python exited with failure\n--- stdout ---\n{}\n--- stderr ---\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );

    let stdout = String::from_utf8(out.stdout).expect("python stdout is utf-8");
    let lines: Vec<String> = stdout.lines().map(|l| l.to_string()).collect();

    let _ = std::fs::remove_dir_all(&dir);

    lines
}

/// Non-modal harness: print `opcode_name(word)|disasm(word)` per input word.
const HARNESS: &str = r#"import dec
with open("words.txt") as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        w = int(line, 16)
        print(dec.opcode_name(w) + "|" + dec.disasm(w))
"#;

/// Modal harness: print, for each combo in order, `opcode_name_in|disasm` per word; combos are
/// emitted contiguously so the Rust side can interleave the same way.
fn modal_harness(combos: usize) -> String {
    format!(
        r#"import dec
words = []
with open("words.txt") as f:
    for line in f:
        line = line.strip()
        if line:
            words.append(int(line, 16))
for combo in range({combos}):
    for w in words:
        print(dec.opcode_name_in(combo, w) + "|" + dec.disasm(combo, w))
"#
    )
}

fn check_example(name: &str, src: &str) {
    let isa =
        compile(src).unwrap_or_else(|e| panic!("`{name}` failed to compile: {} error(s)", e.len()));
    let module = emit_python(&isa);
    let words = sample_words(isa.window_bits());

    if isa.modes.is_empty() {
        let got = run_harness(name, &module, HARNESS, &words);
        assert_eq!(
            got.len(),
            words.len(),
            "`{name}`: python emitted {} lines for {} words",
            got.len(),
            words.len()
        );

        for (i, w) in words.iter().enumerate() {
            let want = oracle_line(&isa, *w);
            assert_eq!(
                got[i], want,
                "`{name}`: mismatch at word {w:#x}: python {:?} != oracle {:?}",
                got[i], want
            );
        }
    } else {
        let combos = isa.mode_combos() as usize;

        let got = run_harness(name, &module, &modal_harness(combos), &words);
        assert_eq!(
            got.len(),
            combos * words.len(),
            "`{name}`: python emitted {} lines for {} combos x {} words",
            got.len(),
            combos,
            words.len()
        );

        let mut idx = 0usize;
        for combo in 0..combos {
            for w in &words {
                let want = oracle_line_mode(&isa, combo, *w);
                assert_eq!(
                    got[idx], want,
                    "`{name}` combo {combo}: mismatch at word {w:#x}: python {:?} != oracle {:?}",
                    got[idx], want
                );
                idx += 1;
            }
        }
    }
}

macro_rules! example {
    ($name:literal) => {
        include_str!(concat!("../../../examples/", $name, ".chipi"))
    };
}

macro_rules! case {
    ($fn:ident, $name:literal) => {
        #[test]
        fn $fn() {
            check_example($name, example!($name));
        }
    };
}

case!(python_decodes_mips, "mips");
case!(python_decodes_rv32i, "rv32i");
case!(python_decodes_names_demo, "names_demo");
case!(python_decodes_subdecoder_demo, "subdecoder_demo");
case!(python_decodes_gekko, "gekko");
case!(python_decodes_gb, "gb");
case!(python_decodes_gc_dsp, "gc_dsp");
case!(python_decodes_gba_arm, "gba_arm");
case!(python_decodes_aarch64, "aarch64");
case!(python_decodes_riscv, "riscv");
case!(python_decodes_sparse_demo, "sparse_demo");
case!(python_decodes_tags_demo, "tags_demo");
case!(python_decodes_cond_demo, "cond_demo");
case!(python_decodes_riscv_rvc, "riscv_rvc");
case!(python_decodes_modes_demo, "modes_demo");
case!(python_decodes_fn_let_width, "fn_let_width");
case!(python_decodes_guard_chain, "guard_chain");
case!(python_decodes_mode_guard, "mode_guard");
case!(python_decodes_axes_demo, "axes_demo");
case!(python_decodes_for_demo, "for_demo");
case!(python_decodes_x86_prefix, "x86_prefix");
// `snes_disasm` and `fetch_expr` use `fetch(N)` stream operands, so they have no word-level
// `disasm`; they are covered by the contextual-harness tests below instead.

// ---- contextual disassembly (fetch/:sym) and prefix streams ----

/// The generated contextual disassembler (`fetch(N)` operands + `:sym`) must match the oracle's
/// `interp::disasm_ctx`, mirroring the Rust backend's `generated_disasm_ctx_matches_oracle`.
#[test]
fn python_disasm_ctx_matches_oracle() {
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

    const HARNESS: &str = r#"import dec

class Mem:
    img = [0xEA, 0xA9, 0x34, 0x12, 0x4C, 0x00, 0x90]

    def read_u8(self, addr):
        return self.img[addr] if addr < len(self.img) else 0

    def symbol(self, addr):
        return None

for pc in [0, 1, 4]:
    (text, ln) = dec.disasm_ctx(pc, Mem())
    print(text + "|" + str(ln))
"#;

    let got = run_harness("snes_ctx", &emit_python(&isa), HARNESS, &[]);
    assert_eq!(got, expected, "python disasm_ctx mismatch vs oracle");
}

/// Expression fetch widths: `disasm_ctx` and `stream_len` must follow the host-supplied mode
/// exactly like the oracle, mirroring the Rust backend's `generated_fetch_expr_matches_oracle`.
#[test]
fn python_fetch_expr_matches_oracle() {
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
            let d = decode_mode(&isa, combo, word as u64);
            let inst = &isa.instrs[d.instr_index.unwrap()];
            let extra = chipi_core::interp::fetched_bytes_combo(&isa, inst, m);
            expected.push(format!("len {}", 1 + extra));
        }
    }

    const HARNESS: &str = r#"import dec

class Mem:
    img = [0xA9, 0x42, 0x99, 0xA2, 0xCD, 0xAB, 0xEA]

    def __init__(self, m):
        self.m = m

    def read_u8(self, addr):
        return self.img[addr] if addr < len(self.img) else 0

    def mode(self, name):
        return self.m

for m in [0, 1]:
    for pc in [0, 3, 6]:
        (text, ln) = dec.disasm_ctx(pc, Mem(m))
        print(text + "|" + str(ln))
    for pc in [0, 3, 6]:
        combo = dec.pack_modes(m)
        print("len " + str(dec.stream_len(combo, Mem.img[pc])))
"#;

    let got = run_harness("fetch_expr_ctx", &emit_python(&isa), HARNESS, &[]);
    assert_eq!(got, expected, "python fetch(expr) mismatch vs oracle");
}

/// Prefix streams: `decode_stream` + `classify_with` must agree with the oracle's
/// `interp::decode_stream` on opcode name and total length for every probe stream.
#[test]
fn python_decode_stream_matches_oracle() {
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

    let module = emit_python(&isa);
    assert!(
        module.contains("def classify_with"),
        "context-reading guards should emit classify_with:\n{module}"
    );

    let mut harness = String::from("import dec\n\nstreams = [\n");
    for bytes in streams {
        let lit: Vec<String> = bytes.iter().map(|b| format!("{b:#04x}")).collect();
        harness.push_str(&format!("    [{}],\n", lit.join(", ")));
    }
    harness.push_str(
        "]\n\nfor s in streams:\n    (word, ln, ctx) = dec.decode_stream(bytes(s))\n    print(dec.OPCODE_NAMES[dec.classify_with(word, ctx)] + \"|\" + str(ln))\n",
    );

    let got = run_harness("x86_stream", &module, &harness, &[]);
    assert_eq!(got, expected, "python decode_stream mismatch vs oracle");
}
