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

/// Regression: the Python backend has no stream context, so a `fetch(N)` operand must produce a
/// module that raises on import rather than a call to an undefined `fn_fetch`.
#[test]
fn python_refuses_fetch_operands() {
    let src = "decoder T { width = 8 bit_order = lsb0 endian = little }\n\
               selector op [0:7]\n\
               a op=0x01 x:u16 = fetch(16) | \"a ${x:04x}\"\n";
    let isa = compile(src).expect("spec compiles");
    let out = emit_python(&isa);
    assert!(
        out.contains("NotImplementedError"),
        "python should refuse fetch specs:\n{out}"
    );
    assert!(
        !out.contains("fn_fetch"),
        "python must not emit an undefined fn_fetch:\n{out}"
    );
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
