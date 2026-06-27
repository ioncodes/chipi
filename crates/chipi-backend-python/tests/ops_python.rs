//! Differential conformance for the `Ops` dispatch layer.
//!
//! For each tracked example we emit the Python module, build a harness that subclasses `dec.Ops`
//! (every handler records its own name), run `run_ops` over a fixed word sample and assert
//! that for every word the handler that fires matches the oracle's classified opcode and that
//! `run_ops` returns the oracle's instruction length. This proves dispatch routes correctly
//! (including `dispatch`-group folding through the default group handler) and that `run_ops` reports
//! the right length, mirroring the Rust and C++ backends.

use chipi_backend_python::{emit_python, ident};
use chipi_core::interp::{decode, decode_mode};
use chipi_core::{compile, Isa};
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

/// The python interpreter to use (overridable via `PYTHON`).
fn python_bin() -> String {
    std::env::var("PYTHON").unwrap_or_else(|_| "python3".to_string())
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

/// The oracle's `<opcode-name>|<len-bytes>` line for a word at mode combination `combo`.
fn oracle_line(isa: &Isa, combo: usize, word: u64) -> String {
    let d = if isa.modes.is_empty() {
        decode(isa, word)
    } else {
        decode_mode(isa, combo, word)
    };
    format!("{}|{}", d.opcode_name, d.len_bytes)
}

fn scratch_dir(test: &str) -> PathBuf {
    let mut dir = std::env::temp_dir();
    dir.push(format!("chipi_py_ops_{test}_{}", std::process::id()));

    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create scratch dir");

    dir
}

/// The recording `Ops` subclass: one override per instruction that stores its own name, plus an
/// `on_invalid` that stores the id-0 opcode name. Group members are overridden too so the default
/// group handler's routing is exercised; the group handler itself keeps the generated default.
fn recorder_class(isa: &Isa) -> String {
    let mut s = String::from("class Rec(dec.Ops):\n    fired = None\n");
    for inst in &isa.instrs {
        s.push_str(&format!("    def {}(self, inst):\n", ident(&inst.name)));
        s.push_str(&format!("        self.fired = {:?}\n", inst.name));
    }
    s.push_str("    def on_invalid(self, inst):\n        self.fired = dec.OPCODE_NAMES[0]\n");
    s
}

/// Build the harness that drives `run_ops` over the word file and prints `<fired>|<len>` per word,
/// combo-major to match the oracle comparison order.
fn harness(isa: &Isa, combos: usize) -> String {
    let modal = !isa.modes.is_empty();
    let run_call = if modal {
        "dec.run_ops(combo, r, w)"
    } else {
        "dec.run_ops(r, w)"
    };

    format!(
        r#"import sys
import dec
{recorder}
words = []
with open("words.txt") as f:
    for line in f:
        line = line.strip()
        if line:
            words.append(int(line, 16))
out = []
for combo in range({combos}):
    for w in words:
        r = Rec()
        length = {run_call}
        out.append(r.fired + "|" + str(length))
sys.stdout.write("\n".join(out))
sys.stdout.write("\n")
"#,
        recorder = recorder_class(isa),
    )
}

fn run_case(tag: &str, src: &str) {
    let isa =
        compile(src).unwrap_or_else(|e| panic!("`{tag}` failed to compile: {} error(s)", e.len()));
    let module = emit_python(&isa);
    let words = sample_words(isa.window_bits());
    let combos = if isa.modes.is_empty() {
        1
    } else {
        isa.mode_combos() as usize
    };

    let dir = scratch_dir(tag);
    std::fs::write(dir.join("dec.py"), &module).unwrap();
    std::fs::write(dir.join("main.py"), harness(&isa, combos)).unwrap();

    let mut wf = std::fs::File::create(dir.join("words.txt")).unwrap();
    let mut buf = String::with_capacity(words.len() * 9);
    for w in &words {
        buf.push_str(&format!("{w:x}\n"));
    }
    wf.write_all(buf.as_bytes()).unwrap();
    drop(wf);

    let out = Command::new(python_bin())
        .arg("main.py")
        .current_dir(&dir)
        .output()
        .unwrap_or_else(|e| panic!("`{tag}`: failed to spawn python: {e}"));

    assert!(
        out.status.success(),
        "`{tag}`: python exited with failure\n--- stdout ---\n{}\n--- stderr ---\n{}\n(scratch: {})",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
        dir.display()
    );

    let stdout = String::from_utf8(out.stdout).expect("python stdout is utf-8");
    let got: Vec<&str> = stdout.lines().collect();
    let expected = words.len() * combos;
    assert_eq!(
        got.len(),
        expected,
        "`{tag}`: harness emitted {} lines, expected {expected}",
        got.len()
    );

    let mut idx = 0usize;
    for combo in 0..combos {
        for w in &words {
            let want = oracle_line(&isa, combo, *w);
            assert_eq!(
                got[idx], want,
                "`{tag}` combo {combo}: dispatch mismatch at word {w:#x}: python {:?} != oracle {:?}\n(scratch: {})",
                got[idx], want, dir.display()
            );
            idx += 1;
        }
    }

    let _ = std::fs::remove_dir_all(&dir);
}

macro_rules! ops_tests {
    ($($fn_name:ident => $ex:literal),* $(,)?) => {
        $(
            #[test]
            fn $fn_name() {
                run_case($ex, include_str!(concat!("../../../examples/", $ex, ".chipi")));
            }
        )*
    };
}

ops_tests! {
    python_ops_mips => "mips",
    python_ops_rv32i => "rv32i",
    python_ops_gekko => "gekko",
    python_ops_gb => "gb",
    python_ops_gc_dsp => "gc_dsp",
    python_ops_gba_arm => "gba_arm",
    python_ops_aarch64 => "aarch64",
    python_ops_riscv => "riscv",
    python_ops_sparse_demo => "sparse_demo",
    python_ops_tags_demo => "tags_demo",
    python_ops_cond_demo => "cond_demo",
    python_ops_riscv_rvc => "riscv_rvc",
    python_ops_modes_demo => "modes_demo",
}
