//! End-to-end CLI checks: drive the `chipi` binary over the example specs.

use std::process::Command;

fn chipi(args: &[&str]) -> (String, String, bool) {
    let out = Command::new(env!("CARGO_BIN_EXE_chipi"))
        .args(args)
        .output()
        .expect("run chipi");

    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

const MIPS: &str = "../../examples/mips.chipi";

#[test]
fn explain_decodes_and_disassembles() {
    let (out, _, ok) = chipi(&["explain", MIPS, "--", "0x00851020"]);

    assert!(ok, "explain should succeed");
    assert!(
        out.contains("-> add"),
        "decode path should reach add:\n{out}"
    );
    assert!(out.contains("add $r2, $r4, $r5"), "disasm missing:\n{out}");
}

#[test]
fn check_roundtrip_counts_match() {
    let (out, _, ok) = chipi(&["check", "--roundtrip", MIPS]);

    assert!(ok);
    assert!(out.contains("19219/19219"), "roundtrip count wrong:\n{out}");
}

#[test]
fn asm_round_trips() {
    let (out, _, ok) = chipi(&["asm", MIPS, "--", "add", "$r2,", "$r4,", "$r5"]);

    assert!(ok, "asm should succeed");
    assert!(out.contains("0x00851020"), "assembled word wrong:\n{out}");
    assert!(
        out.contains("bytes 20 10 85 00"),
        "full byte stream wrong:\n{out}"
    );
    assert!(out.contains("assembled as add"));
}

#[test]
fn stream_explain_runs_the_prefix_scan() {
    let (out, _, ok) = chipi(&[
        "explain",
        "--stream",
        "../../examples/x86_prefix.chipi",
        "--",
        "0x66,0x48,0x90",
    ]);

    assert!(ok);
    assert!(
        out.contains("-> nop"),
        "x86 0x66,0x48,0x90 should decode to nop:\n{out}"
    );
    assert!(
        out.contains("op_ovr=1"),
        "operand-size prefix should set context:\n{out}"
    );
}

#[test]
fn dump_ir_and_tree() {
    let (ir, _, ok1) = chipi(&["dump-ir", MIPS]);
    assert!(ok1);
    assert!(ir.contains("decoder Mips"));

    let (tree, _, ok2) = chipi(&["dump-tree", MIPS]);
    assert!(ok2);
    assert!(tree.contains("primary: op"));
    assert!(tree.contains("residual"));
}

#[test]
fn emit_all_backends() {
    let (rust, _, ok) = chipi(&["emit", "--target", "rust", MIPS]);
    assert!(ok);
    assert!(rust.contains("pub fn classify"));

    for target in ["cpp", "python"] {
        let (out, _, ok) = chipi(&["emit", "--target", target, MIPS]);
        assert!(ok, "emit {target} failed");
        assert!(
            out.contains("OPCODE_NAMES"),
            "emit {target} output looks empty:\n{}",
            &out[..out.len().min(200)]
        );
        assert!(out.contains("Mips"), "emit {target} missing decoder name");
    }
}

#[test]
fn negative_fixture_errors() {
    let (_, err, ok) = chipi(&["check", "../../examples/broken_overlap.chipi"]);

    assert!(!ok, "broken fixture must fail");
    assert!(
        err.contains("FieldOverlap"),
        "expected FieldOverlap:\n{err}"
    );
}
