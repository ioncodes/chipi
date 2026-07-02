//! Helpers shared by the generated-code test binaries: the rustc compile-and-run harness and
//! the backend's pascal-case naming mirror. Each binary uses a subset.

#![allow(dead_code)]

use std::path::PathBuf;
use std::process::Command;

pub fn out_dir() -> PathBuf {
    let d = std::env::temp_dir().join("chipi-rustgen-tests");
    std::fs::create_dir_all(&d).unwrap();
    d
}

/// Write `program` to a scratch file, compile it with `rustc` (disasm feature on) and return
/// the harness stdout lines.
pub fn compile_and_run(name: &str, program: &str) -> Vec<String> {
    let dir = out_dir();
    let src = dir.join(format!("{name}.rs"));
    let bin = dir.join(name);
    std::fs::write(&src, program).unwrap();

    let status = Command::new("rustc")
        .args([
            "--edition",
            "2021",
            "--cfg",
            "feature=\"disasm\"",
            "--cap-lints",
            "allow",
            "-o",
        ])
        .arg(&bin)
        .arg(&src)
        .status()
        .expect("rustc should be available");
    assert!(
        status.success(),
        "`{name}` generated harness failed to compile"
    );

    let output = Command::new(&bin).output().expect("run generated harness");
    assert!(output.status.success(), "`{name}` harness exited non-zero");

    String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect()
}

/// Mirror of the backend's naming: `add_imm` -> `AddImm` (dispatch-group kinds, enum variants).
/// The specs used here only have plain lowercase names, so this stays trivial on purpose.
pub fn pascal(name: &str) -> String {
    let mut out = String::new();
    for seg in name.split('_') {
        let mut chars = seg.chars();
        if let Some(first) = chars.next() {
            out.extend(first.to_uppercase());
            out.push_str(chars.as_str());
        }
    }
    out
}
