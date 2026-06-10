use std::path::PathBuf;

use chipi_core::bindings::{RunMode, run};

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

#[test]
fn check_basic_bindings_passes() {
    let path = fixtures_dir().join("test_basic.bindings.chipi");
    run(&path, None, None, RunMode::Check).expect("check");
}

#[test]
fn check_combined_bindings_passes() {
    let path = fixtures_dir().join("test_combined.bindings.chipi");
    run(&path, None, None, RunMode::Check).expect("check");
}

#[test]
fn generate_combined_without_target_errors() {
    let path = fixtures_dir().join("test_combined.bindings.chipi");
    let err = run(&path, None, None, RunMode::Generate).expect_err("should fail");
    assert!(
        err.iter().any(|e| matches!(
            e.kind,
            chipi_core::error::ErrorKind::MultipleTargetsAmbiguous(_)
        )),
        "expected MultipleTargetsAmbiguous"
    );
}

#[test]
fn generate_writes_decoder_and_dispatch_files() {
    use std::fs;

    // Use a unique scratch directory so this test can run in parallel.
    let tmp = std::env::temp_dir().join("chipi_e2e_basic");
    let _ = fs::remove_dir_all(&tmp);
    fs::create_dir_all(tmp.join("out")).unwrap();

    let src = format!(
        r#"
include "{spec}"

target rust {{
    decoder TestDsp {{
        output "{out_decoder}"
    }}

    dispatch TestDsp {{
        output "{out_dispatch}"

        context crate::cpu::Cpu
        handlers crate::cpu::interpreter
        strategy fn_ptr_lut
        invalid_handler crate::cpu::interpreter::invalid
    }}
}}
"#,
        spec = fixtures_dir()
            .join("test_dsp.chipi")
            .to_string_lossy()
            .replace('\\', "/"),
        out_decoder = tmp
            .join("out/decoder.rs")
            .to_string_lossy()
            .replace('\\', "/"),
        out_dispatch = tmp
            .join("out/dispatch.rs")
            .to_string_lossy()
            .replace('\\', "/"),
    );
    let bindings = tmp.join("test.bindings.chipi");
    fs::write(&bindings, src).unwrap();

    run(&bindings, None, None, RunMode::Generate).expect("generate");

    let dec = fs::read_to_string(tmp.join("out/decoder.rs")).expect("decoder file");
    assert!(dec.contains("Nop"));
    assert!(dec.contains("Halt"));

    let disp = fs::read_to_string(tmp.join("out/dispatch.rs")).expect("dispatch file");
    assert!(disp.contains("crate::cpu::interpreter::invalid"));
    assert!(disp.contains("OP_HALT"));

    let _ = fs::remove_dir_all(&tmp);
}
