use std::path::PathBuf;

use chipi_core::bindings::parser::parse_file_with_includes;
use chipi_core::bindings::resolve::resolve;
use chipi_core::bindings::validate::validate;
use chipi_core::error::ErrorKind;

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

#[test]
fn basic_bindings_passes_validation() {
    let path = fixtures_dir().join("test_basic.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    let resolved = resolve(file).expect("resolve");
    validate(&resolved).expect("validation");
}

#[test]
fn unknown_instruction_in_group_yields_did_you_mean() {
    let path = fixtures_dir().join("test_unknown_instr.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    let resolved = resolve(file).expect("resolve");
    let errs = validate(&resolved).expect_err("should fail");
    let mut found = false;
    for e in &errs {
        if let ErrorKind::UnknownInstructionInGroup {
            instruction,
            suggestion,
        } = &e.kind
        {
            assert_eq!(instruction, "halttt");
            assert_eq!(suggestion.as_deref(), Some("halt"));
            found = true;
        }
    }
    assert!(found, "expected UnknownInstructionInGroup");
}

#[test]
fn missing_invalid_handler_in_dispatch_errors() {
    let path = fixtures_dir().join("test_missing_invalid.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    let resolved = resolve(file).expect("resolve");
    let errs = validate(&resolved).expect_err("should fail");
    assert!(
        errs.iter()
            .any(|e| matches!(e.kind, ErrorKind::MissingInvalidHandler(_))),
        "expected MissingInvalidHandler"
    );
}

#[test]
fn ida_processor_validates() {
    let path = fixtures_dir().join("test_ida.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    let resolved = resolve(file).expect("resolve");
    validate(&resolved).expect("validation");
}

#[test]
fn binja_architecture_validates() {
    let path = fixtures_dir().join("test_binja.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    let resolved = resolve(file).expect("resolve");
    validate(&resolved).expect("validation");
}

#[test]
fn binja_invalid_endianness_errors() {
    let src = r#"
include "test_dsp.chipi"
target binja {
    architecture TestDsp {
        output "out/x.py"
        name "x"
        address_size 2
        default_int_size 2
        endianness middle
        registers { ar0 }
    }
}
"#;
    let path = fixtures_dir().join("inline.bindings.chipi");
    std::fs::write(&path, src).unwrap();
    let file = parse_file_with_includes(&path).expect("parse");
    let resolved = resolve(file).expect("resolve");
    let errs = validate(&resolved).expect_err("should fail");
    let _ = std::fs::remove_file(&path);
    assert!(
        errs.iter()
            .any(|e| matches!(e.kind, ErrorKind::InvalidEndianness(_))),
        "expected InvalidEndianness"
    );
}

#[test]
fn ida_segment_register_not_declared_errors() {
    let src = r#"
include "test_dsp.chipi"
target ida {
    processor TestDsp {
        output "out/x.py"
        name "x"
        long_name "X"
        id 1
        address_size 16
        bytes_per_unit 2
        registers { ar0 ar1 }
        segment_registers { CS }
        flow { calls { call } stops { halt } }
    }
}
"#;
    let path = fixtures_dir().join("inline_ida.bindings.chipi");
    std::fs::write(&path, src).unwrap();
    let file = parse_file_with_includes(&path).expect("parse");
    let resolved = resolve(file).expect("resolve");
    let errs = validate(&resolved).expect_err("should fail");
    let _ = std::fs::remove_file(&path);
    assert!(
        errs.iter()
            .any(|e| matches!(e.kind, ErrorKind::SegmentRegisterNotDeclared(_))),
        "expected SegmentRegisterNotDeclared"
    );
}

#[test]
fn ida_unknown_flow_instruction_errors_with_suggestion() {
    let src = r#"
include "test_dsp.chipi"
target ida {
    processor TestDsp {
        output "out/x.py"
        name "x"
        long_name "X"
        id 1
        address_size 16
        bytes_per_unit 2
        registers { ar0 ar1 }
        segment_registers { ar0 }
        flow { calls { calcc } }
    }
}
"#;
    let path = fixtures_dir().join("inline_flow.bindings.chipi");
    std::fs::write(&path, src).unwrap();
    let file = parse_file_with_includes(&path).expect("parse");
    let resolved = resolve(file).expect("resolve");
    let errs = validate(&resolved).expect_err("should fail");
    let _ = std::fs::remove_file(&path);
    assert!(
        errs.iter()
            .any(|e| matches!(e.kind, ErrorKind::UnknownInstructionInFlow { .. })),
        "expected UnknownInstructionInFlow"
    );
}
