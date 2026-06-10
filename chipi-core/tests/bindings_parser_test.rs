use std::path::PathBuf;

use chipi_core::bindings::parser::{parse, parse_file_with_includes};
use chipi_core::bindings::types::TargetKind;

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

#[test]
fn parse_basic_rust_decoder() {
    let path = fixtures_dir().join("test_basic.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    assert_eq!(file.targets.len(), 1);
    assert_eq!(file.targets[0].kind, TargetKind::Rust);
    assert_eq!(file.targets[0].rust_decoders.len(), 1);
    assert_eq!(file.targets[0].rust_dispatches.len(), 1);

    let dec = &file.targets[0].rust_decoders[0];
    assert_eq!(dec.decoder_name, "TestDsp");
    assert_eq!(dec.output, "out/test_dsp.rs");

    let disp = &file.targets[0].rust_dispatches[0];
    assert_eq!(disp.decoder_name, "TestDsp");
    assert_eq!(disp.context.as_deref(), Some("crate::cpu::Cpu"));
    assert_eq!(disp.handlers.as_deref(), Some("crate::cpu::interpreter"));
    assert_eq!(
        disp.invalid_handler.as_deref(),
        Some("crate::cpu::interpreter::invalid")
    );
    assert!(disp.handler_groups.is_empty());
}

#[test]
fn parse_grouped_dispatch() {
    let path = fixtures_dir().join("test_grouped.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    let disp = &file.targets[0].rust_dispatches[0];
    assert_eq!(disp.handler_groups.len(), 1);
    let g = &disp.handler_groups[0];
    assert_eq!(g.handler_name, "control");
    let names: Vec<&str> = g.instructions.iter().map(|(n, _)| n.as_str()).collect();
    assert_eq!(names, vec!["nop", "halt"]);
}

#[test]
fn parse_ida_target() {
    let path = fixtures_dir().join("test_ida.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    let target = &file.targets[0];
    assert_eq!(target.kind, TargetKind::Ida);
    let proc = &target.ida_processors[0];
    assert_eq!(proc.decoder_name, "TestDsp");
    assert_eq!(proc.id, Some(0x8001));
    assert_eq!(proc.address_size, Some(16));
    assert_eq!(proc.bytes_per_unit, Some(2));
    assert!(proc.registers.contains(&"ar0".to_string()));
    assert_eq!(proc.flow.calls.len(), 1);
    assert_eq!(proc.flow.stops.len(), 1);
}

#[test]
fn parse_binja_target() {
    let path = fixtures_dir().join("test_binja.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    let target = &file.targets[0];
    assert_eq!(target.kind, TargetKind::Binja);
    let arch = &target.binja_architectures[0];
    assert_eq!(arch.decoder_name, "TestDsp");
    assert_eq!(arch.address_size, Some(2));
    assert_eq!(
        arch.endianness.as_ref().map(|(s, _)| s.as_str()),
        Some("big")
    );
}

#[test]
fn parse_combined_file() {
    let path = fixtures_dir().join("test_combined.bindings.chipi");
    let file = parse_file_with_includes(&path).expect("parse");
    assert_eq!(file.targets.len(), 3);
    let kinds: Vec<TargetKind> = file.targets.iter().map(|t| t.kind).collect();
    assert!(kinds.contains(&TargetKind::Rust));
    assert!(kinds.contains(&TargetKind::Ida));
    assert!(kinds.contains(&TargetKind::Binja));
}

#[test]
fn parse_unknown_target_kind() {
    let src = r#"target frob { }"#;
    let p = std::path::Path::new("inline.bindings.chipi");
    let result = parse(src, p);
    let errs = result.expect_err("should fail");
    assert!(errs.iter().any(|e| matches!(
        e.kind,
        chipi_core::error::ErrorKind::UnknownTargetKind(ref s) if s == "frob"
    )));
}

#[test]
fn parse_invalid_strategy() {
    let src = r#"
include "test_dsp.chipi"
target rust {
    dispatch TestDsp {
        output "x.rs"
        context crate::Cpu
        handlers crate::interp
        strategy frob
        invalid_handler crate::interp::invalid
    }
}
"#;
    let p = std::path::Path::new("inline.bindings.chipi");
    let errs = parse(src, p).expect_err("should fail");
    assert!(
        errs.iter()
            .any(|e| matches!(e.kind, chipi_core::error::ErrorKind::InvalidStrategy(_)))
    );
}
