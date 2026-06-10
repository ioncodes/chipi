use std::collections::HashMap;

use chipi_core::Dispatch;
use chipi_core::generate_from_str;
use chipi_core::lut_gen::generate_lut_code;
use chipi_core::parser::parse;
use chipi_core::tree::build_tree;
use chipi_core::validate::validate;

const SMALL_DSP: &str = r#"
decoder Tiny {
    width = 8
    bit_order = msb0
}

a   [0:7]=00000000
    | "a"

b   [0:7]=00000001
    | "b"

c   [0:6]=0000001 last:u1[7:7]
    | "c {last}"

d   [0:5]=000010 rest:u2[6:7]
    | "d {rest}"
"#;

#[test]
fn flat_lut_for_small_decoder_produces_static_table() {
    // Make sure the spec parses + validates first.
    generate_from_str(SMALL_DSP, "tiny.chipi").expect("base codegen");

    let def = parse(SMALL_DSP, "tiny.chipi").expect("parse");
    let validated = validate(&def).expect("validate");
    let tree = build_tree(&validated);
    let groups: HashMap<String, String> = HashMap::new();

    let code = generate_lut_code(
        &validated,
        &tree,
        "crate::interp",
        "crate::Cpu",
        &groups,
        None,
        None,
        Dispatch::FlatLut,
        Some("crate::interp::invalid"),
        &[],
    )
    .expect("flat_lut codegen");

    assert!(code.contains("static DISPATCH"));
    assert!(code.contains("crate::interp::a"));
    assert!(code.contains("crate::interp::invalid"));
    assert!(code.contains("DISPATCH[key]"));
}

#[test]
fn flat_match_compresses_invalid_ranges() {
    let def = parse(SMALL_DSP, "tiny.chipi").expect("parse");
    let validated = validate(&def).expect("validate");
    let tree = build_tree(&validated);
    let groups: HashMap<String, String> = HashMap::new();

    let code = generate_lut_code(
        &validated,
        &tree,
        "crate::interp",
        "crate::Cpu",
        &groups,
        None,
        None,
        Dispatch::FlatMatch,
        Some("crate::interp::invalid"),
        &[],
    )
    .expect("flat_match codegen");

    assert!(code.contains("match"));
    // d covers raw 0x08..=0x0b (000010xx)
    assert!(
        code.contains("0x8..=0xb"),
        "expected compressed range 0x8..=0xb in:\n{}",
        code
    );
    // The default arm goes to invalid_handler
    assert!(code.contains("crate::interp::invalid"));
}

#[test]
fn flat_strategies_with_grouped_handlers() {
    let def = parse(SMALL_DSP, "tiny.chipi").expect("parse");
    let validated = validate(&def).expect("validate");
    let tree = build_tree(&validated);
    let mut groups: HashMap<String, String> = HashMap::new();
    groups.insert("a".to_string(), "ctl".to_string());
    groups.insert("b".to_string(), "ctl".to_string());

    let code = generate_lut_code(
        &validated,
        &tree,
        "crate::interp",
        "crate::Cpu",
        &groups,
        None,
        None,
        Dispatch::FlatLut,
        Some("crate::interp::invalid"),
        &[],
    )
    .expect("flat_lut codegen with groups");

    assert!(code.contains("crate::interp::ctl::<{ OP_A }>"));
    assert!(code.contains("crate::interp::ctl::<{ OP_B }>"));
}
