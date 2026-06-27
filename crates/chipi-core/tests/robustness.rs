//! Robustness: malformed specs are rejected with the expected diagnostic codes, never a panic.

use chipi_core::compile;

fn codes(src: &str) -> Vec<&'static str> {
    match compile(src) {
        Ok(_) => Vec::new(),
        Err(e) => e.iter().map(|d| d.code).collect(),
    }
}

fn expect_code(src: &str, code: &'static str) {
    let cs = codes(src);
    assert!(
        cs.contains(&code),
        "expected `{code}`, got {cs:?} for spec:\n{src}"
    );
}

const HEADER: &str =
    "decoder D { width = 32 bit_order = lsb0 endian = little }\nselector op [31:26]\n";

#[test]
fn missing_decoder() {
    expect_code("selector op [0:7]", "MissingDecoder");
}

#[test]
fn duplicate_decoder() {
    expect_code(
        "decoder A { width = 8 }\ndecoder B { width = 8 }\nselector op [0:7]\nx op=0 | \"x\"",
        "DuplicateDecoder",
    );
}

#[test]
fn bad_width() {
    expect_code(
        "decoder D { width = 128 }\nselector op [0:7]\nx op=0 | \"x\"",
        "BadWidth",
    );
}

#[test]
fn missing_width() {
    expect_code(
        "decoder D { bit_order = lsb0 }\nselector op [0:3]\nx op=0 | \"x\"",
        "MissingWidth",
    );
}

#[test]
fn parse_error_is_not_a_panic() {
    expect_code("decoder D { width = }", "ParseError");
}

#[test]
fn unknown_base_type() {
    expect_code(
        &format!("{HEADER}operand r = q5 {{ display(\"r{{}}\") }}\nx op=0 | \"x\""),
        "UnknownName",
    );
}

#[test]
fn range_out_of_bounds() {
    expect_code(
        &format!("{HEADER}x op=0 r:u5[40:36] | \"x {{r}}\""),
        "RangeOutOfBounds",
    );
}

#[test]
fn width_mismatch() {
    // a 4bit field bound to a u8 type with no resize transform
    expect_code(
        &format!("{HEADER}type t = u8\nx op=0 r:t[11:8] | \"x {{r}}\""),
        "WidthMismatch",
    );
}

#[test]
fn field_overlap() {
    expect_code(
        &format!("{HEADER}x op=0 a:u5[20:16] b:u5[18:14] | \"x {{a}} {{b}}\""),
        "FieldOverlap",
    );
}

#[test]
fn bit_conflict_value_too_wide() {
    expect_code(&format!("{HEADER}x op=99 | \"x\""), "BitConflict");
}

#[test]
fn unknown_selector_constraint() {
    expect_code(&format!("{HEADER}x nope=0 | \"x\""), "UnknownSelector");
}

#[test]
fn duplicate_instruction_name() {
    expect_code(
        &format!("{HEADER}x op=0 | \"x\"\nx op=1 | \"x\""),
        "DuplicateName",
    );
}

#[test]
fn unknown_form() {
    expect_code(&format!("{HEADER}x op=0 uses Nope | \"x\""), "UnknownName");
}

#[test]
fn bad_length_without_else() {
    expect_code(
        &format!("{HEADER}length = | word[0:0] != 0 : 16\nx op=0 | \"x\""),
        "BadLength",
    );
}

#[test]
fn prefix_not_total() {
    expect_code(
        "decoder D { width = 8 bit_order = lsb0 endian = little context { f:u1 = 0 } }\n\
         selector op [0:7]\nprefix s { 0x66 => f = 1 }\nx op=0x90 | \"x\"",
        "PrefixNotTotal",
    );
}

#[test]
fn bad_display_template() {
    expect_code(
        &format!("{HEADER}x op=0 r:u5[20:16] | \"x {{r\""),
        "BadDisplayTemplate",
    );
}

#[test]
fn never_panics_on_arbitrary_text() {
    let garbage = [
        "",
        "   ",
        "decoder",
        "decoder {",
        "}}}}}}",
        "decoder D { width = 8 } selector",
        "x y z = = = | | |",
        "\u{0}\u{1}\u{2}",
        "fn f() -> { }",
        "assemble assemble assemble",
        "decoder D { width = 16 } selector op [0:3] a op=0",
    ];
    for g in garbage {
        let _ = compile(g); // must return Result, never panic
    }
}
