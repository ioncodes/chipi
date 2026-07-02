//! Diagnostics snapshots. Error texts are not semver surface, but they are UX: these tests pin
//! the code and the load-bearing part of the message for the diagnostics a spec author actually
//! hits, so a refactor cannot silently turn a good error into a vague one.

use chipi_core::compile;

/// Compile `src` expecting failure; assert some diagnostic carries `code` and its message
/// contains every `needles` fragment.
fn expect_diag(src: &str, code: &str, needles: &[&str]) {
    let errs = compile(src).expect_err("spec should be rejected");
    let Some(d) = errs.iter().find(|d| d.code == code) else {
        panic!(
            "no `{code}` diagnostic; got {:?}",
            errs.iter()
                .map(|d| (d.code, &d.message))
                .collect::<Vec<_>>()
        );
    };
    for n in needles {
        assert!(
            d.message.contains(n),
            "`{code}` message should mention `{n}`, got: {}",
            d.message
        );
    }
}

const HDR: &str =
    "decoder T { width = 16 bit_order = lsb0 endian = little }\nselector op [12:15]\n";

#[test]
fn field_overlap_names_the_field_and_bit() {
    let src = format!("{HDR}a op=1 x:u8[0:7] y:u8[4:11] | \"a\"\n");
    expect_diag(&src, "FieldOverlap", &["y", "bit"]);
}

#[test]
fn ambiguous_names_both_leaves() {
    let src = format!("{HDR}a op=1 | \"a\"\nb op=1 | \"b\"\n");
    expect_diag(&src, "Ambiguous", &["a", "b"]);
}

#[test]
fn bad_fetch_names_the_offender() {
    let src = format!("{HDR}a op=1 f:u4[0:3] x:u16 = fetch(f) | \"a\"\n");
    expect_diag(&src, "BadFetch", &["f", "mode"]);
}

#[test]
fn form_shape_names_both_leaves_and_the_form() {
    let src =
        format!("{HDR}lda.dp op=1 dp:u8[0:7] | \"lda\"\nsta.dp op=2 addr:u8[0:7] | \"sta\"\n");
    expect_diag(&src, "FormShape", &["sta.dp", "lda.dp", ".dp"]);
}

/// Same names and widths, different computations: an m-width and an x-width fetch under one
/// form would split into two accessors, so the shape check must reject the pair.
#[test]
fn form_shape_catches_mixed_fetch_widths() {
    let src = "decoder T { width = 8 bit_order = lsb0 endian = little mode m: bool = 1 mode x: bool = 1 }\n\
               selector op [0:7]\n\
               lda.imm op=1 imm:u16 = fetch(m ? 8 : 16) | \"lda #${imm:04x}\"\n\
               ldx.imm op=2 imm:u16 = fetch(x ? 8 : 16) | \"ldx #${imm:04x}\"\n";
    expect_diag(src, "FormShape", &["ldx.imm", "lda.imm", ".imm"]);
}

#[test]
fn shadowed_variable_names_the_operand() {
    let src = "decoder T { width = 16 bit_order = lsb0 endian = little mode m: bool = 0 }\n\
               selector op [12:15]\n\
               a op=1 m:u4[3:0] | \"a\"\n";
    expect_diag(src, "DuplicateName", &["m", "shadows"]);
}

#[test]
fn bad_arity_names_builtin_and_counts() {
    let src = format!("{HDR}a op=1 f:u4[0:3] v:u8 = ones(f, 2) | \"a\"\n");
    expect_diag(&src, "BadArity", &["ones"]);
}

#[test]
fn unknown_name_in_guard_mentions_decode_variables() {
    let src = format!("{HDR}a op=1 when q == 1 | \"a\"\n");
    expect_diag(&src, "UnknownName", &["q", "decode variable"]);
}

#[test]
fn display_unknown_field_names_it() {
    let src = format!("{HDR}a op=1 | \"a {{zz}}\"\n");
    expect_diag(&src, "UnknownName", &["zz"]);
}

#[test]
fn group_pattern_without_mnemonic_is_reported() {
    let src = format!("{HDR}lda.dp op=1 dp:u8[0:7] | \"lda\"\ndispatch g {{ ldx.* }}\n");
    expect_diag(&src, "UnknownInstruction", &["ldx"]);
}
