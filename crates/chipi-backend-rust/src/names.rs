//! Identifier and naming helpers for emitted Rust code.

use chipi_core::model::FieldTy;

/// Map non-identifier characters to `_`. Prefix `_` if the result is empty or starts with a digit.
pub fn sanitize(name: &str) -> String {
    let mut out = String::new();
    for (i, c) in name.chars().enumerate() {
        if c.is_ascii_alphanumeric() || c == '_' {
            if i == 0 && c.is_ascii_digit() {
                out.push('_');
            }
            out.push(c);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        out.push_str("isa");
    }
    out
}

/// The `mod opcode` constant name for an opcode (`Invalid` sanitises to `INVALID`).
pub fn const_name(name: &str) -> String {
    sanitize(name).to_uppercase()
}

const KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield", "try", "union", "gen",
];

/// A valid Rust method/field identifier for a spec name, raw-escaping keywords where possible.
pub fn ident(name: &str) -> String {
    let s = sanitize(name);
    match s.as_str() {
        // these cannot be raw identifiers
        "self" | "Self" | "super" | "crate" => format!("{s}_"),
        _ if KEYWORDS.contains(&s.as_str()) => format!("r#{s}"),
        _ => s,
    }
}

/// `add_imm` -> `AddImm`, `lda.dpx` -> `LdaDpx`, `alu` -> `Alu` (used for enum variants/types).
pub fn pascal(name: &str) -> String {
    let mut out = String::new();
    for seg in name.split(|c: char| !c.is_ascii_alphanumeric()) {
        let mut chars = seg.chars();
        if let Some(first) = chars.next() {
            out.extend(first.to_uppercase());
            out.push_str(chars.as_str());
        }
    }

    if out.is_empty() || out.starts_with(|c: char| c.is_ascii_digit()) {
        out.insert(0, '_');
    }

    out
}

/// The accessor method name for a computed operand, from its deduplicated logical name (see
/// [`chipi_core::accessor::computed_accessor_names`]).
pub fn computed_method(logical: &str) -> String {
    sanitize(logical)
}

/// The Rust return type for a value: the smallest standard integer width that
/// holds `value_width` bits, preserving signedness. A 16bit field gets `i16`/`u16`
/// rather than being widened to `i32`/`u32`. The accessor body sign-extends or masks
/// within `value_width` before the final `as {ret_type}` cast, so the narrowing is lossless.
pub fn ret_type(ty: &FieldTy) -> &'static str {
    let bits = match ty.value_width {
        0..=8 => 8,
        9..=16 => 16,
        17..=32 => 32,
        33..=64 => 64,
        _ => 128,
    };
    match (ty.signed, bits) {
        (true, 8) => "i8",
        (false, 8) => "u8",
        (true, 16) => "i16",
        (false, 16) => "u16",
        (true, 32) => "i32",
        (false, 32) => "u32",
        (true, 64) => "i64",
        (false, 64) => "u64",
        (true, _) => "i128",
        (false, _) => "u128",
    }
}

pub use chipi_core::compute::mask_u64;
