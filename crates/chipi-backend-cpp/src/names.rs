//! Identifier and literal helpers for emitted C++17 code.

use chipi_core::model::FieldTy;

/// Map non-identifier characters to `_`; prefix `_` if the result is empty or starts with a digit.
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
        out.push('_');
    }
    out
}

/// The `OP_*` enum constant name for an opcode (`Invalid` sanitises to `INVALID`).
pub fn cname(name: &str) -> String {
    sanitize(name).to_uppercase()
}

/// The full C++ keyword list (C++20).
const KEYWORDS: &[&str] = &[
    "alignas",
    "alignof",
    "and",
    "and_eq",
    "asm",
    "atomic_cancel",
    "atomic_commit",
    "atomic_noexcept",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "char8_t",
    "char16_t",
    "char32_t",
    "class",
    "compl",
    "concept",
    "const",
    "consteval",
    "constexpr",
    "constinit",
    "const_cast",
    "continue",
    "co_await",
    "co_return",
    "co_yield",
    "decltype",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    "noexcept",
    "not",
    "not_eq",
    "nullptr",
    "operator",
    "or",
    "or_eq",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "requires",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq",
];

/// A valid C++ identifier for a spec name, appending `_` if the result is a keyword.
pub fn ident(name: &str) -> String {
    let s = sanitize(name);
    if KEYWORDS.contains(&s.as_str()) {
        format!("{s}_")
    } else {
        s
    }
}

/// `add_imm` -> `AddImm`, `alu` -> `Alu` (used for dispatch-group `enum class` types/variants).
pub fn pascal(name: &str) -> String {
    let mut out = String::new();
    for seg in sanitize(name).split('_') {
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

/// The accessor function name for a computed operand, from its deduplicated logical name (see
/// [`chipi_core::accessor::computed_accessor_names`]).
pub fn computed_method(logical: &str) -> String {
    sanitize(logical)
}

/// The C++ return type for a value (`int32_t/uint32_t/int64_t/uint64_t`, keyed on whether the width is 32 or less).
pub fn ret_type(ty: &FieldTy) -> &'static str {
    match (ty.signed, ty.value_width <= 32) {
        (true, true) => "int32_t",
        (false, true) => "uint32_t",
        (true, false) => "int64_t",
        (false, false) => "uint64_t",
    }
}

pub use chipi_core::compute::mask_u64;

/// A 128bit literal cast to `cast`. A single `0x..ull` when it fits in 64 bits, otherwise a hi/lo
/// `<< 64` split with each half cast.
fn split128(cast: &str, v: u128) -> String {
    if v <= u64::MAX as u128 {
        format!("({cast})0x{:x}ull", v as u64)
    } else {
        let hi = (v >> 64) as u64;
        let lo = v as u64;
        format!("((({cast})0x{hi:x}ull << 64) | ({cast})0x{lo:x}ull)")
    }
}

/// A `u128` literal expression (value layer): `(u128)0x..ull` or a hi/lo split for wide values.
pub fn u128_lit(v: u128) -> String {
    split128("u128", v)
}

/// A `unsigned __int128` mask literal (for condition-layer slice masks).
pub fn u128_mask_lit(v: u128) -> String {
    split128("unsigned __int128", v)
}

/// A `__int128` literal expression (condition layer).
pub fn i128_lit(v: u128) -> String {
    if v <= u64::MAX as u128 {
        format!("(__int128)0x{:x}ull", v as u64)
    } else {
        format!("(__int128){}", split128("unsigned __int128", v))
    }
}

/// Escape a string as a C string literal (mirrors Rust `{:?}` of a `&str` for the subset used).
pub fn c_string(s: &str) -> String {
    let mut out = String::from("\"");
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\x{:02x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}
