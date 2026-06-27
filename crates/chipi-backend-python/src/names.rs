//! Identifier and naming helpers for emitted Python code.

/// Map non-identifier characters to `_`. Prefix `_` if the result is empty or starts with a digit.
pub fn sanitize(name: &str) -> String {
    let mut out = String::new();

    for c in name.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            out.push(c);
        } else {
            out.push('_');
        }
    }

    if out.is_empty() || out.starts_with(|c: char| c.is_ascii_digit()) {
        out.insert(0, '_');
    }

    out
}

/// The full Python 3 keyword set plus the soft keywords `match` and `case`.
const KEYWORDS: &[&str] = &[
    "False", "None", "True", "and", "as", "assert", "async", "await", "break", "class", "continue",
    "def", "del", "elif", "else", "except", "finally", "for", "from", "global", "if", "import",
    "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try", "while",
    "with", "yield", "match", "case",
];

fn is_python_keyword(s: &str) -> bool {
    KEYWORDS.contains(&s)
}

/// A valid Python identifier for a spec name. Keywords get a trailing `_`.
pub fn ident(name: &str) -> String {
    let s = sanitize(name);
    if is_python_keyword(&s) {
        format!("{s}_")
    } else {
        s
    }
}

/// The module-level `OP_*` opcode-id constant name for an opcode (`Invalid` -> `OP_INVALID`).
pub fn cname(name: &str) -> String {
    sanitize(name).to_uppercase()
}

/// `add_imm` -> `AddImm`, `alu` -> `Alu` (used for the dispatch-group `IntEnum` class names).
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

/// The deduplicated accessor function name for `instr`'s computed operand `name`.
pub fn comp_acc(
    acc: &std::collections::HashMap<(String, String), String>,
    instr: &str,
    name: &str,
) -> String {
    computed_method(&acc[&(instr.to_string(), name.to_string())])
}
