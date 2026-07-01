//! Display-template parsing and value rendering.
//!
//! A template is literal text plus `{field}` / `{field:spec}` placeholders and `{cond ? a : b}`
//! conditionals. `\{ \} \? \: \\` are escapes. Conditional branches are sub-templates and may
//! nest. Parsing produces [`Seg`]s consumed by the evaluator and the backends.

use crate::model::{Disp, DispHint, FieldTy, NameDefault};
use chipi_syntax::ast::Expr;
use chipi_syntax::{Diag, Span};

/// Upper bound on a placeholder's zero-pad width (guards against absurd pad widths).
pub const MAX_ZERO_PAD: usize = 256;

/// One rendered template segment.
#[derive(Clone, Debug)]
pub enum Seg {
    Lit(String),
    Field {
        name: String,
        fmt: FmtSpec,
    },
    /// `{field.output}`: a named string output of a subdecoder-typed field.
    SubField {
        field: String,
        output: String,
    },
    /// `{cond ? then : else}`, where branches are sub-templates.
    Cond {
        cond: Expr,
        then: Vec<Seg>,
        els: Vec<Seg>,
    },
}

/// A placeholder format specifier.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FmtSpec {
    pub hex: bool,
    pub dec: bool,
    /// `#`: emit a `0x` prefix on hex.
    pub alt: bool,
    /// `:sym`: resolve as an address via the disasm context (falls back to normal rendering).
    pub sym: bool,
    /// `:rel`: a signed PC-relative displacement; the contextual disassembler resolves the target.
    pub rel: bool,
    pub zero_pad: usize,
}

/// Parse a display template into segments. `at` approximates error locations.
pub fn parse_template(s: &str, at: Span) -> Result<Vec<Seg>, Diag> {
    let chars: Vec<char> = s.chars().collect();
    parse_segs(&chars, at)
}

fn parse_segs(chars: &[char], at: Span) -> Result<Vec<Seg>, Diag> {
    let mut segs = Vec::new();
    let mut lit = String::new();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            '\\' if i + 1 < chars.len() && matches!(chars[i + 1], '{' | '}' | '?' | ':' | '\\') => {
                lit.push(chars[i + 1]);
                i += 2;
            }
            '{' => {
                if !lit.is_empty() {
                    segs.push(Seg::Lit(std::mem::take(&mut lit)));
                }
                let (inner, next) = braced(chars, i, at)?;
                segs.push(brace_seg(&inner, at)?);
                i = next;
            }
            '}' => {
                return Err(Diag::error(
                    "BadDisplayTemplate",
                    "stray `}` in display template (use `\\}` for a literal brace)",
                    at,
                ))
            }
            c => {
                lit.push(c);
                i += 1;
            }
        }
    }
    if !lit.is_empty() {
        segs.push(Seg::Lit(lit));
    }
    Ok(segs)
}

/// From `chars[open] == '{'`, return the balanced inner content and the index past the matching `}`.
fn braced(chars: &[char], open: usize, at: Span) -> Result<(Vec<char>, usize), Diag> {
    let mut depth = 0usize;
    let mut inner = Vec::new();
    let mut i = open;

    while i < chars.len() {
        match chars[i] {
            '{' => {
                depth += 1;
                if depth > 1 {
                    inner.push('{');
                }
            }
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return Ok((inner, i + 1));
                }
                inner.push('}');
            }
            c => inner.push(c),
        }
        i += 1;
    }
    Err(Diag::error(
        "BadDisplayTemplate",
        "unterminated `{` in display template",
        at,
    ))
}

fn brace_seg(inner: &[char], at: Span) -> Result<Seg, Diag> {
    if let Some(q) = top_level(inner, '?') {
        let cond_src: String = inner[..q].iter().collect();
        let rest = &inner[q + 1..];
        let (then_src, else_src): (&[char], &[char]) = match top_level(rest, ':') {
            Some(c) => (&rest[..c], &rest[c + 1..]),
            None => (rest, &[]),
        };
        let cond = chipi_syntax::parse_expr_str(cond_src.trim()).map_err(|_| {
            Diag::error(
                "BadDisplayCondition",
                format!(
                    "invalid condition `{}` in display template",
                    cond_src.trim()
                ),
                at,
            )
        })?;
        return Ok(Seg::Cond {
            cond,
            then: parse_segs(then_src, at)?,
            els: parse_segs(else_src, at)?,
        });
    }
    let body: String = inner.iter().collect();

    // `{field.output}`: a subdecoder field-access. A `.` is only valid here (field/output names are
    // plain identifiers), so its presence unambiguously marks a sub-field reference.
    let trimmed = body.trim();
    if let Some((field, output)) = trimmed.split_once('.') {
        let ident_ok =
            |s: &str| !s.is_empty() && s.chars().all(|c| c.is_alphanumeric() || c == '_');
        if ident_ok(field) && ident_ok(output) {
            return Ok(Seg::SubField {
                field: field.to_string(),
                output: output.to_string(),
            });
        }
    }

    let (name, fmt) = placeholder(&body, at)?;
    Ok(Seg::Field { name, fmt })
}

/// First index of `target` at brace- and bracket-depth 0 (so `:` inside `[hi:lo]` or a nested `{...}`
/// is not mistaken for a conditional separator).
fn top_level(chars: &[char], target: char) -> Option<usize> {
    let (mut bd, mut kd) = (0i32, 0i32);
    for (i, &c) in chars.iter().enumerate() {
        match c {
            '{' => bd += 1,
            '}' => bd -= 1,
            '[' => kd += 1,
            ']' => kd -= 1,
            _ if c == target && bd == 0 && kd == 0 => return Some(i),
            _ => {}
        }
    }
    None
}

fn placeholder(body: &str, at: Span) -> Result<(String, FmtSpec), Diag> {
    let body = body.trim();
    if body.contains(['+', '*', '(']) {
        return Err(Diag::error(
            "Unsupported",
            "in-template arithmetic (`{a+b}`) is not supported",
            at,
        ));
    }
    let (name, spec) = match body.split_once(':') {
        Some((n, s)) => (n.trim(), Some(s.trim())),
        None => (body, None),
    };
    if name.is_empty() {
        return Err(Diag::error(
            "BadDisplayTemplate",
            "empty `{}` placeholder in display template",
            at,
        ));
    }
    // A spec is one or more `:`-separated parts. `sym`/`rel` pick address resolution and may be
    // combined with a width/base part that renders the fallback when no symbol is known, e.g.
    // `06x:sym` prints the symbol if the context has one, otherwise a 6-digit hex value.
    let mut fmt = FmtSpec::default();
    if let Some(spec) = spec {
        for part in spec.split(':') {
            match part.trim() {
                "sym" => fmt.sym = true,
                "rel" => fmt.rel = true,
                part => {
                    for ch in part.chars() {
                        match ch {
                            '#' => fmt.alt = true,
                            'x' | 'X' => fmt.hex = true,
                            'd' => fmt.dec = true,
                            '0'..='9' => {
                                fmt.zero_pad = fmt
                                    .zero_pad
                                    .saturating_mul(10)
                                    .saturating_add(ch as usize - '0' as usize);
                            }
                            _ => {
                                return Err(Diag::error(
                                    "BadDisplayTemplate",
                                    format!("unsupported format spec `{part}` in display template"),
                                    at,
                                ))
                            }
                        }
                    }
                }
            }
        }
    }

    if fmt.zero_pad > MAX_ZERO_PAD {
        return Err(Diag::error(
            "BadDisplayTemplate",
            format!("zero-pad width in display template is too large (max {MAX_ZERO_PAD})"),
            at,
        ));
    }
    Ok((name.to_string(), fmt))
}

/// Render `value` under a `dec`/`hex`/`signed_hex` hint (shared by a type's `Disp::Hint` and a
/// `names { ... }` table's numeric default).
fn render_hint(value: i128, ty: &FieldTy, hint: DispHint) -> String {
    match hint {
        DispHint::Hex => hex(value, ty.signed, ty.value_width, true, 0),
        DispHint::SignedHex => signed_hex(value),
        DispHint::Dec => dec(value, ty.signed),
    }
}

/// Render one field value against its type's display spec and a placeholder format.
pub fn render_field(value: i128, ty: &FieldTy, fmt: &FmtSpec) -> String {
    if fmt.hex {
        return hex(value, ty.signed, ty.value_width, fmt.alt, fmt.zero_pad);
    }
    if fmt.dec {
        return dec(value, ty.signed);
    }
    match &ty.disp {
        Disp::Pattern(p) => p.replace("{}", &dec(value, ty.signed)),
        Disp::Hint(h) => render_hint(value, ty, *h),
        Disp::Names(t) => match t.lookup(value as u64) {
            Some(s) => s.to_string(),
            None => match &t.default {
                NameDefault::Str(s) => s.clone(),
                NameDefault::Hint(h) => render_hint(value, ty, *h),
            },
        },
        Disp::None => {
            if ty.signed {
                dec(value, true)
            } else {
                hex(value, false, ty.value_width, true, 0)
            }
        }
    }
}

fn dec(value: i128, signed: bool) -> String {
    if signed {
        format!("{value}")
    } else {
        format!("{}", value as u128)
    }
}

fn hex(value: i128, signed: bool, value_width: u16, alt: bool, zero_pad: usize) -> String {
    let magnitude: u128 = if signed && value < 0 {
        (value as u128) & crate::compute::mask128(value_width.max(1))
    } else {
        value as u128
    };
    let mut digits = format!("{magnitude:x}");
    if digits.len() < zero_pad {
        digits = format!("{}{}", "0".repeat(zero_pad - digits.len()), digits);
    }

    if alt {
        format!("0x{digits}")
    } else {
        digits
    }
}

fn signed_hex(value: i128) -> String {
    if value < 0 {
        format!("-0x{:x}", value.unsigned_abs())
    } else {
        format!("0x{value:x}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn field_fmt(body: &str) -> FmtSpec {
        let (_, fmt) = placeholder(body, Span::at(0)).expect("placeholder parses");
        fmt
    }

    // `sym`/`rel` are modifiers, not exclusive specs: a width/base part alongside them supplies the
    // fallback rendering used when the disasm context has no symbol for the address.
    #[test]
    fn sym_combines_with_width() {
        let fmt = field_fmt("addr:06x:sym");
        assert!(fmt.sym);
        assert!(fmt.hex);
        assert_eq!(fmt.zero_pad, 6);

        // Order is irrelevant, and bare `:sym` still works with a default (unpadded) fallback.
        assert_eq!(field_fmt("addr:sym:04x"), field_fmt("addr:04x:sym"));
        let bare = field_fmt("addr:sym");
        assert!(bare.sym && !bare.hex && bare.zero_pad == 0);
    }

    #[test]
    fn unknown_spec_part_still_rejected() {
        assert!(placeholder("addr:06q:sym", Span::at(0)).is_err());
    }
}
