//! Character-level parser for format string internals.
//!
//! Parses format strings like `"b{lk ? l}{aa ? a} {li:#x}"` into
//! a sequence of [`FormatPiece`] values.

use crate::error::{Error, ErrorKind, Span};
use crate::types::*;

/// Parse a format string into a sequence of format pieces.
///
/// The input should be the raw content between quotes (without the quotes).
pub fn parse_format_string(input: &str, span: &Span) -> Result<Vec<FormatPiece>, Error> {
    let mut pieces = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut pos = 0;
    let mut literal = String::new();

    while pos < chars.len() {
        match chars[pos] {
            '\\' if pos + 1 < chars.len() => {
                match chars[pos + 1] {
                    '{' | '}' | '?' | ':' | '\\' => {
                        literal.push(chars[pos + 1]);
                        pos += 2;
                    }
                    _ => {
                        literal.push('\\');
                        literal.push(chars[pos + 1]);
                        pos += 2;
                    }
                }
            }
            '{' => {
                if !literal.is_empty() {
                    pieces.push(FormatPiece::Literal(std::mem::take(&mut literal)));
                }
                pos += 1;
                let piece = parse_expr_block(&chars, &mut pos, span)?;
                pieces.push(piece);
            }
            _ => {
                literal.push(chars[pos]);
                pos += 1;
            }
        }
    }

    if !literal.is_empty() {
        pieces.push(FormatPiece::Literal(literal));
    }

    Ok(pieces)
}

/// Parse the content inside `{...}` and return a FormatPiece::FieldRef.
fn parse_expr_block(chars: &[char], pos: &mut usize, span: &Span) -> Result<FormatPiece, Error> {
    skip_ws(chars, pos);

    // Collect all content until the closing `}`
    let content_start = *pos;
    let mut depth = 1;
    while *pos < chars.len() {
        match chars[*pos] {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            '\\' => {
                *pos += 1; // skip next
            }
            _ => {}
        }
        *pos += 1;
    }

    if depth != 0 {
        return Err(Error::new(
            ErrorKind::InvalidFormatString("unclosed '{'".to_string()),
            span.clone(),
        ));
    }

    let content: String = chars[content_start..*pos].iter().collect();
    *pos += 1; // skip closing '}'

    // Check for ternary (has `?` not inside parentheses)
    if let Some(ternary_pos) = find_ternary_question(&content) {
        let field = content[..ternary_pos].trim().to_string();
        let rest = &content[ternary_pos + 1..];

        // Check for `:` separator for else branch
        if let Some(colon_pos) = find_ternary_colon(rest) {
            let if_nonzero = rest[..colon_pos].trim().to_string();
            let if_zero = rest[colon_pos + 1..].trim().to_string();
            return Ok(FormatPiece::FieldRef {
                expr: FormatExpr::Ternary {
                    field,
                    if_nonzero,
                    if_zero: Some(if_zero),
                },
                spec: None,
            });
        } else {
            let if_nonzero = rest.trim().to_string();
            return Ok(FormatPiece::FieldRef {
                expr: FormatExpr::Ternary {
                    field,
                    if_nonzero,
                    if_zero: None,
                },
                spec: None,
            });
        }
    }

    // Split off format specifier: last `:` not inside parentheses
    let (expr_str, spec) = split_format_spec(&content);

    let expr = parse_expression(expr_str.trim(), span)?;

    Ok(FormatPiece::FieldRef {
        expr,
        spec: spec.map(|s| s.to_string()),
    })
}

/// Parse an expression string (field, arithmetic, map call, builtin call, int literal).
fn parse_expression(input: &str, span: &Span) -> Result<FormatExpr, Error> {
    let input = input.trim();
    if input.is_empty() {
        return Err(Error::new(
            ErrorKind::InvalidFormatString("empty expression".to_string()),
            span.clone(),
        ));
    }

    // Try arithmetic: look for +, -, * at the top level (not inside parens)
    // Lowest precedence first: +, -
    if let Some(op_pos) = find_top_level_op(input, &['+', '-']) {
        let left = input[..op_pos].trim();
        let op_char = input.as_bytes()[op_pos] as char;
        let right = input[op_pos + 1..].trim();
        let op = match op_char {
            '+' => ArithOp::Add,
            '-' => ArithOp::Sub,
            _ => unreachable!(),
        };
        return Ok(FormatExpr::Arithmetic {
            left: Box::new(parse_expression(left, span)?),
            op,
            right: Box::new(parse_expression(right, span)?),
        });
    }

    // Next precedence: *, /, %
    if let Some(op_pos) = find_top_level_op(input, &['*', '/', '%']) {
        let left = input[..op_pos].trim();
        let op_char = input.as_bytes()[op_pos] as char;
        let right = input[op_pos + 1..].trim();
        let op = match op_char {
            '*' => ArithOp::Mul,
            '/' => ArithOp::Div,
            '%' => ArithOp::Mod,
            _ => unreachable!(),
        };
        return Ok(FormatExpr::Arithmetic {
            left: Box::new(parse_expression(left, span)?),
            op,
            right: Box::new(parse_expression(right, span)?),
        });
    }

    // Check for function call: identifier(args)
    if let Some(paren_pos) = input.find('(') {
        if input.ends_with(')') {
            let func_name = input[..paren_pos].trim();
            let args_str = &input[paren_pos + 1..input.len() - 1];
            let args = parse_arg_list(args_str, span)?;

            // Check if it's a builtin
            match func_name {
                "rotate_right" => {
                    return Ok(FormatExpr::BuiltinCall {
                        func: BuiltinFunc::RotateRight,
                        args,
                    });
                }
                "rotate_left" => {
                    return Ok(FormatExpr::BuiltinCall {
                        func: BuiltinFunc::RotateLeft,
                        args,
                    });
                }
                _ => {
                    return Ok(FormatExpr::MapCall {
                        map_name: func_name.to_string(),
                        args,
                    });
                }
            }
        }
    }

    // Integer literal
    if let Some(val) = try_parse_int(input) {
        return Ok(FormatExpr::IntLiteral(val));
    }

    // Must be a field reference
    if is_valid_identifier(input) {
        return Ok(FormatExpr::Field(input.to_string()));
    }

    Err(Error::new(
        ErrorKind::InvalidFormatString(format!("invalid expression '{}'", input)),
        span.clone(),
    ))
}

/// Parse a comma-separated list of argument expressions.
fn parse_arg_list(input: &str, span: &Span) -> Result<Vec<FormatExpr>, Error> {
    if input.trim().is_empty() {
        return Ok(Vec::new());
    }

    let mut args = Vec::new();
    let mut depth = 0;
    let mut start = 0;

    for (i, ch) in input.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => {
                let arg = input[start..i].trim();
                args.push(parse_expression(arg, span)?);
                start = i + 1;
            }
            _ => {}
        }
    }

    let last = input[start..].trim();
    if !last.is_empty() {
        args.push(parse_expression(last, span)?);
    }

    Ok(args)
}

/// Find the position of `?` for ternary, not inside parentheses.
fn find_ternary_question(s: &str) -> Option<usize> {
    let mut depth = 0;
    for (i, ch) in s.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            '\\' => continue,
            '?' if depth == 0 => return Some(i),
            _ => {}
        }
    }
    None
}

/// Find the position of `:` for ternary else branch (not inside parens, not escaped).
fn find_ternary_colon(s: &str) -> Option<usize> {
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    let mut depth = 0;
    let mut byte_pos = 0;

    while i < chars.len() {
        match chars[i] {
            '(' => depth += 1,
            ')' => depth -= 1,
            '\\' => {
                i += 1;
                byte_pos += chars[i - 1].len_utf8();
                if i < chars.len() {
                    byte_pos += chars[i].len_utf8();
                }
                i += 1;
                continue;
            }
            ':' if depth == 0 => return Some(byte_pos),
            _ => {}
        }
        byte_pos += chars[i].len_utf8();
        i += 1;
    }
    None
}

/// Split off format specifier from expression.
/// Returns (expression, optional_spec).
fn split_format_spec(content: &str) -> (&str, Option<&str>) {
    // Find the last `:` not inside parentheses
    let mut depth = 0;
    let mut last_colon = None;

    for (i, ch) in content.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            ':' if depth == 0 => last_colon = Some(i),
            _ => {}
        }
    }

    if let Some(colon_pos) = last_colon {
        let spec = &content[colon_pos + 1..];
        // Validate it looks like a format spec (starts with format chars, not an identifier)
        if is_format_spec(spec) {
            return (&content[..colon_pos], Some(spec));
        }
    }

    (content, None)
}

/// Check if a string looks like a Rust format specifier.
fn is_format_spec(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    // Common format specs: #x, #X, #o, #b, #04x, 04x, x, b, etc.
    let first = s.chars().next().unwrap();
    first == '#' || first == '0' || first == 'x' || first == 'X' || first == 'o' || first == 'b'
        || first == '?' || first == 'e' || first == 'E' || first.is_ascii_digit()
}

/// Find a top-level binary operator (not inside parens), scanning right-to-left for left-associativity.
fn find_top_level_op(s: &str, ops: &[char]) -> Option<usize> {
    let chars: Vec<char> = s.chars().collect();
    let mut depth = 0;
    let mut last = None;

    // Map char indices to byte positions
    let mut byte_positions = Vec::with_capacity(chars.len());
    let mut byte_pos = 0;
    for &ch in &chars {
        byte_positions.push(byte_pos);
        byte_pos += ch.len_utf8();
    }

    for (i, &ch) in chars.iter().enumerate() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            c if depth == 0 && ops.contains(&c) => {
                // Don't match if at the very start (could be unary minus)
                if i > 0 {
                    last = Some(byte_positions[i]);
                }
            }
            _ => {}
        }
    }

    last
}

fn try_parse_int(s: &str) -> Option<i64> {
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        i64::from_str_radix(hex, 16).ok()
    } else {
        s.parse::<i64>().ok()
    }
}

fn is_valid_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn skip_ws(chars: &[char], pos: &mut usize) {
    while *pos < chars.len() && chars[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::new("test", 1, 1, 0)
    }

    #[test]
    fn test_plain_literal() {
        let pieces = parse_format_string("hello", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::Literal(s) => assert_eq!(s, "hello"),
            _ => panic!("expected literal"),
        }
    }

    #[test]
    fn test_simple_field_ref() {
        let pieces = parse_format_string("val={field}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 2);
        match &pieces[0] {
            FormatPiece::Literal(s) => assert_eq!(s, "val="),
            _ => panic!("expected literal"),
        }
        match &pieces[1] {
            FormatPiece::FieldRef { expr, spec } => {
                assert!(matches!(expr, FormatExpr::Field(f) if f == "field"));
                assert!(spec.is_none());
            }
            _ => panic!("expected field ref"),
        }
    }

    #[test]
    fn test_field_with_format_spec() {
        let pieces = parse_format_string("{field:#x}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::FieldRef { expr, spec } => {
                assert!(matches!(expr, FormatExpr::Field(f) if f == "field"));
                assert_eq!(spec.as_deref(), Some("#x"));
            }
            _ => panic!("expected field ref"),
        }
    }

    #[test]
    fn test_ternary_no_else() {
        let pieces = parse_format_string("{lk ? l}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::FieldRef { expr, .. } => match expr {
                FormatExpr::Ternary {
                    field,
                    if_nonzero,
                    if_zero,
                } => {
                    assert_eq!(field, "lk");
                    assert_eq!(if_nonzero, "l");
                    assert!(if_zero.is_none());
                }
                _ => panic!("expected ternary"),
            },
            _ => panic!("expected field ref"),
        }
    }

    #[test]
    fn test_ternary_with_else() {
        let pieces = parse_format_string("{aa ? a : b}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::FieldRef { expr, .. } => match expr {
                FormatExpr::Ternary {
                    field,
                    if_nonzero,
                    if_zero,
                } => {
                    assert_eq!(field, "aa");
                    assert_eq!(if_nonzero, "a");
                    assert_eq!(if_zero.as_deref(), Some("b"));
                }
                _ => panic!("expected ternary"),
            },
            _ => panic!("expected field ref"),
        }
    }

    #[test]
    fn test_arithmetic() {
        let pieces = parse_format_string("{a + b * 4}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::FieldRef { expr, .. } => match expr {
                FormatExpr::Arithmetic { op, .. } => {
                    assert_eq!(*op, ArithOp::Add);
                }
                _ => panic!("expected arithmetic"),
            },
            _ => panic!("expected field ref"),
        }
    }

    #[test]
    fn test_map_call() {
        let pieces = parse_format_string("{spr_name(spr)}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::FieldRef { expr, .. } => match expr {
                FormatExpr::MapCall { map_name, args } => {
                    assert_eq!(map_name, "spr_name");
                    assert_eq!(args.len(), 1);
                }
                _ => panic!("expected map call"),
            },
            _ => panic!("expected field ref"),
        }
    }

    #[test]
    fn test_builtin_call() {
        let pieces = parse_format_string("{rotate_right(val, amt)}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::FieldRef { expr, .. } => match expr {
                FormatExpr::BuiltinCall { func, args } => {
                    assert_eq!(*func, BuiltinFunc::RotateRight);
                    assert_eq!(args.len(), 2);
                }
                _ => panic!("expected builtin call"),
            },
            _ => panic!("expected field ref"),
        }
    }

    #[test]
    fn test_escaped_braces() {
        let pieces = parse_format_string("a\\{b\\}c", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::Literal(s) => assert_eq!(s, "a{b}c"),
            _ => panic!("expected literal"),
        }
    }

    #[test]
    fn test_int_literal() {
        let pieces = parse_format_string("{42}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 1);
        match &pieces[0] {
            FormatPiece::FieldRef { expr, .. } => {
                assert!(matches!(expr, FormatExpr::IntLiteral(42)));
            }
            _ => panic!("expected field ref"),
        }
    }

    #[test]
    fn test_mixed() {
        let pieces =
            parse_format_string("b{lk ? l}{aa ? a} {li:#x}", &test_span()).unwrap();
        assert_eq!(pieces.len(), 5);
    }
}
