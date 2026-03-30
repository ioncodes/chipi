//! Shared Python code generation helpers.
//!
//! Provides reusable functions for emitting Python code from the chipi IR.
//! Used by the IDA backend (and future Binary Ninja backend).

use std::collections::HashMap;
use std::fmt::Write;

use crate::tree::DecodeNode;
use crate::types::*;

/// Configuration for display formatting in generated Python code.
#[derive(Debug, Clone, Default)]
pub struct DisplayConfig {
    /// Maps type alias names to display prefixes (e.g., "gpr" -> "r", "fpr" -> "f").
    pub type_prefixes: HashMap<String, String>,
}

/// Emit `_fmt_signed_hex` and `_fmt_hex` helper functions if needed.
pub fn emit_display_format_helpers(out: &mut String, def: &ValidatedDef) {
    if needs_display_format(def) {
        writeln!(out, "def _fmt_signed_hex(v):").unwrap();
        writeln!(out, "    if v < 0:").unwrap();
        writeln!(out, "        return f\"-0x{{-v:x}}\"").unwrap();
        writeln!(out, "    return f\"0x{{v:x}}\"").unwrap();
        writeln!(out).unwrap();
        writeln!(out).unwrap();
        writeln!(out, "def _fmt_hex(v):").unwrap();
        writeln!(out, "    return f\"0x{{v:x}}\"").unwrap();
        writeln!(out).unwrap();
        writeln!(out).unwrap();
    }
}

/// Check if any field uses display(hex) or display(signed_hex).
fn needs_display_format(def: &ValidatedDef) -> bool {
    for instr in &def.instructions {
        for field in &instr.resolved_fields {
            if field.resolved_type.display_format.is_some() {
                return true;
            }
        }
    }
    for sd in &def.sub_decoders {
        for instr in &sd.instructions {
            for field in &instr.resolved_fields {
                if field.resolved_type.display_format.is_some() {
                    return true;
                }
            }
        }
    }
    false
}

/// Emit a `_sign_extend(val, bits)` Python helper function.
pub fn emit_sign_extend_helper(out: &mut String) {
    writeln!(out, "def _sign_extend(val, bits):").unwrap();
    writeln!(out, "    mask = 1 << (bits - 1)").unwrap();
    writeln!(out, "    return (val ^ mask) - mask").unwrap();
    writeln!(out).unwrap();
    writeln!(out).unwrap();
}

/// Emit a `_rotate_right(val, amt, width)` Python helper function.
pub fn emit_rotate_helpers(out: &mut String) {
    writeln!(out, "def _rotate_right(val, amt, width=32):").unwrap();
    writeln!(out, "    amt = amt % width").unwrap();
    writeln!(out, "    mask = (1 << width) - 1").unwrap();
    writeln!(out, "    val = val & mask").unwrap();
    writeln!(
        out,
        "    return ((val >> amt) | (val << (width - amt))) & mask"
    )
    .unwrap();
    writeln!(out).unwrap();
    writeln!(out).unwrap();
    writeln!(out, "def _rotate_left(val, amt, width=32):").unwrap();
    writeln!(out, "    amt = amt % width").unwrap();
    writeln!(out, "    mask = (1 << width) - 1").unwrap();
    writeln!(out, "    val = val & mask").unwrap();
    writeln!(
        out,
        "    return ((val << amt) | (val >> (width - amt))) & mask"
    )
    .unwrap();
    writeln!(out).unwrap();
    writeln!(out).unwrap();
}

/// Generate Python dict-based map (lookup) functions.
pub fn emit_map_functions_python(out: &mut String, maps: &[MapDef]) {
    for map_def in maps {
        let params: Vec<&str> = map_def.params.iter().map(|s| s.as_str()).collect();
        writeln!(out, "def {}({}):", map_def.name, params.join(", ")).unwrap();

        // Find the wildcard/default entry
        let default_entry = map_def
            .entries
            .iter()
            .find(|e| e.keys.len() == 1 && e.keys[0] == MapKey::Wildcard);

        // Build the lookup dict
        writeln!(out, "    _MAP = {{").unwrap();
        for entry in &map_def.entries {
            if entry.keys.iter().any(|k| *k == MapKey::Wildcard) {
                continue; // skip wildcard, it's the default
            }
            let key = if entry.keys.len() == 1 {
                format_map_key_python(&entry.keys[0])
            } else {
                let keys: Vec<String> = entry.keys.iter().map(format_map_key_python).collect();
                format!("({})", keys.join(", "))
            };
            let value = format_pieces_to_python_str_simple(&entry.output);
            writeln!(out, "        {}: {},", key, value).unwrap();
        }
        writeln!(out, "    }}").unwrap();

        let lookup_key = if params.len() == 1 {
            params[0].to_string()
        } else {
            format!("({})", params.join(", "))
        };

        let default_val = if let Some(entry) = default_entry {
            format_pieces_to_python_str_simple(&entry.output)
        } else {
            "\"???\"".to_string()
        };

        writeln!(out, "    return _MAP.get({}, {})", lookup_key, default_val).unwrap();
        writeln!(out).unwrap();
        writeln!(out).unwrap();
    }
}

fn format_map_key_python(key: &MapKey) -> String {
    match key {
        MapKey::Value(v) => format!("{}", v),
        MapKey::Wildcard => "None".to_string(),
    }
}

/// Convert format pieces to a simple Python string expression (no IDA-specific formatting).
/// Returns a Python expression that evaluates to a string.
fn format_pieces_to_python_str_simple(pieces: &[FormatPiece]) -> String {
    if pieces.is_empty() {
        return "\"\"".to_string();
    }

    // Check if all pieces are literals
    let all_literal = pieces.iter().all(|p| matches!(p, FormatPiece::Literal(_)));
    if all_literal {
        let mut s = String::new();
        for piece in pieces {
            if let FormatPiece::Literal(lit) = piece {
                s.push_str(lit);
            }
        }
        return format!("\"{}\"", escape_python_str(&s));
    }

    // Build f-string
    let mut parts = String::new();
    parts.push_str("f\"");
    for piece in pieces {
        match piece {
            FormatPiece::Literal(lit) => {
                parts.push_str(&escape_python_fstr(lit));
            }
            FormatPiece::FieldRef { expr, spec } => {
                parts.push('{');
                parts.push_str(&expr_to_python_simple(expr));
                if let Some(spec) = spec {
                    parts.push(':');
                    parts.push_str(spec);
                }
                parts.push('}');
            }
        }
    }
    parts.push('"');
    parts
}

fn expr_to_python_simple(expr: &FormatExpr) -> String {
    match expr {
        FormatExpr::Field(name) => name.clone(),
        FormatExpr::IntLiteral(val) => format!("{}", val),
        FormatExpr::Arithmetic { left, op, right } => {
            let l = expr_to_python_simple(left);
            let r = expr_to_python_simple(right);
            let op_str = arith_op_str(op);
            format!("({} {} {})", l, op_str, r)
        }
        FormatExpr::MapCall { map_name, args } => {
            let arg_strs: Vec<String> = args.iter().map(|a| expr_to_python_simple(a)).collect();
            format!("{}({})", map_name, arg_strs.join(", "))
        }
        FormatExpr::Ternary {
            field,
            if_nonzero,
            if_zero,
        } => {
            let else_val = if_zero.as_deref().unwrap_or("");
            format!(
                "(\"{}\" if {} else \"{}\")",
                escape_python_str(if_nonzero),
                field,
                escape_python_str(else_val)
            )
        }
        FormatExpr::BuiltinCall { func, args } => {
            let arg_strs: Vec<String> = args.iter().map(|a| expr_to_python_simple(a)).collect();
            match func {
                BuiltinFunc::RotateRight => {
                    format!(
                        "_rotate_right({}, {})",
                        arg_strs.first().map(|s| s.as_str()).unwrap_or("0"),
                        arg_strs.get(1).map(|s| s.as_str()).unwrap_or("0")
                    )
                }
                BuiltinFunc::RotateLeft => {
                    format!(
                        "_rotate_left({}, {})",
                        arg_strs.first().map(|s| s.as_str()).unwrap_or("0"),
                        arg_strs.get(1).map(|s| s.as_str()).unwrap_or("0")
                    )
                }
            }
        }
        FormatExpr::SubDecoderAccess { field, fragment } => {
            format!("{}[\"{}\"]", field, fragment)
        }
    }
}

fn arith_op_str(op: &ArithOp) -> &'static str {
    match op {
        ArithOp::Add => "+",
        ArithOp::Sub => "-",
        ArithOp::Mul => "*",
        ArithOp::Div => "//",
        ArithOp::Mod => "%",
    }
}

/// Escape a string for use inside Python string literals.
pub fn escape_python_str(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Escape a string for use inside Python f-string literals.
pub fn escape_python_fstr(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('{', "{{")
        .replace('}', "}}")
}

/// Generate the Python `_decode(data)` function body using the decision tree.
///
/// Returns the complete function as a string. The function returns
/// `(itype_const, fields_dict, byte_size)` or `None`.
pub fn emit_decode_function(
    out: &mut String,
    def: &ValidatedDef,
    tree: &DecodeNode,
    itype_prefix: &str,
) {
    let unit_bytes = def.config.width / 8;
    let endian = match def.config.endian {
        ByteEndian::Big => "big",
        ByteEndian::Little => "little",
    };
    let variable_length = def.instructions.iter().any(|i| i.unit_count() > 1);

    writeln!(out, "def _decode(data):").unwrap();
    writeln!(out, "    if len(data) < {}:", unit_bytes).unwrap();
    writeln!(out, "        return None").unwrap();
    writeln!(
        out,
        "    opcode = int.from_bytes(data[0:{}], byteorder=\"{}\")",
        unit_bytes, endian
    )
    .unwrap();

    emit_tree_python(
        out,
        tree,
        def,
        1,
        itype_prefix,
        variable_length,
        unit_bytes,
        endian,
    );

    writeln!(out).unwrap();
    writeln!(out).unwrap();
}

/// Recursively emit Python decision tree code.
fn emit_tree_python(
    out: &mut String,
    node: &DecodeNode,
    def: &ValidatedDef,
    indent: usize,
    itype_prefix: &str,
    variable_length: bool,
    unit_bytes: u32,
    endian: &str,
) {
    let pad = "    ".repeat(indent);
    match node {
        DecodeNode::Leaf { instruction_index } => {
            let instr = &def.instructions[*instruction_index];
            if let Some(guard) = leaf_guard_python(instr, unit_bytes, endian) {
                writeln!(out, "{}if {}:", pad, guard).unwrap();
                emit_return_decoded(
                    out,
                    instr,
                    itype_prefix,
                    indent + 1,
                    variable_length,
                    unit_bytes,
                    endian,
                );
                writeln!(out, "{}else:", pad).unwrap();
                writeln!(out, "{}    return None", pad).unwrap();
            } else {
                emit_return_decoded(
                    out,
                    instr,
                    itype_prefix,
                    indent,
                    variable_length,
                    unit_bytes,
                    endian,
                );
            }
        }
        DecodeNode::PriorityLeaves { candidates } => {
            for (i, &idx) in candidates.iter().enumerate() {
                let instr = &def.instructions[idx];
                let guard = leaf_guard_python(instr, unit_bytes, endian);

                if i == 0 {
                    if let Some(guard_expr) = guard {
                        writeln!(out, "{}if {}:", pad, guard_expr).unwrap();
                        emit_return_decoded(
                            out,
                            instr,
                            itype_prefix,
                            indent + 1,
                            variable_length,
                            unit_bytes,
                            endian,
                        );
                    } else {
                        emit_return_decoded(
                            out,
                            instr,
                            itype_prefix,
                            indent,
                            variable_length,
                            unit_bytes,
                            endian,
                        );
                        break;
                    }
                } else if i == candidates.len() - 1 {
                    // Last candidate
                    if let Some(guard_expr) = guard {
                        writeln!(out, "{}elif {}:", pad, guard_expr).unwrap();
                        emit_return_decoded(
                            out,
                            instr,
                            itype_prefix,
                            indent + 1,
                            variable_length,
                            unit_bytes,
                            endian,
                        );
                        writeln!(out, "{}else:", pad).unwrap();
                        writeln!(out, "{}    return None", pad).unwrap();
                    } else {
                        writeln!(out, "{}else:", pad).unwrap();
                        emit_return_decoded(
                            out,
                            instr,
                            itype_prefix,
                            indent + 1,
                            variable_length,
                            unit_bytes,
                            endian,
                        );
                    }
                } else {
                    // Middle
                    let guard_expr = guard.unwrap_or_else(|| "True".to_string());
                    writeln!(out, "{}elif {}:", pad, guard_expr).unwrap();
                    emit_return_decoded(
                        out,
                        instr,
                        itype_prefix,
                        indent + 1,
                        variable_length,
                        unit_bytes,
                        endian,
                    );
                }
            }
        }
        DecodeNode::Fail => {
            writeln!(out, "{}return None", pad).unwrap();
        }
        DecodeNode::Branch {
            range,
            arms,
            default,
        } => {
            let extract = extract_expr_python("opcode", &[*range], unit_bytes, endian);
            let var_name = format!("_v{}", indent);
            writeln!(out, "{}{} = {}", pad, var_name, extract).unwrap();

            let mut first = true;
            for (value, child) in arms {
                if first {
                    writeln!(out, "{}if {} == {:#x}:", pad, var_name, value).unwrap();
                    first = false;
                } else {
                    writeln!(out, "{}elif {} == {:#x}:", pad, var_name, value).unwrap();
                }
                emit_tree_python(
                    out,
                    child,
                    def,
                    indent + 1,
                    itype_prefix,
                    variable_length,
                    unit_bytes,
                    endian,
                );
            }

            // Default arm
            if !arms.is_empty() {
                writeln!(out, "{}else:", pad).unwrap();
                emit_tree_python(
                    out,
                    default,
                    def,
                    indent + 1,
                    itype_prefix,
                    variable_length,
                    unit_bytes,
                    endian,
                );
            } else {
                emit_tree_python(
                    out,
                    default,
                    def,
                    indent,
                    itype_prefix,
                    variable_length,
                    unit_bytes,
                    endian,
                );
            }
        }
    }
}

/// Emit Python code to return a decoded instruction tuple.
fn emit_return_decoded(
    out: &mut String,
    instr: &ValidatedInstruction,
    itype_prefix: &str,
    indent: usize,
    variable_length: bool,
    unit_bytes: u32,
    endian: &str,
) {
    let pad = "    ".repeat(indent);
    let unit_count = instr.unit_count();
    let bytes_consumed = unit_count * unit_bytes;
    let itype_const = format!("{}_{}", itype_prefix, instr.name.to_ascii_uppercase());

    if variable_length && unit_count > 1 {
        writeln!(out, "{}if len(data) < {}:", pad, bytes_consumed).unwrap();
        writeln!(out, "{}    return None", pad).unwrap();
    }

    if instr.resolved_fields.is_empty() {
        writeln!(
            out,
            "{}return ({}, {{}}, {})",
            pad, itype_const, bytes_consumed
        )
        .unwrap();
    } else {
        // Extract fields
        for field in &instr.resolved_fields {
            let extract = extract_expr_python("opcode", &field.ranges, unit_bytes, endian);
            let expr = apply_transforms_python(&extract, &field.resolved_type);
            writeln!(out, "{}{} = {}", pad, field.name, expr).unwrap();
        }

        // Build fields dict
        let field_names: Vec<&str> = instr
            .resolved_fields
            .iter()
            .map(|f| f.name.as_str())
            .collect();
        let dict_entries: Vec<String> = field_names
            .iter()
            .map(|n| format!("\"{}\": {}", n, n))
            .collect();
        writeln!(
            out,
            "{}return ({}, {{{}}}, {})",
            pad,
            itype_const,
            dict_entries.join(", "),
            bytes_consumed
        )
        .unwrap();
    }
}

/// Generate a Python expression to extract bits from ranges.
pub fn extract_expr_python(
    var: &str,
    ranges: &[BitRange],
    unit_bytes: u32,
    endian: &str,
) -> String {
    if ranges.is_empty() {
        return "0".to_string();
    }

    if ranges.len() == 1 {
        let range = ranges[0];
        let source = unit_read_python(range.unit, unit_bytes, endian);
        let source = if range.unit == 0 {
            var.to_string()
        } else {
            source
        };

        let width = range.width();
        let shift = range.end;
        let mask = (1u64 << width) - 1;

        if shift == 0 {
            format!("({} & {:#x})", source, mask)
        } else {
            format!("(({} >> {}) & {:#x})", source, shift, mask)
        }
    } else {
        let mut parts = Vec::new();
        let mut accumulated_width = 0u32;

        for range in ranges {
            let source = if range.unit == 0 {
                var.to_string()
            } else {
                unit_read_python(range.unit, unit_bytes, endian)
            };

            let width = range.width();
            let shift = range.end;
            let mask = (1u64 << width) - 1;

            let extracted = if shift == 0 {
                format!("({} & {:#x})", source, mask)
            } else {
                format!("(({} >> {}) & {:#x})", source, shift, mask)
            };

            if accumulated_width > 0 {
                parts.push(format!("({} << {})", extracted, accumulated_width));
            } else {
                parts.push(extracted);
            }

            accumulated_width += width;
        }

        parts.join(" | ")
    }
}

/// Generate a Python expression to read a unit from the data buffer.
fn unit_read_python(unit: u32, unit_bytes: u32, endian: &str) -> String {
    if unit == 0 {
        "opcode".to_string()
    } else {
        let start = unit * unit_bytes;
        let end = start + unit_bytes;
        format!(
            "int.from_bytes(data[{}:{}], byteorder=\"{}\")",
            start, end, endian
        )
    }
}

/// Apply chipi transforms to a Python expression.
pub fn apply_transforms_python(extract_expr: &str, resolved: &ResolvedFieldType) -> String {
    let mut expr = extract_expr.to_string();

    for transform in &resolved.transforms {
        match transform {
            Transform::SignExtend(n) => {
                expr = format!("_sign_extend({}, {})", expr, n);
            }
            Transform::ZeroExtend(_) => {
                // No-op in Python (arbitrary precision)
            }
            Transform::ShiftLeft(n) => {
                expr = format!("(({}) << {})", expr, n);
            }
        }
    }

    // Handle sub-decoder fields
    if let Some(ref sd_name) = resolved.sub_decoder {
        let decode_fn = format!("_decode_{}", to_snake_case(sd_name));
        return format!("{}({})", decode_fn, expr);
    }

    expr
}

/// Compute a guard condition for a leaf node in Python.
/// Returns `None` if no guard is needed.
pub fn leaf_guard_python(
    instr: &ValidatedInstruction,
    unit_bytes: u32,
    endian: &str,
) -> Option<String> {
    let fixed_bits = instr.fixed_bits();
    if fixed_bits.is_empty() {
        return None;
    }

    let mut units_map: HashMap<u32, Vec<(u32, Bit)>> = HashMap::new();
    for (unit, hw_bit, bit) in fixed_bits {
        units_map.entry(unit).or_default().push((hw_bit, bit));
    }

    let mut conditions = Vec::new();

    for (unit, bits) in &units_map {
        let (mask, value) = compute_mask_value(bits);
        if mask != 0 {
            let source = if *unit == 0 {
                "opcode".to_string()
            } else {
                unit_read_python(*unit, unit_bytes, endian)
            };
            conditions.push(format!("{} & {:#x} == {:#x}", source, mask, value));
        }
    }

    if conditions.is_empty() {
        None
    } else {
        Some(conditions.join(" and "))
    }
}

/// Compute a bitmask and expected value from fixed bits.
fn compute_mask_value(fixed_bits: &[(u32, Bit)]) -> (u64, u64) {
    let mut mask: u64 = 0;
    let mut value: u64 = 0;
    for &(bit_pos, bit_val) in fixed_bits {
        if bit_val == Bit::Wildcard {
            continue;
        }
        mask |= 1u64 << bit_pos;
        if bit_val == Bit::One {
            value |= 1u64 << bit_pos;
        }
    }
    (mask, value)
}

/// Convert format pieces to a Python expression (concatenated string).
/// `fields_var` is the variable name for the fields dict (e.g., "fields").
pub fn format_pieces_to_python_expr(
    pieces: &[FormatPiece],
    fields: &[ResolvedField],
    fields_var: &str,
    display: &DisplayConfig,
) -> String {
    if pieces.is_empty() {
        return "\"\"".to_string();
    }

    let all_literal = pieces.iter().all(|p| matches!(p, FormatPiece::Literal(_)));
    if all_literal {
        let mut s = String::new();
        for piece in pieces {
            if let FormatPiece::Literal(lit) = piece {
                s.push_str(lit);
            }
        }
        return format!("\"{}\"", escape_python_str(&s));
    }

    // Build an f-string-style expression
    let mut result = String::from("f\"");
    for piece in pieces {
        match piece {
            FormatPiece::Literal(lit) => {
                result.push_str(&escape_python_fstr(lit));
            }
            FormatPiece::FieldRef { expr, spec } => {
                if spec.is_some() {
                    // Explicit format spec overrides display hints
                    result.push('{');
                    result.push_str(&expr_to_python(expr, fields, fields_var, display));
                    if let Some(spec) = spec {
                        result.push(':');
                        result.push_str(spec);
                    }
                    result.push('}');
                } else if let Some(wrapper) = resolve_display_wrapper(expr, fields, display) {
                    // Apply display format (hex, signed_hex, or type prefix)
                    result.push('{');
                    result.push_str(&wrapper);
                    result.push('}');
                } else {
                    result.push('{');
                    result.push_str(&expr_to_python(expr, fields, fields_var, display));
                    result.push('}');
                }
            }
        }
    }
    result.push('"');
    result
}

/// Resolve display formatting for a format expression.
/// Returns a Python expression string with the display wrapper applied, or None.
fn resolve_display_wrapper(
    expr: &FormatExpr,
    fields: &[ResolvedField],
    display: &DisplayConfig,
) -> Option<String> {
    // Find the display format from the expression's primary field
    let (display_fmt, alias_name) = resolve_display_info(expr, fields)?;

    let raw_expr = expr_to_python(expr, fields, "fields", display);

    // Check type prefix first (e.g., gpr -> "r")
    if let Some(alias) = &alias_name {
        if let Some(prefix) = display.type_prefixes.get(alias) {
            return Some(format!(
                "\"{}\" + str({})",
                escape_python_str(prefix),
                raw_expr
            ));
        }
    }

    // Check display format hint
    match display_fmt? {
        DisplayFormat::SignedHex => Some(format!("_fmt_signed_hex({})", raw_expr)),
        DisplayFormat::Hex => Some(format!("_fmt_hex({})", raw_expr)),
    }
}

/// Extract display format and alias name from a format expression.
/// Looks at the primary field in the expression.
fn resolve_display_info(
    expr: &FormatExpr,
    fields: &[ResolvedField],
) -> Option<(Option<DisplayFormat>, Option<String>)> {
    match expr {
        FormatExpr::Field(name) => {
            let field = fields.iter().find(|f| f.name == *name)?;
            Some((
                field.resolved_type.display_format,
                field.resolved_type.alias_name.clone(),
            ))
        }
        FormatExpr::Arithmetic { left, right, .. } => {
            // Try left first, then right
            resolve_display_info(left, fields).or_else(|| resolve_display_info(right, fields))
        }
        _ => None,
    }
}

/// Convert a FormatExpr to a Python expression string.
pub fn expr_to_python(
    expr: &FormatExpr,
    fields: &[ResolvedField],
    fields_var: &str,
    display: &DisplayConfig,
) -> String {
    match expr {
        FormatExpr::Field(name) => {
            format!("{}[\"{}\"]", fields_var, name)
        }
        FormatExpr::Ternary {
            field,
            if_nonzero,
            if_zero,
        } => {
            let else_val = if_zero.as_deref().unwrap_or("");
            format!(
                "(\"{}\" if {}[\"{}\"] else \"{}\")",
                escape_python_str(if_nonzero),
                fields_var,
                field,
                escape_python_str(else_val)
            )
        }
        FormatExpr::Arithmetic { left, op, right } => {
            let l = expr_to_python(left, fields, fields_var, display);
            let r = expr_to_python(right, fields, fields_var, display);
            let op_str = arith_op_str(op);
            format!("({} {} {})", l, op_str, r)
        }
        FormatExpr::IntLiteral(val) => format!("{}", val),
        FormatExpr::MapCall { map_name, args } => {
            let arg_strs: Vec<String> = args
                .iter()
                .map(|a| expr_to_python(a, fields, fields_var, display))
                .collect();
            format!("{}({})", map_name, arg_strs.join(", "))
        }
        FormatExpr::BuiltinCall { func, args } => {
            let arg_strs: Vec<String> = args
                .iter()
                .map(|a| expr_to_python(a, fields, fields_var, display))
                .collect();
            match func {
                BuiltinFunc::RotateRight => {
                    format!(
                        "_rotate_right({}, {})",
                        arg_strs.first().map(|s| s.as_str()).unwrap_or("0"),
                        arg_strs.get(1).map(|s| s.as_str()).unwrap_or("0")
                    )
                }
                BuiltinFunc::RotateLeft => {
                    format!(
                        "_rotate_left({}, {})",
                        arg_strs.first().map(|s| s.as_str()).unwrap_or("0"),
                        arg_strs.get(1).map(|s| s.as_str()).unwrap_or("0")
                    )
                }
            }
        }
        FormatExpr::SubDecoderAccess { field, fragment } => {
            format!("{}[\"{}\"][\"{}\"]", fields_var, field, fragment)
        }
    }
}

/// Generate a Python guard condition from a Guard.
pub fn emit_guard_python(
    guard: &Guard,
    _fields: &[ResolvedField],
    fields_var: &str,
    _display: &DisplayConfig,
) -> String {
    let conditions: Vec<String> = guard
        .conditions
        .iter()
        .map(|cond| {
            let left = guard_operand_to_python(&cond.left, fields_var);
            let right = guard_operand_to_python(&cond.right, fields_var);
            let op = match cond.op {
                CompareOp::Eq => "==",
                CompareOp::Ne => "!=",
                CompareOp::Lt => "<",
                CompareOp::Le => "<=",
                CompareOp::Gt => ">",
                CompareOp::Ge => ">=",
            };
            format!("{} {} {}", left, op, right)
        })
        .collect();

    conditions.join(" and ")
}

fn guard_operand_to_python(operand: &GuardOperand, fields_var: &str) -> String {
    match operand {
        GuardOperand::Field(name) => format!("{}[\"{}\"]", fields_var, name),
        GuardOperand::Literal(val) => format!("{}", val),
        GuardOperand::Expr { left, op, right } => {
            let l = guard_operand_to_python(left, fields_var);
            let r = guard_operand_to_python(right, fields_var);
            let op_str = arith_op_str(op);
            format!("({} {} {})", l, op_str, r)
        }
    }
}

/// Generate a Python sub-decoder dispatch function.
pub fn emit_subdecoder_python(out: &mut String, sd: &ValidatedSubDecoder) {
    let fn_name = format!("_decode_{}", to_snake_case(&sd.name));
    let width = sd.width;
    let _unit_bytes = width / 8;

    // Generate map functions for sub-decoder-local maps
    emit_map_functions_python(out, &sd.maps);

    writeln!(out, "def {}(val):", fn_name).unwrap();

    // Build a simple dispatch table using if/elif on masked values
    for (i, instr) in sd.instructions.iter().enumerate() {
        let (mask, value) = compute_instruction_mask_value(instr);

        let keyword = if i == 0 { "if" } else { "elif" };
        writeln!(out, "    {} val & {:#x} == {:#x}:", keyword, mask, value).unwrap();

        // Extract fields
        for field in &instr.resolved_fields {
            let extract = extract_field_from_val(&field.ranges, width);
            let expr = apply_transforms_python(&extract, &field.resolved_type);
            writeln!(out, "        {} = {}", field.name, expr).unwrap();
        }

        // Build fragment dict
        let mut frag_entries = Vec::new();
        for frag in &instr.fragments {
            let frag_expr =
                format_pieces_to_python_subdecoder_str(&frag.pieces, &instr.resolved_fields);
            frag_entries.push(format!("\"{}\": {}", frag.name, frag_expr));
        }
        writeln!(out, "        return {{{}}}", frag_entries.join(", ")).unwrap();
    }

    writeln!(out, "    return None").unwrap();
    writeln!(out).unwrap();
    writeln!(out).unwrap();
}

/// Compute mask/value for a sub-decoder instruction.
fn compute_instruction_mask_value(instr: &ValidatedSubInstruction) -> (u64, u64) {
    let mut mask: u64 = 0;
    let mut value: u64 = 0;

    for seg in &instr.segments {
        if let Segment::Fixed {
            ranges, pattern, ..
        } = seg
        {
            let mut bit_idx = 0;
            for range in ranges {
                for i in 0..range.width() {
                    if bit_idx < pattern.len() {
                        let bit = pattern[bit_idx];
                        if bit != Bit::Wildcard {
                            let hw_bit = range.start - i;
                            mask |= 1u64 << hw_bit;
                            if bit == Bit::One {
                                value |= 1u64 << hw_bit;
                            }
                        }
                        bit_idx += 1;
                    }
                }
            }
        }
    }

    (mask, value)
}

/// Extract a field value from `val` for sub-decoder (single-unit, no data[] reads).
fn extract_field_from_val(ranges: &[BitRange], _width: u32) -> String {
    if ranges.is_empty() {
        return "0".to_string();
    }

    if ranges.len() == 1 {
        let range = ranges[0];
        let width = range.width();
        let shift = range.end;
        let mask = (1u64 << width) - 1;

        if shift == 0 {
            format!("(val & {:#x})", mask)
        } else {
            format!("((val >> {}) & {:#x})", shift, mask)
        }
    } else {
        let mut parts = Vec::new();
        let mut accumulated_width = 0u32;

        for range in ranges {
            let width = range.width();
            let shift = range.end;
            let mask = (1u64 << width) - 1;

            let extracted = if shift == 0 {
                format!("(val & {:#x})", mask)
            } else {
                format!("((val >> {}) & {:#x})", shift, mask)
            };

            if accumulated_width > 0 {
                parts.push(format!("({} << {})", extracted, accumulated_width));
            } else {
                parts.push(extracted);
            }

            accumulated_width += width;
        }

        parts.join(" | ")
    }
}

/// Convert format pieces for a sub-decoder fragment to a Python string expression.
/// Fields are referenced directly by name (not via a dict).
fn format_pieces_to_python_subdecoder_str(
    pieces: &[FormatPiece],
    fields: &[ResolvedField],
) -> String {
    if pieces.is_empty() {
        return "\"\"".to_string();
    }

    let all_literal = pieces.iter().all(|p| matches!(p, FormatPiece::Literal(_)));
    if all_literal {
        let mut s = String::new();
        for piece in pieces {
            if let FormatPiece::Literal(lit) = piece {
                s.push_str(lit);
            }
        }
        return format!("\"{}\"", escape_python_str(&s));
    }

    let mut result = String::from("f\"");
    for piece in pieces {
        match piece {
            FormatPiece::Literal(lit) => {
                result.push_str(&escape_python_fstr(lit));
            }
            FormatPiece::FieldRef { expr, spec } => {
                result.push('{');
                result.push_str(&expr_to_python_direct(expr, fields));
                if let Some(spec) = spec {
                    result.push(':');
                    result.push_str(spec);
                }
                result.push('}');
            }
        }
    }
    result.push('"');
    result
}

/// Convert a FormatExpr to a Python expression where fields are local variables.
fn expr_to_python_direct(expr: &FormatExpr, _fields: &[ResolvedField]) -> String {
    match expr {
        FormatExpr::Field(name) => name.clone(),
        FormatExpr::Ternary {
            field,
            if_nonzero,
            if_zero,
        } => {
            let else_val = if_zero.as_deref().unwrap_or("");
            format!(
                "(\"{}\" if {} else \"{}\")",
                escape_python_str(if_nonzero),
                field,
                escape_python_str(else_val)
            )
        }
        FormatExpr::Arithmetic { left, op, right } => {
            let l = expr_to_python_direct(left, _fields);
            let r = expr_to_python_direct(right, _fields);
            let op_str = arith_op_str(op);
            format!("({} {} {})", l, op_str, r)
        }
        FormatExpr::IntLiteral(val) => format!("{}", val),
        FormatExpr::MapCall { map_name, args } => {
            let arg_strs: Vec<String> = args
                .iter()
                .map(|a| expr_to_python_direct(a, _fields))
                .collect();
            format!("{}({})", map_name, arg_strs.join(", "))
        }
        FormatExpr::BuiltinCall { func, args } => {
            let arg_strs: Vec<String> = args
                .iter()
                .map(|a| expr_to_python_direct(a, _fields))
                .collect();
            match func {
                BuiltinFunc::RotateRight => {
                    format!(
                        "_rotate_right({}, {})",
                        arg_strs.first().map(|s| s.as_str()).unwrap_or("0"),
                        arg_strs.get(1).map(|s| s.as_str()).unwrap_or("0")
                    )
                }
                BuiltinFunc::RotateLeft => {
                    format!(
                        "_rotate_left({}, {})",
                        arg_strs.first().map(|s| s.as_str()).unwrap_or("0"),
                        arg_strs.get(1).map(|s| s.as_str()).unwrap_or("0")
                    )
                }
            }
        }
        FormatExpr::SubDecoderAccess { field, fragment } => {
            format!("{}[\"{}\"]", field, fragment)
        }
    }
}

/// Convert a name to snake_case.
pub fn to_snake_case(name: &str) -> String {
    let mut result = String::new();
    for (i, ch) in name.chars().enumerate() {
        if ch.is_ascii_uppercase() && i > 0 {
            result.push('_');
        }
        result.push(ch.to_ascii_lowercase());
    }
    result
}

/// Check if any instruction uses rotate builtins.
pub fn needs_rotate_helpers(def: &ValidatedDef) -> bool {
    for instr in &def.instructions {
        for fl in &instr.format_lines {
            for piece in &fl.pieces {
                if let FormatPiece::FieldRef { expr, .. } = piece {
                    if expr_uses_rotate(expr) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn expr_uses_rotate(expr: &FormatExpr) -> bool {
    match expr {
        FormatExpr::BuiltinCall { func, .. } => {
            matches!(func, BuiltinFunc::RotateRight | BuiltinFunc::RotateLeft)
        }
        FormatExpr::Arithmetic { left, right, .. } => {
            expr_uses_rotate(left) || expr_uses_rotate(right)
        }
        _ => false,
    }
}

/// Check if any instruction uses sign_extend transforms.
pub fn needs_sign_extend(def: &ValidatedDef) -> bool {
    for instr in &def.instructions {
        for field in &instr.resolved_fields {
            for transform in &field.resolved_type.transforms {
                if matches!(transform, Transform::SignExtend(_)) {
                    return true;
                }
            }
        }
    }
    // Also check sub-decoders
    for sd in &def.sub_decoders {
        for instr in &sd.instructions {
            for field in &instr.resolved_fields {
                for transform in &field.resolved_type.transforms {
                    if matches!(transform, Transform::SignExtend(_)) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

/// Generate the `_format_insn(itype, fields)` Python function.
/// Returns (mnemonic_str, operands_str).
pub fn emit_format_function(
    out: &mut String,
    def: &ValidatedDef,
    itype_prefix: &str,
    display: &DisplayConfig,
) {
    writeln!(out, "def _format_insn(itype, fields):").unwrap();
    writeln!(
        out,
        "    \"\"\"Format an instruction. Returns (mnemonic, operands) strings.\"\"\""
    )
    .unwrap();

    for (i, instr) in def.instructions.iter().enumerate() {
        let itype_const = format!("{}_{}", itype_prefix, instr.name.to_ascii_uppercase());
        let keyword = if i == 0 { "if" } else { "elif" };
        writeln!(out, "    {} itype == {}:", keyword, itype_const).unwrap();

        if instr.format_lines.is_empty() {
            // Fallback: mnemonic is instruction name, operands are field values
            if instr.resolved_fields.is_empty() {
                writeln!(out, "        return \"{}\", \"\"", instr.name).unwrap();
            } else {
                let field_strs: Vec<String> = instr
                    .resolved_fields
                    .iter()
                    .map(|f| format!("str(fields[\"{}\"])", f.name))
                    .collect();
                writeln!(
                    out,
                    "        return \"{}\", \", \".join([{}])",
                    instr.name,
                    field_strs.join(", ")
                )
                .unwrap();
            }
        } else {
            emit_format_lines_python(out, instr, 2, display);
        }
    }

    writeln!(out, "    return \"???\", \"\"").unwrap();
    writeln!(out).unwrap();
    writeln!(out).unwrap();
}

/// Emit format lines for a single instruction as Python code.
fn emit_format_lines_python(
    out: &mut String,
    instr: &ValidatedInstruction,
    indent: usize,
    display: &DisplayConfig,
) {
    let pad = "    ".repeat(indent);

    if instr.format_lines.len() == 1 && instr.format_lines[0].guard.is_none() {
        let fl = &instr.format_lines[0];
        let (mnemonic, operands) = split_format_pieces(&fl.pieces);
        let mnemonic_expr =
            format_pieces_to_python_expr(&mnemonic, &instr.resolved_fields, "fields", display);
        let operands_expr =
            format_pieces_to_python_expr(&operands, &instr.resolved_fields, "fields", display);
        writeln!(out, "{}return {}, {}", pad, mnemonic_expr, operands_expr).unwrap();
        return;
    }

    // Multiple format lines with guards
    for (i, fl) in instr.format_lines.iter().enumerate() {
        let (mnemonic, operands) = split_format_pieces(&fl.pieces);
        let mnemonic_expr =
            format_pieces_to_python_expr(&mnemonic, &instr.resolved_fields, "fields", display);
        let operands_expr =
            format_pieces_to_python_expr(&operands, &instr.resolved_fields, "fields", display);

        if let Some(guard) = &fl.guard {
            let guard_code = emit_guard_python(guard, &instr.resolved_fields, "fields", display);
            if i == 0 {
                writeln!(out, "{}if {}:", pad, guard_code).unwrap();
            } else {
                writeln!(out, "{}elif {}:", pad, guard_code).unwrap();
            }
            writeln!(
                out,
                "{}    return {}, {}",
                pad, mnemonic_expr, operands_expr
            )
            .unwrap();
        } else {
            if i > 0 {
                writeln!(out, "{}else:", pad).unwrap();
                writeln!(
                    out,
                    "{}    return {}, {}",
                    pad, mnemonic_expr, operands_expr
                )
                .unwrap();
            } else {
                writeln!(out, "{}return {}, {}", pad, mnemonic_expr, operands_expr).unwrap();
            }
        }
    }
}

/// Split format pieces into (mnemonic_pieces, operand_pieces).
/// The mnemonic is everything before the first space; operands are the rest.
fn split_format_pieces(pieces: &[FormatPiece]) -> (Vec<FormatPiece>, Vec<FormatPiece>) {
    let mut mnemonic = Vec::new();
    let mut operands = Vec::new();
    let mut found_space = false;

    for piece in pieces {
        if found_space {
            operands.push(piece.clone());
        } else {
            match piece {
                FormatPiece::Literal(lit) => {
                    if let Some(pos) = lit.find(' ') {
                        // Split this literal at the first space
                        let before = &lit[..pos];
                        let after = &lit[pos + 1..];
                        if !before.is_empty() {
                            mnemonic.push(FormatPiece::Literal(before.to_string()));
                        }
                        if !after.is_empty() {
                            operands.push(FormatPiece::Literal(after.to_string()));
                        }
                        found_space = true;
                    } else {
                        mnemonic.push(piece.clone());
                    }
                }
                _ => {
                    // Field ref before any space - part of mnemonic (unusual but possible)
                    mnemonic.push(piece.clone());
                }
            }
        }
    }

    (mnemonic, operands)
}
