//! Instruction type generation - produces a newtype with field accessor methods.
//!
//! Collects all unique fields across all instructions in a `.chipi` spec and
//! generates a `pub struct Name(pub u32)` with one `#[inline]` accessor per field.
//!
//! Fields with the same name but different bit ranges or types generate separate
//! accessors with bit range suffixes (e.g., `d_15_0` and `d_11_0`).

use std::collections::HashMap;
use std::fmt::Write;

use crate::codegen::{signed_type_for, type_bits};
use crate::types::*;

/// Convert a hardware bit position back to DSL notation.
/// `unit` is the fetch unit index (0 for single-unit instructions).
fn hw_to_dsl(hw_bit: u32, unit: u32, width: u32, bit_order: BitOrder) -> u32 {
    let unit_offset = unit * width;
    match bit_order {
        BitOrder::Msb0 => unit_offset + width - 1 - hw_bit,
        BitOrder::Lsb0 => unit_offset + hw_bit,
    }
}

/// A field ready for accessor generation.
#[derive(Debug, Clone)]
struct FieldAccessor {
    /// Function name (may include bit range suffix for conflicts)
    fn_name: String,
    /// Bit ranges
    ranges: Vec<BitRange>,
    /// Base return type
    base_type: String,
    /// Transforms to apply
    transforms: Vec<Transform>,
    /// Original chipi type definition for documentation
    chipi_type: String,
}

/// Collect fields for accessor generation, handling conflicts by adding bit range suffixes.
///
/// Returns (fields, warnings). Fields with the same name but conflicting definitions
/// get separate accessors with bit ranges in their names (e.g., `d_16_31`, `d_20_31`).
fn collect_fields(def: &ValidatedDef) -> (Vec<FieldAccessor>, Vec<String>) {
    let width = def.config.width;
    let bit_order = def.config.bit_order;

    // Group fields by name
    let mut by_name: HashMap<String, Vec<&ResolvedField>> = HashMap::new();
    for instr in &def.instructions {
        for field in &instr.resolved_fields {
            by_name.entry(field.name.clone()).or_default().push(field);
        }
    }

    let mut accessors = Vec::new();
    let mut warnings = Vec::new();

    for (name, fields) in by_name {
        // Deduplicate: keep only unique (ranges, type, transforms) combinations
        let mut unique = Vec::new();
        for field in fields {
            if !unique.iter().any(|f: &&ResolvedField| {
                f.ranges == field.ranges
                    && f.resolved_type.base_type == field.resolved_type.base_type
                    && f.resolved_type.transforms == field.resolved_type.transforms
            }) {
                unique.push(field);
            }
        }

        if unique.len() == 1 {
            // No conflict - use plain field name
            let field = unique[0];
            accessors.push(FieldAccessor {
                fn_name: name.clone(),
                ranges: field.ranges.clone(),
                base_type: field.resolved_type.base_type.clone(),
                transforms: field.resolved_type.transforms.clone(),
                chipi_type: format_chipi_type(field, width, bit_order),
            });
        } else {
            // Conflict - generate accessors with bit range suffixes
            warnings.push(format!(
                "field '{}' has {} conflicting definitions - generating {} variants with bit ranges in names",
                name,
                unique.len(),
                unique.len()
            ));

            for field in unique {
                let range_suffix = format_range_suffix(&field.ranges, width, bit_order);
                let fn_name = format!("{}_{}", name, range_suffix);
                accessors.push(FieldAccessor {
                    fn_name,
                    ranges: field.ranges.clone(),
                    base_type: field.resolved_type.base_type.clone(),
                    transforms: field.resolved_type.transforms.clone(),
                    chipi_type: format_chipi_type(field, width, bit_order),
                });
            }
        }
    }

    // Resolve remaining duplicates: same name+range but different types get type suffix
    let mut name_counts: HashMap<String, usize> = HashMap::new();
    for acc in &accessors {
        *name_counts.entry(acc.fn_name.clone()).or_default() += 1;
    }
    for acc in &mut accessors {
        if name_counts[&acc.fn_name] > 1 {
            acc.fn_name = format!("{}_{}", acc.fn_name, acc.base_type);
        }
    }

    // Sort by function name for stable output
    accessors.sort_by(|a, b| a.fn_name.cmp(&b.fn_name));
    (accessors, warnings)
}

/// Format a bit range as a suffix using DSL bit positions (e.g., "16_31" for MSB0 bits [16:31]).
fn format_range_suffix(ranges: &[BitRange], width: u32, bit_order: BitOrder) -> String {
    if ranges.len() == 1 {
        let r = &ranges[0];
        let dsl_start = hw_to_dsl(r.start, r.unit, width, bit_order);
        let dsl_end = hw_to_dsl(r.end, r.unit, width, bit_order);
        format!("{}_{}", dsl_start, dsl_end)
    } else {
        // Multi-range: concatenate all ranges
        ranges
            .iter()
            .map(|r| {
                let dsl_start = hw_to_dsl(r.start, r.unit, width, bit_order);
                let dsl_end = hw_to_dsl(r.end, r.unit, width, bit_order);
                format!("{}_{}", dsl_start, dsl_end)
            })
            .collect::<Vec<_>>()
            .join("_")
    }
}

/// Format the chipi type definition for a field (for doc comments), using DSL bit positions.
fn format_chipi_type(field: &ResolvedField, width: u32, bit_order: BitOrder) -> String {
    let ranges_str = field
        .ranges
        .iter()
        .map(|r| {
            let dsl_start = hw_to_dsl(r.start, r.unit, width, bit_order);
            let dsl_end = hw_to_dsl(r.end, r.unit, width, bit_order);
            if dsl_start == dsl_end {
                format!("[{}]", dsl_start)
            } else {
                format!("[{}:{}]", dsl_start, dsl_end)
            }
        })
        .collect::<Vec<_>>()
        .join("");

    let type_str = &field.resolved_type.base_type;

    format!("{}: {}{}", field.name, type_str, ranges_str)
}

/// Generate the body expression for a field accessor.
///
/// Returns a Rust expression string that extracts and transforms the field value.
/// `unit_width` is the decoder's `width` (bits per unit), used to offset multi-unit fields.
fn accessor_body(field: &FieldAccessor, raw_expr: &str, unit_width: u32) -> String {
    // Build extraction expression
    let extract = if field.ranges.len() == 1 {
        // Single contiguous range
        let range = &field.ranges[0];
        let width = range.width();
        let shift = range.unit * unit_width + range.end;
        let mask = (1u64 << width) - 1;
        if shift == 0 {
            format!("{} & {:#x}", raw_expr, mask)
        } else {
            format!("({} >> {}) & {:#x}", raw_expr, shift, mask)
        }
    } else {
        // Multi-range: combine parts
        build_multi_range_extract(raw_expr, &field.ranges, unit_width)
    };

    // Apply transforms
    let mut expr = extract;
    for transform in &field.transforms {
        match transform {
            Transform::SignExtend(n) => {
                let signed = signed_type_for(&field.base_type);
                let bits = type_bits(&field.base_type);
                expr = format!(
                    "(((({}) as {}) << ({} - {})) >> ({} - {}))",
                    expr, signed, bits, n, bits, n
                );
            }
            Transform::ZeroExtend(_) => {} // no-op for unsigned
            Transform::ShiftLeft(n) => {
                expr = format!("(({}) << {})", expr, n);
            }
        }
    }

    // Final cast
    if field.base_type == "bool" {
        format!("({}) != 0", expr)
    } else {
        format!("({}) as {}", expr, field.base_type)
    }
}

/// Build a multi-range extraction expression (for fields spanning multiple bit ranges).
fn build_multi_range_extract(raw_expr: &str, ranges: &[BitRange], unit_width: u32) -> String {
    let mut parts = Vec::new();
    let mut accumulated_width = 0u32;

    // Ranges are ordered from low-order to high-order bits
    for range in ranges {
        let width = range.width();
        let shift = range.unit * unit_width + range.end;
        let mask = (1u64 << width) - 1;

        let extracted = if shift == 0 {
            format!("({} & {:#x})", raw_expr, mask)
        } else {
            format!("(({} >> {}) & {:#x})", raw_expr, shift, mask)
        };

        // Shift this part into its position in the final value
        if accumulated_width > 0 {
            parts.push(format!("({} << {})", extracted, accumulated_width));
        } else {
            parts.push(extracted);
        }

        accumulated_width += width;
    }

    parts.join(" | ")
}

/// Generate the instruction type source code.
///
/// `struct_name` - the name of the generated struct (e.g., "Instruction")
///
/// Returns (generated_code, warnings). Warnings describe conflict resolution.
pub fn generate_instr_type(def: &ValidatedDef, struct_name: &str) -> (String, Vec<String>) {
    let (fields, warnings) = collect_fields(def);

    let mut out = String::new();
    writeln!(
        out,
        "// Auto-generated by https://github.com/ioncodes/chipi"
    )
    .unwrap();
    writeln!(out, "// Do not edit.").unwrap();
    writeln!(out).unwrap();

    // Emit warnings as comments
    if !warnings.is_empty() {
        writeln!(out, "// NOTES:").unwrap();
        for warning in &warnings {
            writeln!(out, "//   {}", warning).unwrap();
        }
        writeln!(out).unwrap();
    }

    writeln!(out, "#[derive(Clone, Copy)]").unwrap();
    writeln!(out, "pub struct {}(pub u32);", struct_name).unwrap();
    writeln!(out).unwrap();
    writeln!(out, "#[rustfmt::skip]").unwrap();
    writeln!(out, "impl {} {{", struct_name).unwrap();

    let unit_width = def.config.width;
    for field in &fields {
        let body = accessor_body(field, "self.0", unit_width);
        writeln!(out, "    /// Field: `{}`", field.chipi_type).unwrap();
        writeln!(
            out,
            "    #[inline] pub fn {}(&self) -> {} {{ {} }}",
            field.fn_name, field.base_type, body
        )
        .unwrap();
    }

    // Generate from_bytes constructor
    let actual_max_units = def
        .instructions
        .iter()
        .map(|i| i.unit_count())
        .max()
        .unwrap_or(1);
    let bytes_per_unit = unit_width / 8;
    let total_bytes = actual_max_units * bytes_per_unit;
    let is_big = def.config.endian == ByteEndian::Big;
    let endian_suffix = if is_big { "be" } else { "le" };
    let fn_name = format!("from_{endian_suffix}_bytes");
    let endian_label = if is_big { "big" } else { "little" };

    writeln!(out).unwrap();
    writeln!(
        out,
        "    /// Construct from {endian_label}-endian instruction bytes (reads up to {total_bytes} bytes).",
    )
    .unwrap();
    writeln!(out, "    #[inline]").unwrap();
    writeln!(out, "    pub fn {fn_name}(bytes: &[u8]) -> Self {{").unwrap();

    if actual_max_units == 1 {
        // Single unit: read width/8 bytes as one integer
        match bytes_per_unit {
            1 => writeln!(out, "        Self(bytes[0] as u32)").unwrap(),
            2 => writeln!(
                out,
                "        Self(u16::{fn_name}([bytes[0], bytes[1]]) as u32)"
            )
            .unwrap(),
            _ => writeln!(
                out,
                "        Self(u32::{fn_name}([bytes[0], bytes[1], bytes[2], bytes[3]]))"
            )
            .unwrap(),
        }
    } else {
        // Multi-unit: pack each unit (unit 0 in low bits, unit 1 shifted up, etc.)
        for u in 0..actual_max_units {
            let off = u * bytes_per_unit;
            let var = format!("w{u}");
            match bytes_per_unit {
                1 => writeln!(out, "        let {var} = bytes[{off}] as u32;").unwrap(),
                2 => writeln!(
                    out,
                    "        let {var} = u16::{fn_name}([bytes[{}], bytes[{}]]) as u32;",
                    off,
                    off + 1
                )
                .unwrap(),
                _ => writeln!(
                    out,
                    "        let {var} = u32::{fn_name}([bytes[{}], bytes[{}], bytes[{}], bytes[{}]]);",
                    off, off + 1, off + 2, off + 3
                )
                .unwrap(),
            }
        }
        let pack: Vec<String> = (0..actual_max_units)
            .map(|u| {
                if u == 0 {
                    "w0".to_string()
                } else {
                    format!("(w{u} << {})", u * unit_width)
                }
            })
            .collect();
        writeln!(out, "        Self({})", pack.join(" | ")).unwrap();
    }

    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();

    (out, warnings)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::validate;

    fn validated(source: &str) -> ValidatedDef {
        let def = parser::parse(source, "test.chipi").unwrap();
        validate::validate(&def).unwrap()
    }

    #[test]
    fn test_basic_field_generation() {
        let def = validated(
            r#"
            decoder Test {
            width = 32
            bit_order = msb0
        }
            addi [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:i32[16:31]
        "#,
        );
        let (code, _warnings) = generate_instr_type(&def, "Instruction");
        assert!(code.contains("pub fn ra("));
        assert!(code.contains("pub fn rd("));
        assert!(code.contains("pub fn simm("));
        assert!(code.contains("pub struct Instruction(pub u32);"));
        // Check for doc comments
        assert!(code.contains("/// Field:"));
    }

    #[test]
    fn test_deduplication() {
        let def = validated(
            r#"
            decoder Test {
            width = 32
            bit_order = msb0
        }
            addi  [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:i32[16:31]
            addis [0:5]=001111 rd:u8[6:10] ra:u8[11:15] simm:i32[16:31]
        "#,
        );
        let (code, _warnings) = generate_instr_type(&def, "Instruction");
        // rd should appear exactly once (deduplicated)
        assert_eq!(code.matches("pub fn rd(").count(), 1);
        assert_eq!(code.matches("pub fn ra(").count(), 1);
        assert_eq!(code.matches("pub fn simm(").count(), 1);
    }

    #[test]
    fn test_bool_field() {
        let def = validated(
            r#"
            decoder Test {
            width = 32
            bit_order = msb0
        }
            bx [0:5]=010010 li:i32[6:29] aa:bool[30] lk:bool[31]
        "#,
        );
        let (code, _warnings) = generate_instr_type(&def, "Instruction");
        assert!(code.contains("-> bool"));
        assert!(code.contains("!= 0"));
    }

    #[test]
    fn test_conflicting_fields_generate_variants() {
        let def = validated(
            r#"
            decoder Test {
            width = 32
            bit_order = msb0
        }
            foo [0:5]=000001 rd:u8[6:10] [11:31]=?????????????????????
            bar [0:5]=000010 [6:10]=????? rd:u8[11:15] [16:31]=????????????????
        "#,
        );
        let (code, warnings) = generate_instr_type(&def, "Instruction");
        assert!(!warnings.is_empty());
        assert!(warnings[0].contains("rd"));
        assert!(warnings[0].contains("conflicting"));
        // Should generate both variants with bit range suffixes (DSL positions)
        assert!(code.contains("rd_6_10"));
        assert!(code.contains("rd_11_15"));
    }

    #[test]
    fn test_sign_extend_transform() {
        let def = validated(
            r#"
            decoder Test {
            width = 32
            bit_order = msb0
        }
            type simm16 = i32 { sign_extend(16) }
            addi [0:5]=001110 rd:u8[6:10] [11:15]=????? simm:simm16[16:31]
        "#,
        );
        let (code, _warnings) = generate_instr_type(&def, "Instruction");
        assert!(code.contains("pub fn simm(&self) -> i32"));
        // Should contain sign extension logic
        assert!(code.contains("<<") && code.contains(">>"));
    }

    #[test]
    fn test_shift_left_transform() {
        let def = validated(
            r#"
            decoder Test {
            width = 32
            bit_order = msb0
        }
            type addr = i32 { shift_left(2) }
            bx [0:5]=010010 li:addr[6:29] [30:31]=??
        "#,
        );
        let (code, _warnings) = generate_instr_type(&def, "Instruction");
        assert!(code.contains("pub fn li(&self) -> i32"));
        assert!(code.contains("<< 2"));
    }
}
