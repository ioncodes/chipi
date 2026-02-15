//! Core type definitions for the intermediate representation.
//!
//! Defines the AST and validated IR types used throughout the pipeline.

use crate::error::Span;

/// A complete decoder definition from a `.chipi` file.
#[derive(Debug, Clone)]
pub struct DecoderDef {
    pub imports: Vec<Import>,
    pub config: DecoderConfig,
    pub type_aliases: Vec<TypeAlias>,
    pub maps: Vec<MapDef>,
    pub instructions: Vec<InstructionDef>,
}

/// An import statement for custom types or modules.
#[derive(Debug, Clone)]
pub struct Import {
    pub path: String,
    pub span: Span,
}

/// Decoder configuration from the decoder block.
#[derive(Debug, Clone)]
pub struct DecoderConfig {
    pub name: String,
    pub width: u32,
    pub bit_order: BitOrder,
    /// Optional maximum number of units an instruction can span.
    /// When specified, acts as a compile-time safety guard against typos.
    /// When None, no limit is enforced.
    pub max_units: Option<u32>,
    pub span: Span,
}

/// Bit order convention for DSL input.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitOrder {
    /// MSB0: bit 0 is the most significant bit
    Msb0,
    /// LSB0: bit 0 is the least significant bit
    Lsb0,
}

/// A custom type alias for field types.
#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: String,
    pub base_type: String,
    pub wrapper_type: Option<String>,
    pub transforms: Vec<Transform>,
    pub display_format: Option<DisplayFormat>,
    pub span: Span,
}

/// Transformations applied to extracted field values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Transform {
    /// Sign-extend the value (typically used for signed integers)
    SignExtend(u32),
    /// Zero-extend the value (typically used for unsigned integers)
    ZeroExtend(u32),
    /// Left-shift the value by N bits
    ShiftLeft(u32),
}

/// Display format hint for field values in format strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisplayFormat {
    /// Signed hexadecimal: 0x1A, -0x1A, 0
    SignedHex,
    /// Unsigned hexadecimal: 0x1A, 0
    Hex,
}

/// A map definition (lookup table for format strings).
#[derive(Debug, Clone)]
pub struct MapDef {
    pub name: String,
    pub params: Vec<String>,
    pub entries: Vec<MapEntry>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MapEntry {
    pub keys: Vec<MapKey>,
    pub output: Vec<FormatPiece>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MapKey {
    Value(i64),
    Wildcard,
}

/// A format line attached to an instruction.
#[derive(Debug, Clone)]
pub struct FormatLine {
    pub guard: Option<Guard>,
    pub pieces: Vec<FormatPiece>,
    pub span: Span,
}

/// A guard condition (between `|` and `:`).
#[derive(Debug, Clone)]
pub struct Guard {
    pub conditions: Vec<GuardCondition>,
}

#[derive(Debug, Clone)]
pub struct GuardCondition {
    pub left: GuardOperand,
    pub op: CompareOp,
    pub right: GuardOperand,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GuardOperand {
    Field(String),
    Literal(i64),
    Expr {
        left: Box<GuardOperand>,
        op: ArithOp,
        right: Box<GuardOperand>,
    },
}

/// A piece of a parsed format string.
#[derive(Debug, Clone)]
pub enum FormatPiece {
    Literal(String),
    FieldRef {
        expr: FormatExpr,
        spec: Option<String>,
    },
}

/// Expression inside `{...}` in a format string.
#[derive(Debug, Clone)]
pub enum FormatExpr {
    Field(String),
    Ternary {
        field: String,
        if_nonzero: String,
        if_zero: Option<String>,
    },
    Arithmetic {
        left: Box<FormatExpr>,
        op: ArithOp,
        right: Box<FormatExpr>,
    },
    IntLiteral(i64),
    MapCall {
        map_name: String,
        args: Vec<FormatExpr>,
    },
    BuiltinCall {
        func: BuiltinFunc,
        args: Vec<FormatExpr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinFunc {
    RotateRight,
    RotateLeft,
}

/// Raw instruction definition from parsing.
#[derive(Debug, Clone)]
pub struct InstructionDef {
    pub name: String,
    pub segments: Vec<Segment>,
    pub format_lines: Vec<FormatLine>,
    pub span: Span,
}

/// Part of an instruction definition: either fixed bits or a field.
///
/// For cross-unit fields/patterns, `ranges` contains multiple BitRange objects,
/// one for each unit spanned. Ranges are ordered by unit index.
#[derive(Debug, Clone)]
pub enum Segment {
    /// A fixed bit pattern that must match for this instruction
    Fixed {
        ranges: Vec<BitRange>,
        pattern: Vec<Bit>,
        span: Span,
    },
    /// A variable field to be extracted
    Field {
        name: String,
        field_type: FieldType,
        ranges: Vec<BitRange>,
        span: Span,
    },
}

/// A contiguous range of hardware bits (LSB=0, start >= end).
///
/// For variable-length instructions, the `unit` field identifies which fetch unit
/// this range belongs to (0 = first unit, 1 = second unit, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BitRange {
    /// Which unit (fetch segment) this range belongs to (0-indexed)
    pub unit: u32,
    /// Most significant bit of the range (within this unit)
    pub start: u32,
    /// Least significant bit of the range (within this unit)
    pub end: u32,
}

impl BitRange {
    /// Create a new bit range in unit 0 (for backward compatibility).
    /// Note: During parsing, DSL notation is used (may have start < end).
    /// After validation, hardware notation is used (start >= end).
    pub fn new(start: u32, end: u32) -> Self {
        BitRange { unit: 0, start, end }
    }

    /// Create a new bit range in a specific unit.
    /// Asserts that start >= end (hardware notation).
    pub fn new_in_unit(unit: u32, start: u32, end: u32) -> Self {
        debug_assert!(start >= end, "BitRange: start ({}) must be >= end ({})", start, end);
        BitRange { unit, start, end }
    }

    /// Width of the range in bits (inclusive).
    pub fn width(&self) -> u32 {
        self.start - self.end + 1
    }

    /// Check if a hardware bit position is within this range.
    pub fn contains_bit(&self, bit: u32) -> bool {
        bit >= self.end && bit <= self.start
    }

    /// Iterate over hardware bit positions from MSB to LSB.
    pub fn bits(&self) -> impl Iterator<Item = u32> {
        let start = self.start;
        let end = self.end;
        (end..=start).rev()
    }
}

/// An individual bit: 0 or 1.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bit {
    Zero,
    One,
}

/// Type specification for a field (either an alias or inline with transforms).
#[derive(Debug, Clone)]
pub enum FieldType {
    /// Reference to a named type alias
    Alias(String),
    /// Inline base type with optional transforms
    Inline {
        base_type: String,
        transforms: Vec<Transform>,
    },
}

/// Resolved type information after validation.
#[derive(Debug, Clone)]
pub struct ResolvedFieldType {
    pub base_type: String,
    pub wrapper_type: Option<String>,
    pub transforms: Vec<Transform>,
    pub display_format: Option<DisplayFormat>,
}

/// A fully validated decoder definition, ready for tree building and code generation.
#[derive(Debug, Clone)]
pub struct ValidatedDef {
    pub imports: Vec<Import>,
    pub config: DecoderConfig,
    pub type_aliases: Vec<TypeAlias>,
    pub maps: Vec<MapDef>,
    pub instructions: Vec<ValidatedInstruction>,
}

/// An instruction after validation and field resolution.
#[derive(Debug, Clone)]
pub struct ValidatedInstruction {
    pub name: String,
    pub segments: Vec<Segment>,
    pub resolved_fields: Vec<ResolvedField>,
    pub format_lines: Vec<FormatLine>,
    pub span: Span,
}

/// A field with resolved type information.
#[derive(Debug, Clone)]
pub struct ResolvedField {
    pub name: String,
    pub ranges: Vec<BitRange>,
    pub resolved_type: ResolvedFieldType,
}

impl ValidatedInstruction {
    /// Get the number of units (fetch segments) this instruction spans.
    /// Returns the maximum unit index + 1 across all segments.
    pub fn unit_count(&self) -> u32 {
        self.segments
            .iter()
            .flat_map(|seg| match seg {
                Segment::Fixed { ranges, .. } | Segment::Field { ranges, .. } => ranges.iter(),
            })
            .map(|range| range.unit)
            .max()
            .unwrap_or(0)
            + 1
    }

    /// Get all fixed bit positions and their values as a flat list.
    /// Returns (unit, hw_bit, bit_value) tuples for ALL units (not just unit 0).
    /// This allows checking fixed bits in any unit during decoding.
    pub fn fixed_bits(&self) -> Vec<(u32, u32, Bit)> {
        let mut result = Vec::new();
        for seg in &self.segments {
            if let Segment::Fixed { ranges, pattern, .. } = seg {
                let mut bit_idx = 0;
                for range in ranges {
                    let range_width = range.width() as usize;
                    for i in 0..range_width {
                        if bit_idx < pattern.len() {
                            let hw_bit = range.start - i as u32;
                            result.push((range.unit, hw_bit, pattern[bit_idx]));
                            bit_idx += 1;
                        }
                    }
                }
            }
        }
        result
    }

    /// Get the fixed bit value at a specific hardware bit position in unit 0, if any.
    /// Only considers bits in unit 0 (for backward compatibility with decision tree).
    pub fn fixed_bit_at(&self, hw_bit: u32) -> Option<Bit> {
        for seg in &self.segments {
            if let Segment::Fixed { ranges, pattern, .. } = seg {
                let mut bit_idx = 0;
                for range in ranges {
                    // Only consider unit 0 bits for decision tree purposes
                    if range.unit != 0 {
                        bit_idx += range.width() as usize;
                        continue;
                    }
                    if range.contains_bit(hw_bit) {
                        let offset = (range.start - hw_bit) as usize;
                        let idx = bit_idx + offset;
                        if idx < pattern.len() {
                            return Some(pattern[idx]);
                        }
                    }
                    bit_idx += range.width() as usize;
                }
            }
        }
        None
    }
}
