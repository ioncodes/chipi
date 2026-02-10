//! Core type definitions for the intermediate representation.
//!
//! Defines the AST and validated IR types used throughout the pipeline.

use crate::error::Span;

/// A complete decoder definition from a `.def` file.
#[derive(Debug, Clone)]
pub struct DecoderDef {
    pub imports: Vec<Import>,
    pub config: DecoderConfig,
    pub type_aliases: Vec<TypeAlias>,
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

/// Raw instruction definition from parsing.
#[derive(Debug, Clone)]
pub struct InstructionDef {
    pub name: String,
    pub segments: Vec<Segment>,
    pub span: Span,
}

/// Part of an instruction definition: either fixed bits or a field.
#[derive(Debug, Clone)]
pub enum Segment {
    /// A fixed bit pattern that must match for this instruction
    Fixed {
        range: BitRange,
        pattern: Vec<Bit>,
        span: Span,
    },
    /// A variable field to be extracted
    Field {
        name: String,
        field_type: FieldType,
        range: BitRange,
        span: Span,
    },
}

/// A contiguous range of hardware bits (LSB=0, start >= end).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BitRange {
    /// Most significant bit of the range
    pub start: u32,
    /// Least significant bit of the range
    pub end: u32,
}

impl BitRange {
    /// Create a new bit range. Asserts that start >= end.
    pub fn new(start: u32, end: u32) -> Self {
        debug_assert!(start >= end, "BitRange: start ({}) must be >= end ({})", start, end);
        BitRange { start, end }
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
}

/// A fully validated decoder definition, ready for tree building and code generation.
#[derive(Debug, Clone)]
pub struct ValidatedDef {
    pub imports: Vec<Import>,
    pub config: DecoderConfig,
    pub type_aliases: Vec<TypeAlias>,
    pub instructions: Vec<ValidatedInstruction>,
}

/// An instruction after validation and field resolution.
#[derive(Debug, Clone)]
pub struct ValidatedInstruction {
    pub name: String,
    pub segments: Vec<Segment>,
    pub resolved_fields: Vec<ResolvedField>,
    pub span: Span,
}

/// A field with resolved type information.
#[derive(Debug, Clone)]
pub struct ResolvedField {
    pub name: String,
    pub range: BitRange,
    pub resolved_type: ResolvedFieldType,
}

impl ValidatedInstruction {
    /// Get all fixed bit positions and their values as a flat list.
    pub fn fixed_bits(&self) -> Vec<(u32, Bit)> {
        let mut result = Vec::new();
        for seg in &self.segments {
            if let Segment::Fixed { range, pattern, .. } = seg {
                for (i, bit) in pattern.iter().enumerate() {
                    let hw_bit = range.start - i as u32;
                    result.push((hw_bit, *bit));
                }
            }
        }
        result
    }

    /// Get the fixed bit value at a specific hardware bit position, if any.
    pub fn fixed_bit_at(&self, hw_bit: u32) -> Option<Bit> {
        for seg in &self.segments {
            if let Segment::Fixed { range, pattern, .. } = seg {
                if range.contains_bit(hw_bit) {
                    let idx = (range.start - hw_bit) as usize;
                    return Some(pattern[idx]);
                }
            }
        }
        None
    }
}
