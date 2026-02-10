//! Semantic validation for instruction definitions.
//!
//! Validates a parsed definition for:
//! - Name uniqueness
//! - Type resolution
//! - Bit coverage completeness
//! - Pattern conflicts between instructions

use std::collections::{HashMap, HashSet};

use crate::error::{Error, ErrorKind, Span};
use crate::parser::dsl_to_hardware;
use crate::types::*;

/// Validate a decoder definition and convert to a validated IR.
///
/// This performs multiple checks and transformations:
/// - Converts bit ranges from DSL notation to hardware notation
/// - Checks for duplicate names
/// - Resolves field types
/// - Validates bit coverage and patterns
pub fn validate(def: &DecoderDef) -> Result<ValidatedDef, Vec<Error>> {
    let mut errors = Vec::new();

    // Phase 0: Convert all bit ranges from DSL notation to hardware notation
    let instructions = convert_bit_ranges(def);

    // Phase 1: Name uniqueness
    check_name_uniqueness(&instructions, &def.type_aliases, &mut errors);

    // Phase 2: Type resolution
    resolve_types(&instructions, &def.type_aliases, &mut errors);

    // Phase 3: Bit coverage
    check_bit_coverage(&instructions, def.config.width, &mut errors);

    // Phase 4: Pattern conflicts
    check_pattern_conflicts(&instructions, &mut errors);

    if !errors.is_empty() {
        return Err(errors);
    }

    // Build validated instructions
    let validated_instructions = instructions
        .into_iter()
        .map(|instr| {
            let resolved_fields = instr
                .segments
                .iter()
                .filter_map(|seg| {
                    if let Segment::Field {
                        name,
                        field_type,
                        range,
                        ..
                    } = seg
                    {
                        let resolved = resolve_field_type(field_type, &def.type_aliases);
                        Some(ResolvedField {
                            name: name.clone(),
                            range: *range,
                            resolved_type: resolved,
                        })
                    } else {
                        None
                    }
                })
                .collect();

            ValidatedInstruction {
                name: instr.name.clone(),
                segments: instr.segments.clone(),
                resolved_fields,
                span: instr.span.clone(),
            }
        })
        .collect();

    Ok(ValidatedDef {
        imports: def.imports.clone(),
        config: def.config.clone(),
        type_aliases: def.type_aliases.clone(),
        instructions: validated_instructions,
    })
}

/// Convert all DSL bit ranges to hardware (LSB=0) notation.
fn convert_bit_ranges(def: &DecoderDef) -> Vec<InstructionDef> {
    let width = def.config.width;
    let order = def.config.bit_order;

    def.instructions
        .iter()
        .map(|instr| {
            let segments = instr
                .segments
                .iter()
                .map(|seg| match seg {
                    Segment::Fixed {
                        range,
                        pattern,
                        span,
                    } => {
                        let hw_range = dsl_to_hardware(range.start, range.end, width, order);
                        Segment::Fixed {
                            range: hw_range,
                            pattern: pattern.clone(),
                            span: span.clone(),
                        }
                    }
                    Segment::Field {
                        name,
                        field_type,
                        range,
                        span,
                    } => {
                        let hw_range = dsl_to_hardware(range.start, range.end, width, order);
                        Segment::Field {
                            name: name.clone(),
                            field_type: field_type.clone(),
                            range: hw_range,
                            span: span.clone(),
                        }
                    }
                })
                .collect();

            InstructionDef {
                name: instr.name.clone(),
                segments,
                span: instr.span.clone(),
            }
        })
        .collect()
}

fn check_name_uniqueness(
    instructions: &[InstructionDef],
    type_aliases: &[TypeAlias],
    errors: &mut Vec<Error>,
) {
    let mut seen_instructions: HashMap<&str, &Span> = HashMap::new();
    for instr in instructions {
        if let Some(prev_span) = seen_instructions.get(instr.name.as_str()) {
            errors.push(
                Error::new(
                    ErrorKind::DuplicateInstructionName(instr.name.clone()),
                    instr.span.clone(),
                )
                .with_help(format!("first defined at line {}", prev_span.line)),
            );
        } else {
            seen_instructions.insert(&instr.name, &instr.span);
        }
    }

    let mut seen_types: HashMap<&str, &Span> = HashMap::new();
    for ta in type_aliases {
        if let Some(prev_span) = seen_types.get(ta.name.as_str()) {
            errors.push(
                Error::new(
                    ErrorKind::DuplicateTypeAlias(ta.name.clone()),
                    ta.span.clone(),
                )
                .with_help(format!("first defined at line {}", prev_span.line)),
            );
        } else {
            seen_types.insert(&ta.name, &ta.span);
        }
    }
}

fn resolve_types(
    instructions: &[InstructionDef],
    type_aliases: &[TypeAlias],
    errors: &mut Vec<Error>,
) {
    let alias_names: HashSet<&str> = type_aliases.iter().map(|ta| ta.name.as_str()).collect();

    for instr in instructions {
        for seg in &instr.segments {
            if let Segment::Field {
                field_type, span, ..
            } = seg
            {
                if let FieldType::Alias(alias_name) = field_type {
                    if !alias_names.contains(alias_name.as_str())
                        && !is_builtin_type(alias_name)
                    {
                        errors.push(Error::new(
                            ErrorKind::UnresolvedType(alias_name.clone()),
                            span.clone(),
                        ));
                    }
                }
            }
        }
    }
}

fn check_bit_coverage(
    instructions: &[InstructionDef],
    width: u32,
    errors: &mut Vec<Error>,
) {
    for instr in instructions {
        let mut covered = vec![false; width as usize];

        for seg in &instr.segments {
            let range = match seg {
                Segment::Fixed { range, pattern, span, .. } => {
                    // Check pattern length matches range width
                    if pattern.len() as u32 != range.width() {
                        errors.push(Error::new(
                            ErrorKind::PatternLengthMismatch {
                                instruction: instr.name.clone(),
                                expected: range.width(),
                                got: pattern.len() as u32,
                            },
                            span.clone(),
                        ));
                    }
                    range
                }
                Segment::Field { range, .. } => range,
            };

            for bit in range.end..=range.start {
                if bit < width {
                    let idx = bit as usize;
                    if covered[idx] {
                        errors.push(Error::new(
                            ErrorKind::OverlappingBits {
                                instruction: instr.name.clone(),
                                bit,
                            },
                            instr.span.clone(),
                        ));
                    }
                    covered[idx] = true;
                }
            }
        }

        let missing: Vec<u32> = covered
            .iter()
            .enumerate()
            .filter(|&(_, c)| !c)
            .map(|(i, _)| i as u32)
            .collect();

        if !missing.is_empty() {
            errors.push(Error::new(
                ErrorKind::BitCoverageGap {
                    instruction: instr.name.clone(),
                    missing_bits: missing,
                },
                instr.span.clone(),
            ));
        }
    }
}

fn check_pattern_conflicts(instructions: &[InstructionDef], errors: &mut Vec<Error>) {
    // O(n²) check: two instructions conflict if all their shared fixed bit positions
    // have compatible (identical) values.
    for i in 0..instructions.len() {
        for j in (i + 1)..instructions.len() {
            if patterns_conflict(&instructions[i], &instructions[j]) {
                errors.push(Error::new(
                    ErrorKind::PatternConflict {
                        a: instructions[i].name.clone(),
                        b: instructions[j].name.clone(),
                    },
                    instructions[j].span.clone(),
                ));
            }
        }
    }
}

/// Check if two instructions have conflicting fixed bit patterns.
/// A conflict occurs when both instructions have identical fixed bits at the same positions.
fn patterns_conflict(a: &InstructionDef, b: &InstructionDef) -> bool {
    let a_fixed = fixed_bit_map(a);
    let b_fixed = fixed_bit_map(b);

    for (&bit, &a_val) in &a_fixed {
        if let Some(&b_val) = b_fixed.get(&bit) {
            if a_val != b_val {
                return false;
            }
        }
    }

    if a_fixed.len() != b_fixed.len() {
        return false;
    }

    true
}

fn fixed_bit_map(instr: &InstructionDef) -> HashMap<u32, Bit> {
    let mut map = HashMap::new();
    for seg in &instr.segments {
        if let Segment::Fixed {
            range, pattern, ..
        } = seg
        {
            for (i, bit) in pattern.iter().enumerate() {
                let hw_bit = range.start - i as u32;
                map.insert(hw_bit, *bit);
            }
        }
    }
    map
}

fn resolve_field_type(field_type: &FieldType, type_aliases: &[TypeAlias]) -> ResolvedFieldType {
    match field_type {
        FieldType::Alias(name) => {
            if let Some(alias) = type_aliases.iter().find(|ta| ta.name == *name) {
                ResolvedFieldType {
                    base_type: alias.base_type.clone(),
                    wrapper_type: alias.wrapper_type.clone(),
                    transforms: alias.transforms.clone(),
                }
            } else {
                // Built-in type used as alias
                ResolvedFieldType {
                    base_type: resolve_builtin(name),
                    wrapper_type: None,
                    transforms: Vec::new(),
                }
            }
        }
        FieldType::Inline {
            base_type,
            transforms,
        } => ResolvedFieldType {
            base_type: resolve_builtin(base_type),
            wrapper_type: None,
            transforms: transforms.clone(),
        },
    }
}

fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "u1" | "u2" | "u3" | "u4" | "u5" | "u6" | "u7" | "u8" | "u16" | "u32" | "i8" | "i16" | "i32" | "bool"
    )
}

fn resolve_builtin(name: &str) -> String {
    match name {
        "u1" | "u2" | "u3" | "u4" | "u5" | "u6" | "u7" => "u8".to_string(),
        "bool" => "bool".to_string(),
        other => other.to_string(),
    }
}
