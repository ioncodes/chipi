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
///
/// The result is language-agnostic. Language-specific configuration
/// (type mappings, dispatch strategies) is supplied separately to backends.
pub fn validate(def: &DecoderDef) -> Result<ValidatedDef, Vec<Error>> {
    let mut errors = Vec::new();

    // Collect sub-decoder names for type resolution
    let sub_decoder_names: HashSet<String> =
        def.sub_decoders.iter().map(|sd| sd.name.clone()).collect();

    // Phase 0: Convert all bit ranges from DSL notation to hardware notation
    let instructions = convert_bit_ranges(def);

    // Phase 1: Name uniqueness
    check_name_uniqueness(&instructions, &def.type_aliases, &mut errors);

    // Phase 2: Type resolution (skip sub-decoder types, they're valid)
    resolve_types(
        &instructions,
        &def.type_aliases,
        &sub_decoder_names,
        &mut errors,
    );

    // Phase 3: Bit coverage
    check_bit_coverage(&instructions, def.config.width, &mut errors);

    // Phase 3b: Max units check (if configured)
    if def.config.max_units.is_some() {
        check_max_units(&instructions, &def.config, &mut errors);
    }

    // Phase 4: Pattern conflicts
    check_pattern_conflicts(&instructions, &mut errors);

    // Phase 5: Map validation
    check_maps(&def.maps, &mut errors);

    // Phase 6: Format validation
    check_formats(&instructions, &def.maps, &mut errors);

    // Phase 7: Validate sub-decoders
    let validated_sub_decoders = validate_sub_decoders(&def.sub_decoders, &mut errors);

    // Phase 8: Validate sub-decoder field widths
    check_sub_decoder_field_widths(
        &instructions,
        &def.sub_decoders,
        &sub_decoder_names,
        &mut errors,
    );

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
                        ranges,
                        ..
                    } = seg
                    {
                        let resolved = resolve_field_type(
                            field_type,
                            &def.type_aliases,
                            &sub_decoder_names,
                            &def.sub_decoders,
                        );
                        Some(ResolvedField {
                            name: name.clone(),
                            ranges: ranges.clone(),
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
                format_lines: instr.format_lines.clone(),
                span: instr.span.clone(),
            }
        })
        .collect();

    Ok(ValidatedDef {
        config: def.config.clone(),
        type_aliases: def.type_aliases.clone(),
        maps: def.maps.clone(),
        instructions: validated_instructions,
        sub_decoders: validated_sub_decoders,
    })
}

/// Convert all DSL bit ranges to hardware (LSB=0) notation.
/// Now supports cross-unit fields by splitting them into multiple BitRange objects.
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
                        ranges,
                        pattern,
                        span,
                    } => {
                        // Convert each DSL range to hardware notation
                        // Since ranges is already vec![range], we just need to convert it
                        let hw_ranges =
                            dsl_to_hardware(ranges[0].start, ranges[0].end, width, order);
                        Segment::Fixed {
                            ranges: hw_ranges,
                            pattern: pattern.clone(),
                            span: span.clone(),
                        }
                    }
                    Segment::Field {
                        name,
                        field_type,
                        ranges,
                        span,
                    } => {
                        // Convert each DSL range to hardware notation
                        let hw_ranges =
                            dsl_to_hardware(ranges[0].start, ranges[0].end, width, order);
                        Segment::Field {
                            name: name.clone(),
                            field_type: field_type.clone(),
                            ranges: hw_ranges,
                            span: span.clone(),
                        }
                    }
                })
                .collect();

            InstructionDef {
                name: instr.name.clone(),
                segments,
                format_lines: instr.format_lines.clone(),
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
    sub_decoder_names: &HashSet<String>,
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
                        && !sub_decoder_names.contains(alias_name)
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

fn check_bit_coverage(instructions: &[InstructionDef], width: u32, errors: &mut Vec<Error>) {
    for instr in instructions {
        // Group ranges by unit (flattening all segment ranges)
        let mut units_map: HashMap<u32, Vec<BitRange>> = HashMap::new();

        for seg in &instr.segments {
            match seg {
                Segment::Fixed {
                    ranges,
                    pattern,
                    span,
                    ..
                } => {
                    // Check pattern length matches total range width
                    let total_width: u32 = ranges.iter().map(|r| r.width()).sum();
                    if pattern.len() as u32 != total_width {
                        errors.push(Error::new(
                            ErrorKind::PatternLengthMismatch {
                                instruction: instr.name.clone(),
                                expected: total_width,
                                got: pattern.len() as u32,
                            },
                            span.clone(),
                        ));
                    }
                    for range in ranges {
                        units_map.entry(range.unit).or_default().push(*range);
                    }
                }
                Segment::Field { ranges, .. } => {
                    for range in ranges {
                        units_map.entry(range.unit).or_default().push(*range);
                    }
                }
            };
        }

        // For unit 0: require full coverage
        if let Some(unit0_ranges) = units_map.get(&0) {
            let mut covered = vec![false; width as usize];

            for range in unit0_ranges {
                for bit in range.end..=range.start {
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

        // For units 1+: only check for overlaps, gaps are allowed
        for (unit_idx, ranges) in &units_map {
            if *unit_idx == 0 {
                continue;
            }

            let mut covered = vec![false; width as usize];
            for range in ranges {
                for bit in range.end..=range.start {
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
    }
}

/// Check that instructions don't exceed the configured max_units limit.
fn check_max_units(
    instructions: &[InstructionDef],
    config: &DecoderConfig,
    errors: &mut Vec<Error>,
) {
    let max_units = config
        .max_units
        .expect("check_max_units called without max_units configured");

    for instr in instructions {
        // Find the maximum unit index across all segments
        let max_unit = instr
            .segments
            .iter()
            .flat_map(|seg| match seg {
                Segment::Fixed { ranges, .. } | Segment::Field { ranges, .. } => ranges.iter(),
            })
            .map(|range| range.unit)
            .max()
            .unwrap_or(0);

        let required_units = max_unit + 1;

        if required_units > max_units {
            errors.push(Error::new(
                ErrorKind::ExceedsMaxUnits {
                    instruction: instr.name.clone(),
                    required: required_units,
                    max_units,
                },
                instr.span.clone(),
            ).with_help(format!(
                "set max_units = {} in the decoder block or remove max_units to allow any length",
                required_units
            )));
        }
    }
}

fn check_pattern_conflicts(instructions: &[InstructionDef], errors: &mut Vec<Error>) {
    // O(n^2) check: two instructions conflict if all their shared fixed bit positions
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

fn fixed_bit_map(instr: &InstructionDef) -> HashMap<(u32, u32), Bit> {
    let mut map = HashMap::new();
    for seg in &instr.segments {
        if let Segment::Fixed {
            ranges, pattern, ..
        } = seg
        {
            let mut bit_idx = 0;
            for range in ranges {
                for i in 0..range.width() as usize {
                    if bit_idx < pattern.len() {
                        let hw_bit = range.start - i as u32;
                        // Key is (unit, hw_bit) to avoid collisions between units
                        map.insert((range.unit, hw_bit), pattern[bit_idx]);
                        bit_idx += 1;
                    }
                }
            }
        }
    }
    map
}

fn check_maps(maps: &[MapDef], errors: &mut Vec<Error>) {
    let mut seen_names: HashMap<&str, &Span> = HashMap::new();

    for map in maps {
        // Duplicate map names
        if let Some(prev) = seen_names.get(map.name.as_str()) {
            errors.push(
                Error::new(
                    ErrorKind::DuplicateMapName(map.name.clone()),
                    map.span.clone(),
                )
                .with_help(format!("first defined at line {}", prev.line)),
            );
        } else {
            seen_names.insert(&map.name, &map.span);
        }

        // Check for duplicate entries (same key pattern)
        let mut seen_keys: Vec<&Vec<MapKey>> = Vec::new();
        for entry in &map.entries {
            if seen_keys.iter().any(|k| *k == &entry.keys) {
                errors.push(Error::new(
                    ErrorKind::DuplicateMapEntry {
                        map: map.name.clone(),
                    },
                    entry.span.clone(),
                ));
            } else {
                seen_keys.push(&entry.keys);
            }
        }

        // Check param uniqueness within map
        let mut seen_params: HashSet<&str> = HashSet::new();
        for param in &map.params {
            if !seen_params.insert(param.as_str()) {
                errors.push(Error::new(
                    ErrorKind::InvalidFormatString(format!(
                        "duplicate parameter '{}' in map '{}'",
                        param, map.name
                    )),
                    map.span.clone(),
                ));
            }
        }
    }
}

fn check_formats(instructions: &[InstructionDef], maps: &[MapDef], errors: &mut Vec<Error>) {
    let map_names: HashMap<&str, &MapDef> = maps.iter().map(|m| (m.name.as_str(), m)).collect();

    for instr in instructions {
        if instr.format_lines.is_empty() {
            continue;
        }

        let field_names: HashSet<String> = instr
            .segments
            .iter()
            .filter_map(|seg| {
                if let Segment::Field { name, .. } = seg {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();

        // Check guard ordering: all non-last format lines must have guards
        for (i, fl) in instr.format_lines.iter().enumerate() {
            if i < instr.format_lines.len() - 1 && fl.guard.is_none() {
                errors.push(Error::new(
                    ErrorKind::UnguardedNonLastFormatLine {
                        instruction: instr.name.clone(),
                    },
                    fl.span.clone(),
                ));
            }

            // Check guard field references
            if let Some(guard) = &fl.guard {
                for cond in &guard.conditions {
                    check_guard_operand_field(
                        &cond.left,
                        &field_names,
                        &instr.name,
                        &fl.span,
                        errors,
                    );
                    check_guard_operand_field(
                        &cond.right,
                        &field_names,
                        &instr.name,
                        &fl.span,
                        errors,
                    );
                }
            }

            // Check format string field references
            for piece in &fl.pieces {
                if let FormatPiece::FieldRef { expr, .. } = piece {
                    check_format_expr_fields(
                        expr,
                        &field_names,
                        &instr.name,
                        &fl.span,
                        &map_names,
                        errors,
                    );
                }
            }
        }
    }
}

fn check_guard_operand_field(
    operand: &GuardOperand,
    field_names: &HashSet<String>,
    instr_name: &str,
    span: &Span,
    errors: &mut Vec<Error>,
) {
    match operand {
        GuardOperand::Field(name) => {
            if !field_names.contains(name.as_str()) {
                errors.push(Error::new(
                    ErrorKind::UndefinedFieldInGuard {
                        instruction: instr_name.to_string(),
                        field: name.clone(),
                    },
                    span.clone(),
                ));
            }
        }
        GuardOperand::Expr { left, right, .. } => {
            check_guard_operand_field(left, field_names, instr_name, span, errors);
            check_guard_operand_field(right, field_names, instr_name, span, errors);
        }
        GuardOperand::Literal(_) => {}
    }
}

fn check_format_expr_fields(
    expr: &FormatExpr,
    field_names: &HashSet<String>,
    instr_name: &str,
    span: &Span,
    maps: &HashMap<&str, &MapDef>,
    errors: &mut Vec<Error>,
) {
    match expr {
        FormatExpr::Field(name) => {
            if !field_names.contains(name.as_str()) {
                errors.push(Error::new(
                    ErrorKind::UndefinedFieldInFormat {
                        instruction: instr_name.to_string(),
                        field: name.clone(),
                    },
                    span.clone(),
                ));
            }
        }
        FormatExpr::Ternary { field, .. } => {
            if !field_names.contains(field.as_str()) {
                errors.push(Error::new(
                    ErrorKind::UndefinedFieldInFormat {
                        instruction: instr_name.to_string(),
                        field: field.clone(),
                    },
                    span.clone(),
                ));
            }
        }
        FormatExpr::Arithmetic { left, right, .. } => {
            check_format_expr_fields(left, field_names, instr_name, span, maps, errors);
            check_format_expr_fields(right, field_names, instr_name, span, maps, errors);
        }
        FormatExpr::IntLiteral(_) => {}
        FormatExpr::MapCall { map_name, args, .. } => {
            if let Some(map_def) = maps.get(map_name.as_str()) {
                if args.len() != map_def.params.len() {
                    errors.push(Error::new(
                        ErrorKind::MapArgCountMismatch {
                            map: map_name.clone(),
                            expected: map_def.params.len(),
                            got: args.len(),
                        },
                        span.clone(),
                    ));
                }
            } else {
                errors.push(Error::new(
                    ErrorKind::UndefinedMap(map_name.clone()),
                    span.clone(),
                ));
            }
            for arg in args {
                check_format_expr_fields(arg, field_names, instr_name, span, maps, errors);
            }
        }
        FormatExpr::BuiltinCall { args, .. } => {
            // Builtins are already validated during parsing
            for arg in args {
                check_format_expr_fields(arg, field_names, instr_name, span, maps, errors);
            }
        }
        FormatExpr::SubDecoderAccess { field, .. } => {
            if !field_names.contains(field.as_str()) {
                errors.push(Error::new(
                    ErrorKind::UndefinedFieldInFormat {
                        instruction: instr_name.to_string(),
                        field: field.clone(),
                    },
                    span.clone(),
                ));
            }
            // Fragment name validation is deferred to after sub-decoders are validated
        }
    }
}

fn resolve_field_type(
    field_type: &FieldType,
    type_aliases: &[TypeAlias],
    sub_decoder_names: &HashSet<String>,
    sub_decoders: &[SubDecoderDef],
) -> ResolvedFieldType {
    match field_type {
        FieldType::Alias(name) => {
            // Check if it's a sub-decoder reference
            if sub_decoder_names.contains(name) {
                let sd = sub_decoders.iter().find(|sd| sd.name == *name).unwrap();
                let base = match sd.width {
                    w if w <= 8 => "u8",
                    w if w <= 16 => "u16",
                    _ => "u32",
                };
                return ResolvedFieldType {
                    base_type: base.to_string(),
                    alias_name: None,
                    transforms: Vec::new(),
                    display_format: None,
                    sub_decoder: Some(name.clone()),
                };
            }
            if let Some(alias) = type_aliases.iter().find(|ta| ta.name == *name) {
                ResolvedFieldType {
                    base_type: alias.base_type.clone(),
                    alias_name: Some(name.clone()),
                    transforms: alias.transforms.clone(),
                    display_format: alias.display_format,
                    sub_decoder: None,
                }
            } else {
                // Built-in type used as alias
                ResolvedFieldType {
                    base_type: resolve_builtin(name),
                    alias_name: None,
                    transforms: Vec::new(),
                    display_format: None,
                    sub_decoder: None,
                }
            }
        }
        FieldType::Inline {
            base_type,
            transforms,
        } => ResolvedFieldType {
            base_type: resolve_builtin(base_type),
            alias_name: None,
            transforms: transforms.clone(),
            display_format: None,
            sub_decoder: None,
        },
    }
}

fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "u1" | "u2"
            | "u3"
            | "u4"
            | "u5"
            | "u6"
            | "u7"
            | "u8"
            | "u16"
            | "u32"
            | "i8"
            | "i16"
            | "i32"
            | "bool"
    )
}

fn resolve_builtin(name: &str) -> String {
    match name {
        "u1" | "u2" | "u3" | "u4" | "u5" | "u6" | "u7" => "u8".to_string(),
        other => other.to_string(),
    }
}

/// Validate all sub-decoders and return validated versions.
fn validate_sub_decoders(
    sub_decoders: &[SubDecoderDef],
    errors: &mut Vec<Error>,
) -> Vec<ValidatedSubDecoder> {
    let mut validated = Vec::new();

    for sd in sub_decoders {
        // Convert bit ranges
        let instructions: Vec<_> = sd
            .instructions
            .iter()
            .map(|instr| {
                let segments: Vec<Segment> = instr
                    .segments
                    .iter()
                    .map(|seg| match seg {
                        Segment::Fixed {
                            ranges,
                            pattern,
                            span,
                        } => {
                            let hw_ranges = crate::parser::dsl_to_hardware(
                                ranges[0].start,
                                ranges[0].end,
                                sd.width,
                                sd.bit_order,
                            );
                            Segment::Fixed {
                                ranges: hw_ranges,
                                pattern: pattern.clone(),
                                span: span.clone(),
                            }
                        }
                        Segment::Field {
                            name,
                            field_type,
                            ranges,
                            span,
                        } => {
                            let hw_ranges = crate::parser::dsl_to_hardware(
                                ranges[0].start,
                                ranges[0].end,
                                sd.width,
                                sd.bit_order,
                            );
                            Segment::Field {
                                name: name.clone(),
                                field_type: field_type.clone(),
                                ranges: hw_ranges,
                                span: span.clone(),
                            }
                        }
                    })
                    .collect();
                (instr, segments)
            })
            .collect();

        // Check bit coverage for each sub-decoder instruction
        for (instr, segments) in &instructions {
            let as_instr = InstructionDef {
                name: instr.name.clone(),
                segments: segments.clone(),
                format_lines: Vec::new(),
                span: instr.span.clone(),
            };
            check_bit_coverage(&[as_instr], sd.width, errors);
        }

        // Validate fragment name consistency
        let mut fragment_names: Option<Vec<String>> = None;
        for instr in &sd.instructions {
            let names: Vec<String> = instr.fragments.iter().map(|f| f.name.clone()).collect();
            if let Some(ref expected) = fragment_names {
                if &names != expected {
                    errors.push(Error::new(
                        ErrorKind::InconsistentFragmentNames {
                            subdecoder: sd.name.clone(),
                            instruction: instr.name.clone(),
                            expected: expected.clone(),
                            got: names,
                        },
                        instr.span.clone(),
                    ));
                }
            } else {
                fragment_names = Some(names);
            }
        }

        let fragment_names = fragment_names.unwrap_or_default();

        // Validate maps within sub-decoder
        check_maps(&sd.maps, errors);

        // Build validated sub-decoder instructions
        let validated_instructions = instructions
            .into_iter()
            .map(|(instr, segments)| {
                let resolved_fields = segments
                    .iter()
                    .filter_map(|seg| {
                        if let Segment::Field {
                            name,
                            field_type,
                            ranges,
                            ..
                        } = seg
                        {
                            let resolved =
                                resolve_field_type(&field_type, &[], &HashSet::new(), &[]);
                            Some(ResolvedField {
                                name: name.clone(),
                                ranges: ranges.clone(),
                                resolved_type: resolved,
                            })
                        } else {
                            None
                        }
                    })
                    .collect();

                ValidatedSubInstruction {
                    name: instr.name.clone(),
                    segments,
                    resolved_fields,
                    fragments: instr.fragments.clone(),
                    span: instr.span.clone(),
                }
            })
            .collect();

        validated.push(ValidatedSubDecoder {
            name: sd.name.clone(),
            width: sd.width,
            bit_order: sd.bit_order,
            fragment_names,
            maps: sd.maps.clone(),
            instructions: validated_instructions,
        });
    }

    validated
}

/// Check that sub-decoder field widths don't exceed the sub-decoder's declared width.
fn check_sub_decoder_field_widths(
    instructions: &[InstructionDef],
    sub_decoders: &[SubDecoderDef],
    sub_decoder_names: &HashSet<String>,
    errors: &mut Vec<Error>,
) {
    for instr in instructions {
        for seg in &instr.segments {
            if let Segment::Field {
                name,
                field_type,
                ranges,
                span,
            } = seg
            {
                if let FieldType::Alias(alias_name) = field_type {
                    if sub_decoder_names.contains(alias_name) {
                        let sd = sub_decoders
                            .iter()
                            .find(|sd| sd.name == *alias_name)
                            .unwrap();
                        let field_width: u32 = ranges.iter().map(|r| r.width()).sum();
                        if field_width > sd.width {
                            errors.push(Error::new(
                                ErrorKind::SubDecoderFieldTooWide {
                                    field: name.clone(),
                                    field_width,
                                    subdecoder: alias_name.clone(),
                                    subdecoder_width: sd.width,
                                },
                                span.clone(),
                            ));
                        }
                    }
                }
            }
        }
    }
}
