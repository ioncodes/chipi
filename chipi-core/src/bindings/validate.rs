//! Semantic validation for resolved bindings.
//!
//! Reports unknown decoder names. Reports handler-group instructions that
//! don't exist. Reports IDA and Binja schema issues. Reports missing
//! required fields.

use std::collections::HashSet;

use crate::error::{Error, ErrorKind, Span};
use crate::types::{ValidatedDef, ValidatedSubDecoder};

use super::resolve::ResolvedBindings;
use super::types::*;

pub fn validate(resolved: &ResolvedBindings) -> Result<(), Vec<Error>> {
    let mut errors: Vec<Error> = Vec::new();

    for target in &resolved.file.targets {
        match target.kind {
            TargetKind::Rust => {
                for d in &target.rust_decoders {
                    validate_decoder_ref(resolved, d, &mut errors);
                }
                for d in &target.rust_dispatches {
                    validate_dispatch(resolved, d, None, &mut errors);
                }
            }
            TargetKind::Cpp => {
                for d in &target.cpp_decoders {
                    validate_decoder_ref(resolved, d, &mut errors);
                }
            }
            TargetKind::Ida => {
                for p in &target.ida_processors {
                    validate_processor(resolved, p, &mut errors);
                }
            }
            TargetKind::Binja => {
                for a in &target.binja_architectures {
                    validate_architecture(resolved, a, &mut errors);
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn validate_decoder_ref(resolved: &ResolvedBindings, d: &DecoderBinding, errors: &mut Vec<Error>) {
    if resolved.find_decoder_or_sub(&d.decoder_name).is_none() {
        errors.push(Error::new(
            ErrorKind::UnknownDecoderInBinding {
                name: d.decoder_name.clone(),
                suggestion: closest(&d.decoder_name, &resolved.all_decoder_names()),
            },
            d.span.clone(),
        ));
    }
    for sd in &d.subdecoders {
        if resolved.find_subdecoder(&sd.decoder_name).is_none() {
            errors.push(Error::new(
                ErrorKind::UnknownDecoderInBinding {
                    name: sd.decoder_name.clone(),
                    suggestion: closest(&sd.decoder_name, &resolved.all_decoder_names()),
                },
                sd.span.clone(),
            ));
        }
    }
}

fn validate_dispatch(
    resolved: &ResolvedBindings,
    d: &DispatchBinding,
    parent: Option<&DispatchBinding>,
    errors: &mut Vec<Error>,
) {
    let target = resolved.find_decoder_or_sub(&d.decoder_name);
    if target.is_none() {
        errors.push(Error::new(
            ErrorKind::UnknownDecoderInBinding {
                name: d.decoder_name.clone(),
                suggestion: closest(&d.decoder_name, &resolved.all_decoder_names()),
            },
            d.span.clone(),
        ));
    }

    // Check required fields. invalid_handler may be inherited from parent.
    let inherited_invalid = parent.and_then(|p| p.invalid_handler.as_ref());
    if d.invalid_handler.is_none() && inherited_invalid.is_none() {
        errors.push(Error::new(
            ErrorKind::MissingInvalidHandler(d.decoder_name.clone()),
            d.span.clone(),
        ));
    }

    let inherited_handlers = parent.and_then(|p| p.handlers.as_ref());
    if d.handlers.is_none() && inherited_handlers.is_none() {
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("dispatch {}", d.decoder_name),
                field: "handlers".to_string(),
            },
            d.span.clone(),
        ));
    }

    let inherited_context = parent.and_then(|p| p.context.as_ref());
    if d.context.is_none() && inherited_context.is_none() {
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("dispatch {}", d.decoder_name),
                field: "context".to_string(),
            },
            d.span.clone(),
        ));
    }

    let inherited_strategy = parent.and_then(|p| p.strategy);
    if d.strategy.is_none() && inherited_strategy.is_none() {
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("dispatch {}", d.decoder_name),
                field: "strategy".to_string(),
            },
            d.span.clone(),
        ));
    }

    if d.output.is_none() && parent.is_none() {
        // Top-level dispatch must have its own output. Subdispatches may
        // share the parent's file (we'll reject only if they're declared
        // top-level here).
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("dispatch {}", d.decoder_name),
                field: "output".to_string(),
            },
            d.span.clone(),
        ));
    }

    // Validate handler groups against the resolved decoder's instructions.
    if let Some((spec, sub)) = target {
        let instr_names: Vec<String> = match sub {
            Some(sd) => sd.instructions.iter().map(|i| i.name.clone()).collect(),
            None => spec
                .def
                .instructions
                .iter()
                .map(|i| i.name.clone())
                .collect(),
        };
        let known: HashSet<&str> = instr_names.iter().map(|s| s.as_str()).collect();
        for group in &d.handler_groups {
            for (name, span) in &group.instructions {
                if !known.contains(name.as_str()) {
                    errors.push(Error::new(
                        ErrorKind::UnknownInstructionInGroup {
                            instruction: name.clone(),
                            suggestion: closest(name, &instr_names),
                        },
                        span.clone(),
                    ));
                }
            }
        }
    }

    // Recurse into subdispatches.
    for sd in &d.subdispatches {
        validate_dispatch(resolved, sd, Some(d), errors);
    }
}

fn validate_processor(
    resolved: &ResolvedBindings,
    p: &IdaProcessorBinding,
    errors: &mut Vec<Error>,
) {
    require(
        &p.output,
        "output",
        "processor",
        &p.decoder_name,
        &p.span,
        errors,
    );
    require(
        &p.name,
        "name",
        "processor",
        &p.decoder_name,
        &p.span,
        errors,
    );
    require(
        &p.long_name,
        "long_name",
        "processor",
        &p.decoder_name,
        &p.span,
        errors,
    );
    require_n(&p.id, "id", "processor", &p.decoder_name, &p.span, errors);
    require_n(
        &p.address_size,
        "address_size",
        "processor",
        &p.decoder_name,
        &p.span,
        errors,
    );
    require_n(
        &p.bytes_per_unit,
        "bytes_per_unit",
        "processor",
        &p.decoder_name,
        &p.span,
        errors,
    );

    if p.registers.is_empty() {
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("processor {}", p.decoder_name),
                field: "registers".to_string(),
            },
            p.span.clone(),
        ));
    }

    let target = resolved.find_decoder_or_sub(&p.decoder_name);
    if target.is_none() {
        errors.push(Error::new(
            ErrorKind::UnknownDecoderInBinding {
                name: p.decoder_name.clone(),
                suggestion: closest(&p.decoder_name, &resolved.all_decoder_names()),
            },
            p.span.clone(),
        ));
    }

    let regs: HashSet<&str> = p.registers.iter().map(|s| s.as_str()).collect();
    for (sr, span) in &p.segment_registers {
        if !regs.contains(sr.as_str()) {
            errors.push(Error::new(
                ErrorKind::SegmentRegisterNotDeclared(sr.clone()),
                span.clone(),
            ));
        }
    }

    if let Some((spec, sub)) = target {
        let instr_names: Vec<String> = match sub {
            Some(sd) => sd.instructions.iter().map(|i| i.name.clone()).collect(),
            None => spec
                .def
                .instructions
                .iter()
                .map(|i| i.name.clone())
                .collect(),
        };
        let known: HashSet<&str> = instr_names.iter().map(|s| s.as_str()).collect();
        for group in [&p.flow.calls, &p.flow.returns, &p.flow.stops] {
            for (name, span) in group {
                if !known.contains(name.as_str()) {
                    errors.push(Error::new(
                        ErrorKind::UnknownInstructionInFlow {
                            instruction: name.clone(),
                            suggestion: closest(name, &instr_names),
                        },
                        span.clone(),
                    ));
                }
            }
        }
    }
}

fn validate_architecture(
    resolved: &ResolvedBindings,
    a: &BinjaArchitectureBinding,
    errors: &mut Vec<Error>,
) {
    require(
        &a.output,
        "output",
        "architecture",
        &a.decoder_name,
        &a.span,
        errors,
    );
    require(
        &a.name,
        "name",
        "architecture",
        &a.decoder_name,
        &a.span,
        errors,
    );
    require_n(
        &a.address_size,
        "address_size",
        "architecture",
        &a.decoder_name,
        &a.span,
        errors,
    );
    require_n(
        &a.default_int_size,
        "default_int_size",
        "architecture",
        &a.decoder_name,
        &a.span,
        errors,
    );

    if let Some((value, span)) = &a.endianness {
        if value != "big" && value != "little" {
            errors.push(Error::new(
                ErrorKind::InvalidEndianness(value.clone()),
                span.clone(),
            ));
        }
    } else {
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("architecture {}", a.decoder_name),
                field: "endianness".to_string(),
            },
            a.span.clone(),
        ));
    }

    if a.registers.is_empty() {
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("architecture {}", a.decoder_name),
                field: "registers".to_string(),
            },
            a.span.clone(),
        ));
    }

    if resolved.find_decoder_or_sub(&a.decoder_name).is_none() {
        errors.push(Error::new(
            ErrorKind::UnknownDecoderInBinding {
                name: a.decoder_name.clone(),
                suggestion: closest(&a.decoder_name, &resolved.all_decoder_names()),
            },
            a.span.clone(),
        ));
    }
}

fn require<T: AsRef<str>>(
    value: &Option<T>,
    field: &str,
    block_kind: &str,
    name: &str,
    span: &Span,
    errors: &mut Vec<Error>,
) {
    if value.is_none() {
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("{} {}", block_kind, name),
                field: field.to_string(),
            },
            span.clone(),
        ));
    }
}

fn require_n<T>(
    value: &Option<T>,
    field: &str,
    block_kind: &str,
    name: &str,
    span: &Span,
    errors: &mut Vec<Error>,
) {
    if value.is_none() {
        errors.push(Error::new(
            ErrorKind::MissingBindingsField {
                block: format!("{} {}", block_kind, name),
                field: field.to_string(),
            },
            span.clone(),
        ));
    }
}

/// Suppress unused warnings. `ValidatedDef` and `ValidatedSubDecoder` are
/// brought in via the `target` lookup above.
#[allow(dead_code)]
fn _types_used(_: &ValidatedDef, _: &ValidatedSubDecoder) {}

/// Find the candidate in `pool` closest to `target` by Levenshtein
/// distance. Returns `None` if the closest match's distance exceeds
/// `max(2, target.len() / 3)`.
fn closest(target: &str, pool: &[String]) -> Option<String> {
    let cap = (target.len() / 3).max(2);
    let mut best: Option<(usize, String)> = None;
    for cand in pool {
        let d = levenshtein(target, cand);
        if d > cap {
            continue;
        }
        if best.as_ref().map(|(bd, _)| d < *bd).unwrap_or(true) {
            best = Some((d, cand.clone()));
        }
    }
    best.map(|(_, s)| s)
}

fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur: Vec<usize> = vec![0; b.len() + 1];
    for i in 1..=a.len() {
        cur[0] = i;
        for j in 1..=b.len() {
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            cur[j] = (prev[j] + 1).min(cur[j - 1] + 1).min(prev[j - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    prev[b.len()]
}
