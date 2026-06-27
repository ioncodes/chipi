//! Validation passes.
//!
//! [`validate`] runs the post-resolution leaf checks: bit conflicts, field overlap, coverage
//! warnings, duplicate names and global field-layout consistency. [`check_expr`] and
//! [`check_fn_expr`] validate computation-layer expressions and `fn` bodies. `lower` calls them
//! while building the IR. Range bounds and name resolution are reported in `lower`. Ambiguity and
//! table completeness are checked in `tree`.

use crate::lower::Resolved;
use crate::model::{BitRange, Field, Insn};
use chipi_syntax::ast::Expr;
use chipi_syntax::{Diag, Span};
use std::collections::HashMap;

/// Run the post-resolution validation passes, returning `(errors, warnings)`.
pub fn validate(r: &Resolved) -> (Vec<Diag>, Vec<Diag>) {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Duplicate instruction names.
    let mut first_seen: HashMap<&str, Span> = HashMap::new();
    for a in &r.instrs {
        match first_seen.get(a.name.as_str()) {
            Some(&first) => errors.push(
                Diag::error(
                    "DuplicateName",
                    format!("instruction `{}` is defined more than once", a.name),
                    a.span,
                )
                .label(first, "first defined here"),
            ),
            None => {
                first_seen.insert(&a.name, a.span);
            }
        }
    }

    // Per-leaf bit checks.
    let window = r.decoder.unit_bits as u16;
    for inst in &r.instrs {
        leaf(inst, window, &mut errors, &mut warnings);
    }

    // Cross-leaf consistency.
    field_layout(r, &mut errors);
    (errors, warnings)
}

fn leaf(inst: &Insn, window: u16, errors: &mut Vec<Diag>, warnings: &mut Vec<Diag>) {
    // Required value (+ source span) per bit fixed by a constraint.
    let mut fixed: Vec<Option<(bool, Span)>> = vec![None; window as usize];
    for c in &inst.fixed {
        for bit in c.range.lo..=c.range.hi {
            if bit >= window {
                continue;
            }
            let v = (c.value >> (bit - c.range.lo)) & 1 == 1;
            match fixed[bit as usize] {
                Some((prev, prev_span)) if prev != v => {
                    errors.push(
                        Diag::error(
                            "BitConflict",
                            format!(
                                "bit {bit} is fixed to conflicting values in `{}`",
                                inst.name
                            ),
                            c.span,
                        )
                        .label(prev_span, "previously fixed here"),
                    );
                }
                _ => fixed[bit as usize] = Some((v, c.span)),
            }
        }
    }

    // Owner span per bit bound by a field. Detects field/field and field/constraint overlap.
    let mut owner: Vec<Option<Span>> = vec![None; window as usize];
    for f in &inst.fields {
        for bit in f.range.lo..=f.range.hi {
            if bit >= window {
                continue;
            }
            if let Some(prev) = owner[bit as usize] {
                errors.push(
                    Diag::error(
                        "FieldOverlap",
                        format!(
                            "field `{}` overlaps another bound field at bit {bit}",
                            f.name
                        ),
                        f.span,
                    )
                    .label(prev, "overlapping field here"),
                );
            } else {
                owner[bit as usize] = Some(f.span);
            }
            if let Some((_, cspan)) = fixed[bit as usize] {
                errors.push(
                    Diag::error(
                        "FieldOverlap",
                        format!(
                            "field `{}` overlaps a fixed constraint at bit {bit} in `{}`",
                            f.name, inst.name
                        ),
                        f.span,
                    )
                    .label(cspan, "constraint fixes this bit"),
                );
            }
        }
    }

    // Coverage warning: bits neither fixed nor bound are implicit don't-care.
    let gaps: Vec<u16> = (0..window)
        .filter(|&b| fixed[b as usize].is_none() && owner[b as usize].is_none())
        .collect();
    if !gaps.is_empty() {
        warnings.push(Diag::warning(
            "IncompleteCoverage",
            format!(
                "`{}` leaves {} bit(s) unspecified ({}); treated as implicit don't-care",
                inst.name,
                gaps.len(),
                runs(&gaps)
            ),
            inst.span,
        ));
    }
}

/// Collapse a sorted bit list into "a, b..c" runs.
fn runs(bits: &[u16]) -> String {
    let mut out = Vec::new();
    let mut i = 0;

    while i < bits.len() {
        let start = bits[i];
        let mut end = start;
        while i + 1 < bits.len() && bits[i + 1] == end + 1 {
            end += 1;
            i += 1;
        }
        out.push(if start == end {
            format!("{start}")
        } else {
            format!("{start}..{end}")
        });
        i += 1;
    }
    out.join(", ")
}

/// One global accessor is emitted per field name, so a name shared across instructions must agree
/// on its bit range and value type.
fn field_layout(r: &Resolved, errors: &mut Vec<Diag>) {
    let mut seen: HashMap<&str, (BitRange, bool, u16, Span)> = HashMap::new();
    for inst in &r.instrs {
        for f in &inst.fields {
            let sig = (f.range, f.ty.signed, f.ty.value_width);
            match seen.get(f.name.as_str()) {
                Some(&(range, signed, vw, prev)) => {
                    if (range, signed, vw) != sig {
                        errors.push(
                            Diag::error(
                                "FieldLayout",
                                format!(
                                    "field `{}` has an inconsistent layout across instructions (one \
                                     global accessor is generated per name; its bit range and value \
                                     type must match)",
                                    f.name
                                ),
                                f.span,
                            )
                            .label(prev, "first defined with a different layout here"),
                        );
                    }
                }
                None => {
                    seen.insert(
                        f.name.as_str(),
                        (f.range, f.ty.signed, f.ty.value_width, f.span),
                    );
                }
            }
        }
    }
}

// ---------------------------------------------------------------- expression validation

/// Validate a computation-layer expression (computed operand or guard). Names must be `word` or a
/// bound field. Slices and assembles must be in bounds. `assemble` destinations must not overlap.
/// Calls must be a builtin, a declared `fn`, or `fetch(N)`.
pub fn check_expr(
    e: &Expr,
    fields: &[Field],
    window: u16,
    fn_names: &[String],
    errs: &mut Vec<Diag>,
) {
    match e {
        Expr::Int(_) => {}
        Expr::Name(n) => {
            if n.text != "word" && !fields.iter().any(|f| f.name == n.text) {
                errs.push(Diag::error(
                    "UnknownName",
                    format!(
                        "`{}` is not `word` or a bound field in this instruction",
                        n.text
                    ),
                    n.span,
                ));
            }
        }
        Expr::Slice { base, hi, lo, span } => {
            if lo > hi {
                errs.push(Diag::error(
                    "RangeOutOfBounds",
                    "slice low bound exceeds high bound",
                    *span,
                ));
            }
            if let Expr::Name(n) = base.as_ref() {
                if n.text == "word" && *hi >= window as u32 {
                    errs.push(Diag::error(
                        "RangeOutOfBounds",
                        format!("`word[{hi}]` is outside the {window}bit window"),
                        *span,
                    ));
                }
            }
            check_expr(base, fields, window, fn_names, errs);
        }
        Expr::Assemble {
            out_width,
            parts,
            span,
            ..
        } => {
            if *out_width == 0 || *out_width > 64 {
                errs.push(Diag::error(
                    "WidthMismatch",
                    format!("assemble output width {out_width} must be in 1..=64"),
                    *span,
                ));
                return;
            }
            let mut taken = vec![false; *out_width as usize];
            for p in parts {
                if p.lo > p.hi {
                    errs.push(Diag::error(
                        "RangeOutOfBounds",
                        "assemble destination low bound exceeds high bound",
                        p.span,
                    ));
                } else if p.hi >= *out_width {
                    errs.push(Diag::error(
                        "RangeOutOfBounds",
                        format!(
                            "assemble destination bit {} is >= the output width {out_width}",
                            p.hi
                        ),
                        p.span,
                    ));
                } else {
                    for b in p.lo..=p.hi {
                        if taken[b as usize] {
                            errs.push(Diag::error(
                                "AssembleOverlap",
                                format!("assemble destination bit {b} is assigned more than once"),
                                p.span,
                            ));
                        } else {
                            taken[b as usize] = true;
                        }
                    }
                }
                check_expr(&p.src, fields, window, fn_names, errs);
            }
        }
        Expr::Unary { rhs, .. } => check_expr(rhs, fields, window, fn_names, errs),
        Expr::Binary { lhs, rhs, .. } => {
            check_expr(lhs, fields, window, fn_names, errs);
            check_expr(rhs, fields, window, fn_names, errs);
        }
        Expr::Cond {
            cond, then, els, ..
        } => {
            check_expr(cond, fields, window, fn_names, errs);
            check_expr(then, fields, window, fn_names, errs);
            check_expr(els, fields, window, fn_names, errs);
        }
        Expr::Call { callee, args, span } => {
            if callee.text == "fetch" {
                if !matches!(args.as_slice(), [Expr::Int(_)]) {
                    errs.push(Diag::error(
                        "BadFetch",
                        "`fetch(N)` takes a single constant bit width",
                        *span,
                    ));
                }
            } else {
                if !crate::compute::is_builtin(&callee.text) && !fn_names.contains(&callee.text) {
                    errs.push(Diag::error(
                        "UnknownName",
                        format!("`{}` is not a known builtin or declared `fn`", callee.text),
                        *span,
                    ));
                }
                for a in args {
                    check_expr(a, fields, window, fn_names, errs);
                }
            }
        }
    }
}

/// Validate a `fn` body expression. Names must be params or lets in `scope`. Calls must be a builtin
/// or a declared `fn`. `word` and instruction fields are not visible inside a `fn`.
pub fn check_fn_expr(e: &Expr, scope: &[String], fn_names: &[String], errs: &mut Vec<Diag>) {
    match e {
        Expr::Int(_) => {}
        Expr::Name(n) => {
            if !scope.contains(&n.text) {
                errs.push(Diag::error(
                    "UnknownName",
                    format!(
                        "`{}` is not a parameter or `let` binding in this fn",
                        n.text
                    ),
                    n.span,
                ));
            }
        }
        Expr::Slice { base, .. } => check_fn_expr(base, scope, fn_names, errs),
        Expr::Assemble { parts, .. } => {
            for p in parts {
                check_fn_expr(&p.src, scope, fn_names, errs);
            }
        }
        Expr::Unary { rhs, .. } => check_fn_expr(rhs, scope, fn_names, errs),
        Expr::Binary { lhs, rhs, .. } => {
            check_fn_expr(lhs, scope, fn_names, errs);
            check_fn_expr(rhs, scope, fn_names, errs);
        }
        Expr::Cond {
            cond, then, els, ..
        } => {
            check_fn_expr(cond, scope, fn_names, errs);
            check_fn_expr(then, scope, fn_names, errs);
            check_fn_expr(els, scope, fn_names, errs);
        }
        Expr::Call { callee, args, span } => {
            if !crate::compute::is_builtin(&callee.text) && !fn_names.contains(&callee.text) {
                errs.push(Diag::error(
                    "UnknownName",
                    format!("`{}` is not a known builtin or declared `fn`", callee.text),
                    *span,
                ));
            }
            for a in args {
                check_fn_expr(a, scope, fn_names, errs);
            }
        }
    }
}
