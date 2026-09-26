//! Validation passes.
//!
//! [`validate`] runs the post-resolution leaf checks: bit conflicts, field overlap, coverage
//! warnings, duplicate names and global field-layout consistency. [`check_expr`] and
//! [`check_fn_expr`] validate computation-layer expressions and `fn` bodies. `lower` calls them
//! while building the IR. Range bounds and name resolution are reported in `lower`. Ambiguity and
//! table completeness are checked in `tree`.

use crate::compute::mask_u64;
use crate::lower::Resolved;
use crate::model::{BitRange, Endian, Field, Insn, Mode};
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

    // Decode variables (modes, context fields) share the expression namespace with operands, so
    // an operand reusing a variable name would make guard and length reads ambiguous.
    let var_names: Vec<&str> = r
        .modes
        .iter()
        .map(|m| m.name.as_str())
        .chain(r.decoder.context.iter().map(|c| c.name.as_str()))
        .collect();
    for inst in &r.instrs {
        let op_names = inst
            .fields
            .iter()
            .map(|f| (f.name.as_str(), f.span))
            .chain(inst.computed.iter().map(|c| (c.name.as_str(), c.span)));
        for (name, span) in op_names {
            if var_names.contains(&name) {
                errors.push(Diag::error(
                    "DuplicateName",
                    format!(
                        "operand `{name}` in `{}` shadows the decode variable of the same name",
                        inst.name
                    ),
                    span,
                ));
            }
        }
    }

    // Cross-leaf consistency.
    field_layout(r, &mut errors);

    // Short `length` windows must contain everything the leaf (and the classifier) reads.
    length_windows(r, &mut errors);

    // Leaves sharing a form axis must bind one operand shape.
    form_shapes(r, &mut errors);
    (errors, warnings)
}

/// Leaves sharing a form axis must bind the same operand shape (names, value widths,
/// signedness of bound fields and computed operands, in order). That is what makes the derived
/// form enum useful: a consumer can match over the form once and use one accessor set per form.
fn form_shapes(r: &Resolved, errors: &mut Vec<Diag>) {
    // (operand name, value width, signedness, computation key) per operand, in binding order.
    // The computation key (empty for plain fields) is the same structural identity the accessor
    // dedup uses, so two leaves of one form that compute an operand differently (e.g. an m-width
    // and an x-width fetch) are a shape mismatch even though widths agree: they would split into
    // different accessors and the form's promise of one uniform accessor set would break silently.
    type Shape = Vec<(String, u16, bool, String)>;
    let mut seen: HashMap<&str, (Shape, &str, Span)> = HashMap::new();
    for inst in &r.instrs {
        let Some(form) = &inst.form else {
            continue;
        };
        let shape: Shape = inst
            .fields
            .iter()
            .map(|f| (f.name.clone(), f.ty.value_width, f.ty.signed, String::new()))
            .chain(inst.computed.iter().map(|c| {
                (
                    c.name.clone(),
                    c.ty.value_width,
                    c.ty.signed,
                    crate::accessor::expr_key(&c.expr, inst),
                )
            }))
            .collect();
        match seen.get(form.as_str()) {
            Some((first_shape, first_name, first_span)) => {
                if first_shape != &shape {
                    errors.push(
                        Diag::error(
                            "FormShape",
                            format!(
                                "`{}` binds a different operand shape than `{first_name}` under \
                                 the same form `.{form}`; leaves of one form must agree so the \
                                 form's accessors are uniform",
                                inst.name
                            ),
                            inst.span,
                        )
                        .label(*first_span, "form first bound here"),
                    );
                }
            }
            None => {
                seen.insert(form, (shape, &inst.name, inst.span));
            }
        }
    }
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

// ---------------------------------------------------------------- length windows

/// A short `length` window must contain everything read on its behalf: the leaf's bound
/// fields and fixed constraints, and the primary selector range classification reads before
/// any leaf is known. Bits outside the selected window come from bytes the stream never
/// fetched for this instruction, so an out-of-window read is garbage, not data.
///
/// The window a leaf selects is probed conservatively. The `length` table is evaluated
/// twice over the leaf's fixed bits: once with every free bit at 0 and once with every free
/// bit at 1. Only when both probes agree is the window treated as known; when they disagree
/// (a condition reads bits the leaf leaves free) the leaf is skipped rather than guessed
/// at. Both probe words match the leaf's fixed pattern, so an agreed short window is a
/// reachable decode, and erroring on it is sound. Conditions that read decode variables can
/// select a different window per host state, so a variable-reading `length` skips the pass
/// entirely. Stream consumption is byte-granular (`len_bytes = ceil(bits / 8)`), so the
/// window is rounded up to whole bytes before comparing.
fn length_windows(r: &Resolved, errors: &mut Vec<Diag>) {
    let Some(length) = &r.length else {
        return;
    };

    let window = r.decoder.unit_bits as u16;
    if window % 8 != 0 {
        // `length` selects whole stream bytes; a sub-byte fetch unit has no meaningful
        // byte-granular sub-window to validate against.
        return;
    }
    if length
        .arms
        .iter()
        .any(|a| a.cond.as_ref().is_some_and(reads_decode_var))
    {
        return;
    }

    // Probe each leaf's window: `None` means the leaf's fixed bits do not determine it.
    let full = mask_u64(window);
    let leaf_window = |inst: &Insn| -> Option<u16> {
        let (mask, val) = inst.fixed_mask_val();
        let n0 = length.bits_for(val & full);
        let n1 = length.bits_for((val | !mask) & full);
        (n0 == n1).then_some(n0)
    };
    let windows: Vec<Option<u16>> = r.instrs.iter().map(leaf_window).collect();

    for (inst, win) in r.instrs.iter().zip(&windows) {
        let Some(bits) = *win else { continue };
        let (lo, hi) = window_span(window, bits, r.decoder.endian);
        if (lo, hi) == (0, window - 1) {
            continue;
        }

        for f in &inst.fields {
            if f.range.lo < lo || f.range.hi > hi {
                errors.push(Diag::error(
                    "LengthWindow",
                    format!(
                        "field `{}` in `{}` is bound at bits [{}:{}], outside the {bits}bit \
                         window (numeric bits [{lo}:{hi}]) this encoding selects via `length`; \
                         the field would read unfetched data",
                        f.name, inst.name, f.range.lo, f.range.hi
                    ),
                    f.span,
                ));
            }
        }
        for c in &inst.fixed {
            if c.range.lo < lo || c.range.hi > hi {
                errors.push(Diag::error(
                    "LengthWindow",
                    format!(
                        "`{}` fixes bits [{}:{}], outside the {bits}bit window (numeric bits \
                         [{lo}:{hi}]) this encoding selects via `length`; the constraint would \
                         match unfetched data",
                        inst.name, c.range.lo, c.range.hi
                    ),
                    c.span,
                ));
            }
        }
    }

    // Classification reads the primary selector before any leaf is known, so the range must
    // lie inside the window of every encoding it can be handed. Checked per determined leaf
    // window; the `min_bits` span is the fast path (a selector inside the narrowest possible
    // window is inside every window).
    let Some((pname, prange, pspan)) = primary_range(r, window) else {
        return;
    };
    let (mlo, mhi) = window_span(window, length.min_bits(), r.decoder.endian);
    if prange.lo >= mlo && prange.hi <= mhi {
        return;
    }
    for (inst, win) in r.instrs.iter().zip(&windows) {
        let Some(bits) = *win else { continue };
        let (lo, hi) = window_span(window, bits, r.decoder.endian);
        if prange.lo < lo || prange.hi > hi {
            let mut d = Diag::error(
                "LengthWindow",
                format!(
                    "primary selector `{pname}` reads bits [{}:{}], but `{}` selects a \
                     {bits}bit window (numeric bits [{lo}:{hi}]) via `length`; classification \
                     would read unfetched data for this encoding",
                    prange.lo, prange.hi, inst.name
                ),
                pspan.unwrap_or(inst.span),
            );
            if pspan.is_some() {
                d = d.label(inst.span, "this instruction selects the short window");
            }
            errors.push(d);
        }
    }
}

/// The numeric bit span `[lo, hi]` a leaf may read when `length` selects `bits` out of a
/// `window`-bit fetch unit, rounded up to whole consumed bytes. Big-endian units place the
/// first fetched bytes in the numeric high bits, little-endian units in the low bits.
fn window_span(window: u16, bits: u16, endian: Endian) -> (u16, u16) {
    let consumed = (bits.div_ceil(8) * 8).min(window).max(8);
    match endian {
        Endian::Little => (0, consumed - 1),
        Endian::Big => (window - consumed, window - 1),
    }
}

/// Does `e` read any name other than `word`? In a `length` condition every other name is a
/// decode variable, which makes the selected window depend on host state.
fn reads_decode_var(e: &Expr) -> bool {
    let mut found = false;
    e.walk(&mut |x| {
        if let Expr::Name(n) = x {
            if n.text != "word" {
                found = true;
            }
        }
    });
    found
}

/// The range classification reads first: `tree::pick_primary`'s choice (via the shared
/// `narrow_primary_selector`), with the same whole-word fallback for narrow windows, so this
/// pass inspects the exact range `classify` will read.
fn primary_range(r: &Resolved, window: u16) -> Option<(String, BitRange, Option<Span>)> {
    if r.instrs.is_empty() {
        return None;
    }

    let leaf_masks: Vec<u64> = r.instrs.iter().map(|i| i.fixed_mask_val().0).collect();
    if let Some(s) = crate::tree::narrow_primary_selector(&r.selectors, &leaf_masks) {
        return Some((s.name.clone(), s.range, Some(s.span)));
    }

    if window <= 12 {
        return Some((
            "word".to_string(),
            BitRange {
                lo: 0,
                hi: window - 1,
            },
            None,
        ));
    }
    None
}

// ---------------------------------------------------------------- expression validation

/// Check a call's argument count against the canonical builtin table. Pushes a `BadArity`
/// error when `name` is a builtin called with the wrong number of arguments; does nothing
/// for user `fn` names.
fn check_call_arity(name: &str, got: usize, span: Span, errs: &mut Vec<Diag>) {
    let Some(b) = crate::compute::builtin_of(name) else {
        return;
    };
    if got >= b.min_args && got <= b.max_args {
        return;
    }

    let expected = if b.min_args == b.max_args {
        format!("exactly {}", b.min_args)
    } else if b.max_args == usize::MAX {
        format!("at least {}", b.min_args)
    } else {
        format!("{} to {}", b.min_args, b.max_args)
    };
    errs.push(Diag::error(
        "BadArity",
        format!(
            "`{}` takes {expected} argument{}, but {got} {} given",
            b.name,
            if b.min_args == 1 && b.max_args == 1 {
                ""
            } else {
                "s"
            },
            if got == 1 { "was" } else { "were" }
        ),
        span,
    ));
}

/// Validate a computation-layer expression (computed operand, guard or `length` arm). Names must
/// be `word`, a bound field, or one of `vars` (the decode variables readable in this position;
/// guards and `length` arms see modes and context fields, computed operands see none). Slices and
/// assembles must be in bounds. `assemble` destinations must not overlap. Calls must be a builtin,
/// a declared `fn`, or `fetch(N)`.
pub fn check_expr(
    e: &Expr,
    fields: &[Field],
    window: u16,
    fn_names: &[String],
    vars: &[(String, u16)],
    errs: &mut Vec<Diag>,
) {
    match e {
        Expr::Int(_) => {}
        Expr::Name(n) => {
            let known = n.text == "word"
                || fields.iter().any(|f| f.name == n.text)
                || vars.iter().any(|(vn, _)| *vn == n.text);
            if !known {
                errs.push(Diag::error(
                    "UnknownName",
                    format!(
                        "`{}` is not `word`, a bound field, or a decode variable readable here",
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
            check_expr(base, fields, window, fn_names, vars, errs);
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
                check_expr(&p.src, fields, window, fn_names, vars, errs);
            }
        }
        Expr::Unary { rhs, .. } => check_expr(rhs, fields, window, fn_names, vars, errs),
        Expr::Binary { lhs, rhs, .. } => {
            check_expr(lhs, fields, window, fn_names, vars, errs);
            check_expr(rhs, fields, window, fn_names, vars, errs);
        }
        Expr::Cond {
            cond, then, els, ..
        } => {
            check_expr(cond, fields, window, fn_names, vars, errs);
            check_expr(then, fields, window, fn_names, vars, errs);
            check_expr(els, fields, window, fn_names, vars, errs);
        }
        Expr::Call { callee, args, span } => {
            if callee.text == "fetch" {
                // The width is a constant or an expression over host modes. Mode names are not in
                // `vars` at computed-operand call sites, so the argument's names, its combo-wise
                // evaluation and its top-level-only placement are validated by `check_fetch` in
                // `lower`, not here.
                if args.len() != 1 {
                    errs.push(Diag::error(
                        "BadFetch",
                        "`fetch(width)` takes a single bit-width argument",
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
                check_call_arity(&callee.text, args.len(), *span, errs);
                for a in args {
                    check_expr(a, fields, window, fn_names, vars, errs);
                }
            }
        }
    }
}

/// Validate the `fetch` shape of a computed-operand expression. `fetch` may only be the whole
/// value of the operand, and its width argument may read host modes only (never fields, `word`
/// or context fields), so the width is decidable per mode combination. Each combination's width
/// must land in 1..=64.
pub(crate) fn check_fetch(e: &Expr, span: Span, modes: &[Mode], errs: &mut Vec<Diag>) {
    fn contains_fetch(e: &Expr) -> bool {
        let mut found = false;
        e.walk(&mut |x| {
            if let Expr::Call { callee, .. } = x {
                if callee.text == "fetch" {
                    found = true;
                }
            }
        });
        found
    }

    let Some(arg) = crate::interp::fetch_expr(e) else {
        if contains_fetch(e) {
            errs.push(Diag::error(
                "BadFetch",
                "`fetch` may only be the whole value of a computed operand",
                span,
            ));
        }
        return;
    };

    let mut names = Vec::new();
    crate::compute::expr_names(arg, &mut names);
    let mut bad_name = false;
    for n in &names {
        if !modes.iter().any(|m| &m.name == n) {
            errs.push(Diag::error(
                "BadFetch",
                format!("`fetch` width may read host modes only; `{n}` is not a mode"),
                span,
            ));
            bad_name = true;
        }
    }
    if bad_name || contains_fetch(arg) {
        if contains_fetch(arg) {
            errs.push(Diag::error(
                "BadFetch",
                "`fetch` width must not itself fetch",
                span,
            ));
        }
        return;
    }

    // Evaluate the width for every mode combination. Past the tree builder's combo cap the
    // ModeBudgetExceeded error fires anyway, so skip the sweep there.
    let combos = modes.iter().map(|m| m.cardinality).product::<u64>().max(1);
    if combos > 256 {
        return;
    }
    for combo in 0..combos {
        let vars: Vec<(String, u64)> = modes
            .iter()
            .enumerate()
            .map(|(mi, m)| (m.name.clone(), crate::tree::mode_value(modes, combo, mi)))
            .collect();
        let w = crate::interp::eval_cond(arg, &[], 0, &vars);
        if !(1..=64).contains(&w) {
            errs.push(Diag::error(
                "BadFetch",
                format!(
                    "`fetch` width evaluates to {w} for mode combination {combo}; it must be 1..=64"
                ),
                span,
            ));
            return;
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
            check_call_arity(&callee.text, args.len(), *span, errs);
            for a in args {
                check_fn_expr(a, scope, fn_names, errs);
            }
        }
    }
}
