//! The encoder, the inverse of decode. Fixed bits, affine field inversion and `assemble` scatter
//! are inverted directly. A `fn` computed operand whose body is an affine or shift/mask chain
//! over a single parameter is inverted structurally; anything else falls back to enumerating the
//! bounded input domain, capped at `INV_CAP`. The round-trip property is
//! `encode(decode(word)) == word`, ignoring don't-care bits.

use crate::compute::{eval_value, mask128, mask_u64 as mask, sext128, Env};
use crate::model::*;
use chipi_syntax::ast::{BinOp, Expr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EncodeError {
    UnknownOperand(String),
    FieldOutOfRange {
        name: String,
        value: i128,
        bits: u16,
    },
    /// A computed-`fn` operand value has no encoding: no input produces it.
    NotEncodable(String),
    /// The `fn` is not structurally invertible and its input domain exceeds `INV_CAP`, so no
    /// reverse table is enumerated.
    DomainTooLarge {
        func: String,
        operand: String,
        domain: u128,
    },
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EncodeError::UnknownOperand(n) => write!(f, "no value supplied for operand `{n}`"),
            EncodeError::FieldOutOfRange { name, value, bits } => {
                write!(
                    f,
                    "value {value} does not fit operand `{name}` ({bits} bits)"
                )
            }
            EncodeError::NotEncodable(n) => write!(f, "operand `{n}` value has no valid encoding"),
            EncodeError::DomainTooLarge {
                func,
                operand,
                domain,
            } => {
                write!(
                    f,
                    "fn `{func}` is not directly invertible and operand `{operand}` would need \
                     a reverse table of {domain} entries (over INV_CAP = {INV_CAP})"
                )
            }
        }
    }
}

/// Cap on the `fn`-input domain chipi enumerates to build a reverse table.
pub const INV_CAP: u128 = 1 << 24;

/// Encode an instruction (by index) from `(operand, value)` pairs.
pub fn encode(
    isa: &Isa,
    instr_index: usize,
    values: &[(String, i128)],
) -> Result<u64, EncodeError> {
    encode_probe(isa, instr_index, values).0
}

/// [`encode`], also reporting whether any `fn` inversion fell back to enumerating its input
/// domain (`true`) rather than taking the structural path. A test probe, not a public API.
#[doc(hidden)]
pub fn encode_probe(
    isa: &Isa,
    instr_index: usize,
    values: &[(String, i128)],
) -> (Result<u64, EncodeError>, bool) {
    let mut enumerated = false;
    let result = encode_inner(isa, instr_index, values, &mut enumerated);
    (result, enumerated)
}

fn encode_inner(
    isa: &Isa,
    instr_index: usize,
    values: &[(String, i128)],
    enumerated: &mut bool,
) -> Result<u64, EncodeError> {
    let inst = &isa.instrs[instr_index];
    let get = |name: &str| values.iter().find(|(n, _)| n == name).map(|(_, v)| *v);

    // A supplied value must fit its operand's declared width (signed or unsigned reading).
    // The encoder rejects over-wide values itself: a `fetch` operand has no in-window bits,
    // so nothing downstream would otherwise catch `lda #$1234` against an 8bit immediate.
    for (name, v) in values {
        let vw = inst
            .fields
            .iter()
            .find(|f| &f.name == name)
            .map(|f| f.ty.value_width)
            .or_else(|| {
                inst.computed
                    .iter()
                    .find(|c| &c.name == name)
                    .map(|c| c.ty.value_width)
            });
        if let Some(w) = vw {
            if !value_fits(*v, w) {
                return Err(EncodeError::NotEncodable(name.clone()));
            }
        }
    }

    let mut word = 0u64;

    // fixed bits
    for c in &inst.fixed {
        word |= (c.value << c.range.lo) & c.range.mask();
    }

    // base word for fn-inversion: fixed bits + every supplied field's raw bits.
    let mut base_word = word;
    let mut supplied: Vec<(String, u64)> = Vec::new();
    for f in &inst.fields {
        if let Some(v) = get(&f.name) {
            let raw = invert_xforms(v, &f.ty) & mask(f.range.width());
            base_word |= raw << f.range.lo;
            supplied.push((f.name.clone(), raw));
        }
    }

    // fn-inversion for `name = fn(field...)` computed operands: the structural inverse first,
    // then the bounded enumeration as a fallback.
    let mut resolved: Vec<(String, u64)> = Vec::new();
    for c in &inst.computed {
        if !matches!(&c.expr, Expr::Call { .. }) {
            continue;
        }
        let deps = inversion_deps(inst, c);
        let to_enum: Vec<&Field> = deps
            .iter()
            .copied()
            .filter(|f| get(&f.name).is_none())
            .collect();
        if to_enum.is_empty() {
            continue;
        }
        let Some(target) = get(&c.name) else { continue };
        if let Some(hit) = invert_direct(isa, inst, c, &to_enum, base_word, &supplied, target) {
            resolved.push(hit);
            continue;
        }
        resolved.extend(invert_computed(
            isa, inst, c, &to_enum, base_word, &supplied, target, enumerated,
        )?);
    }

    // bound fields: from a supplied value, or recovered by fn-inversion.
    for f in &inst.fields {
        let raw = if let Some(v) = get(&f.name) {
            invert_xforms(v, &f.ty)
        } else if let Some((_, r)) = resolved.iter().find(|(n, _)| n == &f.name) {
            *r
        } else {
            return Err(EncodeError::UnknownOperand(f.name.clone()));
        };
        word |= (raw & mask(f.range.width())) << f.range.lo;
    }

    // `assemble` computed operands: scatter the value bits back to their word positions.
    for c in &inst.computed {
        if let Expr::Assemble { parts, .. } = &c.expr {
            let value = get(&c.name).ok_or_else(|| EncodeError::UnknownOperand(c.name.clone()))?;
            let v = value as u128;
            for p in parts {
                if let Expr::Slice { base, hi, lo, .. } = &p.src {
                    if matches!(base.as_ref(), Expr::Name(n) if n.text == "word") {
                        let dest_w = (p.hi - p.lo + 1) as u16;
                        let bits = ((v >> p.lo) & mask128(dest_w)) as u64;
                        word |= (bits & mask((hi - lo + 1) as u16)) << lo;
                    }
                }
            }
        }
    }

    Ok(word)
}

/// Does `v` fit a `w`-bit operand, read as either signed or unsigned? (`0x80` is accepted for
/// an `i8` displacement; `0x1234` is rejected for a `u8` direct-page operand.)
pub fn value_fits(v: i128, w: u16) -> bool {
    if w == 0 || w >= 128 {
        return true;
    }
    let lo = -(1i128 << (w - 1));
    let hi = 1i128 << w;
    v >= lo && v < hi
}

/// Reverse a field's transform pipeline: operand value back to raw field bits.
fn invert_xforms(value: i128, ty: &FieldTy) -> u64 {
    let mut v = value as u128;
    for x in ty.xforms.iter().rev() {
        match x {
            Xform::ShiftLeft(n) => v >>= n,
            Xform::ShiftRight(n) => v <<= n,
            Xform::ZeroExtend(_) => {}
            Xform::SignExtend(n) => v &= mask128(*n),
            Xform::RotateLeft(k, w) => v = rot(v, (*w as u32) - (*k as u32 % *w as u32), *w),
            Xform::RotateRight(k, w) => v = rot(v, *k as u32, *w),
        }
    }
    (v as u64) & mask(ty.raw_width)
}

fn rot(v: u128, k: u32, w: u16) -> u128 {
    if w == 0 {
        return 0;
    }
    let m = (1u128 << w) - 1;
    let v = v & m;
    let k = k % w as u32;
    ((v << k) | (v >> (w as u32 - k))) & m
}

/// Fields whose raw bits the fn-inversion must enumerate: every field referenced by the computed
/// expression *or* the leaf guard, in field-declaration order.
fn inversion_deps<'a>(inst: &'a Insn, c: &Computed) -> Vec<&'a Field> {
    let mut names = Vec::new();
    crate::compute::expr_names(&c.expr, &mut names);
    if let Some(g) = &inst.guard {
        crate::compute::expr_names(g, &mut names);
    }
    inst.fields
        .iter()
        .filter(|f| names.contains(&f.name))
        .collect()
}

fn field_widths(inst: &Insn) -> Vec<(String, u16)> {
    inst.fields
        .iter()
        .map(|f| (f.name.clone(), f.range.width()))
        .collect()
}

/// One invertible transform applied to the fn parameter, innermost first.
enum Step {
    Add(u128),
    Mul(u128),
    Shl(u32),
    Shr(u32),
    Or(u128),
    And(u128),
}

/// Structural inversion for the common single-field shapes: a `fn` body that is an affine or
/// shift/mask chain over one parameter (`n * 4 + 2`, `(n << 3) | 5`, `x[7:4]`, ...). Returns the
/// recovered `(field, raw)` pair, or `None` to fall back to enumeration. Every candidate is
/// verified by forward evaluation and is the smallest preimage, so a `Some` is always exactly
/// what the enumeration would have found.
fn invert_direct(
    isa: &Isa,
    inst: &Insn,
    c: &Computed,
    to_enum: &[&Field],
    base_word: u64,
    supplied: &[(String, u64)],
    target: i128,
) -> Option<(String, u64)> {
    let [field] = to_enum else { return None };
    let Expr::Call { callee, args, .. } = &c.expr else {
        return None;
    };
    let func = isa.fns.iter().find(|f| f.name == callee.text)?;
    if !func.lets.is_empty() || func.params.len() != args.len() {
        return None;
    }

    // Exactly one argument may depend on the missing field (or on the word, which contains its
    // bits), and it must be the bare field name so the parameter tracks the raw bits one-to-one.
    let depends = |a: &Expr| uses_name(a, &field.name) || uses_name(a, "word");
    let pos = args.iter().position(depends)?;
    if args.iter().skip(pos + 1).any(depends) {
        return None;
    }
    if !matches!(&args[pos], Expr::Name(n) if n.text == field.name) {
        return None;
    }

    // Bind every other parameter to its argument's value; inside the body they are constants.
    let widths = field_widths(inst);
    let supplied128: Vec<(String, u128)> = supplied
        .iter()
        .map(|(n, r)| (n.clone(), *r as u128))
        .collect();
    let outer = Env {
        word: base_word as u128,
        word_width: isa.window_bits(),
        field: &|n: &str| supplied128.iter().find(|(x, _)| x == n).map(|(_, v)| *v),
        width: &|n: &str| widths.iter().find(|(x, _)| x == n).map(|(_, w)| *w),
        fns: &isa.fns,
    };
    let mut params: Vec<(String, u128, u16)> = Vec::new();
    for (i, ((name, ty), arg)) in func.params.iter().zip(args).enumerate() {
        if i != pos {
            let v = eval_value(arg, &outer) & mask128(ty.width());
            params.push((name.clone(), v, ty.width()));
        }
    }

    // Match the body as a chain of steps over the target parameter. The fn body evaluates with
    // `word` zeroed (see eval_fn), so the constant sides get the same environment.
    let consts = Env {
        word: 0,
        word_width: 64,
        field: &|n: &str| params.iter().find(|(x, _, _)| x == n).map(|(_, v, _)| *v),
        width: &|n: &str| params.iter().find(|(x, _, _)| x == n).map(|(_, _, w)| *w),
        fns: &isa.fns,
    };
    let mut steps = Vec::new();
    body_steps(&func.ret_expr, &func.params[pos].0, &consts, &mut steps)?;

    // Bound the value's possible bits through the chain. A step that could wrap, or a result
    // that the return or operand width would truncate, disqualifies the whole chain.
    let in_bits = mask128(func.params[pos].1.width().min(field.range.width()));
    let out_bits = check_steps(&steps, in_bits)?;
    if out_bits & !mask128(func.ret.width()) != 0 || out_bits & !mask128(c.ty.value_width) != 0 {
        return None;
    }

    // Undo the operand's value-width clamp, then run the chain backwards.
    let y = (target as u128) & mask128(c.ty.value_width);
    let back = if c.ty.signed {
        sext128(y, c.ty.value_width) as i128
    } else {
        y as i128
    };
    if back != target || y & !out_bits != 0 {
        return None;
    }
    let x = apply_inverse(&steps, y)?;
    if x & !in_bits != 0 {
        return None;
    }

    // Verify exactly the way the enumeration would: forward-evaluate the operand and the guard
    // over the candidate word.
    let raw = x as u64;
    let mut raws = supplied128.clone();
    raws.push((field.name.clone(), x));
    let full = base_word | ((raw & mask(field.range.width())) << field.range.lo);
    if eval_synthetic(isa, &c.expr, &raws, &widths, full, Some(&c.ty)) != target {
        return None;
    }
    if let Some(g) = &inst.guard {
        if eval_synthetic(isa, g, &raws, &widths, full, None) == 0 {
            return None;
        }
    }

    Some((field.name.clone(), raw))
}

/// Match a fn body as `Step`s applied to `param`, innermost first. Anything else, including a
/// second use of the parameter, rejects the chain.
fn body_steps(e: &Expr, param: &str, consts: &Env, out: &mut Vec<Step>) -> Option<()> {
    match e {
        Expr::Name(n) if n.text == param => Some(()),
        Expr::Slice { base, hi, lo, .. } => {
            body_steps(base, param, consts, out)?;
            if *lo >= 128 {
                return None;
            }
            out.push(Step::Shr(*lo));
            out.push(Step::And(mask128((hi - lo + 1) as u16)));
            Some(())
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            let (var, konst, var_left) = match (uses_name(lhs, param), uses_name(rhs, param)) {
                (true, false) => (lhs, rhs, true),
                (false, true) => (rhs, lhs, false),
                _ => return None,
            };
            body_steps(var, param, consts, out)?;
            let k = eval_value(konst, consts);
            let step = match op {
                BinOp::Add => Step::Add(k),
                BinOp::Mul if k != 0 => Step::Mul(k),
                BinOp::Shl if var_left && k < 128 => Step::Shl(k as u32),
                BinOp::Shr if var_left && k < 128 => Step::Shr(k as u32),
                BinOp::BitOr => Step::Or(k),
                BinOp::BitAnd => Step::And(k),
                _ => return None,
            };
            out.push(step);
            Some(())
        }
        _ => None,
    }
}

fn uses_name(e: &Expr, name: &str) -> bool {
    let mut found = false;
    e.walk(&mut |x| {
        if let Expr::Name(n) = x {
            if n.text == name {
                found = true;
            }
        }
    });
    found
}

/// Track a superset of the value's possible bits through the chain. `None` means a step could
/// overflow, or set a bit an `|` constant would clobber, so the algebraic inverse would not
/// match the evaluator.
fn check_steps(steps: &[Step], in_bits: u128) -> Option<u128> {
    // Arithmetic loses the exact bit structure; widen to a contiguous mask over the sum's width.
    let widen = |v: u128| mask128((128 - v.leading_zeros()) as u16);
    let mut bits = in_bits;
    for s in steps {
        bits = match *s {
            Step::Add(c) => widen(bits.checked_add(c)?),
            Step::Mul(k) => widen(bits.checked_mul(k)?),
            Step::Shl(k) => {
                if k > bits.leading_zeros() {
                    return None;
                }
                bits << k
            }
            Step::Shr(k) => bits >> k,
            Step::Or(c) => {
                if c & bits != 0 {
                    return None;
                }
                bits | c
            }
            Step::And(m) => bits & m,
        };
    }
    Some(bits)
}

/// Run the chain backwards over the operand's bit pattern, taking the smallest preimage at every
/// lossy step. `None` means the value has no preimage under this chain.
fn apply_inverse(steps: &[Step], value: u128) -> Option<u128> {
    let mut y = value;
    for s in steps.iter().rev() {
        y = match *s {
            Step::Add(c) => y.checked_sub(c)?,
            Step::Mul(k) => {
                if y % k != 0 {
                    return None;
                }
                y / k
            }
            Step::Shl(k) => {
                if y & mask128(k as u16) != 0 {
                    return None;
                }
                y >> k
            }
            Step::Shr(k) => {
                if k > y.leading_zeros() {
                    return None;
                }
                y << k
            }
            Step::Or(c) => {
                if y & c != c {
                    return None;
                }
                y & !c
            }
            Step::And(m) => {
                if y & !m != 0 {
                    return None;
                }
                y
            }
        };
    }
    Some(y)
}

/// Enumerate the raw-bit domain of the unsupplied `deps`. Return the smallest-word assignment
/// whose evaluation equals `target` and passes the guard. Sets `enumerated` once the fallback
/// actually enumerates (the structural inverse and the over-cap error never do).
#[allow(clippy::too_many_arguments)]
fn invert_computed(
    isa: &Isa,
    inst: &Insn,
    c: &Computed,
    deps: &[&Field],
    base_word: u64,
    supplied: &[(String, u64)],
    target: i128,
    enumerated: &mut bool,
) -> Result<Vec<(String, u64)>, EncodeError> {
    // Size the enumeration domain, bailing out if it exceeds the cap.
    let mut domain: u128 = 1;
    for f in deps {
        domain = domain.saturating_mul(1u128 << f.range.width());
        if domain > INV_CAP {
            let func = match &c.expr {
                Expr::Call { callee, .. } => callee.text.clone(),
                _ => c.name.clone(),
            };
            return Err(EncodeError::DomainTooLarge {
                func,
                operand: c.name.clone(),
                domain,
            });
        }
    }

    *enumerated = true;

    let widths = field_widths(inst);
    let mut raws: Vec<(String, u128)> = supplied
        .iter()
        .map(|(n, r)| (n.clone(), *r as u128))
        .collect();
    let dep_start = raws.len();
    for f in deps {
        raws.push((f.name.clone(), 0));
    }

    // Search the domain for the smallest word contribution that hits the target and passes the guard.
    let mut best: Option<(u64, u64)> = None; // (word contribution, winning index)
    for idx in 0..domain as u64 {
        let mut rem = idx;
        let mut contrib = 0u64;
        for (i, f) in deps.iter().enumerate() {
            let span = 1u64 << f.range.width();
            let raw = rem % span;
            rem /= span;
            contrib |= (raw & mask(f.range.width())) << f.range.lo;
            raws[dep_start + i].1 = raw as u128;
        }

        let full = base_word | contrib;
        if eval_synthetic(isa, &c.expr, &raws, &widths, full, Some(&c.ty)) != target {
            continue;
        }
        if let Some(g) = &inst.guard {
            if eval_synthetic(isa, g, &raws, &widths, full, None) == 0 {
                continue;
            }
        }
        if best.map(|(b, _)| contrib < b).unwrap_or(true) {
            best = Some((contrib, idx));
        }
    }

    best.map(|(_, idx)| {
        let mut rem = idx;
        deps.iter()
            .map(|f| {
                let span = 1u64 << f.range.width();
                let raw = rem % span;
                rem /= span;
                (f.name.clone(), raw)
            })
            .collect()
    })
    .ok_or_else(|| EncodeError::NotEncodable(c.name.clone()))
}

/// Evaluate `e` over `word` with prebuilt field tables, exactly as decode does. With `ty`, applies
/// the value-width clamp so the result is comparable to a decoded operand value.
fn eval_synthetic(
    isa: &Isa,
    e: &Expr,
    raws: &[(String, u128)],
    widths: &[(String, u16)],
    word: u64,
    ty: Option<&FieldTy>,
) -> i128 {
    let env = Env {
        word: word as u128,
        word_width: isa.window_bits(),
        field: &|n: &str| raws.iter().find(|(x, _)| x == n).map(|(_, v)| *v),
        width: &|n: &str| widths.iter().find(|(x, _)| x == n).map(|(_, w)| *w),
        fns: &isa.fns,
    };

    let raw = eval_value(e, &env);
    match ty {
        Some(t) if t.signed => sext128(raw, t.value_width) as i128,
        Some(t) => (raw & mask128(t.value_width)) as i128,
        None => raw as i128,
    }
}

/// Every word bit pinned by a fixed constraint, a bound field, or a word-sourced `assemble` slice.
/// Bits outside this mask are implicit don't-cares (encoded as 0).
pub fn care_mask(isa: &Isa, instr_index: usize) -> u64 {
    let inst = &isa.instrs[instr_index];
    let mut care = 0u64;
    for c in &inst.fixed {
        care |= c.range.mask();
    }
    for f in &inst.fields {
        care |= f.range.mask();
    }
    for c in &inst.computed {
        if let Expr::Assemble { parts, .. } = &c.expr {
            for p in parts {
                if let Expr::Slice { base, hi, lo, .. } = &p.src {
                    if matches!(base.as_ref(), Expr::Name(n) if n.text == "word") {
                        care |= mask((hi - lo + 1) as u16) << lo;
                    }
                }
            }
        }
    }
    care
}

/// Round-trip a word: decode it, re-encode from the decoded operand values, then compare,
/// ignoring bits outside the care mask. Returns `None` for words that do not decode to a valid leaf.
pub fn roundtrip(isa: &Isa, word: u64) -> Option<bool> {
    let d = crate::interp::decode(isa, word);
    let instr_index = d.instr_index?;
    let values: Vec<(String, i128)> = d.fields.iter().map(|f| (f.name.clone(), f.value)).collect();

    let re = encode(isa, instr_index, &values).ok()?;
    let care = care_mask(isa, instr_index);
    Some((word & care) == (re & care))
}
