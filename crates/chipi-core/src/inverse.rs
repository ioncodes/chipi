//! The encoder, the inverse of decode. Fixed bits, affine field inversion and `assemble` scatter
//! are inverted directly. Bounded non-affine `fn` computed operands are inverted by enumerating a
//! capped reverse table. The round-trip property is `encode(decode(word)) == word`, ignoring
//! don't-care bits.

use crate::compute::{eval_value, mask128, mask_u64 as mask, sext128, Env};
use crate::model::*;
use chipi_syntax::ast::Expr;
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EncodeError {
    UnknownOperand(String),
    FieldOutOfRange {
        name: String,
        value: i128,
        bits: u16,
    },
    NotInvertible(String),
    /// A computed-`fn` operand value has no encoding in the enumerated reverse table.
    NotEncodable(String),
    /// The `fn` input domain exceeds `INV_CAP`, so no reverse table is enumerated.
    DomainTooLarge {
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
            EncodeError::NotInvertible(n) => write!(f, "operand `{n}` is not invertible"),
            EncodeError::NotEncodable(n) => write!(f, "operand `{n}` value has no valid encoding"),
            EncodeError::DomainTooLarge { operand, domain } => {
                write!(
                    f,
                    "operand `{operand}` reverse table needs {domain} entries (over INV_CAP)"
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
    let inst = &isa.instrs[instr_index];
    let get = |name: &str| values.iter().find(|(n, _)| n == name).map(|(_, v)| *v);

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

    // bounded-fn inversion for `name = fn(field...)` computed operands.
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
        resolved.extend(invert_computed(
            isa, inst, c, &to_enum, base_word, &supplied, target,
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
    let mut names = BTreeSet::new();
    collect_names(&c.expr, &mut names);
    if let Some(g) = &inst.guard {
        collect_names(g, &mut names);
    }
    inst.fields
        .iter()
        .filter(|f| names.contains(&f.name))
        .collect()
}

fn collect_names(e: &Expr, out: &mut BTreeSet<String>) {
    match e {
        Expr::Name(n) => {
            out.insert(n.text.clone());
        }
        Expr::Int(_) => {}
        Expr::Slice { base, .. } => collect_names(base, out),
        Expr::Unary { rhs, .. } => collect_names(rhs, out),
        Expr::Binary { lhs, rhs, .. } => {
            collect_names(lhs, out);
            collect_names(rhs, out);
        }
        Expr::Cond {
            cond, then, els, ..
        } => {
            collect_names(cond, out);
            collect_names(then, out);
            collect_names(els, out);
        }
        Expr::Call { args, .. } => args.iter().for_each(|a| collect_names(a, out)),
        Expr::Assemble { parts, .. } => parts.iter().for_each(|p| collect_names(&p.src, out)),
    }
}

/// Enumerate the raw-bit domain of the unsupplied `deps`. Return the smallest-word assignment
/// whose evaluation equals `target` and passes the guard.
fn invert_computed(
    isa: &Isa,
    inst: &Insn,
    c: &Computed,
    deps: &[&Field],
    base_word: u64,
    supplied: &[(String, u64)],
    target: i128,
) -> Result<Vec<(String, u64)>, EncodeError> {
    // Size the enumeration domain, bailing out if it exceeds the cap.
    let mut domain: u128 = 1;
    for f in deps {
        domain = domain.saturating_mul(1u128 << f.range.width());
        if domain > INV_CAP {
            return Err(EncodeError::DomainTooLarge {
                operand: c.name.clone(),
                domain,
            });
        }
    }

    let widths: Vec<(String, u16)> = inst
        .fields
        .iter()
        .map(|f| (f.name.clone(), f.range.width()))
        .collect();
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
