//! Naming of computed-operand accessors, shared by every backend.
//!
//! A computed operand (`name: ty = expr` / `= fetch(N)` / `= assemble ...`) is emitted as one
//! accessor per instruction. Prefixing each with its instruction name (`jmp_abl__addr`) keeps the
//! flat `Instruction` namespace from colliding, but that prefix is almost always redundant. The same
//! operand (`addr` fetched as 16 bits) is declared the same way across dozens of instructions, so the
//! prefixed methods have identical bodies.
//!
//! [`computed_accessor_names`] folds those duplicates the same way plain bit-fields are folded by
//! [`crate::model::Isa::unique_fields`]. Computed operands that share a name, a return type and an
//! emitted body collapse to one bare accessor (`addr`). Only a real clash gets disambiguated: the
//! same name standing for two different computations, e.g. a 16bit and a 24bit `addr`. The suffix is
//! meaningful (the operand type name when one exists, else the base spelling) rather than the
//! instruction, so you get `addr_abs` / `addr_abl`, not `lda_abl__addr`.
//!
//! The returned names are the logical (pre-sanitisation) identifiers. Each backend applies its own
//! identifier rules on top.

use crate::interp::fetch_width;
use crate::model::{FieldTy, Insn, Isa};
use chipi_syntax::ast::Expr;
use std::collections::{BTreeMap, HashMap, HashSet};

/// The deduplicated accessor name for every computed operand, keyed by `(instruction, operand)`.
///
/// Two computed operands fold onto one name iff they agree on operand name, return type
/// (signedness and value width) and emitted body. A name carried by more than one distinct body is
/// split into `name_<suffix>` accessors instead.
pub fn computed_accessor_names(isa: &Isa) -> HashMap<(String, String), String> {
    // Every computed-operand occurrence, tagged with the structural identity that decides folding
    // and the suffix it would take if its name turns out to clash.
    struct Occ {
        instr: String,
        operand: String,
        identity: String,
        suffix: String,
    }

    let wb = (isa.window_bits() as usize).div_ceil(8);
    let mut occs: Vec<Occ> = Vec::new();
    for inst in &isa.instrs {
        // `fetch(N)` operands read stream bytes past the opcode window, at a running byte offset.
        // Two equally-shaped fetches at different offsets (e.g. `mvn`'s second operand) extract
        // different bytes, so the offset is part of the body and must split the accessor.
        let mut off = wb;
        for c in &inst.computed {
            let body = match fetch_width(&c.expr) {
                Some(bits) => {
                    let key = format!("fetch@{off}");
                    off += (bits as usize).div_ceil(8);
                    key
                }
                None => expr_key(&c.expr, inst),
            };
            let identity = format!(
                "{}|s={}|w={}|{}",
                c.name, c.ty.signed, c.ty.value_width, body
            );
            occs.push(Occ {
                instr: inst.name.clone(),
                operand: c.name.clone(),
                identity,
                suffix: disambiguator(&c.ty),
            });
        }
    }

    // Group occurrences by operand name, preserving first-seen order of the distinct identities.
    let mut by_name: BTreeMap<String, Vec<usize>> = BTreeMap::new();
    for (i, o) in occs.iter().enumerate() {
        by_name.entry(o.operand.clone()).or_default().push(i);
    }

    let mut out: HashMap<(String, String), String> = HashMap::new();
    for (name, idxs) in by_name {
        // Distinct identities under this name, in first-seen order.
        let mut order: Vec<String> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();
        for &i in &idxs {
            if seen.insert(occs[i].identity.clone()) {
                order.push(occs[i].identity.clone());
            }
        }

        // One body for this name: it keeps the bare name. Several bodies: each distinct body gets a
        // disambiguating suffix, with a numeric tail breaking any residual suffix clash so the
        // result is always unique and reproducible.
        let mut label: HashMap<String, String> = HashMap::new();
        if order.len() == 1 {
            label.insert(order[0].clone(), name.clone());
        } else {
            let mut used: HashSet<String> = HashSet::new();
            for ident in &order {
                let suffix = idxs
                    .iter()
                    .map(|&i| &occs[i])
                    .find(|o| &o.identity == ident)
                    .map(|o| o.suffix.clone())
                    .unwrap_or_default();
                let mut cand = format!("{name}_{suffix}");
                let mut n = 2;
                while !used.insert(cand.clone()) {
                    cand = format!("{name}_{suffix}_{n}");
                    n += 1;
                }
                label.insert(ident.clone(), cand);
            }
        }

        for &i in &idxs {
            let o = &occs[i];
            out.insert(
                (o.instr.clone(), o.operand.clone()),
                label[&o.identity].clone(),
            );
        }
    }

    out
}

/// The suffix used to tell two same-named computed operands apart: the originating operand/type name
/// when the operand was bound through one (`abs`, `abl`), else its base spelling (`u16`, `i13`).
fn disambiguator(ty: &FieldTy) -> String {
    if let Some(t) = &ty.type_name {
        return t.clone();
    }
    let c = if ty.signed { 'i' } else { 'u' };
    format!("{c}{}", ty.value_width)
}

/// A structural fingerprint of a computed-operand body, matching how the backends emit it: two
/// expressions with the same key produce byte-identical accessor bodies. A field reference is keyed
/// by its bit range only, because the value evaluator reads a field as raw `(word >> lo) & mask`
/// (see `exprgen::Scope::resolve`), ignoring the field's declared transforms.
fn expr_key(e: &Expr, inst: &Insn) -> String {
    match e {
        Expr::Int(i) => format!("#{}", i.value),
        Expr::Name(n) => {
            if let Some(f) = inst.fields.iter().find(|f| f.name == n.text) {
                format!("f[{}:{}]", f.range.lo, f.range.hi)
            } else {
                format!("n({})", n.text)
            }
        }
        Expr::Slice { base, hi, lo, .. } => {
            format!("s{hi}_{lo}({})", expr_key(base, inst))
        }
        Expr::Assemble {
            out_width,
            parts,
            ext,
            ..
        } => {
            let mut s = format!("a{out_width}:{ext:?}[");
            for p in parts {
                s.push_str(&format!("{}_{}={};", p.hi, p.lo, expr_key(&p.src, inst)));
            }
            s.push(']');
            s
        }
        Expr::Unary { op, rhs, .. } => format!("u{op:?}({})", expr_key(rhs, inst)),
        Expr::Binary { op, lhs, rhs, .. } => {
            format!("b{op:?}({},{})", expr_key(lhs, inst), expr_key(rhs, inst))
        }
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "c({},{},{})",
            expr_key(cond, inst),
            expr_key(then, inst),
            expr_key(els, inst)
        ),
        Expr::Call { callee, args, .. } => {
            let a: Vec<String> = args.iter().map(|x| expr_key(x, inst)).collect();
            format!("{}({})", callee.text, a.join(","))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compile;

    fn names(src: &str) -> std::collections::BTreeMap<(String, String), String> {
        let isa = compile(src).expect("spec compiles");
        computed_accessor_names(&isa).into_iter().collect()
    }

    // A fetched operand declared inline on several instructions, all identical, folds onto one bare
    // accessor; a genuinely different shape (here a wider fetch under the same operand name) splits
    // out by base spelling instead of by instruction.
    #[test]
    fn folds_identical_and_splits_collisions() {
        let src = "
            decoder D { width = 8 bit_order = lsb0 endian = little }
            selector op [0:7]
            a op=0x01 addr:u16 = fetch(16) | \"a ${addr:04x}\"
            b op=0x02 addr:u16 = fetch(16) | \"b ${addr:04x}\"
            c op=0x03 addr:u24 = fetch(24) | \"c ${addr:06x}\"
        ";
        let n = names(src);
        assert_eq!(n[&("a".into(), "addr".into())], "addr_u16");
        assert_eq!(n[&("b".into(), "addr".into())], "addr_u16"); // folded with a
        assert_eq!(n[&("c".into(), "addr".into())], "addr_u24"); // wider fetch, so it splits
    }

    // Two equally-shaped fetches at different byte offsets within one instruction must not fold.
    #[test]
    fn distinct_names_at_distinct_offsets_stay_separate() {
        let src = "
            decoder D { width = 8 bit_order = lsb0 endian = little }
            selector op [0:7]
            mv op=0x54 dst:u8 = fetch(8) src:u8 = fetch(8) | \"mv ${src:02x},${dst:02x}\"
        ";
        let n = names(src);
        // Different operand names, so each keeps its own bare accessor regardless of offset.
        assert_eq!(n[&("mv".into(), "dst".into())], "dst");
        assert_eq!(n[&("mv".into(), "src".into())], "src");
    }

    // Layer 2: a no-range binding of a sourced operand becomes a computed operand carrying the
    // operand type name, so a name collision disambiguates by mode (`addr_abs`) not raw width.
    #[test]
    fn sourced_operand_def_drives_computed_and_naming() {
        let src = "
            decoder D { width = 8 bit_order = lsb0 endian = little }
            selector op [0:7]
            operand abs = u16 fetch(16)
            operand abl = u24 fetch(24)
            lda_abs op=0xAD addr:abs | \"lda ${addr:04x}\"
            sta_abs op=0x8D addr:abs | \"sta ${addr:04x}\"
            jmp_abl op=0x5C addr:abl | \"jmp ${addr:06x}\"
        ";
        let isa = compile(src).expect("spec compiles");

        // The no-range binding lowered to a computed operand tagged with its operand type name.
        let lda = isa.instrs.iter().find(|i| i.name == "lda_abs").unwrap();
        let c = lda.computed.iter().find(|c| c.name == "addr").unwrap();
        assert_eq!(c.ty.type_name.as_deref(), Some("abs"));
        assert_eq!(c.ty.value_width, 16);

        let n: std::collections::BTreeMap<_, _> =
            computed_accessor_names(&isa).into_iter().collect();
        assert_eq!(n[&("lda_abs".into(), "addr".into())], "addr_abs");
        assert_eq!(n[&("sta_abs".into(), "addr".into())], "addr_abs"); // folded across instrs
        assert_eq!(n[&("jmp_abl".into(), "addr".into())], "addr_abl"); // mode-named split
    }

    #[test]
    fn no_range_binding_without_source_is_rejected() {
        let src = "
            decoder D { width = 8 bit_order = lsb0 endian = little }
            selector op [0:7]
            operand reg = u8 { display(\"r{}\") }
            a op=0x01 x:reg | \"a {x}\"
        ";
        assert!(compile(src).is_err());
    }

    #[test]
    fn sourced_operand_with_transforms_is_rejected() {
        let src = "
            decoder D { width = 8 bit_order = lsb0 endian = little }
            selector op [0:7]
            operand bad = u16 fetch(16) { sign_extend(16) }
            a op=0x01 x:bad | \"a {x}\"
        ";
        assert!(compile(src).is_err());
    }
}
