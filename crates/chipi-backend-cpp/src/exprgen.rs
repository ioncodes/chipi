//! Lowering of chipi expressions to C++17 source.
//!
//! There are two forms. Each mirrors a `chipi-core` evaluator exactly, so the generated code matches
//! the oracle. [`emit_value`] uses unsigned `u128` for computed operands, guards, `length` and fn
//! bodies. [`emit_cond`] uses signed `__int128` for display conditions.

use crate::names::{i128_lit, ident, u128_lit, u128_mask_lit};
use chipi_core::compute::{infer_width_fields, infer_width_locals, mask128};
use chipi_core::model::Field;
use chipi_syntax::ast::{BinOp, Expr, UnOp};
use std::collections::{BTreeMap, HashMap};

/// Name-resolution scope for the `u128` value evaluator.
pub enum Scope<'a> {
    /// Operand/guard/length context: `word` and the leaf's bound fields, read off `base`.
    Computed {
        fields: &'a [Field],
        window: u16,
        base: &'a str,
    },
    /// `fn` body context: params and `let`s, bound to `v_<name>` locals with tracked widths.
    Fn { widths: HashMap<String, u16> },
}

impl Scope<'_> {
    fn resolve(&self, name: &str) -> String {
        match self {
            Scope::Computed { fields, base, .. } => {
                if name == "word" {
                    format!("((u128){base})")
                } else if let Some(f) = fields.iter().find(|f| f.name == name) {
                    format!(
                        "(((u128){base} >> {}) & cmask128({}))",
                        f.range.lo,
                        f.range.width()
                    )
                } else {
                    "(u128)0".to_string()
                }
            }
            Scope::Fn { .. } => format!("v_{}", crate::names::sanitize(name)),
        }
    }

    fn width_of(&self, e: &Expr) -> u16 {
        match self {
            Scope::Computed { fields, window, .. } => infer_width_fields(e, fields, *window),
            Scope::Fn { widths } => infer_width_locals(e, widths),
        }
    }
}

/// Lower an expression to a `u128`-valued C++ expression.
pub fn emit_value(e: &Expr, scope: &Scope) -> String {
    match e {
        Expr::Int(i) => format!("(u128){}", u128_lit(i.value)),
        Expr::Name(n) => scope.resolve(&n.text),
        Expr::Slice { base, hi, lo, .. } => {
            format!(
                "((({}) >> {lo}) & cmask128({}))",
                emit_value(base, scope),
                hi - lo + 1
            )
        }
        Expr::Assemble {
            out_width,
            parts,
            ext,
            ..
        } => {
            let mut s = String::from("({ u128 __v = 0; ");
            for p in parts {
                s.push_str(&format!(
                    "__v |= (({}) & cmask128({})) << {}; ",
                    emit_value(&p.src, scope),
                    p.hi - p.lo + 1,
                    p.lo
                ));
            }
            let tail = match ext {
                chipi_core::model::Ext::SignExtend => {
                    format!("sext128(__v & cmask128({ow}), {ow});", ow = out_width)
                }
                _ => format!("__v & cmask128({out_width});"),
            };
            s.push_str(&tail);
            s.push_str(" })");
            s
        }
        Expr::Unary { op, rhs, .. } => {
            let r = emit_value(rhs, scope);
            match op {
                UnOp::Not => format!("(~({r}))"),
                UnOp::Neg => format!("((u128)0 - ({r}))"),
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            value_binop(*op, &emit_value(lhs, scope), &emit_value(rhs, scope))
        }
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "((({}) != 0) ? ({}) : ({}))",
            emit_value(cond, scope),
            emit_value(then, scope),
            emit_value(els, scope)
        ),
        Expr::Call { callee, args, .. } => emit_call(&callee.text, args, scope),
    }
}

fn value_binop(op: BinOp, a: &str, b: &str) -> String {
    match op {
        BinOp::Add => format!("(({a}) + ({b}))"),
        BinOp::Sub => format!("(({a}) - ({b}))"),
        BinOp::Mul => format!("(({a}) * ({b}))"),
        BinOp::Div => format!("({{ u128 __d = {b}; (__d == 0) ? (u128)0 : (({a}) / __d); }})"),
        BinOp::Rem => format!("({{ u128 __d = {b}; (__d == 0) ? (u128)0 : (({a}) % __d); }})"),
        BinOp::BitAnd => format!("(({a}) & ({b}))"),
        BinOp::BitOr => format!("(({a}) | ({b}))"),
        BinOp::BitXor => format!("(({a}) ^ ({b}))"),
        BinOp::Shl => {
            format!(
                "({{ u128 __s = {b}; (__s < 128) ? (u128)(({a}) << (unsigned)__s) : (u128)0; }})"
            )
        }
        BinOp::Shr => {
            format!(
                "({{ u128 __s = {b}; (__s < 128) ? (u128)(({a}) >> (unsigned)__s) : (u128)0; }})"
            )
        }
        BinOp::Eq => format!("((u128)(({a}) == ({b})))"),
        BinOp::Ne => format!("((u128)(({a}) != ({b})))"),
        BinOp::Lt => format!("((u128)(({a}) < ({b})))"),
        BinOp::Le => format!("((u128)(({a}) <= ({b})))"),
        BinOp::Gt => format!("((u128)(({a}) > ({b})))"),
        BinOp::Ge => format!("((u128)(({a}) >= ({b})))"),
        BinOp::LAnd => format!("((u128)((({a}) != 0) && (({b}) != 0)))"),
        BinOp::LOr => format!("((u128)((({a}) != 0) || (({b}) != 0)))"),
    }
}

fn emit_call(name: &str, args: &[Expr], scope: &Scope) -> String {
    let a = |i: usize| {
        args.get(i)
            .map(|e| emit_value(e, scope))
            .unwrap_or_else(|| "(u128)0".to_string())
    };
    match name {
        "concat" => {
            let mut s = String::from("({ u128 __c = 0; ");
            for arg in args {
                let w = scope.width_of(arg);
                s.push_str(&format!("__c = (__c << {w}) | (({}) & cmask128({w})); ", emit_value(arg, scope)));
            }
            s.push_str("__c; })");
            s
        }
        "sign_extend" => format!("sext128({}, (uint16_t)({}))", a(0), a(1)),
        "zero_extend" => format!("(({}) & cmask128((uint16_t)({})))", a(0), a(1)),
        "ones" => format!("cmask128((uint16_t)({}))", a(0)),
        "replicate" => format!("replicate128({}, (uint16_t)({}), (uint16_t)({}))", a(0), a(1), a(2)),
        "rotate_left" => format!("rotl128({}, {}, (uint16_t)({}), true)", a(0), a(1), a(2)),
        "rotate_right" => format!("rotl128({}, {}, (uint16_t)({}), false)", a(0), a(1), a(2)),
        "bit_width" => format!("bitwidth128({})", a(0)),
        "clz" => format!(
            "({{ u128 __w = (u128)({}); u128 __bw = bitwidth128(({}) & cmask128((uint16_t)({}))); (__w >= __bw) ? (__w - __bw) : (u128)0; }})",
            a(1), a(0), a(1)
        ),
        "ctz" => {
            let w = args.first().map(|e| scope.width_of(e)).unwrap_or(64);
            format!("ctz128({}, {w})", a(0))
        }
        "popcount" => format!("popcount128({})", a(0)),
        "min" => format!("({{ u128 __a = {}; u128 __b = {}; (__a < __b) ? __a : __b; }})", a(0), a(1)),
        "max" => format!("({{ u128 __a = {}; u128 __b = {}; (__a > __b) ? __a : __b; }})", a(0), a(1)),
        "mask_from_range" => {
            format!("maskrange128((uint16_t)({}), (uint16_t)({}), (uint16_t)({}))", a(0), a(1), a(2))
        }
        other => {
            let argv: Vec<String> = args.iter().map(|e| emit_value(e, scope)).collect();
            format!("fn_{}({})", crate::names::sanitize(other), argv.join(", "))
        }
    }
}

/// Lower a display-arm / in-template condition to a signed `__int128`-valued C++ expression.
/// `acc` maps an operand name to its accessor method (e.g. `rd(word)`). Unknown names fall back to
/// the global accessor.
pub fn emit_cond(e: &Expr, base: &str, acc: &BTreeMap<String, String>) -> String {
    match e {
        Expr::Int(i) => format!("((__int128){})", i128_lit(i.value)),
        Expr::Name(n) => {
            if n.text == "word" {
                format!("((__int128){base})")
            } else if let Some(m) = acc.get(&n.text) {
                format!("((__int128){m})")
            } else {
                format!("((__int128){}({base}))", ident(&n.text))
            }
        }
        Expr::Unary { op, rhs, .. } => {
            let r = emit_cond(rhs, base, acc);
            match op {
                UnOp::Not => format!("(~({r}))"),
                UnOp::Neg => format!("((__int128)0 - ({r}))"),
            }
        }
        Expr::Slice {
            base: b, hi, lo, ..
        } => {
            if *lo >= 128 {
                "((__int128)0)".to_string()
            } else {
                let w = (*hi).saturating_sub(*lo).saturating_add(1).min(128) as u16;
                let mask = mask128(w);
                format!(
                    "((__int128)((((unsigned __int128)({})) >> {lo}) & {}))",
                    emit_cond(b, base, acc),
                    u128_mask_lit(mask)
                )
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            cond_binop(*op, &emit_cond(lhs, base, acc), &emit_cond(rhs, base, acc))
        }
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "((({}) != 0) ? ({}) : ({}))",
            emit_cond(cond, base, acc),
            emit_cond(then, base, acc),
            emit_cond(els, base, acc)
        ),
        Expr::Assemble { .. } => {
            "((__int128)0 /* assemble in condition unsupported */)".to_string()
        }
        Expr::Call { .. } => "((__int128)0 /* builtins in conditions unsupported */)".to_string(),
    }
}

fn cond_binop(op: BinOp, a: &str, b: &str) -> String {
    let sh = format!(
        "({{ __int128 __sb = {b}; ((__sb >= 0) && (__sb < 128)) ? (unsigned)__sb : 0u; }})"
    );
    match op {
        BinOp::Add => format!("(({a}) + ({b}))"),
        BinOp::Sub => format!("(({a}) - ({b}))"),
        BinOp::Mul => format!("(({a}) * ({b}))"),
        BinOp::Div => {
            format!("({{ __int128 __d = {b}; (__d == 0) ? (__int128)0 : (({a}) / __d); }})")
        }
        BinOp::Rem => {
            format!("({{ __int128 __d = {b}; (__d == 0) ? (__int128)0 : (({a}) % __d); }})")
        }
        BinOp::BitAnd => format!("(({a}) & ({b}))"),
        BinOp::BitOr => format!("(({a}) | ({b}))"),
        BinOp::BitXor => format!("(({a}) ^ ({b}))"),
        BinOp::Shl => format!("(({a}) << ({sh}))"),
        BinOp::Shr => format!("(({a}) >> ({sh}))"),
        BinOp::Eq => format!("((__int128)(({a}) == ({b})))"),
        BinOp::Ne => format!("((__int128)(({a}) != ({b})))"),
        BinOp::Lt => format!("((__int128)(({a}) < ({b})))"),
        BinOp::Le => format!("((__int128)(({a}) <= ({b})))"),
        BinOp::Gt => format!("((__int128)(({a}) > ({b})))"),
        BinOp::Ge => format!("((__int128)(({a}) >= ({b})))"),
        BinOp::LAnd => format!("((__int128)((({a}) != 0) && (({b}) != 0)))"),
        BinOp::LOr => format!("((__int128)((({a}) != 0) || (({b}) != 0)))"),
    }
}
