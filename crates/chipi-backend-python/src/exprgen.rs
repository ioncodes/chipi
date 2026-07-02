//! Lowering of chipi expressions to Python source.
//!
//! There are two forms, each mirroring a `chipi-core` evaluator so the generated code matches the
//! oracle exactly. [`emit_value`] is the unsigned `u128` form, kept in `[0, 2^128)` via `_M128`
//! masking; it is used for computed operands, guards, `length` and fn bodies. [`emit_cond`] is the
//! signed `i128` form (via `_s128`), used for display-arm and length conditions.

use crate::names::{comp_acc, ident, sanitize};
use chipi_core::compute::{infer_width, infer_width_locals, WidthEnv};
use chipi_core::model::{Ext, Field, Insn};
use chipi_syntax::ast::{BinOp, Expr, UnOp};
use std::collections::HashMap;

/// Name-resolution scope for the `u128` value evaluator.
pub enum Scope<'a> {
    /// Operand/guard/length context: `word` and the leaf's bound fields, read off `base`.
    /// `vars` maps decode-variable names to runtime Python expressions (e.g. a context field read
    /// as `ctx["osize"]` or a host mode as `ctx.mode("m")`); host modes and word-level context
    /// reads are constant-folded before emission and never reach this map.
    Computed {
        fields: &'a [Field],
        window: u16,
        base: &'a str,
        vars: &'a [(String, String, u16)],
    },
    /// `fn` body context: params and `let`s, bound to `v_<name>` locals with tracked widths.
    Fn { widths: HashMap<String, u16> },
}

impl Scope<'_> {
    fn resolve(&self, name: &str) -> String {
        match self {
            Scope::Computed {
                fields, base, vars, ..
            } => {
                if name == "word" {
                    format!("({base} & _M128)")
                } else if let Some(f) = fields.iter().find(|f| f.name == name) {
                    format!(
                        "(({base} >> {}) & _cmask128({}))",
                        f.range.lo,
                        f.range.width()
                    )
                } else if let Some((_, expr, _)) = vars.iter().find(|(n, _, _)| n == name) {
                    format!("(({expr}) & _M128)")
                } else {
                    "0".to_string()
                }
            }
            Scope::Fn { .. } => format!("v_{}", sanitize(name)),
        }
    }

    fn width_of(&self, e: &Expr) -> u16 {
        match self {
            Scope::Computed {
                fields,
                window,
                vars,
                ..
            } => {
                let fw = |n: &str| {
                    fields
                        .iter()
                        .find(|f| f.name == n)
                        .map(|f| f.range.width())
                        .or_else(|| vars.iter().find(|(vn, _, _)| vn == n).map(|(_, _, w)| *w))
                };
                infer_width(
                    e,
                    &WidthEnv {
                        word_width: *window,
                        field_width: &fw,
                    },
                )
            }
            Scope::Fn { widths } => infer_width_locals(e, widths),
        }
    }
}

/// Lower an expression to a `u128`-valued Python expression (result kept in `[0, 2^128)`).
pub fn emit_value(e: &Expr, scope: &Scope) -> String {
    match e {
        Expr::Int(i) => format!("{}", i.value),
        Expr::Name(n) => scope.resolve(&n.text),
        Expr::Slice { base, hi, lo, .. } => {
            format!(
                "((({}) >> {lo}) & _cmask128({}))",
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
            let terms: Vec<String> = parts
                .iter()
                .map(|p| {
                    format!(
                        "((({}) & _cmask128({})) << {})",
                        emit_value(&p.src, scope),
                        p.hi - p.lo + 1,
                        p.lo
                    )
                })
                .collect();

            let or = if terms.is_empty() {
                "0".to_string()
            } else {
                terms.join(" | ")
            };

            match ext {
                Ext::SignExtend => {
                    format!("_sext128(({or}) & _cmask128({ow}), {ow})", ow = out_width)
                }
                _ => format!("(({or}) & _cmask128({out_width}))"),
            }
        }
        Expr::Unary { op, rhs, .. } => {
            let r = emit_value(rhs, scope);

            match op {
                UnOp::Not => format!("((~({r})) & _M128)"),
                UnOp::Neg => format!("((-({r})) & _M128)"),
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            value_binop(*op, &emit_value(lhs, scope), &emit_value(rhs, scope))
        }
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "(({}) if ({}) != 0 else ({}))",
            emit_value(then, scope),
            emit_value(cond, scope),
            emit_value(els, scope)
        ),
        Expr::Call { callee, args, .. } => emit_call(&callee.text, args, scope),
    }
}

fn value_binop(op: BinOp, a: &str, b: &str) -> String {
    match op {
        BinOp::Add => format!("((({a}) + ({b})) & _M128)"),
        BinOp::Sub => format!("((({a}) - ({b})) & _M128)"),
        BinOp::Mul => format!("((({a}) * ({b})) & _M128)"),
        BinOp::Div => format!("(_div128({a}, {b}))"),
        BinOp::Rem => format!("(_rem128({a}, {b}))"),
        BinOp::BitAnd => format!("((({a}) & ({b})) & _M128)"),
        BinOp::BitOr => format!("((({a}) | ({b})) & _M128)"),
        BinOp::BitXor => format!("((({a}) ^ ({b})) & _M128)"),
        BinOp::Shl => format!("(((({a}) << ({b})) & _M128) if ({b}) < 128 else 0)"),
        BinOp::Shr => format!("((({a}) >> ({b})) if ({b}) < 128 else 0)"),
        BinOp::Eq => format!("(1 if ({a}) == ({b}) else 0)"),
        BinOp::Ne => format!("(1 if ({a}) != ({b}) else 0)"),
        BinOp::Lt => format!("(1 if ({a}) < ({b}) else 0)"),
        BinOp::Le => format!("(1 if ({a}) <= ({b}) else 0)"),
        BinOp::Gt => format!("(1 if ({a}) > ({b}) else 0)"),
        BinOp::Ge => format!("(1 if ({a}) >= ({b}) else 0)"),
        BinOp::LAnd => format!("(1 if (({a}) != 0 and ({b}) != 0) else 0)"),
        BinOp::LOr => format!("(1 if (({a}) != 0 or ({b}) != 0) else 0)"),
    }
}

fn emit_call(name: &str, args: &[Expr], scope: &Scope) -> String {
    let a = |i: usize| {
        args.get(i)
            .map(|e| emit_value(e, scope))
            .unwrap_or_else(|| "0".to_string())
    };
    match name {
        "concat" => {
            // Left fold, first arg most significant; each arg masked to its inferred width.
            let mut acc = String::from("(0)");
            for arg in args {
                let w = scope.width_of(arg);
                acc = format!(
                    "(({acc} << {w} | (({}) & _cmask128({w}))))",
                    emit_value(arg, scope)
                );
            }

            format!("({acc} & _M128)")
        }
        "sign_extend" => format!("_sext128({}, ({}) & 0xffff)", a(0), a(1)),
        "zero_extend" => format!("(({}) & _cmask128(({}) & 0xffff))", a(0), a(1)),
        "ones" => format!("_cmask128(({}) & 0xffff)", a(0)),
        "replicate" => format!(
            "_replicate128({}, ({}) & 0xffff, ({}) & 0xffff)",
            a(0),
            a(1),
            a(2)
        ),
        "rotate_left" => format!("_rotl128({}, {}, ({}) & 0xffff, True)", a(0), a(1), a(2)),
        "rotate_right" => format!("_rotl128({}, {}, ({}) & 0xffff, False)", a(0), a(1), a(2)),
        "bit_width" => format!("_bitwidth128({})", a(0)),
        "clz" => {
            // (w - bit_width(v & cmask(w))) saturating to 0.
            let w = format!("(({}) & 0xffff)", a(1));
            format!("(({w}) - _bitwidth128(({}) & _cmask128({w})) if ({w}) >= _bitwidth128(({}) & _cmask128({w})) else 0)", a(0), a(0))
        }
        "ctz" => {
            let w = args.first().map(|e| scope.width_of(e)).unwrap_or(64);
            format!("_ctz128({}, {w})", a(0))
        }
        "popcount" => format!("_popcount128({})", a(0)),
        "min" => format!("min({}, {})", a(0), a(1)),
        "max" => format!("max({}, {})", a(0), a(1)),
        "mask_from_range" => {
            format!(
                "_maskrange128(({}) & 0xffff, ({}) & 0xffff, ({}) & 0xffff)",
                a(0),
                a(1),
                a(2)
            )
        }
        other => {
            let argv: Vec<String> = args.iter().map(|e| emit_value(e, scope)).collect();

            format!("fn_{}({})", sanitize(other), argv.join(", "))
        }
    }
}

/// Lower a display-arm/length condition to a signed `i128`-valued Python expression (`_s128`
/// semantics). `inst` resolves operand names. A name in `raw` resolves to that runtime expression
/// (the contextual path routes modes through `ctx.mode(..)` and fetched operands through their
/// `__op_*` locals this way); otherwise a computed operand becomes its accessor call and anything
/// else a bound field accessor `<ident>(word)`.
pub fn emit_cond(
    e: &Expr,
    inst: &Insn,
    accmap: &std::collections::HashMap<(String, String), String>,
    raw: &std::collections::BTreeMap<String, String>,
) -> String {
    match e {
        Expr::Int(i) => format!("({})", i.value),
        Expr::Name(n) => {
            if n.text == "word" {
                "(word)".to_string()
            } else if let Some(r) = raw.get(&n.text) {
                format!("({r})")
            } else if inst.computed.iter().any(|c| c.name == n.text) {
                format!("({}(word))", comp_acc(accmap, &inst.name, &n.text))
            } else {
                format!("({}(word))", ident(&n.text))
            }
        }
        Expr::Unary { op, rhs, .. } => {
            let r = emit_cond(rhs, inst, accmap, raw);

            match op {
                UnOp::Not => format!("(_s128(~({r})))"),
                UnOp::Neg => format!("(_s128(-({r})))"),
            }
        }
        Expr::Slice { base, hi, lo, .. } => {
            if *lo >= 128 {
                "0".to_string()
            } else {
                let w = (hi.saturating_sub(*lo).saturating_add(1)).min(128) as u16;

                format!(
                    "(_s128(((({}) & _M128) >> {lo}) & {}))",
                    emit_cond(base, inst, accmap, raw),
                    width_mask(w)
                )
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => cond_binop(
            *op,
            &emit_cond(lhs, inst, accmap, raw),
            &emit_cond(rhs, inst, accmap, raw),
        ),
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "(({}) if ({}) != 0 else ({}))",
            emit_cond(then, inst, accmap, raw),
            emit_cond(cond, inst, accmap, raw),
            emit_cond(els, inst, accmap, raw)
        ),
        Expr::Assemble { .. } | Expr::Call { .. } => "0".to_string(),
    }
}

fn cond_binop(op: BinOp, a: &str, b: &str) -> String {
    let sh = format!("(({b}) if 0 <= ({b}) < 128 else 0)");
    match op {
        BinOp::Add => format!("(_s128(({a}) + ({b})))"),
        BinOp::Sub => format!("(_s128(({a}) - ({b})))"),
        BinOp::Mul => format!("(_s128(({a}) * ({b})))"),
        BinOp::Div => format!("(_cdiv128({a}, {b}))"),
        BinOp::Rem => format!("(_crem128({a}, {b}))"),
        BinOp::BitAnd => format!("(_s128(({a}) & ({b})))"),
        BinOp::BitOr => format!("(_s128(({a}) | ({b})))"),
        BinOp::BitXor => format!("(_s128(({a}) ^ ({b})))"),
        BinOp::Shl => format!("(_s128((({a}) << {sh})))"),
        BinOp::Shr => format!("(_cshr128(({a}), {sh}))"),
        BinOp::Eq => format!("(1 if ({a}) == ({b}) else 0)"),
        BinOp::Ne => format!("(1 if ({a}) != ({b}) else 0)"),
        BinOp::Lt => format!("(1 if ({a}) < ({b}) else 0)"),
        BinOp::Le => format!("(1 if ({a}) <= ({b}) else 0)"),
        BinOp::Gt => format!("(1 if ({a}) > ({b}) else 0)"),
        BinOp::Ge => format!("(1 if ({a}) >= ({b}) else 0)"),
        BinOp::LAnd => format!("(1 if ({a}) != 0 and ({b}) != 0 else 0)"),
        BinOp::LOr => format!("(1 if ({a}) != 0 or ({b}) != 0 else 0)"),
    }
}

/// A 128bit-capped width mask literal as Python source.
pub fn width_mask(w: u16) -> String {
    if w >= 128 {
        "((1 << 128) - 1)".to_string()
    } else {
        format!("{:#x}", (1u128 << w) - 1)
    }
}

/// Lower a prefix-arm assignment expression to a `u64`-valued Python expression (`byte` is the
/// unit). Mirrors the Rust backend's `emit_prefix` and the oracle's `eval_prefix`.
pub fn emit_prefix(e: &Expr) -> String {
    match e {
        Expr::Int(i) => format!("{}", i.value),
        Expr::Name(n) => {
            if n.text == "byte" || n.text == "word" {
                "byte".to_string()
            } else {
                "0".to_string()
            }
        }
        Expr::Slice { base, hi, lo, .. } => {
            let mask = if hi - lo + 1 >= 64 {
                u64::MAX
            } else {
                (1u64 << (hi - lo + 1)) - 1
            };
            format!("((({}) >> {lo}) & {mask:#x})", emit_prefix(base))
        }
        Expr::Unary { op, rhs, .. } => {
            let r = emit_prefix(rhs);
            match op {
                UnOp::Not => format!("((~({r})) & _M64)"),
                UnOp::Neg => format!("((-({r})) & _M64)"),
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            prefix_binop(*op, &emit_prefix(lhs), &emit_prefix(rhs))
        }
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "(({}) if ({}) != 0 else ({}))",
            emit_prefix(then),
            emit_prefix(cond),
            emit_prefix(els)
        ),
        Expr::Assemble { .. } | Expr::Call { .. } => "0".to_string(),
    }
}

fn prefix_binop(op: BinOp, a: &str, b: &str) -> String {
    match op {
        BinOp::Add => format!("((({a}) + ({b})) & _M64)"),
        BinOp::Sub => format!("((({a}) - ({b})) & _M64)"),
        BinOp::Mul => format!("((({a}) * ({b})) & _M64)"),
        BinOp::Div => format!("(0 if ({b}) == 0 else (({a}) // ({b})))"),
        BinOp::Rem => format!("(0 if ({b}) == 0 else (({a}) % ({b})))"),
        BinOp::BitAnd => format!("(({a}) & ({b}))"),
        BinOp::BitOr => format!("(({a}) | ({b}))"),
        BinOp::BitXor => format!("(({a}) ^ ({b}))"),
        BinOp::Shl => format!("(((({a}) << ({b})) & _M64) if ({b}) < 64 else 0)"),
        BinOp::Shr => format!("((({a}) >> ({b})) if ({b}) < 64 else 0)"),
        BinOp::Eq => format!("(1 if ({a}) == ({b}) else 0)"),
        BinOp::Ne => format!("(1 if ({a}) != ({b}) else 0)"),
        BinOp::Lt => format!("(1 if ({a}) < ({b}) else 0)"),
        BinOp::Le => format!("(1 if ({a}) <= ({b}) else 0)"),
        BinOp::Gt => format!("(1 if ({a}) > ({b}) else 0)"),
        BinOp::Ge => format!("(1 if ({a}) >= ({b}) else 0)"),
        BinOp::LAnd => format!("(1 if ({a}) != 0 and ({b}) != 0 else 0)"),
        BinOp::LOr => format!("(1 if ({a}) != 0 or ({b}) != 0 else 0)"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chipi_core::compute::BUILTINS;
    use chipi_syntax::ast::IntLit;
    use chipi_syntax::Span;

    fn int(v: u128) -> Expr {
        Expr::Int(IntLit {
            value: v,
            width_hint: Some(4),
            span: Span::at(0),
        })
    }

    /// Sync test against the canonical table in `chipi_core::compute`: every builtin must
    /// have an `emit_call` arm here. The unknown-name fallback emits a user-fn call
    /// (`fn_<name>(...)`), so any builtin producing that shape was forgotten.
    #[test]
    fn every_builtin_has_an_emit_call_arm() {
        let scope = Scope::Computed {
            fields: &[],
            window: 32,
            base: "word",
            vars: &[],
        };

        for b in BUILTINS {
            let args: Vec<Expr> = (0..b.min_args).map(|_| int(1)).collect();
            let out = emit_call(b.name, &args, &scope);
            assert!(
                !out.starts_with("fn_"),
                "builtin `{}` fell through to the user-fn fallback in the Python backend: {out}",
                b.name
            );
        }
    }
}
