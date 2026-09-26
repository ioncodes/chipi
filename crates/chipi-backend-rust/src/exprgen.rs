//! Lowering of chipi expressions to Rust source.
//!
//! Three kinds, each mirroring a `chipi-core` evaluator exactly so generated code agrees with the
//! oracle: [`emit_value`] (unsigned `u128`, for computed operands / guards / `length` / fn bodies),
//! [`emit_cond`] (signed `i128`, for display conditions) and [`emit_prefix`] (`u64`, for the
//! prefix scan).

use crate::names::{ident, sanitize};
use chipi_core::compute::{infer_width_locals, mask128};
use chipi_core::model::Field;
use chipi_syntax::ast::{BinOp, Expr, UnOp};
use std::collections::{BTreeMap, HashMap};

/// Name-resolution scope for the `u128` value evaluator.
pub enum Scope<'a> {
    /// Operand/guard/length context: `word` and the leaf's bound fields, read off `base`.
    /// `vars` maps decode-variable names to runtime Rust expressions (e.g. a context field read
    /// as `ctx.osize`); host modes and word-level context reads are constant-folded before
    /// emission and never reach this map.
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
                    format!("({base} as u128)")
                } else if let Some(f) = fields.iter().find(|f| f.name == name) {
                    format!(
                        "(({base} as u128 >> {}) & cmask128({}))",
                        f.range.lo,
                        f.range.width()
                    )
                } else if let Some((_, expr, _)) = vars.iter().find(|(n, _, _)| n == name) {
                    format!("({expr} as u128)")
                } else {
                    "0u128".to_string()
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
                chipi_core::compute::infer_width(
                    e,
                    &chipi_core::compute::WidthEnv {
                        word_width: *window,
                        field_width: &fw,
                    },
                )
            }
            Scope::Fn { widths } => infer_width_locals(e, widths),
        }
    }
}

/// Lower an expression to a `u128`-valued Rust expression.
pub fn emit_value(e: &Expr, scope: &Scope) -> String {
    match e {
        Expr::Int(i) => format!("{}u128", i.value),
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
            let mut s = String::from("{ let mut __v: u128 = 0; ");
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
                    format!("sext128(__v & cmask128({ow}), {ow})", ow = out_width)
                }
                _ => format!("__v & cmask128({out_width})"),
            };

            s.push_str(&tail);
            s.push_str(" }");
            s
        }
        Expr::Unary { op, rhs, .. } => {
            let r = emit_value(rhs, scope);
            match op {
                UnOp::Not => format!("(!({r}))"),
                UnOp::Neg => format!("({r}).wrapping_neg()"),
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            value_binop(*op, &emit_value(lhs, scope), &emit_value(rhs, scope))
        }
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "(if ({}) != 0 {{ {} }} else {{ {} }})",
            emit_value(cond, scope),
            emit_value(then, scope),
            emit_value(els, scope)
        ),
        Expr::Call { callee, args, .. } => emit_call(&callee.text, args, scope),
    }
}

fn value_binop(op: BinOp, a: &str, b: &str) -> String {
    match op {
        BinOp::Add => format!("({a}).wrapping_add({b})"),
        BinOp::Sub => format!("({a}).wrapping_sub({b})"),
        BinOp::Mul => format!("({a}).wrapping_mul({b})"),
        BinOp::Div => format!("{{ let __d = {b}; if __d == 0 {{ 0u128 }} else {{ ({a}) / __d }} }}"),
        BinOp::Rem => format!("{{ let __d = {b}; if __d == 0 {{ 0u128 }} else {{ ({a}) % __d }} }}"),
        BinOp::BitAnd => format!("(({a}) & ({b}))"),
        BinOp::BitOr => format!("(({a}) | ({b}))"),
        BinOp::BitXor => format!("(({a}) ^ ({b}))"),
        BinOp::Shl => format!("{{ let __s = {b}; if __s < 128 {{ ({a}).checked_shl(__s as u32).unwrap_or(0) }} else {{ 0u128 }} }}"),
        BinOp::Shr => format!("{{ let __s = {b}; if __s < 128 {{ ({a}).checked_shr(__s as u32).unwrap_or(0) }} else {{ 0u128 }} }}"),
        BinOp::Eq => format!("(if ({a}) == ({b}) {{ 1u128 }} else {{ 0u128 }})"),
        BinOp::Ne => format!("(if ({a}) != ({b}) {{ 1u128 }} else {{ 0u128 }})"),
        BinOp::Lt => format!("(if ({a}) < ({b}) {{ 1u128 }} else {{ 0u128 }})"),
        BinOp::Le => format!("(if ({a}) <= ({b}) {{ 1u128 }} else {{ 0u128 }})"),
        BinOp::Gt => format!("(if ({a}) > ({b}) {{ 1u128 }} else {{ 0u128 }})"),
        BinOp::Ge => format!("(if ({a}) >= ({b}) {{ 1u128 }} else {{ 0u128 }})"),
        BinOp::LAnd => format!("(if ({a}) != 0 && ({b}) != 0 {{ 1u128 }} else {{ 0u128 }})"),
        BinOp::LOr => format!("(if ({a}) != 0 || ({b}) != 0 {{ 1u128 }} else {{ 0u128 }})"),
    }
}

fn emit_call(name: &str, args: &[Expr], scope: &Scope) -> String {
    let a = |i: usize| {
        args.get(i)
            .map(|e| emit_value(e, scope))
            .unwrap_or_else(|| "0u128".to_string())
    };
    match name {
        "concat" => {
            let mut s = String::from("{ let mut __c: u128 = 0; ");
            for arg in args {
                let w = scope.width_of(arg);
                s.push_str(&format!(
                    "__c = (__c << {w}) | (({}) & cmask128({w})); ",
                    emit_value(arg, scope)
                ));
            }

            s.push_str("__c }");
            s
        }
        "sign_extend" => format!("sext128({}, ({}) as u16)", a(0), a(1)),
        "zero_extend" => format!("(({}) & cmask128(({}) as u16))", a(0), a(1)),
        "ones" => format!("cmask128(({}) as u16)", a(0)),
        "replicate" => format!(
            "replicate128({}, ({}) as u16, ({}) as u16)",
            a(0),
            a(1),
            a(2)
        ),
        "rotate_left" => format!("rotl128({}, {}, ({}) as u16, true)", a(0), a(1), a(2)),
        "rotate_right" => format!("rotl128({}, {}, ({}) as u16, false)", a(0), a(1), a(2)),
        "bit_width" => format!("bitwidth128({})", a(0)),
        "clz" => format!(
            "(({}) as u128).saturating_sub(bitwidth128(({}) & cmask128(({}) as u16)))",
            a(1),
            a(0),
            a(1)
        ),
        "ctz" => {
            let w = args.first().map(|e| scope.width_of(e)).unwrap_or(64);
            format!("{{ let __x = {}; if __x == 0 {{ {w}u128 }} else {{ __x.trailing_zeros() as u128 }} }}", a(0))
        }
        "popcount" => format!("(({}).count_ones() as u128)", a(0)),
        "min" => format!("(({}).min({}))", a(0), a(1)),
        "max" => format!("(({}).max({}))", a(0), a(1)),
        "mask_from_range" => format!(
            "maskrange128(({}) as u16, ({}) as u16, ({}) as u16)",
            a(0),
            a(1),
            a(2)
        ),
        other => {
            let argv: Vec<String> = args.iter().map(|e| emit_value(e, scope)).collect();
            format!("fn_{}({})", sanitize(other), argv.join(", "))
        }
    }
}

/// Lower a display-arm condition to a signed `i128`-valued Rust expression. `recv` is the receiver
/// (`self` in `Display`, `inst` in `disasm_ctx`); `acc` maps an operand name to its accessor method.
/// `raw` wins over both: it maps a name straight to a runtime expression, which is how mode reads
/// (and, in the enum renderer, operand locals and the re-read `word`) resolve.
pub fn emit_cond(
    e: &Expr,
    recv: &str,
    acc: &BTreeMap<String, String>,
    raw: &BTreeMap<String, String>,
) -> String {
    match e {
        Expr::Int(i) => format!("({}i128)", i.value),
        Expr::Name(n) => {
            if let Some(r) = raw.get(&n.text) {
                // A name resolved to a raw runtime expression: a `ctx.mode(..)` read, or (in the
                // enum renderer, which has no accessor receiver) a bound operand local or the
                // re-read `word`.
                format!("(({r}) as i128)")
            } else if n.text == "word" {
                format!("({recv}.0 as i128)")
            } else {
                let m = acc
                    .get(&n.text)
                    .cloned()
                    .unwrap_or_else(|| format!("{}()", ident(&n.text)));
                format!("({recv}.{m} as i128)")
            }
        }
        Expr::Unary { op, rhs, .. } => {
            let r = emit_cond(rhs, recv, acc, raw);
            match op {
                UnOp::Not => format!("(!({r}))"),
                UnOp::Neg => format!("(({r}).wrapping_neg())"),
            }
        }
        Expr::Slice { base, hi, lo, .. } => {
            if *lo >= 128 {
                "(0i128)".to_string()
            } else {
                let mask = mask128((hi - lo + 1) as u16);
                format!(
                    "((((({}) as u128) >> {lo}) & {mask:#x}u128) as i128)",
                    emit_cond(base, recv, acc, raw)
                )
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => cond_binop(
            *op,
            &emit_cond(lhs, recv, acc, raw),
            &emit_cond(rhs, recv, acc, raw),
        ),
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "(if ({}) != 0 {{ {} }} else {{ {} }})",
            emit_cond(cond, recv, acc, raw),
            emit_cond(then, recv, acc, raw),
            emit_cond(els, recv, acc, raw)
        ),
        Expr::Assemble { .. } | Expr::Call { .. } => "(0i128)".to_string(),
    }
}

fn cond_binop(op: BinOp, a: &str, b: &str) -> String {
    // A shift amount clamped into range, built only for the shift arms below.
    let sh = || format!("(if (0..128).contains(&({b})) {{ ({b}) as u32 }} else {{ 0 }})");
    match op {
        BinOp::Add => format!("({a}).wrapping_add({b})"),
        BinOp::Sub => format!("({a}).wrapping_sub({b})"),
        BinOp::Mul => format!("({a}).wrapping_mul({b})"),
        BinOp::Div => format!(
            "{{ let __d = {b}; if __d == 0 {{ 0i128 }} else {{ ({a}).wrapping_div(__d) }} }}"
        ),
        BinOp::Rem => format!(
            "{{ let __d = {b}; if __d == 0 {{ 0i128 }} else {{ ({a}).wrapping_rem(__d) }} }}"
        ),
        BinOp::BitAnd => format!("(({a}) & ({b}))"),
        BinOp::BitOr => format!("(({a}) | ({b}))"),
        BinOp::BitXor => format!("(({a}) ^ ({b}))"),
        BinOp::Shl => format!("({a}).wrapping_shl({})", sh()),
        BinOp::Shr => format!("({a}).wrapping_shr({})", sh()),
        BinOp::Eq => format!("(if ({a}) == ({b}) {{ 1i128 }} else {{ 0i128 }})"),
        BinOp::Ne => format!("(if ({a}) != ({b}) {{ 1i128 }} else {{ 0i128 }})"),
        BinOp::Lt => format!("(if ({a}) < ({b}) {{ 1i128 }} else {{ 0i128 }})"),
        BinOp::Le => format!("(if ({a}) <= ({b}) {{ 1i128 }} else {{ 0i128 }})"),
        BinOp::Gt => format!("(if ({a}) > ({b}) {{ 1i128 }} else {{ 0i128 }})"),
        BinOp::Ge => format!("(if ({a}) >= ({b}) {{ 1i128 }} else {{ 0i128 }})"),
        BinOp::LAnd => format!("(if ({a}) != 0 && ({b}) != 0 {{ 1i128 }} else {{ 0i128 }})"),
        BinOp::LOr => format!("(if ({a}) != 0 || ({b}) != 0 {{ 1i128 }} else {{ 0i128 }})"),
    }
}

/// Lower a prefix-arm assignment expression to a `u64`-valued Rust expression (`byte` is the unit).
pub fn emit_prefix(e: &Expr) -> String {
    match e {
        Expr::Int(i) => format!("{}u64", i.value),
        Expr::Name(n) => {
            if n.text == "byte" || n.text == "word" {
                "byte".to_string()
            } else {
                "0u64".to_string()
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
                UnOp::Not => format!("!({r})"),
                UnOp::Neg => format!("({r}).wrapping_neg()"),
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            prefix_binop(*op, &emit_prefix(lhs), &emit_prefix(rhs))
        }
        Expr::Cond {
            cond, then, els, ..
        } => format!(
            "(if ({}) != 0 {{ {} }} else {{ {} }})",
            emit_prefix(cond),
            emit_prefix(then),
            emit_prefix(els)
        ),
        Expr::Assemble { .. } | Expr::Call { .. } => "0u64".to_string(),
    }
}

fn prefix_binop(op: BinOp, a: &str, b: &str) -> String {
    match op {
        BinOp::Add => format!("({a}).wrapping_add({b})"),
        BinOp::Sub => format!("({a}).wrapping_sub({b})"),
        BinOp::Mul => format!("({a}).wrapping_mul({b})"),
        BinOp::Div => format!("({a}).checked_div({b}).unwrap_or(0)"),
        BinOp::Rem => format!("({a}).checked_rem({b}).unwrap_or(0)"),
        BinOp::BitAnd => format!("(({a}) & ({b}))"),
        BinOp::BitOr => format!("(({a}) | ({b}))"),
        BinOp::BitXor => format!("(({a}) ^ ({b}))"),
        BinOp::Shl => format!("({a}).checked_shl(({b}) as u32).unwrap_or(0)"),
        BinOp::Shr => format!("({a}).checked_shr(({b}) as u32).unwrap_or(0)"),
        BinOp::Eq => format!("((({a}) == ({b})) as u64)"),
        BinOp::Ne => format!("((({a}) != ({b})) as u64)"),
        BinOp::Lt => format!("((({a}) < ({b})) as u64)"),
        BinOp::Le => format!("((({a}) <= ({b})) as u64)"),
        BinOp::Gt => format!("((({a}) > ({b})) as u64)"),
        BinOp::Ge => format!("((({a}) >= ({b})) as u64)"),
        BinOp::LAnd => format!("((({a}) != 0 && ({b}) != 0) as u64)"),
        BinOp::LOr => format!("((({a}) != 0 || ({b}) != 0) as u64)"),
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
                "builtin `{}` fell through to the user-fn fallback in the Rust backend: {out}",
                b.name
            );
        }
    }
}
