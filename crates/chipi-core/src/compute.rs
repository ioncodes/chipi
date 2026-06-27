//! The portable computation layer: a small, pure, total evaluator over fixed-width
//! bit vectors carried as `u128`, plus a static width-inference pass.
//!
//! It evaluates computed operands, `when` guards, `assemble` scatters, user `fn` bodies, `length`
//! conditions and prefix assignments. Every backend mirrors these rules so the oracle and the
//! generated code give the same results. Evaluation never panics: divide or modulo by zero is 0,
//! an out-of-range shift is 0, an unknown name or call is 0. The consumer applies the value-width
//! clamp at the operand boundary, not inside this evaluator.

use crate::model::Func;
use chipi_syntax::ast::{AssemblePart, BinOp, Expr, Ext, UnOp};

/// The builtin function names recognised by the computation layer.
pub const BUILTINS: &[&str] = &[
    "concat",
    "sign_extend",
    "zero_extend",
    "ones",
    "replicate",
    "rotate_left",
    "rotate_right",
    "bit_width",
    "clz",
    "ctz",
    "popcount",
    "min",
    "max",
    "mask_from_range",
];

pub fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(&name)
}

/// Evaluation environment: the `word`, per-name value and width lookups, plus the `fn` table.
pub struct Env<'a> {
    pub word: u128,
    pub word_width: u16,
    pub field: &'a dyn Fn(&str) -> Option<u128>,
    pub width: &'a dyn Fn(&str) -> Option<u16>,
    pub fns: &'a [Func],
}

impl Env<'_> {
    fn widths(&self) -> WidthEnv<'_> {
        WidthEnv {
            word_width: self.word_width,
            field_width: self.width,
        }
    }
}

/// Width-inference environment: width of `word` plus declared field/param widths.
pub struct WidthEnv<'a> {
    pub word_width: u16,
    pub field_width: &'a dyn Fn(&str) -> Option<u16>,
}

/// Mask of the low `width` bits.
pub fn mask128(width: u16) -> u128 {
    if width >= 128 {
        u128::MAX
    } else {
        (1u128 << width) - 1
    }
}

/// Mask of the low `width` bits, capped at 64 bits.
pub fn mask_u64(width: u16) -> u64 {
    if width >= 64 {
        u64::MAX
    } else {
        (1u64 << width) - 1
    }
}

/// Sign-extend the low `n` bits of `v` into a 128bit two's-complement pattern.
pub fn sext128(v: u128, n: u16) -> u128 {
    if n == 0 || n >= 128 {
        return v;
    }
    let m = mask128(n);
    let x = v & m;
    if (x >> (n - 1)) & 1 == 1 {
        x | !m
    } else {
        x
    }
}

/// Evaluate an expression to its `u128` bit pattern.
pub fn eval_value(e: &Expr, env: &Env) -> u128 {
    match e {
        Expr::Int(i) => i.value,
        Expr::Name(n) => {
            if n.text == "word" {
                env.word
            } else {
                (env.field)(&n.text).unwrap_or(0)
            }
        }
        Expr::Slice { base, hi, lo, .. } => {
            let v = eval_value(base, env);
            (v >> *lo) & mask128((hi - lo + 1) as u16)
        }
        Expr::Assemble {
            out_width,
            parts,
            ext,
            ..
        } => eval_assemble(*out_width as u16, parts, *ext, env),
        Expr::Unary { op, rhs, .. } => {
            let v = eval_value(rhs, env);
            match op {
                // `~` is NOT width-masked here; the consumer applies the final value-width clamp.
                UnOp::Not => !v,
                UnOp::Neg => v.wrapping_neg(),
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => binop(*op, eval_value(lhs, env), eval_value(rhs, env)),
        Expr::Cond {
            cond, then, els, ..
        } => {
            if eval_value(cond, env) != 0 {
                eval_value(then, env)
            } else {
                eval_value(els, env)
            }
        }
        Expr::Call { callee, args, .. } => {
            if is_builtin(&callee.text) {
                builtin(&callee.text, args, env)
            } else if let Some(f) = env.fns.iter().find(|f| f.name == callee.text) {
                let argv: Vec<u128> = args.iter().map(|a| eval_value(a, env)).collect();
                eval_fn(f, &argv, env.fns)
            } else {
                0
            }
        }
    }
}

/// Evaluate a user `fn`: bind params (masked to width), run `let`s in order, return masked.
pub fn eval_fn(f: &Func, args: &[u128], fns: &[Func]) -> u128 {
    // (name, value, width); a let's local width is pinned to 64 (matches the reference model).
    let mut locals: Vec<(String, u128, u16)> = f
        .params
        .iter()
        .zip(args)
        .map(|((n, ty), v)| (n.clone(), *v & mask128(ty.width()), ty.width()))
        .collect();

    let run = |e: &Expr, locals: &[(String, u128, u16)]| -> u128 {
        let env = Env {
            word: 0,
            word_width: 64,
            field: &|n: &str| locals.iter().find(|(x, _, _)| x == n).map(|(_, v, _)| *v),
            width: &|n: &str| locals.iter().find(|(x, _, _)| x == n).map(|(_, _, w)| *w),
            fns,
        };
        eval_value(e, &env)
    };

    // run `let`s in order, each visible to later lets and the return expression.
    for (name, expr) in &f.lets {
        let v = run(expr, &locals);
        locals.push((name.clone(), v, 64));
    }

    run(&f.ret_expr, &locals) & mask128(f.ret.width())
}

fn binop(op: BinOp, a: u128, b: u128) -> u128 {
    let yes_no = |x: bool| u128::from(x);
    let shamt = |b: u128| if b < 128 { b as u32 } else { 128 };
    match op {
        BinOp::Add => a.wrapping_add(b),
        BinOp::Sub => a.wrapping_sub(b),
        BinOp::Mul => a.wrapping_mul(b),
        BinOp::Div => a.checked_div(b).unwrap_or(0),
        BinOp::Rem => a.checked_rem(b).unwrap_or(0),
        BinOp::BitAnd => a & b,
        BinOp::BitOr => a | b,
        BinOp::BitXor => a ^ b,
        BinOp::Shl => a.checked_shl(shamt(b)).unwrap_or(0),
        BinOp::Shr => a.checked_shr(shamt(b)).unwrap_or(0),
        BinOp::Eq => yes_no(a == b),
        BinOp::Ne => yes_no(a != b),
        BinOp::Lt => yes_no(a < b),
        BinOp::Le => yes_no(a <= b),
        BinOp::Gt => yes_no(a > b),
        BinOp::Ge => yes_no(a >= b),
        BinOp::LAnd => yes_no(a != 0 && b != 0),
        BinOp::LOr => yes_no(a != 0 || b != 0),
    }
}

fn eval_assemble(out_width: u16, parts: &[AssemblePart], ext: Ext, env: &Env) -> u128 {
    let mut v = 0u128;
    for p in parts {
        let w = (p.hi - p.lo + 1) as u16;
        let chunk = eval_value(&p.src, env) & mask128(w);
        v |= chunk << p.lo;
    }
    v &= mask128(out_width);

    match ext {
        Ext::SignExtend => sext128(v, out_width),
        Ext::None | Ext::ZeroExtend => v,
    }
}

fn builtin(name: &str, args: &[Expr], env: &Env) -> u128 {
    let arg = |i: usize| args.get(i).map(|e| eval_value(e, env)).unwrap_or(0);
    match name {
        "concat" => {
            // First argument is most significant; each contributes its inferred width.
            let we = env.widths();
            let mut acc = 0u128;
            for a in args {
                let w = infer_width(a, &we);
                acc = (acc << w) | (eval_value(a, env) & mask128(w));
            }
            acc
        }
        "sign_extend" => sext128(arg(0), arg(1) as u16),
        "zero_extend" => arg(0) & mask128(arg(1) as u16),
        "ones" => mask128(arg(0) as u16),
        "replicate" => replicate(arg(0), arg(1) as u16, arg(2) as u16),
        "rotate_left" => rotate(arg(0), arg(1), arg(2) as u16, true),
        "rotate_right" => rotate(arg(0), arg(1), arg(2) as u16, false),
        "bit_width" => bit_width(arg(0)),
        "clz" => {
            let w = arg(1) as u16;
            (w as u128).saturating_sub(bit_width(arg(0) & mask128(w)))
        }
        "ctz" => {
            let x = arg(0);
            if x == 0 {
                args.first()
                    .map(|e| infer_width(e, &env.widths()) as u128)
                    .unwrap_or(64)
            } else {
                x.trailing_zeros() as u128
            }
        }
        "popcount" => arg(0).count_ones() as u128,
        "min" => arg(0).min(arg(1)),
        "max" => arg(0).max(arg(1)),
        "mask_from_range" => mask_from_range(arg(0) as u16, arg(1) as u16, arg(2) as u16),
        _ => 0,
    }
}

fn bit_width(v: u128) -> u128 {
    (128 - v.leading_zeros()) as u128
}

fn replicate(v: u128, elem: u16, total: u16) -> u128 {
    if elem == 0 || total == 0 {
        return 0;
    }
    let chunk = v & mask128(elem);

    let mut out = 0u128;
    let mut shift = 0u16;
    while shift < total {
        out |= chunk << shift;
        shift += elem;
    }

    out & mask128(total)
}

fn rotate(v: u128, n: u128, w: u16, left: bool) -> u128 {
    if w == 0 {
        return 0;
    }
    let m = mask128(w);
    let v = v & m;
    let n = (n % w as u128) as u16;
    if n == 0 {
        return v;
    }
    let (l, r) = if left { (n, w - n) } else { (w - n, n) };
    ((v << l) | (v >> r)) & m
}

fn mask_from_range(b: u16, e: u16, w: u16) -> u128 {
    let mut out = 0u128;
    if b <= e {
        for bit in b..=e.min(w.saturating_sub(1)) {
            out |= 1u128 << bit;
        }
    } else {
        for bit in b..w {
            out |= 1u128 << bit;
        }
        for bit in 0..=e {
            out |= 1u128 << bit;
        }
    }
    out & mask128(w)
}

/// Infer a width over a leaf's bound `fields`, with `word` sized to `window`. Shared by every
/// backend's value-expression width pass so generated code agrees with the oracle.
pub fn infer_width_fields(e: &Expr, fields: &[crate::model::Field], window: u16) -> u16 {
    let fw = |n: &str| fields.iter().find(|f| f.name == n).map(|f| f.range.width());
    infer_width(
        e,
        &WidthEnv {
            word_width: window,
            field_width: &fw,
        },
    )
}

/// Infer a width inside a `fn` body, where names resolve to tracked local `widths` and `word` is 64.
pub fn infer_width_locals(e: &Expr, widths: &std::collections::HashMap<String, u16>) -> u16 {
    let fw = |n: &str| widths.get(n).copied();
    infer_width(
        e,
        &WidthEnv {
            word_width: 64,
            field_width: &fw,
        },
    )
}

/// Infer a static bit width for an expression (used by `~`, `concat`, `ctz` and the operand mask).
pub fn infer_width(e: &Expr, env: &WidthEnv) -> u16 {
    match e {
        Expr::Int(i) => i
            .width_hint
            .unwrap_or_else(|| (128 - i.value.leading_zeros()).max(1) as u16),
        Expr::Name(n) => {
            if n.text == "word" {
                env.word_width
            } else {
                (env.field_width)(&n.text).unwrap_or(64)
            }
        }
        Expr::Slice { hi, lo, .. } => (hi - lo + 1) as u16,
        Expr::Assemble { out_width, .. } => *out_width as u16,
        Expr::Unary { rhs, .. } => infer_width(rhs, env),
        Expr::Binary { op, lhs, rhs, .. } => match op {
            BinOp::Eq
            | BinOp::Ne
            | BinOp::Lt
            | BinOp::Le
            | BinOp::Gt
            | BinOp::Ge
            | BinOp::LAnd
            | BinOp::LOr => 1,
            _ => infer_width(lhs, env).max(infer_width(rhs, env)),
        },
        Expr::Cond { then, els, .. } => infer_width(then, env).max(infer_width(els, env)),
        Expr::Call { callee, args, .. } => match callee.text.as_str() {
            "concat" => args.iter().map(|a| infer_width(a, env)).sum(),
            "ones" | "zero_extend" | "sign_extend" => 64,
            "bit_width" | "clz" | "ctz" | "popcount" => 8,
            "replicate" | "rotate_left" | "rotate_right" | "mask_from_range" => 64,
            "min" | "max" => args.iter().map(|a| infer_width(a, env)).max().unwrap_or(1),
            _ => 64,
        },
    }
}
