//! The reference decode evaluator, the oracle.
//!
//! It classifies a word through the same lowered decode tree the backends emit. It extracts the
//! matched leaf's operands, applies the transform pipeline, evaluates computed operands and guards,
//! and renders the disassembly. Every backend is validated against this evaluator, so the
//! arithmetic here matches the generated code exactly.

use crate::compute::{eval_value, mask128, mask_u64 as mask, sext128, Env};
use crate::model::*;
use crate::render::{self, FmtSpec, Seg};
use crate::tree::{Residual, Slot, Tree};
use chipi_syntax::ast::{BinOp, Expr, UnOp};

#[derive(Clone, Debug)]
pub struct FieldValue {
    pub name: String,
    pub range: BitRange,
    pub raw: u64,
    pub value: i128,
    pub rendered: String,
    pub ty: FieldTy,
}

#[derive(Clone, Debug)]
pub struct Decoded {
    pub opcode_id: usize,
    pub opcode_name: String,
    pub instr_index: Option<usize>,
    pub len_bytes: u8,
    pub path: Vec<String>,
    pub fields: Vec<FieldValue>,
    pub disasm: Option<String>,
    /// Final decode-local context after a prefix scan (empty without `context`).
    pub context: Vec<(String, u64)>,
    /// Leading prefix units consumed before the opcode window (0 for fixed-width).
    pub prefix_len: u8,
}

impl Decoded {
    pub fn is_valid(&self) -> bool {
        self.opcode_id != 0
    }
}

/// Decode a fetched `word` with the default-mode tree.
pub fn decode(isa: &Isa, word: u64) -> Decoded {
    decode_with(isa, &isa.tree, word)
}

/// Decode with the tree for mode combination `combo`.
pub fn decode_mode(isa: &Isa, combo: usize, word: u64) -> Decoded {
    let tree = isa.mode_trees.get(combo).unwrap_or(&isa.tree);
    decode_with(isa, tree, word)
}

fn decode_with(isa: &Isa, tree: &Tree, word: u64) -> Decoded {
    let mut path = Vec::new();

    let p_lo = tree.primary.range.lo;
    let p_val = (word >> p_lo) & mask(tree.primary.range.width());
    path.push(format!(
        "{} = {p_val:#x}  [primary, {}]",
        tree.primary.name,
        tree.primary_lowering.label()
    ));

    let mut opcode_id = match tree.slots.get(p_val as usize) {
        Some(Slot::Leaf(id)) => *id,
        Some(Slot::Invalid) | None => 0,
        Some(Slot::Residual(ri)) => match &tree.residuals[*ri] {
            Residual::Keyed {
                key,
                lowering,
                arms,
                default,
            } => {
                let k = (word >> key.range.lo) & mask(key.range.width());
                let id = arms
                    .iter()
                    .find(|(kk, _)| *kk == k)
                    .map(|(_, id)| *id)
                    .unwrap_or(*default);
                path.push(format!(
                    "{} = {k:#x}  [residual, {}]",
                    key.name,
                    lowering.label()
                ));
                id
            }
            Residual::Sparse { lowering, arms } => {
                let id = arms
                    .iter()
                    .find(|a| (word & a.mask) == a.val)
                    .map(|a| a.opcode)
                    .unwrap_or(0);
                let name = if id == 0 {
                    "invalid"
                } else {
                    &tree.opcodes[id].name
                };
                path.push(format!(
                    "[residual verify chain, {}] -> {name}",
                    lowering.label()
                ));
                id
            }
        },
    };

    // leaf guard: a guard evaluating to zero makes the encoding reserved (invalid).
    if opcode_id != 0 {
        let inst = &isa.instrs[tree.opcodes[opcode_id].instr];
        if let Some(g) = &inst.guard {
            if eval_in_word(isa, g, inst, word, None) == 0 {
                opcode_id = 0;
                path.push("guard failed -> reserved".to_string());
            }
        }
    }

    if opcode_id == 0 {
        path.push("-> decode_invalid".to_string());
        let len_bytes = window_bits(isa, word, isa.max_len_bytes as u16 * 8).div_ceil(8) as u8;
        return Decoded {
            opcode_id: 0,
            opcode_name: "Invalid".to_string(),
            instr_index: None,
            len_bytes,
            path,
            fields: Vec::new(),
            disasm: None,
            context: Vec::new(),
            prefix_len: 0,
        };
    }

    let instr_index = tree.opcodes[opcode_id].instr;
    let inst = &isa.instrs[instr_index];
    path.push(format!("-> {}", inst.name));

    // bound fields: extract raw bits and run the transform pipeline.
    let mut fields: Vec<FieldValue> = inst
        .fields
        .iter()
        .map(|f| {
            let raw = (word >> f.range.lo) & mask(f.range.width());
            let value = apply_xforms(raw, &f.ty);
            FieldValue {
                name: f.name.clone(),
                range: f.range,
                raw,
                value,
                rendered: render::render_field(value, &f.ty, &FmtSpec::default()),
                ty: f.ty.clone(),
            }
        })
        .collect();

    // computed operands: evaluated over the word plus the already-decoded bound fields.
    for c in &inst.computed {
        let raw = eval_in_word(isa, &c.expr, inst, word, Some(&fields));
        let vw = c.ty.value_width;

        let value: i128 = if c.ty.signed {
            sext128(raw as u128, vw) as i128
        } else {
            ((raw as u128) & mask128(vw)) as i128
        };
        let rendered = render::render_field(value, &c.ty, &FmtSpec::default());

        fields.push(FieldValue {
            name: c.name.clone(),
            range: BitRange {
                lo: 0,
                hi: vw.saturating_sub(1),
            },
            raw,
            value,
            rendered,
            ty: c.ty.clone(),
        });
    }

    let disasm = render_disasm(isa, inst, &fields, word);
    let len_bits = window_bits(isa, word, isa.window_bits());

    Decoded {
        opcode_id,
        opcode_name: inst.name.clone(),
        instr_index: Some(instr_index),
        len_bytes: len_bits.div_ceil(8) as u8,
        path,
        fields,
        disasm: Some(disasm),
        context: Vec::new(),
        prefix_len: 0,
    }
}

/// Evaluate `e` over `word` in an instruction context (raw field bits plus the word) as a `u64`.
/// `current` supplies already-decoded operand raws, for computed operands that read earlier fields.
/// With `None`, only the leaf's bound fields are visible, which the guard uses.
fn eval_in_word(
    isa: &Isa,
    e: &Expr,
    inst: &Insn,
    word: u64,
    current: Option<&[FieldValue]>,
) -> u64 {
    // Resolve names by searching the leaf's fields (or the already-decoded operand
    // raws) on demand, so no per-call lookup tables are allocated. `decode` runs this
    // tens of thousands of times per roundtrip check.
    let field = |n: &str| -> Option<u128> {
        match current {
            Some(fs) => fs.iter().find(|f| f.name == n).map(|f| f.raw as u128),
            None => inst
                .fields
                .iter()
                .find(|f| f.name == n)
                .map(|f| ((word >> f.range.lo) & mask(f.range.width())) as u128),
        }
    };
    let width = |n: &str| {
        inst.fields
            .iter()
            .find(|f| f.name == n)
            .map(|f| f.range.width())
    };

    let env = Env {
        word: word as u128,
        word_width: isa.window_bits(),
        field: &field,
        width: &width,
        fns: &isa.fns,
    };

    eval_value(e, &env) as u64
}

/// Decode from a byte stream: run the prefix scan (consuming leading units and mutating
/// decode-local `context`), then decode the post-prefix window.
pub fn decode_stream(isa: &Isa, bytes: &[u8]) -> Decoded {
    let mut ctx: Vec<(String, u64)> = isa
        .decoder
        .context
        .iter()
        .map(|c| (c.name.clone(), c.default))
        .collect();
    let mut cursor = 0usize;

    if let Some(pfx) = &isa.prefix {
        while cursor < bytes.len() {
            let byte = bytes[cursor];
            let Some(arm) = pfx.arm_for(byte) else { break };
            for (name, e) in &arm.assigns {
                let v = eval_prefix(e, byte as u64);
                let width = isa
                    .decoder
                    .context
                    .iter()
                    .find(|c| &c.name == name)
                    .map(|c| c.width)
                    .unwrap_or(64);
                let masked = v & mask(width);
                if let Some(slot) = ctx.iter_mut().find(|(n, _)| n == name) {
                    slot.1 = masked;
                }
            }
            match arm.term {
                PrefixTerm::Continue => cursor += 1,
                PrefixTerm::Finish => {
                    cursor += 1;
                    break;
                }
                PrefixTerm::Done => break,
            }
        }
    }

    let word = read_window(isa, bytes, cursor);

    let mut d = decode_with(isa, &isa.tree, word);
    d.prefix_len = cursor as u8;
    d.len_bytes = (cursor as u8).saturating_add(d.len_bytes);
    d.context = ctx;
    d
}

fn window_bits(isa: &Isa, word: u64, fallback: u16) -> u16 {
    match &isa.length {
        Some(l) => l.bits_for(word),
        None => fallback,
    }
}

/// Assemble `nbytes` into a word using the decoder's byte order. `get(i)` supplies the i-th byte.
fn assemble_word(nbytes: usize, endian: Endian, get: impl Fn(usize) -> u8) -> u64 {
    let mut w = 0u64;
    for i in 0..nbytes {
        let b = get(i) as u64;
        match endian {
            Endian::Little => w |= b << (8 * i),
            Endian::Big => w = (w << 8) | b,
        }
    }
    w
}

fn read_window(isa: &Isa, bytes: &[u8], cursor: usize) -> u64 {
    let nbytes = (isa.window_bits() as usize).div_ceil(8);
    assemble_word(nbytes, isa.decoder.endian, |i| {
        bytes.get(cursor + i).copied().unwrap_or(0)
    })
}

fn eval_prefix(e: &Expr, byte: u64) -> u64 {
    let env = Env {
        word: byte as u128,
        word_width: 8,
        field: &|n: &str| (n == "byte" || n == "word").then_some(byte as u128),
        width: &|n: &str| (n == "byte" || n == "word").then_some(8),
        fns: &[],
    };
    eval_value(e, &env) as u64
}

/// The first display arm whose guard passes (an unconditional arm always passes).
fn first_arm<'a>(inst: &'a Insn, fields: &[FieldValue], word: u64) -> Option<&'a DisplayArm> {
    inst.display.iter().find(|arm| match &arm.cond {
        None => true,
        Some(e) => eval_cond(e, fields, word) != 0,
    })
}

fn render_disasm(isa: &Isa, inst: &Insn, fields: &[FieldValue], word: u64) -> String {
    match first_arm(inst, fields, word) {
        Some(arm) => render_arm(isa, &arm.segs, fields, word, None),
        None => inst.name.clone(),
    }
}

fn render_arm(
    isa: &Isa,
    segs: &[Seg],
    fields: &[FieldValue],
    word: u64,
    ctx: Option<&dyn DisasmCtx>,
) -> String {
    let mut out = String::new();
    for seg in segs {
        match seg {
            Seg::Lit(s) => out.push_str(s),
            Seg::Cond { cond, then, els } => {
                let branch = if eval_cond(cond, fields, word) != 0 {
                    then
                } else {
                    els
                };
                out.push_str(&render_arm(isa, branch, fields, word, ctx));
            }
            Seg::SubField { field, output } => {
                out.push_str(&render_sub(isa, fields, field, output, ctx))
            }
            Seg::Field { name, fmt } => {
                let Some(f) = fields.iter().find(|f| &f.name == name) else {
                    out.push_str("{?}");
                    continue;
                };
                match (fmt.sym, ctx) {
                    (true, Some(ctx)) => match ctx.symbol(f.value as u64) {
                        Some((n, 0)) => out.push_str(&n),
                        Some((n, off)) => {
                            out.push_str(&n);
                            out.push_str(&format!("+{off:#x}"));
                        }
                        None => out.push_str(&render::render_field(f.value, &f.ty, fmt)),
                    },
                    _ => out.push_str(&render::render_field(f.value, &f.ty, fmt)),
                }
            }
        }
    }
    out
}

/// Render `{field.output}`: decode the bound sub-field's value through its subdecoder and render the
/// matching arm's named output. An unmatched value (or unknown output) renders as empty.
fn render_sub(
    isa: &Isa,
    fields: &[FieldValue],
    field: &str,
    output: &str,
    ctx: Option<&dyn DisasmCtx>,
) -> String {
    let Some(fv) = fields.iter().find(|f| f.name == field) else {
        return "{?}".to_string();
    };
    let Some(sdn) = fv.ty.subdecoder.as_ref() else {
        return "{?}".to_string();
    };
    let Some(sd) = isa.subdecoders.iter().find(|s| &s.name == sdn) else {
        return "{?}".to_string();
    };
    let value = fv.raw;
    let Some(arm) = sd.arm_for(value) else {
        return String::new();
    };
    let Some((_, segs)) = arm.outputs.iter().find(|(n, _)| n == output) else {
        return String::new();
    };

    let sub_fields: Vec<FieldValue> = arm
        .fields
        .iter()
        .map(|f| {
            let raw = (value >> f.range.lo) & mask(f.range.width());
            let v = apply_xforms(raw, &f.ty);
            FieldValue {
                name: f.name.clone(),
                range: f.range,
                raw,
                value: v,
                rendered: render::render_field(v, &f.ty, &FmtSpec::default()),
                ty: f.ty.clone(),
            }
        })
        .collect();

    render_arm(isa, segs, &sub_fields, value, ctx)
}

fn pick_arm<'a>(inst: &'a Insn, fields: &[FieldValue], word: u64) -> &'a [Seg] {
    first_arm(inst, fields, word)
        .or_else(|| inst.display.last())
        .map(|a| a.segs.as_slice())
        .unwrap_or(&[])
}

// ---------------------------------------------------------------- contextual disassembly

/// The host context a contextual disassembler needs: stream bytes, symbol resolution and modes.
pub trait DisasmCtx {
    fn read_u8(&self, addr: u64) -> u8;
    fn symbol(&self, _addr: u64) -> Option<(String, u64)> {
        None
    }
    fn mode(&self, _name: &str) -> u64 {
        0
    }
}

/// The bit width of a `fetch(N)` computed-operand expression.
pub fn fetch_width(e: &Expr) -> Option<u16> {
    if let Expr::Call { callee, args, .. } = e {
        if callee.text == "fetch" && args.len() == 1 {
            if let Expr::Int(i) = &args[0] {
                return Some(i.value as u16);
            }
        }
    }
    None
}

/// Contextual disassembly: classify the opcode at `pc`, fetch `fetch(N)` operands from the stream,
/// render the display (resolving `:sym` operands) and return `(text, total length in bytes)`.
pub fn disasm_ctx(isa: &Isa, pc: u64, ctx: &dyn DisasmCtx) -> (String, u8) {
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let word = read_bytes(ctx, pc, wb, isa.decoder.endian);
    let d = decode(isa, word);
    let Some(idx) = d.instr_index else {
        return ("(invalid)".to_string(), wb as u8);
    };
    let inst = &isa.instrs[idx];

    let mut fields = d.fields.clone();
    let mut cursor = pc + wb as u64;

    // fetch trailing `fetch(N)` operands from the stream past the opcode window.
    for c in &inst.computed {
        if let Some(bits) = fetch_width(&c.expr) {
            let nb = (bits as usize).div_ceil(8);
            let raw = read_bytes(ctx, cursor, nb, isa.decoder.endian);
            cursor += nb as u64;
            let value = apply_xforms(raw, &c.ty);
            if let Some(f) = fields.iter_mut().find(|f| f.name == c.name) {
                f.value = value;
                f.raw = raw;
            }
        }
    }
    let total = (cursor - pc) as u8;

    let text = render_arm(isa, pick_arm(inst, &fields, word), &fields, word, Some(ctx));
    (text, total)
}

fn read_bytes(ctx: &dyn DisasmCtx, addr: u64, nbytes: usize, endian: Endian) -> u64 {
    assemble_word(nbytes, endian, |i| ctx.read_u8(addr + i as u64))
}

/// Apply the transform pipeline to a raw field value. Matches the generated accessor exactly:
/// 64bit (un)signed arithmetic, with a final `as iN/uN` truncation to the return-type width.
pub fn apply_xforms(raw: u64, ty: &FieldTy) -> i128 {
    let ret_bits = if ty.value_width <= 32 { 32 } else { 64 };
    let mut cur: u64 = raw;
    let mut signed: Option<i64> = None;

    for x in &ty.xforms {
        match (signed, *x) {
            (None, Xform::ShiftLeft(n)) => cur = cur.wrapping_shl(n as u32),
            (None, Xform::ShiftRight(n)) => cur = cur.wrapping_shr(n as u32),
            (None, Xform::ZeroExtend(n)) => cur &= mask(n),
            (None, Xform::SignExtend(n)) => signed = Some(sext64(cur, n)),
            (None, Xform::RotateLeft(k, w)) => cur = rotl64(cur, k as u32, w as u32),
            (None, Xform::RotateRight(k, w)) => cur = rotr64(cur, k as u32, w as u32),
            (Some(sv), Xform::ShiftLeft(n)) => signed = Some(sv.wrapping_shl(n as u32)),
            (Some(sv), Xform::ShiftRight(n)) => signed = Some(sv.wrapping_shr(n as u32)),
            (Some(sv), Xform::ZeroExtend(n)) => {
                cur = (sv as u64) & mask(n);
                signed = None;
            }
            (Some(sv), Xform::SignExtend(n)) => signed = Some(sext64(sv as u64, n)),
            (Some(sv), Xform::RotateLeft(k, w)) => {
                signed = Some(rotl64(sv as u64, k as u32, w as u32) as i64)
            }
            (Some(sv), Xform::RotateRight(k, w)) => {
                signed = Some(rotr64(sv as u64, k as u32, w as u32) as i64)
            }
        }
    }

    if ty.signed {
        let v = signed.unwrap_or(cur as i64);
        if ret_bits == 32 {
            (v as i32) as i128
        } else {
            v as i128
        }
    } else {
        let v = signed.map(|s| s as u64).unwrap_or(cur);
        if ret_bits == 32 {
            (v as u32) as i128
        } else {
            v as i128
        }
    }
}

/// The signed (i128) evaluator used for display-arm conditions and `length` conditions. Distinct
/// from the unsigned `compute::eval_value` used for operands/guards/fn bodies.
pub(crate) fn eval_cond(e: &Expr, fields: &[FieldValue], word: u64) -> i128 {
    match e {
        Expr::Int(i) => i.value as i128,
        Expr::Name(n) => {
            if n.text == "word" {
                word as i128
            } else {
                fields
                    .iter()
                    .find(|f| f.name == n.text)
                    .map(|f| f.value)
                    .unwrap_or(0)
            }
        }
        Expr::Unary { op, rhs, .. } => {
            let v = eval_cond(rhs, fields, word);
            match op {
                UnOp::Not => !v,
                UnOp::Neg => v.wrapping_neg(),
            }
        }
        Expr::Slice { base, hi, lo, .. } => {
            let v = eval_cond(base, fields, word) as u128;
            if *lo >= 128 {
                0
            } else {
                let w = hi.saturating_sub(*lo).saturating_add(1).min(128) as u16;
                ((v >> *lo) & mask128(w)) as i128
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            let a = eval_cond(lhs, fields, word);
            let b = eval_cond(rhs, fields, word);

            let yn = |x: bool| if x { 1 } else { 0 };
            let sh = |b: i128| if (0..128).contains(&b) { b as u32 } else { 0 };

            match op {
                BinOp::Add => a.wrapping_add(b),
                BinOp::Sub => a.wrapping_sub(b),
                BinOp::Mul => a.wrapping_mul(b),
                BinOp::Div => {
                    if b == 0 {
                        0
                    } else {
                        a.wrapping_div(b)
                    }
                }
                BinOp::Rem => {
                    if b == 0 {
                        0
                    } else {
                        a.wrapping_rem(b)
                    }
                }
                BinOp::BitAnd => a & b,
                BinOp::BitOr => a | b,
                BinOp::BitXor => a ^ b,
                BinOp::Shl => a.wrapping_shl(sh(b)),
                BinOp::Shr => a.wrapping_shr(sh(b)),
                BinOp::Eq => yn(a == b),
                BinOp::Ne => yn(a != b),
                BinOp::Lt => yn(a < b),
                BinOp::Le => yn(a <= b),
                BinOp::Gt => yn(a > b),
                BinOp::Ge => yn(a >= b),
                BinOp::LAnd => yn(a != 0 && b != 0),
                BinOp::LOr => yn(a != 0 || b != 0),
            }
        }
        Expr::Cond {
            cond, then, els, ..
        } => {
            if eval_cond(cond, fields, word) != 0 {
                eval_cond(then, fields, word)
            } else {
                eval_cond(els, fields, word)
            }
        }
        Expr::Assemble { .. } | Expr::Call { .. } => 0,
    }
}

fn sext64(v: u64, n: u16) -> i64 {
    let n = n as u32;
    if n == 0 || n >= 64 {
        return v as i64;
    }
    let shift = 64 - n;
    ((v as i64) << shift) >> shift
}

fn rotl64(v: u64, k: u32, w: u32) -> u64 {
    if w == 0 {
        return 0;
    }
    let m = if w >= 64 { u64::MAX } else { (1u64 << w) - 1 };
    let v = v & m;
    let k = k % w;
    if k == 0 {
        v
    } else {
        ((v << k) | (v >> (w - k))) & m
    }
}

fn rotr64(v: u64, k: u32, w: u32) -> u64 {
    if w == 0 {
        return 0;
    }
    rotl64(v, (w - (k % w)) % w, w)
}
