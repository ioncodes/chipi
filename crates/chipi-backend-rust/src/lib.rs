//! Emit Rust source from a resolved [`chipi_core::Isa`].
//!
//! [`emit_decoder`] produces one self-contained module with no dependencies: opcode constants, a
//! dense classification path (one table per distinct mode tree, with guards folded per
//! combination), shift/mask accessors, identity-axis metadata for dotted leaf names (`Mnemonic`
//! and `Form` enums with `mnemonic()`/`form()` accessors), a full `[Handler; N]` dispatch table,
//! an operand-signature `Ops` trait, an optional prefix scan (`scan_prefixes`/`decode_stream`,
//! plus `classify_with(word, &Context)` when guards read context fields) and a feature-gated
//! disassembler. Specs with `fetch` or `:sym`/`:rel` operands get the contextual surface: a
//! `DisasmCtx` trait, per-operand stream accessors (mode-dependent widths resolve through
//! `ctx.mode`), `stream_len` and `disasm_ctx(pc, ctx)`. The hot path does not allocate. Text
//! rendering sits behind `feature = "disasm"`.
//!
//! Word-level entry points (`classify(word)`, `Display`) evaluate every decode variable at its
//! declared default, matching the oracle's `decode(word)`; the `_in(combo)` and `_with(ctx)`
//! variants carry real mode and context values.

mod exprgen;
mod names;

/// Map a name to a valid Rust identifier. Re-exported so dependents (e.g. `chipi-macros`) use the
/// same rules instead of re-deriving them.
pub use names::sanitize;

use chipi_core::accessor::computed_accessor_names;
use chipi_core::interp::{
    fetch_expr, fetch_has_expr, fetch_width, fetched_bytes, fetched_bytes_combo, is_fetch,
};
use chipi_core::model::*;
use chipi_core::render::{segs_have_sym, FmtSpec, Seg};
use chipi_core::Isa;
use chipi_syntax::ast::{BinOp, Expr, UnOp};
use exprgen::{emit_cond, emit_prefix, emit_value, Scope};
use names::{const_name, ident, mask_u64, pascal, ret_type};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Write as _;

/// Which `Instruction` representation the Rust backend emits.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Dispatch {
    /// `Instruction(handle)` newtype with lazy operand accessors and the `Ops` trait (default).
    #[default]
    Newtype,
    /// A nested `Instruction` enum whose variants carry pre-extracted operands. Decoding is eager
    /// (`decode(.., pc, ctx)`), dispatch is a `match` and each `dispatch {}` group becomes a
    /// payload-bearing sub-enum wrapped by one `Instruction` variant.
    Enum,
}

/// Code-generation options for the Rust backend.
#[derive(Clone, Copy, Debug, Default)]
pub struct EmitOptions {
    pub dispatch: Dispatch,
}

/// Emit the full decoder module for `isa` using the default (newtype) dispatch.
pub fn emit_decoder(isa: &Isa) -> String {
    emit_decoder_with(isa, EmitOptions::default())
}

/// Emit the decoder module for `isa` with explicit options (e.g. the nested-enum dispatch).
pub fn emit_decoder_with(isa: &Isa, opts: EmitOptions) -> String {
    match opts.dispatch {
        Dispatch::Newtype => emit_newtype(isa),
        Dispatch::Enum => emit_decoder_enum(isa),
    }
}

fn emit_newtype(isa: &Isa) -> String {
    let m = Model::new(isa);

    if let Some(reason) = unsupported_var_reads(isa, &m) {
        return format!("{}compile_error!(\"chipi: {reason}\");\n", header(isa));
    }

    let mut s = String::new();
    s.push_str(&header(isa));
    s.push_str(&preamble(&m));
    s.push_str(&opcode_consts(isa));
    s.push_str(&axis_consts(isa, &m));
    s.push_str(&classify(isa, &m));
    s.push_str(&handle_impl(isa, &m));
    s.push_str(&user_fns(isa));
    s.push_str(&computed_accessors(isa));
    s.push_str(&decode_dispatch(isa, &m));
    s.push_str(&ops_dispatch(isa, &m));

    if isa.prefix.is_some() {
        s.push_str(&prefix_scan(isa, &m));
    }
    if m.needs_disasm_ctx {
        s.push_str(&disasm_ctx_support(isa, &m));
    }
    if m.emit_display {
        s.push_str(&display_impl(isa, &m));
    }
    if m.needs_disasm_ctx {
        s.push_str(&disasm_ctx_fn(isa, &m));
    }

    s
}

/// Cross-cutting flags derived once.
struct Model {
    modal: bool,
    has_guard: bool,
    /// Some guard reads a prefix-assigned context field, so a `classify_with(word, &Context)`
    /// entry point is emitted next to the default-context `classify(word)`.
    ctx_guards: bool,
    needs_disasm_ctx: bool,
    emit_display: bool,
    needs_preamble128: bool,
    /// member name to (group name, variant)
    grouped: HashMap<String, (String, String)>,
}

impl Model {
    fn new(isa: &Isa) -> Self {
        let modal = !isa.modes.is_empty();
        let has_guard = isa.instrs.iter().any(|i| i.guard.is_some());
        let has_computed = isa.instrs.iter().any(|i| !i.computed.is_empty());
        let has_fetch = isa
            .instrs
            .iter()
            .any(|i| i.computed.iter().any(|c| is_fetch(&c.expr)));
        let needs_sym = isa
            .instrs
            .iter()
            .any(|i| i.display.iter().any(|a| segs_have_sym(&a.segs)));

        let needs_disasm_ctx = has_fetch || needs_sym;
        // The static disassembler needs no host context. Non-modal ISAs get it whenever nothing is
        // fetched from the stream (a `:sym` field just renders unresolved). A modal ISA gets it
        // only when it needs no `DisasmCtx` at all; with `:sym` in play the contextual path already
        // covers modal rendering and a second modal renderer would be dead weight.
        let emit_display = if modal { !needs_disasm_ctx } else { !has_fetch };
        let needs_preamble128 =
            has_computed || !isa.fns.is_empty() || isa.length.is_some() || has_guard;

        let ctx_names: Vec<String> = isa.decoder.context.iter().map(|c| c.name.clone()).collect();
        let ctx_guards = !ctx_names.is_empty()
            && isa.prefix.is_some()
            && !modal
            && isa.instrs.iter().any(|i| {
                i.guard
                    .as_ref()
                    .is_some_and(|g| chipi_core::compute::expr_reads_any(g, &ctx_names))
            });

        let mut grouped = HashMap::new();
        for g in &isa.groups {
            for m in &g.members {
                grouped.insert(m.clone(), (g.name.clone(), pascal(m)));
            }
        }

        Model {
            modal,
            has_guard,
            ctx_guards,
            needs_disasm_ctx,
            emit_display,
            needs_preamble128,
            grouped,
        }
    }
}

/// Opcode ids in tree order (slot 0 = Invalid, rest sorted by name). Ids index `instrs` via `instr`.
fn id_const(isa: &Isa, id: usize) -> String {
    if id == 0 {
        "opcode::INVALID".to_string()
    } else {
        format!("opcode::{}", const_name(&isa.tree.opcodes[id].name))
    }
}

/// The expression for an instruction's window length: a per-instruction lookup when the spec has a
/// `length` table, otherwise the fixed `LEN` constant.
fn len_expr(isa: &Isa) -> &'static str {
    if isa.length.is_some() {
        "inst_len(word)"
    } else {
        "LEN"
    }
}

/// A reason string if the spec reads decode variables somewhere this backend cannot evaluate
/// them at runtime yet: `length` arms are emitted as a pure function of the word, and the
/// static `Display` path has no variable source (the contextual disassembler resolves them
/// through `ctx.mode(..)`).
fn unsupported_var_reads(isa: &Isa, m: &Model) -> Option<&'static str> {
    if isa.display_reads_vars() && !(m.needs_disasm_ctx && !m.emit_display) {
        return Some(
            "display conditions reading decode variables outside the contextual \
             disassembler path are not supported yet",
        );
    }
    if isa.length_reads_vars() {
        return Some("`length` arms reading decode variables are not supported yet");
    }
    None
}

/// A `u64` Rust expression for a fetch width in bits, resolving mode reads via `ctx.mode(..)`.
fn fetch_bits_rust(isa: &Isa, arg: &chipi_syntax::ast::Expr) -> String {
    if let chipi_syntax::ast::Expr::Int(i) = arg {
        return format!("{}u64", i.value);
    }
    let vars: Vec<(String, String, u16)> = isa
        .modes
        .iter()
        .map(|m| {
            (
                m.name.clone(),
                format!("ctx.mode({:?})", m.name),
                m.value_width(),
            )
        })
        .collect();
    let scope = Scope::Computed {
        fields: &[],
        window: isa.window_bits(),
        base: "0u64",
        vars: &vars,
    };
    format!("(({}) as u64)", emit_value(arg, &scope))
}

// ---------------------------------------------------------------- sections

fn header(isa: &Isa) -> String {
    format!(
        "// === generated by chipi for decoder `{}` ({}bit unit, {:?}, {:?}) ===\n\
         // DO NOT EDIT: regenerate with `chipi emit`. Output is reproducible.\n\
         #![allow(dead_code, unexpected_cfgs, clippy::all, clippy::pedantic)]\n\n",
        isa.decoder.name, isa.decoder.unit_bits, isa.decoder.bit_order, isa.decoder.endian
    )
}

const RUNTIME_PREAMBLE: &str = r#"// ---- runtime preamble ----
#[inline]
fn sext64(v: u64, n: u32) -> i64 {
    if n == 0 || n >= 64 { return v as i64; }
    let shift = 64 - n;
    ((v as i64) << shift) >> shift
}

#[inline]
fn rotl64(v: u64, k: u32, w: u32) -> u64 {
    if w == 0 { return 0; }
    let m = if w >= 64 { u64::MAX } else { (1u64 << w) - 1 };
    let v = v & m; let k = k % w;
    if k == 0 { v } else { ((v << k) | (v >> (w - k))) & m }
}

"#;

const COMPUTE_PREAMBLE: &str = r#"// ---- computation-layer preamble ----
#[inline]
fn cmask128(w: u16) -> u128 { if w >= 128 { u128::MAX } else { (1u128 << w) - 1 } }
#[inline]
fn sext128(v: u128, n: u16) -> u128 {
    if n == 0 || n >= 128 { return v; }
    let m = cmask128(n);
    let x = v & m;
    if (x >> (n - 1)) & 1 == 1 { x | !m } else { x }
}
#[inline]
fn rotl128(v: u128, n: u128, w: u16, left: bool) -> u128 {
    if w == 0 { return 0; }
    let m = cmask128(w); let v = v & m; let n = (n % w as u128) as u16;
    if n == 0 { return v; }
    let (l, r) = if left { (n, w - n) } else { (w - n, n) };
    ((v << l) | (v >> r)) & m
}
#[inline]
fn bitwidth128(v: u128) -> u128 { (128 - v.leading_zeros()) as u128 }
#[inline]
fn replicate128(v: u128, elem: u16, total: u16) -> u128 {
    if elem == 0 || total == 0 { return 0; }
    let chunk = v & cmask128(elem);
    let mut out = 0u128; let mut s = 0u16;
    while s < total { out |= chunk << s; s += elem; }
    out & cmask128(total)
}
#[inline]
fn maskrange128(b: u16, e: u16, w: u16) -> u128 {
    let mut out = 0u128;
    if b <= e {
        let mut bit = b;
        while bit <= e.min(w.saturating_sub(1)) { out |= 1u128 << bit; bit += 1; }
    } else {
        let mut bit = b; while bit < w { out |= 1u128 << bit; bit += 1; }
        let mut bit = 0u16; while bit <= e { out |= 1u128 << bit; bit += 1; }
    }
    out & cmask128(w)
}

"#;

fn preamble(m: &Model) -> String {
    let mut s = String::from(RUNTIME_PREAMBLE);
    if m.needs_preamble128 {
        s.push_str(COMPUTE_PREAMBLE);
    }
    s
}

fn opcode_consts(isa: &Isa) -> String {
    let mut s = String::new();

    let n = isa.tree.opcode_count();
    let _ = writeln!(s, "pub const OPCODE_COUNT: usize = {n};\n");

    s.push_str("/// Opcode ids double as dispatch-table indices (0 = invalid).\n");
    s.push_str("pub mod opcode {\n");
    for (id, op) in isa.tree.opcodes.iter().enumerate() {
        let _ = writeln!(s, "    pub const {}: usize = {id};", const_name(&op.name));
    }
    s.push_str("}\n\n");

    let _ = writeln!(s, "pub static OPCODE_NAMES: [&str; OPCODE_COUNT] = [");
    for op in isa.tree.opcodes.iter() {
        let _ = writeln!(s, "    {:?},", op.name);
    }
    s.push_str("];\n\n");

    if !isa.tags.is_empty() {
        s.push_str(
            "/// Tags carried by each opcode (by id); `Invalid` and untagged opcodes are `&[]`.\n",
        );
        let _ = writeln!(s, "pub static OPCODE_TAGS: [&[&str]; OPCODE_COUNT] = [");
        for op in isa.tree.opcodes.iter() {
            let tags = if op.instr == usize::MAX {
                &[][..]
            } else {
                &isa.instrs[op.instr].tags[..]
            };
            if tags.is_empty() {
                s.push_str("    &[],\n");
            } else {
                let list: Vec<String> = tags.iter().map(|t| format!("{t:?}")).collect();
                let _ = writeln!(s, "    &[{}],", list.join(", "));
            }
        }
        s.push_str("];\n\n");
    }

    s
}

/// The element type of a primary table holding `opcode_count + residuals` distinct values.
fn elem_ty(values: usize) -> &'static str {
    if values <= u16::MAX as usize {
        "u16"
    } else {
        "u32"
    }
}

fn primary_table(tree: &chipi_core::tree::Tree, suffix: &str) -> String {
    let base = tree.opcode_count();
    let elem = elem_ty(base + tree.residuals.len());

    let mut s = String::new();
    let _ = writeln!(
        s,
        "// Dense primary table over `{}` (lowering: {}). Values {base} or higher are residual sentinels.",
        tree.primary.name,
        tree.primary_lowering.label()
    );
    let _ = writeln!(
        s,
        "static PRIMARY{suffix}: [{elem}; {}] = [",
        tree.slots.len()
    );

    let mut row = String::new();
    for (i, slot) in tree.slots.iter().enumerate() {
        let v = slot.table_value(base);
        row.push_str(&format!("{v},"));
        if i % 16 == 15 {
            row.push('\n');
        }
    }
    if !row.ends_with('\n') {
        row.push('\n');
    }

    s.push_str(&row);
    s.push_str("];\n\n");
    s
}

/// The body of a classification routine for one tree (everything inside the `fn` braces).
/// Decode-variable reads inside chain guards fold through `subst` (mode combo values, context
/// defaults); names left over resolve through `ctx_vars` as runtime expressions.
fn routing_body(
    isa: &Isa,
    tree: &chipi_core::tree::Tree,
    table: &str,
    subst: &[(String, u64, u16)],
    ctx_vars: &[(String, String, u16)],
) -> String {
    use chipi_core::tree::Residual;

    let base = tree.opcode_count();
    let p_lo = tree.primary.range.lo;
    let p_mask = mask_u64(tree.primary.range.width());

    let mut s = String::new();
    let _ = writeln!(
        s,
        "    let primary = ((word as u64 >> {p_lo}) & {p_mask:#x}) as usize;"
    );

    if tree.residuals.is_empty() {
        let _ = writeln!(s, "    {table}[primary] as usize");
        return s;
    }

    let _ = writeln!(s, "    match {table}[primary] {{");
    for (ri, r) in tree.residuals.iter().enumerate() {
        let sentinel = base + ri;
        match r {
            Residual::Keyed {
                key,
                lowering,
                arms,
                default,
            } => {
                let k_lo = key.range.lo;
                let k_mask = mask_u64(key.range.width());

                let _ = writeln!(
                    s,
                    "        {sentinel} => match (word as u64 >> {k_lo}) & {k_mask:#x} {{ // residual `{}` ({})",
                    key.name,
                    lowering.label()
                );
                for (kv, id) in arms {
                    let _ = writeln!(s, "            {kv:#x} => {},", id_const(isa, *id));
                }
                let _ = writeln!(s, "            _ => {},", id_const(isa, *default));
                s.push_str("        },\n");
            }
            Residual::Sparse { lowering, arms } => {
                let _ = writeln!(
                    s,
                    "        {sentinel} => {{ // residual verify chain ({})",
                    lowering.label()
                );
                for a in arms {
                    let guard = isa
                        .sparse_arm_guard(tree, a)
                        .map(|g| {
                            let inst = &isa.instrs[tree.opcodes[a.opcode].instr];
                            let g = chipi_core::compute::subst_vars(g, subst);
                            let scope = Scope::Computed {
                                fields: &inst.fields,
                                window: isa.window_bits(),
                                base: "word",
                                vars: ctx_vars,
                            };
                            format!(" && ({}) != 0", emit_value(&g, &scope))
                        })
                        .unwrap_or_default();
                    let _ = writeln!(
                        s,
                        "            if (word as u64 & {:#x}) == {:#x}{guard} {{ return {}; }}",
                        a.mask,
                        a.val,
                        id_const(isa, a.opcode)
                    );
                }
                s.push_str("            opcode::INVALID\n        }\n");
            }
        }
    }
    s.push_str("        id => id as usize,\n    }\n");
    s
}

/// Identity axes: when leaf names are dotted (`lda.dpx`), derive the `Mnemonic` and `Form`
/// enums plus per-opcode tables and accessors. Undotted leaves map to `Form::None`.
fn axis_consts(isa: &Isa, m: &Model) -> String {
    if !isa.has_axes() {
        return String::new();
    }
    let mut s = axis_tables(isa);

    s.push_str("impl Instruction {\n");
    if m.modal {
        s.push_str("    #[inline]\n    pub fn mnemonic_in(self, combo: usize) -> Mnemonic { OPCODE_MNEMONIC[self.opcode_in(combo)] }\n");
        s.push_str("    #[inline]\n    pub fn form_in(self, combo: usize) -> Form { OPCODE_FORM[self.opcode_in(combo)] }\n");
    } else {
        s.push_str("    #[inline]\n    pub fn mnemonic(self) -> Mnemonic { OPCODE_MNEMONIC[self.opcode()] }\n");
        s.push_str("    #[inline]\n    pub fn form(self) -> Form { OPCODE_FORM[self.opcode()] }\n");
    }
    s.push_str("}\n\n");

    s
}

/// The identity-axis accessors for the nested-enum decoder. The decoded variant already fixes the
/// leaf, so the lookup goes through `opcode_id()` and needs no mode combo even in modal specs.
fn axis_consts_enum(isa: &Isa) -> String {
    if !isa.has_axes() {
        return String::new();
    }
    let mut s = axis_tables(isa);

    s.push_str("impl Instruction {\n");
    s.push_str("    /// The mnemonic axis of this leaf (`Invalid` maps to `Mnemonic::Invalid`).\n");
    s.push_str(
        "    #[inline]\n    pub fn mnemonic(&self) -> Mnemonic { OPCODE_MNEMONIC[self.opcode_id()] }\n",
    );
    s.push_str(
        "    /// The form axis of this leaf (undotted leaves and `Invalid` map to `Form::None`).\n",
    );
    s.push_str("    #[inline]\n    pub fn form(&self) -> Form { OPCODE_FORM[self.opcode_id()] }\n");
    s.push_str("}\n\n");

    s
}

/// The axis enums and per-opcode tables shared by both dispatch styles: `Mnemonic`, `Form`, their
/// name tables and `OPCODE_MNEMONIC` / `OPCODE_FORM`.
fn axis_tables(isa: &Isa) -> String {
    let mnems = isa.mnemonics();
    let forms = isa.form_axes();

    let mut s = String::new();

    s.push_str("/// The mnemonic axis of the leaf names (`lda.dpx` -> `Lda`).\n");
    s.push_str(
        "#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]\npub enum Mnemonic {\n    Invalid,\n",
    );
    for mn in &mnems {
        let _ = writeln!(s, "    {},", pascal(mn));
    }
    s.push_str("}\n\n");

    let _ = writeln!(
        s,
        "pub const MNEMONIC_NAMES: [&str; {}] = [",
        mnems.len() + 1
    );
    s.push_str("    \"Invalid\",\n");
    for mn in &mnems {
        let _ = writeln!(s, "    {mn:?},");
    }
    s.push_str("];\n\n");

    s.push_str("impl Mnemonic {\n    #[inline]\n    pub fn name(self) -> &'static str { MNEMONIC_NAMES[self as usize] }\n}\n\n");

    s.push_str(
        "/// The form axis of the leaf names (`lda.dpx` -> `Dpx`; undotted leaves -> `None`).\n",
    );
    s.push_str("#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]\npub enum Form {\n    None,\n");
    for f in &forms {
        let _ = writeln!(s, "    {},", pascal(f));
    }
    s.push_str("}\n\n");

    let _ = writeln!(s, "pub const FORM_NAMES: [&str; {}] = [", forms.len() + 1);
    s.push_str("    \"none\",\n");
    for f in &forms {
        let _ = writeln!(s, "    {f:?},");
    }
    s.push_str("];\n\n");

    s.push_str("impl Form {\n    #[inline]\n    pub fn name(self) -> &'static str { FORM_NAMES[self as usize] }\n}\n\n");

    let n = isa.tree.opcodes.len();
    let _ = writeln!(s, "pub const OPCODE_MNEMONIC: [Mnemonic; {n}] = [");
    for op in isa.tree.opcodes.iter() {
        if op.instr == usize::MAX {
            s.push_str("    Mnemonic::Invalid,\n");
        } else {
            let _ = writeln!(
                s,
                "    Mnemonic::{},",
                pascal(&isa.instrs[op.instr].mnemonic)
            );
        }
    }
    s.push_str("];\n\n");

    let _ = writeln!(s, "pub const OPCODE_FORM: [Form; {n}] = [");
    for op in isa.tree.opcodes.iter() {
        let form = if op.instr == usize::MAX {
            None
        } else {
            isa.instrs[op.instr].form.as_deref()
        };
        match form {
            Some(f) => {
                let _ = writeln!(s, "    Form::{},", pascal(f));
            }
            None => s.push_str("    Form::None,\n"),
        }
    }
    s.push_str("];\n\n");

    s
}

fn classify(isa: &Isa, m: &Model) -> String {
    let handle = isa.handle_ty();
    let mut s = String::new();

    if m.modal {
        // One primary table per distinct tree (combinations with identical leaf sets share it).
        // When the tree's chain guards read no decode variable, the routing function is shared
        // too; otherwise routing folds per combination over the shared table.
        let chain_reads: Vec<bool> = isa
            .mode_trees
            .iter()
            .map(|t| isa.tree_chain_reads_vars(t))
            .collect();

        for (t, tree) in isa.mode_trees.iter().enumerate() {
            s.push_str(&primary_table(tree, &format!("_{t}")));

            if !chain_reads[t] {
                let first = isa.combo_tree.iter().position(|&x| x == t).unwrap_or(0);
                let subst = isa.combo_subst(first as u64);

                let _ = writeln!(
                    s,
                    "#[inline]\nfn classify_tree_{t}(word: {handle}) -> usize {{"
                );
                s.push_str(&routing_body(
                    isa,
                    tree,
                    &format!("PRIMARY_{t}"),
                    &subst,
                    &[],
                ));
                s.push_str("}\n\n");
            }
        }

        // Per-combination wrappers: guard folding is per combination even when a tree is shared.
        let combos = isa.mode_combos() as usize;
        let mut dispatch_arms = Vec::with_capacity(combos);

        for i in 0..combos {
            let t = isa.combo_tree[i];
            let tree = &isa.mode_trees[t];
            let subst = isa.combo_subst(i as u64);

            let raw_call = if chain_reads[t] {
                let _ = writeln!(
                    s,
                    "#[inline]\nfn classify_raw_{i}(word: {handle}) -> usize {{"
                );
                s.push_str(&routing_body(
                    isa,
                    tree,
                    &format!("PRIMARY_{t}"),
                    &subst,
                    &[],
                ));
                s.push_str("}\n\n");
                format!("classify_raw_{i}(word)")
            } else {
                format!("classify_tree_{t}(word)")
            };

            if m.has_guard {
                let _ = writeln!(s, "#[inline]\nfn classify_{i}(word: {handle}) -> usize {{");
                let _ = writeln!(s, "    let __id = {raw_call};\n    match __id {{");
                s.push_str(&guard_arms(isa, &subst, &[]));
                s.push_str("        _ => __id,\n    }\n}\n\n");
                dispatch_arms.push(format!("classify_{i}(word)"));
            } else {
                dispatch_arms.push(raw_call);
            }
        }

        let _ = writeln!(s, "pub const MODE_COMBOS: usize = {};\n", isa.mode_combos());

        s.push_str("/// Classify `word` for mode combination `combo` (see `pack_modes`).\n");
        s.push_str(&format!("#[inline]\npub fn classify(combo: usize, word: {handle}) -> usize {{\n    match combo {{\n"));
        for (i, arm) in dispatch_arms.iter().enumerate() {
            let _ = writeln!(s, "        {i} => {arm},");
        }
        s.push_str("        _ => 0,\n    }\n}\n\n");

        s.push_str(
            "/// Pack mode values (declaration order) into a `classify` combination index.\n",
        );
        let params: Vec<String> = isa
            .modes
            .iter()
            .map(|md| format!("{}: u64", sanitize(&md.name)))
            .collect();
        let _ = writeln!(
            s,
            "#[inline]\npub fn pack_modes({}) -> usize {{",
            params.join(", ")
        );
        s.push_str("    let mut idx = 0usize;\n    let mut radix = 1u64;\n");
        for md in &isa.modes {
            let _ = writeln!(
                s,
                "    idx += (({} % {c}) * radix) as usize; radix *= {c};",
                sanitize(&md.name),
                c = md.cardinality
            );
        }
        s.push_str("    idx\n}\n\n");

        return s;
    }

    s.push_str(&primary_table(&isa.tree, ""));

    // Word-level classify folds every decode variable to its default, matching the oracle's
    // `decode(word)`. Context-dependent classification goes through `classify_with`.
    let subst = isa.default_subst();

    if m.has_guard {
        let _ = writeln!(s, "#[inline]\nfn classify_raw(word: {handle}) -> usize {{");
        s.push_str(&routing_body(isa, &isa.tree, "PRIMARY", &subst, &[]));
        s.push_str("}\n\n");

        if !isa.decoder.context.is_empty() {
            s.push_str(
                "/// Classify `word` with every decode variable at its default (as if no prefix\n\
                 /// was seen). Use `classify_with` after a prefix scan.\n",
            );
        }
        let _ = writeln!(s, "#[inline]\npub fn classify(word: {handle}) -> usize {{");
        s.push_str("    let __id = classify_raw(word);\n    match __id {\n");
        s.push_str(&guard_arms(isa, &subst, &[]));
        s.push_str("        _ => __id,\n    }\n}\n\n");
    } else {
        let _ = writeln!(s, "#[inline]\npub fn classify(word: {handle}) -> usize {{");
        s.push_str(&routing_body(isa, &isa.tree, "PRIMARY", &subst, &[]));
        s.push_str("}\n\n");
    }

    if m.ctx_guards {
        let ctx_vars: Vec<(String, String, u16)> = isa
            .decoder
            .context
            .iter()
            .map(|c| (c.name.clone(), format!("ctx.{}", ident(&c.name)), c.width))
            .collect();

        let _ = writeln!(
            s,
            "#[inline]\nfn classify_raw_with(word: {handle}, ctx: &Context) -> usize {{"
        );
        s.push_str(&routing_body(isa, &isa.tree, "PRIMARY", &[], &ctx_vars));
        s.push_str("}\n\n");

        s.push_str("/// Classify `word` under a scanned prefix context.\n");
        let _ = writeln!(
            s,
            "#[inline]\npub fn classify_with(word: {handle}, ctx: &Context) -> usize {{"
        );
        s.push_str("    let __id = classify_raw_with(word, ctx);\n    match __id {\n");
        s.push_str(&guard_arms(isa, &[], &ctx_vars));
        s.push_str("        _ => __id,\n    }\n}\n\n");
    }
    s
}

/// The `match __id` guard arms shared by every classify wrapper: each guarded opcode re-checks
/// its guard, with decode variables folded through `subst` or resolved through `ctx_vars`.
fn guard_arms(
    isa: &Isa,
    subst: &[(String, u64, u16)],
    ctx_vars: &[(String, String, u16)],
) -> String {
    let mut s = String::new();
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if let Some(g) = &inst.guard {
            let g = chipi_core::compute::subst_vars(g, subst);
            let scope = Scope::Computed {
                fields: &inst.fields,
                window: isa.window_bits(),
                base: "word",
                vars: ctx_vars,
            };
            let _ = writeln!(
                s,
                "        opcode::{} => if ({}) != 0 {{ __id }} else {{ opcode::INVALID }},",
                const_name(&inst.name),
                emit_value(&g, &scope)
            );
        }
    }
    s
}

fn accessor_body(f: &Field) -> String {
    accessor_body_base(f, "self.0")
}

/// `accessor_body` reading the raw field from an arbitrary window base (`self.0` for the newtype
/// accessors, `word` for the eager enum decoder). The two emit identical text when `base` matches.
fn accessor_body_base(f: &Field, base: &str) -> String {
    let mut s = format!(
        "{{ let v = (({base} as u64) >> {}) & {:#x};",
        f.range.lo,
        mask_u64(f.range.width())
    );
    for x in &f.ty.xforms {
        match x {
            Xform::ShiftLeft(n) => s.push_str(&format!(" let v = v << {n};")),
            Xform::ShiftRight(n) => s.push_str(&format!(" let v = v >> {n};")),
            Xform::ZeroExtend(n) => s.push_str(&format!(" let v = v & {:#x};", mask_u64(*n))),
            Xform::SignExtend(n) => s.push_str(&format!(" let v = sext64(v as u64, {n});")),
            Xform::RotateLeft(k, w) => s.push_str(&format!(" let v = rotl64(v as u64, {k}, {w});")),
            Xform::RotateRight(k, w) => {
                let kk = if *w == 0 {
                    0
                } else {
                    (*w as u32 - (*k as u32 % *w as u32)) % *w as u32
                };
                s.push_str(&format!(" let v = rotl64(v as u64, {kk}, {w});"));
            }
        }
    }

    s.push_str(&format!(" v as {} }}", ret_type(&f.ty)));
    s
}

fn handle_impl(isa: &Isa, m: &Model) -> String {
    let handle = isa.handle_ty();

    let mut s = String::new();
    s.push_str("#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]\n");
    let _ = writeln!(s, "pub struct Instruction(pub {handle});\n");

    s.push_str("impl Instruction {\n");
    if m.modal {
        s.push_str("    #[inline]\n    pub fn opcode_in(self, combo: usize) -> usize { classify(combo, self.0) }\n");
        s.push_str("    #[inline]\n    pub fn opcode_name_in(self, combo: usize) -> &'static str { OPCODE_NAMES[self.opcode_in(combo)] }\n");
        if !isa.tags.is_empty() {
            s.push_str("    #[inline]\n    pub fn tags_in(self, combo: usize) -> &'static [&'static str] { OPCODE_TAGS[self.opcode_in(combo)] }\n");
        }
    } else {
        s.push_str(
            "    #[inline(always)]\n    pub fn opcode(self) -> usize { classify(self.0) }\n",
        );
        s.push_str("    #[inline]\n    pub fn opcode_name(self) -> &'static str { OPCODE_NAMES[self.opcode()] }\n");
        if m.ctx_guards {
            s.push_str("    /// `opcode` under a scanned prefix context (see `scan_prefixes`).\n");
            s.push_str("    #[inline]\n    pub fn opcode_with(self, ctx: &Context) -> usize { classify_with(self.0, ctx) }\n");
            s.push_str("    #[inline]\n    pub fn opcode_name_with(self, ctx: &Context) -> &'static str { OPCODE_NAMES[self.opcode_with(ctx)] }\n");
        }
        if !isa.tags.is_empty() {
            s.push_str("    #[inline]\n    pub fn tags(self) -> &'static [&'static str] { OPCODE_TAGS[self.opcode()] }\n");
        }
    }

    for (name, f) in isa.unique_fields() {
        let _ = writeln!(
            s,
            "    /// `{name}` = stream bits [{}:{}]",
            f.range.hi, f.range.lo
        );
        let _ = writeln!(
            s,
            "    #[inline(always)]\n    pub fn {}(self) -> {} {}",
            ident(&name),
            ret_type(&f.ty),
            accessor_body(f)
        );
    }
    s.push_str("}\n\n");
    s
}

fn user_fns(isa: &Isa) -> String {
    if isa.fns.is_empty() {
        return String::new();
    }
    let mut s = String::from("// ---- user fns (computation layer) ----\n");
    for f in &isa.fns {
        let params: Vec<String> = f
            .params
            .iter()
            .map(|(n, _)| format!("v_{}: u128", sanitize(n)))
            .collect();
        let _ = writeln!(
            s,
            "fn fn_{}({}) -> u128 {{",
            sanitize(&f.name),
            params.join(", ")
        );

        // Mask each argument to its declared width on entry, matching the
        // reference evaluator (eval_fn) and the C++/Python backends.
        for (n, ty) in &f.params {
            let _ = writeln!(
                s,
                "    let v_{0} = v_{0} & cmask128({1});",
                sanitize(n),
                ty.width()
            );
        }

        let mut widths: HashMap<String, u16> = f
            .params
            .iter()
            .map(|(n, ty)| (n.clone(), ty.width()))
            .collect();
        for (ln, le, lw) in &f.lets {
            let scope = Scope::Fn {
                widths: widths.clone(),
            };
            let _ = writeln!(
                s,
                "    let v_{} = {};",
                sanitize(ln),
                emit_value(le, &scope)
            );
            widths.insert(ln.clone(), *lw);
        }

        let scope = Scope::Fn { widths };
        let _ = writeln!(
            s,
            "    ({}) & cmask128({})",
            emit_value(&f.ret_expr, &scope),
            f.ret.width()
        );
        s.push_str("}\n\n");
    }
    s
}

/// The deduplicated accessor method name for one instruction's computed operand.
fn comp_acc(acc: &HashMap<(String, String), String>, inst: &Insn, c: &Computed) -> String {
    names::computed_method(&acc[&(inst.name.clone(), c.name.clone())])
}

fn computed_accessors(isa: &Isa) -> String {
    let acc = computed_accessor_names(isa);
    let mut emitted: HashSet<String> = HashSet::new();
    let mut s = String::new();
    let mut opened = false;
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        for c in &inst.computed {
            if is_fetch(&c.expr) {
                continue;
            }
            // Folded operands share one accessor; emit each unique name once.
            if !emitted.insert(comp_acc(&acc, inst, c)) {
                continue;
            }
            if !opened {
                s.push_str("impl Instruction {\n");
                opened = true;
            }

            let scope = Scope::Computed {
                fields: &inst.fields,
                window: isa.window_bits(),
                base: "self.0",
                vars: &[],
            };
            let body = emit_value(&c.expr, &scope);

            let cast = if c.ty.signed {
                format!("sext128(__v, {}) as {}", c.ty.value_width, ret_type(&c.ty))
            } else {
                format!(
                    "(__v & cmask128({})) as {}",
                    c.ty.value_width,
                    ret_type(&c.ty)
                )
            };
            let _ = writeln!(
                s,
                "    /// computed operand `{}` of `{}`",
                c.name, inst.name
            );
            let _ = writeln!(
                s,
                "    #[inline(always)]\n    pub fn {}(self) -> {} {{ let __v: u128 = {body}; {cast} }}",
                comp_acc(&acc, inst, c),
                ret_type(&c.ty)
            );
        }
    }
    if opened {
        s.push_str("}\n\n");
    }
    s
}

fn decode_dispatch(isa: &Isa, m: &Model) -> String {
    let handle = isa.handle_ty();
    let mut s = String::new();

    if let Some(len) = &isa.length {
        s.push_str("/// Window length in bits selected by the spec's `length` expression.\n");
        let _ = writeln!(
            s,
            "#[inline]\npub fn inst_len_bits(word: {handle}) -> u16 {{"
        );
        for arm in &len.arms {
            match &arm.cond {
                Some(c) => {
                    let scope = Scope::Computed {
                        fields: &[],
                        window: isa.window_bits(),
                        base: "word",
                        vars: &[],
                    };
                    let _ = writeln!(
                        s,
                        "    if ({}) != 0 {{ return {}; }}",
                        emit_value(c, &scope),
                        arm.bits
                    );
                }
                None => {
                    let _ = writeln!(s, "    {}", arm.bits);
                }
            }
        }
        s.push_str("}\n");

        let _ = writeln!(s, "#[inline]\npub fn inst_len(word: {handle}) -> u8 {{ inst_len_bits(word).div_ceil(8) as u8 }}");
        let _ = writeln!(s, "#[inline]\npub fn decode(word: {handle}) -> (Instruction, u8) {{ (Instruction(word), inst_len(word)) }}\n");
    } else {
        let _ = writeln!(s, "pub const LEN: u8 = {};", isa.max_len_bytes);
        let _ = writeln!(s, "#[inline]\npub fn decode(word: {handle}) -> (Instruction, u8) {{ (Instruction(word), LEN) }}\n");
    }

    let (cp, ca) = combo_threads(m);

    s.push_str("pub type Handler<Ctx> = fn(&mut Ctx, Instruction);\n");
    s.push_str("#[inline]\npub fn decode_invalid<Ctx>(_ctx: &mut Ctx, _inst: Instruction) {}\n");
    s.push_str(
        "/// A dispatch table with every slot set to `decode_invalid`. Override the ones you handle.\n",
    );
    s.push_str("pub fn default_table<Ctx>() -> [Handler<Ctx>; OPCODE_COUNT] {\n    [decode_invalid as Handler<Ctx>; OPCODE_COUNT]\n}\n");
    let _ = writeln!(
        s,
        "#[inline]\npub fn dispatch<Ctx>({cp}table: &[Handler<Ctx>; OPCODE_COUNT], ctx: &mut Ctx, inst: Instruction) {{\n    table[classify({ca}inst.0)](ctx, inst);\n}}"
    );

    let len_expr = len_expr(isa);
    let _ = writeln!(
        s,
        "#[inline]\npub fn run<Ctx>({cp}table: &[Handler<Ctx>; OPCODE_COUNT], ctx: &mut Ctx, word: {handle}) -> u8 {{\n    table[classify({ca}word)](ctx, inst_from(word));\n    {len_expr}\n}}"
    );

    s.push_str(&format!(
        "#[inline]\nfn inst_from(word: {handle}) -> Instruction {{ Instruction(word) }}\n\n"
    ));
    s
}

fn combo_threads(m: &Model) -> (&'static str, &'static str) {
    if m.modal {
        ("combo: usize, ", "combo, ")
    } else {
        ("", "")
    }
}

fn ops_dispatch(isa: &Isa, m: &Model) -> String {
    let handle = isa.handle_ty();
    let (cp, ca) = combo_threads(m);

    let mut s = String::new();
    s.push_str("// ---- context-generic dispatch ----\n");
    for g in &isa.groups {
        let variants: Vec<String> = g.members.iter().map(|m| pascal(m)).collect();
        s.push_str("#[derive(Clone, Copy, PartialEq, Eq, Debug)]\n");
        let _ = writeln!(
            s,
            "pub enum {}Kind {{ {} }}",
            pascal(&g.name),
            variants.join(", ")
        );
    }

    s.push_str("/// Implement once per consumer. Read operands via `Instruction` accessors.\n");
    s.push_str("pub trait Ops {\n");
    for g in &isa.groups {
        let kind = format!("{}Kind", pascal(&g.name));
        s.push_str("    #[allow(unused_variables)]\n");
        let _ = writeln!(
            s,
            "    fn {}(&mut self, op: {kind}, inst: Instruction) {{",
            ident(&g.name)
        );
        s.push_str("        match op {\n");
        for mname in &g.members {
            let _ = writeln!(
                s,
                "            {kind}::{} => self.{}(inst),",
                pascal(mname),
                ident(mname)
            );
        }
        s.push_str("        }\n    }\n");
        for mname in &g.members {
            s.push_str("    #[allow(unused_variables)]\n");
            let _ = writeln!(
                s,
                "    fn {}(&mut self, inst: Instruction) {{}}",
                ident(mname)
            );
        }
    }

    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if m.grouped.contains_key(&inst.name) {
            continue;
        }
        let _ = writeln!(
            s,
            "    fn {}(&mut self, inst: Instruction);",
            ident(&inst.name)
        );
    }

    s.push_str("    /// Called for unmapped encodings. Override to trap or log.\n");
    s.push_str(
        "    #[allow(unused_variables)]\n    fn on_invalid(&mut self, inst: Instruction) {}\n}\n\n",
    );

    let _ = writeln!(s, "#[inline]\npub fn dispatch_ops<H: Ops>({cp}h: &mut H, inst: Instruction) {{\n    match classify({ca}inst.0) {{");
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if let Some((group, variant)) = m.grouped.get(&inst.name) {
            let _ = writeln!(
                s,
                "        opcode::{} => h.{}({}Kind::{variant}, inst),",
                const_name(&inst.name),
                ident(group),
                pascal(group)
            );
        } else {
            let _ = writeln!(
                s,
                "        opcode::{} => h.{}(inst),",
                const_name(&inst.name),
                ident(&inst.name)
            );
        }
    }
    s.push_str("        _ => h.on_invalid(inst),\n    }\n}\n");

    let len_expr = len_expr(isa);
    let _ = writeln!(
        s,
        "#[inline]\npub fn run_ops<H: Ops>({cp}h: &mut H, word: {handle}) -> u8 {{\n    dispatch_ops({ca}h, Instruction(word));\n    {len_expr}\n}}\n"
    );

    s
}

fn ctx_ty(width: u16) -> &'static str {
    match width {
        0..=8 => "u8",
        9..=16 => "u16",
        17..=32 => "u32",
        _ => "u64",
    }
}

fn prefix_scan(isa: &Isa, m: &Model) -> String {
    let prefix = isa.prefix.as_ref().unwrap();
    let handle = isa.handle_ty();

    let mut s = String::new();
    s.push_str("#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]\npub struct Context {\n");
    for c in &isa.decoder.context {
        let _ = writeln!(s, "    pub {}: {},", ident(&c.name), ctx_ty(c.width));
    }
    s.push_str("}\n\n");

    s.push_str("/// Scan leading prefix units. Returns the consumed count and the context.\n");
    s.push_str("pub fn scan_prefixes(bytes: &[u8]) -> (usize, Context) {\n");
    let inits: Vec<String> = isa
        .decoder
        .context
        .iter()
        .map(|c| format!("{}: {} as {}", ident(&c.name), c.default, ctx_ty(c.width)))
        .collect();
    let _ = writeln!(s, "    let mut ctx = Context {{ {} }};", inits.join(", "));
    s.push_str("    let mut cursor = 0usize;\n    while cursor < bytes.len() {\n        let byte = bytes[cursor] as u64;\n");
    for arm in &prefix.arms {
        let cond = match arm.pat {
            PrefixPat::Byte(b) => format!("byte == {b:#x}"),
            PrefixPat::Range(lo, hi) => format!("({lo:#x}..={hi:#x}).contains(&byte)"),
            PrefixPat::Wildcard => "true".to_string(),
        };
        let _ = writeln!(s, "        if {cond} {{");

        for (name, e) in &arm.assigns {
            let width = isa
                .decoder
                .context
                .iter()
                .find(|c| &c.name == name)
                .map(|c| c.width)
                .unwrap_or(64);
            let _ = writeln!(
                s,
                "            ctx.{} = (({}) & {:#x}) as {};",
                ident(name),
                emit_prefix(e),
                mask_u64(width),
                ctx_ty(width)
            );
        }

        match arm.term {
            PrefixTerm::Continue => s.push_str("            cursor += 1; continue;\n"),
            PrefixTerm::Finish => s.push_str("            cursor += 1; break;\n"),
            PrefixTerm::Done => s.push_str("            break;\n"),
        }
        s.push_str("        }\n");
    }
    s.push_str("        break;\n    }\n    (cursor, ctx)\n}\n\n");

    // read_window
    s.push_str(&format!("#[inline]\nfn read_window(bytes: &[u8], at: usize) -> {handle} {{\n    let mut w: {handle} = 0;\n"));
    let nbytes = (isa.window_bits() as usize).div_ceil(8);

    match isa.decoder.endian {
        Endian::Little => {
            for i in 0..nbytes {
                let _ = writeln!(
                    s,
                    "    w |= (*bytes.get(at + {i}).unwrap_or(&0) as {handle}) << {};",
                    8 * i
                );
            }
        }
        Endian::Big => {
            for i in 0..nbytes {
                let _ = writeln!(
                    s,
                    "    w = (w << 8) | (*bytes.get(at + {i}).unwrap_or(&0) as {handle});"
                );
            }
        }
    }
    s.push_str("    w\n}\n\n");

    if !m.modal {
        let len_expr = len_expr(isa);
        s.push_str(
            "/// Decode a byte stream: run the prefix scan, then decode the post-prefix window.\n",
        );
        s.push_str("pub fn decode_stream(bytes: &[u8]) -> (Instruction, u8, Context) {\n");
        s.push_str("    let (plen, ctx) = scan_prefixes(bytes);\n    let word = read_window(bytes, plen);\n");
        let _ = writeln!(
            s,
            "    (Instruction(word), plen as u8 + {len_expr}, ctx)\n}}\n"
        );
    }
    s
}

// ---------------------------------------------------------------- display

/// Operand name to value type for one instruction (no accessor strings, so the global dedup map is
/// not needed). Used where only the type matters: `Display`-vs-`disasm_ctx` delegation checks and the
/// enum renderer, which reads operands from local bindings rather than accessors.
fn op_types(inst: &Insn) -> BTreeMap<String, FieldTy> {
    let mut m = BTreeMap::new();
    for f in &inst.fields {
        m.insert(f.name.clone(), f.ty.clone());
    }
    for c in &inst.computed {
        m.insert(c.name.clone(), c.ty.clone());
    }
    m
}

/// Operand name to (accessor call without receiver, value type) for one instruction. Computed
/// operands use their deduplicated accessor name from `acc_names`.
fn op_acc(
    acc_names: &HashMap<(String, String), String>,
    inst: &Insn,
) -> BTreeMap<String, (String, FieldTy)> {
    let mut m = BTreeMap::new();

    for f in &inst.fields {
        m.insert(
            f.name.clone(),
            (format!("{}()", ident(&f.name)), f.ty.clone()),
        );
    }
    for c in &inst.computed {
        m.insert(
            c.name.clone(),
            (format!("{}()", comp_acc(acc_names, inst, c)), c.ty.clone()),
        );
    }

    m
}

fn acc_methods(acc: &BTreeMap<String, (String, FieldTy)>) -> BTreeMap<String, String> {
    acc.iter()
        .map(|(k, (m, _))| (k.clone(), m.clone()))
        .collect()
}

/// The interpreter and data types behind the table-driven disassembler. Instead of one
/// `write!`-heavy renderer per instruction, which compiles slowly, each instruction gathers its
/// operand values through the inline accessors. It hands them, with a small data program, to this
/// one interpreter. The compiler then builds data rows plus one renderer instead of thousands of
/// separate ones. Gathering through the accessors means computed operands work too, not just plain
/// bit-fields.
const DISASM_RUNTIME: &str = r#"
#[cfg(feature = "disasm")]
#[derive(Clone, Copy)]
struct DFmt { hex: bool, dec: bool, alt: bool, zero_pad: usize }
#[cfg(feature = "disasm")]
struct DRend { signed: bool, value_width: u16, disp: u8, pattern: &'static str, names: &'static [(u64, &'static str)], names_def: &'static str, names_def_kind: u8 }
#[cfg(feature = "disasm")]
enum CNode {
    Int(i128),
    Val(usize),
    Word,
    Un(u8, usize),
    Bin(u8, usize, usize),
    Slice(usize, u16, u16),
    Tern(usize, usize, usize),
}
#[cfg(feature = "disasm")]
enum DSeg {
    Lit(&'static str),
    Field(usize, usize, DFmt),
    Cond(&'static [CNode], &'static [DSeg], &'static [DSeg]),
    /// `{field.output}`: render operand-slot `usize` through a subdecoder output function.
    Sub(usize, fn(u64) -> String),
}

#[cfg(feature = "disasm")]
fn d_mask128(w: u16) -> u128 { if w >= 128 { u128::MAX } else { (1u128 << w) - 1 } }

#[cfg(feature = "disasm")]
fn d_binop(op: u8, a: i128, b: i128) -> i128 {
    let yn = |x: bool| if x { 1 } else { 0 };
    let sh = |b: i128| if (0..128).contains(&b) { b as u32 } else { 0 };
    match op {
        0 => a.wrapping_add(b),
        1 => a.wrapping_sub(b),
        2 => a.wrapping_mul(b),
        3 => if b == 0 { 0 } else { a.wrapping_div(b) },
        4 => if b == 0 { 0 } else { a.wrapping_rem(b) },
        5 => a & b,
        6 => a | b,
        7 => a ^ b,
        8 => a.wrapping_shl(sh(b)),
        9 => a.wrapping_shr(sh(b)),
        10 => yn(a == b),
        11 => yn(a != b),
        12 => yn(a < b),
        13 => yn(a <= b),
        14 => yn(a > b),
        15 => yn(a >= b),
        16 => yn(a != 0 && b != 0),
        _ => yn(a != 0 || b != 0),
    }
}

#[cfg(feature = "disasm")]
fn d_eval(nodes: &[CNode], i: usize, vals: &[i128], word: i128) -> i128 {
    match nodes[i] {
        CNode::Int(v) => v,
        CNode::Val(s) => vals[s],
        CNode::Word => word,
        CNode::Un(op, a) => {
            let x = d_eval(nodes, a, vals, word);
            if op == 0 { !x } else { x.wrapping_neg() }
        }
        CNode::Slice(a, hi, lo) => {
            let v = d_eval(nodes, a, vals, word) as u128;
            if lo >= 128 {
                0
            } else {
                let w = hi.saturating_sub(lo).saturating_add(1).min(128);
                ((v >> lo) & d_mask128(w)) as i128
            }
        }
        CNode::Bin(op, a, b) => {
            let x = d_eval(nodes, a, vals, word);
            let y = d_eval(nodes, b, vals, word);
            d_binop(op, x, y)
        }
        CNode::Tern(c, t, e) => {
            if d_eval(nodes, c, vals, word) != 0 {
                d_eval(nodes, t, vals, word)
            } else {
                d_eval(nodes, e, vals, word)
            }
        }
    }
}

#[cfg(feature = "disasm")]
fn d_dec(f: &mut core::fmt::Formatter<'_>, v: i128, signed: bool) -> core::fmt::Result {
    if signed { write!(f, "{}", v) } else { write!(f, "{}", v as u128) }
}

#[cfg(feature = "disasm")]
fn d_hex(f: &mut core::fmt::Formatter<'_>, v: i128, signed: bool, vw: u16, alt: bool, pad: usize) -> core::fmt::Result {
    let magnitude: u128 = if signed && v < 0 { (v as u128) & d_mask128(vw.max(1)) } else { v as u128 };
    if alt { f.write_str("0x")?; }
    write!(f, "{:0width$x}", magnitude, width = pad)
}

#[cfg(feature = "disasm")]
fn d_field(f: &mut core::fmt::Formatter<'_>, v: i128, rd: &DRend, fmt: &DFmt) -> core::fmt::Result {
    if fmt.hex { return d_hex(f, v, rd.signed, rd.value_width, fmt.alt, fmt.zero_pad); }
    if fmt.dec { return d_dec(f, v, rd.signed); }
    match rd.disp {
        0 => {
            let dec_s = if rd.signed { v.to_string() } else { (v as u128).to_string() };
            for (i, part) in rd.pattern.split("{}").enumerate() {
                if i > 0 { f.write_str(&dec_s)?; }
                f.write_str(part)?;
            }
            Ok(())
        }
        1 => d_hex(f, v, rd.signed, rd.value_width, true, 0),
        2 => if v < 0 { write!(f, "-0x{:x}", v.unsigned_abs()) } else { write!(f, "0x{:x}", v) },
        3 => d_dec(f, v, rd.signed),
        5 => {
            let k = v as u64;
            if let Some((_, s)) = rd.names.iter().find(|(kk, _)| *kk == k) {
                f.write_str(s)
            } else {
                match rd.names_def_kind {
                    1 => d_dec(f, v, rd.signed),
                    2 => d_hex(f, v, rd.signed, rd.value_width, true, 0),
                    3 => if v < 0 { write!(f, "-0x{:x}", v.unsigned_abs()) } else { write!(f, "0x{:x}", v) },
                    _ => f.write_str(rd.names_def),
                }
            }
        }
        _ => if rd.signed { d_dec(f, v, true) } else { d_hex(f, v, false, rd.value_width, true, 0) },
    }
}

#[cfg(feature = "disasm")]
fn d_render(f: &mut core::fmt::Formatter<'_>, word: i128, vals: &[i128], prog: &[DSeg]) -> core::fmt::Result {
    for seg in prog {
        match seg {
            DSeg::Lit(t) => f.write_str(t)?,
            DSeg::Field(slot, rend, fmt) => d_field(f, vals[*slot], &DRENDS[*rend], fmt)?,
            DSeg::Sub(slot, func) => f.write_str(&func(vals[*slot] as u64))?,
            DSeg::Cond(c, t, e) => {
                let branch = if d_eval(c, c.len() - 1, vals, word) != 0 { t } else { e };
                d_render(f, word, vals, branch)?;
            }
        }
    }
    Ok(())
}
"#;

/// A `DRend` data row (how to render one operand's value) as Rust source.
fn rend_literal(ty: &FieldTy) -> String {
    if let Disp::Names(t) = &ty.disp {
        let entries: String = t
            .entries
            .iter()
            .map(|(k, s)| format!("({k}u64, {s:?})"))
            .collect::<Vec<_>>()
            .join(", ");
        let (names_def, kind) = match &t.default {
            NameDefault::Str(s) => (format!("{s:?}"), 0u8),
            NameDefault::Hint(DispHint::Dec) => ("\"\"".to_string(), 1),
            NameDefault::Hint(DispHint::Hex) => ("\"\"".to_string(), 2),
            NameDefault::Hint(DispHint::SignedHex) => ("\"\"".to_string(), 3),
        };
        return format!(
            "DRend {{ signed: {}, value_width: {}, disp: 5, pattern: \"\", names: &[{entries}], names_def: {names_def}, names_def_kind: {kind} }}",
            ty.signed, ty.value_width
        );
    }
    let (disp, pattern) = match &ty.disp {
        Disp::Pattern(p) => (0, p.as_str()),
        Disp::Hint(DispHint::Hex) => (1, ""),
        Disp::Hint(DispHint::SignedHex) => (2, ""),
        Disp::Hint(DispHint::Dec) => (3, ""),
        Disp::Names(_) => unreachable!("names handled above"),
        Disp::None => (4, ""),
    };
    format!(
        "DRend {{ signed: {}, value_width: {}, disp: {}, pattern: {:?}, names: &[], names_def: \"\", names_def_kind: 0 }}",
        ty.signed, ty.value_width, disp, pattern
    )
}

/// Operand names that must be gathered into value slots: every field shown by a display arm, plus
/// every operand referenced inside an in-template conditional. In first-appearance order.
fn displayed_ops(inst: &Insn) -> Vec<String> {
    let mut ops: Vec<String> = Vec::new();
    for arm in &inst.display {
        collect_seg_ops(&arm.segs, &mut ops);
    }
    ops
}

fn collect_seg_ops(segs: &[Seg], ops: &mut Vec<String>) {
    for seg in segs {
        match seg {
            Seg::Lit(_) => {}
            Seg::Field { name, .. } => push_unique(ops, name),
            Seg::SubField { field, .. } => push_unique(ops, field),
            Seg::Cond { cond, then, els } => {
                collect_expr_ops(cond, ops);
                collect_seg_ops(then, ops);
                collect_seg_ops(els, ops);
            }
        }
    }
}

fn collect_expr_ops(e: &Expr, ops: &mut Vec<String>) {
    match e {
        Expr::Int(_) => {}
        Expr::Name(n) => {
            if n.text != "word" {
                push_unique(ops, &n.text);
            }
        }
        Expr::Slice { base, .. } => collect_expr_ops(base, ops),
        Expr::Unary { rhs, .. } => collect_expr_ops(rhs, ops),
        Expr::Binary { lhs, rhs, .. } => {
            collect_expr_ops(lhs, ops);
            collect_expr_ops(rhs, ops);
        }
        Expr::Cond {
            cond, then, els, ..
        } => {
            collect_expr_ops(cond, ops);
            collect_expr_ops(then, ops);
            collect_expr_ops(els, ops);
        }
        Expr::Call { args, .. } => args.iter().for_each(|a| collect_expr_ops(a, ops)),
        Expr::Assemble { parts, .. } => parts.iter().for_each(|p| collect_expr_ops(&p.src, ops)),
    }
}

fn push_unique(ops: &mut Vec<String>, name: &str) {
    if !ops.iter().any(|o| o == name) {
        ops.push(name.to_string());
    }
}

fn binop_code(op: BinOp) -> u8 {
    match op {
        BinOp::Add => 0,
        BinOp::Sub => 1,
        BinOp::Mul => 2,
        BinOp::Div => 3,
        BinOp::Rem => 4,
        BinOp::BitAnd => 5,
        BinOp::BitOr => 6,
        BinOp::BitXor => 7,
        BinOp::Shl => 8,
        BinOp::Shr => 9,
        BinOp::Eq => 10,
        BinOp::Ne => 11,
        BinOp::Lt => 12,
        BinOp::Le => 13,
        BinOp::Gt => 14,
        BinOp::Ge => 15,
        BinOp::LAnd => 16,
        BinOp::LOr => 17,
    }
}

/// Serialise a display condition into flat `CNode` rows (post-order, so the root is last). Returns
/// the root index. Mirrors `interp::eval_cond` (signed i128 semantics).
fn serialize_cond(e: &Expr, ops: &[String], nodes: &mut Vec<String>) -> usize {
    let lit = match e {
        Expr::Int(i) => format!("CNode::Int({}i128)", i.value as i128),
        Expr::Name(n) => {
            if n.text == "word" {
                "CNode::Word".to_string()
            } else {
                let slot = ops
                    .iter()
                    .position(|o| o == &n.text)
                    .expect("cond operand gathered");
                format!("CNode::Val({slot})")
            }
        }
        Expr::Unary { op, rhs, .. } => {
            let a = serialize_cond(rhs, ops, nodes);
            format!("CNode::Un({}, {a})", if *op == UnOp::Not { 0 } else { 1 })
        }
        Expr::Slice { base, hi, lo, .. } => {
            let a = serialize_cond(base, ops, nodes);
            format!("CNode::Slice({a}, {hi}, {lo})")
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            let a = serialize_cond(lhs, ops, nodes);
            let b = serialize_cond(rhs, ops, nodes);
            format!("CNode::Bin({}, {a}, {b})", binop_code(*op))
        }
        Expr::Cond {
            cond, then, els, ..
        } => {
            let c = serialize_cond(cond, ops, nodes);
            let t = serialize_cond(then, ops, nodes);
            let e = serialize_cond(els, ops, nodes);
            format!("CNode::Tern({c}, {t}, {e})")
        }
        // `assemble`/calls never appear in display conditions (the oracle treats them as 0).
        Expr::Assemble { .. } | Expr::Call { .. } => "CNode::Int(0i128)".to_string(),
    };
    nodes.push(lit);
    nodes.len() - 1
}

/// Build a data program (`&[DSeg]` contents) for one display arm, recursing into in-template
/// conditionals.
fn build_program(
    segs: &[Seg],
    ops: &[String],
    acc: &BTreeMap<String, (String, FieldTy)>,
    drends: &mut Vec<String>,
) -> String {
    let mut parts: Vec<String> = Vec::new();
    for seg in segs {
        match seg {
            Seg::Lit(t) => parts.push(format!("DSeg::Lit({t:?})")),
            Seg::Field { name, fmt } => {
                let slot = ops.iter().position(|o| o == name).expect("gathered slot");
                let (_, ty) = acc.get(name).expect("operand ty");
                let ri = intern(rend_literal(ty), drends);
                parts.push(format!(
                    "DSeg::Field({slot}, {ri}, DFmt {{ hex: {}, dec: {}, alt: {}, zero_pad: {} }})",
                    fmt.hex, fmt.dec, fmt.alt, fmt.zero_pad
                ));
            }
            Seg::Cond { cond, then, els } => {
                let mut nodes: Vec<String> = Vec::new();
                serialize_cond(cond, ops, &mut nodes);
                let then_p = build_program(then, ops, acc, drends);
                let els_p = build_program(els, ops, acc, drends);
                parts.push(format!(
                    "DSeg::Cond(&[{}], &[{}], &[{}])",
                    nodes.join(", "),
                    then_p,
                    els_p
                ));
            }
            Seg::SubField { field, output } => {
                let slot = ops.iter().position(|o| o == field).expect("gathered slot");
                let sdn = acc
                    .get(field)
                    .and_then(|(_, ty)| ty.subdecoder.clone())
                    .expect("subdecoder field");
                parts.push(format!("DSeg::Sub({slot}, {})", sub_fn_name(&sdn, output)));
            }
        }
    }
    parts.join(", ")
}

/// The emitted sub-render function name for a `(subdecoder, output)` pair.
fn sub_fn_name(subdecoder: &str, output: &str) -> String {
    format!("sub_{}_{}", sanitize(subdecoder), sanitize(output))
}

/// Emit one `fn sub_<name>_<output>(v: u64) -> String` per subdecoder output: an ordered chain of
/// `if (v & mask) == val` arms (most-specific-first), each rendering that arm's output template.
fn emit_sub_fns(isa: &Isa) -> String {
    let mut s = String::new();
    for sd in &isa.subdecoders {
        for oname in &sd.outputs {
            let _ = writeln!(
                s,
                "#[cfg(feature = \"disasm\")]\nfn {}(v: u64) -> String {{",
                sub_fn_name(&sd.name, oname)
            );
            for arm in &sd.arms {
                let segs = arm.output(oname).unwrap_or(&[]);
                let _ = writeln!(s, "    if (v & {:#x}) == {:#x} {{", arm.mask, arm.val);
                s.push_str("        let mut __s = String::new();\n");
                emit_sub_segs(segs, &arm.fields, &mut s);
                s.push_str("        return __s;\n    }\n");
            }
            s.push_str("    String::new()\n}\n");
        }
    }
    s
}

/// Emit the body that renders one subdecoder output template into `__s`, reading each field out of
/// the sub-value `v`. Conditionals and nested sub-fields are rejected during resolution.
fn emit_sub_segs(segs: &[Seg], fields: &[Field], s: &mut String) {
    for seg in segs {
        match seg {
            Seg::Lit(t) => {
                let _ = writeln!(s, "        __s.push_str({t:?});");
            }
            Seg::Field { name, fmt } => match fields.iter().find(|f| &f.name == name) {
                Some(f) => {
                    let mask = mask_u64(f.range.width());
                    let valexpr = format!("(((v >> {}) & {mask:#x}) as i128)", f.range.lo);
                    let rendered = ctx_value(&valexpr, fmt, Some(&f.ty));
                    let _ = writeln!(s, "        __s.push_str(&{rendered});");
                }
                None => {
                    let _ = writeln!(s, "        __s.push_str(\"{{{name}}}\");");
                }
            },
            Seg::Cond { .. } | Seg::SubField { .. } => {}
        }
    }
}

/// Intern a `DField` literal into the shared table, returning its index (the table is small).
fn intern(lit: String, table: &mut Vec<String>) -> usize {
    match table.iter().position(|x| *x == lit) {
        Some(i) => i,
        None => {
            table.push(lit);
            table.len() - 1
        }
    }
}

fn display_impl(isa: &Isa, m: &Model) -> String {
    let acc_names = computed_accessor_names(isa);
    let mut drends: Vec<String> = Vec::new();
    let mut prog_consts = String::new();
    let mut fns = String::new();
    let mut match_arms = String::new();
    let any = !isa.instrs.is_empty();

    // For a modal ISA the renderer lives behind a (word, combo) wrapper instead of `Display` on
    // `Instruction`, so classification uses the caller's mode combination.
    let recv = if m.modal { "self.0" } else { "self" };

    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        let name = sanitize(&inst.name);

        let acc = op_acc(&acc_names, inst);
        let ops = displayed_ops(inst);

        // Gather the operand values once, through the inline accessors.
        let (gather_let, gather_ref) = if ops.is_empty() {
            (String::new(), "&[]")
        } else {
            let gather: Vec<String> = ops
                .iter()
                .map(|n| format!("self.{} as i128", acc.get(n).expect("displayed op").0))
                .collect();
            (
                format!("let v: [i128; {}] = [{}]; ", ops.len(), gather.join(", ")),
                "&v",
            )
        };

        // One data program per display arm (in-template conditionals are handled inside).
        let mut arm_consts: Vec<String> = Vec::new();
        for (ai, arm) in inst.display.iter().enumerate() {
            let prog = build_program(&arm.segs, &ops, &acc, &mut drends);
            let cname = format!("DISP_{name}_{ai}");
            let _ = writeln!(
                prog_consts,
                "#[cfg(feature = \"disasm\")]\nstatic {cname}: &[DSeg] = &[{prog}];"
            );
            arm_consts.push(cname);
        }

        // A single unconditional arm renders directly. Arm-level conditions become an if/else chain
        // evaluated against `self`.
        let body = if inst.display.len() == 1 && inst.display[0].cond.is_none() {
            format!(
                "{{ {gather_let}d_render(f, self.0 as i128, {gather_ref}, {}) }}",
                arm_consts[0]
            )
        } else {
            let acc_map = acc_methods(&acc);
            let mut chain = String::new();
            let mut closed = false;
            for (ai, arm) in inst.display.iter().enumerate() {
                match &arm.cond {
                    Some(c) => chain.push_str(&format!(
                        "if ({}) != 0 {{ d_render(f, self.0 as i128, {gather_ref}, {}) }} else ",
                        emit_cond(c, "self", &acc_map, &BTreeMap::new()),
                        arm_consts[ai]
                    )),
                    None => {
                        chain.push_str(&format!(
                            "{{ d_render(f, self.0 as i128, {gather_ref}, {}) }}",
                            arm_consts[ai]
                        ));
                        closed = true;
                        break;
                    }
                }
            }
            if !closed {
                chain.push_str(&format!("{{ write!(f, {:?}) }}", inst.name));
            }
            format!("{{ {gather_let}{chain} }}")
        };

        let _ = writeln!(
            fns,
            "    fn fmt_{name}(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {body}"
        );
        let _ = writeln!(
            match_arms,
            "            opcode::{} => {recv}.fmt_{name}(f),",
            const_name(&inst.name)
        );
    }

    let mut s = String::new();
    if any {
        s.push_str(DISASM_RUNTIME);
        s.push_str("\n#[cfg(feature = \"disasm\")]\nstatic DRENDS: &[DRend] = &[\n");
        for d in &drends {
            let _ = writeln!(s, "    {d},");
        }
        s.push_str("];\n");
        s.push_str(&emit_sub_fns(isa));
        s.push_str(&prog_consts);
    }
    s.push_str("\n#[cfg(feature = \"disasm\")]\nimpl Instruction {\n");
    s.push_str(&fns);
    s.push_str("}\n");

    if m.modal {
        s.push_str("\n#[cfg(feature = \"disasm\")]\n");
        s.push_str(
            "/// An `Instruction` paired with the mode combination it should render under.\n",
        );
        s.push_str("struct DisasmIn(Instruction, usize);\n");
        s.push_str("\n#[cfg(feature = \"disasm\")]\nimpl core::fmt::Display for DisasmIn {\n");
        s.push_str("    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {\n");
        s.push_str("        match self.0.opcode_in(self.1) {\n");
        s.push_str(&match_arms);
        s.push_str("            _ => write!(f, \"(invalid)\"),\n        }\n    }\n}\n");
        s.push_str("\n#[cfg(feature = \"disasm\")]\nimpl Instruction {\n");
        s.push_str(
            "    /// Render the textual form under mode combination `combo` (see `pack_modes`).\n",
        );
        s.push_str("    pub fn disasm_in(self, combo: usize) -> String {\n");
        s.push_str("        DisasmIn(self, combo).to_string()\n");
        s.push_str("    }\n}\n");
    } else {
        s.push_str("\n#[cfg(feature = \"disasm\")]\nimpl core::fmt::Display for Instruction {\n");
        s.push_str("    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {\n");
        s.push_str("        match self.opcode() {\n");
        s.push_str(&match_arms);
        s.push_str("            _ => write!(f, \"(invalid)\"),\n        }\n    }\n}\n");
    }
    s
}

// ---------------------------------------------------------------- contextual disasm

fn disasm_ctx_support(isa: &Isa, m: &Model) -> String {
    let handle = isa.handle_ty();
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let acc_names = computed_accessor_names(isa);

    let mut s = String::new();
    s.push_str("\n/// Host context for contextual disassembly (stream bytes, symbols, modes).\n");
    s.push_str("pub trait DisasmCtx {\n    fn read_u8(&self, addr: u64) -> u8;\n");
    s.push_str("    fn symbol(&self, _addr: u64) -> Option<(String, u64)> { None }\n");
    s.push_str("    fn mode(&self, _name: &str) -> u64 { 0 }\n}\n\n");

    // stream operand accessors
    let mut emitted: HashSet<String> = HashSet::new();
    s.push_str("impl Instruction {\n");
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        // The running stream offset: a static byte count plus, once an expression-width fetch
        // precedes, dynamic byte-count terms over `ctx.mode(..)`.
        let mut off = wb;
        let mut dyn_terms: Vec<String> = Vec::new();
        for c in &inst.computed {
            let Some(arg) = fetch_expr(&c.expr) else {
                continue;
            };
            let const_bits = fetch_width(&c.expr);
            let method = comp_acc(&acc_names, inst, c);

            // Folded fetch operands share one accessor. Emit each unique name once, but always
            // advance the per-instruction stream offset.
            if emitted.insert(method.clone()) {
                let mask = mask_u64(c.ty.value_width);
                let ret = ret_type(&c.ty);

                let _ = writeln!(
                    s,
                    "    /// stream operand `{}` of `{}`, read at `pc`.",
                    c.name, inst.name
                );

                if let (Some(bits), true) = (const_bits, dyn_terms.is_empty()) {
                    // Static offset and width: a single-expression body.
                    let nb = (bits as usize).div_ceil(8);
                    let raw = byte_read(off, nb, isa.decoder.endian);
                    let val = if c.ty.signed {
                        format!("sext64(({raw}) & {mask:#x}, {}) as {ret}", c.ty.value_width)
                    } else {
                        format!("(({raw}) & {mask:#x}) as {ret}")
                    };
                    let _ = writeln!(
                        s,
                        "    #[inline]\n    pub fn {method}<C: DisasmCtx>(self, pc: u64, ctx: &C) -> {ret} {{ {val} }}"
                    );
                } else {
                    // Mode-dependent offset or width: compute both at runtime.
                    let off_expr = if dyn_terms.is_empty() {
                        format!("{off}u64")
                    } else {
                        format!("{off}u64 + {}", dyn_terms.join(" + "))
                    };
                    let bits_expr = fetch_bits_rust(isa, arg);
                    let shift = match isa.decoder.endian {
                        Endian::Little => "8 * i",
                        Endian::Big => "8 * (nb - 1 - i)",
                    };
                    let val = if c.ty.signed {
                        format!("sext64(raw & {mask:#x}, {}) as {ret}", c.ty.value_width)
                    } else {
                        format!("(raw & {mask:#x}) as {ret}")
                    };
                    let _ = writeln!(
                        s,
                        "    #[inline]\n    pub fn {method}<C: DisasmCtx>(self, pc: u64, ctx: &C) -> {ret} {{\n\
                         \x20       let off = {off_expr};\n\
                         \x20       let nb = ({bits_expr} + 7) / 8;\n\
                         \x20       let mut raw: u64 = 0;\n\
                         \x20       let mut i: u64 = 0;\n\
                         \x20       while i < nb {{\n\
                         \x20           raw |= (ctx.read_u8(pc + off + i) as u64) << ({shift});\n\
                         \x20           i += 1;\n\
                         \x20       }}\n\
                         \x20       {val}\n\
                         \x20   }}"
                    );
                }
            }

            match (const_bits, dyn_terms.is_empty()) {
                (Some(bits), true) => off += (bits as usize).div_ceil(8),
                _ => dyn_terms.push(format!("(({} + 7) / 8)", fetch_bits_rust(isa, arg))),
            }
        }
    }
    s.push_str("}\n\n");

    // stream_len, id-keyed so callers already holding an opcode id skip the re-classify.
    let (lp, la) = combo_threads(m);
    let combos = if m.modal { isa.mode_combos() } else { 1 };
    let per_inst: Vec<(usize, Vec<usize>)> = isa
        .instr_order()
        .iter()
        .map(|&idx| {
            let inst = &isa.instrs[idx];
            let extras = (0..combos)
                .map(|c| fetched_bytes_combo(isa, inst, c))
                .collect();
            (idx, extras)
        })
        .collect();
    // A combo-independent table leaves the combo parameter unread; underscore it then.
    let combo_used = per_inst
        .iter()
        .any(|(_, extras)| !extras.iter().all(|&e| e == extras[0]));
    let id_lp = if m.modal && !combo_used {
        "_combo: usize, "
    } else {
        lp
    };

    s.push_str("/// Total byte length of opcode `id` (window + fetched operands).\n");
    let _ = writeln!(
        s,
        "#[inline]\npub fn stream_len_of({id_lp}id: usize) -> u8 {{\n    match id {{"
    );
    for (idx, extras) in &per_inst {
        let inst = &isa.instrs[*idx];
        if extras.iter().all(|&e| e == 0) {
            continue;
        }
        if extras.iter().all(|&e| e == extras[0]) {
            let _ = writeln!(
                s,
                "        opcode::{} => {},",
                const_name(&inst.name),
                wb + extras[0]
            );
        } else {
            // Mode-dependent fetch widths: one arm per combination.
            let arms: Vec<String> = extras
                .iter()
                .enumerate()
                .map(|(c, e)| format!("{c} => {}", wb + e))
                .collect();
            let _ = writeln!(
                s,
                "        opcode::{} => match combo {{ {}, _ => {wb} }},",
                const_name(&inst.name),
                arms.join(", ")
            );
        }
    }
    let _ = writeln!(s, "        _ => {wb},\n    }}\n}}\n");

    s.push_str("/// Total byte length of the instruction at `word` (window + fetched operands).\n");
    let _ = writeln!(
        s,
        "#[inline]\npub fn stream_len({lp}word: {handle}) -> u8 {{ stream_len_of({la}classify({la}word)) }}\n"
    );
    s
}

fn byte_read(off: usize, nb: usize, endian: Endian) -> String {
    let mut terms = Vec::new();

    for i in 0..nb {
        let shift = match endian {
            Endian::Little => i * 8,
            Endian::Big => (nb - 1 - i) * 8,
        };
        terms.push(format!(
            "((ctx.read_u8(pc + {off} + {i}) as u64) << {shift})"
        ));
    }

    terms.join(" | ")
}

/// Whether an instruction's contextual disassembly is identical to its `Display` output, so its
/// `disasm_ctx` arm can delegate to `Display` rather than emit a second renderer: no fetched
/// operand, no `:sym`/`:rel` field and a single unconditional display arm (and `Display` is emitted
/// at all). This roughly halves the disassembly code emitted for symbol-only ISAs.
fn ctx_delegatable(m: &Model, inst: &Insn) -> bool {
    m.emit_display
        && inst.computed.iter().all(|c| !is_fetch(&c.expr))
        && inst.display.len() == 1
        && inst.display[0].cond.is_none()
        && !segs_have_sym(&inst.display[0].segs)
        && !has_signed_hex_field(inst, &inst.display[0].segs)
}

/// `Display` renders a signed value as explicit hex (`{x:x}`) using the accessor's full 32bit or
/// 64bit two's-complement form, while `disasm_ctx` and the oracle mask it to the value width. Such
/// an instruction must keep its own contextual renderer rather than delegate to `Display`.
fn has_signed_hex_field(inst: &Insn, segs: &[Seg]) -> bool {
    let acc = op_types(inst);
    segs.iter().any(|seg| match seg {
        Seg::Field { name, fmt } => fmt.hex && acc.get(name).map(|ty| ty.signed).unwrap_or(false),
        Seg::Cond { then, els, .. } => {
            has_signed_hex_field(inst, then) || has_signed_hex_field(inst, els)
        }
        Seg::SubField { .. } | Seg::Lit(_) => false,
    })
}

fn disasm_ctx_fn(isa: &Isa, m: &Model) -> String {
    let handle = isa.handle_ty();
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let acc_names = computed_accessor_names(isa);
    let read0 = {
        // read the opcode window from pc
        let mut terms = Vec::new();
        for i in 0..wb {
            let shift = match isa.decoder.endian {
                Endian::Little => i * 8,
                Endian::Big => (wb - 1 - i) * 8,
            };
            terms.push(format!("((ctx.read_u8(pc + {i}) as u64) << {shift})"));
        }
        terms.join(" | ")
    };

    let mut s = String::new();

    // One small contextual renderer per instruction (same rationale as `display_impl`), skipping
    // those that delegate to `Display`.
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if ctx_delegatable(m, inst) {
            continue;
        }
        let mut body = String::new();
        for c in &inst.computed {
            if is_fetch(&c.expr) {
                let _ = writeln!(
                    body,
                    "    let __op_{}: i128 = inst.{}(pc, ctx) as i128;",
                    sanitize(&c.name),
                    comp_acc(&acc_names, inst, c)
                );
            }
        }

        // The instruction's total byte length: static unless a fetch width is mode-dependent.
        let total = if fetch_has_expr(inst) {
            let terms: Vec<String> = inst
                .computed
                .iter()
                .filter_map(|c| fetch_expr(&c.expr))
                .map(|arg| format!("(({} + 7) / 8)", fetch_bits_rust(isa, arg)))
                .collect();
            let _ = writeln!(
                body,
                "    let __total = ({wb}u64 + {}) as u8;",
                terms.join(" + ")
            );
            "__total".to_string()
        } else {
            format!("{}", wb + fetched_bytes(inst))
        };

        body.push_str("    let mut __r = String::new();\n");
        body.push_str(&ctx_seg_stmts(isa, &acc_names, inst, &total));
        let _ = writeln!(body, "    (__r, {total})");

        let _ = writeln!(
            s,
            "#[cfg(feature = \"disasm\")]\nfn disasm_ctx_{}<C: DisasmCtx>(inst: Instruction, pc: u64, ctx: &C) -> (String, u8) {{\n{body}}}",
            sanitize(&inst.name)
        );
    }

    s.push_str("\n#[cfg(feature = \"disasm\")]\n");
    s.push_str(
        "/// Classify at `pc`, fetch stream operands, render with symbols. Returns (text, len).\n",
    );
    s.push_str("pub fn disasm_ctx<C: DisasmCtx>(pc: u64, ctx: &C) -> (String, u8) {\n");
    let _ = writeln!(s, "    let word = ({read0}) as {handle};");
    s.push_str("    let inst = Instruction(word);\n");
    if m.modal {
        let modes: Vec<String> = isa
            .modes
            .iter()
            .map(|md| format!("ctx.mode({:?})", md.name))
            .collect();
        let _ = writeln!(s, "    let combo = pack_modes({});", modes.join(", "));
        s.push_str("    match inst.opcode_in(combo) {\n");
    } else {
        s.push_str("    match inst.opcode() {\n");
    }
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if ctx_delegatable(m, inst) {
            // identical to `Display`. No fetched bytes, so the length is just the opcode window
            let _ = writeln!(
                s,
                "        opcode::{} => (inst.to_string(), {wb}),",
                const_name(&inst.name)
            );
        } else {
            let _ = writeln!(
                s,
                "        opcode::{} => disasm_ctx_{}(inst, pc, ctx),",
                const_name(&inst.name),
                sanitize(&inst.name)
            );
        }
    }

    let _ = writeln!(s, "        _ => (\"(invalid)\".to_string(), {wb}),");
    s.push_str("    }\n}\n");
    s
}

fn ctx_seg_stmts(
    isa: &Isa,
    acc_names: &HashMap<(String, String), String>,
    inst: &Insn,
    total: &str,
) -> String {
    let acc = op_acc(acc_names, inst);
    let cond_acc = acc_methods(&acc);
    // Mode reads inside display conditions resolve through the host context at runtime.
    let mode_raw: BTreeMap<String, String> = isa
        .modes
        .iter()
        .map(|m| (m.name.clone(), format!("ctx.mode({:?})", m.name)))
        .collect();
    let fetched: BTreeSet<String> = inst
        .computed
        .iter()
        .filter(|c| is_fetch(&c.expr))
        .map(|c| c.name.clone())
        .collect();

    let mut s = String::new();
    ctx_render_segs(
        inst.ctx_pick_arm(),
        &acc,
        &cond_acc,
        &mode_raw,
        &fetched,
        total,
        &mut s,
    );
    s
}

#[allow(clippy::too_many_arguments)]
fn ctx_render_segs(
    segs: &[Seg],
    acc: &BTreeMap<String, (String, FieldTy)>,
    cond_acc: &BTreeMap<String, String>,
    mode_raw: &BTreeMap<String, String>,
    fetched: &BTreeSet<String>,
    total: &str,
    s: &mut String,
) {
    for seg in segs {
        match seg {
            Seg::Lit(t) => {
                let _ = writeln!(s, "            __r.push_str({t:?});");
            }
            Seg::Cond { cond, then, els } => {
                let _ = writeln!(
                    s,
                    "            if ({}) != 0 {{",
                    emit_cond(cond, "inst", cond_acc, mode_raw)
                );
                ctx_render_segs(then, acc, cond_acc, mode_raw, fetched, total, s);
                s.push_str("            } else {\n");
                ctx_render_segs(els, acc, cond_acc, mode_raw, fetched, total, s);
                s.push_str("            }\n");
            }
            Seg::SubField { field, output } => {
                match acc
                    .get(field)
                    .and_then(|(method, ty)| ty.subdecoder.as_ref().map(|sd| (method, sd)))
                {
                    Some((method, sdn)) => {
                        let _ = writeln!(
                            s,
                            "            __r.push_str(&{}((inst.{method}) as u64));",
                            sub_fn_name(sdn, output)
                        );
                    }
                    None => {
                        let _ = writeln!(s, "            __r.push_str(\"{{{field}.{output}}}\");");
                    }
                }
            }
            Seg::Field { name, fmt } => {
                let valexpr = if fetched.contains(name) {
                    format!("__op_{}", sanitize(name))
                } else if let Some((method, _)) = acc.get(name) {
                    format!("(inst.{method} as i128)")
                } else if let Some(raw) = mode_raw.get(name) {
                    format!("(({raw}) as i128)")
                } else {
                    let _ = writeln!(s, "            __r.push_str(\"{{{name}}}\");");
                    continue;
                };

                let ty = acc.get(name).map(|(_, t)| t.clone());
                ctx_field_render(fmt, &valexpr, ty.as_ref(), total, s);
            }
        }
    }
}

/// The two `Some` arms of a `ctx.symbol(...)` match (exact-hit and named-plus-offset), emitted at a
/// given indent. Both the `:sym` and `:rel` renderers share them.
fn sym_hit_arms(indent: &str) -> String {
    format!(
        "{indent}Some((n, 0)) => __r.push_str(&n),\n\
         {indent}Some((n, o)) => {{ __r.push_str(&n); __r.push_str(&format!(\"+{{:#x}}\", o)); }}\n"
    )
}

fn ctx_field_render(fmt: &FmtSpec, val: &str, ty: Option<&FieldTy>, total: &str, s: &mut String) {
    if fmt.rel {
        let _ = writeln!(
            s,
            "            {{ let __abs = (((pc as i128) + ({total}) as i128 + ({val})) as u64) & 0xffff;"
        );
        s.push_str("              match ctx.symbol(__abs) {\n");
        s.push_str(&sym_hit_arms("                  "));
        s.push_str("                  None => __r.push_str(&format!(\"{:04x}\", __abs)),\n              } }\n");
        return;
    }
    if fmt.sym {
        let _ = writeln!(s, "            match ctx.symbol(({val}) as u64) {{");
        s.push_str(&sym_hit_arms("                "));
        let _ = writeln!(
            s,
            "                None => __r.push_str(&{}),",
            ctx_value(val, fmt, ty)
        );
        s.push_str("            }\n");
        return;
    }
    let _ = writeln!(s, "            __r.push_str(&{});", ctx_value(val, fmt, ty));
}

fn ctx_value(val: &str, fmt: &FmtSpec, ty: Option<&FieldTy>) -> String {
    if fmt.hex {
        let mask = ty.map(|t| mask_u64(t.value_width)).unwrap_or(u64::MAX);
        let pfx = if fmt.alt { "0x" } else { "" };
        let inner = if fmt.zero_pad > 0 {
            format!("{{:0{}x}}", fmt.zero_pad)
        } else {
            "{:x}".to_string()
        };
        return format!("format!(\"{pfx}{inner}\", ((({val}) as u64) & {mask:#x}))");
    }
    if fmt.dec {
        return format!("format!(\"{{}}\", {val})");
    }

    match ty.map(|t| &t.disp) {
        Some(Disp::Pattern(p)) => {
            let (pre, post) = p.split_once("{}").unwrap_or((p.as_str(), ""));
            format!("format!(\"{pre}{{}}{post}\", {val})")
        }
        Some(Disp::Hint(DispHint::SignedHex)) => format!(
            "{{ let v = {val}; if v < 0 {{ format!(\"-0x{{:x}}\", (v as i64).unsigned_abs()) }} else {{ format!(\"0x{{:x}}\", v) }} }}"
        ),
        Some(Disp::Hint(DispHint::Hex)) => format!("format!(\"0x{{:x}}\", {val})"),
        Some(Disp::Hint(DispHint::Dec)) => format!("format!(\"{{}}\", {val})"),
        Some(Disp::Names(t)) => {
            let arms: String = t
                .entries
                .iter()
                .map(|(k, s)| format!("{k}u64 => {s:?}.to_string(), "))
                .collect();
            let default = match &t.default {
                NameDefault::Str(s) => format!("{s:?}.to_string()"),
                NameDefault::Hint(DispHint::Dec) => format!("format!(\"{{}}\", {val})"),
                NameDefault::Hint(DispHint::Hex) => {
                    format!("format!(\"0x{{:x}}\", (({val}) as u64))")
                }
                NameDefault::Hint(DispHint::SignedHex) => format!(
                    "{{ let v = {val}; if v < 0 {{ format!(\"-0x{{:x}}\", (v as i64).unsigned_abs()) }} else {{ format!(\"0x{{:x}}\", v) }} }}"
                ),
            };
            format!("match ({val}) as u64 {{ {arms} _ => {default} }}")
        }
        Some(Disp::None) | None => {
            let is_signed = ty.map(|t| t.signed).unwrap_or(false);
            if is_signed {
                format!("format!(\"{{}}\", {val})")
            } else {
                let mask = ty.map(|t| mask_u64(t.value_width)).unwrap_or(u64::MAX);
                format!("format!(\"0x{{:x}}\", ((({val}) as u64) & {mask:#x}))")
            }
        }
    }
}

// ---------------------------------------------------------------- stubs

/// Emit an editable `impl Ops` skeleton.
pub fn emit_stubs(isa: &Isa) -> String {
    let mut s = String::new();
    let _ = writeln!(
        s,
        "// chipi `Ops` handler skeleton for decoder `{}`. Rename `MyCpu` and fill in the bodies.",
        isa.decoder.name
    );
    s.push_str("// Drive it with `run_ops(&mut cpu, word)`.\n");
    s.push_str("#![allow(unused_variables)]\nuse super::generated::{Instruction, Ops};\n\n");
    s.push_str(
        "#[derive(Default)]\npub struct MyCpu {\n    // interpreter / lowering state\n}\n\n",
    );

    s.push_str("impl Ops for MyCpu {\n");
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        let _ = writeln!(
            s,
            "    fn {}(&mut self, inst: Instruction) {{\n        todo!({:?})\n    }}",
            ident(&inst.name),
            inst.name
        );
    }
    s.push_str("}\n");
    s
}

// ---------------------------------------------------------------- nested-enum dispatch
//
// The enum backend is the fixed-width simple-ISA path: it decodes one fixed opcode window (plus
// fixed-size fetched operands) eagerly into a nested `Instruction` enum with every operand
// pre-extracted, so consumers match on variants instead of calling accessors. That eager shape is
// the point, and it fixes the scope. Everything that makes the instruction stream non-uniform
// stays out: variable `length` windows, `prefix` scans, expression-width fetches and subdecoder
// string outputs are refused with a `compile_error!` that points at the newtype backend, which
// supports all of them. Identity metadata (opcode ids, names, tags, mnemonic/form axes) and
// in-template display conditionals are supported; conditionals evaluate from the variant's bound
// operands, with a raw `word` re-read through `ctx` for conditions that reference it.

/// A spec feature outside the enum backend's scope, if any. Such specs get a clear
/// `compile_error!` rather than subtly wrong output; see the section comment above for the
/// scope rationale.
fn enum_unsupported(isa: &Isa) -> Option<&'static str> {
    if isa.length.is_some() {
        return Some("`length` (variable-window) specs");
    }
    if isa.instrs.iter().any(fetch_has_expr) {
        return Some("expression-width `fetch` operands");
    }
    if isa.prefix.is_some() {
        return Some("`prefix` specs");
    }
    if !isa.subdecoders.is_empty() {
        return Some("`subdecoder` specs");
    }
    None
}

/// The payload fields of one leaf, as `(binding ident, rust type)`, in-window fields first then
/// computed operands, both in declaration order.
fn variant_field_types(inst: &Insn) -> Vec<(String, &'static str)> {
    let mut v = Vec::new();
    for f in &inst.fields {
        v.push((ident(&f.name), ret_type(&f.ty)));
    }
    for c in &inst.computed {
        v.push((ident(&c.name), ret_type(&c.ty)));
    }
    v
}

/// The struct-variant declaration tail: ` { dp: u8, addr: u16 }`, or empty for a unit variant.
fn variant_decl(inst: &Insn) -> String {
    let fs = variant_field_types(inst);
    if fs.is_empty() {
        return String::new();
    }
    let body: Vec<String> = fs.iter().map(|(n, t)| format!("{n}: {t}")).collect();
    format!(" {{ {} }}", body.join(", "))
}

/// The struct-variant construction tail used inside `decode`: ` { dp: <extract>, addr: <extract> }`,
/// where each operand is read once from the decode-time `word` / `ctx`. Empty for a unit variant.
fn variant_ctor(isa: &Isa, inst: &Insn) -> String {
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let mut parts: Vec<String> = Vec::new();

    for f in &inst.fields {
        parts.push(format!(
            "{}: {}",
            ident(&f.name),
            accessor_body_base(f, "word")
        ));
    }

    let mut off = wb;
    for c in &inst.computed {
        if let Some(bits) = fetch_width(&c.expr) {
            let nb = (bits as usize).div_ceil(8);
            let raw = byte_read(off, nb, isa.decoder.endian);
            let mask = mask_u64(c.ty.value_width);

            let val = if c.ty.signed {
                format!(
                    "sext64(({raw}) & {mask:#x}, {}) as {}",
                    c.ty.value_width,
                    ret_type(&c.ty)
                )
            } else {
                format!("(({raw}) & {mask:#x}) as {}", ret_type(&c.ty))
            };
            parts.push(format!("{}: {val}", ident(&c.name)));
            off += nb;
        } else {
            let scope = Scope::Computed {
                fields: &inst.fields,
                window: isa.window_bits(),
                base: "word",
                vars: &[],
            };
            let body = emit_value(&c.expr, &scope);
            let cast = if c.ty.signed {
                format!("sext128(__v, {}) as {}", c.ty.value_width, ret_type(&c.ty))
            } else {
                format!(
                    "(__v & cmask128({})) as {}",
                    c.ty.value_width,
                    ret_type(&c.ty)
                )
            };
            parts.push(format!(
                "{}: {{ let __v: u128 = {body}; {cast} }}",
                ident(&c.name)
            ));
        }
    }

    if parts.is_empty() {
        return String::new();
    }
    format!(" {{ {} }}", parts.join(", "))
}

/// The full constructor expression for a leaf, nesting through its dispatch group if it has one.
fn variant_value(isa: &Isa, inst: &Insn, m: &Model) -> String {
    let fields = variant_ctor(isa, inst);
    if let Some((group, variant)) = m.grouped.get(&inst.name) {
        let gp = pascal(group);
        format!("Instruction::{gp}({gp}::{variant}{fields})")
    } else {
        format!("Instruction::{}{fields}", pascal(&inst.name))
    }
}

/// The binding pattern for a leaf (`Instruction::Store(Store::StaAbs { addr })`), binding every
/// operand by its ident so a `match` arm can read them.
fn leaf_pattern(inst: &Insn, m: &Model) -> String {
    let fs = variant_field_types(inst);
    let bind = if fs.is_empty() {
        String::new()
    } else {
        let names: Vec<String> = fs.iter().map(|(n, _)| n.clone()).collect();
        format!(" {{ {} }}", names.join(", "))
    };
    if let Some((group, variant)) = m.grouped.get(&inst.name) {
        let gp = pascal(group);
        format!("Instruction::{gp}({gp}::{variant}{bind})")
    } else {
        format!("Instruction::{}{bind}", pascal(&inst.name))
    }
}

/// A field-ignoring pattern for projection matches: ` { .. }` when the leaf carries operands.
fn variant_ignore(inst: &Insn) -> &'static str {
    if variant_field_types(inst).is_empty() {
        ""
    } else {
        " { .. }"
    }
}

/// Emit the nested `Instruction` enum plus one sub-enum per dispatch group.
fn enum_types(isa: &Isa, m: &Model) -> String {
    let by_name: HashMap<&str, &Insn> = isa.instrs.iter().map(|i| (i.name.as_str(), i)).collect();
    let handle = isa.handle_ty();
    let derive = "#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]\n";

    let mut s = String::new();
    for g in &isa.groups {
        let gp = pascal(&g.name);
        s.push_str(&format!("/// Dispatch group `{}`.\n", g.name));
        s.push_str(derive);
        let _ = writeln!(s, "pub enum {gp} {{");
        for mname in &g.members {
            let inst = by_name[mname.as_str()];
            let _ = writeln!(s, "    {}{},", pascal(mname), variant_decl(inst));
        }
        s.push_str("}\n\n");
    }

    s.push_str("/// One decoded instruction with its operands extracted and transformed.\n");
    s.push_str(derive);
    s.push_str("pub enum Instruction {\n");
    for g in &isa.groups {
        let gp = pascal(&g.name);
        let _ = writeln!(s, "    {gp}({gp}),");
    }
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if m.grouped.contains_key(&inst.name) {
            continue;
        }
        let _ = writeln!(s, "    {}{},", pascal(&inst.name), variant_decl(inst));
    }
    let _ = writeln!(s, "    Invalid {{ word: {handle} }},");
    s.push_str("}\n\n");
    s
}

/// The `DisasmCtx` trait the eager decoder reads its stream through (identical to the newtype one,
/// so a host can implement it once and feed either backend).
fn enum_ctx_trait() -> String {
    let mut s = String::new();
    s.push_str("/// Host context: supplies stream bytes and optional symbols to the decoder.\n");
    s.push_str("pub trait DisasmCtx {\n    fn read_u8(&self, addr: u64) -> u8;\n");
    s.push_str("    fn symbol(&self, _addr: u64) -> Option<(String, u64)> { None }\n");
    s.push_str("    fn mode(&self, _name: &str) -> u64 { 0 }\n}\n\n");
    s
}

/// Read the opcode window from `pc` through `ctx` as the handle type.
fn window_read(isa: &Isa) -> String {
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let mut terms = Vec::new();
    for i in 0..wb {
        let shift = match isa.decoder.endian {
            Endian::Little => i * 8,
            Endian::Big => (wb - 1 - i) * 8,
        };
        terms.push(format!("((ctx.read_u8(pc + {i}) as u64) << {shift})"));
    }
    terms.join(" | ")
}

/// Emit `OPCODE_LEN` and the eager `decode`.
fn enum_decode(isa: &Isa, m: &Model) -> String {
    let handle = isa.handle_ty();
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let (cp, ca) = combo_threads(m);
    let read0 = window_read(isa);

    let mut s = String::new();
    s.push_str("/// Total decoded byte length per opcode id (window + fetched operands).\n");
    let _ = writeln!(s, "pub static OPCODE_LEN: [u8; OPCODE_COUNT] = [");
    for op in isa.tree.opcodes.iter() {
        let len = if op.instr == usize::MAX {
            wb
        } else {
            wb + fetched_bytes(&isa.instrs[op.instr])
        };
        let _ = writeln!(s, "    {len},");
    }
    s.push_str("];\n\n");

    s.push_str("/// Decode one instruction at `pc`, reading operand bytes through `ctx`.\n");
    let _ = writeln!(
        s,
        "pub fn decode<C: DisasmCtx>({cp}pc: u64, ctx: &C) -> (Instruction, u8) {{"
    );
    let _ = writeln!(s, "    let word = ({read0}) as {handle};");
    let _ = writeln!(s, "    let id = classify({ca}word);");
    s.push_str("    let inst = match id {\n");
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        let _ = writeln!(
            s,
            "        opcode::{} => {},",
            const_name(&inst.name),
            variant_value(isa, inst, m)
        );
    }
    s.push_str("        _ => Instruction::Invalid { word },\n    };\n");
    s.push_str("    (inst, OPCODE_LEN[id])\n}\n\n");
    s
}

/// Emit `opcode_id` projection (on `Instruction` and each sub-enum) plus the `name`/`len`/`tags`
/// convenience methods that read the static tables.
fn enum_queries(isa: &Isa, m: &Model) -> String {
    let by_name: HashMap<&str, &Insn> = isa.instrs.iter().map(|i| (i.name.as_str(), i)).collect();
    let mut s = String::new();

    for g in &isa.groups {
        let gp = pascal(&g.name);
        let _ = writeln!(s, "impl {gp} {{");
        s.push_str("    /// The opcode id (dispatch-table index) of this instruction.\n");
        s.push_str("    pub fn opcode_id(&self) -> usize {\n        match self {\n");
        for mname in &g.members {
            let inst = by_name[mname.as_str()];
            let _ = writeln!(
                s,
                "            {gp}::{}{} => opcode::{},",
                pascal(mname),
                variant_ignore(inst),
                const_name(mname)
            );
        }
        s.push_str("        }\n    }\n}\n\n");
    }

    s.push_str("impl Instruction {\n");
    s.push_str("    /// The opcode id (dispatch-table index). `Invalid` is `opcode::INVALID`.\n");
    s.push_str("    pub fn opcode_id(&self) -> usize {\n        match self {\n");
    for g in &isa.groups {
        let gp = pascal(&g.name);
        let _ = writeln!(s, "            Instruction::{gp}(x) => x.opcode_id(),");
    }
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if m.grouped.contains_key(&inst.name) {
            continue;
        }
        let _ = writeln!(
            s,
            "            Instruction::{}{} => opcode::{},",
            pascal(&inst.name),
            variant_ignore(inst),
            const_name(&inst.name)
        );
    }
    s.push_str("            Instruction::Invalid { .. } => opcode::INVALID,\n        }\n    }\n");
    s.push_str("    /// The leaf mnemonic name (`\"invalid\"` for `Invalid`).\n");
    s.push_str("    pub fn name(&self) -> &'static str { OPCODE_NAMES[self.opcode_id()] }\n");
    s.push_str("    /// The total decoded byte length.\n");
    s.push_str("    pub fn len(&self) -> u8 { OPCODE_LEN[self.opcode_id()] }\n");
    if !isa.tags.is_empty() {
        s.push_str("    /// Tags carried by this instruction (e.g. address-mode tags).\n");
        s.push_str(
            "    pub fn tags(&self) -> &'static [&'static str] { OPCODE_TAGS[self.opcode_id()] }\n",
        );
    }
    s.push_str("}\n\n");
    s
}

/// Emit the feature-gated `render(pc, ctx) -> String`, formatting each leaf from its bound operands.
fn enum_render(isa: &Isa) -> String {
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let m = Model::new(isa);
    let handle = isa.handle_ty();
    let read0 = window_read(isa);

    let mut s = String::new();
    s.push_str("\n#[cfg(feature = \"disasm\")]\nimpl Instruction {\n");
    s.push_str("    /// Render assembly text, resolving symbols and relative targets.\n");
    s.push_str("    pub fn render<C: DisasmCtx>(&self, pc: u64, ctx: &C) -> String {\n");
    s.push_str("        let mut __r = String::new();\n        match self {\n");

    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        // Expression-width fetches are rejected by enum_unsupported, so this total is static.
        let total = format!("{}", wb + fetched_bytes(inst));
        let segs = inst.ctx_pick_arm();

        // Display conditions resolve names straight to runtime expressions: bound operand
        // locals first, then host mode reads, then the raw window word (re-read through `ctx`
        // at `pc`; the variant does not retain it).
        let mut raw: BTreeMap<String, String> = BTreeMap::new();
        for name in op_types(inst).keys() {
            raw.insert(name.clone(), format!("*{}", ident(name)));
        }
        for md in &isa.modes {
            raw.entry(md.name.clone())
                .or_insert_with(|| format!("ctx.mode({:?})", md.name));
        }
        raw.insert("word".to_string(), "__w".to_string());

        let mut body = String::new();
        if segs_cond_use_word(segs) {
            let _ = writeln!(body, "                let __w = ({read0}) as {handle};");
        }
        enum_render_segs(segs, inst, &total, &raw, &mut body);

        let _ = writeln!(s, "            {} => {{", leaf_pattern(inst, &m));
        s.push_str(&body);
        s.push_str("            }\n");
    }

    s.push_str("            Instruction::Invalid { .. } => __r.push_str(\"(invalid)\"),\n");
    s.push_str("        }\n        __r\n    }\n}\n");
    s
}

/// Does any in-template condition in `segs` (recursively) read the raw `word`?
fn segs_cond_use_word(segs: &[Seg]) -> bool {
    segs.iter().any(|seg| match seg {
        Seg::Cond { cond, then, els } => {
            cond_uses_word(cond) || segs_cond_use_word(then) || segs_cond_use_word(els)
        }
        _ => false,
    })
}

/// Does a display condition read the raw `word`? A hit only makes the renderer bind the `__w`
/// local; a `word` buried in an `Assemble`/`Call` (which `emit_cond` lowers to a constant)
/// merely leaves that binding unused.
fn cond_uses_word(e: &Expr) -> bool {
    let mut found = false;
    e.walk(&mut |x| {
        if let Expr::Name(n) = x {
            if n.text == "word" {
                found = true;
            }
        }
    });
    found
}

/// Emit the statements that render one display arm into `r`, reading each operand from its bound
/// local (`*ident`). Reuses the same value formatters as the newtype contextual disassembler so the
/// text matches the oracle exactly. `raw` carries the name-to-expression map for conditions.
fn enum_render_segs(
    segs: &[Seg],
    inst: &Insn,
    total: &str,
    raw: &BTreeMap<String, String>,
    s: &mut String,
) {
    let acc = op_types(inst);
    for seg in segs {
        match seg {
            Seg::Lit(t) => {
                let _ = writeln!(s, "                __r.push_str({t:?});");
            }
            Seg::Field { name, fmt } => match acc.get(name) {
                Some(ty) => {
                    let val = format!("(*{} as i128)", ident(name));
                    ctx_field_render(fmt, &val, Some(ty), total, s);
                }
                None => {
                    let _ = writeln!(s, "                __r.push_str(\"{{{name}}}\");");
                }
            },
            Seg::Cond { cond, then, els } => {
                let _ = writeln!(
                    s,
                    "                if ({}) != 0 {{",
                    emit_cond(cond, "self", &BTreeMap::new(), raw)
                );
                enum_render_segs(then, inst, total, raw, s);
                s.push_str("                } else {\n");
                enum_render_segs(els, inst, total, raw, s);
                s.push_str("                }\n");
            }
            // Subdecoder specs are rejected up front by `enum_unsupported`, so a `{field.output}`
            // segment can never reach the enum renderer.
            Seg::SubField { .. } => {}
        }
    }
}

/// Emit the nested-enum decoder module for `isa`.
fn emit_decoder_enum(isa: &Isa) -> String {
    let mut s = String::new();
    s.push_str(&header(isa));

    if let Some(reason) = enum_unsupported(isa) {
        let _ = writeln!(
            s,
            "compile_error!(\"chipi: the enum backend is the eager fixed-width decoder for simple \
             ISAs and does not support {reason}; use the default newtype backend \
             (`style = newtype`) for this spec\");"
        );
        return s;
    }

    let m = Model::new(isa);
    s.push_str(&preamble(&m));
    s.push_str(&opcode_consts(isa));
    s.push_str(&axis_consts_enum(isa));
    s.push_str(&classify(isa, &m));
    s.push_str(&user_fns(isa));
    s.push_str(&enum_types(isa, &m));
    s.push_str(&enum_ctx_trait());
    s.push_str(&enum_decode(isa, &m));
    s.push_str(&enum_queries(isa, &m));
    s.push_str(&enum_render(isa));
    s
}
