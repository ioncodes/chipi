//! Python 3 decoder emission from a resolved [`chipi_core::Isa`].
//!
//! [`emit_python`] produces one self-contained, dependency-free Python module: opcode names, a dense
//! classification path, shift/mask field accessors, computed-operand accessors, user `fn`s, an
//! optional embedded `length` and a disassembler. Specs using `fetch(N)` or `:sym`/`:rel` get the
//! contextual surface (a duck-typed `ctx` with `read_u8`/`symbol`/`mode`, stream accessors,
//! `stream_len`, `disasm_ctx(pc, ctx)`); prefix specs get `scan_prefixes`/`decode_stream` plus
//! `classify_with(word, ctx)` when guards read context fields. It is the IDA / Binary Ninja
//! sibling of the Rust and C++ backends and aims to match the `chipi_core::interp` oracle.
//!
//! The emitter keeps three numeric domains separate, matching the oracle. Field accessors
//! work in 64bit (with a 32/64bit return truncation). The computation layer (computed operands,
//! guards, `length`, `assemble`, builtins, fn bodies) works in unsigned 128bit. Display
//! conditions work in signed 128bit.

#![forbid(unsafe_code)]

mod exprgen;
mod names;

use chipi_core::accessor::computed_accessor_names;
use chipi_core::interp::{
    fetch_expr, fetch_has_expr, fetch_width, fetched_bytes, fetched_bytes_combo, is_fetch,
};
use chipi_core::model::*;
use chipi_core::render::{segs_have_sym, FmtSpec, Seg};
use chipi_core::tree::{Residual, Tree};
use chipi_core::Isa;
use exprgen::{emit_cond, emit_prefix, emit_value, width_mask, Scope};
use names::{cname, comp_acc, pascal, sanitize};

/// Re-exported so tests and tooling can reconstruct the exact handler-method names the `Ops`
/// dispatch layer emits (keyword-sanitised, e.g. `and` -> `and_`).
pub use names::ident;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Write as _;

/// Cross-cutting flags derived once (the Python sibling of the other backends' `Model`).
struct Model {
    modal: bool,
    has_guard: bool,
    /// A `fetch` operand or a `:sym`/`:rel` placeholder needs the `disasm_ctx` path.
    needs_disasm_ctx: bool,
    /// Whether the word-level `disasm` is emitted at all (mirrors the Rust backend's gate).
    emit_display: bool,
    /// Some guard reads a prefix-assigned context field, so a `classify_with(word, ctx)`
    /// entry point is emitted next to the default-context `classify(word)`.
    ctx_guards: bool,
}

impl Model {
    fn new(isa: &Isa) -> Self {
        let modal = !isa.modes.is_empty();
        let has_guard = isa.instrs.iter().any(|i| i.guard.is_some());
        let has_fetch = isa
            .instrs
            .iter()
            .any(|i| i.computed.iter().any(|c| is_fetch(&c.expr)));
        let needs_sym = isa
            .instrs
            .iter()
            .any(|i| i.display.iter().any(|a| segs_have_sym(&a.segs)));
        let needs_disasm_ctx = has_fetch || needs_sym;
        let emit_display = if modal { !needs_disasm_ctx } else { !has_fetch };

        let ctx_names: Vec<String> = isa.decoder.context.iter().map(|c| c.name.clone()).collect();
        let ctx_guards = !ctx_names.is_empty()
            && isa.prefix.is_some()
            && !modal
            && isa.instrs.iter().any(|i| {
                i.guard
                    .as_ref()
                    .is_some_and(|g| chipi_core::compute::expr_reads_any(g, &ctx_names))
            });

        Model {
            modal,
            has_guard,
            needs_disasm_ctx,
            emit_display,
            ctx_guards,
        }
    }
}

/// A reason string if the spec uses a feature the Python backend cannot emit yet, else `None`.
/// Mirrors the Rust backend's `unsupported_var_reads`.
fn py_unsupported(isa: &Isa, m: &Model) -> Option<&'static str> {
    if isa.display_reads_vars() && !(m.needs_disasm_ctx && !m.emit_display) {
        return Some(
            "display conditions reading decode variables outside the contextual \
             disassembler path",
        );
    }
    if isa.length_reads_vars() {
        return Some("`length` arms reading decode variables");
    }
    None
}

/// Emit a self-contained Python 3 decoder for `isa`.
pub fn emit_python(isa: &Isa) -> String {
    let m = Model::new(isa);
    let modal = m.modal;

    // Refuse the few shapes this backend still cannot evaluate at runtime by raising on import
    // rather than emitting silently divergent code.
    if let Some(reason) = py_unsupported(isa, &m) {
        return format!(
            "{}raise NotImplementedError(\"chipi: the Python backend does not support {reason}\")\n",
            header(isa)
        );
    }

    let mut s = String::new();

    s.push_str(&header(isa));
    s.push_str(&imports(isa));
    s.push_str(&opcode_names(isa));
    s.push_str(&tag_consts(isa));
    s.push_str(&axis_consts(isa));
    s.push_str(&preamble(isa, &m));

    s.push_str(&classify(isa, &m));
    s.push_str(&opcode_name_wrapper(modal));

    s.push_str(&field_accessors(isa));
    s.push_str(&user_fns(isa));
    s.push_str(&computed_accessors(isa));
    s.push_str(&length_emit(isa));
    s.push_str(&ops_dispatch(isa, modal));

    if isa.prefix.is_some() {
        s.push_str(&prefix_scan_py(isa, &m));
    }
    if m.needs_disasm_ctx {
        s.push_str(&disasm_ctx_support_py(isa, &m));
    }

    s.push_str(&sub_fns_py(isa));
    if m.emit_display {
        s.push_str(&disasm_emit(isa, modal));
    }
    if m.needs_disasm_ctx {
        s.push_str(&disasm_ctx_fn_py(isa, &m));
    }

    s
}

/// Standard-library imports the emitted module needs: `abc` for the `Ops` base class and `enum`
/// only when there are `dispatch` groups (which emit `IntEnum` kind types).
fn imports(isa: &Isa) -> String {
    let mut s = String::from("import abc\n");
    if !isa.groups.is_empty() {
        s.push_str("import enum\n");
    }
    s.push('\n');
    s
}

// ---------------------------------------------------------------- helpers

/// Whether any instruction declares a computed operand.
fn has_computed(isa: &Isa) -> bool {
    isa.instrs.iter().any(|i| !i.computed.is_empty())
}

/// Reverse map from instruction index to opcode id (table slot), 0 if it has none.
fn opcode_ids(isa: &Isa) -> Vec<usize> {
    let mut ids = vec![0usize; isa.instrs.len()];
    for (id, op) in isa.tree.opcodes.iter().enumerate() {
        if op.instr != usize::MAX {
            ids[op.instr] = id;
        }
    }
    ids
}

/// A 64bit-capped width mask literal as Python source.
fn mask64(w: u16) -> String {
    format!("{:#x}", chipi_core::compute::mask_u64(w))
}

/// Debug-format a string as a Python string literal (`{:?}` matches Python's escape conventions).
fn pystr(s: &str) -> String {
    format!("{s:?}")
}

// ---------------------------------------------------------------- sections

fn header(isa: &Isa) -> String {
    format!(
        "# generated by chipi (Python backend) for decoder `{}`. DO NOT EDIT.\n\
         # Output is reproducible; regenerate with `chipi emit`.\n\n",
        isa.decoder.name
    )
}

fn opcode_names(isa: &Isa) -> String {
    let mut s = String::from("OPCODE_NAMES = [\n");

    for op in isa.tree.opcodes.iter() {
        let _ = writeln!(s, "    {},", pystr(&op.name));
    }

    s.push_str("]\n\n");
    s
}

/// Identity axes: for dotted leaf names, the mnemonic/form name lists, per-opcode index
/// tables and lookup helpers, mirroring what the Rust backend exposes.
/// Tag metadata, mirroring the Rust backend's OPCODE_TAGS: a tuple of tag names per opcode id.
fn tag_consts(isa: &Isa) -> String {
    if isa.tags.is_empty() {
        return String::new();
    }
    let mut s = String::from("OPCODE_TAGS = [\n");
    for op in isa.tree.opcodes.iter() {
        let tags = if op.instr == usize::MAX {
            &[][..]
        } else {
            &isa.instrs[op.instr].tags[..]
        };
        let list: Vec<String> = tags.iter().map(|t| pystr(t)).collect();
        let _ = writeln!(
            s,
            "    ({}),",
            if list.is_empty() {
                String::new()
            } else {
                format!("{},", list.join(", "))
            }
        );
    }
    s.push_str("]\n\n");
    s.push_str("def tags(opcode):\n    return OPCODE_TAGS[opcode]\n\n");
    s
}

fn axis_consts(isa: &Isa) -> String {
    if !isa.has_axes() {
        return String::new();
    }
    let mnems = isa.mnemonics();
    let forms = isa.form_axes();
    let mut s = String::new();

    s.push_str("MNEMONIC_NAMES = [\n    \"Invalid\",\n");
    for mn in &mnems {
        let _ = writeln!(s, "    {},", pystr(mn));
    }
    s.push_str("]\n");

    s.push_str("FORM_NAMES = [\n    \"none\",\n");
    for f in &forms {
        let _ = writeln!(s, "    {},", pystr(f));
    }
    s.push_str("]\n\n");

    let mn_index = |name: &str| {
        mnems
            .iter()
            .position(|m| m == name)
            .map(|i| i + 1)
            .unwrap_or(0)
    };
    let f_index = |name: &str| {
        forms
            .iter()
            .position(|f| f == name)
            .map(|i| i + 1)
            .unwrap_or(0)
    };

    let mut row = String::new();
    for op in isa.tree.opcodes.iter() {
        let idx = if op.instr == usize::MAX {
            0
        } else {
            mn_index(&isa.instrs[op.instr].mnemonic)
        };
        let _ = write!(row, "{idx},");
    }
    let _ = writeln!(s, "OPCODE_MNEMONIC = [{row}]");

    let mut row = String::new();
    for op in isa.tree.opcodes.iter() {
        let idx = if op.instr == usize::MAX {
            0
        } else {
            isa.instrs[op.instr]
                .form
                .as_deref()
                .map(&f_index)
                .unwrap_or(0)
        };
        let _ = write!(row, "{idx},");
    }
    let _ = writeln!(s, "OPCODE_FORM = [{row}]\n");

    s.push_str("def mnemonic(opcode):\n    return OPCODE_MNEMONIC[opcode]\n\n");
    s.push_str("def form(opcode):\n    return OPCODE_FORM[opcode]\n\n");

    s
}

const RUNTIME_PREAMBLE: &str = r#"# ---- runtime preamble ----
_M64 = (1 << 64) - 1

def _mask64(w):
    return _M64 if w >= 64 else (1 << w) - 1

def _sext64(v, n):
    v &= _M64
    if n == 0 or n >= 64:
        return v - (1 << 64) if v & (1 << 63) else v
    v &= _mask64(n)
    return v - (1 << n) if v & (1 << (n - 1)) else v

def _rotl64(v, k, w):
    if w == 0:
        return 0
    m = _mask64(w)
    v &= m
    k %= w
    return v if k == 0 else ((v << k) | (v >> (w - k))) & m

def _rotr64(v, k, w):
    if w == 0:
        return 0
    return _rotl64(v, (w - (k % w)) % w, w)

# ---- disasm display-condition helpers (i128 wrapping semantics) ----
_M128 = (1 << 128) - 1

def _s128(v):
    v &= _M128
    return v - (1 << 128) if v & (1 << 127) else v

def _cshr128(a, b):
    return _s128(a >> b)

def _cdiv128(a, b):
    if b == 0:
        return 0
    q = abs(a) // abs(b)
    return _s128(-q if (a < 0) != (b < 0) else q)

def _crem128(a, b):
    if b == 0:
        return 0
    r = abs(a) % abs(b)
    return _s128(-r if a < 0 else r)

"#;

const COMPUTE_PREAMBLE: &str = r#"# ---- computation-layer preamble (computed operands) ----
def _cmask128(w):
    return _M128 if w >= 128 else (1 << w) - 1

def _sext128(v, n):
    v &= _M128
    if n == 0 or n >= 128:
        return v
    m = _cmask128(n)
    x = v & m
    return (x | (_M128 & ~m)) if (x >> (n - 1)) & 1 else x

def _rotl128(v, n, w, left):
    if w == 0:
        return 0
    m = _cmask128(w)
    v &= m
    n %= w
    if n == 0:
        return v
    (l, r) = (n, w - n) if left else (w - n, n)
    return ((v << l) | (v >> r)) & m

def _bitwidth128(v):
    v &= _M128
    return v.bit_length()

def _replicate128(v, elem, total):
    if elem == 0 or total == 0:
        return 0
    chunk = v & _cmask128(elem)
    out = 0
    sh = 0
    while sh < total:
        out |= chunk << sh
        sh += elem
    return out & _cmask128(total)

def _maskrange128(b, e, w):
    out = 0
    if b <= e:
        i = b
        while i <= e and i < w:
            out |= 1 << i
            i += 1
    else:
        i = b
        while i < w:
            out |= 1 << i
            i += 1
        j = 0
        while j <= e:
            out |= 1 << j
            j += 1
    return out & _cmask128(w)

def _popcount128(v):
    return bin(v & _M128).count('1')

def _ctz128(v, w):
    v &= _M128
    return w if v == 0 else (v & -v).bit_length() - 1

def _div128(a, b):
    return 0 if b == 0 else ((a // b) & _M128)

def _rem128(a, b):
    return 0 if b == 0 else ((a % b) & _M128)

"#;

fn preamble(isa: &Isa, m: &Model) -> String {
    let mut s = String::from(RUNTIME_PREAMBLE);

    if has_computed(isa) || !isa.fns.is_empty() || isa.length.is_some() || m.has_guard {
        s.push_str(COMPUTE_PREAMBLE);
    }

    s
}

/// A dense primary table as a Python list literal.
fn primary_table(tree: &Tree, suffix: &str) -> String {
    let base = tree.opcode_count();

    let mut row = String::new();
    for slot in &tree.slots {
        let v = slot.table_value(base);
        let _ = write!(row, "{v},");
    }

    format!("_PRIMARY{suffix} = [{row}]\n\n")
}

/// The body of a routing function (statements with 4-space indent, no `def` line).
/// Decode-variable reads inside chain guards fold through `subst` (mode combo values, context
/// defaults); names left over resolve through `ctx_vars` as runtime expressions.
fn routing_body(
    isa: &Isa,
    tree: &Tree,
    table: &str,
    subst: &[(String, u64, u16)],
    ctx_vars: &[(String, String, u16)],
) -> String {
    let base = tree.opcode_count();
    let p_lo = tree.primary.range.lo;
    let p_mask = mask64(tree.primary.range.width());

    let mut s = String::new();
    let _ = writeln!(s, "    primary = (word >> {p_lo}) & {p_mask}");

    if tree.residuals.is_empty() {
        let _ = writeln!(s, "    return {table}[primary]");
        return s;
    }

    let _ = writeln!(s, "    slot = {table}[primary]");

    for (ri, r) in tree.residuals.iter().enumerate() {
        let sentinel = base + ri;
        let _ = writeln!(s, "    if slot == {sentinel}:");

        match r {
            Residual::Keyed {
                key, arms, default, ..
            } => {
                let k_lo = key.range.lo;
                let k_mask = mask64(key.range.width());

                let _ = writeln!(s, "        k = (word >> {k_lo}) & {k_mask}");
                for (kv, id) in arms {
                    let _ = writeln!(s, "        if k == {kv:#x}: return {id}");
                }
                let _ = writeln!(s, "        return {default}");
            }
            Residual::Sparse { arms, .. } => {
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
                            format!(" and (({}) != 0)", emit_value(&g, &scope))
                        })
                        .unwrap_or_default();
                    let _ = writeln!(
                        s,
                        "        if (word & {:#x}) == {:#x}{guard}: return {}",
                        a.mask, a.val, a.opcode
                    );
                }
                s.push_str("        return 0\n");
            }
        }
    }

    s.push_str("    return slot\n");
    s
}

/// Per-guarded-opcode re-checks appended to a classify wrapper: decode variables fold through
/// `subst`, with any leftovers resolving through `ctx_vars` as runtime expressions.
fn guard_arms(
    isa: &Isa,
    subst: &[(String, u64, u16)],
    ctx_vars: &[(String, String, u16)],
) -> String {
    let mut s = String::new();
    let ids = opcode_ids(isa);
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if let Some(g) = &inst.guard {
            let id = ids[idx];
            let g = chipi_core::compute::subst_vars(g, subst);
            let scope = Scope::Computed {
                fields: &inst.fields,
                window: isa.window_bits(),
                base: "word",
                vars: ctx_vars,
            };
            let _ = writeln!(
                s,
                "    if __id == {id} and not (({}) != 0): return 0",
                emit_value(&g, &scope)
            );
        }
    }
    s
}

fn classify(isa: &Isa, m: &Model) -> String {
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

                let _ = writeln!(s, "def _classify_tree_{t}(word):");
                s.push_str(&routing_body(
                    isa,
                    tree,
                    &format!("_PRIMARY_{t}"),
                    &subst,
                    &[],
                ));
                s.push('\n');
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
                let _ = writeln!(s, "def _classify_raw_{i}(word):");
                s.push_str(&routing_body(
                    isa,
                    tree,
                    &format!("_PRIMARY_{t}"),
                    &subst,
                    &[],
                ));
                s.push('\n');
                format!("_classify_raw_{i}(word)")
            } else {
                format!("_classify_tree_{t}(word)")
            };

            if m.has_guard {
                let _ = writeln!(s, "def _classify_{i}(word):");
                let _ = writeln!(s, "    __id = {raw_call}");
                s.push_str(&guard_arms(isa, &subst, &[]));
                s.push_str("    return __id\n\n");
                dispatch_arms.push(format!("_classify_{i}(word)"));
            } else {
                dispatch_arms.push(raw_call);
            }
        }

        let _ = writeln!(s, "MODE_COMBOS = {}\n", isa.mode_combos());

        s.push_str("def classify(combo, word):\n");
        for (i, arm) in dispatch_arms.iter().enumerate() {
            let _ = writeln!(s, "    if combo == {i}: return {arm}");
        }
        s.push_str("    return 0\n\n");

        let params: Vec<String> = isa.modes.iter().map(|m| sanitize(&m.name)).collect();
        let _ = writeln!(s, "def pack_modes({}):", params.join(", "));
        s.push_str("    idx = 0\n    radix = 1\n");
        for m in &isa.modes {
            let _ = writeln!(
                s,
                "    idx += ({} % {c}) * radix; radix *= {c}",
                sanitize(&m.name),
                c = m.cardinality
            );
        }
        s.push_str("    return idx\n\n");

        return s;
    }

    s.push_str(&primary_table(&isa.tree, ""));

    // Word-level classify folds every decode variable to its default, matching the oracle's
    // `decode(word)` (this backend has no prefix scan, so there is no runtime context).
    let subst = isa.default_subst();

    if m.has_guard {
        s.push_str("def _classify_raw(word):\n");
        s.push_str(&routing_body(isa, &isa.tree, "_PRIMARY", &subst, &[]));
        s.push('\n');

        s.push_str("def classify(word):\n");
        s.push_str("    __id = _classify_raw(word)\n");
        s.push_str(&guard_arms(isa, &subst, &[]));
        s.push_str("    return __id\n\n");
    } else {
        s.push_str("def classify(word):\n");
        s.push_str(&routing_body(isa, &isa.tree, "_PRIMARY", &subst, &[]));
        s.push('\n');
    }

    if m.ctx_guards {
        let ctx_vars: Vec<(String, String, u16)> = isa
            .decoder
            .context
            .iter()
            .map(|c| (c.name.clone(), format!("ctx[{}]", pystr(&c.name)), c.width))
            .collect();

        s.push_str("def _classify_raw_with(word, ctx):\n");
        s.push_str(&routing_body(isa, &isa.tree, "_PRIMARY", &[], &ctx_vars));
        s.push('\n');

        s.push_str(
            "def classify_with(word, ctx):\n    \"\"\"Classify `word` under a scanned prefix context (see `scan_prefixes`).\"\"\"\n",
        );
        s.push_str("    __id = _classify_raw_with(word, ctx)\n");
        s.push_str(&guard_arms(isa, &[], &ctx_vars));
        s.push_str("    return __id\n\n");
    }

    s
}

fn opcode_name_wrapper(modal: bool) -> String {
    if modal {
        "def opcode_name_in(combo, word):\n    return OPCODE_NAMES[classify(combo, word)]\n\n"
            .to_string()
    } else {
        "def opcode_name(word):\n    return OPCODE_NAMES[classify(word)]\n\n".to_string()
    }
}

fn field_accessors(isa: &Isa) -> String {
    let mut s = String::new();

    for (name, f) in isa.unique_fields() {
        let _ = writeln!(s, "def {}(word):", ident(&name));
        let _ = writeln!(
            s,
            "    v = (word >> {}) & {}",
            f.range.lo,
            mask64(f.range.width())
        );

        let mut signed = false;
        for x in &f.ty.xforms {
            match x {
                Xform::ShiftLeft(n) => {
                    if signed {
                        let _ = writeln!(s, "    v = _sext64((v << {n}) & _M64, 64)");
                    } else {
                        let _ = writeln!(s, "    v = (v << {n}) & _M64");
                    }
                }
                Xform::ShiftRight(n) => {
                    let _ = writeln!(s, "    v = v >> {n}");
                }
                Xform::ZeroExtend(n) => {
                    let _ = writeln!(s, "    v &= {}", mask64(*n));
                    signed = false;
                }
                Xform::SignExtend(n) => {
                    let _ = writeln!(s, "    v = _sext64(v & _M64, {n})");
                    signed = true;
                }
                Xform::RotateLeft(k, w) => {
                    let _ = writeln!(s, "    v = _rotl64(v & _M64, {k}, {w})");
                    signed = false;
                }
                Xform::RotateRight(k, w) => {
                    let _ = writeln!(s, "    v = _rotr64(v & _M64, {k}, {w})");
                    signed = false;
                }
            }
        }
        // return-type truncation
        let bits = if f.ty.value_width <= 32 { 32u32 } else { 64 };
        let m = if bits >= 64 {
            u64::MAX
        } else {
            (1u64 << bits) - 1
        };
        let _ = writeln!(s, "    v &= {m:#x}");

        if f.ty.signed {
            let sign = 1u128 << (bits - 1);
            let span = 1u128 << bits;
            let _ = writeln!(s, "    if v & {sign:#x}: v -= {span:#x}");
        }

        s.push_str("    return v\n\n");
    }

    s
}

fn user_fns(isa: &Isa) -> String {
    if isa.fns.is_empty() {
        return String::new();
    }

    let mut s = String::from("# ---- user fns (computation layer) ----\n");

    for f in &isa.fns {
        let params: Vec<String> = f
            .params
            .iter()
            .map(|(n, _)| format!("v_{}", sanitize(n)))
            .collect();
        let _ = writeln!(s, "def fn_{}({}):", sanitize(&f.name), params.join(", "));

        let mut widths: HashMap<String, u16> = f
            .params
            .iter()
            .map(|(n, ty)| (n.clone(), ty.width()))
            .collect();
        for (n, ty) in &f.params {
            let _ = writeln!(
                s,
                "    v_{} = v_{} & _cmask128({})",
                sanitize(n),
                sanitize(n),
                ty.width()
            );
        }

        for (ln, le, lw) in &f.lets {
            let scope = Scope::Fn {
                widths: widths.clone(),
            };
            let _ = writeln!(s, "    v_{} = {}", sanitize(ln), emit_value(le, &scope));

            widths.insert(ln.clone(), *lw);
        }

        let scope = Scope::Fn { widths };
        let _ = writeln!(
            s,
            "    return ({}) & _cmask128({})",
            emit_value(&f.ret_expr, &scope),
            f.ret.width()
        );

        s.push('\n');
    }

    s
}

fn computed_accessors(isa: &Isa) -> String {
    if !has_computed(isa) {
        return String::new();
    }

    let acc = computed_accessor_names(isa);
    let mut emitted: HashSet<String> = HashSet::new();
    let mut s = String::new();

    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        for c in &inst.computed {
            // Stream `fetch` operands read bytes past the opcode window; their accessors live in
            // the contextual-disassembly section and take `(pc, ctx)` instead of the word.
            if is_fetch(&c.expr) {
                continue;
            }
            // Folded operands share one accessor; emit each unique name once.
            let method = comp_acc(&acc, &inst.name, &c.name);
            if !emitted.insert(method.clone()) {
                continue;
            }

            let scope = Scope::Computed {
                fields: &inst.fields,
                window: isa.window_bits(),
                base: "word",
                vars: &[],
            };

            let _ = writeln!(s, "def {method}(word):");
            let _ = writeln!(s, "    __v = ({}) & _M128", emit_value(&c.expr, &scope));

            if c.ty.signed {
                let _ = writeln!(s, "    __v = _sext128(__v, {})", c.ty.value_width);
                s.push_str("    return __v - (1 << 128) if __v & (1 << 127) else __v\n");
            } else {
                let _ = writeln!(s, "    return __v & _cmask128({})", c.ty.value_width);
            }

            s.push('\n');
        }
    }

    s
}

fn length_emit(isa: &Isa) -> String {
    let mut s = String::new();
    match &isa.length {
        None => {
            let _ = writeln!(s, "LEN = {}\n", isa.max_len_bytes);
            s.push_str("def inst_len(word):\n    return LEN\n\n");
        }
        Some(len) => {
            s.push_str("def inst_len_bits(word):\n");

            for arm in &len.arms {
                match &arm.cond {
                    Some(c) => {
                        // length conditions use the computation layer and reference only `word`.
                        let scope = Scope::Computed {
                            fields: &[],
                            window: isa.window_bits(),
                            base: "word",
                            vars: &[],
                        };
                        let _ = writeln!(
                            s,
                            "    if ({}) != 0: return {}",
                            emit_value(c, &scope),
                            arm.bits
                        );
                    }
                    None => {
                        let _ = writeln!(s, "    return {}", arm.bits);
                    }
                }
            }

            s.push_str("    return 0\n\n");
            s.push_str("def inst_len(word):\n    return (inst_len_bits(word) + 7) // 8\n\n");
        }
    }
    s
}

// ---------------------------------------------------------------- dispatch (Ops)

/// Maps each member name to its group name, for every instruction folded into a `dispatch` group.
fn grouped_members(isa: &Isa) -> HashMap<String, String> {
    let mut grouped = HashMap::new();
    for g in &isa.groups {
        for member in &g.members {
            grouped.insert(member.clone(), g.name.clone());
        }
    }
    grouped
}

/// The operand-signature `Ops` dispatch layer, mirroring the Rust and C++ backends: module-level
/// `OP_*` opcode-id constants, an `IntEnum` kind per `dispatch` group, an `Ops` base class whose
/// handlers receive the decoded word and `dispatch_ops`/`run_ops`. Each group folds its members
/// behind one handler (defaulting to the per-member handlers); every ungrouped instruction is an
/// `abc.abstractmethod` the consumer must implement.
fn ops_dispatch(isa: &Isa, modal: bool) -> String {
    let grouped = grouped_members(isa);

    let mut s = String::new();
    s.push_str("# ---- context-generic dispatch (Ops) ----\n");

    for (id, op) in isa.tree.opcodes.iter().enumerate() {
        let _ = writeln!(s, "OP_{} = {id}", cname(&op.name));
    }
    s.push('\n');

    for g in &isa.groups {
        let _ = writeln!(s, "class {}Kind(enum.IntEnum):", pascal(&g.name));
        for (i, member) in g.members.iter().enumerate() {
            let _ = writeln!(s, "    {} = {i}", cname(member));
        }
        s.push('\n');
    }

    s.push_str("class Ops(abc.ABC):\n");
    s.push_str(
        "    \"\"\"Implement once per consumer; pull operands via the accessors above.\"\"\"\n\n",
    );

    for g in &isa.groups {
        let kind = format!("{}Kind", pascal(&g.name));
        let _ = writeln!(s, "    def {}(self, op, inst):", ident(&g.name));
        for (i, member) in g.members.iter().enumerate() {
            let kw = if i == 0 { "if" } else { "elif" };
            let _ = writeln!(s, "        {kw} op == {kind}.{}:", cname(member));
            let _ = writeln!(s, "            self.{}(inst)", ident(member));
        }
        s.push('\n');
        for member in &g.members {
            let _ = writeln!(s, "    def {}(self, inst):", ident(member));
            s.push_str("        pass\n\n");
        }
    }

    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        if grouped.contains_key(&inst.name) {
            continue;
        }
        s.push_str("    @abc.abstractmethod\n");
        let _ = writeln!(s, "    def {}(self, inst):", ident(&inst.name));
        s.push_str("        ...\n\n");
    }

    s.push_str("    def on_invalid(self, inst):\n        pass\n\n\n");

    let (sig, classify_args) = if modal {
        ("combo, h, inst", "combo, inst")
    } else {
        ("h, inst", "inst")
    };
    let _ = writeln!(s, "def dispatch_ops({sig}):");
    let _ = writeln!(s, "    op = classify({classify_args})");
    let mut first = true;
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        let kw = if first { "if" } else { "elif" };
        first = false;
        if let Some(group) = grouped.get(&inst.name) {
            let _ = writeln!(s, "    {kw} op == OP_{}:", cname(&inst.name));
            let _ = writeln!(
                s,
                "        h.{}({}Kind.{}, inst)",
                ident(group),
                pascal(group),
                cname(&inst.name)
            );
        } else {
            let _ = writeln!(s, "    {kw} op == OP_{}:", cname(&inst.name));
            let _ = writeln!(s, "        h.{}(inst)", ident(&inst.name));
        }
    }
    s.push_str("    else:\n        h.on_invalid(inst)\n\n\n");

    let (run_sig, dispatch_args) = if modal {
        ("combo, h, word", "combo, h, word")
    } else {
        ("h, word", "h, word")
    };
    let _ = writeln!(s, "def run_ops({run_sig}):");
    let _ = writeln!(s, "    dispatch_ops({dispatch_args})");
    s.push_str("    return inst_len(word)\n\n");

    s
}

// ---------------------------------------------------------------- disassembly

/// Resolve a `{name}` placeholder: computed operand, else bound field, else fallback.
fn render_field_py(
    accmap: &HashMap<(String, String), String>,
    inst: &Insn,
    name: &str,
    fmt: &FmtSpec,
) -> String {
    if let Some(c) = inst.computed.iter().find(|c| c.name == name) {
        let acc = format!("{}(word)", comp_acc(accmap, &inst.name, name));
        return render_value_py(&c.ty, &acc, fmt);
    }

    if let Some(f) = inst.fields.iter().find(|f| f.name == name) {
        let acc = format!("{}(word)", ident(name));
        return render_value_py(&f.ty, &acc, fmt);
    }

    format!("str({}(word))", ident(name))
}

/// Render one operand value against its type's display spec and a placeholder format.
fn render_value_py(ty: &FieldTy, acc: &str, fmt: &FmtSpec) -> String {
    if fmt.hex {
        return py_hex_str(ty, acc, fmt.alt, fmt.zero_pad);
    }
    if fmt.dec {
        return py_dec_str(ty, acc);
    }

    match &ty.disp {
        Disp::Pattern(p) => {
            // Substitute the value at every `{}`, matching render::render_field's
            // `p.replace("{}", ...)` and the C++ backend.
            let val = py_dec_str(ty, acc);
            let parts: Vec<&str> = p.split("{}").collect();

            let mut pieces: Vec<String> = Vec::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    pieces.push(val.clone());
                }
                if !part.is_empty() {
                    pieces.push(pystr(part));
                }
            }

            if pieces.is_empty() {
                pystr("")
            } else {
                pieces.join(" + ")
            }
        }
        Disp::Hint(DispHint::Hex) => py_hex_str(ty, acc, true, 0),
        Disp::Hint(DispHint::SignedHex) => {
            format!("((\"-0x%x\" % -({acc})) if ({acc}) < 0 else (\"0x%x\" % ({acc})))")
        }
        Disp::Hint(DispHint::Dec) => py_dec_str(ty, acc),
        Disp::Names(t) => {
            let default = match &t.default {
                NameDefault::Str(s) => pystr(s),
                NameDefault::Hint(DispHint::Dec) => py_dec_str(ty, acc),
                NameDefault::Hint(DispHint::Hex) => py_hex_str(ty, acc, true, 0),
                NameDefault::Hint(DispHint::SignedHex) => {
                    format!("((\"-0x%x\" % -({acc})) if ({acc}) < 0 else (\"0x%x\" % ({acc})))")
                }
            };
            let mut dict = String::from("{");
            for (i, (k, name)) in t.entries.iter().enumerate() {
                if i > 0 {
                    dict.push_str(", ");
                }
                dict.push_str(&format!("{k}: {}", pystr(name)));
            }
            dict.push('}');
            format!("({dict}.get(({acc}), {default}))")
        }
        Disp::None => {
            if ty.signed {
                py_dec_str(ty, acc)
            } else {
                py_hex_str(ty, acc, true, 0)
            }
        }
    }
}

fn py_dec_str(ty: &FieldTy, acc: &str) -> String {
    if ty.signed {
        format!("str({acc})")
    } else {
        format!("str(({acc}) & {})", width_mask(ty.value_width))
    }
}

fn py_hex_str(ty: &FieldTy, acc: &str, alt: bool, zero_pad: usize) -> String {
    let mag = if ty.signed {
        format!(
            "((({acc}) & {}) if ({acc}) < 0 else ({acc}))",
            width_mask(ty.value_width.max(1))
        )
    } else {
        format!("(({acc}) & {})", width_mask(ty.value_width))
    };

    let inner = if zero_pad > 0 {
        format!("(\"%0{zero_pad}x\" % {mag})")
    } else {
        format!("(\"%x\" % {mag})")
    };

    if alt {
        format!("(\"0x\" + {inner})")
    } else {
        inner
    }
}

/// A Python string-concatenation expression for a list of segments (always begins with `""`).
fn arm_string_expr(
    accmap: &HashMap<(String, String), String>,
    inst: &Insn,
    segs: &[Seg],
) -> String {
    let mut s = String::from("\"\"");

    for seg in segs {
        match seg {
            Seg::Lit(t) => {
                let _ = write!(s, " + {}", pystr(t));
            }
            Seg::Field { name, fmt } => {
                let _ = write!(s, " + {}", render_field_py(accmap, inst, name, fmt));
            }
            Seg::Cond { cond, then, els } => {
                let _ = write!(
                    s,
                    " + (({}) if ({}) != 0 else ({}))",
                    arm_string_expr(accmap, inst, then),
                    emit_cond(cond, inst, accmap, &BTreeMap::new()),
                    arm_string_expr(accmap, inst, els)
                );
            }
            Seg::SubField { field, output } => {
                if let Some(f) = inst.fields.iter().find(|f| &f.name == field) {
                    if let Some(sdn) = &f.ty.subdecoder {
                        let _ = write!(
                            s,
                            " + {}({}(word))",
                            sub_fn_name_py(sdn, output),
                            ident(field)
                        );
                    }
                }
            }
        }
    }

    s
}

/// The emitted sub-render function name for a `(subdecoder, output)` pair.
fn sub_fn_name_py(subdecoder: &str, output: &str) -> String {
    format!("sub_{}_{}", sanitize(subdecoder), sanitize(output))
}

/// Emit one `def sub_<name>_<output>(v)` per subdecoder output: an ordered chain of
/// `if (v & mask) == val: return <template>` arms (most-specific-first).
fn sub_fns_py(isa: &Isa) -> String {
    let mut s = String::new();
    for sd in &isa.subdecoders {
        for oname in &sd.outputs {
            let _ = writeln!(s, "def {}(v):", sub_fn_name_py(&sd.name, oname));
            for arm in &sd.arms {
                let segs = arm.output(oname).unwrap_or(&[]);
                let _ = writeln!(
                    s,
                    "    if (v & {:#x}) == {:#x}: return {}",
                    arm.mask,
                    arm.val,
                    sub_segs_py(segs, &arm.fields)
                );
            }
            s.push_str("    return \"\"\n\n");
        }
    }
    s
}

fn sub_segs_py(segs: &[Seg], fields: &[Field]) -> String {
    let mut parts: Vec<String> = vec!["\"\"".to_string()];
    for seg in segs {
        match seg {
            Seg::Lit(t) => parts.push(pystr(t)),
            Seg::Field { name, fmt } => {
                if let Some(f) = fields.iter().find(|f| &f.name == name) {
                    let valexpr = format!("((v >> {}) & {})", f.range.lo, mask64(f.range.width()));
                    parts.push(render_value_py(&f.ty, &valexpr, fmt));
                }
            }
            // Conditionals and nested sub-fields are rejected in subdecoder outputs at resolution.
            Seg::Cond { .. } | Seg::SubField { .. } => {}
        }
    }
    parts.join(" + ")
}

fn disasm_emit(isa: &Isa, modal: bool) -> String {
    let accmap = computed_accessor_names(isa);
    let mut s = String::new();

    if modal {
        s.push_str("def disasm(combo, word):\n    op = classify(combo, word)\n");
    } else {
        s.push_str("def disasm(word):\n    op = classify(word)\n");
    }

    let ids = opcode_ids(isa);
    let mut first = true;
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        let id = ids[idx];

        let kw = if first { "if" } else { "elif" };
        first = false;

        let _ = writeln!(s, "    {kw} op == {id}:");
        s.push_str(&disasm_arms(&accmap, inst));
    }

    s.push_str("    return \"(invalid)\"\n");
    s
}

/// The display-arm chain for one instruction (8-space indented statements under the `op ==` branch).
fn disasm_arms(accmap: &HashMap<(String, String), String>, inst: &Insn) -> String {
    let mut s = String::new();

    let mut first = true;
    let mut had_uncond = false;
    for arm in &inst.display {
        match &arm.cond {
            Some(c) => {
                let kw = if first { "if" } else { "elif" };
                let _ = writeln!(
                    s,
                    "        {kw} ({}) != 0:",
                    emit_cond(c, inst, accmap, &BTreeMap::new())
                );
                let _ = writeln!(
                    s,
                    "            return {}",
                    arm_string_expr(accmap, inst, &arm.segs)
                );
                first = false;
            }
            None => {
                if first {
                    let _ = writeln!(
                        s,
                        "        return {}",
                        arm_string_expr(accmap, inst, &arm.segs)
                    );
                } else {
                    s.push_str("        else:\n");
                    let _ = writeln!(
                        s,
                        "            return {}",
                        arm_string_expr(accmap, inst, &arm.segs)
                    );
                }
                had_uncond = true;
                break;
            }
        }
    }

    if !had_uncond {
        let _ = writeln!(s, "        return {}", pystr(&inst.name));
    }

    s
}

// ---------------------------------------------------------------- prefix scan

/// `scan_prefixes`, `_read_window` and (for non-modal specs) `decode_stream`, mirroring the Rust
/// backend and the oracle's `interp::decode_stream`. The context travels as a plain dict keyed by
/// the declared field names.
fn prefix_scan_py(isa: &Isa, m: &Model) -> String {
    let prefix = isa.prefix.as_ref().unwrap();
    let nbytes = (isa.window_bits() as usize).div_ceil(8);

    let mut s = String::from("# ---- prefix scan ----\n");
    s.push_str("def scan_prefixes(data):\n");
    s.push_str(
        "    \"\"\"Scan leading prefix units. Returns (consumed count, context dict).\"\"\"\n",
    );

    let inits: Vec<String> = isa
        .decoder
        .context
        .iter()
        .map(|c| format!("{}: {}", pystr(&c.name), c.default))
        .collect();
    let _ = writeln!(s, "    ctx = {{{}}}", inits.join(", "));
    s.push_str("    cursor = 0\n");
    s.push_str("    while cursor < len(data):\n");
    s.push_str("        byte = data[cursor]\n");

    for arm in &prefix.arms {
        let cond = match arm.pat {
            PrefixPat::Byte(b) => format!("byte == {b:#x}"),
            PrefixPat::Range(lo, hi) => format!("{lo:#x} <= byte <= {hi:#x}"),
            PrefixPat::Wildcard => "True".to_string(),
        };
        let _ = writeln!(s, "        if {cond}:");

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
                "            ctx[{}] = ({}) & {:#x}",
                pystr(name),
                emit_prefix(e),
                chipi_core::compute::mask_u64(width)
            );
        }

        match arm.term {
            PrefixTerm::Continue => s.push_str("            cursor += 1\n            continue\n"),
            PrefixTerm::Finish => s.push_str("            cursor += 1\n            break\n"),
            PrefixTerm::Done => s.push_str("            break\n"),
        }
    }
    s.push_str("        break\n");
    s.push_str("    return (cursor, ctx)\n\n");

    // The opcode window starting at `at`, assembled with the decoder's byte order (missing bytes
    // read as zero).
    s.push_str("def _read_window(data, at):\n    w = 0\n");
    match isa.decoder.endian {
        Endian::Little => {
            for i in 0..nbytes {
                let _ = writeln!(
                    s,
                    "    w |= (data[at + {i}] if at + {i} < len(data) else 0) << {}",
                    8 * i
                );
            }
        }
        Endian::Big => {
            for i in 0..nbytes {
                let _ = writeln!(
                    s,
                    "    w = (w << 8) | (data[at + {i}] if at + {i} < len(data) else 0)"
                );
            }
        }
    }
    s.push_str("    return w\n\n");

    if !m.modal {
        s.push_str("def decode_stream(data):\n");
        s.push_str(
            "    \"\"\"Decode a byte stream: prefix scan, then the post-prefix window.\n\n\
             \x20   Returns (word, total byte length, context dict).\n    \"\"\"\n",
        );
        s.push_str("    (plen, ctx) = scan_prefixes(data)\n");
        s.push_str("    word = _read_window(data, plen)\n");
        s.push_str("    return (word, plen + inst_len(word), ctx)\n\n");
    }
    s
}

// ---------------------------------------------------------------- contextual disasm

/// A Python expression for a fetch width in bits, resolving mode reads via `ctx.mode(..)`.
fn fetch_bits_py(isa: &Isa, arg: &chipi_syntax::ast::Expr) -> String {
    if let chipi_syntax::ast::Expr::Int(i) = arg {
        return format!("{}", i.value);
    }
    let vars: Vec<(String, String, u16)> = isa
        .modes
        .iter()
        .map(|md| {
            (
                md.name.clone(),
                format!("ctx.mode({})", pystr(&md.name)),
                md.value_width(),
            )
        })
        .collect();
    let scope = Scope::Computed {
        fields: &[],
        window: isa.window_bits(),
        base: "0",
        vars: &vars,
    };
    format!("({})", emit_value(arg, &scope))
}

/// An or-chain of `ctx.read_u8` byte reads assembling `nb` stream bytes at `pc + off`.
fn byte_read_py(off: usize, nb: usize, endian: Endian) -> String {
    let mut terms = Vec::new();

    for i in 0..nb {
        let shift = match endian {
            Endian::Little => i * 8,
            Endian::Big => (nb - 1 - i) * 8,
        };
        terms.push(format!("(ctx.read_u8(pc + {}) << {shift})", off + i));
    }

    terms.join(" | ")
}

/// The return expression of a stream accessor: mask `raw` to the value width and sign-extend when
/// signed, matching the Rust backend exactly.
fn stream_value_py(ty: &FieldTy) -> String {
    let mask = chipi_core::compute::mask_u64(ty.value_width);
    if ty.signed {
        format!("_sext64(raw & {mask:#x}, {})", ty.value_width)
    } else {
        format!("raw & {mask:#x}")
    }
}

/// The per-operand stream accessors and `stream_len`.
fn disasm_ctx_support_py(isa: &Isa, m: &Model) -> String {
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let acc_names = computed_accessor_names(isa);

    let mut s = String::from("# ---- contextual disassembly ----\n");
    s.push_str("# The `ctx` object is duck-typed and must provide:\n");
    s.push_str("#   read_u8(addr) -> int          one stream byte at `addr`\n");
    s.push_str("#   symbol(addr) -> None | (name, offset)   symbol lookup (only for :sym/:rel)\n");
    s.push_str("#   mode(name) -> int             host mode value (only for modal specs)\n\n");

    // Stream operand accessors, reading fetched bytes past the opcode window.
    let mut emitted: HashSet<String> = HashSet::new();
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
            let method = comp_acc(&acc_names, &inst.name, &c.name);

            // Folded fetch operands share one accessor. Emit each unique name once, but always
            // advance the per-instruction stream offset.
            if emitted.insert(method.clone()) {
                let val = stream_value_py(&c.ty);

                let _ = writeln!(
                    s,
                    "# stream operand `{}` of `{}`, read at `pc`.",
                    c.name, inst.name
                );
                let _ = writeln!(s, "def {method}(pc, ctx):");

                if let (Some(bits), true) = (const_bits, dyn_terms.is_empty()) {
                    // Static offset and width.
                    let nb = (bits as usize).div_ceil(8);
                    let raw = byte_read_py(off, nb, isa.decoder.endian);
                    let _ = writeln!(s, "    raw = {raw}");
                } else {
                    // Mode-dependent offset or width: compute both at runtime.
                    let off_expr = if dyn_terms.is_empty() {
                        format!("{off}")
                    } else {
                        format!("{off} + {}", dyn_terms.join(" + "))
                    };
                    let bits_expr = fetch_bits_py(isa, arg);
                    let shift = match isa.decoder.endian {
                        Endian::Little => "8 * i",
                        Endian::Big => "8 * (nb - 1 - i)",
                    };
                    let _ = writeln!(s, "    off = {off_expr}");
                    let _ = writeln!(s, "    nb = ({bits_expr} + 7) // 8");
                    s.push_str("    raw = 0\n");
                    s.push_str("    i = 0\n");
                    s.push_str("    while i < nb:\n");
                    let _ = writeln!(s, "        raw |= ctx.read_u8(pc + off + i) << ({shift})");
                    s.push_str("        i += 1\n");
                }

                let _ = writeln!(s, "    return {val}");
                s.push('\n');
            }

            match (const_bits, dyn_terms.is_empty()) {
                (Some(bits), true) => off += (bits as usize).div_ceil(8),
                _ => dyn_terms.push(format!("(({} + 7) // 8)", fetch_bits_py(isa, arg))),
            }
        }
    }

    // stream_len, id-keyed so callers already holding an opcode id skip the re-classify.
    let args = if m.modal { "combo, " } else { "" };
    let _ = writeln!(s, "def stream_len_of({args}op):");
    s.push_str("    \"\"\"Total byte length of opcode `op` (window + fetched operands).\"\"\"\n");
    let combos = if m.modal {
        isa.mode_combos() as usize
    } else {
        1
    };
    let ids = opcode_ids(isa);
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        let extras: Vec<usize> = (0..combos)
            .map(|c| fetched_bytes_combo(isa, inst, c as u64))
            .collect();
        if extras.iter().all(|&e| e == 0) {
            continue;
        }
        let _ = writeln!(s, "    if op == {}:", ids[idx]);
        if extras.iter().all(|&e| e == extras[0]) {
            let _ = writeln!(s, "        return {}", wb + extras[0]);
        } else {
            // Mode-dependent fetch widths: one arm per combination.
            for (c, e) in extras.iter().enumerate() {
                let _ = writeln!(s, "        if combo == {c}:");
                let _ = writeln!(s, "            return {}", wb + e);
            }
            let _ = writeln!(s, "        return {wb}");
        }
    }
    let _ = writeln!(s, "    return {wb}\n");

    let _ = writeln!(s, "def stream_len({args}word):");
    s.push_str(
        "    \"\"\"Total byte length of the instruction at `word` (window + fetched operands).\"\"\"\n",
    );
    let _ = writeln!(s, "    return stream_len_of({args}classify({args}word))\n");
    s
}

/// A synthetic unsigned 64bit type for template placeholders that name a decode variable rather
/// than an operand (mirrors the Rust backend's untyped `ctx_value` fallback).
fn u64_field_ty() -> FieldTy {
    FieldTy {
        base: BaseTy::U(64),
        xforms: Vec::new(),
        disp: Disp::None,
        type_name: None,
        raw_width: 64,
        value_width: 64,
        signed: false,
        subdecoder: None,
    }
}

/// Operand name to (value expression, type) for the contextual renderer: bound fields and
/// computed operands through their word accessors, with fetched operands overridden to their
/// `__op_*` locals by the caller.
fn op_acc_py(
    acc_names: &HashMap<(String, String), String>,
    inst: &Insn,
) -> BTreeMap<String, (String, FieldTy)> {
    let mut m = BTreeMap::new();

    for f in &inst.fields {
        m.insert(
            f.name.clone(),
            (format!("{}(word)", ident(&f.name)), f.ty.clone()),
        );
    }
    for c in &inst.computed {
        m.insert(
            c.name.clone(),
            (
                format!("{}(word)", comp_acc(acc_names, &inst.name, &c.name)),
                c.ty.clone(),
            ),
        );
    }

    m
}

/// The symbol-hit statements shared by the `:sym` and `:rel` renderers.
fn sym_hit_py(indent: &str, s: &mut String) {
    let _ = writeln!(s, "{indent}    __r += __sym[0]");
    let _ = writeln!(s, "{indent}    if __sym[1] != 0:");
    let _ = writeln!(s, "{indent}        __r += \"+0x%x\" % __sym[1]");
}

#[allow(clippy::too_many_arguments)]
fn ctx_render_segs_py(
    segs: &[Seg],
    inst: &Insn,
    acc_names: &HashMap<(String, String), String>,
    acc: &BTreeMap<String, (String, FieldTy)>,
    raw: &BTreeMap<String, String>,
    fetched: &HashSet<String>,
    total: &str,
    indent: &str,
    s: &mut String,
) {
    let deeper = format!("{indent}    ");
    for seg in segs {
        match seg {
            Seg::Lit(t) => {
                let _ = writeln!(s, "{indent}__r += {}", pystr(t));
            }
            Seg::Cond { cond, then, els } => {
                let _ = writeln!(
                    s,
                    "{indent}if ({}) != 0:",
                    emit_cond(cond, inst, acc_names, raw)
                );
                if then.is_empty() {
                    let _ = writeln!(s, "{deeper}pass");
                } else {
                    ctx_render_segs_py(then, inst, acc_names, acc, raw, fetched, total, &deeper, s);
                }
                let _ = writeln!(s, "{indent}else:");
                if els.is_empty() {
                    let _ = writeln!(s, "{deeper}pass");
                } else {
                    ctx_render_segs_py(els, inst, acc_names, acc, raw, fetched, total, &deeper, s);
                }
            }
            Seg::SubField { field, output } => {
                let call = if fetched.contains(field) {
                    Some(format!("__op_{}", sanitize(field)))
                } else {
                    acc.get(field).map(|(v, _)| v.clone())
                };
                match (
                    call,
                    acc.get(field).and_then(|(_, ty)| ty.subdecoder.clone()),
                ) {
                    (Some(call), Some(sdn)) => {
                        let _ =
                            writeln!(s, "{indent}__r += {}({call})", sub_fn_name_py(&sdn, output));
                    }
                    _ => {
                        let _ = writeln!(
                            s,
                            "{indent}__r += {}",
                            pystr(&format!("{{{field}.{output}}}"))
                        );
                    }
                }
            }
            Seg::Field { name, fmt } => {
                let (val, ty) = if fetched.contains(name) {
                    let ty = acc
                        .get(name)
                        .map(|(_, t)| t.clone())
                        .unwrap_or_else(u64_field_ty);
                    (format!("__op_{}", sanitize(name)), ty)
                } else if let Some((v, ty)) = acc.get(name) {
                    (v.clone(), ty.clone())
                } else if let Some(rawv) = raw.get(name) {
                    // Not an operand: a decode variable, resolved through `ctx.mode(..)`.
                    (rawv.clone(), u64_field_ty())
                } else {
                    let _ = writeln!(s, "{indent}__r += {}", pystr(&format!("{{{name}}}")));
                    continue;
                };

                if fmt.rel {
                    let _ = writeln!(s, "{indent}__abs = (pc + ({total}) + ({val})) & 0xffff");
                    let _ = writeln!(s, "{indent}__sym = ctx.symbol(__abs)");
                    let _ = writeln!(s, "{indent}if __sym is not None:");
                    sym_hit_py(indent, s);
                    let _ = writeln!(s, "{indent}else:");
                    let _ = writeln!(s, "{deeper}__r += \"%04x\" % __abs");
                    continue;
                }
                if fmt.sym {
                    let _ = writeln!(s, "{indent}__sym = ctx.symbol(({val}) & _M64)");
                    let _ = writeln!(s, "{indent}if __sym is not None:");
                    sym_hit_py(indent, s);
                    let _ = writeln!(s, "{indent}else:");
                    let _ = writeln!(s, "{deeper}__r += {}", render_value_py(&ty, &val, fmt));
                    continue;
                }
                let _ = writeln!(s, "{indent}__r += {}", render_value_py(&ty, &val, fmt));
            }
        }
    }
}

/// Per-instruction contextual renderers plus the `disasm_ctx(pc, ctx)` entry point.
fn disasm_ctx_fn_py(isa: &Isa, m: &Model) -> String {
    let wb = (isa.window_bits() as usize).div_ceil(8);
    let acc_names = computed_accessor_names(isa);
    let ids = opcode_ids(isa);

    let mut s = String::new();

    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        let _ = writeln!(
            s,
            "def _disasm_ctx_{}(word, pc, ctx):",
            sanitize(&inst.name)
        );

        for c in &inst.computed {
            if is_fetch(&c.expr) {
                let _ = writeln!(
                    s,
                    "    __op_{} = {}(pc, ctx)",
                    sanitize(&c.name),
                    comp_acc(&acc_names, &inst.name, &c.name)
                );
            }
        }

        // The instruction's total byte length: static unless a fetch width is mode-dependent.
        let total = if fetch_has_expr(inst) {
            let terms: Vec<String> = inst
                .computed
                .iter()
                .filter_map(|c| fetch_expr(&c.expr))
                .map(|arg| format!("(({} + 7) // 8)", fetch_bits_py(isa, arg)))
                .collect();
            let _ = writeln!(s, "    __total = {wb} + {}", terms.join(" + "));
            "__total".to_string()
        } else {
            format!("{}", wb + fetched_bytes(inst))
        };

        s.push_str("    __r = \"\"\n");

        let acc = op_acc_py(&acc_names, inst);
        // Mode reads inside display conditions and templates resolve through the host context
        // at runtime; fetched operands resolve to the `__op_*` locals bound above.
        let mut raw: BTreeMap<String, String> = BTreeMap::new();
        for md in &isa.modes {
            raw.insert(md.name.clone(), format!("ctx.mode({})", pystr(&md.name)));
        }
        let fetched: HashSet<String> = inst
            .computed
            .iter()
            .filter(|c| is_fetch(&c.expr))
            .map(|c| c.name.clone())
            .collect();
        for name in &fetched {
            raw.insert(name.clone(), format!("__op_{}", sanitize(name)));
        }

        ctx_render_segs_py(
            inst.ctx_pick_arm(),
            inst,
            &acc_names,
            &acc,
            &raw,
            &fetched,
            &total,
            "    ",
            &mut s,
        );

        let _ = writeln!(s, "    return (__r, {total})\n");
    }

    s.push_str("def disasm_ctx(pc, ctx):\n");
    s.push_str(
        "    \"\"\"Classify at `pc`, fetch stream operands, render with symbols.\n\n\
         \x20   Returns (text, total byte length). See the `ctx` protocol above.\n    \"\"\"\n",
    );
    let read0 = byte_read_py(0, wb, isa.decoder.endian);
    let _ = writeln!(s, "    word = {read0}");
    if m.modal {
        let modes: Vec<String> = isa
            .modes
            .iter()
            .map(|md| format!("ctx.mode({})", pystr(&md.name)))
            .collect();
        let _ = writeln!(s, "    combo = pack_modes({})", modes.join(", "));
        s.push_str("    op = classify(combo, word)\n");
    } else {
        s.push_str("    op = classify(word)\n");
    }
    for &idx in &isa.instr_order() {
        let inst = &isa.instrs[idx];
        let _ = writeln!(s, "    if op == {}:", ids[idx]);
        let _ = writeln!(
            s,
            "        return _disasm_ctx_{}(word, pc, ctx)",
            sanitize(&inst.name)
        );
    }
    let _ = writeln!(s, "    return (\"(invalid)\", {wb})");
    s
}
