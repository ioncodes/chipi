//! The resolved intermediate representation consumed by validation, decode-tree building, the
//! reference evaluator, the encoder, the text assembler and every backend.
//!
//! All stream ranges are numeric `[lo, hi]` (inclusive) over the leaf window. Bit `i` of a
//! field value lives at numeric position `lo + i`, independent of the source `bit_order`. Backends
//! and the evaluator never see source-order ranges.

use chipi_syntax::ast::Expr;
use chipi_syntax::{Diag, Span};

pub use chipi_syntax::ast::{
    BitOrder, DispHint, Endian, Ext, ModeKind, PrefixPat, PrefixTerm, Xform,
};

use crate::render::Seg;
use crate::tree::Tree;

/// A contiguous numeric bit range `[lo, hi]` (inclusive) over the decode window.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BitRange {
    pub lo: u16,
    pub hi: u16,
}

impl BitRange {
    pub fn width(self) -> u16 {
        self.hi - self.lo + 1
    }

    /// The range's mask over a 64bit window.
    pub fn mask(self) -> u64 {
        let w = self.width();
        let ones = if w >= 64 { u64::MAX } else { (1u64 << w) - 1 };
        ones << self.lo
    }
}

/// A primitive base type behind a named operand/type or a directly-bound field.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BaseTy {
    U(u16),
    I(u16),
    Bool,
}

impl BaseTy {
    pub fn width(self) -> u16 {
        match self {
            BaseTy::U(w) | BaseTy::I(w) => w,
            BaseTy::Bool => 1,
        }
    }

    pub fn signed(self) -> bool {
        matches!(self, BaseTy::I(_))
    }
}

/// The fallback of a [`Disp::Names`] table when no entry matches.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NameDefault {
    /// A literal string (e.g. `"???"`).
    Str(String),
    /// Render the numeric value with this hint.
    Hint(DispHint),
}

/// A resolved value-to-string lookup table (`names { 0 => "GE", ..., _ => dec }`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamesTable {
    pub entries: Vec<(u64, String)>,
    pub default: NameDefault,
}

impl NamesTable {
    /// The string for `value`, if some entry matches.
    pub fn lookup(&self, value: u64) -> Option<&str> {
        self.entries
            .iter()
            .find(|(k, _)| *k == value)
            .map(|(_, s)| s.as_str())
    }
}

/// How a value is rendered by default (from its `operand`/`type` declaration).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Disp {
    /// `"$r{}"`: the `{}` is replaced with the decimal value.
    Pattern(String),
    Hint(DispHint),
    /// `names { ... }`: a value-to-string table with a fallback.
    Names(NamesTable),
    None,
}

/// A declared `operand`/`type` (kept for `dump-ir` and host mapping; binding sites carry [`FieldTy`]).
#[derive(Clone, Debug)]
pub struct TypeDef {
    pub name: String,
    pub base: BaseTy,
    pub xforms: Vec<Xform>,
    pub disp: Disp,
    /// `true` for `operand`, `false` for `type`.
    pub is_operand: bool,
    /// An optional value recipe (`fetch(N)` / `assemble N { ... }`): a no-range binding of this
    /// operand becomes a computed operand evaluating this expression. `None` for a plain type.
    pub source: Option<Expr>,
    pub span: Span,
}

/// A fully resolved value type at a binding site (or a computed operand).
#[derive(Clone, Debug)]
pub struct FieldTy {
    pub base: BaseTy,
    pub xforms: Vec<Xform>,
    pub disp: Disp,
    /// Originating `operand`/`type` name, if any.
    pub type_name: Option<String>,
    /// Width of the raw extracted field, in bits.
    pub raw_width: u16,
    /// Width of the post-transform value, in bits.
    pub value_width: u16,
    /// Whether the value is signed (drives accessor return type and display).
    pub signed: bool,
    /// The subdecoder that decodes this field into named strings, if the field's type is a
    /// `subdecoder`. The field still decodes/encodes as plain bits; this drives `{field.output}`.
    pub subdecoder: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Selector {
    pub name: String,
    pub range: BitRange,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub range: BitRange,
    pub ty: FieldTy,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Form {
    pub name: String,
    pub fields: Vec<Field>,
    pub span: Span,
}

/// One fixed-bit constraint, retained with its span for diagnostics.
#[derive(Clone, Debug)]
pub struct Fixed {
    pub range: BitRange,
    pub value: u64,
    pub span: Span,
    /// Human label (selector name or `"bits"`).
    pub label: String,
}

/// A computed operand `name: ty = expr` (or `= assemble ...` / `= fetch(N)`).
#[derive(Clone, Debug)]
pub struct Computed {
    pub name: String,
    pub ty: FieldTy,
    pub expr: Expr,
    pub span: Span,
}

/// One display arm: an optional guard condition and the parsed template segments.
#[derive(Clone, Debug)]
pub struct DisplayArm {
    pub cond: Option<Expr>,
    pub segs: Vec<Seg>,
    pub span: Span,
}

/// A resolved instruction leaf.
#[derive(Clone, Debug)]
pub struct Insn {
    /// The full (possibly dotted) leaf name, unique per spec.
    pub name: String,
    /// The part before the dot (the whole name for undotted leaves).
    pub mnemonic: String,
    /// The form axis: the part after the dot, if the leaf name has one.
    pub form: Option<String>,
    pub fixed: Vec<Fixed>,
    pub fields: Vec<Field>,
    pub computed: Vec<Computed>,
    /// Mode constraints `(mode_index, value)`: the leaf exists only when each mode equals its value.
    pub mode_constraints: Vec<(usize, u64)>,
    /// `when expr` guard: the leaf only matches when this evaluates non-zero.
    pub guard: Option<Expr>,
    pub display: Vec<DisplayArm>,
    /// Tags carried by this instruction, in declaration order.
    pub tags: Vec<String>,
    pub span: Span,
}

impl Insn {
    /// The display arm the contextual disassembler renders (first unconditional, else first,
    /// else empty). Shared by the oracle's `disasm_ctx` and every backend's contextual renderer.
    pub fn ctx_pick_arm(&self) -> &[Seg] {
        self.display
            .iter()
            .find(|a| a.cond.is_none())
            .or_else(|| self.display.first())
            .map(|a| a.segs.as_slice())
            .unwrap_or(&[])
    }

    /// Combined fixed mask/value over the window.
    pub fn fixed_mask_val(&self) -> (u64, u64) {
        let mut mask = 0u64;
        let mut val = 0u64;
        for c in &self.fixed {
            let m = c.range.mask();
            mask |= m;
            val |= (c.value << c.range.lo) & m;
        }
        (mask, val)
    }
}

/// A bounded pure function (`fn`).
#[derive(Clone, Debug)]
pub struct Func {
    pub name: String,
    pub params: Vec<(String, BaseTy)>,
    pub ret: BaseTy,
    /// `(name, initializer, width)`. The width is inferred once during lowering, so the
    /// evaluator and every backend agree without re-running width inference per use.
    pub lets: Vec<(String, Expr, u16)>,
    pub ret_expr: Expr,
    pub span: Span,
}

/// A resolved host `mode` selector.
#[derive(Clone, Debug)]
pub struct Mode {
    pub name: String,
    pub kind: ModeKind,
    /// Distinct value count (Bool = 2, Enum(n) = n, Uint(w) = 2^w, capped).
    pub cardinality: u64,
    pub default: u64,
}

impl Mode {
    /// Bits needed to hold any value of this mode (its width as a decode variable).
    pub fn value_width(&self) -> u16 {
        (64 - (self.cardinality.saturating_sub(1)).leading_zeros()).max(1) as u16
    }
}

/// A dispatch group (`dispatch name { members }`).
#[derive(Clone, Debug)]
pub struct Group {
    pub name: String,
    /// Member instruction names, in declaration order (the enum-variant order).
    pub members: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct CtxField {
    pub name: String,
    pub width: u16,
    pub default: u64,
}

#[derive(Clone, Debug)]
pub struct Decoder {
    pub name: String,
    pub unit_bits: u8,
    pub bit_order: BitOrder,
    pub endian: Endian,
    /// Decode-local scratch fields (`context { ... }`); reset per instruction.
    pub context: Vec<CtxField>,
}

/// A resolved prefix scan.
#[derive(Clone, Debug)]
pub struct Prefix {
    pub arms: Vec<PrefixArm>,
}

#[derive(Clone, Debug)]
pub struct PrefixArm {
    pub pat: PrefixPat,
    pub assigns: Vec<(String, Expr)>,
    pub term: PrefixTerm,
}

impl Prefix {
    /// The first arm matching `byte` (`_` is the catch-all).
    pub fn arm_for(&self, byte: u8) -> Option<&PrefixArm> {
        self.arms.iter().find(|a| match a.pat {
            PrefixPat::Byte(b) => b == byte,
            PrefixPat::Range(lo, hi) => lo <= byte && byte <= hi,
            PrefixPat::Wildcard => true,
        })
    }
}

/// An embedded `length` expression: the first arm whose condition holds selects the window width.
#[derive(Clone, Debug)]
pub struct Length {
    pub arms: Vec<LengthArm>,
}

#[derive(Clone, Debug)]
pub struct LengthArm {
    pub cond: Option<Expr>,
    pub bits: u16,
}

impl Length {
    /// Window width in bits for `word`: first matching arm (a `None` condition always matches).
    /// Decode variables sit at whatever `vars` supplies (empty means every read is 0).
    pub fn bits_for_vars(&self, word: u64, vars: &[(String, u64)]) -> u16 {
        for arm in &self.arms {
            match &arm.cond {
                None => return arm.bits,
                Some(c) => {
                    if crate::interp::eval_cond(c, &[], word, vars) != 0 {
                        return arm.bits;
                    }
                }
            }
        }
        self.arms.last().map(|a| a.bits).unwrap_or(0)
    }

    /// [`Length::bits_for_vars`] with no decode variables in scope.
    pub fn bits_for(&self, word: u64) -> u16 {
        self.bits_for_vars(word, &[])
    }

    /// The widest window any arm can select (sizes the handle).
    pub fn max_bits(&self) -> u16 {
        self.arms.iter().map(|a| a.bits).max().unwrap_or(0)
    }

    /// The narrowest window any arm can select (the shortest encoding classification must
    /// still route correctly).
    pub fn min_bits(&self) -> u16 {
        self.arms.iter().map(|a| a.bits).min().unwrap_or(0)
    }
}

/// One arm of a [`SubDecoder`]: a fixed mask/value pattern over the sub-field, its own bound fields,
/// and a string template (parsed [`Seg`]s) per declared output.
#[derive(Clone, Debug)]
pub struct SubArm {
    pub name: String,
    pub mask: u64,
    pub val: u64,
    pub fields: Vec<Field>,
    /// `(output_name, template segments)` in declaration order.
    pub outputs: Vec<(String, Vec<Seg>)>,
    pub span: Span,
}

impl SubArm {
    /// The template segments for output `name` (every declared output is set on every arm).
    pub fn output(&self, name: &str) -> Option<&[Seg]> {
        self.outputs
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, segs)| segs.as_slice())
    }
}

/// A subdecoder: decodes a bound sub-field into named strings (display only). The arms are matched
/// most-specific-first (most fixed bits wins); the first match supplies every output.
#[derive(Clone, Debug)]
pub struct SubDecoder {
    pub name: String,
    pub unit_bits: u8,
    pub bit_order: BitOrder,
    pub outputs: Vec<String>,
    pub arms: Vec<SubArm>,
}

impl SubDecoder {
    /// The first arm matching `value` (arms are pre-sorted most-specific-first).
    pub fn arm_for(&self, value: u64) -> Option<&SubArm> {
        self.arms.iter().find(|a| (value & a.mask) == a.val)
    }
}

/// The complete resolved program plus its lowered decode tree(s).
#[derive(Clone, Debug)]
pub struct Isa {
    pub decoder: Decoder,
    pub selectors: Vec<Selector>,
    pub types: Vec<TypeDef>,
    pub forms: Vec<Form>,
    pub fns: Vec<Func>,
    pub modes: Vec<Mode>,
    pub instrs: Vec<Insn>,
    /// All declared tags, sorted-unique.
    pub tags: Vec<String>,
    /// Dispatch groups (those declared with `dispatch`).
    pub groups: Vec<Group>,
    /// Subdecoders (`subdecoder Name { ... }`), referenced by `field:Name` bindings.
    pub subdecoders: Vec<SubDecoder>,
    pub length: Option<Length>,
    pub prefix: Option<Prefix>,
    /// The default-mode decode tree.
    pub tree: Tree,
    /// The distinct decode trees: mode combinations whose constraints filter to the same leaf
    /// set share one tree (length 1 with no modes). Resolve a combination via [`Isa::tree_for`].
    pub mode_trees: Vec<Tree>,
    /// Mode combination index -> index into `mode_trees` (one entry per combination).
    pub combo_tree: Vec<usize>,
    /// Maximum consumed length over all leaves, in bytes.
    pub max_len_bytes: u8,
    /// Non-fatal diagnostics (coverage gaps, incomplete tables).
    pub warnings: Vec<Diag>,
    /// [`Isa::default_var_values`], precomputed at `compile()` so the word-level decode entry
    /// points can borrow it instead of rebuilding the table per call.
    pub vars_default: Vec<(String, u64)>,
    /// [`Isa::combo_var_values`] per mode combination, precomputed at `compile()`.
    pub vars_combo: Vec<Vec<(String, u64)>>,
}

impl Isa {
    /// Leaf window width in bits (one fetch unit).
    pub fn window_bits(&self) -> u16 {
        self.decoder.unit_bits as u16
    }

    /// Instruction indices ordered by name. This is the emission order shared by every backend.
    pub fn instr_order(&self) -> Vec<usize> {
        let mut v: Vec<usize> = (0..self.instrs.len()).collect();
        v.sort_by(|&a, &b| self.instrs[a].name.cmp(&self.instrs[b].name));
        v
    }

    /// Each unique field name in BTree (ascending) order; the first definition in program order wins.
    pub fn unique_fields(&self) -> std::collections::BTreeMap<String, &Field> {
        let mut map = std::collections::BTreeMap::new();
        for inst in &self.instrs {
            for f in &inst.fields {
                map.entry(f.name.clone()).or_insert(f);
            }
        }
        map
    }

    /// Smallest power-of-two byte width (in bits) that holds the handle.
    pub fn handle_bits(&self) -> u16 {
        match self.max_len_bytes as u16 * 8 {
            0..=8 => 8,
            9..=16 => 16,
            17..=32 => 32,
            _ => 64,
        }
    }

    pub fn handle_ty(&self) -> &'static str {
        match self.handle_bits() {
            8 => "u8",
            16 => "u16",
            32 => "u32",
            _ => "u64",
        }
    }

    /// Number of mode combinations (cross-product cardinality), at least 1.
    pub fn mode_combos(&self) -> u64 {
        self.modes
            .iter()
            .map(|m| m.cardinality)
            .product::<u64>()
            .max(1)
    }

    /// Pack mode values (declaration order) into a combination index (mixed radix).
    pub fn pack_modes(&self, vals: &[u64]) -> u64 {
        let mut idx = 0u64;
        let mut radix = 1u64;
        for (m, v) in self.modes.iter().zip(vals) {
            idx += (v % m.cardinality) * radix;
            radix *= m.cardinality;
        }
        idx
    }

    /// The combination index for all modes' default values.
    pub fn default_combo(&self) -> u64 {
        default_combo(&self.modes)
    }

    /// All decode variables `(name, width)`: host modes then prefix-assigned context fields.
    /// Guards and `length` arms may read these by name.
    pub fn var_widths(&self) -> Vec<(String, u16)> {
        var_widths(&self.modes, &self.decoder.context)
    }

    /// Mode values for combination `combo` as substitution triples `(name, value, width)`.
    pub fn mode_subst(&self, combo: u64) -> Vec<(String, u64, u16)> {
        let mut radix = 1u64;
        self.modes
            .iter()
            .map(|m| {
                let v = (combo / radix) % m.cardinality;
                radix *= m.cardinality;
                (m.name.clone(), v, m.value_width())
            })
            .collect()
    }

    /// Word-level substitution: every decode variable folded to its default. This is what
    /// generated `classify(word)` / `disasm(word)` entry points use, matching `decode(word)`.
    pub fn default_subst(&self) -> Vec<(String, u64, u16)> {
        self.combo_subst(self.default_combo())
    }

    /// Substitution for mode combination `combo`: modes at the combo's values, context fields at
    /// defaults. This is what a per-combination classify body folds through.
    pub fn combo_subst(&self, combo: u64) -> Vec<(String, u64, u16)> {
        let mut v = self.mode_subst(combo);
        v.extend(self.context_default_subst());
        v
    }

    /// Whether any leaf name carries a form axis (`lda.dpx`). Identity-axis codegen
    /// (Mnemonic/Form enums and accessors) activates only then.
    pub fn has_axes(&self) -> bool {
        self.instrs.iter().any(|i| i.form.is_some())
    }

    /// The mnemonic axis: every distinct mnemonic, sorted, with slot 0 reserved (like opcode
    /// ids, index 0 is Invalid in the generated enum).
    pub fn mnemonics(&self) -> Vec<String> {
        let set: std::collections::BTreeSet<String> =
            self.instrs.iter().map(|i| i.mnemonic.clone()).collect();
        set.into_iter().collect()
    }

    /// The form axis: every distinct form, sorted. Undotted leaves map to the generated
    /// `Form::None` variant, which is not part of this list.
    pub fn form_axes(&self) -> Vec<String> {
        let set: std::collections::BTreeSet<String> =
            self.instrs.iter().filter_map(|i| i.form.clone()).collect();
        set.into_iter().collect()
    }

    /// The names of all decode variables (modes and context fields).
    pub fn var_names(&self) -> Vec<String> {
        self.var_widths().into_iter().map(|(n, _)| n).collect()
    }

    /// Context-field defaults as substitution triples `(name, value, width)`.
    pub fn context_default_subst(&self) -> Vec<(String, u64, u16)> {
        self.decoder
            .context
            .iter()
            .map(|c| (c.name.clone(), c.default, c.width))
            .collect()
    }

    /// Every decode variable at its default value: what word-level entry points (`decode(word)`,
    /// generated `classify(word)`) use when no host or stream supplies real values. Precomputed
    /// once per compile as [`Isa::vars_default`]; hot paths should borrow that instead.
    pub fn default_var_values(&self) -> Vec<(String, u64)> {
        self.combo_var_values(self.default_combo())
    }

    /// Decode-variable values for mode combination `combo` (context fields at defaults).
    /// Precomputed per combination as [`Isa::vars_combo`]; hot paths should borrow that instead.
    pub fn combo_var_values(&self, combo: u64) -> Vec<(String, u64)> {
        self.combo_subst(combo)
            .into_iter()
            .map(|(n, val, _)| (n, val))
            .collect()
    }

    /// The decode tree for mode combination `combo`. Out-of-range combinations fall back to the
    /// default-mode tree, matching what `decode_mode` always did.
    pub fn tree_for(&self, combo: usize) -> &Tree {
        self.combo_tree
            .get(combo)
            .and_then(|&t| self.mode_trees.get(t))
            .unwrap_or(&self.tree)
    }

    /// Whether any guard-checked sparse-chain arm in `tree` reads a decode variable. When false,
    /// the tree's routing body folds identically for every combination sharing the tree, so a
    /// backend can emit one routing function per distinct tree instead of one per combination.
    pub fn tree_chain_reads_vars(&self, tree: &Tree) -> bool {
        let names = self.var_names();
        tree.residuals.iter().any(|r| match r {
            crate::tree::Residual::Sparse { arms, .. } => arms.iter().any(|a| {
                a.check_guard
                    && self.instrs[tree.opcodes[a.opcode].instr]
                        .guard
                        .as_ref()
                        .is_some_and(|g| crate::compute::expr_reads_any(g, &names))
            }),
            crate::tree::Residual::Keyed { .. } => false,
        })
    }

    /// Whether any display arm (its condition or a rendered/conditional template field) reads a
    /// decode variable. Such reads only resolve through the contextual disassembler's
    /// `ctx.mode(..)`; the static display path has no variable source, so backends refuse specs
    /// where this holds and the static path would be emitted.
    pub fn display_reads_vars(&self) -> bool {
        let names = self.var_names();
        self.instrs.iter().any(|i| {
            i.display.iter().any(|a| {
                a.cond
                    .as_ref()
                    .is_some_and(|c| crate::compute::expr_reads_any(c, &names))
                    || segs_read_vars(&a.segs, &names)
            })
        })
    }

    /// Whether any `length` arm condition reads a decode variable. Backends emit `length` as a
    /// pure function of the word, so such specs are refused.
    pub fn length_reads_vars(&self) -> bool {
        let names = self.var_names();
        self.length.as_ref().is_some_and(|l| {
            l.arms.iter().any(|a| {
                a.cond
                    .as_ref()
                    .is_some_and(|c| crate::compute::expr_reads_any(c, &names))
            })
        })
    }

    /// The guard a sparse chain arm must check as part of matching, if any. Backends emit this
    /// into the arm condition so a failed guard falls through to the next arm.
    pub fn sparse_arm_guard<'a>(
        &'a self,
        tree: &Tree,
        arm: &crate::tree::SparseArm,
    ) -> Option<&'a Expr> {
        if !arm.check_guard {
            return None;
        }
        self.instrs[tree.opcodes[arm.opcode].instr].guard.as_ref()
    }
}

/// The decode variables readable by guards and `length` arms: `(name, width)` for every host
/// mode and prefix-assigned context field. The free-function form serves `lower`, which needs
/// it before an [`Isa`] exists; [`Isa::var_widths`] delegates here.
pub fn var_widths(modes: &[Mode], context: &[CtxField]) -> Vec<(String, u16)> {
    modes
        .iter()
        .map(|m| (m.name.clone(), m.value_width()))
        .chain(context.iter().map(|c| (c.name.clone(), c.width)))
        .collect()
}

/// Whether any segment of a display template reads one of `names` (through an in-template
/// condition or by rendering the variable directly).
fn segs_read_vars(segs: &[Seg], names: &[String]) -> bool {
    segs.iter().any(|seg| match seg {
        Seg::Cond { cond, then, els } => {
            crate::compute::expr_reads_any(cond, names)
                || segs_read_vars(then, names)
                || segs_read_vars(els, names)
        }
        Seg::Field { name, .. } => names.contains(name),
        Seg::SubField { .. } | Seg::Lit(_) => false,
    })
}

/// The combination index for all modes' default values (mixed radix, declaration order).
pub fn default_combo(modes: &[Mode]) -> u64 {
    let mut idx = 0u64;
    let mut radix = 1u64;
    for m in modes {
        idx += (m.default % m.cardinality) * radix;
        radix *= m.cardinality;
    }
    idx
}
