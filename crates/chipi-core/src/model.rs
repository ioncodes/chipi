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
    pub name: String,
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
    pub lets: Vec<(String, Expr)>,
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
    pub fn bits_for(&self, word: u64) -> u16 {
        for arm in &self.arms {
            match &arm.cond {
                None => return arm.bits,
                Some(c) => {
                    if crate::interp::eval_cond(c, &[], word) != 0 {
                        return arm.bits;
                    }
                }
            }
        }
        self.arms.last().map(|a| a.bits).unwrap_or(0)
    }

    /// The widest window any arm can select (sizes the handle).
    pub fn max_bits(&self) -> u16 {
        self.arms.iter().map(|a| a.bits).max().unwrap_or(0)
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
    /// One decode tree per mode combination (length 1 with no modes).
    pub mode_trees: Vec<Tree>,
    /// Maximum consumed length over all leaves, in bytes.
    pub max_len_bytes: u8,
    /// Non-fatal diagnostics (coverage gaps, incomplete tables).
    pub warnings: Vec<Diag>,
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
