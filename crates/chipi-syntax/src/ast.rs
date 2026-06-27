//! Abstract syntax tree for the chipi DSL.
//!
//! The AST keeps source spellings and spans. Numeric masks, width inference and all semantic
//! checks come later in `chipi-core`. Stream ranges here keep their source endpoint order
//! (see [`SrcRange`]); they are normalised during resolution.

use crate::source::{Span, Spanned};

#[derive(Clone, Debug)]
pub struct Spec {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug)]
pub enum Item {
    Decoder(Decoder),
    Selector(Selector),
    /// An `operand` or `type` declaration (distinguished by [`ValueDecl::kind`]).
    Value(ValueDecl),
    Form(Form),
    Func(FuncDecl),
    Length(LengthDecl),
    Prefix(PrefixDecl),
    Group(GroupDecl),
    SubDecoder(SubDecoder),
    Instr(Instr),
}

// ---------------------------------------------------------------- atoms

#[derive(Clone, Debug)]
pub struct Ident {
    pub text: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct IntLit {
    pub value: u128,
    pub width_hint: Option<u16>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StrLit {
    pub text: String,
    pub span: Span,
}

/// A stream range `[a:b]` (or `[bit]`) with endpoints in *source* order, not yet normalised.
#[derive(Clone, Copy, Debug)]
pub struct SrcRange {
    pub a: u32,
    pub b: u32,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BitOrder {
    Lsb0,
    Msb0,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Endian {
    Big,
    Little,
}

// ---------------------------------------------------------------- decoder header

#[derive(Clone, Debug)]
pub struct Decoder {
    pub name: Ident,
    pub width: Option<Spanned<u32>>,
    pub bit_order: Option<Spanned<BitOrder>>,
    pub endian: Option<Spanned<Endian>>,
    pub modes: Vec<ModeDecl>,
    pub context: Vec<CtxField>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ModeKind {
    Bool,
    Enum(Vec<String>),
    Uint(u16),
}

/// A host-owned `mode m: kind = default` selector declared in the decoder header.
#[derive(Clone, Debug)]
pub struct ModeDecl {
    pub name: Ident,
    pub kind: ModeKind,
    pub default: u64,
    pub span: Span,
}

/// A decode-local `context { f:uN = default ... }` scratch field, reset per instruction.
#[derive(Clone, Debug)]
pub struct CtxField {
    pub name: Ident,
    pub width: u16,
    pub default: u64,
    pub span: Span,
}

// ---------------------------------------------------------------- selectors / values / forms

#[derive(Clone, Debug)]
pub struct Selector {
    pub name: Ident,
    pub range: SrcRange,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueKind {
    Operand,
    Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Xform {
    SignExtend(u16),
    ZeroExtend(u16),
    ShiftLeft(u16),
    ShiftRight(u16),
    RotateLeft(u16, u16),
    RotateRight(u16, u16),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DispHint {
    Hex,
    SignedHex,
    Dec,
}

/// The fallback of a `names { ... }` table when no entry matches.
#[derive(Clone, Debug)]
pub enum NameDefault {
    /// `_ => "???"`: a literal string.
    Str(String),
    /// `_ => dec` / `_ => hex` / `_ => signed_hex`: render the numeric value.
    Hint(DispHint),
}

/// A `names { 0 => "GE", 1 => "L", ..., _ => dec }` value-to-string table.
#[derive(Clone, Debug)]
pub struct NamesTable {
    pub entries: Vec<(u128, String)>,
    pub default: NameDefault,
    pub span: Span,
}

/// The `display(...)` attribute of a value declaration.
#[derive(Clone, Debug)]
pub enum DispAttr {
    /// `display("$r{}")`: a pattern whose `{}` is filled with the value.
    Pattern(StrLit),
    /// `display(signed_hex)` etc.
    Hint(DispHint),
    /// `display(names { 0 => "GE", ..., _ => dec })`: a value-to-string lookup table.
    Names(NamesTable),
}

/// An `operand`/`type` declaration: a base integer type plus a transform pipeline and display.
///
/// `source` is an optional value recipe (`= u16 fetch(16)` / `= i13 assemble N { ... }`): when a
/// later instruction binds this operand with no bit range, the operand becomes a computed operand
/// driven by this recipe rather than a plain field slice. It lets a fetched/assembled operand be
/// declared once and reused, instead of repeating the recipe on every instruction.
#[derive(Clone, Debug)]
pub struct ValueDecl {
    pub kind: ValueKind,
    pub name: Ident,
    pub base: Ident,
    pub xforms: Vec<Xform>,
    pub disp: Option<DispAttr>,
    pub source: Option<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FormField {
    pub name: Ident,
    pub ty: Ident,
    pub range: SrcRange,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Form {
    pub name: Ident,
    pub fields: Vec<FormField>,
    pub span: Span,
}

// ---------------------------------------------------------------- fn / length / prefix / groups

/// `fn name(p:uN, ...) -> ty { let x = expr ... return expr }`: a bounded pure function.
#[derive(Clone, Debug)]
pub struct FuncDecl {
    pub name: Ident,
    pub params: Vec<(Ident, Ident)>,
    pub ret: Ident,
    pub lets: Vec<(Ident, Expr)>,
    pub ret_expr: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LengthArm {
    /// `None` for the final `else` catch-all arm.
    pub cond: Option<Expr>,
    pub bits: u32,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LengthDecl {
    pub arms: Vec<LengthArm>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum PrefixPat {
    Byte(u8),
    Range(u8, u8),
    Wildcard,
}

/// How a matched prefix arm ends the scan.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrefixTerm {
    /// consume this unit and keep scanning (bare `field = expr` arm)
    Continue,
    /// consume this unit and stop (`finish`)
    Finish,
    /// stop without consuming, so this unit is the opcode (`done`)
    Done,
}

#[derive(Clone, Debug)]
pub struct PrefixArm {
    pub pat: PrefixPat,
    pub assigns: Vec<(Ident, Expr)>,
    pub term: PrefixTerm,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct PrefixDecl {
    pub name: Ident,
    pub arms: Vec<PrefixArm>,
    pub span: Span,
}

/// `name { members }` tags its members; `dispatch name { members }` also folds them into a group.
#[derive(Clone, Debug)]
pub struct GroupDecl {
    pub tag: Ident,
    pub members: Vec<Ident>,
    pub dispatch: bool,
    pub span: Span,
}

// ---------------------------------------------------------------- subdecoders

/// One named string output of a subdecoder arm: `mnemonic = "..."`.
#[derive(Clone, Debug)]
pub struct SubOutput {
    pub name: Ident,
    pub template: StrLit,
    pub span: Span,
}

/// One arm of a subdecoder: a pattern over the sub-field (constraints + bindings) plus a string
/// template for each declared output.
#[derive(Clone, Debug)]
pub struct SubArm {
    pub name: Ident,
    pub constraints: Vec<Constraint>,
    pub bindings: Vec<Binding>,
    pub outputs: Vec<SubOutput>,
    pub span: Span,
}

/// `subdecoder Name { width = N bit_order = B outputs { a, b } <arms> }`. A subdecoder decodes a
/// bound sub-field into one or more named strings, referenced from a display template as
/// `{field.output}`. It affects display only: the bound field decodes and encodes as plain bits.
#[derive(Clone, Debug)]
pub struct SubDecoder {
    pub name: Ident,
    pub width: Option<Spanned<u32>>,
    pub bit_order: Option<Spanned<BitOrder>>,
    pub outputs: Vec<Ident>,
    pub arms: Vec<SubArm>,
    pub span: Span,
}

// ---------------------------------------------------------------- instructions

#[derive(Clone, Debug)]
pub enum Constraint {
    /// `op = 0`, where `name` resolves to a selector or a mode.
    Named { name: Ident, value: IntLit },
    /// `[31:26] = 0b...`: an explicit stream-range constraint.
    Range { range: SrcRange, value: IntLit },
}

#[derive(Clone, Debug)]
pub struct TyRef {
    pub name: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub name: Ident,
    pub ty: TyRef,
    pub range: Option<SrcRange>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Computed {
    pub name: Ident,
    pub ty: Ident,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct DisplayArm {
    pub cond: Option<Expr>,
    pub template: StrLit,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Instr {
    pub name: Ident,
    pub constraints: Vec<Constraint>,
    pub bindings: Vec<Binding>,
    pub uses: Option<Ident>,
    pub computed: Vec<Computed>,
    pub guard: Option<Expr>,
    pub display: Vec<DisplayArm>,
    pub span: Span,
}

// ---------------------------------------------------------------- expressions

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    LAnd,
    LOr,
}

/// Width extension applied to an `assemble` result.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ext {
    None,
    SignExtend,
    ZeroExtend,
}

/// One `[hi:lo] = src` entry of an `assemble` block.
#[derive(Clone, Debug)]
pub struct AssemblePart {
    pub hi: u32,
    pub lo: u32,
    pub src: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(IntLit),
    Name(Ident),
    Slice {
        base: Box<Expr>,
        hi: u32,
        lo: u32,
        span: Span,
    },
    /// `assemble N { [hi:lo] = src ... } [sign_extend|zero_extend]`: declarative bit scatter.
    Assemble {
        out_width: u32,
        parts: Vec<AssemblePart>,
        ext: Ext,
        span: Span,
    },
    Unary {
        op: UnOp,
        rhs: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span,
    },
    Cond {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
        span: Span,
    },
    Call {
        callee: Ident,
        args: Vec<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(i) => i.span,
            Expr::Name(n) => n.span,
            Expr::Slice { span, .. }
            | Expr::Assemble { span, .. }
            | Expr::Unary { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Cond { span, .. }
            | Expr::Call { span, .. } => *span,
        }
    }
}
