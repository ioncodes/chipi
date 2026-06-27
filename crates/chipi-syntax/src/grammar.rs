//! Recursive-descent parser for the chipi DSL.
//!
//! The parser is fail-fast: it returns the first syntax error it meets. Semantic checks that want
//! to report many problems at once live in `chipi-core`.

use crate::ast::*;
use crate::lex::{lex, Token, TokenKind};
use crate::report::Diag;
use crate::source::Span;

type PResult<T> = Result<T, Diag>;

/// Parse a whole spec source.
pub fn parse(src: &str) -> PResult<Spec> {
    let mut p = Parser::new(lex(src)?);
    p.spec()
}

/// Parse a single standalone expression (used for display-template conditions).
pub fn parse_expr_str(src: &str) -> PResult<Expr> {
    let mut p = Parser::new(lex(src)?);
    let e = p.expr()?;
    if !p.eof() {
        return Err(p.fail(format!("unexpected {} after expression", show(p.peek()))));
    }
    Ok(e)
}

struct Parser {
    toks: Vec<Token>,
    pos: usize,
    /// While set, a top-level `|` ends an instruction-body expression (display delimiter) rather
    /// than acting as bitwise-or. Parentheses clear it.
    pipe_is_delim: bool,
    /// While set (inside an `assemble { ... }` body), `[..] =` is a destination, so a `[..]` that is
    /// followed by `=` is not treated as a postfix slice of the preceding term.
    inside_assemble: bool,
}

impl Parser {
    fn new(toks: Vec<Token>) -> Self {
        Parser {
            toks,
            pos: 0,
            pipe_is_delim: false,
            inside_assemble: false,
        }
    }

    // -------- token cursor --------

    fn peek(&self) -> &TokenKind {
        &self.toks[self.pos].kind
    }

    fn peek2(&self) -> Option<&TokenKind> {
        self.toks.get(self.pos + 1).map(|t| &t.kind)
    }

    fn here(&self) -> Span {
        self.toks[self.pos].span
    }

    fn last(&self) -> Span {
        self.toks[self.pos.saturating_sub(1)].span
    }

    fn eof(&self) -> bool {
        matches!(self.peek(), TokenKind::Eof)
    }

    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        t
    }

    fn is(&self, k: &TokenKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(k)
    }

    fn kw(&self, word: &str) -> bool {
        matches!(self.peek(), TokenKind::Ident(s) if s == word)
    }

    fn fail(&self, msg: impl Into<String>) -> Diag {
        Diag::error("ParseError", msg, self.here())
    }

    fn err<T>(&self, msg: impl Into<String>) -> PResult<T> {
        Err(self.fail(msg))
    }

    fn want(&mut self, k: TokenKind, what: &str) -> PResult<Span> {
        if self.is(&k) {
            Ok(self.bump().span)
        } else {
            self.err(format!("expected {what}, found {}", show(self.peek())))
        }
    }

    /// Consume a comma separator if the next token is one. Returns `true` if a comma was eaten,
    /// which list loops use to decide whether to continue.
    fn eat_comma(&mut self) -> bool {
        if matches!(self.peek(), TokenKind::Comma) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn ident(&mut self) -> PResult<Ident> {
        match self.peek().clone() {
            TokenKind::Ident(text) => {
                let span = self.bump().span;
                Ok(Ident { text, span })
            }
            other => self.err(format!("expected identifier, found {}", show(&other))),
        }
    }

    fn int(&mut self) -> PResult<IntLit> {
        match self.peek().clone() {
            TokenKind::Int { value, width_hint } => {
                let span = self.bump().span;
                Ok(IntLit {
                    value,
                    width_hint,
                    span,
                })
            }
            other => self.err(format!("expected an integer, found {}", show(&other))),
        }
    }

    fn string(&mut self) -> PResult<StrLit> {
        match self.peek().clone() {
            TokenKind::Str(text) => {
                let span = self.bump().span;
                Ok(StrLit { text, span })
            }
            other => self.err(format!("expected a string, found {}", show(&other))),
        }
    }

    fn u32_lit(&mut self) -> PResult<(u32, Span)> {
        let lit = self.int()?;
        if lit.value > u32::MAX as u128 {
            return Err(Diag::error(
                "ParseError",
                "integer is too large here",
                lit.span,
            ));
        }
        Ok((lit.value as u32, lit.span))
    }

    fn u16_arg(&mut self) -> PResult<u16> {
        let lit = self.int()?;
        if lit.value > u16::MAX as u128 {
            return Err(Diag::error("ParseError", "argument is too large", lit.span));
        }
        Ok(lit.value as u16)
    }

    // -------- top level --------

    fn spec(&mut self) -> PResult<Spec> {
        let mut items = Vec::new();
        while !self.eof() {
            items.push(self.item()?);
        }
        Ok(Spec { items })
    }

    fn item(&mut self) -> PResult<Item> {
        match self.peek() {
            TokenKind::Ident(s) => match s.as_str() {
                "decoder" => Ok(Item::Decoder(self.decoder()?)),
                "selector" => Ok(Item::Selector(self.selector()?)),
                "operand" => Ok(Item::Value(self.value_decl(ValueKind::Operand)?)),
                "type" => Ok(Item::Value(self.value_decl(ValueKind::Type)?)),
                "form" => Ok(Item::Form(self.form()?)),
                "fn" => Ok(Item::Func(self.func()?)),
                "length" => Ok(Item::Length(self.length()?)),
                "prefix" => Ok(Item::Prefix(self.prefix()?)),
                "dispatch" => Ok(Item::Group(self.group(true)?)),
                "subdecoder" => Ok(Item::SubDecoder(self.subdecoder()?)),
                // `name { ... }` (the next token is `{`) is a tag or dispatch group. An instruction
                // never opens a brace right after its mnemonic.
                _ if matches!(self.peek2(), Some(TokenKind::LBrace)) => {
                    Ok(Item::Group(self.group(false)?))
                }
                _ => Ok(Item::Instr(self.instr()?)),
            },
            other => self.err(format!(
                "expected a top-level item (decoder, selector, operand, type, form, fn, or an \
                 instruction), found {}",
                show(other)
            )),
        }
    }

    // -------- decoder header --------

    fn decoder(&mut self) -> PResult<Decoder> {
        let start = self.bump().span; // 'decoder'
        let name = self.ident()?;
        self.want(TokenKind::LBrace, "`{`")?;

        let mut width = None;
        let mut bit_order = None;
        let mut endian = None;
        let mut modes = Vec::new();
        let mut context = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            let key = self.ident()?;
            match key.text.as_str() {
                "width" => {
                    self.want(TokenKind::Assign, "`=`")?;
                    let (v, sp) = self.u32_lit()?;
                    width = Some(crate::source::Spanned::new(v, sp));
                }
                "bit_order" => {
                    self.want(TokenKind::Assign, "`=`")?;
                    let id = self.ident()?;
                    let bo = match id.text.as_str() {
                        "lsb0" => BitOrder::Lsb0,
                        "msb0" => BitOrder::Msb0,
                        _ => {
                            return Err(Diag::error(
                                "ParseError",
                                "bit_order must be `lsb0` or `msb0`",
                                id.span,
                            ))
                        }
                    };
                    bit_order = Some(crate::source::Spanned::new(bo, id.span));
                }
                "endian" => {
                    self.want(TokenKind::Assign, "`=`")?;
                    let id = self.ident()?;
                    let en = match id.text.as_str() {
                        "big" => Endian::Big,
                        "little" => Endian::Little,
                        _ => {
                            return Err(Diag::error(
                                "ParseError",
                                "endian must be `big` or `little`",
                                id.span,
                            ))
                        }
                    };
                    endian = Some(crate::source::Spanned::new(en, id.span));
                }
                "mode" => modes.push(self.mode_decl()?),
                "context" => context = self.context_block()?,
                other => {
                    return Err(Diag::error(
                        "ParseError",
                        format!("unknown decoder attribute `{other}`"),
                        key.span,
                    ))
                }
            }
        }

        let end = self.want(TokenKind::RBrace, "`}`")?;

        Ok(Decoder {
            name,
            width,
            bit_order,
            endian,
            modes,
            context,
            span: start.to(end),
        })
    }

    fn mode_decl(&mut self) -> PResult<ModeDecl> {
        let name = self.ident()?;
        self.want(TokenKind::Colon, "`:`")?;
        let ty = self.ident()?;
        let kind = match ty.text.as_str() {
            "bool" => ModeKind::Bool,
            "enum" => {
                self.want(TokenKind::LBrace, "`{`")?;
                let mut variants = Vec::new();
                loop {
                    variants.push(self.ident()?.text);
                    if !self.eat_comma() {
                        break;
                    }
                }
                self.want(TokenKind::RBrace, "`}`")?;
                ModeKind::Enum(variants)
            }
            t if t.starts_with('u')
                && t.len() > 1
                && t[1..].bytes().all(|c| c.is_ascii_digit()) =>
            {
                ModeKind::Uint(t[1..].parse().unwrap_or(1))
            }
            _ => {
                return Err(Diag::error(
                    "ParseError",
                    "mode type must be `bool`, `enum { ... }`, or `u<N>`",
                    ty.span,
                ))
            }
        };

        let mut default = 0u64;
        if matches!(self.peek(), TokenKind::Assign) {
            self.bump();
            match self.peek().clone() {
                TokenKind::Int { value, .. } => {
                    self.bump();
                    default = value as u64;
                }
                TokenKind::Ident(v) => {
                    self.bump();
                    if let ModeKind::Enum(vs) = &kind {
                        default = vs.iter().position(|x| *x == v).unwrap_or(0) as u64;
                    }
                }
                other => {
                    return self.err(format!("expected a mode default, found {}", show(&other)))
                }
            }
        }

        Ok(ModeDecl {
            span: name.span,
            name,
            kind,
            default,
        })
    }

    fn context_block(&mut self) -> PResult<Vec<CtxField>> {
        self.want(TokenKind::LBrace, "`{` after `context`")?;
        let mut fields = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            let name = self.ident()?;
            self.want(TokenKind::Colon, "`:` in context field")?;
            let ty = self.ident()?;
            let width = ty
                .text
                .strip_prefix('u')
                .filter(|s| !s.is_empty() && s.bytes().all(|c| c.is_ascii_digit()))
                .and_then(|s| s.parse::<u16>().ok())
                .filter(|w| (1..=64).contains(w))
                .ok_or_else(|| {
                    Diag::error(
                        "ParseError",
                        "context field type must be `u1`..`u64`",
                        ty.span,
                    )
                })?;

            let mut default = 0u64;
            if matches!(self.peek(), TokenKind::Assign) {
                self.bump();
                default = self.int()?.value as u64;
            }

            let span = name.span.to(self.last());
            fields.push(CtxField {
                name,
                width,
                default,
                span,
            });
        }
        self.want(TokenKind::RBrace, "`}`")?;
        Ok(fields)
    }

    // -------- selectors / values / forms --------

    fn selector(&mut self) -> PResult<Selector> {
        let start = self.bump().span; // 'selector'
        let name = self.ident()?;
        let range = self.src_range()?;
        Ok(Selector {
            name,
            range,
            span: start.to(range.span),
        })
    }

    fn value_decl(&mut self, kind: ValueKind) -> PResult<ValueDecl> {
        let start = self.bump().span; // 'operand' / 'type'
        let name = self.ident()?;
        self.want(TokenKind::Assign, "`=`")?;
        let base = self.ident()?;

        // An optional value recipe right after the base: `fetch(N)` or `assemble N { ... }`. Only
        // these two introduce a source, so a following `{` (attrs) or the next item is unambiguous.
        let source = if self.kw("fetch") || self.kw("assemble") {
            Some(self.expr()?)
        } else {
            None
        };

        let (xforms, disp) = self.value_attrs()?;
        Ok(ValueDecl {
            kind,
            name,
            base,
            xforms,
            disp,
            source,
            span: start.to(self.last()),
        })
    }

    fn value_attrs(&mut self) -> PResult<(Vec<Xform>, Option<DispAttr>)> {
        let mut xforms = Vec::new();
        let mut disp = None;

        if !matches!(self.peek(), TokenKind::LBrace) {
            return Ok((xforms, disp));
        }
        self.bump(); // '{'

        while !matches!(self.peek(), TokenKind::RBrace) {
            let id = self.ident()?;
            self.want(TokenKind::LParen, "`(`")?;
            match id.text.as_str() {
                "sign_extend" => xforms.push(Xform::SignExtend(self.u16_arg()?)),
                "zero_extend" => xforms.push(Xform::ZeroExtend(self.u16_arg()?)),
                "shift_left" => xforms.push(Xform::ShiftLeft(self.u16_arg()?)),
                "shift_right" => xforms.push(Xform::ShiftRight(self.u16_arg()?)),
                "rotate_left" => {
                    let (k, w) = self.two_u16()?;
                    xforms.push(Xform::RotateLeft(k, w));
                }
                "rotate_right" => {
                    let (k, w) = self.two_u16()?;
                    xforms.push(Xform::RotateRight(k, w));
                }
                "display" => {
                    let d = match self.peek().clone() {
                        TokenKind::Str(text) => {
                            let span = self.bump().span;
                            DispAttr::Pattern(StrLit { text, span })
                        }
                        TokenKind::Ident(h) if h == "names" => {
                            self.bump();
                            DispAttr::Names(self.names_table()?)
                        }
                        TokenKind::Ident(h) => {
                            let span = self.bump().span;
                            let hint = match h.as_str() {
                                "hex" => DispHint::Hex,
                                "signed_hex" => DispHint::SignedHex,
                                "dec" => DispHint::Dec,
                                _ => {
                                    return Err(Diag::error(
                                        "ParseError",
                                        "display hint must be `hex`, `signed_hex`, `dec`, or \
                                         `names { ... }`",
                                        span,
                                    ))
                                }
                            };
                            DispAttr::Hint(hint)
                        }
                        other => {
                            return self.err(format!(
                                "expected a display hint, pattern, or `names {{ ... }}`, found {}",
                                show(&other)
                            ))
                        }
                    };
                    self.want(TokenKind::RParen, "`)`")?;
                    disp = Some(d);
                    self.eat_comma();
                    continue;
                }
                other => {
                    return Err(Diag::error(
                        "ParseError",
                        format!("unknown type/operand modifier `{other}`"),
                        id.span,
                    ))
                }
            }
            self.want(TokenKind::RParen, "`)`")?;
            self.eat_comma();
        }

        self.want(TokenKind::RBrace, "`}`")?;
        Ok((xforms, disp))
    }

    fn two_u16(&mut self) -> PResult<(u16, u16)> {
        let a = self.u16_arg()?;
        self.want(TokenKind::Comma, "`,`")?;
        let b = self.u16_arg()?;
        Ok((a, b))
    }

    /// Parse a `names { 0 => "GE", 1 => "L", ..., _ => dec }` value-to-string table. The `_ => ...`
    /// default arm is mandatory and may be a literal string or a `dec`/`hex`/`signed_hex` hint.
    fn names_table(&mut self) -> PResult<NamesTable> {
        let start = self.want(TokenKind::LBrace, "`{` after `names`")?;
        let mut entries = Vec::new();
        let mut default = None;

        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            if self.kw("_") {
                let sp = self.bump().span;
                self.want(TokenKind::FatArrow, "`=>` after `_`")?;
                if default.is_some() {
                    return Err(Diag::error(
                        "ParseError",
                        "a `names` table has more than one `_ =>` default arm",
                        sp,
                    ));
                }
                default = Some(self.name_default()?);
            } else {
                let key = self.int()?.value;
                self.want(TokenKind::FatArrow, "`=>` in a names arm")?;
                let s = self.string()?;
                entries.push((key, s.text));
            }
            self.eat_comma();
        }

        let end = self.want(TokenKind::RBrace, "`}`")?;
        let default = default.ok_or_else(|| {
            Diag::error(
                "ParseError",
                "a `names { ... }` table needs a `_ => <default>` arm",
                start.to(end),
            )
        })?;
        Ok(NamesTable {
            entries,
            default,
            span: start.to(end),
        })
    }

    fn name_default(&mut self) -> PResult<NameDefault> {
        match self.peek().clone() {
            TokenKind::Str(text) => {
                self.bump();
                Ok(NameDefault::Str(text))
            }
            TokenKind::Ident(h) => {
                let span = self.bump().span;
                let hint = match h.as_str() {
                    "hex" => DispHint::Hex,
                    "signed_hex" => DispHint::SignedHex,
                    "dec" => DispHint::Dec,
                    _ => {
                        return Err(Diag::error(
                            "ParseError",
                            "a names default must be a string or `hex`/`signed_hex`/`dec`",
                            span,
                        ))
                    }
                };
                Ok(NameDefault::Hint(hint))
            }
            other => self.err(format!(
                "expected a names default (string or hint), found {}",
                show(&other)
            )),
        }
    }

    fn form(&mut self) -> PResult<Form> {
        let start = self.bump().span; // 'form'
        let name = self.ident()?;
        self.want(TokenKind::LBrace, "`{`")?;

        let mut fields = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            let fname = self.ident()?;
            self.want(TokenKind::Colon, "`:`")?;
            let ty = self.ident()?;
            let range = self.src_range()?;
            fields.push(FormField {
                span: fname.span.to(range.span),
                name: fname,
                ty,
                range,
            });
        }

        let end = self.want(TokenKind::RBrace, "`}`")?;
        Ok(Form {
            name,
            fields,
            span: start.to(end),
        })
    }

    fn func(&mut self) -> PResult<FuncDecl> {
        let start = self.bump().span; // 'fn'
        let name = self.ident()?;
        self.want(TokenKind::LParen, "`(`")?;

        let mut params = Vec::new();
        if !matches!(self.peek(), TokenKind::RParen) {
            loop {
                let pn = self.ident()?;
                self.want(TokenKind::Colon, "`:`")?;
                let pt = self.ident()?;
                params.push((pn, pt));
                if !self.eat_comma() {
                    break;
                }
            }
        }
        self.want(TokenKind::RParen, "`)`")?;

        self.want(TokenKind::Arrow, "`->`")?;
        let ret = self.ident()?;
        self.want(TokenKind::LBrace, "`{`")?;

        let mut lets = Vec::new();
        let mut ret_expr = None;
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            if self.kw("let") {
                self.bump();
                let ln = self.ident()?;
                self.want(TokenKind::Assign, "`=`")?;
                let e = self.expr()?;
                lets.push((ln, e));
            } else if self.kw("return") {
                self.bump();
                ret_expr = Some(self.expr()?);
                break;
            } else {
                return self.err("expected `let` or `return` in fn body");
            }
        }

        let end = self.want(TokenKind::RBrace, "`}`")?;
        let ret_expr = ret_expr
            .ok_or_else(|| Diag::error("ParseError", "fn body has no `return`", start.to(end)))?;

        Ok(FuncDecl {
            name,
            params,
            ret,
            lets,
            ret_expr,
            span: start.to(end),
        })
    }

    fn length(&mut self) -> PResult<LengthDecl> {
        let start = self.bump().span; // 'length'
        self.want(TokenKind::Assign, "`=` after `length`")?;

        let mut arms = Vec::new();
        while matches!(self.peek(), TokenKind::Pipe) {
            let bar = self.bump().span;
            let cond = if self.kw("else") {
                self.bump();
                None
            } else {
                Some(self.instr_expr()?)
            };
            self.want(TokenKind::Colon, "`:` after a length condition")?;
            let (bits, bsp) = self.u32_lit()?;
            arms.push(LengthArm {
                cond,
                bits,
                span: bar.to(bsp),
            });
        }

        if arms.is_empty() {
            return self.err("`length` needs at least one `| <cond> : <bits>` arm");
        }

        let end = arms.last().unwrap().span;
        Ok(LengthDecl {
            arms,
            span: start.to(end),
        })
    }

    fn prefix(&mut self) -> PResult<PrefixDecl> {
        let start = self.bump().span; // 'prefix'
        let name = self.ident()?;
        self.want(TokenKind::LBrace, "`{` after the prefix name")?;

        let mut arms = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            let arm_start = self.here();
            let pat = if self.kw("_") {
                self.bump();
                PrefixPat::Wildcard
            } else {
                let lo = self.int()?.value as u8;
                if matches!(self.peek(), TokenKind::DotDot) {
                    self.bump();
                    let hi = self.int()?.value as u8;
                    PrefixPat::Range(lo, hi)
                } else {
                    PrefixPat::Byte(lo)
                }
            };
            self.want(TokenKind::FatArrow, "`=>` in a prefix arm")?;
            let (assigns, term) = self.prefix_action()?;
            arms.push(PrefixArm {
                pat,
                assigns,
                term,
                span: arm_start.to(self.last()),
            });
        }

        let end = self.want(TokenKind::RBrace, "`}`")?;
        Ok(PrefixDecl {
            name,
            arms,
            span: start.to(end),
        })
    }

    fn prefix_action(&mut self) -> PResult<(Vec<(Ident, Expr)>, PrefixTerm)> {
        if self.kw("done") {
            self.bump();
            return Ok((Vec::new(), PrefixTerm::Done));
        }
        if self.kw("finish") {
            self.bump();
            return Ok((Vec::new(), PrefixTerm::Finish));
        }
        if matches!(self.peek(), TokenKind::LBrace) {
            self.bump();
            let mut assigns = Vec::new();
            let mut term = PrefixTerm::Continue;
            while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
                if self.kw("finish") {
                    self.bump();
                    term = PrefixTerm::Finish;
                    break;
                }
                if self.kw("done") {
                    self.bump();
                    term = PrefixTerm::Done;
                    break;
                }
                assigns.push(self.prefix_assign()?);
            }
            self.want(TokenKind::RBrace, "`}` to close the prefix action")?;
            return Ok((assigns, term));
        }
        let a = self.prefix_assign()?;
        Ok((vec![a], PrefixTerm::Continue))
    }

    fn prefix_assign(&mut self) -> PResult<(Ident, Expr)> {
        let name = self.ident()?;
        self.want(TokenKind::Assign, "`=` in a prefix assignment")?;
        let e = self.expr()?;
        Ok((name, e))
    }

    fn group(&mut self, dispatch: bool) -> PResult<GroupDecl> {
        let start = self.here();
        if dispatch {
            self.bump(); // 'dispatch'
        }
        let tag = self.ident()?;
        self.want(TokenKind::LBrace, "`{` after the group name")?;

        let mut members = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            members.push(self.ident()?);
            self.eat_comma();
        }

        let end = self.want(TokenKind::RBrace, "`}`")?;
        Ok(GroupDecl {
            tag,
            members,
            dispatch,
            span: start.to(end),
        })
    }

    // -------- subdecoders --------

    fn subdecoder(&mut self) -> PResult<SubDecoder> {
        let start = self.bump().span; // 'subdecoder'
        let name = self.ident()?;
        self.want(TokenKind::LBrace, "`{`")?;

        let mut width = None;
        let mut bit_order = None;
        let mut outputs = Vec::new();
        let mut arms = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            if self.kw("width") {
                self.bump();
                self.want(TokenKind::Assign, "`=`")?;
                let (v, sp) = self.u32_lit()?;
                width = Some(crate::source::Spanned::new(v, sp));
            } else if self.kw("bit_order") {
                self.bump();
                self.want(TokenKind::Assign, "`=`")?;
                let id = self.ident()?;
                let bo = match id.text.as_str() {
                    "lsb0" => BitOrder::Lsb0,
                    "msb0" => BitOrder::Msb0,
                    _ => {
                        return Err(Diag::error(
                            "ParseError",
                            "bit_order must be `lsb0` or `msb0`",
                            id.span,
                        ))
                    }
                };
                bit_order = Some(crate::source::Spanned::new(bo, id.span));
            } else if self.kw("outputs") {
                self.bump();
                self.want(TokenKind::LBrace, "`{` after `outputs`")?;
                while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
                    outputs.push(self.ident()?);
                    self.eat_comma();
                }
                self.want(TokenKind::RBrace, "`}`")?;
            } else {
                arms.push(self.sub_arm()?);
            }
        }

        let end = self.want(TokenKind::RBrace, "`}`")?;
        Ok(SubDecoder {
            name,
            width,
            bit_order,
            outputs,
            arms,
            span: start.to(end),
        })
    }

    fn sub_arm(&mut self) -> PResult<SubArm> {
        let name = self.ident()?;
        let mut constraints = Vec::new();
        let mut bindings = Vec::new();

        loop {
            match self.peek() {
                TokenKind::Pipe => break,
                TokenKind::Eof => {
                    return self.err("subdecoder arm is missing a `| <output> = \"...\"` line")
                }
                TokenKind::LBracket => {
                    let range = self.src_range()?;
                    self.want(TokenKind::Assign, "`=`")?;
                    let value = self.int()?;
                    constraints.push(Constraint::Range { range, value });
                }
                TokenKind::Ident(_) => {
                    let lhs = self.ident()?;
                    match self.peek() {
                        TokenKind::Assign => {
                            self.bump();
                            let value = self.int()?;
                            constraints.push(Constraint::Named { name: lhs, value });
                        }
                        TokenKind::Colon => {
                            self.bump();
                            let ty = self.ty_ref()?;
                            let range = self.src_range()?;
                            bindings.push(Binding {
                                span: lhs.span.to(range.span),
                                name: lhs,
                                ty,
                                range: Some(range),
                            });
                        }
                        other => {
                            return self.err(format!(
                                "expected `=` (constraint) or `:` (binding) after `{}`, found {}",
                                lhs.text,
                                show(other)
                            ))
                        }
                    }
                }
                other => {
                    return self.err(format!("unexpected {} in subdecoder arm body", show(other)))
                }
            }
        }

        let mut outputs = Vec::new();
        while matches!(self.peek(), TokenKind::Pipe) {
            let bar = self.bump().span;
            let oname = self.ident()?;
            self.want(TokenKind::Assign, "`=` after a subdecoder output name")?;
            let template = self.string()?;
            outputs.push(SubOutput {
                span: bar.to(template.span),
                name: oname,
                template,
            });
        }
        if outputs.is_empty() {
            return self.err("subdecoder arm is missing a `| <output> = \"...\"` line");
        }

        let span = name.span.to(outputs.last().unwrap().span);
        Ok(SubArm {
            name,
            constraints,
            bindings,
            outputs,
            span,
        })
    }

    // -------- instructions --------

    fn instr(&mut self) -> PResult<Instr> {
        let name = self.ident()?;
        let mut constraints = Vec::new();
        let mut bindings = Vec::new();
        let mut uses = None;
        let mut computed = Vec::new();
        let mut guard = None;

        loop {
            match self.peek() {
                TokenKind::Pipe => break,
                TokenKind::Eof => {
                    return self.err("instruction is missing a `| \"...\"` display line")
                }
                TokenKind::LBracket => {
                    let range = self.src_range()?;
                    self.want(TokenKind::Assign, "`=`")?;
                    let value = self.int()?;
                    constraints.push(Constraint::Range { range, value });
                }
                TokenKind::Ident(t) if t == "uses" => {
                    self.bump();
                    uses = Some(self.ident()?);
                }
                TokenKind::Ident(t) if t == "when" => {
                    self.bump();
                    guard = Some(self.instr_expr()?);
                }
                TokenKind::Ident(_) => {
                    let lhs = self.ident()?;
                    match self.peek() {
                        TokenKind::Assign => {
                            self.bump();
                            let value = self.int()?;
                            constraints.push(Constraint::Named { name: lhs, value });
                        }
                        TokenKind::Colon => {
                            self.bump();
                            let ty = self.ty_ref()?;
                            if matches!(self.peek(), TokenKind::Assign) {
                                self.bump();
                                let expr = self.instr_expr()?;
                                computed.push(Computed {
                                    span: lhs.span.to(expr.span()),
                                    name: lhs,
                                    ty: ty.name,
                                    expr,
                                });
                            } else if matches!(self.peek(), TokenKind::LBracket) {
                                let range = self.src_range()?;
                                bindings.push(Binding {
                                    span: lhs.span.to(range.span),
                                    name: lhs,
                                    ty,
                                    range: Some(range),
                                });
                            } else {
                                bindings.push(Binding {
                                    span: lhs.span.to(ty.span),
                                    name: lhs,
                                    ty,
                                    range: None,
                                });
                            }
                        }
                        other => {
                            return self.err(format!(
                                "expected `=` (constraint) or `:` (binding) after `{}`, found {}",
                                lhs.text,
                                show(other)
                            ))
                        }
                    }
                }
                other => {
                    return self.err(format!("unexpected {} in instruction body", show(other)))
                }
            }
        }

        let mut display = Vec::new();
        while matches!(self.peek(), TokenKind::Pipe) {
            let bar = self.bump().span;
            let cond = if matches!(self.peek(), TokenKind::Str(_)) {
                None
            } else {
                let e = self.expr()?;
                self.want(TokenKind::Colon, "`:` after a display condition")?;
                Some(e)
            };
            let template = self.string()?;
            display.push(DisplayArm {
                span: bar.to(template.span),
                cond,
                template,
            });
        }

        if display.is_empty() {
            return self.err("instruction is missing a `| \"...\"` display line");
        }

        let span = name.span.to(display.last().unwrap().span);
        Ok(Instr {
            name,
            constraints,
            bindings,
            uses,
            computed,
            guard,
            display,
            span,
        })
    }

    fn ty_ref(&mut self) -> PResult<TyRef> {
        let name = self.ident()?;
        let mut args = Vec::new();
        let mut span = name.span;
        if matches!(self.peek(), TokenKind::LParen) {
            self.bump();
            if !matches!(self.peek(), TokenKind::RParen) {
                loop {
                    args.push(self.expr()?);
                    if !self.eat_comma() {
                        break;
                    }
                }
            }
            span = span.to(self.want(TokenKind::RParen, "`)`")?);
        }
        Ok(TyRef { name, args, span })
    }

    /// Parse a `[a:b]` bracket range (or `[a]`, where `b` defaults to `a`). Returns the two
    /// endpoints in source order plus the bracket span.
    fn index_range(&mut self) -> PResult<(u32, u32, Span)> {
        let lb = self.want(TokenKind::LBracket, "`[`")?;
        let (a, _) = self.u32_lit()?;
        let b = if matches!(self.peek(), TokenKind::Colon) {
            self.bump();
            self.u32_lit()?.0
        } else {
            a
        };
        let rb = self.want(TokenKind::RBracket, "`]`")?;
        Ok((a, b, lb.to(rb)))
    }

    fn src_range(&mut self) -> PResult<SrcRange> {
        let (a, b, span) = self.index_range()?;
        Ok(SrcRange { a, b, span })
    }

    // -------- expressions --------

    fn expr(&mut self) -> PResult<Expr> {
        self.ternary()
    }

    /// An instruction-body expression, where a bare top-level `|` is the display delimiter.
    fn instr_expr(&mut self) -> PResult<Expr> {
        let saved = self.pipe_is_delim;
        self.pipe_is_delim = true;
        let r = self.expr();
        self.pipe_is_delim = saved;
        r
    }

    fn ternary(&mut self) -> PResult<Expr> {
        let cond = self.binary(0)?;
        if matches!(self.peek(), TokenKind::Question) {
            self.bump();
            let then = self.expr()?;
            self.want(TokenKind::Colon, "`:` in `? :`")?;
            let els = self.ternary()?;
            let span = cond.span().to(els.span());
            Ok(Expr::Cond {
                cond: Box::new(cond),
                then: Box::new(then),
                els: Box::new(els),
                span,
            })
        } else {
            Ok(cond)
        }
    }

    fn binary(&mut self, min_bp: u8) -> PResult<Expr> {
        let mut lhs = self.unary()?;
        while let Some((op, bp)) = infix(self.peek()) {
            if bp < min_bp {
                break;
            }
            if self.pipe_is_delim && op == BinOp::BitOr {
                break;
            }
            self.bump();
            let rhs = self.binary(bp + 1)?;
            let span = lhs.span().to(rhs.span());
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
            };
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> PResult<Expr> {
        let op = match self.peek() {
            TokenKind::Tilde => Some(UnOp::Not),
            TokenKind::Minus => Some(UnOp::Neg),
            _ => None,
        };
        if let Some(op) = op {
            let start = self.bump().span;
            let rhs = self.unary()?;
            let span = start.to(rhs.span());
            Ok(Expr::Unary {
                op,
                rhs: Box::new(rhs),
                span,
            })
        } else {
            self.postfix()
        }
    }

    fn postfix(&mut self) -> PResult<Expr> {
        let mut base = self.primary()?;
        while matches!(self.peek(), TokenKind::LBracket)
            && !(self.inside_assemble && self.bracket_then_assign())
        {
            let (hi, lo, span) = self.index_range()?;
            base = Expr::Slice {
                base: Box::new(base),
                hi,
                lo,
                span,
            };
        }
        Ok(base)
    }

    /// True when the current `[` begins a bracket group whose matching `]` is immediately followed
    /// by `=`, i.e. an `assemble` destination rather than a postfix slice.
    fn bracket_then_assign(&self) -> bool {
        let mut depth = 0i32;
        let mut i = self.pos;
        while i < self.toks.len() {
            match self.toks[i].kind {
                TokenKind::LBracket => depth += 1,
                TokenKind::RBracket => {
                    depth -= 1;
                    if depth == 0 {
                        return matches!(
                            self.toks.get(i + 1).map(|t| &t.kind),
                            Some(TokenKind::Assign)
                        );
                    }
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn primary(&mut self) -> PResult<Expr> {
        match self.peek().clone() {
            TokenKind::Int { value, width_hint } => {
                let span = self.bump().span;
                Ok(Expr::Int(IntLit {
                    value,
                    width_hint,
                    span,
                }))
            }
            TokenKind::Ident(t) if t == "assemble" => self.assemble(),
            TokenKind::Ident(_) => {
                let id = self.ident()?;
                if matches!(self.peek(), TokenKind::LParen) {
                    self.bump();
                    let mut args = Vec::new();
                    if !matches!(self.peek(), TokenKind::RParen) {
                        loop {
                            args.push(self.expr()?);
                            if !self.eat_comma() {
                                break;
                            }
                        }
                    }
                    let rp = self.want(TokenKind::RParen, "`)`")?;
                    Ok(Expr::Call {
                        span: id.span.to(rp),
                        callee: id,
                        args,
                    })
                } else {
                    Ok(Expr::Name(id))
                }
            }
            TokenKind::LParen => {
                self.bump();

                let saved = self.pipe_is_delim;
                self.pipe_is_delim = false; // inside parens `|` is bitwise-or
                let inner = self.expr();
                self.pipe_is_delim = saved;

                let inner = inner?;
                self.want(TokenKind::RParen, "`)`")?;
                Ok(inner)
            }
            other => self.err(format!("expected an expression, found {}", show(&other))),
        }
    }

    fn assemble(&mut self) -> PResult<Expr> {
        let start = self.bump().span; // 'assemble'

        let saved_pipe = self.pipe_is_delim;
        let saved_asm = self.inside_assemble;
        self.pipe_is_delim = false;
        self.inside_assemble = true;

        let (out_width, _) = self.u32_lit()?;
        self.want(TokenKind::LBrace, "`{`")?;

        let mut parts = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            let (hi, lo, brackets) = self.index_range()?;
            self.want(TokenKind::Assign, "`=`")?;
            let src = self.expr()?;
            parts.push(AssemblePart {
                hi,
                lo,
                span: brackets.to(src.span()),
                src,
            });
        }
        let end = self.want(TokenKind::RBrace, "`}`")?;

        let ext = if self.kw("sign_extend") {
            self.bump();
            Ext::SignExtend
        } else if self.kw("zero_extend") {
            self.bump();
            Ext::ZeroExtend
        } else {
            Ext::None
        };

        self.pipe_is_delim = saved_pipe;
        self.inside_assemble = saved_asm;

        Ok(Expr::Assemble {
            out_width,
            parts,
            ext,
            span: start.to(end),
        })
    }
}

/// Map a token to its binary operator and binding power (higher binds tighter).
fn infix(k: &TokenKind) -> Option<(BinOp, u8)> {
    Some(match k {
        TokenKind::PipePipe => (BinOp::LOr, 1),
        TokenKind::AmpAmp => (BinOp::LAnd, 2),
        TokenKind::Pipe => (BinOp::BitOr, 3),
        TokenKind::Caret => (BinOp::BitXor, 4),
        TokenKind::Amp => (BinOp::BitAnd, 5),
        TokenKind::EqEq => (BinOp::Eq, 6),
        TokenKind::Ne => (BinOp::Ne, 6),
        TokenKind::Lt => (BinOp::Lt, 7),
        TokenKind::Le => (BinOp::Le, 7),
        TokenKind::Gt => (BinOp::Gt, 7),
        TokenKind::Ge => (BinOp::Ge, 7),
        TokenKind::Shl => (BinOp::Shl, 8),
        TokenKind::Shr => (BinOp::Shr, 8),
        TokenKind::Plus => (BinOp::Add, 9),
        TokenKind::Minus => (BinOp::Sub, 9),
        TokenKind::Star => (BinOp::Mul, 10),
        TokenKind::Slash => (BinOp::Div, 10),
        TokenKind::Percent => (BinOp::Rem, 10),
        _ => return None,
    })
}

/// A short human description of a token for error messages.
fn show(k: &TokenKind) -> String {
    match k {
        TokenKind::Ident(s) => format!("`{s}`"),
        TokenKind::Int { value, .. } => format!("`{value}`"),
        TokenKind::Str(_) => "a string".to_string(),
        TokenKind::Eof => "end of input".to_string(),
        TokenKind::LBrace => "`{`".into(),
        TokenKind::RBrace => "`}`".into(),
        TokenKind::LParen => "`(`".into(),
        TokenKind::RParen => "`)`".into(),
        TokenKind::LBracket => "`[`".into(),
        TokenKind::RBracket => "`]`".into(),
        TokenKind::Colon => "`:`".into(),
        TokenKind::Comma => "`,`".into(),
        TokenKind::Dot => "`.`".into(),
        TokenKind::DotDot => "`..`".into(),
        TokenKind::Arrow => "`->`".into(),
        TokenKind::FatArrow => "`=>`".into(),
        TokenKind::Pipe => "`|`".into(),
        TokenKind::PipePipe => "`||`".into(),
        TokenKind::Amp => "`&`".into(),
        TokenKind::AmpAmp => "`&&`".into(),
        TokenKind::Caret => "`^`".into(),
        TokenKind::Tilde => "`~`".into(),
        TokenKind::Question => "`?`".into(),
        TokenKind::Assign => "`=`".into(),
        TokenKind::EqEq => "`==`".into(),
        TokenKind::Ne => "`!=`".into(),
        TokenKind::Lt => "`<`".into(),
        TokenKind::Le => "`<=`".into(),
        TokenKind::Gt => "`>`".into(),
        TokenKind::Ge => "`>=`".into(),
        TokenKind::Shl => "`<<`".into(),
        TokenKind::Shr => "`>>`".into(),
        TokenKind::Plus => "`+`".into(),
        TokenKind::Minus => "`-`".into(),
        TokenKind::Star => "`*`".into(),
        TokenKind::Slash => "`/`".into(),
        TokenKind::Percent => "`%`".into(),
    }
}
