//! Hand-written lexer. Turns source text into a flat vector of spanned tokens.

use crate::report::Diag;
use crate::source::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(String),
    /// Integer literal value, plus an optional width hint guessed from the spelling.
    /// Hex gives 4 bits per digit, binary 1 bit per digit, decimal none.
    Int {
        value: u128,
        width_hint: Option<u16>,
    },
    Str(String),

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Colon,
    Comma,
    Dot,
    DotDot,
    Arrow,    // ->
    FatArrow, // =>
    Pipe,     // |
    PipePipe, // ||
    Amp,      // &
    AmpAmp,   // &&
    Caret,    // ^
    Tilde,    // ~
    Question, // ?
    Assign,   // =
    EqEq,     // ==
    Ne,       // !=
    Lt,
    Le,
    Gt,
    Ge,
    Shl, // <<
    Shr, // >>
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Tokenize `src`. Stops at the first bad token.
pub fn lex(src: &str) -> Result<Vec<Token>, Diag> {
    let mut lx = Lexer {
        src,
        bytes: src.as_bytes(),
        pos: 0,
        out: Vec::new(),
    };
    lx.run()?;
    Ok(lx.out)
}

struct Lexer<'a> {
    src: &'a str,
    bytes: &'a [u8],
    pos: usize,
    out: Vec<Token>,
}

impl Lexer<'_> {
    fn run(&mut self) -> Result<(), Diag> {
        while self.pos < self.bytes.len() {
            match self.bytes[self.pos] {
                b' ' | b'\t' | b'\r' | b'\n' => self.pos += 1,
                b'#' => self.skip_comment(),
                b'"' => self.string()?,
                b'0'..=b'9' => self.number()?,
                c if ident_start(c) => self.ident(),
                _ => self.punct()?,
            }
        }

        let end = self.bytes.len();
        self.emit(TokenKind::Eof, end, end);
        Ok(())
    }

    fn emit(&mut self, kind: TokenKind, start: usize, end: usize) {
        self.out.push(Token {
            kind,
            span: Span::new(start as u32, end as u32),
        });
    }

    fn skip_comment(&mut self) {
        while self.pos < self.bytes.len() && self.bytes[self.pos] != b'\n' {
            self.pos += 1;
        }
    }

    fn string(&mut self) -> Result<(), Diag> {
        let start = self.pos;
        self.pos += 1; // opening quote

        let mut text = String::new();
        loop {
            if self.pos >= self.bytes.len() {
                return Err(Diag::error(
                    "LexUnterminatedString",
                    "unterminated string literal",
                    Span::new(start as u32, self.bytes.len() as u32),
                ));
            }
            let c = self.bytes[self.pos];
            if c == b'"' {
                self.pos += 1;
                break;
            }
            // String-level escapes are only `\"` and `\\`. The display mini-language
            // sequences `\{ \} \? \: \\` pass through unchanged, backslash and all, so
            // the template parser can handle them later.
            if c == b'\\' && self.pos + 1 < self.bytes.len() {
                match self.bytes[self.pos + 1] {
                    b'"' => {
                        text.push('"');
                        self.pos += 2;
                        continue;
                    }
                    b'\\' => {
                        text.push('\\');
                        self.pos += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            let w = utf8_len(c);
            text.push_str(&self.src[self.pos..self.pos + w]);
            self.pos += w;
        }

        self.emit(TokenKind::Str(text), start, self.pos);
        Ok(())
    }

    fn number(&mut self) -> Result<(), Diag> {
        let start = self.pos;
        let b = self.bytes;
        let has_next = start + 1 < b.len();
        let (radix, per_digit) =
            if b[start] == b'0' && has_next && matches!(b[start + 1], b'x' | b'X') {
                self.pos += 2;
                (16u32, 4u16)
            } else if b[start] == b'0' && has_next && matches!(b[start + 1], b'b' | b'B') {
                self.pos += 2;
                (2u32, 1u16)
            } else {
                (10u32, 0u16)
            };

        let digits_from = self.pos;
        while self.pos < b.len() && (digit_in(b[self.pos], radix) || b[self.pos] == b'_') {
            self.pos += 1;
        }

        let digits: String = self.src[digits_from..self.pos]
            .chars()
            .filter(|&c| c != '_')
            .collect();
        let span = Span::new(start as u32, self.pos as u32);

        if digits.is_empty() {
            return Err(Diag::error(
                "LexBadNumber",
                "numeric literal has no digits",
                span,
            ));
        }

        let value = u128::from_str_radix(&digits, radix)
            .map_err(|_| Diag::error("LexBadNumber", "numeric literal out of range", span))?;

        let width_hint = if radix == 10 {
            None
        } else {
            Some((digits.len() as u16).saturating_mul(per_digit))
        };
        self.emit(TokenKind::Int { value, width_hint }, start, self.pos);
        Ok(())
    }

    fn ident(&mut self) {
        let start = self.pos;
        self.pos += 1;
        while self.pos < self.bytes.len() && ident_continue(self.bytes[self.pos]) {
            self.pos += 1;
        }
        let text = self.src[start..self.pos].to_string();
        self.emit(TokenKind::Ident(text), start, self.pos);
    }

    fn punct(&mut self) -> Result<(), Diag> {
        let start = self.pos;
        let b = self.bytes;
        let c = b[start];
        let d = b.get(start + 1).copied();

        let (kind, len) = match (c, d) {
            (b'-', Some(b'>')) => (TokenKind::Arrow, 2),
            (b'.', Some(b'.')) => (TokenKind::DotDot, 2),
            (b'=', Some(b'>')) => (TokenKind::FatArrow, 2),
            (b'=', Some(b'=')) => (TokenKind::EqEq, 2),
            (b'!', Some(b'=')) => (TokenKind::Ne, 2),
            (b'<', Some(b'=')) => (TokenKind::Le, 2),
            (b'>', Some(b'=')) => (TokenKind::Ge, 2),
            (b'<', Some(b'<')) => (TokenKind::Shl, 2),
            (b'>', Some(b'>')) => (TokenKind::Shr, 2),
            (b'&', Some(b'&')) => (TokenKind::AmpAmp, 2),
            (b'|', Some(b'|')) => (TokenKind::PipePipe, 2),
            (b'{', _) => (TokenKind::LBrace, 1),
            (b'}', _) => (TokenKind::RBrace, 1),
            (b'(', _) => (TokenKind::LParen, 1),
            (b')', _) => (TokenKind::RParen, 1),
            (b'[', _) => (TokenKind::LBracket, 1),
            (b']', _) => (TokenKind::RBracket, 1),
            (b':', _) => (TokenKind::Colon, 1),
            (b',', _) => (TokenKind::Comma, 1),
            (b'.', _) => (TokenKind::Dot, 1),
            (b'|', _) => (TokenKind::Pipe, 1),
            (b'&', _) => (TokenKind::Amp, 1),
            (b'^', _) => (TokenKind::Caret, 1),
            (b'~', _) => (TokenKind::Tilde, 1),
            (b'?', _) => (TokenKind::Question, 1),
            (b'=', _) => (TokenKind::Assign, 1),
            (b'<', _) => (TokenKind::Lt, 1),
            (b'>', _) => (TokenKind::Gt, 1),
            (b'+', _) => (TokenKind::Plus, 1),
            (b'-', _) => (TokenKind::Minus, 1),
            (b'*', _) => (TokenKind::Star, 1),
            (b'/', _) => (TokenKind::Slash, 1),
            (b'%', _) => (TokenKind::Percent, 1),
            _ => {
                return Err(Diag::error(
                    "LexUnexpectedChar",
                    format!("unexpected character `{}`", c as char),
                    Span::new(start as u32, (start + 1) as u32),
                ))
            }
        };

        self.pos += len;
        self.emit(kind, start, start + len);
        Ok(())
    }
}

fn ident_start(c: u8) -> bool {
    c == b'_' || c.is_ascii_alphabetic()
}

fn ident_continue(c: u8) -> bool {
    c == b'_' || c.is_ascii_alphanumeric()
}

fn digit_in(c: u8, radix: u32) -> bool {
    match radix {
        16 => c.is_ascii_hexdigit(),
        2 => c == b'0' || c == b'1',
        _ => c.is_ascii_digit(),
    }
}

fn utf8_len(first: u8) -> usize {
    match first {
        _ if first < 0x80 => 1,
        _ if first >> 5 == 0b110 => 2,
        _ if first >> 4 == 0b1110 => 3,
        _ => 4,
    }
}
