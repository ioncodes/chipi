//! Lexer and recursive-descent parser for `*.bindings.chipi` files.
//!
//! The grammar is brace-based:
//!
//! ```text
//! file       = (include | target)*
//! include    = 'include' STRING
//! target     = 'target' IDENT '{' target_body '}'
//! ```
//!
//! Tokens:
//! - `Ident`. Matches `[A-Za-z_][A-Za-z0-9_:]*`. `::` is part of paths.
//! - `String`. Double-quoted.
//! - `Number`. Decimal or `0x...` hex.
//! - `{` `}` `<` `>` `=` `,`
//! - `#`-to-EOL comments

use std::collections::BTreeMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::config::Dispatch;
use crate::error::{Error, ErrorKind, Span};

use super::types::*;

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    Ident(String),
    Str(String),
    Num(u64),
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Eq,
    Comma,
    Eof,
}

#[derive(Debug, Clone)]
struct Token {
    tok: Tok,
    span: Span,
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
    line: usize,
    col: usize,
    file: String,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str, filename: &str) -> Self {
        Self {
            src: source.as_bytes(),
            pos: 0,
            line: 1,
            col: 1,
            file: filename.to_string(),
        }
    }

    fn span(&self, start_line: usize, start_col: usize, len: usize) -> Span {
        Span::new(&self.file, start_line, start_col, len)
    }

    fn peek_byte(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let b = self.peek_byte()?;
        self.pos += 1;
        if b == b'\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(b)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_byte() {
                Some(b' ' | b'\t' | b'\n' | b'\r') => {
                    self.bump();
                }
                Some(b'#') => {
                    while let Some(b) = self.peek_byte() {
                        if b == b'\n' {
                            break;
                        }
                        self.bump();
                    }
                }
                _ => break,
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, Error> {
        self.skip_whitespace_and_comments();
        let start_line = self.line;
        let start_col = self.col;
        let start_pos = self.pos;

        let b = match self.peek_byte() {
            Some(b) => b,
            None => {
                return Ok(Token {
                    tok: Tok::Eof,
                    span: self.span(start_line, start_col, 0),
                });
            }
        };

        match b {
            b'{' => {
                self.bump();
                Ok(Token {
                    tok: Tok::LBrace,
                    span: self.span(start_line, start_col, 1),
                })
            }
            b'}' => {
                self.bump();
                Ok(Token {
                    tok: Tok::RBrace,
                    span: self.span(start_line, start_col, 1),
                })
            }
            b'<' => {
                self.bump();
                Ok(Token {
                    tok: Tok::LAngle,
                    span: self.span(start_line, start_col, 1),
                })
            }
            b'>' => {
                self.bump();
                Ok(Token {
                    tok: Tok::RAngle,
                    span: self.span(start_line, start_col, 1),
                })
            }
            b'=' => {
                self.bump();
                Ok(Token {
                    tok: Tok::Eq,
                    span: self.span(start_line, start_col, 1),
                })
            }
            b',' => {
                self.bump();
                Ok(Token {
                    tok: Tok::Comma,
                    span: self.span(start_line, start_col, 1),
                })
            }
            b'"' => {
                self.bump();
                let mut s = String::new();
                loop {
                    match self.peek_byte() {
                        None => {
                            return Err(Error::new(
                                ErrorKind::BindingsParse("unterminated string literal".to_string()),
                                self.span(start_line, start_col, 1),
                            ));
                        }
                        Some(b'"') => {
                            self.bump();
                            break;
                        }
                        Some(b'\\') => {
                            self.bump();
                            match self.bump() {
                                Some(b'"') => s.push('"'),
                                Some(b'\\') => s.push('\\'),
                                Some(b'n') => s.push('\n'),
                                Some(b't') => s.push('\t'),
                                Some(c) => s.push(c as char),
                                None => {
                                    return Err(Error::new(
                                        ErrorKind::BindingsParse(
                                            "unterminated string escape".to_string(),
                                        ),
                                        self.span(start_line, start_col, 1),
                                    ));
                                }
                            }
                        }
                        Some(c) => {
                            s.push(c as char);
                            self.bump();
                        }
                    }
                }
                let len = self.pos - start_pos;
                Ok(Token {
                    tok: Tok::Str(s),
                    span: self.span(start_line, start_col, len),
                })
            }
            b'0'..=b'9' => {
                let mut s = String::new();
                while let Some(c) = self.peek_byte() {
                    if c.is_ascii_alphanumeric() || c == b'x' || c == b'X' {
                        s.push(c as char);
                        self.bump();
                    } else {
                        break;
                    }
                }
                let n = if let Some(rest) = s.strip_prefix("0x").or(s.strip_prefix("0X")) {
                    u64::from_str_radix(rest, 16)
                } else {
                    s.parse::<u64>()
                };
                let n = n.map_err(|_| {
                    Error::new(
                        ErrorKind::BindingsParse(format!("invalid number literal '{}'", s)),
                        self.span(start_line, start_col, s.len()),
                    )
                })?;
                let len = self.pos - start_pos;
                Ok(Token {
                    tok: Tok::Num(n),
                    span: self.span(start_line, start_col, len),
                })
            }
            c if is_ident_start(c) => {
                let mut s = String::new();
                while let Some(c) = self.peek_byte() {
                    if is_ident_cont(c) {
                        s.push(c as char);
                        self.bump();
                    } else if c == b':' && self.src.get(self.pos + 1) == Some(&b':') {
                        s.push_str("::");
                        self.bump();
                        self.bump();
                    } else if c == b'$' {
                        // allow $VAR / ${VAR} inside path-like idents (for output paths
                        // we use strings, so this is not strictly needed; kept for
                        // robustness)
                        s.push(c as char);
                        self.bump();
                    } else {
                        break;
                    }
                }
                let len = self.pos - start_pos;
                Ok(Token {
                    tok: Tok::Ident(s),
                    span: self.span(start_line, start_col, len),
                })
            }
            _ => Err(Error::new(
                ErrorKind::BindingsParse(format!("unexpected character '{}'", b as char)),
                self.span(start_line, start_col, 1),
            )),
        }
    }
}

fn is_ident_start(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

fn is_ident_cont(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

struct Parser<'a> {
    lex: Lexer<'a>,
    peeked: Option<Token>,
    file_path: PathBuf,
}

/// Parse a bindings file (without resolving includes).
pub fn parse(source: &str, file_path: &Path) -> Result<BindingsFile, Vec<Error>> {
    let filename = file_path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("<bindings>");
    let mut p = Parser {
        lex: Lexer::new(source, filename),
        peeked: None,
        file_path: file_path.to_path_buf(),
    };
    p.parse_file()
}

/// Parse a bindings file from disk (without resolving includes).
pub fn parse_file(path: &Path) -> Result<BindingsFile, Vec<Error>> {
    let source = std::fs::read_to_string(path).map_err(|_| {
        vec![Error::new(
            ErrorKind::IncludeNotFound(path.display().to_string()),
            Span::new(&path.display().to_string(), 1, 1, 0),
        )]
    })?;
    parse(&source, path)
}

/// Parse a bindings file and recursively follow `include "*.bindings.chipi"`
/// directives. Spec includes (`*.chipi`) are recorded but not parsed.
pub fn parse_file_with_includes(path: &Path) -> Result<BindingsFile, Vec<Error>> {
    let mut visited: HashSet<PathBuf> = HashSet::new();
    parse_file_recursive(path, &mut visited)
}

fn parse_file_recursive(
    path: &Path,
    visited: &mut HashSet<PathBuf>,
) -> Result<BindingsFile, Vec<Error>> {
    let canonical = path.canonicalize().map_err(|_| {
        vec![Error::new(
            ErrorKind::IncludeNotFound(path.display().to_string()),
            Span::new(&path.display().to_string(), 1, 1, 0),
        )]
    })?;
    if !visited.insert(canonical.clone()) {
        return Err(vec![Error::new(
            ErrorKind::BindingsCircularInclude(path.display().to_string()),
            Span::new(&path.display().to_string(), 1, 1, 0),
        )]);
    }
    let mut file = parse_file(&canonical)?;
    file.path = canonical.clone();
    let base = canonical.parent().unwrap_or(Path::new(".")).to_path_buf();
    let mut merged_targets: Vec<TargetBinding> = std::mem::take(&mut file.targets);
    let mut merged_specs: Vec<(PathBuf, Span)> = std::mem::take(&mut file.spec_includes);

    let bindings_inc = std::mem::take(&mut file.bindings_includes);
    let mut resolved_bindings_inc: Vec<(PathBuf, Span)> = Vec::new();
    for (rel, span) in bindings_inc {
        let abs = if rel.is_absolute() {
            rel
        } else {
            base.join(rel)
        };
        let inc = parse_file_recursive(&abs, visited)?;
        // Deep-merge: pull in all transitively included bindings + specs.
        merged_targets.extend(inc.targets);
        merged_specs.extend(inc.spec_includes);
        resolved_bindings_inc.push((inc.path, span));
    }

    // Resolve relative spec paths to canonical absolute paths.
    let mut resolved_specs: Vec<(PathBuf, Span)> = Vec::new();
    for (rel, span) in merged_specs {
        let abs = if rel.is_absolute() {
            rel
        } else {
            base.join(rel)
        };
        let canon = abs.canonicalize().unwrap_or(abs);
        resolved_specs.push((canon, span));
    }

    file.targets = merged_targets;
    file.spec_includes = resolved_specs;
    file.bindings_includes = resolved_bindings_inc;
    Ok(file)
}

impl<'a> Parser<'a> {
    fn peek(&mut self) -> Result<&Token, Error> {
        if self.peeked.is_none() {
            self.peeked = Some(self.lex.next_token()?);
        }
        Ok(self.peeked.as_ref().unwrap())
    }

    /// Convenience: peek and return an owned copy so callers don't trip the
    /// borrow checker when they need to call `self.next()` in the same
    /// match arm.
    fn peek_clone(&mut self) -> Result<Token, Error> {
        Ok(self.peek()?.clone())
    }

    fn next(&mut self) -> Result<Token, Error> {
        if let Some(t) = self.peeked.take() {
            return Ok(t);
        }
        self.lex.next_token()
    }

    fn expect_ident(&mut self) -> Result<(String, Span), Error> {
        let t = self.next()?;
        match t.tok {
            Tok::Ident(s) => Ok((s, t.span)),
            other => Err(Error::new(
                ErrorKind::BindingsParse(format!("expected identifier, got {:?}", other)),
                t.span,
            )),
        }
    }

    fn expect_string(&mut self) -> Result<(String, Span), Error> {
        let t = self.next()?;
        match t.tok {
            Tok::Str(s) => Ok((s, t.span)),
            other => Err(Error::new(
                ErrorKind::BindingsParse(format!("expected string literal, got {:?}", other)),
                t.span,
            )),
        }
    }

    fn expect_number(&mut self) -> Result<(u64, Span), Error> {
        let t = self.next()?;
        match t.tok {
            Tok::Num(n) => Ok((n, t.span)),
            other => Err(Error::new(
                ErrorKind::BindingsParse(format!("expected number, got {:?}", other)),
                t.span,
            )),
        }
    }

    fn expect_lbrace(&mut self) -> Result<Span, Error> {
        let t = self.next()?;
        match t.tok {
            Tok::LBrace => Ok(t.span),
            other => Err(Error::new(
                ErrorKind::BindingsParse(format!("expected '{{', got {:?}", other)),
                t.span,
            )),
        }
    }

    fn parse_file(&mut self) -> Result<BindingsFile, Vec<Error>> {
        let mut errors: Vec<Error> = Vec::new();
        let mut spec_includes: Vec<(PathBuf, Span)> = Vec::new();
        let mut bindings_includes: Vec<(PathBuf, Span)> = Vec::new();
        let mut targets: Vec<TargetBinding> = Vec::new();

        loop {
            let t = match self.peek() {
                Ok(t) => t,
                Err(e) => {
                    errors.push(e);
                    break;
                }
            };
            match &t.tok {
                Tok::Eof => break,
                Tok::Ident(name) if name == "include" => {
                    self.next().ok();
                    match self.expect_string() {
                        Ok((s, span)) => {
                            let p = PathBuf::from(&s);
                            if s.ends_with(".bindings.chipi") {
                                bindings_includes.push((p, span));
                            } else if s.ends_with(".chipi") {
                                spec_includes.push((p, span));
                            } else {
                                errors.push(Error::new(
                                    ErrorKind::BindingsParse(format!(
                                        "include path '{}' must end in .chipi or .bindings.chipi",
                                        s
                                    )),
                                    span,
                                ));
                            }
                        }
                        Err(e) => errors.push(e),
                    }
                }
                Tok::Ident(name) if name == "target" => match self.parse_target() {
                    Ok(t) => targets.push(t),
                    Err(e) => errors.push(e),
                },
                _ => {
                    let t = self.next().unwrap();
                    errors.push(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "expected 'include' or 'target' at top level, got {:?}",
                            t.tok
                        )),
                        t.span,
                    ));
                }
            }
        }

        if errors.is_empty() {
            Ok(BindingsFile {
                path: self.file_path.clone(),
                spec_includes,
                bindings_includes,
                targets,
            })
        } else {
            Err(errors)
        }
    }

    fn parse_target(&mut self) -> Result<TargetBinding, Error> {
        // Consume 'target'
        let target_tok = self.next()?;
        let span = target_tok.span;
        let (kind_name, kind_span) = self.expect_ident()?;
        let kind = match kind_name.as_str() {
            "rust" => TargetKind::Rust,
            "cpp" | "c++" => TargetKind::Cpp,
            "ida" => TargetKind::Ida,
            "binja" => TargetKind::Binja,
            other => {
                return Err(Error::new(
                    ErrorKind::UnknownTargetKind(other.to_string()),
                    kind_span,
                ));
            }
        };
        self.expect_lbrace()?;

        let mut binding = TargetBinding::empty(kind, span);

        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Eof => {
                    return Err(Error::new(
                        ErrorKind::BindingsParse(
                            "unexpected end of file inside target block".to_string(),
                        ),
                        t.span.clone(),
                    ));
                }
                Tok::Ident(name) => match (kind, name.as_str()) {
                    (TargetKind::Rust, "decoder") => {
                        let d = self.parse_decoder_block()?;
                        binding.rust_decoders.push(d);
                    }
                    (TargetKind::Rust, "dispatch") => {
                        let d = self.parse_dispatch_block()?;
                        binding.rust_dispatches.push(d);
                    }
                    (TargetKind::Cpp, "decoder") => {
                        let d = self.parse_decoder_block()?;
                        binding.cpp_decoders.push(d);
                    }
                    (TargetKind::Ida, "processor") => {
                        let p = self.parse_processor_block()?;
                        binding.ida_processors.push(p);
                    }
                    (TargetKind::Binja, "architecture") => {
                        let a = self.parse_architecture_block()?;
                        binding.binja_architectures.push(a);
                    }
                    _ => {
                        let t = self.next()?;
                        return Err(Error::new(
                            ErrorKind::BindingsParse(format!(
                                "unexpected '{}' inside target {}",
                                name,
                                kind.name()
                            )),
                            t.span,
                        ));
                    }
                },
                other => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "unexpected token {:?} inside target block",
                            other
                        )),
                        t.span,
                    ));
                }
            }
        }

        Ok(binding)
    }

    fn parse_decoder_block(&mut self) -> Result<DecoderBinding, Error> {
        // Consume 'decoder'
        self.next()?;
        let (name, span) = self.expect_ident()?;
        self.expect_lbrace()?;

        let mut output: Option<String> = None;
        let mut type_map: BTreeMap<String, String> = BTreeMap::new();
        let mut subdecoders: Vec<DecoderBinding> = Vec::new();
        let mut cpp_namespace: Option<String> = None;
        let mut cpp_guard_style: Option<String> = None;
        let mut cpp_includes: Vec<String> = Vec::new();

        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Eof => {
                    return Err(Error::new(
                        ErrorKind::BindingsParse(
                            "unexpected end of file inside decoder block".to_string(),
                        ),
                        t.span.clone(),
                    ));
                }
                Tok::Ident(name) => {
                    let name = name.clone();
                    match name.as_str() {
                        "output" => {
                            self.next()?;
                            let (s, _) = self.expect_string()?;
                            output = Some(s);
                        }
                        "type" => {
                            self.next()?;
                            let (lhs, _) = self.expect_ident()?;
                            // expect '='
                            let eq = self.next()?;
                            if !matches!(eq.tok, Tok::Eq) {
                                return Err(Error::new(
                                    ErrorKind::BindingsParse(
                                        "expected '=' in type alias".to_string(),
                                    ),
                                    eq.span,
                                ));
                            }
                            let (rhs, _) = self.expect_ident()?;
                            type_map.insert(lhs, rhs);
                        }
                        "subdecoder" => {
                            let sd = self.parse_decoder_block()?;
                            subdecoders.push(sd);
                        }
                        "namespace" => {
                            self.next()?;
                            let (s, _) = self.expect_string()?;
                            cpp_namespace = Some(s);
                        }
                        "guard_style" => {
                            self.next()?;
                            let (s, _) = self.expect_ident()?;
                            cpp_guard_style = Some(s);
                        }
                        "includes" => {
                            self.next()?;
                            cpp_includes = self.parse_string_block()?;
                        }
                        other => {
                            let t = self.next()?;
                            return Err(Error::new(
                                ErrorKind::BindingsParse(format!(
                                    "unexpected '{}' inside decoder block",
                                    other
                                )),
                                t.span,
                            ));
                        }
                    }
                }
                _ => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "unexpected token {:?} inside decoder block",
                            t.tok
                        )),
                        t.span,
                    ));
                }
            }
        }

        let output = output.ok_or_else(|| {
            Error::new(
                ErrorKind::MissingBindingsField {
                    block: format!("decoder {}", name),
                    field: "output".to_string(),
                },
                span.clone(),
            )
        })?;

        Ok(DecoderBinding {
            decoder_name: name,
            span,
            output,
            type_map,
            subdecoders,
            cpp_namespace,
            cpp_guard_style,
            cpp_includes,
        })
    }

    fn parse_dispatch_block(&mut self) -> Result<DispatchBinding, Error> {
        // Consume 'dispatch' (or 'subdispatch'). The caller handles either.
        self.next()?;
        let (name, span) = self.expect_ident()?;
        self.expect_lbrace()?;

        let mut output: Option<String> = None;
        let mut context: Option<String> = None;
        let mut handlers: Option<String> = None;
        let mut strategy: Option<Dispatch> = None;
        let mut invalid_handler: Option<String> = None;
        let mut instruction_type: Option<InstructionTypeBinding> = None;
        let mut handler_groups: Vec<HandlerBinding> = Vec::new();
        let mut subdispatches: Vec<DispatchBinding> = Vec::new();
        let mut handler_consts: Vec<String> = Vec::new();

        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Eof => {
                    return Err(Error::new(
                        ErrorKind::BindingsParse(
                            "unexpected end of file inside dispatch block".to_string(),
                        ),
                        t.span.clone(),
                    ));
                }
                Tok::Ident(name) => {
                    let name = name.clone();
                    match name.as_str() {
                        "output" => {
                            self.next()?;
                            let (s, _) = self.expect_string()?;
                            output = Some(s);
                        }
                        "context" => {
                            self.next()?;
                            let (s, _) = self.expect_ident()?;
                            context = Some(s);
                        }
                        "handlers" => {
                            self.next()?;
                            let (s, _) = self.expect_ident()?;
                            handlers = Some(s);
                        }
                        "strategy" => {
                            self.next()?;
                            let (s, span) = self.expect_ident()?;
                            strategy = Some(parse_strategy(&s, &span)?);
                        }
                        "invalid_handler" => {
                            self.next()?;
                            let (s, _) = self.expect_ident()?;
                            invalid_handler = Some(s);
                        }
                        "handler_const" => {
                            self.next()?;
                            let (s, _) = self.expect_ident()?;
                            handler_consts.push(s);
                        }
                        "instruction_type" => {
                            self.next()?;
                            let (path, _) = self.expect_ident()?;
                            // Optional `{ output "..." }`
                            let next = self.peek()?;
                            if matches!(next.tok, Tok::LBrace) {
                                self.next()?;
                                let mut inner_out: Option<String> = None;
                                loop {
                                    let t = self.peek_clone()?;
                                    match &t.tok {
                                        Tok::RBrace => {
                                            self.next()?;
                                            break;
                                        }
                                        Tok::Ident(n) if n == "output" => {
                                            self.next()?;
                                            let (s, _) = self.expect_string()?;
                                            inner_out = Some(s);
                                        }
                                        _ => {
                                            let t = self.next()?;
                                            return Err(Error::new(
                                                ErrorKind::BindingsParse(format!(
                                                    "unexpected token {:?} inside instruction_type block",
                                                    t.tok
                                                )),
                                                t.span,
                                            ));
                                        }
                                    }
                                }
                                instruction_type = Some(InstructionTypeBinding {
                                    type_path: path,
                                    output: inner_out,
                                });
                            } else {
                                instruction_type = Some(InstructionTypeBinding {
                                    type_path: path,
                                    output: None,
                                });
                            }
                        }
                        "handler" => {
                            let group = self.parse_handler_block()?;
                            handler_groups.push(group);
                        }
                        "subdispatch" => {
                            let sd = self.parse_dispatch_block()?;
                            subdispatches.push(sd);
                        }
                        other => {
                            let t = self.next()?;
                            return Err(Error::new(
                                ErrorKind::BindingsParse(format!(
                                    "unexpected '{}' inside dispatch block",
                                    other
                                )),
                                t.span,
                            ));
                        }
                    }
                }
                _ => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "unexpected token {:?} inside dispatch block",
                            t.tok
                        )),
                        t.span,
                    ));
                }
            }
        }

        Ok(DispatchBinding {
            decoder_name: name,
            span,
            output,
            context,
            handlers,
            strategy,
            invalid_handler,
            instruction_type,
            handler_groups,
            subdispatches,
            handler_consts,
        })
    }

    fn parse_handler_block(&mut self) -> Result<HandlerBinding, Error> {
        // Consume 'handler'
        self.next()?;
        let (handler_name, span) = self.expect_ident()?;

        self.expect_lbrace()?;
        let mut instructions: Vec<(String, Span)> = Vec::new();
        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Comma => {
                    self.next()?;
                }
                Tok::Ident(name) => {
                    let name = name.clone();
                    let t = self.next()?;
                    instructions.push((name, t.span));
                }
                other => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "unexpected token {:?} inside handler block",
                            other
                        )),
                        t.span,
                    ));
                }
            }
        }

        Ok(HandlerBinding {
            handler_name,
            instructions,
            span,
        })
    }

    fn parse_processor_block(&mut self) -> Result<IdaProcessorBinding, Error> {
        self.next()?; // 'processor'
        let (name, span) = self.expect_ident()?;
        self.expect_lbrace()?;

        let mut output: Option<String> = None;
        let mut p_name: Option<String> = None;
        let mut long_name: Option<String> = None;
        let mut id: Option<u64> = None;
        let mut address_size: Option<u32> = None;
        let mut bytes_per_unit: Option<u32> = None;
        let mut registers: Vec<String> = Vec::new();
        let mut segment_registers: Vec<(String, Span)> = Vec::new();
        let mut flow = IdaFlowBinding::default();

        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Eof => {
                    return Err(Error::new(
                        ErrorKind::BindingsParse(
                            "unexpected end of file inside processor block".to_string(),
                        ),
                        t.span.clone(),
                    ));
                }
                Tok::Ident(name) => {
                    let name = name.clone();
                    match name.as_str() {
                        "output" => {
                            self.next()?;
                            let (s, _) = self.expect_string()?;
                            output = Some(s);
                        }
                        "name" => {
                            self.next()?;
                            let (s, _) = self.expect_string()?;
                            p_name = Some(s);
                        }
                        "long_name" => {
                            self.next()?;
                            let (s, _) = self.expect_string()?;
                            long_name = Some(s);
                        }
                        "id" => {
                            self.next()?;
                            let (n, _) = self.expect_number()?;
                            id = Some(n);
                        }
                        "address_size" => {
                            self.next()?;
                            let (n, _) = self.expect_number()?;
                            address_size = Some(n as u32);
                        }
                        "bytes_per_unit" => {
                            self.next()?;
                            let (n, _) = self.expect_number()?;
                            bytes_per_unit = Some(n as u32);
                        }
                        "registers" => {
                            self.next()?;
                            registers = self.parse_ident_block()?;
                        }
                        "segment_registers" => {
                            self.next()?;
                            segment_registers = self.parse_ident_block_with_spans()?;
                        }
                        "flow" => {
                            self.next()?;
                            flow = self.parse_flow_block()?;
                        }
                        other => {
                            let t = self.next()?;
                            return Err(Error::new(
                                ErrorKind::BindingsParse(format!(
                                    "unexpected '{}' inside processor block",
                                    other
                                )),
                                t.span,
                            ));
                        }
                    }
                }
                _ => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "unexpected token {:?} inside processor block",
                            t.tok
                        )),
                        t.span,
                    ));
                }
            }
        }

        Ok(IdaProcessorBinding {
            decoder_name: name,
            span,
            output,
            name: p_name,
            long_name,
            id,
            address_size,
            bytes_per_unit,
            registers,
            segment_registers,
            flow,
        })
    }

    fn parse_architecture_block(&mut self) -> Result<BinjaArchitectureBinding, Error> {
        self.next()?; // 'architecture'
        let (name, span) = self.expect_ident()?;
        self.expect_lbrace()?;

        let mut output: Option<String> = None;
        let mut a_name: Option<String> = None;
        let mut address_size: Option<u32> = None;
        let mut default_int_size: Option<u32> = None;
        let mut endianness: Option<(String, Span)> = None;
        let mut registers: Vec<String> = Vec::new();

        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Eof => {
                    return Err(Error::new(
                        ErrorKind::BindingsParse(
                            "unexpected end of file inside architecture block".to_string(),
                        ),
                        t.span.clone(),
                    ));
                }
                Tok::Ident(name) => {
                    let name = name.clone();
                    match name.as_str() {
                        "output" => {
                            self.next()?;
                            let (s, _) = self.expect_string()?;
                            output = Some(s);
                        }
                        "name" => {
                            self.next()?;
                            let (s, _) = self.expect_string()?;
                            a_name = Some(s);
                        }
                        "address_size" => {
                            self.next()?;
                            let (n, _) = self.expect_number()?;
                            address_size = Some(n as u32);
                        }
                        "default_int_size" => {
                            self.next()?;
                            let (n, _) = self.expect_number()?;
                            default_int_size = Some(n as u32);
                        }
                        "endianness" => {
                            self.next()?;
                            let (s, span) = self.expect_ident()?;
                            endianness = Some((s, span));
                        }
                        "registers" => {
                            self.next()?;
                            registers = self.parse_ident_block()?;
                        }
                        other => {
                            let t = self.next()?;
                            return Err(Error::new(
                                ErrorKind::BindingsParse(format!(
                                    "unexpected '{}' inside architecture block",
                                    other
                                )),
                                t.span,
                            ));
                        }
                    }
                }
                _ => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "unexpected token {:?} inside architecture block",
                            t.tok
                        )),
                        t.span,
                    ));
                }
            }
        }

        Ok(BinjaArchitectureBinding {
            decoder_name: name,
            span,
            output,
            name: a_name,
            address_size,
            default_int_size,
            endianness,
            registers,
        })
    }

    fn parse_ident_block(&mut self) -> Result<Vec<String>, Error> {
        let pairs = self.parse_ident_block_with_spans()?;
        Ok(pairs.into_iter().map(|(s, _)| s).collect())
    }

    fn parse_string_block(&mut self) -> Result<Vec<String>, Error> {
        self.expect_lbrace()?;
        let mut out = Vec::new();
        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Comma => {
                    self.next()?;
                }
                Tok::Str(_) => {
                    let t = self.next()?;
                    if let Tok::Str(s) = t.tok {
                        out.push(s);
                    }
                }
                other => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "expected string literal, got {:?}",
                            other
                        )),
                        t.span,
                    ));
                }
            }
        }
        Ok(out)
    }

    fn parse_ident_block_with_spans(&mut self) -> Result<Vec<(String, Span)>, Error> {
        self.expect_lbrace()?;
        let mut out = Vec::new();
        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Comma => {
                    self.next()?;
                }
                Tok::Ident(_) => {
                    let t = self.next()?;
                    if let Tok::Ident(s) = t.tok {
                        out.push((s, t.span));
                    }
                }
                other => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!("expected identifier, got {:?}", other)),
                        t.span,
                    ));
                }
            }
        }
        Ok(out)
    }

    fn parse_flow_block(&mut self) -> Result<IdaFlowBinding, Error> {
        self.expect_lbrace()?;
        let mut flow = IdaFlowBinding::default();
        loop {
            let t = self.peek_clone()?;
            match &t.tok {
                Tok::RBrace => {
                    self.next()?;
                    break;
                }
                Tok::Ident(name) => {
                    let name = name.clone();
                    match name.as_str() {
                        "calls" => {
                            self.next()?;
                            flow.calls = self.parse_ident_block_with_spans()?;
                        }
                        "returns" => {
                            self.next()?;
                            flow.returns = self.parse_ident_block_with_spans()?;
                        }
                        "stops" => {
                            self.next()?;
                            flow.stops = self.parse_ident_block_with_spans()?;
                        }
                        other => {
                            let t = self.next()?;
                            return Err(Error::new(
                                ErrorKind::BindingsParse(format!("unknown flow key '{}'", other)),
                                t.span,
                            ));
                        }
                    }
                }
                _ => {
                    let t = self.next()?;
                    return Err(Error::new(
                        ErrorKind::BindingsParse(format!(
                            "unexpected token {:?} inside flow block",
                            t.tok
                        )),
                        t.span,
                    ));
                }
            }
        }
        Ok(flow)
    }
}

fn parse_strategy(s: &str, span: &Span) -> Result<Dispatch, Error> {
    match s {
        "fn_ptr_lut" => Ok(Dispatch::FnPtrLut),
        "jump_table" => Ok(Dispatch::JumpTable),
        "flat_lut" => Ok(Dispatch::FlatLut),
        "flat_match" => Ok(Dispatch::FlatMatch),
        other => Err(Error::new(
            ErrorKind::InvalidStrategy(other.to_string()),
            span.clone(),
        )),
    }
}
