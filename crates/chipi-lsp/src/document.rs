//! Source positions and compiler-backed document analysis.

use crate::symbols::{self, Symbol};
use chipi_syntax::{Diag, Severity, Span};
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString,
    Position, Range, Uri,
};

pub struct Document {
    pub text: String,
    pub version: i32,
    pub symbols: Vec<Symbol>,
    pub diagnostics: Vec<Diagnostic>,
}

impl Document {
    pub fn new(text: String, version: i32, uri: &Uri) -> Self {
        let symbols = chipi_syntax::parse(&text)
            .map(|spec| symbols::collect(&spec))
            .unwrap_or_default();
        let diags = match chipi_core::compile(&text) {
            Ok(isa) => isa.warnings,
            Err(diags) => diags,
        };
        let diagnostics = diags.iter().map(|d| diagnostic(&text, uri, d)).collect();
        Self {
            text,
            version,
            symbols,
            diagnostics,
        }
    }
}

pub fn diagnostic(text: &str, uri: &Uri, diag: &Diag) -> Diagnostic {
    Diagnostic {
        range: range(text, diag.span),
        severity: Some(match diag.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        code: Some(NumberOrString::String(diag.code.into())),
        source: Some("chipi".into()),
        message: diag.message.clone(),
        related_information: if diag.labels.is_empty() {
            None
        } else {
            Some(
                diag.labels
                    .iter()
                    .map(|label| DiagnosticRelatedInformation {
                        location: Location {
                            uri: uri.clone(),
                            range: range(text, label.span),
                        },
                        message: label.note.clone(),
                    })
                    .collect(),
            )
        },
        ..Diagnostic::default()
    }
}

/// LSP 3.17 defaults to UTF-16 positions, even though compiler spans are UTF-8 bytes.
pub fn position(text: &str, offset: usize) -> Position {
    let mut offset = offset.min(text.len());
    while !text.is_char_boundary(offset) {
        offset -= 1;
    }
    let before = &text[..offset];
    let line = before.bytes().filter(|&b| b == b'\n').count();
    let start = before.rfind('\n').map_or(0, |i| i + 1);
    // CRLF is one line ending, not part of the line's character count.
    let column = text[start..offset]
        .trim_end_matches('\r')
        .encode_utf16()
        .count();
    Position::new(line as u32, column as u32)
}

pub fn offset(text: &str, pos: Position) -> Option<usize> {
    let mut start = 0;
    for _ in 0..pos.line {
        start += text[start..].find('\n')? + 1;
    }
    let line = text[start..].split('\n').next()?.trim_end_matches('\r');
    let mut units = 0;
    for (i, ch) in line.char_indices() {
        if units == pos.character {
            return Some(start + i);
        }
        units += ch.len_utf16() as u32;
        if units > pos.character {
            return None;
        }
    }
    (units == pos.character).then_some(start + line.len())
}

pub fn range(text: &str, span: Span) -> Range {
    Range::new(
        position(text, span.start as usize),
        position(text, span.end as usize),
    )
}

pub fn word_at(text: &str, offset: usize) -> Option<(&str, Span)> {
    if offset > text.len() || !text.is_char_boundary(offset) {
        return None;
    }
    let is_word = |b: u8| b.is_ascii_alphanumeric() || b == b'_' || b == b'.';
    let bytes = text.as_bytes();
    let mut start = offset;
    let mut end = offset;
    while start > 0 && is_word(bytes[start - 1]) {
        start -= 1;
    }
    while end < bytes.len() && is_word(bytes[end]) {
        end += 1;
    }
    (start < end).then(|| (&text[start..end], Span::new(start as u32, end as u32)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn positions_handle_utf16_crlf_and_eof() {
        let text = "# \u{1f600} cafe\u{e9}\r\nop";
        assert_eq!(position(text, 7), Position::new(0, 5));
        assert_eq!(offset(text, Position::new(0, 3)), None);
        assert_eq!(offset(text, Position::new(1, 2)), Some(text.len()));
        assert_eq!(offset(text, Position::new(2, 0)), None);
        for i in 0..=text.len() {
            if text.is_char_boundary(i) && !text[..i].ends_with('\r') {
                assert_eq!(offset(text, position(text, i)), Some(i));
            }
        }
        assert_eq!(offset("x\n", Position::new(1, 0)), Some(2));
    }
}
