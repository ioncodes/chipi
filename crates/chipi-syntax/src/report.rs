//! Compiler diagnostics: a stable error code, a message, a primary span, optional secondary
//! labels and a small self-contained renderer (no external crates).

use crate::source::{Source, Span};
use std::fmt::{self, Write as _};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Severity {
    Error,
    Warning,
}

impl Severity {
    fn word(self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
        }
    }
}

/// A secondary span + note, shown under the primary location.
#[derive(Clone)]
pub struct Label {
    pub span: Span,
    pub note: String,
}

/// A single diagnostic. `code` is a short stable tag (the diagnostic's name).
#[derive(Clone)]
pub struct Diag {
    pub severity: Severity,
    pub code: &'static str,
    pub message: String,
    pub span: Span,
    pub labels: Vec<Label>,
}

impl Diag {
    pub fn error(code: &'static str, message: impl Into<String>, span: Span) -> Self {
        Diag {
            severity: Severity::Error,
            code,
            message: message.into(),
            span,
            labels: Vec::new(),
        }
    }

    pub fn warning(code: &'static str, message: impl Into<String>, span: Span) -> Self {
        Diag {
            severity: Severity::Warning,
            code,
            message: message.into(),
            span,
            labels: Vec::new(),
        }
    }

    /// Attach a secondary labelled span (builder style).
    pub fn label(mut self, span: Span, note: impl Into<String>) -> Self {
        self.labels.push(Label {
            span,
            note: note.into(),
        });
        self
    }

    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }

    /// Render the diagnostic against its source into a human-readable block (trailing newline).
    pub fn render(&self, src: &Source) -> String {
        let mut out = String::new();
        let (line, col) = src.locate(self.span.start);
        let _ = writeln!(
            out,
            "{}[{}]: {}",
            self.severity.word(),
            self.code,
            self.message
        );
        let _ = writeln!(out, " --> {}:{}:{}", src.name, line, col);
        emit_snippet(&mut out, src, self.span, None);

        for lbl in &self.labels {
            let (ll, lc) = src.locate(lbl.span.start);
            let _ = writeln!(out, " ::: {}:{}:{}", src.name, ll, lc);
            emit_snippet(&mut out, src, lbl.span, Some(&lbl.note));
        }
        out
    }
}

impl fmt::Debug for Diag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}[{}]: {}",
            self.severity.word(),
            self.code,
            self.message
        )
    }
}

/// One source line plus a caret run under the primary span.
fn emit_snippet(out: &mut String, src: &Source, span: Span, note: Option<&str>) {
    let (line_no, _) = src.locate(span.start);
    let text = src.line_text(span.start);
    let origin = src.line_origin(span.start);
    let gutter = line_no.to_string();
    let pad = " ".repeat(gutter.len());

    let _ = writeln!(out, "{pad} |");
    let _ = writeln!(out, "{gutter} | {text}");

    let lead = (span.start.saturating_sub(origin)) as usize;
    let stop = ((span.end.saturating_sub(origin)) as usize).min(text.len());
    let from = lead.min(stop);
    let carets = text[from..stop].chars().count().max(1);

    let mut row = format!("{pad} | {}{}", " ".repeat(lead), "^".repeat(carets));
    if let Some(n) = note {
        row.push(' ');
        row.push_str(n);
    }

    let _ = writeln!(out, "{row}");
}
