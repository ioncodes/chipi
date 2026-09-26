//! Source buffers, byte spans and span-annotated values.

use std::fmt;

/// A half-open byte range `[start, end)` into a single source buffer.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub const fn new(start: u32, end: u32) -> Self {
        Span { start, end }
    }

    /// An empty span anchored at one byte offset (for "expected ... here" diagnostics).
    pub const fn at(pos: u32) -> Self {
        Span {
            start: pos,
            end: pos,
        }
    }

    /// The tightest span enclosing both `self` and `other`.
    pub fn to(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

/// A value tagged with the source span it was parsed from.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub const fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.fmt(f)
    }
}

/// A named source buffer with precomputed line offsets for fast position lookup.
#[derive(Clone)]
pub struct Source {
    pub name: String,
    pub text: String,
    /// Byte offset of the first character of each line; `line_at[0] == 0`.
    line_at: Vec<u32>,
}

impl Source {
    pub fn new(name: impl Into<String>, text: impl Into<String>) -> Self {
        let text = text.into();

        let mut line_at = vec![0u32];
        for (i, b) in text.bytes().enumerate() {
            if b == b'\n' {
                line_at.push((i + 1) as u32);
            }
        }

        Source {
            name: name.into(),
            text,
            line_at,
        }
    }

    /// 1-based `(line, column)` for a byte offset; columns count `char`s.
    pub fn locate(&self, offset: u32) -> (usize, usize) {
        let offset = offset.min(self.text.len() as u32);
        let line = match self.line_at.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => i - 1,
        };

        let start = self.line_at[line] as usize;
        let col = self.text[start..offset as usize].chars().count() + 1;
        (line + 1, col)
    }

    /// The full text of the line containing `offset`, newline excluded.
    pub fn line_text(&self, offset: u32) -> &str {
        let (line, _) = self.locate(offset);

        let start = self.line_at[line - 1] as usize;
        let end = self
            .line_at
            .get(line)
            .map(|&s| (s as usize).saturating_sub(1))
            .unwrap_or(self.text.len());
        &self.text[start..end.min(self.text.len())]
    }

    /// Byte offset of the first character of the line containing `offset`.
    pub fn line_origin(&self, offset: u32) -> u32 {
        let (line, _) = self.locate(offset);
        self.line_at[line - 1]
    }
}
