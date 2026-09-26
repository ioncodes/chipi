//! Conservative formatting: normalize spacing and indentation without rewriting tokens,
//! comments, string contents, or the author's line breaks.

use chipi_syntax::lex::{lex, Token, TokenKind as K};
use lsp_types::FormattingOptions;

pub fn format(text: &str, options: &FormattingOptions) -> Option<String> {
    let tokens = lex(text).ok()?;
    let newline = if text.contains("\r\n") { "\r\n" } else { "\n" };
    let indent = if options.insert_spaces {
        " ".repeat(options.tab_size.clamp(1, 16) as usize)
    } else {
        "\t".into()
    };
    let mut interpolation = vec![false; tokens.len()];
    for i in 1..tokens.len().saturating_sub(2) {
        if tokens[i].kind == K::LBrace
            && matches!(tokens[i - 1].kind, K::Ident(_))
            && matches!(tokens[i + 1].kind, K::Ident(_))
            && tokens[i + 2].kind == K::RBrace
            && tokens[i - 1].span.end == tokens[i].span.start
        {
            interpolation[i] = true;
            interpolation[i + 2] = true;
        }
    }
    let instr_spans: Vec<_> = chipi_syntax::parse(text)
        .ok()
        .map(|spec| {
            spec.items
                .into_iter()
                .filter_map(|item| match item {
                    chipi_syntax::ast::Item::Instr(i) => Some(i.span),
                    _ => None,
                })
                .collect()
        })
        .unwrap_or_default();
    let mut out = String::new();
    let mut origin = 0;
    let mut depth = 0usize;
    let mut cursor = 0;
    for raw in text.split_inclusive('\n') {
        let line = raw.trim_end_matches(['\r', '\n']);
        let end = origin + line.len();
        let first = cursor;
        while cursor < tokens.len() && (tokens[cursor].span.start as usize) < end {
            if tokens[cursor].kind == K::Eof {
                break;
            }
            cursor += 1;
        }
        let line_tokens = &tokens[first..cursor];
        // Do not touch any physical line crossed by a multiline string literal.
        let crosses_string = tokens
            .get(first.wrapping_sub(1))
            .is_some_and(|t| t.span.end as usize > origin)
            || line_tokens.iter().any(|t| t.span.end as usize > end);
        if crosses_string {
            out.push_str(line);
        } else if !line.trim().is_empty() {
            let closing = line_tokens.first().map_or(0, |t| {
                usize::from(t.kind == K::RBrace && !interpolation[first])
            });
            let continuation = instr_spans
                .iter()
                .any(|s| (s.start as usize) < origin && origin < s.end as usize);
            out.push_str(&indent.repeat(depth.saturating_sub(closing) + usize::from(continuation)));
            if line_tokens.is_empty() {
                out.push_str(line.trim());
            } else {
                for (j, token) in line_tokens.iter().enumerate() {
                    if j > 0
                        && space_between(
                            &line_tokens[j - 1],
                            token,
                            interpolation[first + j - 1],
                            interpolation[first + j],
                        )
                    {
                        out.push(' ');
                    }
                    out.push_str(&text[token.span.start as usize..token.span.end as usize]);
                }
                let tail = &text[line_tokens.last()?.span.end as usize..end];
                if !tail.trim().is_empty() {
                    out.push_str("  ");
                    out.push_str(tail.trim());
                }
            }
        }
        for (j, token) in line_tokens.iter().enumerate() {
            if !interpolation[first + j] {
                match token.kind {
                    K::LBrace => depth += 1,
                    K::RBrace => depth = depth.saturating_sub(1),
                    _ => {}
                }
            }
        }
        out.push_str(newline);
        origin += raw.len();
    }
    if options.trim_final_newlines == Some(true) {
        let len = out.trim_end_matches(['\r', '\n']).len();
        out.truncate(len);
        if !out.is_empty() {
            out.push_str(newline);
        }
    }
    if options.insert_final_newline == Some(false) && !text.ends_with('\n') {
        out.truncate(out.len().saturating_sub(newline.len()));
    }
    // A formatter must never merge tokens (e.g. `- >` into `->`) or change literals.
    let formatted = lex(&out).ok()?;
    if tokens
        .iter()
        .map(|t| &t.kind)
        .eq(formatted.iter().map(|t| &t.kind))
    {
        Some(out)
    } else {
        None
    }
}

fn space_between(
    prev: &Token,
    next: &Token,
    prev_interpolation: bool,
    next_interpolation: bool,
) -> bool {
    if (next_interpolation && next.kind == K::LBrace)
        || (prev_interpolation && prev.kind == K::LBrace)
        || (next_interpolation && next.kind == K::RBrace)
    {
        return false;
    }
    if matches!(
        next.kind,
        K::Comma | K::Colon | K::Dot | K::DotDot | K::RParen | K::RBracket
    ) || matches!(
        prev.kind,
        K::Colon | K::Dot | K::DotDot | K::LParen | K::LBracket | K::Tilde
    ) {
        return false;
    }
    if next.kind == K::LParen && matches!(prev.kind, K::Ident(_)) {
        return false;
    }
    if next.kind == K::LBracket && matches!(prev.kind, K::Ident(_) | K::RParen | K::RBracket) {
        return false;
    }
    if prev.kind == K::Dot && next.kind == K::Star {
        return false;
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preserves_comments_strings_and_interpolation() {
        let src = "decoder X{\nwidth=8   # keep me\n}\nfor n in 0..2{\n op{n} [7:0]=n | \"# {n}: \\\"x\\\"\"\n}\n";
        let options = FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            ..FormattingOptions::default()
        };
        let result = format(src, &options).unwrap();
        assert!(result.contains("    width = 8  # keep me"), "{result}");
        assert!(result.contains("    op{n} [7:0] = n"), "{result}");
        assert_eq!(format(&result, &options).unwrap(), result);
        chipi_syntax::parse(&result).unwrap();
    }

    #[test]
    fn formats_all_examples_and_corpus_without_changing_tokens() {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        for dir in ["examples", "corpus"] {
            for entry in std::fs::read_dir(root.join(dir)).unwrap() {
                let path = entry.unwrap().path();
                if path.extension().and_then(|s| s.to_str()) != Some("chipi") {
                    continue;
                }
                let text = std::fs::read_to_string(&path).unwrap();
                if chipi_syntax::parse(&text).is_err() {
                    continue;
                }
                for spaces in [false, true] {
                    let options = FormattingOptions {
                        tab_size: 4,
                        insert_spaces: spaces,
                        ..FormattingOptions::default()
                    };
                    let output = format(&text, &options).expect("token-preserving format");
                    chipi_syntax::parse(&output).unwrap();
                    assert_eq!(
                        format(&output, &options).unwrap(),
                        output,
                        "{}",
                        path.display()
                    );
                }
            }
        }
    }
}
