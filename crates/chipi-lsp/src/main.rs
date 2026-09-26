//! Stdio Language Server Protocol implementation for chipi.

#![forbid(unsafe_code)]

mod document;
mod format;
mod symbols;

use chipi_syntax::Span;
use document::{offset, position, range, word_at, Document};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::*;
use serde::de::DeserializeOwned;
use serde_json::{json, Value};
use std::{
    collections::{BTreeMap, HashMap},
    error::Error,
};

type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;

fn main() -> std::process::ExitCode {
    let args: Vec<_> = std::env::args().skip(1).collect();
    match args.as_slice() {
        [] => {}
        [arg] if arg == "--stdio" => {}
        [arg] if arg == "--version" || arg == "-V" => {
            println!("chipi-lsp {}", env!("CARGO_PKG_VERSION"));
            return std::process::ExitCode::SUCCESS;
        }
        [arg] if arg == "--help" || arg == "-h" => {
            println!("chipi-lsp: language server for .chipi files\n\nUsage: chipi-lsp [--stdio | --version | --help]");
            return std::process::ExitCode::SUCCESS;
        }
        _ => {
            eprintln!("usage: chipi-lsp [--stdio | --version | --help]");
            return std::process::ExitCode::FAILURE;
        }
    }
    match run() {
        Ok(()) => std::process::ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("chipi-lsp: {err}");
            std::process::ExitCode::FAILURE
        }
    }
}

fn run() -> Result<()> {
    let (connection, threads) = Connection::stdio();
    let (id, params) = connection.initialize_start()?;
    let hierarchical_symbols = params
        .pointer("/capabilities/textDocument/documentSymbol/hierarchicalDocumentSymbolSupport")
        .and_then(Value::as_bool)
        .unwrap_or(false);
    connection.initialize_finish(
        id,
        json!({
            "serverInfo": { "name": "chipi-lsp", "version": env!("CARGO_PKG_VERSION") },
            "capabilities": {
                "positionEncoding": "utf-16",
                "textDocumentSync": { "openClose": true, "change": 2 },
                "completionProvider": { "triggerCharacters": [":", "."] },
                "hoverProvider": true,
                "definitionProvider": true,
                "referencesProvider": true,
                "documentSymbolProvider": true,
                "documentFormattingProvider": true,
                "foldingRangeProvider": true
            }
        }),
    )?;
    let mut documents = HashMap::<String, Document>::new();
    let mut shutdown = false;
    let mut exited = false;
    for message in &connection.receiver {
        match message {
            Message::Request(req) => {
                let response = if shutdown {
                    Response::new_err(
                        req.id,
                        ErrorCode::InvalidRequest as i32,
                        "server has shut down".into(),
                    )
                } else if req.method == "shutdown" {
                    shutdown = true;
                    Response::new_ok(req.id, Value::Null)
                } else {
                    respond(req, &documents, hierarchical_symbols)
                };
                connection.sender.send(response.into())?;
            }
            Message::Notification(note) if note.method == "exit" => {
                exited = true;
                break;
            }
            Message::Notification(note) if !shutdown => {
                if let Err(err) = notify(note, &mut documents, &connection) {
                    eprintln!("chipi-lsp: {err}");
                }
            }
            _ => {}
        }
    }
    drop(connection);
    threads.join()?;
    if !shutdown || !exited {
        return Err("client disconnected without shutdown and exit".into());
    }
    Ok(())
}

fn params<T: DeserializeOwned>(value: Value) -> std::result::Result<T, String> {
    serde_json::from_value(value).map_err(|err| err.to_string())
}

fn publish(
    connection: &Connection,
    uri: Uri,
    version: Option<i32>,
    diagnostics: Vec<Diagnostic>,
) -> Result<()> {
    connection.sender.send(
        Notification::new(
            "textDocument/publishDiagnostics".into(),
            PublishDiagnosticsParams {
                uri,
                version,
                diagnostics,
            },
        )
        .into(),
    )?;
    Ok(())
}

fn notify(
    note: Notification,
    documents: &mut HashMap<String, Document>,
    connection: &Connection,
) -> Result<()> {
    match note.method.as_str() {
        "textDocument/didOpen" => {
            let p: DidOpenTextDocumentParams = params(note.params)?;
            let item = p.text_document;
            let doc = Document::new(item.text, item.version, &item.uri);
            publish(
                connection,
                item.uri.clone(),
                Some(doc.version),
                doc.diagnostics.clone(),
            )?;
            documents.insert(item.uri.as_str().to_owned(), doc);
        }
        "textDocument/didChange" => {
            let p: DidChangeTextDocumentParams = params(note.params)?;
            let uri = p.text_document.uri;
            let Some(old) = documents.get(uri.as_str()) else {
                return Ok(());
            };
            if p.text_document.version <= old.version {
                return Ok(());
            }
            let mut text = old.text.clone();
            for change in p.content_changes {
                if let Some(r) = change.range {
                    let start = offset(&text, r.start).ok_or("invalid change start")?;
                    let end = offset(&text, r.end).ok_or("invalid change end")?;
                    if end < start {
                        return Err("reversed change range".into());
                    }
                    text.replace_range(start..end, &change.text);
                } else {
                    text = change.text;
                }
            }
            let doc = Document::new(text, p.text_document.version, &uri);
            publish(
                connection,
                uri.clone(),
                Some(doc.version),
                doc.diagnostics.clone(),
            )?;
            documents.insert(uri.as_str().to_owned(), doc);
        }
        "textDocument/didClose" => {
            let p: DidCloseTextDocumentParams = params(note.params)?;
            documents.remove(p.text_document.uri.as_str());
            publish(connection, p.text_document.uri, None, Vec::new())?;
        }
        // Unknown notifications and cancellation for already-completed synchronous requests
        // have no response under JSON-RPC.
        _ => {}
    }
    Ok(())
}

fn respond(req: Request, docs: &HashMap<String, Document>, hierarchical_symbols: bool) -> Response {
    let known = matches!(
        req.method.as_str(),
        "textDocument/completion"
            | "textDocument/hover"
            | "textDocument/definition"
            | "textDocument/references"
            | "textDocument/documentSymbol"
            | "textDocument/formatting"
            | "textDocument/foldingRange"
    );
    if !known {
        return Response::new_err(
            req.id,
            ErrorCode::MethodNotFound as i32,
            format!("unsupported method: {}", req.method),
        );
    }
    match request(&req.method, req.params, docs, hierarchical_symbols) {
        Ok(value) => Response::new_ok(req.id, value),
        Err(err) => Response::new_err(req.id, ErrorCode::InvalidParams as i32, err),
    }
}

fn request(
    method: &str,
    value: Value,
    docs: &HashMap<String, Document>,
    hierarchical_symbols: bool,
) -> std::result::Result<Value, String> {
    #[derive(serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct DocumentParams {
        text_document: TextDocumentIdentifier,
    }
    let p: DocumentParams = params(value.clone())?;
    let uri = p.text_document.uri;
    let Some(doc) = docs.get(uri.as_str()) else {
        return Ok(Value::Null);
    };
    match method {
        "textDocument/documentSymbol" => {
            let symbols: Vec<_> = doc.symbols.iter().filter(|s| s.scope.is_none()).map(|s| {
                if hierarchical_symbols {
                    json!({ "name": s.name, "kind": s.kind, "detail": s.detail,
                        "range": range(&doc.text, s.span), "selectionRange": range(&doc.text, s.selection) })
                } else {
                    json!({ "name": s.name, "kind": s.kind,
                        "location": { "uri": uri, "range": range(&doc.text, s.span) } })
                }
            }).collect();
            Ok(json!(symbols))
        }
        "textDocument/formatting" => {
            let p: DocumentFormattingParams = params(value)?;
            let result = format::format(&doc.text, &p.options);
            Ok(json!(result
                .filter(|text| text != &doc.text)
                .map(|new_text| vec![TextEdit {
                    range: Range::new(Position::new(0, 0), position(&doc.text, doc.text.len())),
                    new_text
                }])
                .unwrap_or_default()))
        }
        "textDocument/foldingRange" => {
            let mut ranges: Vec<_> = doc
                .symbols
                .iter()
                .filter(|s| s.scope.is_none())
                .filter_map(|s| {
                    let r = range(&doc.text, s.span);
                    (r.end.line > r.start.line)
                        .then(|| json!({"startLine": r.start.line, "endLine": r.end.line}))
                })
                .collect();
            ranges.dedup();
            Ok(json!(ranges))
        }
        _ => {
            let p: TextDocumentPositionParams = params(value.clone())?;
            let at = offset(&doc.text, p.position).ok_or("position is outside the document")?;
            if method == "textDocument/completion" {
                return Ok(completions(doc, at));
            }
            let Some((word, span)) = word_at(&doc.text, at) else {
                return Ok(Value::Null);
            };
            let Some(symbol) = symbols::resolve(&doc.symbols, word, at) else {
                return Ok(if method == "textDocument/hover" {
                    builtin_detail(word).map(|detail| json!({"contents": {"kind": "plaintext", "value": detail}, "range": range(&doc.text, span)})).unwrap_or(Value::Null)
                } else {
                    Value::Null
                });
            };
            match method {
                "textDocument/definition" => Ok(json!(Location {
                    uri,
                    range: range(&doc.text, symbol.selection)
                })),
                "textDocument/references" => {
                    let p: ReferenceParams = params(value)?;
                    Ok(json!(references(
                        doc,
                        symbol,
                        &uri,
                        p.context.include_declaration
                    )))
                }
                _ => {
                    let snippet = doc
                        .text
                        .get(symbol.span.start as usize..symbol.span.end as usize)
                        .unwrap_or(&symbol.name);
                    Ok(
                        json!({ "contents": { "kind": "plaintext", "value": format!("{} {}\n\n{}", symbol.detail, symbol.name, snippet) }, "range": range(&doc.text, span) }),
                    )
                }
            }
        }
    }
}

const KEYWORDS: &[&str] = &[
    "decoder",
    "width",
    "bit_order",
    "endian",
    "lsb0",
    "msb0",
    "little",
    "big",
    "selector",
    "operand",
    "type",
    "form",
    "uses",
    "fn",
    "let",
    "return",
    "when",
    "length",
    "else",
    "mode",
    "bool",
    "context",
    "prefix",
    "done",
    "finish",
    "dispatch",
    "subdecoder",
    "outputs",
    "for",
    "in",
    "fetch",
    "assemble",
    "display",
    "names",
    "hex",
    "dec",
    "signed_hex",
    "word",
];

fn builtin_detail(name: &str) -> Option<String> {
    chipi_core::compute::builtin_of(name).map(|b| {
        let args = if b.min_args == b.max_args {
            b.min_args.to_string()
        } else {
            format!("{} or more", b.min_args)
        };
        format!("{}: builtin function ({args} arguments)", b.name)
    })
}

fn completions(doc: &Document, at: usize) -> Value {
    let mut items = BTreeMap::new();
    for &keyword in KEYWORDS {
        items.insert(
            keyword.to_string(),
            CompletionItem {
                label: keyword.into(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..CompletionItem::default()
            },
        );
    }
    for b in chipi_core::compute::BUILTINS {
        items.insert(
            b.name.to_string(),
            CompletionItem {
                label: b.name.into(),
                detail: builtin_detail(b.name),
                kind: Some(CompletionItemKind::FUNCTION),
                ..CompletionItem::default()
            },
        );
    }
    for width in 1..=64 {
        for prefix in ["u", "i"] {
            let name = format!("{prefix}{width}");
            items.insert(
                name.clone(),
                CompletionItem {
                    label: name,
                    kind: Some(CompletionItemKind::TYPE_PARAMETER),
                    ..CompletionItem::default()
                },
            );
        }
    }
    // While a declaration is being typed, parse the rest of the buffer without that line.
    // Keep offsets intact so definitions never point into an older document version.
    let recovered;
    let symbols = if doc.symbols.is_empty() {
        let start = doc.text[..at].rfind('\n').map_or(0, |i| i + 1);
        let end = doc.text[at..].find('\n').map_or(doc.text.len(), |i| at + i);
        let mut text = doc.text.clone();
        text.replace_range(start..end, &" ".repeat(end - start));
        recovered = chipi_syntax::parse(&text)
            .or_else(|_| chipi_syntax::parse(&text[..start]))
            .map(|s| symbols::collect(&s))
            .unwrap_or_default();
        &recovered
    } else {
        &doc.symbols
    };
    // Globals first; local bindings take precedence for the same label.
    for local in [false, true] {
        for s in symbols
            .iter()
            .filter(|s| s.scope.is_some() == local && s.visible_at(at))
        {
            items.insert(
                s.name.clone(),
                CompletionItem {
                    label: s.name.clone(),
                    detail: Some(s.detail.clone()),
                    kind: Some(s.completion_kind()),
                    ..CompletionItem::default()
                },
            );
        }
    }
    let replacement = word_at(&doc.text, at)
        .map(|(_, span)| range(&doc.text, span))
        .unwrap_or_else(|| Range::new(position(&doc.text, at), position(&doc.text, at)));
    for item in items.values_mut() {
        item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
            range: replacement,
            new_text: item.label.clone(),
        }));
    }
    json!({ "isIncomplete": false, "items": items.into_values().collect::<Vec<_>>() })
}

fn references(
    doc: &Document,
    target: &symbols::Symbol,
    uri: &Uri,
    include_declaration: bool,
) -> Vec<Location> {
    let mut found = Vec::new();
    // Strings and comments are deliberately excluded: literal assembly text is not a
    // symbol use. Definitions and code references follow the same scope resolution.
    if let Ok(tokens) = chipi_syntax::lex::lex(&doc.text) {
        for token in tokens {
            if !matches!(token.kind, chipi_syntax::lex::TokenKind::Ident(_)) {
                continue;
            }
            let at = token.span.start as usize;
            let Some((word, span)) = word_at(&doc.text, at) else {
                continue;
            };
            if !include_declaration && span == target.selection {
                continue;
            }
            if let Some(s) = symbols::resolve(&doc.symbols, word, at) {
                if s.selection == target.selection && s.name == target.name {
                    let location = Location {
                        uri: uri.clone(),
                        range: range(&doc.text, Span::new(span.start, span.end)),
                    };
                    if !found.contains(&location) {
                        found.push(location);
                    }
                }
            }
        }
    }
    found
}
