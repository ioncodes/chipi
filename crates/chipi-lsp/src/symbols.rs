//! A source-based symbol index. Local operands and function bindings stay in their scope.

use chipi_syntax::{
    ast::{Ident, Item, Spec},
    Span,
};
use lsp_types::{CompletionItemKind, SymbolKind};

pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub detail: String,
    pub span: Span,
    pub selection: Span,
    pub scope: Option<Span>,
}

impl Symbol {
    pub fn visible_at(&self, offset: usize) -> bool {
        self.scope.map_or(true, |scope| {
            scope.start as usize <= offset && offset <= scope.end as usize
        })
    }

    pub fn completion_kind(&self) -> CompletionItemKind {
        match self.kind {
            SymbolKind::FUNCTION => CompletionItemKind::FUNCTION,
            SymbolKind::STRUCT => CompletionItemKind::STRUCT,
            SymbolKind::CLASS => CompletionItemKind::CLASS,
            SymbolKind::FIELD => CompletionItemKind::FIELD,
            SymbolKind::CONSTANT => CompletionItemKind::CONSTANT,
            _ => CompletionItemKind::VARIABLE,
        }
    }
}

pub fn collect(spec: &Spec) -> Vec<Symbol> {
    let mut out = Vec::new();
    let mut add = |name: &Ident, span, kind, detail: &str, scope| {
        out.push(Symbol {
            name: name.text.clone(),
            kind,
            detail: detail.into(),
            span,
            selection: name.span,
            scope,
        });
    };
    for item in &spec.items {
        match item {
            Item::Decoder(d) => {
                add(&d.name, d.span, SymbolKind::MODULE, "decoder", None);
                for m in &d.modes {
                    add(&m.name, m.span, SymbolKind::VARIABLE, "host mode", None);
                }
                for c in &d.context {
                    add(
                        &c.name,
                        c.span,
                        SymbolKind::VARIABLE,
                        "prefix context",
                        None,
                    );
                }
            }
            Item::Selector(s) => add(&s.name, s.span, SymbolKind::CONSTANT, "selector", None),
            Item::Value(v) => add(
                &v.name,
                v.span,
                SymbolKind::CLASS,
                &format!("{:?}: {}", v.kind, v.base.text),
                None,
            ),
            Item::Form(f) => {
                add(&f.name, f.span, SymbolKind::STRUCT, "form", None);
                for field in &f.fields {
                    add(
                        &field.name,
                        field.span,
                        SymbolKind::FIELD,
                        &field.ty.text,
                        Some(f.span),
                    );
                }
            }
            Item::Func(f) => {
                add(
                    &f.name,
                    f.span,
                    SymbolKind::FUNCTION,
                    &format!("fn -> {}", f.ret.text),
                    None,
                );
                for (name, ty) in &f.params {
                    add(
                        name,
                        name.span,
                        SymbolKind::VARIABLE,
                        &ty.text,
                        Some(f.span),
                    );
                }
                for (name, _) in &f.lets {
                    add(
                        name,
                        name.span,
                        SymbolKind::VARIABLE,
                        "let",
                        Some(Span::new(name.span.start, f.span.end)),
                    );
                }
            }
            Item::Prefix(p) => add(&p.name, p.span, SymbolKind::NAMESPACE, "prefix", None),
            Item::Group(g) => add(
                &g.tag,
                g.span,
                SymbolKind::NAMESPACE,
                if g.dispatch { "dispatch group" } else { "tag" },
                None,
            ),
            Item::SubDecoder(s) => {
                add(&s.name, s.span, SymbolKind::CLASS, "subdecoder", None);
                for arm in &s.arms {
                    add(
                        &arm.name,
                        arm.span,
                        SymbolKind::ENUM_MEMBER,
                        "subdecoder arm",
                        Some(s.span),
                    );
                    for b in &arm.bindings {
                        add(
                            &b.name,
                            b.span,
                            SymbolKind::FIELD,
                            &b.ty.name.text,
                            Some(arm.span),
                        );
                    }
                }
            }
            Item::Instr(i) => {
                add(
                    &i.name,
                    i.span,
                    SymbolKind::ENUM_MEMBER,
                    "instruction",
                    None,
                );
                for b in &i.bindings {
                    add(
                        &b.name,
                        b.span,
                        SymbolKind::FIELD,
                        &b.ty.name.text,
                        Some(i.span),
                    );
                }
                for c in &i.computed {
                    add(&c.name, c.span, SymbolKind::FIELD, &c.ty.text, Some(i.span));
                }
                if let Some(used) = &i.uses {
                    for item in &spec.items {
                        if let Item::Form(f) = item {
                            if f.name.text == used.text {
                                for field in &f.fields {
                                    add(
                                        &field.name,
                                        field.span,
                                        SymbolKind::FIELD,
                                        &field.ty.text,
                                        Some(i.span),
                                    );
                                }
                            }
                        }
                    }
                }
            }
            Item::Length(_) => {}
        }
    }
    out
}

pub fn resolve<'a>(symbols: &'a [Symbol], word: &str, offset: usize) -> Option<&'a Symbol> {
    symbols
        .iter()
        .filter(|s| s.name == word && s.visible_at(offset))
        .min_by_key(|s| s.scope.map_or(u32::MAX, |scope| scope.end - scope.start))
}
