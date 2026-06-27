//! `chipi-macros`: the `isa!` procedural macro, which generates a decoder module from a `.chipi`
//! spec at compile time, with no build script required.
//!
//! ```ignore
//! chipi_macros::isa!("examples/mips.chipi");
//! // expands to `pub mod mips { /* decode, classify, Ops, Display, ... */ }`
//! ```
//!
//! The path is resolved relative to the invoking crate's `CARGO_MANIFEST_DIR`. Spec errors surface
//! as a `compile_error!` carrying the rendered chipi diagnostics.

use proc_macro::{TokenStream, TokenTree};
use std::path::Path;

/// Generate a decoder module from a `.chipi` spec file (path relative to the crate root).
///
/// ```ignore
/// chipi_macros::isa!("isa/cpu.chipi");                 // newtype dispatch (default)
/// chipi_macros::isa!("isa/cpu.chipi", style = enum);   // nested-enum dispatch
/// ```
#[proc_macro]
pub fn isa(input: TokenStream) -> TokenStream {
    let mut trees = input.into_iter();

    let path = match trees.next() {
        Some(TokenTree::Literal(lit)) => lit.to_string().trim_matches('"').to_string(),
        _ => return fail("isa! expects a string-literal path to a .chipi spec"),
    };

    if path.is_empty() {
        return fail("isa! expects a string-literal path to a .chipi spec");
    }

    // Optional trailing options: `, style = enum` (or `, enum`). Commas and `=` are noise. The only
    // idents that matter are `style`, `enum` and `newtype`.
    let mut dispatch = chipi_backend_rust::Dispatch::Newtype;
    for t in trees {
        match t {
            TokenTree::Punct(_) => {}
            TokenTree::Ident(id) => match id.to_string().as_str() {
                "style" => {}
                "enum" => dispatch = chipi_backend_rust::Dispatch::Enum,
                "newtype" => dispatch = chipi_backend_rust::Dispatch::Newtype,
                other => {
                    return fail(&format!(
                        "isa!: unknown option `{other}` (expected `style = enum|newtype`)"
                    ))
                }
            },
            other => return fail(&format!("isa!: unexpected token `{other}` in options")),
        }
    }

    let base = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_default();
    let full = Path::new(&base).join(&path);

    let src = match std::fs::read_to_string(&full) {
        Ok(s) => s,
        Err(e) => return fail(&format!("isa!: cannot read {}: {e}", full.display())),
    };

    let isa = match chipi_core::compile(&src) {
        Ok(p) => p,
        Err(errs) => {
            let source = chipi_core::Source::new(path, src.clone());
            let rendered = chipi_core::render_diagnostics(&errs, &source);

            return fail(&format!("isa!: spec failed to compile:\n{rendered}"));
        }
    };

    let code =
        chipi_backend_rust::emit_decoder_with(&isa, chipi_backend_rust::EmitOptions { dispatch });
    // Drop the generated file's inner attributes. We add equivalent ones on the wrapping
    // module so they cover every item.
    let body: String = code
        .lines()
        .filter(|l| !l.trim_start().starts_with("#!["))
        .collect::<Vec<_>>()
        .join("\n");

    let module = chipi_backend_rust::sanitize(&isa.decoder.name);

    // Register the spec as a build dependency. `read_to_string` does not tell rustc to watch the
    // file, so without this an edited `.chipi` would leave a stale decoder in place. Emitting an
    // `include_str!` of the absolute path is the standard idiom until `tracked_path` stabilises.
    let track = format!(
        "const _: &str = include_str!({:?});",
        full.to_string_lossy()
    );
    let wrapped = format!(
        "#[allow(non_snake_case)]\npub mod {module} {{\n#![allow(dead_code, unexpected_cfgs, clippy::all, clippy::pedantic)]\n{track}\n{body}\n}}"
    );

    match wrapped.parse() {
        Ok(ts) => ts,
        Err(e) => fail(&format!("isa!: generated code did not parse: {e}")),
    }
}

fn fail(msg: &str) -> TokenStream {
    format!("compile_error!({msg:?});").parse().unwrap()
}
