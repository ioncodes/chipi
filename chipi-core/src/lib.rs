//! # chipi-core
//!
//! Core library for the chipi instruction decoder generator.
//!
//! ## Crate structure
//!
//! - [`parser`]: parses `.chipi` instruction-spec files into a raw AST.
//!   See [`types::DecoderDef`].
//! - [`validate`]: validates and lowers the AST into a language-agnostic IR.
//!   See [`types::ValidatedDef`].
//! - [`tree`]: builds an optimal decision tree for instruction dispatch.
//! - [`backend`]: language-specific code generation backends.
//! - [`config`]: internal generation and dispatch config types.
//! - [`codegen`]: Rust decoder/disassembler code generation.
//! - [`lut_gen`]: Rust emulator dispatch LUT generation.
//! - [`instr_gen`]: Rust instruction newtype generation.
//! - [`bindings`]: parser and lowerer for `*.bindings.chipi` configs.

pub mod backend;
pub mod bindings;
pub mod codegen;
pub mod codegen_binja;
pub mod codegen_cpp;
pub mod codegen_ida;
pub mod codegen_python;
pub mod config;
pub mod error;
pub mod format_parser;
pub mod instr_gen;
pub mod lut_gen;
pub mod parser;
pub mod tree;
pub mod types;
pub mod validate;

use std::collections::HashMap;

use error::Errors;
use types::DecoderDef;

pub use config::Dispatch;

/// Parse a `.chipi` file from a file path and return the decoder definition.
pub fn parse(input: &str) -> Result<DecoderDef, Box<dyn std::error::Error>> {
    let path = std::path::Path::new(input);
    parser::parse_file(path).map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)
}

/// Parse source text directly without reading from a file.
pub fn parse_str(source: &str, filename: &str) -> Result<DecoderDef, Vec<error::Error>> {
    parser::parse(source, filename)
}

/// Parse source text and generate Rust decoder code.
/// Returns the generated Rust code as a `String`.
pub fn generate_from_str(
    source: &str,
    filename: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let def = parser::parse(source, filename)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

    let validated = validate::validate(&def)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

    let tree = tree::build_tree(&validated);
    let code = codegen::generate_code(&validated, &tree, &HashMap::new(), &HashMap::new());

    Ok(code)
}
