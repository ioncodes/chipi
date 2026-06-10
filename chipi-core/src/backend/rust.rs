//! Rust code generation backend.
//!
//! Generates `.rs` files with instruction enums, decode functions,
//! display implementations, and sub-decoder dispatch code.

use crate::codegen;
use crate::config::GenTarget;
use crate::tree;
use crate::types::ValidatedDef;

use super::{CodegenBackend, CodegenError};

/// The Rust code generation backend.
pub struct RustBackend;

impl CodegenBackend for RustBackend {
    fn lang(&self) -> &str {
        "rust"
    }

    fn generate(&self, ir: &ValidatedDef, config: &GenTarget) -> Result<String, CodegenError> {
        let tree = tree::build_tree(ir);
        let code = codegen::generate_code(ir, &tree, &config.type_map, &config.dispatch_overrides);
        Ok(code)
    }

    fn formatter_command(&self) -> Option<&[&str]> {
        Some(&["rustfmt"])
    }
}
