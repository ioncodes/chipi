//! Binary Ninja Architecture plugin backend.
//!
//! Generates a Python Architecture plugin for Binary Ninja
//! from a validated `.chipi` definition.

use crate::codegen_binja;
use crate::config::GenTarget;
use crate::tree;
use crate::types::ValidatedDef;

use super::{CodegenBackend, CodegenError};

/// The Binary Ninja code generation backend.
pub struct BinjaBackend;

impl CodegenBackend for BinjaBackend {
    fn lang(&self) -> &str {
        "binja"
    }

    fn generate(&self, ir: &ValidatedDef, config: &GenTarget) -> Result<String, CodegenError> {
        let tree = tree::build_tree(ir);
        let opts = config.lang_options.as_binja().ok_or_else(|| {
            CodegenError::Internal("Binary Ninja backend requires Binja options".into())
        })?;
        Ok(codegen_binja::generate_binja_code(ir, &tree, opts))
    }

    fn formatter_command(&self) -> Option<&[&str]> {
        Some(&["ruff", "format"])
    }
}
