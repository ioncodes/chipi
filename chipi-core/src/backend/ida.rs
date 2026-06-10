//! IDA Pro processor module backend.
//!
//! Generates a Python (IDAPython) processor module for IDA Pro 9.x
//! from a validated `.chipi` definition.

use crate::codegen_ida;
use crate::config::GenTarget;
use crate::tree;
use crate::types::ValidatedDef;

use super::{CodegenBackend, CodegenError};

/// The IDA Pro code generation backend.
pub struct IdaBackend;

impl CodegenBackend for IdaBackend {
    fn lang(&self) -> &str {
        "ida"
    }

    fn generate(&self, ir: &ValidatedDef, config: &GenTarget) -> Result<String, CodegenError> {
        let tree = tree::build_tree(ir);
        let opts = config
            .lang_options
            .as_ida()
            .ok_or_else(|| CodegenError::Internal("IDA backend requires IDA options".into()))?;
        Ok(codegen_ida::generate_ida_code(
            ir,
            &tree,
            opts,
            &config.type_map,
        ))
    }

    fn formatter_command(&self) -> Option<&[&str]> {
        Some(&["ruff", "format"])
    }
}
