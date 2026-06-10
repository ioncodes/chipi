//! C++ decoder backend.
//!
//! Generates a single-header C++ decoder/disassembler from a validated
//! `.chipi` definition.

use crate::codegen_cpp;
use crate::config::{CppOptions, GenTarget};
use crate::tree;
use crate::types::ValidatedDef;

use super::{CodegenBackend, CodegenError};

/// The C++ code generation backend.
pub struct CppBackend;

impl CodegenBackend for CppBackend {
    fn lang(&self) -> &str {
        "cpp"
    }

    fn generate(&self, ir: &ValidatedDef, config: &GenTarget) -> Result<String, CodegenError> {
        let tree = tree::build_tree(ir);
        let default_opts = CppOptions::default();
        let opts = config.lang_options.as_cpp().unwrap_or(&default_opts);
        Ok(codegen_cpp::generate_cpp_code(
            ir,
            &tree,
            opts,
            &config.type_map,
        ))
    }

    fn formatter_command(&self) -> Option<&[&str]> {
        Some(&["clang-format", "-i"])
    }
}
