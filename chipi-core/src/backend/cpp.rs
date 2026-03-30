//! C++ decoder backend.
//!
//! Generates a single-header C++ decoder/disassembler from a validated
//! `.chipi` definition.

use crate::codegen_cpp;
use crate::config::GenTarget;
use crate::tree;
use crate::types::ValidatedDef;

use super::{CodegenBackend, CodegenError};

/// The C++ code generation backend.
pub struct CppBackend;

/// Parsed C++-specific configuration from `lang_options`.
pub struct CppOptions {
    /// C++ namespace for generated code. Defaults to decoder name in snake_case.
    pub namespace: Option<String>,
    /// Include guard style: "pragma" (default) or "ifndef".
    pub guard_style: GuardStyle,
    /// Additional `#include` directives for user-provided type headers.
    pub includes: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GuardStyle {
    Pragma,
    Ifndef,
}

impl CodegenBackend for CppBackend {
    fn lang(&self) -> &str {
        "cpp"
    }

    fn validate_lang_options(&self, options: &toml::Value) -> Result<(), Vec<String>> {
        let table = match options {
            toml::Value::Table(t) => t,
            _ => return Ok(()),
        };

        let known_keys = ["namespace", "guard_style", "includes"];
        let mut errors = Vec::new();
        for key in table.keys() {
            if !known_keys.contains(&key.as_str()) {
                errors.push(format!("unknown lang_option: {}", key));
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn generate(&self, ir: &ValidatedDef, config: &GenTarget) -> Result<String, CodegenError> {
        let tree = tree::build_tree(ir);
        let opts = parse_cpp_options(config);
        Ok(codegen_cpp::generate_cpp_code(
            ir,
            &tree,
            &opts,
            &config.type_map,
        ))
    }

    fn formatter_command(&self) -> Option<&[&str]> {
        Some(&["clang-format", "-i"])
    }
}

fn parse_cpp_options(config: &GenTarget) -> CppOptions {
    let table = match &config.lang_options {
        Some(toml::Value::Table(t)) => Some(t.clone()),
        _ => None,
    };

    let namespace = table
        .as_ref()
        .and_then(|t| t.get("namespace"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    let guard_style = table
        .as_ref()
        .and_then(|t| t.get("guard_style"))
        .and_then(|v| v.as_str())
        .map(|s| match s {
            "ifndef" => GuardStyle::Ifndef,
            _ => GuardStyle::Pragma,
        })
        .unwrap_or(GuardStyle::Pragma);

    let includes = table
        .as_ref()
        .and_then(|t| t.get("includes"))
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default();

    CppOptions {
        namespace,
        guard_style,
        includes,
    }
}
