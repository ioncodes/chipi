//! Code generation backends.
//!
//! Each backend implements [`CodegenBackend`]. Each one emits source code
//! for one target language.

pub mod binja;
pub mod cpp;
pub mod ida;
pub mod rust;

use crate::config::GenTarget;
use crate::types::ValidatedDef;

/// Operand classification shared across backends.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandKind {
    Register,
    Immediate,
    Address,
    Memory,
}

/// Flow analysis configuration shared across backends.
#[derive(Debug, Clone, Default)]
pub struct FlowConfig {
    pub calls: Vec<String>,
    pub branches: Vec<String>,
    pub unconditional_branches: Vec<String>,
    pub returns: Vec<String>,
    pub stops: Vec<String>,
}

/// Trait for language-specific code generation backends.
pub trait CodegenBackend {
    /// Language identifier. Examples: `"rust"`, `"cpp"`, `"ida"`, `"binja"`.
    fn lang(&self) -> &str;

    /// Generate source code from the decoder IR and configuration.
    fn generate(&self, ir: &ValidatedDef, config: &GenTarget) -> Result<String, CodegenError>;

    /// Command to format the generated source. `None` skips formatting.
    fn formatter_command(&self) -> Option<&[&str]>;
}

#[derive(Debug)]
pub enum CodegenError {
    Internal(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::Internal(msg) => write!(f, "codegen error: {}", msg),
        }
    }
}

impl std::error::Error for CodegenError {}

/// Get a backend by language name.
pub fn get_backend(lang: &str) -> Option<Box<dyn CodegenBackend>> {
    match lang {
        "rust" => Some(Box::new(rust::RustBackend)),
        "ida" => Some(Box::new(ida::IdaBackend)),
        "binja" => Some(Box::new(binja::BinjaBackend)),
        "cpp" => Some(Box::new(cpp::CppBackend)),
        _ => None,
    }
}

/// Run a formatter command on a file. Warns on failure; does not error.
pub fn run_formatter(cmd: &[&str], path: &str) {
    let status = std::process::Command::new(cmd[0])
        .args(&cmd[1..])
        .arg(path)
        .status();

    match status {
        Ok(s) if s.success() => {}
        Ok(s) => eprintln!("warning: formatter exited with {}", s),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            eprintln!("warning: formatter '{}' not found, skipping", cmd[0]);
        }
        Err(e) => eprintln!("warning: failed to run formatter: {}", e),
    }
}
