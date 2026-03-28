//! Code generation backends.
//!
//! Each backend implements [`CodegenBackend`] to emit source code for a
//! specific target language. Only the Rust backend exists today; the trait
//! is the extensibility point for future languages.

pub mod rust;

use crate::config::GenTarget;
use crate::types::ValidatedDef;

/// Trait for language-specific code generation backends.
pub trait CodegenBackend {
    /// Language identifier (e.g., `"rust"`).
    fn lang(&self) -> &str;

    /// Validate language-specific options from the `lang_options` config field.
    /// Returns errors for unknown or invalid keys.
    fn validate_lang_options(&self, options: &toml::Value) -> Result<(), Vec<String>>;

    /// Generate source code from the decoder IR and configuration.
    fn generate(&self, ir: &ValidatedDef, config: &GenTarget) -> Result<String, CodegenError>;

    /// Optional: command to format the generated source.
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
