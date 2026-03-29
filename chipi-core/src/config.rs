//! Configuration types for chipi code generation.
//!
//! Defines the TOML config schema (`chipi.toml`) and the target types
//! that carry all settings for code generation runs.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// Top-level chipi.toml configuration.
///
/// A single config file can define multiple generation targets of both kinds.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct ChipiConfig {
    /// Decoder/disassembler generation targets.
    #[serde(rename = "gen", default)]
    pub targets: Vec<GenTarget>,

    /// Emulator dispatch LUT generation targets.
    #[serde(default)]
    pub lut: Vec<LutTarget>,
}

/// A single decoder/disassembler code generation target.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct GenTarget {
    /// Path to the input `.chipi` file.
    /// Relative paths are resolved from the TOML file's directory.
    /// Supports `$VAR` / `${VAR}` environment variable expansion (e.g. `$OUT_DIR`).
    pub input: String,

    /// Target language backend. Currently only `"rust"` is supported.
    pub lang: String,

    /// Output file path.
    /// Relative paths are resolved from the TOML file's directory.
    /// Supports `$VAR` / `${VAR}` environment variable expansion (e.g. `$OUT_DIR`).
    pub output: String,

    /// Whether to run a language-appropriate formatter on the output.
    #[serde(default)]
    pub format: bool,

    /// Default dispatch strategy for all decoders/sub-decoders.
    #[serde(default)]
    pub dispatch: Dispatch,

    /// Per-decoder dispatch strategy overrides.
    #[serde(default)]
    pub dispatch_overrides: HashMap<String, Dispatch>,

    /// Type mappings: chipi type name -> language-specific type path.
    #[serde(default)]
    pub type_map: HashMap<String, String>,

    /// Reserved for language-specific settings.
    #[serde(default)]
    pub lang_options: Option<toml::Value>,
}

impl GenTarget {
    /// Create a new `GenTarget` with the given input, lang, and output.
    pub fn new(
        input: impl Into<String>,
        lang: impl Into<String>,
        output: impl Into<String>,
    ) -> Self {
        Self {
            input: input.into(),
            lang: lang.into(),
            output: output.into(),
            format: false,
            dispatch: Dispatch::default(),
            dispatch_overrides: HashMap::new(),
            type_map: HashMap::new(),
            lang_options: None,
        }
    }
}

/// A single emulator dispatch LUT generation target.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LutTarget {
    /// Path to the input `.chipi` file.
    /// Supports `$VAR` expansion and relative paths (resolved from the TOML file's directory).
    pub input: String,

    /// Output file path for the LUT dispatch code.
    /// Supports `$VAR` expansion (e.g. `$OUT_DIR/lut.rs`).
    pub output: String,

    /// Rust module path where handler functions live.
    pub handler_mod: String,

    /// Mutable context type passed to every handler.
    pub ctx_type: String,

    /// Dispatch strategy.
    #[serde(default)]
    pub dispatch: Dispatch,

    /// Instruction groups: group name -> list of instruction names.
    /// Instructions in a group share one const-generic handler function.
    #[serde(default)]
    pub groups: HashMap<String, Vec<String>>,

    /// Rust module path where the generated OP_* constants live.
    /// Required when using groups so stubs can import the constants.
    #[serde(default)]
    pub lut_mod: Option<String>,

    /// Override the type of the second handler parameter (default: width-derived u8/u16/u32).
    /// Set to a wrapper type like `"crate::cpu::Instruction"`.
    #[serde(default)]
    pub instr_type: Option<String>,

    /// Expression to extract the raw integer from the instr local.
    /// Default: `"instr.0"` when `instr_type` is set, `"opcode"` otherwise.
    #[serde(default)]
    pub raw_expr: Option<String>,

    /// Output file path for the instruction newtype with field accessors.
    /// Supports `$VAR` expansion.
    /// If set, generates a `pub struct Name(pub u32)` with accessor methods.
    #[serde(default)]
    pub instr_type_output: Option<String>,
}

/// Dispatch strategy for code generation.
///
/// Controls how decoders, sub-decoders, and emulator LUTs dispatch to handlers.
/// The names are language-neutral; each backend maps them to the appropriate
/// language construct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Dispatch {
    /// `#[inline(always)]` match statement (Rust), `switch` (C++), etc.
    JumpTable,
    /// Static function pointer lookup table.
    #[default]
    FnPtrLut,
}

/// Load a `ChipiConfig` from a TOML file.
pub fn load_config(path: &Path) -> Result<ChipiConfig, ConfigError> {
    let content =
        std::fs::read_to_string(path).map_err(|e| ConfigError::Io(path.to_path_buf(), e))?;
    let config: ChipiConfig =
        toml::from_str(&content).map_err(|e| ConfigError::Parse(path.to_path_buf(), e))?;
    Ok(config)
}

/// Expand environment variables (`$VAR` or `${VAR}`) in a string.
/// Unresolved variables are left as-is.
fn expand_env(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '$' {
            let braced = chars.peek() == Some(&'{');
            if braced {
                chars.next();
            }
            let mut name = String::new();
            if braced {
                while let Some(&c) = chars.peek() {
                    if c == '}' {
                        chars.next();
                        break;
                    }
                    name.push(c);
                    chars.next();
                }
            } else {
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        name.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
            }
            if let Ok(val) = std::env::var(&name) {
                result.push_str(&val);
            } else if braced {
                result.push_str(&format!("${{{}}}", name));
            } else {
                result.push('$');
                result.push_str(&name);
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// Resolve a path: expand env vars, then make relative paths relative to base_dir.
fn resolve_path(path: &str, base_dir: &Path) -> String {
    let expanded = expand_env(path);
    let p = Path::new(&expanded);
    if p.is_absolute() {
        expanded
    } else {
        base_dir.join(&expanded).to_string_lossy().into_owned()
    }
}

/// Resolve paths in a `GenTarget` relative to a base directory.
/// Supports `$OUT_DIR`, `$CARGO_MANIFEST_DIR`, etc. in paths.
pub fn resolve_gen_paths(target: &mut GenTarget, base_dir: &Path) {
    target.input = resolve_path(&target.input, base_dir);
    target.output = resolve_path(&target.output, base_dir);
}

/// Resolve paths in a `LutTarget` relative to a base directory.
/// Supports `$OUT_DIR`, `$CARGO_MANIFEST_DIR`, etc. in paths.
pub fn resolve_lut_paths(target: &mut LutTarget, base_dir: &Path) {
    target.input = resolve_path(&target.input, base_dir);
    target.output = resolve_path(&target.output, base_dir);
    if let Some(ref mut p) = target.instr_type_output {
        *p = resolve_path(p, base_dir);
    }
}

#[derive(Debug)]
pub enum ConfigError {
    Io(PathBuf, std::io::Error),
    Parse(PathBuf, toml::de::Error),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::Io(path, e) => write!(f, "failed to read {}: {}", path.display(), e),
            ConfigError::Parse(path, e) => {
                write!(f, "failed to parse {}: {}", path.display(), e)
            }
        }
    }
}

impl std::error::Error for ConfigError {}
