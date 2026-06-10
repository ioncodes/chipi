//! Internal configuration types consumed by the code generation pipeline.
//!
//! These types are populated by the bindings frontend (`crate::bindings`).
//! They are not user-facing. The user-facing format is `*.bindings.chipi`.

use std::collections::HashMap;
use std::path::Path;

use crate::backend::{FlowConfig, OperandKind};

/// A single decoder/disassembler code generation target.
#[derive(Debug, Clone)]
pub struct GenTarget {
    /// Path to the input `.chipi` file.
    pub input: String,

    /// Target language backend. One of `"rust"`, `"cpp"`, `"ida"`, `"binja"`.
    pub lang: String,

    /// Output file path. Supports `$VAR` and `${VAR}` env var expansion.
    pub output: String,

    /// Run a language-appropriate formatter on the output when true.
    pub format: bool,

    /// Default dispatch strategy for all decoders and sub-decoders.
    pub dispatch: Dispatch,

    /// Per-decoder dispatch strategy overrides.
    pub dispatch_overrides: HashMap<String, Dispatch>,

    /// Map a chipi type name to a language-specific type path.
    pub type_map: HashMap<String, String>,

    /// Backend-specific options. Each backend reads only its own variant.
    pub lang_options: LangOptions,
}

impl GenTarget {
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
            lang_options: LangOptions::None,
        }
    }
}

/// A single emulator dispatch LUT generation target.
#[derive(Debug, Clone)]
pub struct LutTarget {
    /// Path to the input `.chipi` file.
    pub input: String,

    /// Output file path for the LUT dispatch code.
    pub output: String,

    /// Rust module path where handler functions live.
    pub handler_mod: String,

    /// Mutable context type passed to every handler.
    pub ctx_type: String,

    /// Dispatch strategy.
    pub dispatch: Dispatch,

    /// Map a group name to its list of instruction names.
    pub groups: HashMap<String, Vec<String>>,

    /// Rust module path where the generated `OP_*` constants live.
    pub lut_mod: Option<String>,

    /// Override the type of the second handler parameter.
    pub instr_type: Option<String>,

    /// Expression to extract the raw integer from the instr local.
    pub raw_expr: Option<String>,

    /// Output file path for the instruction newtype with field accessors.
    pub instr_type_output: Option<String>,

    /// Map a sub-decoder name to its group map.
    pub subdecoder_groups: HashMap<String, HashMap<String, Vec<String>>>,

    /// Map a sub-decoder name to its instr-type output path.
    pub subdecoder_instr_type_outputs: HashMap<String, String>,

    /// Map a sub-decoder name to its instr-type Rust path.
    pub subdecoder_instr_types: HashMap<String, String>,

    /// Map a sub-decoder name to its dispatch strategy.
    /// An empty map makes sub-decoders inherit from `dispatch`.
    pub subdecoder_dispatch: HashMap<String, Dispatch>,

    /// Path to the handler called for unmatched opcodes.
    /// `None` falls back to a `todo!()` panic.
    pub invalid_handler: Option<String>,

    /// Map a sub-decoder name to its invalid handler path.
    pub subdecoder_invalid_handlers: HashMap<String, String>,

    /// Map a sub-decoder name to its handler module override.
    pub subdecoder_handler_mods: HashMap<String, String>,
}

/// Dispatch strategy for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Dispatch {
    /// `#[inline(always)]` match statement.
    JumpTable,
    /// Static function pointer lookup table per decision-tree branch.
    #[default]
    FnPtrLut,
    /// Full-width function-pointer table indexed by raw decoder value.
    FlatLut,
    /// Full-width match with adjacent equal handlers compressed into ranges.
    FlatMatch,
}

/// Backend-specific options.
#[derive(Debug, Clone, Default)]
pub enum LangOptions {
    #[default]
    None,
    Cpp(CppOptions),
    Ida(IdaOptions),
    Binja(BinjaOptions),
}

impl LangOptions {
    pub fn as_cpp(&self) -> Option<&CppOptions> {
        match self {
            LangOptions::Cpp(o) => Some(o),
            _ => None,
        }
    }

    pub fn as_ida(&self) -> Option<&IdaOptions> {
        match self {
            LangOptions::Ida(o) => Some(o),
            _ => None,
        }
    }

    pub fn as_binja(&self) -> Option<&BinjaOptions> {
        match self {
            LangOptions::Binja(o) => Some(o),
            _ => None,
        }
    }
}

/// IDA processor module options.
#[derive(Debug, Clone)]
pub struct IdaOptions {
    pub processor_name: String,
    pub processor_long_name: String,
    pub processor_id: u64,
    pub register_names: Vec<String>,
    pub segment_registers: Vec<String>,
    pub address_size: u32,
    /// Bytes per addressable unit. Word-addressed architectures use 2.
    pub bytes_per_unit: u32,
    pub flags: Vec<String>,
    pub operand_types: HashMap<String, OperandKind>,
    /// Map type alias names to display prefixes. Example: `"gpr"` -> `"r"`.
    pub display_prefixes: HashMap<String, String>,
    pub flow: FlowConfig,
}

/// Binary Ninja architecture plugin options.
#[derive(Debug, Clone)]
pub struct BinjaOptions {
    pub architecture_name: String,
    pub address_size: u32,
    pub default_int_size: u32,
    pub max_instr_length: u32,
    pub endianness: String,
    pub register_names: Vec<String>,
    pub register_size: u32,
    pub stack_pointer: Option<String>,
    pub link_register: Option<String>,
    pub bytes_per_unit: u32,
    pub display_prefixes: HashMap<String, String>,
    pub operand_types: HashMap<String, OperandKind>,
    pub flow: FlowConfig,
}

/// C++ backend options.
#[derive(Debug, Clone, Default)]
pub struct CppOptions {
    /// C++ namespace for generated code. Defaults to the decoder name in
    /// snake_case.
    pub namespace: Option<String>,
    pub guard_style: CppGuardStyle,
    /// Extra `#include` directives for user-provided type headers.
    pub includes: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CppGuardStyle {
    #[default]
    Pragma,
    Ifndef,
}

/// Expand environment variables (`$VAR` or `${VAR}`) in a string.
/// Unresolved variables are left as-is.
pub fn expand_env(s: &str) -> String {
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

/// Resolve a path. Expands env vars first. Joins relative paths to `base_dir`.
pub fn resolve_path(path: &str, base_dir: &Path) -> String {
    let expanded = expand_env(path);
    let p = Path::new(&expanded);
    if p.is_absolute() {
        expanded
    } else {
        base_dir.join(&expanded).to_string_lossy().into_owned()
    }
}

/// Resolve paths in a `GenTarget` relative to a base directory.
pub fn resolve_gen_paths(target: &mut GenTarget, base_dir: &Path) {
    target.input = resolve_path(&target.input, base_dir);
    target.output = resolve_path(&target.output, base_dir);
}

/// Resolve paths in a `LutTarget` relative to a base directory.
pub fn resolve_lut_paths(target: &mut LutTarget, base_dir: &Path) {
    target.input = resolve_path(&target.input, base_dir);
    target.output = resolve_path(&target.output, base_dir);
    if let Some(ref mut p) = target.instr_type_output {
        *p = resolve_path(p, base_dir);
    }
    for p in target.subdecoder_instr_type_outputs.values_mut() {
        *p = resolve_path(p, base_dir);
    }
}
