//! AST and resolved-model types for `*.bindings.chipi` files.

use std::collections::BTreeMap;
use std::path::PathBuf;

use crate::config::Dispatch;
use crate::error::Span;

/// One parsed bindings file. State is post-include-resolution by default.
#[derive(Debug, Clone)]
pub struct BindingsFile {
    /// Canonical path of the file this AST came from.
    pub path: PathBuf,
    /// Each `include "*.chipi"` directive. These are spec includes.
    pub spec_includes: Vec<(PathBuf, Span)>,
    /// Each `include "*.bindings.chipi"` directive.
    pub bindings_includes: Vec<(PathBuf, Span)>,
    /// All `target ... { ... }` blocks at the top level.
    pub targets: Vec<TargetBinding>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetKind {
    Rust,
    Cpp,
    Ida,
    Binja,
}

impl TargetKind {
    pub fn name(self) -> &'static str {
        match self {
            TargetKind::Rust => "rust",
            TargetKind::Cpp => "cpp",
            TargetKind::Ida => "ida",
            TargetKind::Binja => "binja",
        }
    }
}

/// A `target <name> { ... }` block.
#[derive(Debug, Clone)]
pub struct TargetBinding {
    pub kind: TargetKind,
    pub span: Span,
    pub rust_decoders: Vec<DecoderBinding>,
    pub rust_dispatches: Vec<DispatchBinding>,
    pub cpp_decoders: Vec<DecoderBinding>,
    pub ida_processors: Vec<IdaProcessorBinding>,
    pub binja_architectures: Vec<BinjaArchitectureBinding>,
}

impl TargetBinding {
    pub fn empty(kind: TargetKind, span: Span) -> Self {
        Self {
            kind,
            span,
            rust_decoders: Vec::new(),
            rust_dispatches: Vec::new(),
            cpp_decoders: Vec::new(),
            ida_processors: Vec::new(),
            binja_architectures: Vec::new(),
        }
    }
}

/// `decoder <Name> { ... }` block. Used under `target rust` or `target cpp`.
#[derive(Debug, Clone)]
pub struct DecoderBinding {
    pub decoder_name: String,
    pub span: Span,
    pub output: String,
    pub type_map: BTreeMap<String, String>,
    pub subdecoders: Vec<DecoderBinding>,
    /// C++ namespace. Only used under `target cpp`.
    pub cpp_namespace: Option<String>,
    /// C++ include guard style. Either `"pragma"` or `"ifndef"`.
    pub cpp_guard_style: Option<String>,
    /// Extra `#include` directives. Only used under `target cpp`.
    pub cpp_includes: Vec<String>,
}

/// `dispatch <Name> { ... }` (Rust only).
#[derive(Debug, Clone)]
pub struct DispatchBinding {
    pub decoder_name: String,
    pub span: Span,
    pub output: Option<String>,
    pub context: Option<String>,
    pub handlers: Option<String>,
    pub strategy: Option<Dispatch>,
    pub invalid_handler: Option<String>,
    pub instruction_type: Option<InstructionTypeBinding>,
    pub handler_groups: Vec<HandlerBinding>,
    pub subdispatches: Vec<DispatchBinding>,
    /// Extra const-generic argument appended to every handler reference in
    /// the generated LUT. Used when the user's handler signature has more
    /// const generics than the per-instruction OP. Each entry becomes its
    /// own `{ ... }`-wrapped argument. Example: `["crate::sys::GC"]`
    /// produces `handler::<{ OP_X }, { crate::sys::GC }>`.
    pub handler_consts: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct InstructionTypeBinding {
    pub type_path: String,
    pub output: Option<String>,
}

#[derive(Debug, Clone)]
pub struct HandlerBinding {
    pub handler_name: String,
    pub instructions: Vec<(String, Span)>,
    pub span: Span,
}

/// `processor <Name> { ... }` inside `target ida`.
#[derive(Debug, Clone)]
pub struct IdaProcessorBinding {
    pub decoder_name: String,
    pub span: Span,
    pub output: Option<String>,
    pub name: Option<String>,
    pub long_name: Option<String>,
    pub id: Option<u64>,
    pub address_size: Option<u32>,
    pub bytes_per_unit: Option<u32>,
    pub registers: Vec<String>,
    pub segment_registers: Vec<(String, Span)>,
    pub flow: IdaFlowBinding,
}

#[derive(Debug, Clone, Default)]
pub struct IdaFlowBinding {
    pub calls: Vec<(String, Span)>,
    pub returns: Vec<(String, Span)>,
    pub stops: Vec<(String, Span)>,
}

/// `architecture <Name> { ... }` inside `target binja`.
#[derive(Debug, Clone)]
pub struct BinjaArchitectureBinding {
    pub decoder_name: String,
    pub span: Span,
    pub output: Option<String>,
    pub name: Option<String>,
    pub address_size: Option<u32>,
    pub default_int_size: Option<u32>,
    pub endianness: Option<(String, Span)>,
    pub registers: Vec<String>,
}
