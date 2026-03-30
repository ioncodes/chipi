//! # chipi-core
//!
//! Core library for the chipi instruction decoder generator.
//!
//! This crate provides the parser, validation, IR, and code generation backends.
//! It is consumed by `chipi-cli` (the standalone CLI tool) and `chipi-build`
//! (the `build.rs` helper for Rust projects).
//!
//! ## Crate structure
//!
//! - [`parser`]: parses `.chipi` files into a raw AST ([`types::DecoderDef`])
//! - [`validate`]: validates and lowers the AST into a language-agnostic IR ([`types::ValidatedDef`])
//! - [`tree`]: builds an optimal decision tree for instruction dispatch
//! - [`backend`]: code generation backends (currently Rust only)
//! - [`config`]: TOML config schema and [`config::Dispatch`] enum
//! - [`codegen`]: Rust decoder/disassembler code generation
//! - [`lut_gen`]: Rust emulator dispatch LUT generation
//! - [`instr_gen`]: Rust instruction newtype generation
//!
//! ## Quick start
//!
//! For `build.rs` usage, prefer `chipi-build` which wraps this library with
//! `cargo:rerun-if-changed` support. For CLI usage, use `chipi-cli`.
//! Use `chipi-core` directly only when you need low-level control.
//!
//! ```ignore
//! // Decoder/disassembler generation
//! chipi_core::CodegenBuilder::new("dsp.chipi")
//!     .type_map("reg5", "crate::dsp::DspReg")
//!     .decoder_dispatch("GcDspExt", chipi_core::Dispatch::JumpTable)
//!     .output("out.rs")
//!     .run()?;
//!
//! // Emulator dispatch LUT (programmatic)
//! chipi_core::LutBuilder::new("cpu.chipi")
//!     .handler_mod("crate::cpu::interpreter")
//!     .ctx_type("crate::Cpu")
//!     .group("alu", ["addi", "addis"])
//!     .build_lut("out/lut.rs")?;
//!
//! // Emulator dispatch LUT (from chipi.toml config)
//! let cfg = chipi_core::config::load_config(Path::new("chipi.toml"))?;
//! for target in &cfg.lut {
//!     chipi_core::LutBuilder::run_target(target)?;
//! }
//! ```

pub mod backend;
pub mod codegen;
pub mod codegen_binja;
pub mod codegen_cpp;
pub mod codegen_ida;
pub mod codegen_python;
pub mod config;
pub mod error;
pub mod format_parser;
pub mod instr_gen;
pub mod lut_gen;
pub mod parser;
pub mod tree;
pub mod types;
pub mod validate;

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use error::Errors;
use types::DecoderDef;

/// Parse a `.chipi` file from a file path and return the decoder definition.
///
/// # Errors
///
/// Returns an error if the file cannot be read or parsed.
///
/// # Example
///
/// ```ignore
/// let def = chipi::parse("thumb.chipi")?;
/// ```
pub fn parse(input: &str) -> Result<DecoderDef, Box<dyn std::error::Error>> {
    let path = Path::new(input);
    // Use include-aware parsing from file path
    parser::parse_file(path).map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)
}

/// Parse source text directly without reading from a file.
///
/// # Arguments
///
/// * `source`: `.chipi` source code
/// * `filename`: name used in error messages
pub fn parse_str(source: &str, filename: &str) -> Result<DecoderDef, Vec<error::Error>> {
    parser::parse(source, filename)
}

/// Validate a parsed definition and write generated Rust code to a file.
///
/// # Errors
///
/// Returns validation or I/O errors.
pub fn emit(def: &DecoderDef, output: &str) -> Result<(), Box<dyn std::error::Error>> {
    let validated = validate::validate(def)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

    let tree = tree::build_tree(&validated);
    let code = codegen::generate_code(&validated, &tree, &HashMap::new(), &HashMap::new());

    fs::write(output, code)?;
    Ok(())
}

/// Full pipeline: parse a `.chipi` file and generate a Rust decoder.
///
/// # Example
///
/// ```ignore
/// chipi::generate("thumb.chipi", "thumb_decoder.rs")?;
/// ```
pub fn generate(input: &str, output: &str) -> Result<(), Box<dyn std::error::Error>> {
    let def = parse(input)?;
    emit(&def, output)?;
    Ok(())
}

/// Generate a function-pointer LUT from a `.chipi` spec file.
///
/// Produces a Rust source file containing:
/// - `pub type Handler = fn(&mut Ctx, u32)`
/// - Static dispatch tables (`_T0`, `_T1`, ...) indexed by opcode bit ranges
/// - `pub fn dispatch(ctx: &mut Ctx, opcode: u32)`
///
/// `handler_mod` is the module path where handler functions live, e.g.
/// `"crate::cpu::interpreter"`Each instruction `foo` in the spec must have
/// a corresponding `pub fn foo(ctx: &mut Ctx, opcode: u32)` there.
///
/// `ctx_type` is the mutable context passed to every handler, e.g.
/// `"crate::gekko::Gekko"`.
///
/// # Example (build.rs)
///
/// ```ignore
/// chipi::generate_lut(
///     "cpu.chipi",
///     out_dir.join("cpu_lut.rs").to_str().unwrap(),
///     "crate::cpu::interpreter",
///     "crate::Cpu",
/// )?;
/// ```
pub fn generate_lut(
    input: &str,
    output: &str,
    handler_mod: &str,
    ctx_type: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let def = parse(input)?;
    let validated = validate::validate(&def)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;
    let t = tree::build_tree(&validated);
    let code = lut_gen::generate_lut_code(
        &validated,
        &t,
        handler_mod,
        ctx_type,
        &HashMap::new(),
        None,
        None,
        Dispatch::FnPtrLut,
    );
    fs::write(output, code)?;
    Ok(())
}

/// Generate an instruction newtype with field accessor methods from a `.chipi` spec.
///
/// Collects all unique fields across all instructions and generates a
/// `pub struct Name(pub u32)` with one `#[inline]` accessor method per field.
///
/// Fields with the same name but conflicting definitions (different bit ranges
/// or types) generate separate accessors with bit range suffixes (e.g., `d_15_0`
/// and `d_11_0`).
///
/// # Example
///
/// ```ignore
/// chipi::generate_instr_type("cpu.chipi", "out/instruction.rs", "Instruction")?;
/// ```
///
/// Then in your code:
///
/// ```ignore
/// mod cpu {
///     include!(concat!(env!("OUT_DIR"), "/instruction.rs"));
/// }
/// ```
pub fn generate_instr_type(
    input: &str,
    output: &str,
    struct_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let def = parse(input)?;
    let validated = validate::validate(&def)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;
    let (code, warnings) = instr_gen::generate_instr_type(&validated, struct_name);

    // Print warnings to stderr (visible during cargo build)
    for warning in &warnings {
        eprintln!("warning: {}", warning);
    }

    fs::write(output, code)?;
    Ok(())
}

/// Builder for generating a function-pointer LUT and handler stubs,
/// with optional grouping of instructions under shared const-generic handlers.
///
/// Use this when you want multiple instructions to share one handler function
/// via a `const OP: u32` generic parameter. See the crate documentation for
/// the full pattern.
///
/// # Example (build.rs)
///
/// ```ignore
/// chipi::LutBuilder::new("cpu.chipi")
///     .handler_mod("crate::cpu::interpreter")
///     .ctx_type("crate::Cpu")
///     .lut_mod("crate::cpu::lut")
///     .group("alu", ["addi", "addis", "ori", "oris"])
///     .group("mem", ["lwz", "stw", "lbz", "stb"])
///     .build_lut(out_dir.join("cpu_lut.rs").to_str().unwrap())?;
///
/// ```
#[derive(Default)]
pub struct LutBuilder {
    input: String,
    handler_mod: String,
    ctx_type: String,
    /// instruction name -> group fn name
    instr_to_group: HashMap<String, String>,
    /// group fn name -> instruction names (for stubs)
    group_to_instrs: HashMap<String, Vec<String>>,
    lut_mod: Option<String>,
    /// Type of the second parameter of every handler (default: `u32`).
    instr_type: Option<String>,
    /// Expression to extract the raw `u32` from the instr local (default: `"instr.0"`
    /// when `instr_type` is set, `"opcode"` otherwise).
    raw_expr: Option<String>,
    /// Dispatch strategy (default: `FnPtrLut`).
    dispatch: Dispatch,
    /// Sub-decoder groups: sub-decoder name -> { instr_name -> group_fn_name }
    subdecoder_groups: HashMap<String, HashMap<String, String>>,
    /// Sub-decoder instruction types: sub-decoder name -> Rust type path
    subdecoder_instr_types: HashMap<String, String>,
}

impl LutBuilder {
    /// Create a new builder targeting the given `.chipi` spec file.
    pub fn new(input: impl Into<String>) -> Self {
        Self {
            input: input.into(),
            ..Default::default()
        }
    }

    /// Set the Rust module path where handler functions live (e.g. `"crate::cpu::interpreter"`).
    pub fn handler_mod(mut self, m: impl Into<String>) -> Self {
        self.handler_mod = m.into();
        self
    }

    /// Set the mutable context type passed to every handler (e.g. `"crate::Cpu"`).
    pub fn ctx_type(mut self, t: impl Into<String>) -> Self {
        self.ctx_type = t.into();
        self
    }

    /// Set the Rust module path where the generated `OP_*` constants live
    /// (e.g. `"crate::cpu::lut"`). Required when using groups so that stubs
    /// can `use {lut_mod}::*` to import the constants.
    pub fn lut_mod(mut self, path: impl Into<String>) -> Self {
        self.lut_mod = Some(path.into());
        self
    }

    /// Override the type of the second parameter of every handler function.
    ///
    /// Defaults to `u32` (raw opcode word). Set to a wrapper type such as
    /// `"crate::cpu::semantics::Instruction"` to have handlers receive a
    /// richer type instead. You must also call [`Self::raw_expr`] to tell
    /// chipi how to extract the underlying `u32` for table indexing.
    pub fn instr_type(mut self, t: impl Into<String>) -> Self {
        self.instr_type = Some(t.into());
        self
    }

    /// Expression that yields a `u32` from the `instr` local inside a generated
    /// dispatch function. Only meaningful when [`Self::instr_type`] is set.
    ///
    /// For a newtype `struct Instruction(pub u32)` this is `"instr.0"` (the default
    /// when `instr_type` is set). For a struct with a `raw()` method use `"instr.raw()"`.
    pub fn raw_expr(mut self, expr: impl Into<String>) -> Self {
        self.raw_expr = Some(expr.into());
        self
    }

    /// Set the dispatch strategy.
    ///
    /// - [`Dispatch::FnPtrLut`] (default): static `[Handler; N]` arrays with indirect
    ///   calls. Each tree level gets its own table.
    /// - [`Dispatch::JumpTable`]: a single `#[inline(always)]` function with nested
    ///   match statements. The compiler can inline handler calls for zero-overhead
    ///   dispatch when handlers are also `#[inline(always)]`.
    pub fn dispatch(mut self, strategy: Dispatch) -> Self {
        self.dispatch = strategy;
        self
    }

    /// Register a group: `name` is the shared handler function name (e.g. `"alu"`),
    /// `instrs` lists the instruction names that route to it.
    ///
    /// Each instruction in `instrs` will appear in the LUT as
    /// `handler_mod::alu::<{ OP_INSTR }>` instead of `handler_mod::instr`.
    /// The generated stub is `pub fn alu<const OP: u32>(...)` with a `match OP` body.
    pub fn group(
        mut self,
        name: impl Into<String>,
        instrs: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        let name = name.into();
        let instrs: Vec<String> = instrs.into_iter().map(|s| s.into()).collect();
        for instr in &instrs {
            self.instr_to_group.insert(instr.clone(), name.clone());
        }
        self.group_to_instrs.insert(name, instrs);
        self
    }

    /// Create a `LutBuilder` from a [`config::LutTarget`].
    pub fn from_config(target: &config::LutTarget) -> Self {
        let mut builder = Self::new(&target.input)
            .handler_mod(&target.handler_mod)
            .ctx_type(&target.ctx_type)
            .dispatch(target.dispatch);

        if let Some(ref lut_mod) = target.lut_mod {
            builder = builder.lut_mod(lut_mod);
        }
        if let Some(ref instr_type) = target.instr_type {
            builder = builder.instr_type(instr_type);
        }
        if let Some(ref raw_expr) = target.raw_expr {
            builder = builder.raw_expr(raw_expr);
        }
        for (name, instrs) in &target.groups {
            builder = builder.group(name, instrs.iter().map(|s| s.as_str()));
        }
        // Build sub-decoder groups: sd_name -> { instr_name -> group_fn_name }
        for (sd_name, groups) in &target.subdecoder_groups {
            let mut instr_to_group = HashMap::new();
            for (group_name, instrs) in groups {
                for instr in instrs {
                    instr_to_group.insert(instr.clone(), group_name.clone());
                }
            }
            builder
                .subdecoder_groups
                .insert(sd_name.clone(), instr_to_group);
        }
        for (sd_name, sd_type) in &target.subdecoder_instr_types {
            builder
                .subdecoder_instr_types
                .insert(sd_name.clone(), sd_type.clone());
        }
        builder
    }

    /// Run all outputs defined in a [`config::LutTarget`].
    ///
    /// Generates the LUT file, and optionally the instruction type and stubs
    /// if configured. Stubs are only generated if the target file does not exist.
    pub fn run_target(target: &config::LutTarget) -> Result<(), Box<dyn std::error::Error>> {
        let builder = Self::from_config(target);

        builder.build_lut(&target.output)?;

        if let Some(ref instr_output) = target.instr_type_output {
            builder.build_instr_type(instr_output)?;
        }

        for (sd_name, sd_output) in &target.subdecoder_instr_type_outputs {
            builder.build_subdecoder_instr_type(sd_name, sd_output)?;
        }

        Ok(())
    }

    /// Generate the LUT source file.
    pub fn build_lut(&self, output: &str) -> Result<(), Box<dyn std::error::Error>> {
        let def = parse(&self.input)?;
        let validated = validate::validate(&def)
            .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;
        let t = tree::build_tree(&validated);
        let mut code = lut_gen::generate_lut_code(
            &validated,
            &t,
            &self.handler_mod,
            &self.ctx_type,
            &self.instr_to_group,
            self.instr_type.as_deref(),
            self.raw_expr.as_deref(),
            self.dispatch,
        );

        // Generate dispatch functions for sub-decoders that have groups configured
        for sd in &validated.sub_decoders {
            if let Some(groups) = self.subdecoder_groups.get(&sd.name) {
                code.push('\n');
                code.push_str(&lut_gen::generate_subdecoder_dispatch(
                    &validated,
                    sd,
                    &self.handler_mod,
                    &self.ctx_type,
                    groups,
                    self.subdecoder_instr_types
                        .get(&sd.name)
                        .map(|s| s.as_str()),
                ));
            }
        }

        fs::write(output, code)?;
        Ok(())
    }

    /// Generate an instruction newtype with field accessor methods.
    ///
    /// Collects all unique fields from the spec and generates a
    /// `pub struct Name(pub u32)` with one `#[inline]` accessor per field.
    ///
    /// The struct name is derived from the last path segment of `.instr_type()`
    /// (e.g., `"crate::cpu::Instruction"` -> `"Instruction"`), or defaults to
    /// `"Instruction"` if `.instr_type()` was not called.
    ///
    /// Fields with conflicting definitions across instructions generate separate
    /// accessors with bit range suffixes (e.g., `d_15_0` and `d_11_0`).
    ///
    /// # Example
    ///
    /// ```ignore
    /// chipi::LutBuilder::new("cpu.chipi")
    ///     .instr_type("crate::cpu::Instruction")
    ///     .build_instr_type(out_dir.join("instruction.rs").to_str().unwrap())?;
    /// ```
    pub fn build_instr_type(&self, output: &str) -> Result<(), Box<dyn std::error::Error>> {
        let def = parse(&self.input)?;
        let validated = validate::validate(&def)
            .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

        // Derive struct name from instr_type path or default to "Instruction"
        let struct_name = self
            .instr_type
            .as_deref()
            .and_then(|t| t.rsplit("::").next())
            .unwrap_or("Instruction");

        let (code, warnings) = instr_gen::generate_instr_type(&validated, struct_name);

        // Print warnings to stderr (visible during cargo build)
        for warning in &warnings {
            eprintln!("cargo:warning={}", warning);
        }

        fs::write(output, code)?;
        Ok(())
    }

    /// Generate an instruction newtype for a sub-decoder.
    ///
    /// Collects all unique fields from the sub-decoder's instructions and generates
    /// a `pub struct Name(pub u8/u16/u32)` with accessor methods.
    pub fn build_subdecoder_instr_type(
        &self,
        sd_name: &str,
        output: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let def = parse(&self.input)?;
        let validated = validate::validate(&def)
            .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

        let sd = validated
            .sub_decoders
            .iter()
            .find(|sd| sd.name == sd_name)
            .ok_or_else(|| format!("sub-decoder '{}' not found in spec", sd_name))?;

        let (code, warnings) = instr_gen::generate_subdecoder_instr_type(sd, sd_name);

        for warning in &warnings {
            eprintln!("cargo:warning={}", warning);
        }

        fs::write(output, code)?;
        Ok(())
    }
}

/// Parse, validate, and generate code from source text. Returns the
/// generated Rust code as a `String`.
///
/// # Errors
///
/// Returns parse or validation errors.
pub fn generate_from_str(
    source: &str,
    filename: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let def = parser::parse(source, filename)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

    let validated = validate::validate(&def)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

    let tree = tree::build_tree(&validated);
    let code = codegen::generate_code(&validated, &tree, &HashMap::new(), &HashMap::new());

    Ok(code)
}

pub use config::Dispatch;

/// Builder for generating a decoder with type mappings and dispatch strategy control.
///
/// Use this when you need to map chipi type names to Rust wrapper types (replacing
/// the removed `import`/`as` syntax) or control the dispatch strategy per decoder.
///
/// # Example (build.rs)
///
/// ```ignore
/// chipi::CodegenBuilder::new("src/gcdsp.chipi")
///     .type_map("reg5", "crate::dsp::DspReg")
///     .decoder_dispatch("GcDsp", chipi::Dispatch::FnPtrLut)
///     .decoder_dispatch("GcDspExt", chipi::Dispatch::JumpTable)
///     .output("src/generated/gcdsp.rs")
///     .run();
/// ```
#[derive(Default)]
pub struct CodegenBuilder {
    input: String,
    type_maps: HashMap<String, String>,
    dispatch_overrides: HashMap<String, Dispatch>,
    output: Option<String>,
}

impl CodegenBuilder {
    /// Create a new builder targeting the given `.chipi` spec file.
    pub fn new(input: impl Into<String>) -> Self {
        Self {
            input: input.into(),
            ..Default::default()
        }
    }

    /// Map a chipi type name to a Rust type path.
    ///
    /// Fields declared with this type name in the `.chipi` file will use the
    /// given Rust type in generated code. The codegen emits a `use` statement
    /// for paths containing `::`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// .type_map("reg5", "crate::dsp::DspReg")
    /// ```
    pub fn type_map(mut self, chipi_type: &str, rust_path: &str) -> Self {
        self.type_maps
            .insert(chipi_type.to_string(), rust_path.to_string());
        self
    }

    /// Set the dispatch strategy for a specific decoder or sub-decoder.
    ///
    /// Defaults: `JumpTable` for sub-decoders, decision tree for main decoders.
    pub fn decoder_dispatch(mut self, decoder_name: &str, strategy: Dispatch) -> Self {
        self.dispatch_overrides
            .insert(decoder_name.to_string(), strategy);
        self
    }

    /// Set the output file path.
    pub fn output(mut self, path: &str) -> Self {
        self.output = Some(path.to_string());
        self
    }

    /// Run the full pipeline: parse, validate, and generate code.
    pub fn run(&self) -> Result<(), Box<dyn std::error::Error>> {
        let def = parse(&self.input)?;
        let validated = validate::validate(&def)
            .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

        let tree = tree::build_tree(&validated);
        let code =
            codegen::generate_code(&validated, &tree, &self.type_maps, &self.dispatch_overrides);

        if let Some(ref output) = self.output {
            fs::write(output, code)?;
        }

        Ok(())
    }
}
