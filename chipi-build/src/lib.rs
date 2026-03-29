//! Build script helper for chipi code generation in Rust projects.
//!
//! Provides builder APIs and config-driven generation for use in `build.rs` files.
//!
//! # Config-driven usage
//!
//! ```ignore
//! // build.rs
//! fn main() {
//!     chipi_build::run_config("chipi.toml")
//!         .expect("chipi codegen failed");
//! }
//! ```
//!
//! # Programmatic usage
//!
//! ```ignore
//! // Decoder/disassembler generation
//! chipi_build::generate("src/dsp/gcdsp.chipi")
//!     .type_map("reg5", "crate::dsp::DspReg")
//!     .dispatch_default(chipi_build::Dispatch::FnPtrLut)
//!     .dispatch_for("GcDspExt", chipi_build::Dispatch::JumpTable)
//!     .output("src/dsp/generated/gcdsp.rs")
//!     .run()
//!     .expect("chipi codegen failed");
//!
//! // Emulator dispatch LUT generation
//! chipi_build::lut("cpu.chipi")
//!     .handler_mod("crate::cpu::interpreter")
//!     .ctx_type("crate::Cpu")
//!     .output("out/cpu_lut.rs")
//!     .run()
//!     .expect("chipi lut failed");
//! ```

use std::collections::HashMap;
use std::path::Path;

pub use chipi_core::config::Dispatch;

// ---------------------------------------------------------------------------
// Config-driven entry point
// ---------------------------------------------------------------------------

/// Run all targets defined in a `chipi.toml` config file.
///
/// Processes all `[[gen]]` and `[[lut]]` targets. Emits `cargo:rerun-if-changed`
/// for all input files and their includes.
pub fn run_config(path: impl AsRef<Path>) -> Result<(), Box<dyn std::error::Error>> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map(std::path::PathBuf::from)
        .unwrap_or_default();
    let path = manifest_dir.join(path.as_ref());
    let mut cfg = chipi_core::config::load_config(&path)?;
    let base_dir = path.parent().unwrap_or(Path::new("."));

    println!("cargo:rerun-if-changed={}", path.display());

    for target in &mut cfg.targets {
        chipi_core::config::resolve_gen_paths(target, base_dir);
        run_gen_target(target)?;
    }

    for target in &mut cfg.lut {
        chipi_core::config::resolve_lut_paths(target, base_dir);
        run_lut_target(target)?;
    }

    Ok(())
}

fn run_gen_target(
    target: &chipi_core::config::GenTarget,
) -> Result<(), Box<dyn std::error::Error>> {
    let (def, deps) = chipi_core::parser::parse_file_with_deps(Path::new(&target.input))
        .map_err(|errs| Box::new(chipi_core::error::Errors(errs)) as Box<dyn std::error::Error>)?;

    for dep in &deps {
        println!("cargo:rerun-if-changed={}", dep.display());
    }

    let validated = chipi_core::validate::validate(&def)
        .map_err(|errs| Box::new(chipi_core::error::Errors(errs)) as Box<dyn std::error::Error>)?;

    let backend = chipi_core::backend::get_backend(&target.lang).unwrap();
    let code = backend.generate(&validated, target)?;
    std::fs::write(&target.output, code)?;

    if target.format {
        if let Some(cmd) = backend.formatter_command() {
            chipi_core::backend::run_formatter(cmd, &target.output);
        }
    }

    Ok(())
}

fn run_lut_target(
    target: &chipi_core::config::LutTarget,
) -> Result<(), Box<dyn std::error::Error>> {
    let (_, deps) = chipi_core::parser::parse_file_with_deps(Path::new(&target.input))
        .map_err(|errs| Box::new(chipi_core::error::Errors(errs)) as Box<dyn std::error::Error>)?;

    for dep in &deps {
        println!("cargo:rerun-if-changed={}", dep.display());
    }

    chipi_core::LutBuilder::run_target(target)
}

// ---------------------------------------------------------------------------
// Decoder/disassembler generation builder
// ---------------------------------------------------------------------------

/// Start building a decoder/disassembler generation target.
pub fn generate(input: impl Into<String>) -> GenBuilder {
    GenBuilder {
        input: input.into(),
        output: None,
        type_map: HashMap::new(),
        dispatch: Dispatch::FnPtrLut,
        dispatch_overrides: HashMap::new(),
        format: false,
    }
}

/// Builder for decoder/disassembler code generation.
pub struct GenBuilder {
    input: String,
    output: Option<String>,
    type_map: HashMap<String, String>,
    dispatch: Dispatch,
    dispatch_overrides: HashMap<String, Dispatch>,
    format: bool,
}

impl GenBuilder {
    /// Map a chipi type name to a Rust type path.
    pub fn type_map(mut self, chipi_type: &str, rust_path: &str) -> Self {
        self.type_map
            .insert(chipi_type.to_string(), rust_path.to_string());
        self
    }

    /// Set the default dispatch strategy.
    pub fn dispatch_default(mut self, dispatch: Dispatch) -> Self {
        self.dispatch = dispatch;
        self
    }

    /// Set a per-decoder dispatch strategy override.
    pub fn dispatch_for(mut self, decoder_name: &str, dispatch: Dispatch) -> Self {
        self.dispatch_overrides
            .insert(decoder_name.to_string(), dispatch);
        self
    }

    /// Enable running `rustfmt` on the generated output.
    pub fn format(mut self, yes: bool) -> Self {
        self.format = yes;
        self
    }

    /// Set the output file path.
    pub fn output(mut self, path: impl Into<String>) -> Self {
        self.output = Some(path.into());
        self
    }

    /// Run the code generation pipeline.
    pub fn run(self) -> Result<(), Box<dyn std::error::Error>> {
        let output = self.output.as_deref().ok_or("output path not set")?;
        let target = chipi_core::config::GenTarget {
            input: self.input,
            lang: "rust".to_string(),
            output: output.to_string(),
            format: self.format,
            dispatch: self.dispatch,
            dispatch_overrides: self.dispatch_overrides,
            type_map: self.type_map,
            lang_options: None,
        };
        run_gen_target(&target)
    }
}

// ---------------------------------------------------------------------------
// Emulator dispatch LUT generation builder
// ---------------------------------------------------------------------------

/// Start building an emulator dispatch LUT generation target.
pub fn lut(input: impl Into<String>) -> LutBuilder {
    LutBuilder {
        input: input.into(),
        output: None,
        handler_mod: String::new(),
        ctx_type: String::new(),
        dispatch: Dispatch::FnPtrLut,
        groups: HashMap::new(),
        lut_mod: None,
        instr_type: None,
        raw_expr: None,
        instr_type_output: None,
    }
}

/// Builder for emulator dispatch LUT generation.
pub struct LutBuilder {
    input: String,
    output: Option<String>,
    handler_mod: String,
    ctx_type: String,
    dispatch: Dispatch,
    groups: HashMap<String, Vec<String>>,
    lut_mod: Option<String>,
    instr_type: Option<String>,
    raw_expr: Option<String>,
    instr_type_output: Option<String>,
}

impl LutBuilder {
    pub fn output(mut self, path: impl Into<String>) -> Self {
        self.output = Some(path.into());
        self
    }

    pub fn handler_mod(mut self, m: impl Into<String>) -> Self {
        self.handler_mod = m.into();
        self
    }

    pub fn ctx_type(mut self, t: impl Into<String>) -> Self {
        self.ctx_type = t.into();
        self
    }

    pub fn dispatch(mut self, strategy: Dispatch) -> Self {
        self.dispatch = strategy;
        self
    }

    pub fn group(
        mut self,
        name: impl Into<String>,
        instrs: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        self.groups
            .insert(name.into(), instrs.into_iter().map(|s| s.into()).collect());
        self
    }

    pub fn lut_mod(mut self, path: impl Into<String>) -> Self {
        self.lut_mod = Some(path.into());
        self
    }

    pub fn instr_type(mut self, t: impl Into<String>) -> Self {
        self.instr_type = Some(t.into());
        self
    }

    pub fn raw_expr(mut self, expr: impl Into<String>) -> Self {
        self.raw_expr = Some(expr.into());
        self
    }

    /// Also generate an instruction newtype with field accessors.
    pub fn instr_type_output(mut self, path: impl Into<String>) -> Self {
        self.instr_type_output = Some(path.into());
        self
    }

    /// Run the LUT generation pipeline.
    pub fn run(self) -> Result<(), Box<dyn std::error::Error>> {
        let output = self.output.as_deref().ok_or("output path not set")?;
        let target = chipi_core::config::LutTarget {
            input: self.input,
            output: output.to_string(),
            handler_mod: self.handler_mod,
            ctx_type: self.ctx_type,
            dispatch: self.dispatch,
            groups: self.groups,
            lut_mod: self.lut_mod,
            instr_type: self.instr_type,
            raw_expr: self.raw_expr,
            instr_type_output: self.instr_type_output,
        };
        run_lut_target(&target)
    }
}
