//! # chipi
//!
//! Generate instruction decoders and disassemblers from `.chipi` files.
//!
//! Write your CPU instruction encoding in a simple DSL, and chipi generates
//! the Rust decoder and formatting code for you.
//!
//! ## Usage
//!
//! ```ignore
//! // build.rs
//! chipi::generate("cpu.chipi", out_dir.join("cpu.rs").to_str().unwrap())?;
//!
//! // Or with type mappings and sub-decoder control:
//! chipi::CodegenBuilder::new("dsp.chipi")
//!     .type_map("reg5", "crate::dsp::DspReg")
//!     .decoder_dispatch("GcDspExt", chipi::Dispatch::JumpTable)
//!     .output(out_dir.join("dsp.rs").to_str().unwrap())
//!     .run()?;
//! ```
//!
//! ```ignore
//! // Use the generated decoder
//! mod cpu {
//!     include!(concat!(env!("OUT_DIR"), "/cpu.rs"));
//! }
//!
//! match cpu::CpuInstruction::decode(&data[offset..]) {
//!     Some((instr, bytes)) => {
//!         println!("{}", instr);
//!         offset += bytes;
//!     }
//!     None => println!("invalid instruction"),
//! }
//! ```
//!
//! ## Example .chipi file
//!
//! ```text
//! decoder Cpu {
//!     width = 32
//!     bit_order = msb0
//!     endian = big
//! }
//!
//! type simm16 = i32 { sign_extend(16) }
//!
//! addi [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:simm16[16:31]
//!      | ra == 0: "li {rd}, {simm}"
//!      | "addi {rd}, {ra}, {simm}"
//! ```
//!
//! ## DSL Features
//!
//! - **Variable-length instructions**: bit positions beyond `width-1` reference subsequent units
//! - **Custom types**: `type simm16 = i32 { sign_extend(16) }` with transforms and display hints
//! - **Type mappings**: [`CodegenBuilder::type_map`] maps chipi types to Rust wrapper types
//! - **Includes**: `include "other.chipi"` pulls in definitions from other files
//! - **Sub-decoders**: `subdecoder Name { ... }` for inline decoding of bit-fields
//! - **Wildcard bits**: `?` in patterns for wildcard bits
//! - **Overlapping patterns**: more specific patterns are checked first
//! - **Format strings**: `{field}`, `{field ? yes : no}`, `{a + b}`, `{map(arg)}`
//! - **Sub-decoder fragments**: `{ext.mnemonic}` accesses named output fragments
//! - **Guards**: `| ra == 0: "li {rd}, {simm}"` for conditional formatting
//! - **Maps**: lookup tables for format strings
//! - **Formatting trait**: override per-instruction display via a generated trait
//!
//! ## Sub-decoders
//!
//! Sub-decoders decode a bit-field within a parent instruction using a separate
//! dispatch table, producing named string fragments that merge into the parent's output.
//!
//! ```text
//! # ext.chipi
//! subdecoder Ext {
//!     width = 8
//!     bit_order = msb0
//! }
//!
//! ext_dr  [0:5]=000001 r:u8[6:7]
//!         | .mnemonic = "'DR"
//!         | .operands = " : $ar{r}"
//!
//! ext_nop [0:5]=000000 [6:7]=??
//!         | .mnemonic = ""
//!         | .operands = ""
//! ```
//!
//! ```text
//! # main.chipi
//! include "ext.chipi"
//!
//! decoder Main {
//!     width = 16
//!     bit_order = msb0
//! }
//!
//! addr [0:7]=01000000 ext:Ext[8:15]
//!      | "ADDR{ext.mnemonic}{ext.operands}"
//! ```
//!
//! ## Dispatch Strategies
//!
//! Both [`CodegenBuilder::decoder_dispatch`] (for sub-decoders) and
//! [`LutBuilder::dispatch`] (for emulator interpreters) accept a [`Dispatch`] strategy:
//!
//! - [`Dispatch::FnPtrLut`] (default): `static [fn; N]` arrays with indirect calls.
//!   Fast to compile, good general-purpose default.
//! - [`Dispatch::JumpTable`]: `#[inline(always)]` nested match statements that call
//!   handlers directly. When handlers are also `#[inline(always)]`, the compiler can
//!   flatten the entire dispatch + execution chain (hyperinlining).
//!
//! ## Emulator Dispatch
//!
//! [`LutBuilder`] generates dispatch code for emulator interpreters.
//! Use `.dispatch(Dispatch::JumpTable)` for hyperinlining.
//! See the crate-level README for the full pattern.
//!
//! ## API
//!
//! ```ignore
//! // Simple: parse and generate
//! chipi::generate("cpu.chipi", "out.rs")?;
//!
//! // With type maps and dispatch control
//! chipi::CodegenBuilder::new("dsp.chipi")
//!     .type_map("reg5", "crate::dsp::DspReg")
//!     .decoder_dispatch("Ext", chipi::Dispatch::JumpTable)
//!     .output("out.rs")
//!     .run()?;
//!
//! // Emulator dispatch (fn ptr LUT, default)
//! chipi::LutBuilder::new("cpu.chipi")
//!     .handler_mod("crate::cpu::interpreter")
//!     .ctx_type("crate::Cpu")
//!     .group("alu", ["addi", "addis"])
//!     .build_lut("out/lut.rs")?;
//!
//! // Emulator dispatch (jump table, hyperinlining)
//! chipi::LutBuilder::new("cpu.chipi")
//!     .handler_mod("crate::cpu::interpreter")
//!     .ctx_type("crate::Cpu")
//!     .dispatch(chipi::Dispatch::JumpTable)
//!     .build_lut("out/lut.rs")?;
//! ```

pub mod codegen;
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
    let code = codegen::generate_code(&validated, &tree);

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

/// Generate handler stub functions for every instruction in a `.chipi` spec.
///
/// Each stub has the form:
/// ```rust,ignore
/// pub fn twi(_ctx: &mut Ctx, _opcode: u32) { todo!("twi") }
/// ```
///
/// Intended to be run **once** to bootstrap an interpreter module. After that,
/// replace `todo!()` bodies with real implementations as you go.
pub fn generate_stubs(
    input: &str,
    output: &str,
    ctx_type: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let def = parse(input)?;
    let validated = validate::validate(&def)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;
    let code = lut_gen::generate_stubs_code(&validated, ctx_type, &HashMap::new(), None, None);
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
/// if !stubs.exists() {
///     chipi::LutBuilder::new("cpu.chipi")
///         .ctx_type("crate::Cpu")
///         .lut_mod("crate::cpu::lut")
///         .group("alu", ["addi", "addis", "ori", "oris"])
///         .group("mem", ["lwz", "stw", "lbz", "stb"])
///         .build_stubs(stubs.to_str().unwrap())?;
/// }
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

    /// Generate the LUT source file.
    pub fn build_lut(&self, output: &str) -> Result<(), Box<dyn std::error::Error>> {
        let def = parse(&self.input)?;
        let validated = validate::validate(&def)
            .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;
        let t = tree::build_tree(&validated);
        let code = lut_gen::generate_lut_code(
            &validated,
            &t,
            &self.handler_mod,
            &self.ctx_type,
            &self.instr_to_group,
            self.instr_type.as_deref(),
            self.raw_expr.as_deref(),
            self.dispatch,
        );
        fs::write(output, code)?;
        Ok(())
    }

    /// Generate handler stubs source file.
    pub fn build_stubs(&self, output: &str) -> Result<(), Box<dyn std::error::Error>> {
        let def = parse(&self.input)?;
        let validated = validate::validate(&def)
            .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;
        let code = lut_gen::generate_stubs_code(
            &validated,
            &self.ctx_type,
            &self.group_to_instrs,
            self.lut_mod.as_deref(),
            self.instr_type.as_deref(),
        );
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
    let code = codegen::generate_code(&validated, &tree);

    Ok(code)
}

/// Dispatch strategy for code generation.
///
/// Controls how decoders, sub-decoders, and emulator LUTs dispatch to handlers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Dispatch {
    /// Generate an `#[inline(always)]` match statement and `#[inline(always)]`
    /// handler functions. The compiler can inline the entire sub-decoder dispatch
    /// into the parent, producing optimal code for emulator hot paths.
    /// Can cause slow compilation when the sub-decoder is referenced by many
    /// parent instructions.
    JumpTable,
    /// Generate a `static [Option<fn>; N]` array indexed by the raw field value.
    /// Each handler is a regular (non-inlined) function. Fast to compile, good
    /// default for disassemblers and large dispatch tables.
    #[default]
    FnPtrLut,
}

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
        let validated =
            validate::validate_with_options(&def, &self.type_maps, &self.dispatch_overrides)
                .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

        let tree = tree::build_tree(&validated);
        let code = codegen::generate_code(&validated, &tree);

        if let Some(ref output) = self.output {
            fs::write(output, code)?;
        }

        Ok(())
    }
}
