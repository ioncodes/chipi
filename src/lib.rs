//! # chipi
//!
//! Generate instruction decoders and disassemblers from `.chipi` files.
//!
//! Write your CPU instruction encoding in a simple DSL, and chipi generates
//! the Rust decoder and formatting code for you.
//!
//! ## Usage
//!
//! Add to `Cargo.toml`:
//!
//! ```toml
//! [build-dependencies]
//! chipi = "0.5.3"
//! ```
//!
//! Create `build.rs`:
//!
//! ```ignore
//! use std::env;
//! use std::path::PathBuf;
//!
//! fn main() {
//!     let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
//!     chipi::generate("cpu.chipi", out_dir.join("cpu.rs").to_str().unwrap())
//!         .expect("failed to generate decoder");
//!     println!("cargo:rerun-if-changed=cpu.chipi");
//! }
//! ```
//!
//! Use the generated decoder:
//!
//! ```ignore
//! mod cpu {
//!     include!(concat!(env!("OUT_DIR"), "/cpu.rs"));
//! }
//!
//! // decode() always takes &[u8] and returns (instruction, bytes_consumed)
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
//! type simm24 = i32 { sign_extend(24), shift_left(2) }
//!
//! bx   [0:5]=010010 li:simm24[6:29] aa:bool[30] lk:bool[31]
//!      | "b{lk ? l}{aa ? a} {li:#x}"
//!
//! addi [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:simm16[16:31]
//!      | ra == 0: "li {rd}, {simm}"
//!      | "addi {rd}, {ra}, {simm}"
//! ```
//!
//! ## Syntax
//!
//! ### Decoder block
//!
//! ```text
//! decoder Name {
//!     width = 32        # 8, 16, or 32 bits
//!     bit_order = msb0  # msb0 or lsb0
//!     endian = big      # big or little (default: big)
//!     max_units = 4     # optional: safety guard (validates bit ranges)
//! }
//! ```
//!
//! #### Variable-Length Instructions
//!
//! chipi automatically generates variable-length decoders when you use bit positions
//! beyond `width-1`. Simply reference subsequent units in your bit ranges:
//!
//! ```text
//! decoder Dsp {
//!     width = 16
//!     bit_order = msb0
//!     endian = big
//!     max_units = 2     # Optional safety check: ensures bits don't exceed 32 (width * max_units)
//! }
//!
//! nop    [0:15]=0000000000000000        # 1 unit (16 bits)
//! lri    [0:10]=00000010000 rd:u5[11:15] imm:u16[16:31]  # 2 units (32 bits)
//! ```
//!
//! The generated `decode` always has the signature:
//! `pub fn decode(data: &[u8]) -> Option<(Self, usize)>`
//!
//! It accepts raw bytes and returns the decoded instruction along with the
//! number of bytes consumed.
//!
//! ### Instructions
//!
//! Each instruction is one line with a name, fixed bit patterns, and fields:
//!
//! ```text
//! add [0:5]=011111 rd:u8[6:10] ra:u8[11:15]
//! ```
//!
//! Fixed bits use `[range]=pattern`. Fields use `name:type[range]`.
//!
//! #### Wildcard Bits
//!
//! Use `?` in bit patterns for bits that can be any value:
//!
//! ```text
//! # Match when bits [15:8] are 0x8c, bits [7:0] can be anything
//! clr15   [15:0]=10001100????????
//!         | "CLR15"
//!
//! # Mix wildcards with specific bits
//! nop     [7:4]=0000 [3:0]=????
//!         | "nop"
//! ```
//!
//! Wildcard bits are excluded from the matching mask, so instructions match
//! regardless of the values in those positions. This is useful for reserved or
//! architecturally undefined bits.
//!
//! #### Overlapping Patterns
//!
//! chipi supports overlapping instruction patterns where one pattern is a subset of another.
//! More specific patterns (with more fixed bits) are checked first:
//!
//! ```text
//! # Generic instruction - matches 0x1X (any value in bits 4-7)
//! load  [0:3]=0001 reg:u4[4:7]
//!       | "load r{reg}"
//!
//! # Specific instruction - matches only 0x1F
//! load_max [0:3]=0001 [4:7]=1111
//!          | "load rmax"
//! ```
//!
//! The decoder will check `load_max` first (all bits fixed), then fall back to `load`
//! (bits 4-7 are wildcards). This works across all units in variable-length decoders.
//!
//! ### Types
//!
//! Builtin types:
//! * `bool` (converts bit to true/false)
//! * `u1` to `u7` (maps to u8)
//! * `u8`, `u16`, `u32`
//! * `i8`, `i16`, `i32`
//!
//! Custom types:
//!
//! ```text
//! type simm = i32 { sign_extend(16) }
//! type reg = u8 as Register
//! ```
//!
//! Available transformations:
//! * `sign_extend(n)` - sign extend from n bits
//! * `zero_extend(n)` - zero extend from n bits
//! * `shift_left(n)` - shift left by n bits
//!
//! Display format hints (controls how the field is printed in format strings):
//! * `display(signed_hex)` - signed hex: `0x1A`, `-0x1A`, `0`
//! * `display(hex)` - unsigned hex: `0x1A`, `0`
//!
//! ### Imports
//!
//! Import Rust types to wrap extracted values:
//!
//! ```text
//! import crate::cpu::Register
//! import std::num::Wrapping
//! ```
//!
//! ### Format lines
//!
//! Format lines follow an instruction and define its disassembly output:
//!
//! ```text
//! bx [0:5]=010010 li:simm24[6:29] aa:bool[30] lk:bool[31]
//!    | "b{lk ? l}{aa ? a} {li:#x}"
//! ```
//!
//! Features:
//! * `{field}` - insert field value, with optional format spec: `{field:#x}`
//! * `{field ? text}` - emit `text` if nonzero, `{field ? yes : no}` for else
//! * `{a + b * 4}` - inline arithmetic (`+`, `-`, `*`, `/`, `%`)
//! * `{-field}` - unary negation
//! * `{map_name(arg)}` - call a map lookup
//! * `{rotate_right(val, amt)}` - builtin functions
//! * Guards: `| ra == 0: "li {rd}, {simm}"` - conditional format selection
//! * Guard arithmetic: `| sh == 32 - mb : "srwi ..."` - arithmetic in guard operands
//!
//! ### Maps
//!
//! Lookup tables for use in format strings:
//!
//! ```text
//! map spr_name(spr) {
//!     1 => "xer"
//!     8 => "lr"
//!     9 => "ctr"
//!     _ => "???"
//! }
//! ```
//!
//! ### Formatting trait
//!
//! chipi generates a `{Name}Format` trait with one method per instruction.
//! Default implementations come from format lines. Override selectively:
//!
//! ```ignore
//! struct MyFormat;
//! impl cpu::CpuFormat for MyFormat {
//!     fn fmt_bx(li: i32, aa: bool, lk: bool,
//!               f: &mut std::fmt::Formatter) -> std::fmt::Result {
//!         write!(f, "BRANCH {:#x}", li)
//!     }
//! }
//!
//! println!("{}", instr.display::<MyFormat>());
//! ```
//!
//! ## Emulator LUT
//!
//! chipi can generate a function-pointer **lookup table** for emulator dispatch.
//! Each opcode is routed directly to a handler function via static `[Handler; N]`
//! arrays derived from the same decision tree.
//!
//! ### build.rs
//!
//! Use [`LutBuilder`] to configure and emit both the LUT and the handler stubs:
//!
//! ```ignore
//! use std::env;
//! use std::path::PathBuf;
//!
//! fn main() {
//!     let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
//!     let manifest = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
//!     let spec = "cpu.chipi";
//!
//!     let builder = chipi::LutBuilder::new(spec)
//!         .handler_mod("crate::cpu::interpreter")
//!         .ctx_type("crate::Cpu");
//!
//!     // Regenerated every build, stays in sync with the spec
//!     builder
//!         .build_lut(out_dir.join("cpu_lut.rs").to_str().unwrap())
//!         .expect("failed to generate LUT");
//!
//!     // Written once, hand-edits are never overwritten
//!     let stubs = manifest.join("src/cpu/interpreter.rs");
//!     if !stubs.exists() {
//!         builder.build_stubs(stubs.to_str().unwrap())
//!             .expect("failed to generate stubs");
//!     }
//!
//!     println!("cargo:rerun-if-changed={spec}");
//! }
//! ```
//!
//! ### Include and dispatch
//!
//! ```ignore
//! // src/cpu.rs
//! #[allow(dead_code, non_upper_case_globals)]
//! pub mod lut {
//!     include!(concat!(env!("OUT_DIR"), "/cpu_lut.rs"));
//! }
//!
//! // fetch-decode-execute
//! let opcode = mem.read_u32(cpu.pc);
//! cpu.pc = cpu.pc.wrapping_add(4);
//! crate::cpu::lut::dispatch(&mut ctx, opcode);
//! ```
//!
//! ### Handler stubs
//!
//! On the first build, `build_stubs` writes `src/cpu/interpreter.rs` with
//! `todo!()` bodies. Replace each `todo!()` as you go; the file is never
//! regenerated so hand-edits are safe.
//!
//! The second parameter type is derived from the spec's `width`:
//! `u8` (8-bit), `u16` (16-bit), or `u32` (32-bit).
//!
//! ```ignore
//! pub fn addi(_ctx: &mut crate::Cpu, _opcode: u32) { todo!("addi") }
//! pub fn lwz(_ctx: &mut crate::Cpu, _opcode: u32) { todo!("lwz")  }
//! // ... one fn per instruction
//! ```
//!
//! ### Grouped handlers with const generics
//!
//! Use `.group()` to fold multiple instructions into one handler via a
//! `const OP: u32` generic parameter. Each LUT entry is a separate
//! monomorphization.
//!
//! Provide `.lut_mod()` so that generated stubs can `use` the `OP_*` constants:
//!
//! ```ignore
//! chipi::LutBuilder::new("cpu.chipi")
//!     .handler_mod("crate::cpu::interpreter")
//!     .ctx_type("crate::Cpu")
//!     .lut_mod("crate::cpu::lut")
//!     .group("alu", ["addi", "addis", "ori", "oris"])
//!     .build_lut(out_dir.join("cpu_lut.rs").to_str().unwrap())?;
//! ```
//!
//! ### Custom instruction wrapper type
//!
//! Use `.instr_type()` to replace the raw integer with a richer type.
//! chipi uses it in the generated `Handler` alias and all stub signatures.
//! `.raw_expr()` tells chipi how to extract the underlying integer for table
//! indexing; it defaults to `"instr.0"` for newtype wrappers.
//!
//! ```ignore
//! chipi::LutBuilder::new("cpu.chipi")
//!     .handler_mod("crate::cpu::interpreter")
//!     .ctx_type("crate::Cpu")
//!     .instr_type("crate::cpu::Instruction")  // struct Instruction(pub u32)
//!     // .raw_expr("instr.0")                 // default for newtype wrappers
//!     .build_lut(out_dir.join("cpu_lut.rs").to_str().unwrap())?;
//! ```
//!
//! Generated `Handler` type and stub signature:
//! ```ignore
//! pub type Handler = fn(&mut crate::Cpu, crate::cpu::Instruction);
//!
//! pub fn addi(_ctx: &mut crate::Cpu, _instr: crate::cpu::Instruction) { todo!("addi") }
//! ```
//!
//! ## Instruction Type Generation
//!
//! chipi can auto-generate the instruction newtype with field accessor methods,
//! eliminating the need to hand-write bit extraction code. This is useful in
//! cases where a thin wrapper for decoding is prefered (e.g. emulation).
//!
//! ### build.rs
//!
//! Add `.build_instr_type()` to your `LutBuilder` chain:
//!
//! ```ignore
//! chipi::LutBuilder::new("cpu.chipi")
//!     .instr_type("crate::cpu::Instruction")
//!     .build_instr_type(out_dir.join("instruction.rs").to_str().unwrap())?;
//! ```
//!
//! ### Generated output
//!
//! Creates a newtype with `#[inline]` accessor methods for every unique field:
//!
//! ```ignore
//! pub struct Instruction(pub u32);
//!
//! #[rustfmt::skip]
//! impl Instruction {
//!     #[inline] pub fn rd(&self) -> u8 { ((self.0 >> 21) & 0x1f) as u8 }
//!     #[inline] pub fn ra(&self) -> u8 { ((self.0 >> 16) & 0x1f) as u8 }
//!     #[inline] pub fn simm(&self) -> i32 { ((((self.0 >> 0) & 0xffff) as i32) << 16) >> 16 }
//!     #[inline] pub fn rc(&self) -> bool { (self.0 & 0x1) != 0 }
//!     // ... one accessor per unique field across all instructions
//! }
//! ```
//!
//! ### Usage
//!
//! Include the generated file and optionally add custom methods:
//!
//! ```ignore
//! // src/cpu/semantics.rs
//! include!(concat!(env!("OUT_DIR"), "/instruction.rs"));
//!
//! // Add custom accessors not derivable from the spec
//! impl Instruction {
//!     /// SPR field with swapped halves (PowerPC)
//!     pub fn spr_decoded(&self) -> u32 {
//!         let raw = self.spr();
//!         (raw >> 5) | ((raw & 0x1f) << 5)
//!     }
//! }
//! ```
//!
//! ### Conflict handling
//!
//! Fields with the same name but different bit ranges across instructions generate
//! separate accessors with bit range suffixes (e.g., `d_15_0()` and `d_11_0()`).
//! You can add convenience aliases in a separate `impl` block if needed.
//!
//! ## API
//!
//! ```ignore
//! // Parse and generate decoder from file
//! chipi::generate("cpu.chipi", "out.rs")?;
//!
//! // Generate decoder from source string
//! let code = chipi::generate_from_str(source, "cpu.chipi")?;
//!
//! // Step-by-step
//! let def = chipi::parse("cpu.chipi")?;
//! chipi::emit(&def, "out.rs")?;
//!
//! // Emulator LUT, simple
//! // (instr type auto-derived from spec width: u8 / u16 / u32)
//! chipi::generate_lut("cpu.chipi", "out/lut.rs", "crate::interp", "crate::Cpu")?;
//! chipi::generate_stubs("cpu.chipi", "src/interp.rs", "crate::Cpu")?; // once only
//!
//! // Instruction type generation
//! chipi::generate_instr_type("cpu.chipi", "out/instruction.rs", "Instruction")?;
//!
//! // Emulator LUT, full control via LutBuilder
//! chipi::LutBuilder::new("cpu.chipi")
//!     .handler_mod("crate::cpu::interpreter")
//!     .ctx_type("crate::Cpu")
//!     .lut_mod("crate::cpu::lut")              // needed when using groups
//!     .group("alu", ["addi", "addis"])         // const-generic shared handler
//!     .instr_type("crate::cpu::Instruction")   // optional wrapper type
//!     .build_lut("out/lut.rs")?
//!     .build_instr_type("out/instruction.rs")?;  // generate instruction type
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
    let source = fs::read_to_string(path)?;
    let filename = path
        .file_name()
        .and_then(|f| f.to_str())
        .unwrap_or(input);

    parser::parse(&source, filename).map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)
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
    let code = lut_gen::generate_lut_code(&validated, &t, handler_mod, ctx_type, &HashMap::new(), None, None);
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
