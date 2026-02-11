//! # chipi
//!
//! Generate instruction decoders from `.chipi` files.
//!
//! Write your CPU instruction encoding in a simple DSL, and chipi generates
//! the Rust decoder code for you.
//!
//! ## Usage
//!
//! Add to `Cargo.toml`:
//!
//! ```toml
//! [build-dependencies]
//! chipi = "0.1"
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
//! match cpu::Instruction::decode(raw) {
//!     Some(instr) => println!("{:?}", instr),
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
//! }
//!
//! type simm16 = i32 { sign_extend(16) }
//!
//! add  [0:5]=011111 rd:u8[6:10] ra:u8[11:15] rb:u8[16:20] [21:30]=0100001010 oe:bool[31]
//! addi [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:simm16[16:31]
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
//! }
//! ```
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
//! ### Imports
//!
//! Import Rust types to wrap extracted values:
//!
//! ```text
//! import crate::cpu::Register
//! import std::num::Wrapping
//! ```
//!
//! ## API
//!
//! ```ignore
//! // Parse and generate from file
//! chipi::generate("cpu.chipi", "out.rs")?;
//!
//! // Generate from string
//! let code = chipi::generate_from_str(source, "cpu.chipi")?;
//!
//! // Step by step
//! let def = chipi::parse("cpu.chipi")?;
//! chipi::emit(&def, "out.rs")?;
//! ```

pub mod codegen;
pub mod error;
pub mod parser;
pub mod tree;
pub mod types;
pub mod validate;

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
