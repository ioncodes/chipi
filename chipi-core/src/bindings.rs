//! `*.bindings.chipi` configuration format.
//!
//! The bindings frontend parses brace-based configuration files. These
//! files select decoders and dispatches from `.chipi` specs. They bind
//! them to per-language generation targets. The targets are Rust, C++,
//! IDA, and Binary Ninja. The frontend then lowers the result into the
//! internal `GenTarget` and `LutTarget` structures.

pub mod lower;
pub mod parser;
pub mod resolve;
pub mod run;
pub mod types;
pub mod validate;

pub use lower::{LoweredBindings, lower_resolved};
pub use parser::parse_file_with_includes as parse_bindings_file;
pub use resolve::{ResolvedBindings, resolve};
pub use run::{RunMode, run};
pub use types::*;
