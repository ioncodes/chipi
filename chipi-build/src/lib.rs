//! Build-script helper for chipi code generation.
//!
//! Drives `*.bindings.chipi` files from a Rust project's `build.rs`.
//!
//! # Example
//!
//! ```ignore
//! // build.rs
//! fn main() {
//!     chipi_build::generate_bindings("specs/gekko.bindings.chipi")
//!         .expect("chipi codegen failed");
//! }
//! ```
//!
//! To select one target or decoder out of many:
//!
//! ```ignore
//! chipi_build::generate_bindings_target("specs/gekko.bindings.chipi", "rust")?;
//! chipi_build::generate_bindings_decoder("specs/dsp.bindings.chipi", "rust", "GcDsp")?;
//! ```

use std::path::Path;

use chipi_core::bindings::{self, RunMode, lower_resolved, parse_bindings_file, resolve};
use chipi_core::error::Errors;

/// Run every target and decoder in the bindings file.
pub fn generate_bindings(path: impl AsRef<Path>) -> Result<(), Box<dyn std::error::Error>> {
    run(path, None, None)
}

/// Run only the named target. Examples: `"rust"`, `"ida"`.
pub fn generate_bindings_target(
    path: impl AsRef<Path>,
    target: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    run(path, Some(target), None)
}

/// Run only the named decoder under the given target.
/// Accepts a decoder, dispatch, processor, or architecture name.
pub fn generate_bindings_decoder(
    path: impl AsRef<Path>,
    target: &str,
    decoder: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    run(path, Some(target), Some(decoder))
}

fn run(
    path: impl AsRef<Path>,
    target: Option<&str>,
    decoder: Option<&str>,
) -> Result<(), Box<dyn std::error::Error>> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map(std::path::PathBuf::from)
        .unwrap_or_default();
    let path = manifest_dir.join(path.as_ref());

    let parsed = parse_bindings_file(&path).map_err(wrap_errs)?;
    let resolved = resolve(parsed).map_err(wrap_errs)?;
    bindings::validate::validate(&resolved).map_err(wrap_errs)?;
    let lowered = lower_resolved(&resolved).map_err(wrap_errs)?;

    // Emit cargo:rerun-if-changed for every file we touched.
    println!("cargo:rerun-if-changed={}", path.display());
    for f in &resolved.all_files {
        println!("cargo:rerun-if-changed={}", f.display());
    }

    bindings::run::run_lowered(
        &resolved,
        &lowered,
        &path,
        target,
        decoder,
        RunMode::Generate,
    )
    .map_err(wrap_errs)?;

    Ok(())
}

fn wrap_errs(errs: Vec<chipi_core::error::Error>) -> Box<dyn std::error::Error> {
    Box::new(Errors(errs))
}
