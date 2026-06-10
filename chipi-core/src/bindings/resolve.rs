//! Bindings include resolution.
//!
//! Parses every `*.chipi` spec referenced by the bindings file. Resolves
//! includes transitively. Offers lookup by decoder name.

use std::collections::HashMap;
use std::path::PathBuf;

use crate::error::Error;
use crate::parser::parse_file_with_deps;
use crate::types::{ValidatedDef, ValidatedSubDecoder};
use crate::validate::validate;

use super::types::BindingsFile;

/// A bindings file with all includes resolved and every spec parsed and
/// validated.
pub struct ResolvedBindings {
    pub file: BindingsFile,
    /// Every spec or bindings file touched. Used for
    /// `cargo:rerun-if-changed`.
    pub all_files: Vec<PathBuf>,
    /// One entry per included spec file. Each entry is post-validate.
    pub specs: Vec<ResolvedSpec>,
}

pub struct ResolvedSpec {
    pub path: PathBuf,
    pub def: ValidatedDef,
}

impl ResolvedBindings {
    /// Find the spec that defines a top-level `decoder` named `name`.
    /// Returns `None` if no spec defines it.
    pub fn find_decoder(&self, name: &str) -> Option<&ResolvedSpec> {
        self.specs.iter().find(|s| s.def.config.name == name)
    }

    /// Find the parent spec and sub-decoder where a sub-decoder named
    /// `name` is defined.
    pub fn find_subdecoder(&self, name: &str) -> Option<(&ResolvedSpec, &ValidatedSubDecoder)> {
        for spec in &self.specs {
            for sd in &spec.def.sub_decoders {
                if sd.name == name {
                    return Some((spec, sd));
                }
            }
        }
        None
    }

    /// Find a decoder by name. Accepts a top-level decoder or a sub-decoder.
    pub fn find_decoder_or_sub(
        &self,
        name: &str,
    ) -> Option<(&ResolvedSpec, Option<&ValidatedSubDecoder>)> {
        if let Some(s) = self.find_decoder(name) {
            return Some((s, None));
        }
        if let Some((s, sd)) = self.find_subdecoder(name) {
            return Some((s, Some(sd)));
        }
        None
    }

    /// Names of every reachable top-level decoder and sub-decoder.
    pub fn all_decoder_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        for spec in &self.specs {
            names.push(spec.def.config.name.clone());
            for sd in &spec.def.sub_decoders {
                names.push(sd.name.clone());
            }
        }
        names
    }
}

/// Resolve a parsed bindings file. Loads and validates every referenced
/// spec. Returns everything packaged together.
pub fn resolve(file: BindingsFile) -> Result<ResolvedBindings, Vec<Error>> {
    let mut all_files: Vec<PathBuf> = Vec::new();
    all_files.push(file.path.clone());
    for (p, _) in &file.bindings_includes {
        all_files.push(p.clone());
    }

    // De-duplicate spec includes by canonical path
    let mut seen: HashMap<PathBuf, ()> = HashMap::new();
    let mut specs: Vec<ResolvedSpec> = Vec::new();
    let mut errors: Vec<Error> = Vec::new();

    for (path, _span) in &file.spec_includes {
        if seen.contains_key(path) {
            continue;
        }
        seen.insert(path.clone(), ());

        let (def, deps) = match parse_file_with_deps(path) {
            Ok(v) => v,
            Err(errs) => {
                errors.extend(errs);
                continue;
            }
        };

        for dep in deps {
            if !all_files.contains(&dep) {
                all_files.push(dep);
            }
        }
        if !all_files.contains(path) {
            all_files.push(path.clone());
        }

        match validate(&def) {
            Ok(v) => specs.push(ResolvedSpec {
                path: path.clone(),
                def: v,
            }),
            Err(errs) => errors.extend(errs),
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(ResolvedBindings {
        file,
        all_files,
        specs,
    })
}
