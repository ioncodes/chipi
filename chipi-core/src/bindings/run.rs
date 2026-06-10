//! Execute lowered bindings.
//!
//! Drives the codegen backends to write output files. Or prints a
//! human-readable preview.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::backend::{self, run_formatter};
use crate::config::{
    self, BinjaOptions, GenTarget, IdaOptions, LangOptions, LutTarget, resolve_gen_paths,
    resolve_lut_paths,
};
use crate::error::{Error, ErrorKind, Errors, Span};
use crate::instr_gen;
use crate::lut_gen::{self, generate_lut_code, generate_subdecoder_flat_dispatch};
use crate::tree;
use crate::types::ValidatedDef;
use crate::validate::validate as validate_spec;

use super::lower::{LoweredBindings, LoweredItem, LoweredKind, lower_resolved};
use super::parser::parse_file_with_includes;
use super::resolve::{ResolvedBindings, resolve};
use super::types::{TargetBinding, TargetKind};
use super::validate::validate as validate_bindings;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunMode {
    Generate,
    Preview,
    Check,
}

/// One-shot pipeline. Runs parse, resolve, validate, lower, and execute.
pub fn run(
    bindings_path: &Path,
    target_filter: Option<&str>,
    decoder_filter: Option<&str>,
    mode: RunMode,
) -> Result<(), Vec<Error>> {
    let parsed = parse_file_with_includes(bindings_path)?;
    let resolved = resolve(parsed)?;
    validate_bindings(&resolved)?;
    let lowered = lower_resolved(&resolved)?;
    run_lowered(
        &resolved,
        &lowered,
        bindings_path,
        target_filter,
        decoder_filter,
        mode,
    )
}

/// Run a pre-resolved and pre-lowered set of items.
/// The CLI uses this so it can also print resolution and dependency lists.
pub fn run_lowered(
    resolved: &ResolvedBindings,
    lowered: &LoweredBindings,
    bindings_path: &Path,
    target_filter: Option<&str>,
    decoder_filter: Option<&str>,
    mode: RunMode,
) -> Result<(), Vec<Error>> {
    if mode == RunMode::Check {
        // Validation already ran; nothing else to do.
        return Ok(());
    }

    // Filter by target
    let target_kind = match target_filter {
        Some(t) => Some(parse_target_kind(t)?),
        None => None,
    };
    let target_kinds_present: Vec<TargetKind> =
        resolved.file.targets.iter().map(|t| t.kind).collect();
    if target_kind.is_none() && distinct(&target_kinds_present).len() > 1 {
        return Err(vec![Error::new(
            ErrorKind::MultipleTargetsAmbiguous(
                distinct(&target_kinds_present)
                    .iter()
                    .map(|k| k.name().to_string())
                    .collect(),
            ),
            Span::new(&bindings_path.display().to_string(), 1, 1, 0),
        )]);
    }

    let base_dir = bindings_path
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));

    let filtered: Vec<&LoweredItem> = lowered
        .items
        .iter()
        .filter(|it| target_kind.map_or(true, |k| k == it.target_kind))
        .filter(|it| decoder_filter.map_or(true, |d| d == it.decoder_name))
        .collect();

    if filtered.is_empty() {
        return Err(vec![Error::new(
            ErrorKind::BindingsParse(format!(
                "no targets match {}{}",
                target_filter
                    .map(|s| format!("--target {} ", s))
                    .unwrap_or_default(),
                decoder_filter
                    .map(|s| format!("--decoder {}", s))
                    .unwrap_or_default()
            )),
            Span::new(&bindings_path.display().to_string(), 1, 1, 0),
        )]);
    }

    // Decoder ambiguity: if a decoder filter is needed for the requested
    // target but absent and multiple decoders exist
    if decoder_filter.is_none() {
        let names: Vec<String> = filtered.iter().map(|i| i.decoder_name.clone()).collect();
        if distinct(&names).len() > 1
            && lowered.items.len() > filtered.len()
            && target_kind.is_none()
        {
            // multi-target multi-decoder -> ambiguous targets already handled
        } else if distinct(&names).len() > 1 && target_kind.is_some() {
            return Err(vec![Error::new(
                ErrorKind::MultipleDecodersAmbiguous(distinct(&names)),
                Span::new(&bindings_path.display().to_string(), 1, 1, 0),
            )]);
        }
    }

    for item in &filtered {
        match &item.kind {
            LoweredKind::Gen(g) => run_gen(g, &base_dir, mode)?,
            LoweredKind::Lut(l) => run_lut(l, &base_dir, mode)?,
            LoweredKind::InstrType {
                input,
                output,
                struct_name,
                subdecoder,
            } => run_instr_type(input, output, struct_name, subdecoder.as_deref(), &base_dir)?,
        }
    }

    Ok(())
}

fn parse_target_kind(name: &str) -> Result<TargetKind, Vec<Error>> {
    Ok(match name {
        "rust" => TargetKind::Rust,
        "cpp" | "c++" => TargetKind::Cpp,
        "ida" => TargetKind::Ida,
        "binja" => TargetKind::Binja,
        other => {
            return Err(vec![Error::new(
                ErrorKind::UnknownTargetKind(other.to_string()),
                Span::new("<cli>", 1, 1, 0),
            )]);
        }
    })
}

fn distinct<T: Clone + Eq>(v: &[T]) -> Vec<T> {
    let mut out: Vec<T> = Vec::new();
    for x in v {
        if !out.iter().any(|y| y == x) {
            out.push(x.clone());
        }
    }
    out
}

fn run_gen(target: &GenTarget, base_dir: &Path, mode: RunMode) -> Result<(), Vec<Error>> {
    let mut t = target.clone();
    resolve_gen_paths(&mut t, base_dir);

    if mode == RunMode::Preview {
        print_gen_preview(&t);
        return Ok(());
    }

    let (def, _deps) = crate::parser::parse_file_with_deps(Path::new(&t.input))?;
    let validated = validate_spec(&def)?;
    let backend = backend::get_backend(&t.lang).ok_or_else(|| {
        vec![Error::new(
            ErrorKind::BindingsParse(format!("unknown backend '{}'", t.lang)),
            Span::new("<lower>", 0, 0, 0),
        )]
    })?;
    let code = backend.generate(&validated, &t).map_err(|e| {
        vec![Error::new(
            ErrorKind::BindingsParse(format!("codegen error: {}", e)),
            Span::new("<lower>", 0, 0, 0),
        )]
    })?;
    std::fs::write(&t.output, code).map_err(|e| {
        vec![Error::new(
            ErrorKind::BindingsParse(format!("failed to write {}: {}", t.output, e)),
            Span::new("<lower>", 0, 0, 0),
        )]
    })?;
    if t.format {
        if let Some(cmd) = backend.formatter_command() {
            run_formatter(cmd, &t.output);
        }
    }
    Ok(())
}

fn run_lut(target: &LutTarget, base_dir: &Path, mode: RunMode) -> Result<(), Vec<Error>> {
    let mut t = target.clone();
    resolve_lut_paths(&mut t, base_dir);

    if mode == RunMode::Preview {
        print_lut_preview(&t);
        return Ok(());
    }

    let (def, _deps) = crate::parser::parse_file_with_deps(Path::new(&t.input))?;
    let validated = validate_spec(&def)?;
    let tree = tree::build_tree(&validated);

    // Build group map (instr_name -> handler_fn_name)
    let mut instr_to_group: HashMap<String, String> = HashMap::new();
    for (group, instrs) in &t.groups {
        for instr in instrs {
            instr_to_group.insert(instr.clone(), group.clone());
        }
    }

    let mut code = generate_lut_code(
        &validated,
        &tree,
        &t.handler_mod,
        &t.ctx_type,
        &instr_to_group,
        t.instr_type.as_deref(),
        t.raw_expr.as_deref(),
        t.dispatch,
        t.invalid_handler.as_deref(),
        &t.handler_consts,
    )?;

    // Sub-decoder dispatch generation
    for sd in &validated.sub_decoders {
        let strategy = t
            .subdecoder_dispatch
            .get(&sd.name)
            .copied()
            .unwrap_or(t.dispatch);
        let sd_handler_mod = t
            .subdecoder_handler_mods
            .get(&sd.name)
            .cloned()
            .unwrap_or_else(|| t.handler_mod.clone());
        let sd_invalid = t
            .subdecoder_invalid_handlers
            .get(&sd.name)
            .cloned()
            .or_else(|| t.invalid_handler.clone());
        let sd_groups: HashMap<String, String> = t
            .subdecoder_groups
            .get(&sd.name)
            .map(|g| {
                let mut m = HashMap::new();
                for (group, instrs) in g {
                    for instr in instrs {
                        m.insert(instr.clone(), group.clone());
                    }
                }
                m
            })
            .unwrap_or_default();
        let sd_instr_type = t.subdecoder_instr_types.get(&sd.name).cloned();

        match strategy {
            config::Dispatch::FlatLut | config::Dispatch::FlatMatch => {
                code.push('\n');
                let block = generate_subdecoder_flat_dispatch(
                    sd,
                    &sd_handler_mod,
                    &t.ctx_type,
                    &sd_groups,
                    sd_instr_type.as_deref(),
                    sd_invalid.as_deref(),
                    strategy,
                    &t.handler_consts,
                )?;
                code.push_str(&block);
            }
            _ => {
                // Existing wide-match flat-style dispatch (precedence-based).
                if !sd_groups.is_empty() || t.subdecoder_dispatch.contains_key(&sd.name) {
                    code.push('\n');
                    code.push_str(&lut_gen::generate_subdecoder_dispatch(
                        &validated,
                        sd,
                        &sd_handler_mod,
                        &t.ctx_type,
                        &sd_groups,
                        sd_instr_type.as_deref(),
                        &t.handler_consts,
                    ));
                }
            }
        }
    }

    std::fs::write(&t.output, code).map_err(|e| {
        vec![Error::new(
            ErrorKind::BindingsParse(format!("failed to write {}: {}", t.output, e)),
            Span::new("<lower>", 0, 0, 0),
        )]
    })?;

    // Optional instruction-type newtype output
    if let Some(it_out) = &t.instr_type_output {
        let struct_name = t
            .instr_type
            .as_deref()
            .and_then(|p| p.rsplit("::").next())
            .unwrap_or("Instruction");
        let (it_code, _warnings) = instr_gen::generate_instr_type(&validated, struct_name);
        std::fs::write(it_out, it_code).map_err(|e| {
            vec![Error::new(
                ErrorKind::BindingsParse(format!("failed to write {}: {}", it_out, e)),
                Span::new("<lower>", 0, 0, 0),
            )]
        })?;
    }

    // Sub-decoder instr-type newtype outputs
    for (sd_name, out) in &t.subdecoder_instr_type_outputs {
        let sd = validated
            .sub_decoders
            .iter()
            .find(|s| &s.name == sd_name)
            .ok_or_else(|| {
                vec![Error::new(
                    ErrorKind::UnknownDecoderInBinding {
                        name: sd_name.clone(),
                        suggestion: None,
                    },
                    Span::new("<lower>", 0, 0, 0),
                )]
            })?;
        let (it_code, _warnings) = instr_gen::generate_subdecoder_instr_type(sd, sd_name);
        std::fs::write(out, it_code).map_err(|e| {
            vec![Error::new(
                ErrorKind::BindingsParse(format!("failed to write {}: {}", out, e)),
                Span::new("<lower>", 0, 0, 0),
            )]
        })?;
    }

    Ok(())
}

fn run_instr_type(
    input: &Path,
    output: &str,
    struct_name: &str,
    subdecoder: Option<&str>,
    base_dir: &Path,
) -> Result<(), Vec<Error>> {
    let _ = base_dir;
    let (def, _) = crate::parser::parse_file_with_deps(input)?;
    let validated = validate_spec(&def)?;
    let (code, _warnings) = match subdecoder {
        Some(name) => {
            let sd = validated
                .sub_decoders
                .iter()
                .find(|s| s.name == name)
                .ok_or_else(|| {
                    vec![Error::new(
                        ErrorKind::UnknownDecoderInBinding {
                            name: name.to_string(),
                            suggestion: None,
                        },
                        Span::new("<instr-type>", 0, 0, 0),
                    )]
                })?;
            instr_gen::generate_subdecoder_instr_type(sd, struct_name)
        }
        None => instr_gen::generate_instr_type(&validated, struct_name),
    };
    std::fs::write(output, code).map_err(|e| {
        vec![Error::new(
            ErrorKind::BindingsParse(format!("failed to write {}: {}", output, e)),
            Span::new("<instr-type>", 0, 0, 0),
        )]
    })?;
    Ok(())
}

fn print_gen_preview(t: &GenTarget) {
    println!("target: {}", t.lang);
    println!();
    println!("decoder:");
    println!("  input: {}", t.input);
    println!("  output: {}", t.output);
    let mut keys: Vec<&String> = t.type_map.keys().collect();
    keys.sort();
    for k in keys {
        println!("  type {} -> {}", k, t.type_map[k]);
    }
    match &t.lang_options {
        LangOptions::Ida(o) => print_ida_preview(o),
        LangOptions::Binja(o) => print_binja_preview(o),
        _ => {}
    }
    println!();
}

fn print_ida_preview(o: &IdaOptions) {
    println!();
    println!("processor:");
    println!("  name: {}", o.processor_name);
    println!("  long name: {}", o.processor_long_name);
    println!("  id: {:#x}", o.processor_id);
    println!("  address size: {}", o.address_size);
    println!("  bytes per unit: {}", o.bytes_per_unit);
    println!();
    println!("registers:");
    for r in &o.register_names {
        println!("  {}", r);
    }
    if !o.segment_registers.is_empty() {
        println!();
        println!("segment registers:");
        for r in &o.segment_registers {
            println!("  {}", r);
        }
    }
    println!();
    println!("flow:");
    println!("  calls: {}", o.flow.calls.join(", "));
    println!("  returns: {}", o.flow.returns.join(", "));
    println!("  stops: {}", o.flow.stops.join(", "));
}

fn print_binja_preview(o: &BinjaOptions) {
    println!();
    println!("architecture:");
    println!("  name: {}", o.architecture_name);
    println!("  address size: {}", o.address_size);
    println!("  default int size: {}", o.default_int_size);
    println!("  endianness: {}", o.endianness);
    println!();
    println!("registers:");
    for r in &o.register_names {
        println!("  {}", r);
    }
}

fn print_lut_preview(t: &LutTarget) {
    println!("dispatch:");
    println!("  input: {}", t.input);
    println!("  output: {}", t.output);
    println!("  context: {}", t.ctx_type);
    println!("  handlers: {}", t.handler_mod);
    println!("  strategy: {:?}", t.dispatch);
    if let Some(ih) = &t.invalid_handler {
        println!("  invalid handler: {}", ih);
    }
    if let Some(it) = &t.instr_type {
        println!("  instruction_type: {}", it);
    }
    if !t.groups.is_empty() {
        println!();
        println!("handlers:");
        let mut keys: Vec<&String> = t.groups.keys().collect();
        keys.sort();
        for g in keys {
            for instr in &t.groups[g] {
                println!(
                    "  {} -> {}::{}::<{{ OP_{} }}>",
                    instr,
                    t.handler_mod,
                    g,
                    instr.to_uppercase()
                );
            }
        }
    }
    println!();
}

// Re-export helpers used from validate
fn _unused(_: &TargetBinding, _: &ValidatedDef, _: Errors) {}
