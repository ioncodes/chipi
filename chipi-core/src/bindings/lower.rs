//! Lower a resolved bindings model into internal `GenTarget` and
//! `LutTarget` structures. Those are what the codegen pipeline consumes.

use std::collections::HashMap;
use std::path::PathBuf;

use crate::backend::{FlowConfig, OperandKind};
use crate::config::{
    BinjaOptions, CppGuardStyle, CppOptions, Dispatch, GenTarget, IdaOptions, LangOptions,
    LutTarget,
};
use crate::error::{Error, ErrorKind};

use super::resolve::ResolvedBindings;
use super::types::*;

/// One lowered target. The kind is either decoder-style codegen
/// (`GenTarget`) or emulator-dispatch (`LutTarget`). Each value keeps its
/// `target` and `decoder` keys so CLI filters can pick a subset.
pub struct LoweredItem {
    pub target_kind: TargetKind,
    pub decoder_name: String,
    pub kind: LoweredKind,
}

pub enum LoweredKind {
    Gen(GenTarget),
    Lut(LutTarget),
    /// Generate an instruction-type newtype only.
    /// Carries the output path and struct name. `subdecoder` is `Some`
    /// when the newtype is for a sub-decoder.
    InstrType {
        input: PathBuf,
        output: String,
        struct_name: String,
        subdecoder: Option<String>,
    },
}

pub struct LoweredBindings {
    pub items: Vec<LoweredItem>,
}

pub fn lower_resolved(resolved: &ResolvedBindings) -> Result<LoweredBindings, Vec<Error>> {
    let mut items: Vec<LoweredItem> = Vec::new();
    let mut errors: Vec<Error> = Vec::new();

    for target in &resolved.file.targets {
        match target.kind {
            TargetKind::Rust => {
                for d in &target.rust_decoders {
                    if let Err(mut errs) = lower_rust_decoder(resolved, d, &mut items) {
                        errors.append(&mut errs);
                    }
                }
                for d in &target.rust_dispatches {
                    if let Err(mut errs) = lower_rust_dispatch(resolved, d, None, &mut items) {
                        errors.append(&mut errs);
                    }
                }
            }
            TargetKind::Cpp => {
                for d in &target.cpp_decoders {
                    if let Err(mut errs) = lower_cpp_decoder(resolved, d, &mut items) {
                        errors.append(&mut errs);
                    }
                }
            }
            TargetKind::Ida => {
                for p in &target.ida_processors {
                    if let Err(mut errs) = lower_ida_processor(resolved, p, &mut items) {
                        errors.append(&mut errs);
                    }
                }
            }
            TargetKind::Binja => {
                for a in &target.binja_architectures {
                    if let Err(mut errs) = lower_binja_architecture(resolved, a, &mut items) {
                        errors.append(&mut errs);
                    }
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(LoweredBindings { items })
    } else {
        Err(errors)
    }
}

fn spec_path(resolved: &ResolvedBindings, decoder_name: &str) -> Result<PathBuf, Error> {
    if let Some((spec, _)) = resolved.find_decoder_or_sub(decoder_name) {
        Ok(spec.path.clone())
    } else {
        Err(Error::new(
            ErrorKind::UnknownDecoderInBinding {
                name: decoder_name.to_string(),
                suggestion: None,
            },
            crate::error::Span::new("<lower>", 0, 0, 0),
        ))
    }
}

fn lower_rust_decoder(
    resolved: &ResolvedBindings,
    d: &DecoderBinding,
    items: &mut Vec<LoweredItem>,
) -> Result<(), Vec<Error>> {
    let input = spec_path(resolved, &d.decoder_name).map_err(|e| vec![e])?;
    let mut type_map = HashMap::new();
    for (k, v) in &d.type_map {
        type_map.insert(k.clone(), v.clone());
    }
    let target = GenTarget {
        input: input.to_string_lossy().into_owned(),
        lang: "rust".to_string(),
        output: d.output.clone(),
        format: false,
        dispatch: Dispatch::default(),
        dispatch_overrides: HashMap::new(),
        type_map,
        lang_options: LangOptions::None,
    };
    items.push(LoweredItem {
        target_kind: TargetKind::Rust,
        decoder_name: d.decoder_name.clone(),
        kind: LoweredKind::Gen(target),
    });

    // Sub-decoders share the same generated decoder file in the existing
    // backend, but the user may still want a separate output for nested
    // sub-decoder type maps. Today the Rust backend doesn't generate a
    // separate file per sub-decoder beyond what the parent decoder emits,
    // so we ignore subdecoders here. (The dispatch side handles them.)
    let _ = &d.subdecoders;

    Ok(())
}

fn lower_cpp_decoder(
    resolved: &ResolvedBindings,
    d: &DecoderBinding,
    items: &mut Vec<LoweredItem>,
) -> Result<(), Vec<Error>> {
    let input = spec_path(resolved, &d.decoder_name).map_err(|e| vec![e])?;
    let mut type_map = HashMap::new();
    for (k, v) in &d.type_map {
        type_map.insert(k.clone(), v.clone());
    }
    let guard_style = match d.cpp_guard_style.as_deref() {
        Some("ifndef") => CppGuardStyle::Ifndef,
        _ => CppGuardStyle::Pragma,
    };
    let cpp = CppOptions {
        namespace: d.cpp_namespace.clone(),
        guard_style,
        includes: d.cpp_includes.clone(),
    };
    let target = GenTarget {
        input: input.to_string_lossy().into_owned(),
        lang: "cpp".to_string(),
        output: d.output.clone(),
        format: false,
        dispatch: Dispatch::default(),
        dispatch_overrides: HashMap::new(),
        type_map,
        lang_options: LangOptions::Cpp(cpp),
    };
    items.push(LoweredItem {
        target_kind: TargetKind::Cpp,
        decoder_name: d.decoder_name.clone(),
        kind: LoweredKind::Gen(target),
    });
    Ok(())
}

fn lower_ida_processor(
    resolved: &ResolvedBindings,
    p: &IdaProcessorBinding,
    items: &mut Vec<LoweredItem>,
) -> Result<(), Vec<Error>> {
    let input = spec_path(resolved, &p.decoder_name).map_err(|e| vec![e])?;
    let opts = IdaOptions {
        processor_name: p.name.clone().unwrap_or_default(),
        processor_long_name: p.long_name.clone().unwrap_or_default(),
        processor_id: p.id.unwrap_or(0),
        register_names: p.registers.clone(),
        segment_registers: p.segment_registers.iter().map(|(s, _)| s.clone()).collect(),
        address_size: p.address_size.unwrap_or(32),
        bytes_per_unit: p.bytes_per_unit.unwrap_or(1),
        flags: Vec::new(),
        operand_types: HashMap::<String, OperandKind>::new(),
        display_prefixes: HashMap::new(),
        flow: FlowConfig {
            calls: p.flow.calls.iter().map(|(s, _)| s.clone()).collect(),
            branches: Vec::new(),
            unconditional_branches: Vec::new(),
            returns: p.flow.returns.iter().map(|(s, _)| s.clone()).collect(),
            stops: p.flow.stops.iter().map(|(s, _)| s.clone()).collect(),
        },
    };
    let output = p.output.clone().unwrap_or_default();
    let target = GenTarget {
        input: input.to_string_lossy().into_owned(),
        lang: "ida".to_string(),
        output,
        format: false,
        dispatch: Dispatch::default(),
        dispatch_overrides: HashMap::new(),
        type_map: HashMap::new(),
        lang_options: LangOptions::Ida(opts),
    };
    items.push(LoweredItem {
        target_kind: TargetKind::Ida,
        decoder_name: p.decoder_name.clone(),
        kind: LoweredKind::Gen(target),
    });
    Ok(())
}

fn lower_binja_architecture(
    resolved: &ResolvedBindings,
    a: &BinjaArchitectureBinding,
    items: &mut Vec<LoweredItem>,
) -> Result<(), Vec<Error>> {
    let input = spec_path(resolved, &a.decoder_name).map_err(|e| vec![e])?;
    let endian = a
        .endianness
        .as_ref()
        .map(|(s, _)| match s.as_str() {
            "big" => "BigEndian",
            "little" => "LittleEndian",
            _ => "LittleEndian",
        })
        .unwrap_or("LittleEndian")
        .to_string();
    let address_size = a.address_size.unwrap_or(4);
    let opts = BinjaOptions {
        architecture_name: a.name.clone().unwrap_or_default(),
        address_size,
        default_int_size: a.default_int_size.unwrap_or(address_size),
        max_instr_length: address_size,
        endianness: endian,
        register_names: a.registers.clone(),
        register_size: address_size,
        stack_pointer: None,
        link_register: None,
        bytes_per_unit: 1,
        display_prefixes: HashMap::new(),
        operand_types: HashMap::<String, OperandKind>::new(),
        flow: FlowConfig::default(),
    };
    let output = a.output.clone().unwrap_or_default();
    let target = GenTarget {
        input: input.to_string_lossy().into_owned(),
        lang: "binja".to_string(),
        output,
        format: false,
        dispatch: Dispatch::default(),
        dispatch_overrides: HashMap::new(),
        type_map: HashMap::new(),
        lang_options: LangOptions::Binja(opts),
    };
    items.push(LoweredItem {
        target_kind: TargetKind::Binja,
        decoder_name: a.decoder_name.clone(),
        kind: LoweredKind::Gen(target),
    });
    Ok(())
}

fn lower_rust_dispatch(
    resolved: &ResolvedBindings,
    d: &DispatchBinding,
    parent: Option<&DispatchBinding>,
    items: &mut Vec<LoweredItem>,
) -> Result<(), Vec<Error>> {
    let input = spec_path(resolved, &d.decoder_name).map_err(|e| vec![e])?;

    let context = d
        .context
        .clone()
        .or_else(|| parent.and_then(|p| p.context.clone()))
        .unwrap_or_default();
    let handlers = d
        .handlers
        .clone()
        .or_else(|| parent.and_then(|p| p.handlers.clone()))
        .unwrap_or_default();
    let strategy = d
        .strategy
        .or_else(|| parent.and_then(|p| p.strategy))
        .unwrap_or(Dispatch::FnPtrLut);
    let invalid_handler = d
        .invalid_handler
        .clone()
        .or_else(|| parent.and_then(|p| p.invalid_handler.clone()));
    let handler_consts: Vec<String> = if !d.handler_consts.is_empty() {
        d.handler_consts.clone()
    } else {
        parent.map(|p| p.handler_consts.clone()).unwrap_or_default()
    };

    let mut groups: HashMap<String, Vec<String>> = HashMap::new();
    for g in &d.handler_groups {
        let names: Vec<String> = g.instructions.iter().map(|(n, _)| n.clone()).collect();
        groups.insert(g.handler_name.clone(), names);
    }

    let instr_type = d
        .instruction_type
        .as_ref()
        .map(|it| it.type_path.clone())
        .or_else(|| {
            parent.and_then(|p| p.instruction_type.as_ref().map(|it| it.type_path.clone()))
        });
    let raw_expr = if instr_type.is_some() {
        Some("instr.0".to_string())
    } else {
        None
    };
    let instr_type_output = d.instruction_type.as_ref().and_then(|it| it.output.clone());

    let lut_mod: Option<String> = None;

    let mut subdecoder_groups: HashMap<String, HashMap<String, Vec<String>>> = HashMap::new();
    let mut subdecoder_instr_types: HashMap<String, String> = HashMap::new();
    let mut subdecoder_instr_type_outputs: HashMap<String, String> = HashMap::new();
    let mut subdecoder_dispatch: HashMap<String, Dispatch> = HashMap::new();
    let mut subdecoder_invalid_handlers: HashMap<String, String> = HashMap::new();
    let mut subdecoder_handler_mods: HashMap<String, String> = HashMap::new();

    for sd in &d.subdispatches {
        let mut sg: HashMap<String, Vec<String>> = HashMap::new();
        for g in &sd.handler_groups {
            let names: Vec<String> = g.instructions.iter().map(|(n, _)| n.clone()).collect();
            sg.insert(g.handler_name.clone(), names);
        }
        if !sg.is_empty() {
            subdecoder_groups.insert(sd.decoder_name.clone(), sg);
        }
        if let Some(it) = &sd.instruction_type {
            subdecoder_instr_types.insert(sd.decoder_name.clone(), it.type_path.clone());
            if let Some(out) = &it.output {
                subdecoder_instr_type_outputs.insert(sd.decoder_name.clone(), out.clone());
            }
        }
        let strat = sd.strategy.unwrap_or(strategy);
        subdecoder_dispatch.insert(sd.decoder_name.clone(), strat);

        if let Some(h) = &sd.invalid_handler {
            subdecoder_invalid_handlers.insert(sd.decoder_name.clone(), h.clone());
        } else if let Some(h) = &invalid_handler {
            subdecoder_invalid_handlers.insert(sd.decoder_name.clone(), h.clone());
        }
        if let Some(h) = &sd.handlers {
            subdecoder_handler_mods.insert(sd.decoder_name.clone(), h.clone());
        }
    }

    let target = LutTarget {
        input: input.to_string_lossy().into_owned(),
        output: d.output.clone().unwrap_or_default(),
        handler_mod: handlers,
        ctx_type: context,
        dispatch: strategy,
        groups,
        lut_mod,
        instr_type,
        raw_expr,
        instr_type_output,
        subdecoder_groups,
        subdecoder_instr_type_outputs,
        subdecoder_instr_types,
        subdecoder_dispatch,
        invalid_handler,
        subdecoder_invalid_handlers,
        subdecoder_handler_mods,
        handler_consts,
    };
    items.push(LoweredItem {
        target_kind: TargetKind::Rust,
        decoder_name: d.decoder_name.clone(),
        kind: LoweredKind::Lut(target),
    });

    // Subdispatches feed in via the sub_decoder_* fields above; we don't
    // emit a separate LoweredItem per subdispatch.
    let _ = parent;

    Ok(())
}
