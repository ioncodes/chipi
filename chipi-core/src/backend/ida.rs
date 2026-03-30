//! IDA Pro processor module backend.
//!
//! Generates a Python (IDAPython) processor module for IDA Pro 9.x
//! from a validated `.chipi` definition.

use std::collections::HashMap;

use crate::codegen_ida;
use crate::config::GenTarget;
use crate::tree;
use crate::types::ValidatedDef;

use super::{CodegenBackend, CodegenError, FlowConfig, OperandKind};

/// The IDA Pro code generation backend.
pub struct IdaBackend;

/// Parsed IDA-specific configuration from `lang_options`.
pub struct IdaOptions {
    pub processor_name: String,
    pub processor_long_name: String,
    pub processor_id: u64,
    pub register_names: Vec<String>,
    pub segment_registers: Vec<String>,
    pub address_size: u32,
    /// Bytes per addressable unit. Word-addressed architectures (like GC DSP)
    /// use 2, meaning raw address values must be multiplied by this to get
    /// IDA byte addresses.
    pub bytes_per_unit: u32,
    pub flags: Vec<String>,
    pub operand_types: HashMap<String, OperandKind>,
    /// Maps type alias names to display prefixes (e.g., "gpr" -> "r").
    pub display_prefixes: HashMap<String, String>,
    pub flow: FlowConfig,
}

impl CodegenBackend for IdaBackend {
    fn lang(&self) -> &str {
        "ida"
    }

    fn validate_lang_options(&self, options: &toml::Value) -> Result<(), Vec<String>> {
        let table = match options {
            toml::Value::Table(t) => t,
            _ => return Ok(()),
        };

        let mut errors = Vec::new();

        // Required fields
        if !table.contains_key("processor_name") {
            errors.push("missing required lang_option: processor_name".to_string());
        }
        if !table.contains_key("processor_long_name") {
            errors.push("missing required lang_option: processor_long_name".to_string());
        }
        if !table.contains_key("processor_id") {
            errors.push("missing required lang_option: processor_id".to_string());
        }
        if !table.contains_key("register_names") {
            errors.push("missing required lang_option: register_names".to_string());
        }

        // Validate known keys
        let known_keys = [
            "processor_name",
            "processor_long_name",
            "processor_id",
            "register_names",
            "segment_registers",
            "address_size",
            "bytes_per_unit",
            "flags",
            "operand_types",
            "display_prefixes",
            "flow",
        ];
        for key in table.keys() {
            if !known_keys.contains(&key.as_str()) {
                errors.push(format!("unknown lang_option: {}", key));
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn generate(&self, ir: &ValidatedDef, config: &GenTarget) -> Result<String, CodegenError> {
        let tree = tree::build_tree(ir);
        let opts = parse_ida_options(config).map_err(|e| CodegenError::Internal(e))?;
        Ok(codegen_ida::generate_ida_code(
            ir,
            &tree,
            &opts,
            &config.type_map,
        ))
    }

    fn formatter_command(&self) -> Option<&[&str]> {
        Some(&["ruff", "format"])
    }
}

/// Parse `lang_options` and `type_map` into `IdaOptions`.
fn parse_ida_options(config: &GenTarget) -> Result<IdaOptions, String> {
    let table = match &config.lang_options {
        Some(toml::Value::Table(t)) => t.clone(),
        Some(_) => return Err("lang_options must be a table".to_string()),
        None => return Err("IDA backend requires lang_options configuration".to_string()),
    };

    let processor_name = table
        .get("processor_name")
        .and_then(|v| v.as_str())
        .ok_or("processor_name must be a string")?
        .to_string();

    let processor_long_name = table
        .get("processor_long_name")
        .and_then(|v| v.as_str())
        .ok_or("processor_long_name must be a string")?
        .to_string();

    let processor_id = table
        .get("processor_id")
        .and_then(|v| v.as_integer())
        .ok_or("processor_id must be an integer")? as u64;

    let register_names = table
        .get("register_names")
        .and_then(|v| v.as_array())
        .ok_or("register_names must be an array")?
        .iter()
        .map(|v| {
            v.as_str()
                .ok_or("register_names entries must be strings")
                .map(|s| s.to_string())
        })
        .collect::<Result<Vec<String>, _>>()?;

    if register_names.is_empty() {
        return Err("register_names must not be empty".to_string());
    }

    let segment_registers = if let Some(sr) = table.get("segment_registers") {
        sr.as_array()
            .ok_or("segment_registers must be an array")?
            .iter()
            .map(|v| {
                v.as_str()
                    .ok_or("segment_registers entries must be strings")
                    .map(|s| s.to_string())
            })
            .collect::<Result<Vec<String>, _>>()?
    } else {
        // Default: last 2 registers are CS, DS
        let len = register_names.len();
        if len >= 2 {
            vec![
                register_names[len - 2].clone(),
                register_names[len - 1].clone(),
            ]
        } else {
            register_names.clone()
        }
    };

    let address_size = table
        .get("address_size")
        .and_then(|v| v.as_integer())
        .unwrap_or(32) as u32;

    let bytes_per_unit = table
        .get("bytes_per_unit")
        .and_then(|v| v.as_integer())
        .unwrap_or(1) as u32;

    let flags = if let Some(f) = table.get("flags") {
        f.as_array()
            .ok_or("flags must be an array")?
            .iter()
            .map(|v| {
                v.as_str()
                    .ok_or("flags entries must be strings")
                    .map(|s| s.to_string())
            })
            .collect::<Result<Vec<String>, _>>()?
    } else {
        Vec::new()
    };

    let operand_types = if let Some(ot) = table.get("operand_types") {
        let ot_table = ot.as_table().ok_or("operand_types must be a table")?;
        let mut map = HashMap::new();
        for (key, val) in ot_table {
            let kind_str = val.as_str().ok_or("operand_types values must be strings")?;
            let kind = match kind_str {
                "register" | "reg" => OperandKind::Register,
                "immediate" | "imm" => OperandKind::Immediate,
                "address" | "addr" => OperandKind::Address,
                "memory" | "mem" => OperandKind::Memory,
                _ => {
                    return Err(format!(
                        "unknown operand type '{}' for field '{}'",
                        kind_str, key
                    ));
                }
            };
            map.insert(key.clone(), kind);
        }
        map
    } else {
        HashMap::new()
    };

    let display_prefixes = if let Some(dp) = table.get("display_prefixes") {
        let dp_table = dp.as_table().ok_or("display_prefixes must be a table")?;
        let mut map = HashMap::new();
        for (key, val) in dp_table {
            let prefix = val
                .as_str()
                .ok_or("display_prefixes values must be strings")?;
            map.insert(key.clone(), prefix.to_string());
        }
        map
    } else {
        HashMap::new()
    };

    let flow = if let Some(f) = table.get("flow") {
        let f_table = f.as_table().ok_or("flow must be a table")?;
        FlowConfig {
            calls: parse_string_array(f_table, "calls")?,
            branches: parse_string_array(f_table, "branches")?,
            unconditional_branches: parse_string_array(f_table, "unconditional_branches")?,
            returns: parse_string_array(f_table, "returns")?,
            stops: parse_string_array(f_table, "stops")?,
        }
    } else {
        FlowConfig::default()
    };

    Ok(IdaOptions {
        processor_name,
        processor_long_name,
        processor_id,
        register_names,
        segment_registers,
        address_size,
        bytes_per_unit,
        flags,
        operand_types,
        display_prefixes,
        flow,
    })
}

fn parse_string_array(
    table: &toml::map::Map<String, toml::Value>,
    key: &str,
) -> Result<Vec<String>, String> {
    if let Some(v) = table.get(key) {
        v.as_array()
            .ok_or(format!("{} must be an array", key))?
            .iter()
            .map(|v| {
                v.as_str()
                    .ok_or(format!("{} entries must be strings", key))
                    .map(|s| s.to_string())
            })
            .collect()
    } else {
        Ok(Vec::new())
    }
}
