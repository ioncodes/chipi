//! Binary Ninja Architecture plugin backend.
//!
//! Generates a Python Architecture plugin for Binary Ninja
//! from a validated `.chipi` definition.

use std::collections::HashMap;

use crate::codegen_binja;
use crate::config::GenTarget;
use crate::tree;
use crate::types::ValidatedDef;

use super::{CodegenBackend, CodegenError, FlowConfig, OperandKind};

/// The Binary Ninja code generation backend.
pub struct BinjaBackend;

/// Parsed Binary Ninja-specific configuration from `lang_options`.
pub struct BinjaOptions {
    pub architecture_name: String,
    pub address_size: u32,
    pub default_int_size: u32,
    pub max_instr_length: u32,
    pub endianness: String,
    pub register_names: Vec<String>,
    pub register_size: u32,
    pub stack_pointer: Option<String>,
    pub link_register: Option<String>,
    pub bytes_per_unit: u32,
    pub display_prefixes: HashMap<String, String>,
    pub operand_types: HashMap<String, OperandKind>,
    pub flow: FlowConfig,
}

impl CodegenBackend for BinjaBackend {
    fn lang(&self) -> &str {
        "binja"
    }

    fn validate_lang_options(&self, options: &toml::Value) -> Result<(), Vec<String>> {
        let table = match options {
            toml::Value::Table(t) => t,
            _ => return Ok(()),
        };

        let mut errors = Vec::new();

        if !table.contains_key("architecture_name") {
            errors.push("missing required lang_option: architecture_name".to_string());
        }
        if !table.contains_key("register_names") {
            errors.push("missing required lang_option: register_names".to_string());
        }

        let known_keys = [
            "architecture_name",
            "address_size",
            "default_int_size",
            "max_instr_length",
            "endianness",
            "register_names",
            "register_size",
            "stack_pointer",
            "link_register",
            "bytes_per_unit",
            "display_prefixes",
            "operand_types",
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
        let opts = parse_binja_options(config).map_err(CodegenError::Internal)?;
        Ok(codegen_binja::generate_binja_code(ir, &tree, &opts))
    }

    fn formatter_command(&self) -> Option<&[&str]> {
        Some(&["ruff", "format"])
    }
}

fn parse_binja_options(config: &GenTarget) -> Result<BinjaOptions, String> {
    let table = match &config.lang_options {
        Some(toml::Value::Table(t)) => t.clone(),
        Some(_) => return Err("lang_options must be a table".to_string()),
        None => return Err("Binary Ninja backend requires lang_options configuration".to_string()),
    };

    let architecture_name = table
        .get("architecture_name")
        .and_then(|v| v.as_str())
        .ok_or("architecture_name must be a string")?
        .to_string();

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

    let address_size = table
        .get("address_size")
        .and_then(|v| v.as_integer())
        .unwrap_or(4) as u32;

    let default_int_size = table
        .get("default_int_size")
        .and_then(|v| v.as_integer())
        .unwrap_or(address_size as i64) as u32;

    let max_instr_length = table
        .get("max_instr_length")
        .and_then(|v| v.as_integer())
        .unwrap_or(4) as u32;

    let endianness = table
        .get("endianness")
        .and_then(|v| v.as_str())
        .unwrap_or("LittleEndian")
        .to_string();

    let register_size = table
        .get("register_size")
        .and_then(|v| v.as_integer())
        .unwrap_or(address_size as i64) as u32;

    let stack_pointer = table
        .get("stack_pointer")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    let link_register = table
        .get("link_register")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    let bytes_per_unit = table
        .get("bytes_per_unit")
        .and_then(|v| v.as_integer())
        .unwrap_or(1) as u32;

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

    Ok(BinjaOptions {
        architecture_name,
        address_size,
        default_int_size,
        max_instr_length,
        endianness,
        register_names,
        register_size,
        stack_pointer,
        link_register,
        bytes_per_unit,
        display_prefixes,
        operand_types,
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
