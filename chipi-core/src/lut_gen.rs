//! Function-pointer LUT generation from a validated definition and dispatch tree.
//!
//! Translates chipi's decision tree into a set of static `[Handler; N]` arrays,
//! one per tree `Branch`, plus a small inline dispatch function for each.
//! The result is an emulator-style "look-up table" dispatch in generated Rust code.
//!
//! ## Basic usage (one handler per instruction)
//!
//! Each instruction maps directly to `{handler_mod}::{instruction_name}`.
//!
//! ## Grouped handlers with const generics
//!
//! Multiple instructions can share a single handler via const generics:
//!
//! ```text
//! // Generated LUT entry for addi (in the "alu" group):
//! crate::cpu::interpreter::alu::<{ OP_ADDI }>
//!
//! // User implementation:
//! pub fn alu<const OP: u32>(ctx: &mut Cpu, instr: Instruction) {
//!     match OP {
//!         OP_ADDI  => { /* ... */ }
//!         OP_ADDIS => { /* ... */ }
//!         _ => unreachable!(),
//!     }
//! }
//! ```
//!
//! The compiler monomorphizes each instantiation, so runtime dispatch is still a
//! single indirect call into the table with no further branching overhead.

use std::collections::HashMap;
use std::fmt::Write;

use crate::tree::DecodeNode;
use crate::types::*;

struct Ctx<'a> {
    def: &'a ValidatedDef,
    handler_mod: &'a str,
    ctx_type: &'a str,
    /// Type of the second parameter passed to every handler.
    /// `"u32"` means the raw opcode word (default); any other value is a
    /// user-supplied wrapper type.
    instr_type: &'a str,
    /// Expression that yields a `u32` from the local `instr` / `opcode`
    /// parameter inside a generated dispatch function. For `u32` this is just
    /// the parameter name itself; for a newtype wrapper it might be `"instr.0"`.
    raw_expr: &'a str,
    uid: usize,
    /// Accumulated auxiliary items (tables + dispatch fns) in emission order.
    buf: String,
    /// instr_name -> group handler function name (empty = no grouping)
    groups: &'a HashMap<String, String>,
}

impl<'a> Ctx<'a> {
    fn uid(&mut self) -> usize {
        let id = self.uid;
        self.uid += 1;
        id
    }

    /// Parameter name used in generated function signatures.
    /// `"opcode"` for primitive integer types, `"instr"` for wrapper types.
    fn param_name(&self) -> &'static str {
        if is_primitive(self.instr_type) {
            "opcode"
        } else {
            "instr"
        }
    }

    /// Return the handler expression for `instr_name` as it should appear in a
    /// static table or a direct call.
    ///
    /// - Ungrouped: `handler_mod::instr_name`
    /// - Grouped:   `handler_mod::group_name::<{ OP_INSTR_NAME }>`
    fn handler_for(&self, instr_name: &str) -> String {
        if let Some(group) = self.groups.get(instr_name) {
            format!(
                "{}::{group}::<{{ {} }}>",
                self.handler_mod,
                op_const_name(instr_name)
            )
        } else {
            format!("{}::{}", self.handler_mod, instr_name)
        }
    }
}

/// Generate the Rust source for a function-pointer LUT.
///
/// `handler_mod`  - module path for handler functions, e.g. `"crate::cpu::interpreter"`.
/// `ctx_type`     - context type passed to every handler, e.g. `"crate::gekko::Gekko"`.
/// `groups`       - maps each instruction name to the group handler that should serve it.
///                  Pass an empty map for one-handler-per-instruction behaviour.
/// `instr_type`   - type of the second parameter of every handler. Pass `None` to
///                  derive automatically from the spec's `width` (`u8`/`u16`/`u32`).
///                  Pass `Some("crate::cpu::Instruction")` to use a wrapper type.
/// `raw_expr`     - expression that yields the underlying integer from the `instr`
///                  local inside a dispatch function. Ignored when `instr_type` is
///                  `None` (auto). Defaults to `"instr.0"` for wrapper types.
///
/// The generated file contains:
/// - `pub const OP_*: u32`: one constant per instruction, usable as const generic args
/// - `pub type Handler = fn(&mut Ctx, InstrType);`
/// - Static dispatch tables (`_T0`, `_T1`, ...) sized to each bit-range level
/// - `pub fn dispatch(ctx: &mut Ctx, instr: InstrType)`
pub fn generate_lut_code(
    def: &ValidatedDef,
    tree: &DecodeNode,
    handler_mod: &str,
    ctx_type: &str,
    groups: &HashMap<String, String>,
    instr_type: Option<&str>,
    raw_expr: Option<&str>,
    dispatch: crate::Dispatch,
) -> String {
    let instr_type = instr_type.unwrap_or_else(|| width_to_type(def.config.width));
    let raw_expr = raw_expr.unwrap_or_else(|| {
        if is_primitive(instr_type) {
            "opcode"
        } else {
            "instr.0"
        }
    });

    let mut ctx = Ctx {
        def,
        handler_mod,
        ctx_type,
        instr_type,
        raw_expr,
        uid: 0,
        buf: String::new(),
        groups,
    };

    let ct = ctx_type;
    let it = instr_type;
    let pn = ctx.param_name();
    let re = raw_expr;

    let mut out = String::new();
    writeln!(out, "// Auto-generated by chipi. Do not edit.").unwrap();
    writeln!(out).unwrap();

    // OP_* constants
    writeln!(
        out,
        "// Per-instruction constants. Use as const-generic arguments:"
    )
    .unwrap();
    writeln!(
        out,
        "// `fn alu<const OP: u32>(ctx, instr) {{ match OP {{ OP_ADDI => ... }} }}`"
    )
    .unwrap();
    for (i, instr) in def.instructions.iter().enumerate() {
        writeln!(out, "pub const {}: u32 = {i};", op_const_name(&instr.name)).unwrap();
    }
    writeln!(out).unwrap();

    match dispatch {
        crate::Dispatch::FnPtrLut => {
            let root = emit_node(tree, &mut ctx);

            writeln!(out, "pub type Handler = fn(&mut {ct}, {it});").unwrap();
            writeln!(out).unwrap();
            writeln!(out, "#[cold]").unwrap();
            writeln!(out, "#[inline(never)]").unwrap();
            writeln!(out, "fn _unimpl(_ctx: &mut {ct}, {pn}: {it}) {{").unwrap();
            writeln!(out, "    todo!(\"unimplemented opcode {{:#010x}}\", {re})").unwrap();
            writeln!(out, "}}").unwrap();
            writeln!(out).unwrap();
            out.push_str(&ctx.buf);
            writeln!(out, "/// Dispatch an instruction word to its handler.").unwrap();
            writeln!(out, "#[inline(always)]").unwrap();
            writeln!(out, "pub fn dispatch(ctx: &mut {ct}, {pn}: {it}) {{").unwrap();
            writeln!(out, "    {root}(ctx, {pn});").unwrap();
            writeln!(out, "}}").unwrap();
        }
        crate::Dispatch::JumpTable => {
            writeln!(out, "/// Dispatch an instruction word to its handler.").unwrap();
            writeln!(out, "#[inline(always)]").unwrap();
            writeln!(out, "pub fn dispatch(ctx: &mut {ct}, {pn}: {it}) {{").unwrap();
            emit_jump_table_node(&mut out, tree, &mut ctx, 1);
            writeln!(out, "}}").unwrap();
        }
    }

    // Generate instr_size() for variable-length decoders
    if needs_variable_length(def) {
        writeln!(out).unwrap();
        writeln!(
            out,
            "/// Returns the size of the instruction in units (words)."
        )
        .unwrap();
        writeln!(out, "#[inline(always)]").unwrap();
        writeln!(out, "pub fn instr_size({pn}: {it}) -> u32 {{").unwrap();
        emit_size_node(&mut out, tree, def, &re, 1);
        writeln!(out, "}}").unwrap();
    }

    out
}

/// Generate handler stub functions for every instruction.
///
/// `group_to_instrs` maps each group handler name to the instruction names it covers.
/// Pass an empty map for one-stub-per-instruction behaviour.
///
/// `lut_mod` is the Rust module path where the generated `OP_*` constants live
/// (e.g. `"crate::cpu::lut"`). Required when groups are non-empty so that the
/// const-generic stubs can reference those constants.
///
/// `instr_type` is the type of the second parameter. Pass `None` to derive from the
/// spec's `width` (same default as `generate_lut_code`).
///
/// Intended to be run **once** to bootstrap an interpreter module.
pub fn generate_stubs_code(
    def: &ValidatedDef,
    ctx_type: &str,
    group_to_instrs: &HashMap<String, Vec<String>>,
    lut_mod: Option<&str>,
    instr_type: Option<&str>,
) -> String {
    let instr_type = instr_type.unwrap_or_else(|| width_to_type(def.config.width));

    // Reverse map: instr_name -> group fn name
    let instr_to_group: HashMap<&str, &str> = group_to_instrs
        .iter()
        .flat_map(|(g, v)| v.iter().map(move |i| (i.as_str(), g.as_str())))
        .collect();

    let it = instr_type;
    let pn = if is_primitive(instr_type) {
        "_opcode"
    } else {
        "_instr"
    };

    let mut out = String::new();
    writeln!(
        out,
        "// Handler stubs. Implement each function and remove the todo!()"
    )
    .unwrap();
    writeln!(out, "#![allow(unused_variables)]").unwrap();
    writeln!(out).unwrap();

    // If there are groups and we know where the OP constants live, import them.
    if !group_to_instrs.is_empty() {
        if let Some(lut) = lut_mod {
            writeln!(out, "use {lut}::*;").unwrap();
            writeln!(out).unwrap();
        }
    }

    // Emit one const-generic stub per group, with a match arm per instruction.
    let mut emitted_groups: Vec<&str> = group_to_instrs.keys().map(|s| s.as_str()).collect();
    emitted_groups.sort();
    for group in emitted_groups {
        let instrs = &group_to_instrs[group];
        writeln!(
            out,
            "pub fn {group}<const OP: u32>(_ctx: &mut {ctx_type}, {pn}: {it}) {{"
        )
        .unwrap();
        writeln!(out, "    match OP {{").unwrap();
        for instr in instrs {
            writeln!(
                out,
                "        {} => todo!(\"{instr}\"),",
                op_const_name(instr)
            )
            .unwrap();
        }
        writeln!(out, "        _ => unreachable!(),").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }

    // Emit individual stubs for ungrouped instructions.
    for instr in &def.instructions {
        if instr_to_group.contains_key(instr.name.as_str()) {
            continue;
        }
        writeln!(
            out,
            "pub fn {}(_ctx: &mut {ctx_type}, {pn}: {it}) {{ todo!(\"{}\") }}",
            instr.name, instr.name,
        )
        .unwrap();
    }

    out
}

fn emit_node(node: &DecodeNode, ctx: &mut Ctx) -> String {
    match node {
        DecodeNode::Fail => "_unimpl".to_string(),

        DecodeNode::Leaf { instruction_index } => {
            ctx.handler_for(&ctx.def.instructions[*instruction_index].name)
        }

        DecodeNode::PriorityLeaves { candidates } => {
            let id = ctx.uid();
            let fn_name = format!("_priority_{id}");
            // Clone these strings before the loop so we can mutably borrow ctx.buf below.
            let ct = ctx.ctx_type.to_string();
            let it = ctx.instr_type.to_string();
            let pn = ctx.param_name();
            let re = ctx.raw_expr.to_string();

            let mut body = String::new();
            writeln!(body, "#[inline(always)]").unwrap();
            writeln!(body, "fn {fn_name}(ctx: &mut {ct}, {pn}: {it}) {{").unwrap();

            let mut has_open_branch = false;
            for (i, &idx) in candidates.iter().enumerate() {
                let handler = ctx.handler_for(&ctx.def.instructions[idx].name);
                let guard = full_guard_expr(&ctx.def.instructions[idx], &re);

                match (i, guard) {
                    (0, Some(g)) => {
                        writeln!(body, "    if {g} {{").unwrap();
                        writeln!(body, "        {handler}(ctx, {pn});").unwrap();
                        has_open_branch = true;
                    }
                    (_, Some(g)) => {
                        writeln!(body, "    }} else if {g} {{").unwrap();
                        writeln!(body, "        {handler}(ctx, {pn});").unwrap();
                    }
                    (0, None) => {
                        writeln!(body, "    {handler}(ctx, {pn});").unwrap();
                        has_open_branch = false;
                        break;
                    }
                    (_, None) => {
                        writeln!(body, "    }} else {{").unwrap();
                        writeln!(body, "        {handler}(ctx, {pn});").unwrap();
                        writeln!(body, "    }}").unwrap();
                        has_open_branch = false;
                        break;
                    }
                }
            }
            if has_open_branch {
                writeln!(body, "    }}").unwrap();
            }
            writeln!(body, "}}\n").unwrap();

            ctx.buf.push_str(&body);
            fn_name
        }

        DecodeNode::Branch {
            range,
            arms,
            default,
        } => {
            let id = ctx.uid();
            let table = format!("_T{id}");
            let dispatch = format!("_d{id}");
            let size = 1usize << range.width();
            // Clone these strings before emitting so we can mutably borrow ctx.buf below.
            let ct = ctx.ctx_type.to_string();
            let it = ctx.instr_type.to_string();
            let pn = ctx.param_name();
            let re = ctx.raw_expr.to_string();

            let default_handler = emit_node(default, ctx);
            let mut entries: Vec<String> = vec![default_handler; size];
            for (value, child) in arms {
                let handler = emit_node(child, ctx);
                let idx = *value as usize;
                if idx < size {
                    entries[idx] = handler;
                }
            }

            writeln!(ctx.buf, "static {table}: [Handler; {size}] = [").unwrap();
            for (i, entry) in entries.iter().enumerate() {
                writeln!(ctx.buf, "    {entry}, // {i:#x}").unwrap();
            }
            writeln!(ctx.buf, "];\n").unwrap();

            let extract = range_extract_expr(range, &re);
            writeln!(ctx.buf, "#[inline(always)]").unwrap();
            writeln!(ctx.buf, "fn {dispatch}(ctx: &mut {ct}, {pn}: {it}) {{").unwrap();
            writeln!(ctx.buf, "    {table}[({extract}) as usize](ctx, {pn});").unwrap();
            writeln!(ctx.buf, "}}\n").unwrap();

            dispatch
        }
    }
}

/// Emit a decode tree node as nested match statements for the JumpTable strategy.
fn emit_jump_table_node(out: &mut String, node: &DecodeNode, ctx: &mut Ctx, indent: usize) {
    let pad = "    ".repeat(indent);
    let pn = ctx.param_name();
    // Clone raw_expr so we can pass `ctx` mutably to recursive calls below.
    let re = ctx.raw_expr.to_string();

    match node {
        DecodeNode::Fail => {
            writeln!(
                out,
                "{pad}todo!(\"unimplemented opcode {{:#010x}}\", {re});"
            )
            .unwrap();
        }
        DecodeNode::Leaf { instruction_index } => {
            let handler = ctx.handler_for(&ctx.def.instructions[*instruction_index].name);
            writeln!(out, "{pad}{handler}(ctx, {pn});").unwrap();
        }
        DecodeNode::PriorityLeaves { candidates } => {
            for (i, &idx) in candidates.iter().enumerate() {
                let handler = ctx.handler_for(&ctx.def.instructions[idx].name);
                let guard = full_guard_expr(&ctx.def.instructions[idx], &re);

                match (i, guard) {
                    (0, Some(g)) => {
                        writeln!(out, "{pad}if {g} {{").unwrap();
                        writeln!(out, "{pad}    {handler}(ctx, {pn});").unwrap();
                    }
                    (_, Some(g)) => {
                        writeln!(out, "{pad}}} else if {g} {{").unwrap();
                        writeln!(out, "{pad}    {handler}(ctx, {pn});").unwrap();
                    }
                    (0, None) => {
                        writeln!(out, "{pad}{handler}(ctx, {pn});").unwrap();
                        return;
                    }
                    (_, None) => {
                        writeln!(out, "{pad}}} else {{").unwrap();
                        writeln!(out, "{pad}    {handler}(ctx, {pn});").unwrap();
                        writeln!(out, "{pad}}}").unwrap();
                        return;
                    }
                }
            }
            // Close the last if branch
            writeln!(out, "{pad}}}").unwrap();
        }
        DecodeNode::Branch {
            range,
            arms,
            default,
        } => {
            let extract = range_extract_expr(range, &re);
            writeln!(out, "{pad}match ({extract}) as usize {{").unwrap();

            for (value, child) in arms {
                writeln!(out, "{pad}    {value:#x} => {{").unwrap();
                emit_jump_table_node(out, child, ctx, indent + 2);
                writeln!(out, "{pad}    }}").unwrap();
            }

            writeln!(out, "{pad}    _ => {{").unwrap();
            emit_jump_table_node(out, default, ctx, indent + 2);
            writeln!(out, "{pad}    }}").unwrap();
            writeln!(out, "{pad}}}").unwrap();
        }
    }
}

/// Map a decoder `width` to the corresponding Rust unsigned integer type.
fn width_to_type(width: u32) -> &'static str {
    match width {
        8 => "u8",
        16 => "u16",
        _ => "u32",
    }
}

/// Returns `true` for the Rust primitive unsigned integer types that chipi may
/// emit automatically. Used to decide the parameter name (`opcode` vs `instr`)
/// and whether a raw-extraction expression is needed.
fn is_primitive(t: &str) -> bool {
    matches!(t, "u8" | "u16" | "u32")
}

/// Convert an instruction name to its `OP_*` constant name.
///
/// e.g. `"addi"` -> `"OP_ADDI"`, `"ps_add."` -> `"OP_PS_ADD_DOT"`
pub fn op_const_name(name: &str) -> String {
    let sanitised = name.to_uppercase().replace('.', "_DOT").replace('-', "_");
    format!("OP_{sanitised}")
}

fn full_guard_expr(instr: &ValidatedInstruction, raw_expr: &str) -> Option<String> {
    let mut mask: u32 = 0;
    let mut value: u32 = 0;
    for (unit, hw_bit, bit) in instr.fixed_bits() {
        if unit != 0 || bit == Bit::Wildcard {
            continue;
        }
        mask |= 1 << hw_bit;
        if bit == Bit::One {
            value |= 1 << hw_bit;
        }
    }
    if mask == 0 {
        None
    } else {
        Some(format!("{raw_expr} & {mask:#010x} == {value:#010x}"))
    }
}

fn range_extract_expr(range: &BitRange, raw_expr: &str) -> String {
    let width = range.width();
    let shift = range.end;
    let mask = (1u32 << width) - 1;
    if shift == 0 {
        format!("{raw_expr} & {mask:#x}")
    } else {
        format!("({raw_expr} >> {shift}) & {mask:#x}")
    }
}

/// Check if any instruction in the decoder requires multiple units.
fn needs_variable_length(def: &ValidatedDef) -> bool {
    def.instructions.iter().any(|i| i.unit_count() > 1)
}

/// Emit a decode tree node that returns the instruction size in units.
fn emit_size_node(
    out: &mut String,
    node: &DecodeNode,
    def: &ValidatedDef,
    raw_expr: &str,
    indent: usize,
) {
    let pad = "    ".repeat(indent);

    match node {
        DecodeNode::Fail => {
            writeln!(out, "{pad}1").unwrap();
        }
        DecodeNode::Leaf { instruction_index } => {
            let size = def.instructions[*instruction_index].unit_count();
            writeln!(out, "{pad}{size}").unwrap();
        }
        DecodeNode::PriorityLeaves { candidates } => {
            for (i, &idx) in candidates.iter().enumerate() {
                let size = def.instructions[idx].unit_count();
                let guard = full_guard_expr(&def.instructions[idx], raw_expr);

                match (i, guard) {
                    (0, Some(g)) => {
                        writeln!(out, "{pad}if {g} {{").unwrap();
                        writeln!(out, "{pad}    {size}").unwrap();
                    }
                    (_, Some(g)) => {
                        writeln!(out, "{pad}}} else if {g} {{").unwrap();
                        writeln!(out, "{pad}    {size}").unwrap();
                    }
                    (0, None) => {
                        writeln!(out, "{pad}{size}").unwrap();
                        return;
                    }
                    (_, None) => {
                        writeln!(out, "{pad}}} else {{").unwrap();
                        writeln!(out, "{pad}    {size}").unwrap();
                        writeln!(out, "{pad}}}").unwrap();
                        return;
                    }
                }
            }
            writeln!(out, "{pad}}}").unwrap();
        }
        DecodeNode::Branch {
            range,
            arms,
            default,
        } => {
            let extract = range_extract_expr(range, raw_expr);
            writeln!(out, "{pad}match ({extract}) as usize {{").unwrap();

            for (value, child) in arms {
                writeln!(out, "{pad}    {value:#x} => {{").unwrap();
                emit_size_node(out, child, def, raw_expr, indent + 2);
                writeln!(out, "{pad}    }}").unwrap();
            }

            writeln!(out, "{pad}    _ => {{").unwrap();
            emit_size_node(out, default, def, raw_expr, indent + 2);
            writeln!(out, "{pad}    }}").unwrap();
            writeln!(out, "{pad}}}").unwrap();
        }
    }
}
