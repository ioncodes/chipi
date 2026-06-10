//! Function-pointer LUT generation. Consumes a validated definition and a
//! dispatch tree.
//!
//! The output is one or more static `[Handler; N]` arrays. There is one
//! per tree `Branch`. Each table has a small inline dispatch function next
//! to it. The result is an emulator-style lookup-table dispatch.
//!
//! ## Basic usage (one handler per instruction)
//!
//! Each instruction maps directly to `{handler_mod}::{instruction_name}`.
//!
//! ## Grouped handlers with const generics
//!
//! Multiple instructions can share a single handler via const generics.
//! Example:
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
//! The compiler monomorphizes each instantiation. Runtime dispatch is one
//! indirect call into the table. There is no extra branching overhead.

use std::collections::HashMap;
use std::fmt::Write;

use crate::error::{Error, ErrorKind, Span};
use crate::tree::DecodeNode;
use crate::types::*;

struct Ctx<'a> {
    def: &'a ValidatedDef,
    handler_mod: &'a str,
    ctx_type: &'a str,
    /// Type of the second parameter passed to every handler.
    instr_type: &'a str,
    /// Expression that yields a `u32` from the local `instr` or `opcode`
    /// parameter inside a generated dispatch function.
    raw_expr: &'a str,
    uid: usize,
    /// Accumulated auxiliary items in emission order. Holds tables and
    /// dispatch functions.
    buf: String,
    /// Map from instruction name to its group handler function name. Empty
    /// means no grouping.
    groups: &'a HashMap<String, String>,
    /// Optional handler for unmatched opcodes. Replaces the default
    /// `todo!()` panic body of `_unimpl`.
    invalid_handler: Option<&'a str>,
}

impl<'a> Ctx<'a> {
    fn uid(&mut self) -> usize {
        let id = self.uid;
        self.uid += 1;
        id
    }

    /// Parameter name used in generated function signatures.
    /// Returns `"opcode"` for primitive integer types. Returns `"instr"`
    /// for wrapper types.
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
/// `handler_mod`: module path where handlers live. Example:
/// `"crate::cpu::interpreter"`.
///
/// `ctx_type`: context type passed to every handler. Example:
/// `"crate::gekko::Gekko"`.
///
/// `groups`: map from instruction name to group handler name. An empty map
/// gives one handler per instruction.
///
/// `instr_type`: type of the second handler parameter. Pass `None` to
/// derive from the spec's `width`. The auto type is `u8`, `u16`, or `u32`.
/// Pass `Some("crate::cpu::Instruction")` to use a wrapper type.
///
/// `raw_expr`: expression that yields the underlying integer from the
/// `instr` local. Ignored when `instr_type` is `None`. Defaults to
/// `"instr.0"` for wrapper types.
///
/// `invalid_handler`: handler called for unmatched opcodes. `None` falls
/// back to a `todo!()` panic.
///
/// The generated file contains:
///
/// - `pub const OP_*: u32`. One constant per instruction. Usable as a
///   const-generic argument.
/// - `pub type Handler = fn(&mut Ctx, InstrType);`
/// - Static dispatch tables `_T0`, `_T1`, etc. One per bit-range level.
/// - `pub fn dispatch(ctx: &mut Ctx, instr: InstrType)`.
pub fn generate_lut_code(
    def: &ValidatedDef,
    tree: &DecodeNode,
    handler_mod: &str,
    ctx_type: &str,
    groups: &HashMap<String, String>,
    instr_type: Option<&str>,
    raw_expr: Option<&str>,
    dispatch: crate::Dispatch,
    invalid_handler: Option<&str>,
) -> Result<String, Vec<Error>> {
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
        invalid_handler,
    };

    let ct = ctx_type;
    let it = instr_type;
    let pn = ctx.param_name();
    let re = raw_expr;

    let mut out = String::new();
    writeln!(
        out,
        "// Auto-generated by https://github.com/ioncodes/chipi"
    )
    .unwrap();
    writeln!(out, "// Do not edit.").unwrap();
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
            if let Some(handler) = invalid_handler {
                writeln!(out, "    {handler}(_ctx, {pn})").unwrap();
            } else {
                writeln!(out, "    todo!(\"unimplemented opcode {{:#010x}}\", {re})").unwrap();
            }
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
        crate::Dispatch::FlatLut => {
            emit_flat_lut(
                &mut out,
                FlatTargetSource::TopLevel(def),
                &mut ctx,
                "dispatch",
                ct,
                it,
                pn,
                re,
            )?;
        }
        crate::Dispatch::FlatMatch => {
            emit_flat_match(
                &mut out,
                FlatTargetSource::TopLevel(def),
                &mut ctx,
                "dispatch",
                ct,
                it,
                pn,
                re,
            )?;
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

    Ok(out)
}

/// Generate handler stub functions for every instruction.
///
/// `group_to_instrs` maps a group handler name to the instructions it
/// covers. An empty map gives one stub per instruction.
///
/// `lut_mod` is the Rust module path where the generated `OP_*` constants
/// live. Example: `"crate::cpu::lut"`. Required when groups are non-empty.
/// The const-generic stubs need this to reference the constants.
///
/// `instr_type` is the type of the second parameter. Pass `None` to derive
/// from the spec's `width`.
///
/// Run this once to bootstrap an interpreter module.
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
            if let Some(handler) = ctx.invalid_handler {
                writeln!(out, "{pad}{handler}(ctx, {pn});").unwrap();
            } else {
                writeln!(
                    out,
                    "{pad}todo!(\"unimplemented opcode {{:#010x}}\", {re});"
                )
                .unwrap();
            }
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

/// Generate a flat dispatch function for a sub-decoder.
///
/// Produces `pub fn dispatch_{snake_name}(ctx: &mut Ctx, val: u8)` that
/// decodes the extension bits and calls the appropriate handler.
pub fn generate_subdecoder_dispatch(
    _def: &ValidatedDef,
    sd: &ValidatedSubDecoder,
    handler_mod: &str,
    ctx_type: &str,
    groups: &HashMap<String, String>,
    instr_type: Option<&str>,
) -> String {
    let snake_name = sd.name.chars().fold(String::new(), |mut acc, c| {
        if c.is_uppercase() && !acc.is_empty() {
            acc.push('_');
        }
        acc.push(c.to_ascii_lowercase());
        acc
    });
    let dispatch_fn = format!("dispatch_{snake_name}");
    let lut_size = 1usize << sd.width;

    let mut out = String::new();

    // OP_EXT_* constants
    writeln!(out, "// Sub-decoder constants for {}", sd.name).unwrap();
    for (i, instr) in sd.instructions.iter().enumerate() {
        writeln!(out, "pub const {}: u32 = {i};", op_const_name(&instr.name)).unwrap();
    }
    writeln!(out).unwrap();

    // Build dispatch table: value -> instruction index
    // Prefer more specific matches (more fixed bits)
    let mut dispatch_table: Vec<Option<usize>> = vec![None; lut_size];
    let mut specificity: Vec<u32> = vec![0; lut_size];

    for (instr_idx, instr) in sd.instructions.iter().enumerate() {
        // Count fixed (non-wildcard) bits
        let fixed_count: u32 = instr
            .segments
            .iter()
            .map(|seg| {
                if let Segment::Fixed { pattern, .. } = seg {
                    pattern
                        .iter()
                        .filter(|b| matches!(b, Bit::Zero | Bit::One))
                        .count() as u32
                } else {
                    0
                }
            })
            .sum();

        for val in 0..lut_size {
            let matches = instr.segments.iter().all(|seg| {
                if let Segment::Fixed {
                    ranges, pattern, ..
                } = seg
                {
                    let mut bit_idx = 0;
                    for range in ranges {
                        for i in 0..range.width() as usize {
                            if bit_idx < pattern.len() {
                                let hw_bit = range.start - i as u32;
                                let bit_val = (val >> hw_bit) & 1;
                                match pattern[bit_idx] {
                                    Bit::Zero if bit_val != 0 => return false,
                                    Bit::One if bit_val != 1 => return false,
                                    _ => {}
                                }
                                bit_idx += 1;
                            }
                        }
                    }
                    true
                } else {
                    true
                }
            });
            if matches && (dispatch_table[val].is_none() || fixed_count > specificity[val]) {
                dispatch_table[val] = Some(instr_idx);
                specificity[val] = fixed_count;
            }
        }
    }

    // Emit jump table dispatch
    let param_type = width_to_type(sd.width);
    let (param_name, param_type_str, raw_expr) = if let Some(it) = instr_type {
        ("instr", it.to_string(), "instr.0".to_string())
    } else {
        ("val", param_type.to_string(), "val".to_string())
    };
    writeln!(out, "/// Dispatch a sub-decoder extension opcode.").unwrap();
    writeln!(out, "#[inline(always)]").unwrap();
    writeln!(
        out,
        "pub fn {dispatch_fn}(ctx: &mut {ctx_type}, {param_name}: {param_type_str}) {{"
    )
    .unwrap();
    writeln!(out, "    match {raw_expr} {{").unwrap();

    let mut i = 0;
    while i < lut_size {
        let current = dispatch_table[i];
        let start = i;
        while i < lut_size && dispatch_table[i] == current {
            i += 1;
        }
        let end = i - 1;

        let pattern = if start == end {
            format!("{:#x}", start)
        } else {
            format!("{:#x}..={:#x}", start, end)
        };

        match current {
            Some(idx) => {
                let instr_name = &sd.instructions[idx].name;
                let handler = if let Some(group) = groups.get(instr_name) {
                    format!(
                        "{}::{group}::<{{ {} }}>",
                        handler_mod,
                        op_const_name(instr_name)
                    )
                } else {
                    format!("{}::{}", handler_mod, instr_name)
                };
                writeln!(out, "        {pattern} => {handler}(ctx, {param_name}),").unwrap();
            }
            None => {
                writeln!(out, "        {pattern} => {{}},").unwrap();
            }
        }
    }

    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();

    out
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

// ---------------------------------------------------------------------------
// Flat dispatch (flat_lut, flat_match)
// ---------------------------------------------------------------------------

/// Source of instructions for flat-dispatch enumeration.
/// `TopLevel` considers only unit-0 segments. `Sub` considers all segments.
pub(crate) enum FlatTargetSource<'a> {
    TopLevel(&'a ValidatedDef),
    Sub(&'a ValidatedSubDecoder),
}

impl<'a> FlatTargetSource<'a> {
    fn width(&self) -> u32 {
        match self {
            FlatTargetSource::TopLevel(d) => d.config.width,
            FlatTargetSource::Sub(s) => s.width,
        }
    }

    fn instruction_count(&self) -> usize {
        match self {
            FlatTargetSource::TopLevel(d) => d.instructions.len(),
            FlatTargetSource::Sub(s) => s.instructions.len(),
        }
    }

    fn instr_name(&self, idx: usize) -> &str {
        match self {
            FlatTargetSource::TopLevel(d) => &d.instructions[idx].name,
            FlatTargetSource::Sub(s) => &s.instructions[idx].name,
        }
    }

    /// Whether instruction `idx` matches raw value `raw`.
    /// Top-level decoders look only at unit-0 segments. Variable-length
    /// instructions still flat-dispatch on their first word.
    fn matches(&self, idx: usize, raw: u64) -> bool {
        let segments = match self {
            FlatTargetSource::TopLevel(d) => &d.instructions[idx].segments,
            FlatTargetSource::Sub(s) => &s.instructions[idx].segments,
        };
        let only_unit_zero = matches!(self, FlatTargetSource::TopLevel(_));

        for seg in segments {
            if let Segment::Fixed {
                ranges, pattern, ..
            } = seg
            {
                let mut bit_idx = 0;
                for range in ranges {
                    let in_unit_0 = range.unit == 0;
                    let range_width = range.width() as usize;
                    if only_unit_zero && !in_unit_0 {
                        bit_idx += range_width;
                        continue;
                    }
                    for i in 0..range_width {
                        if bit_idx >= pattern.len() {
                            break;
                        }
                        let hw_bit = range.start - i as u32;
                        let bit_val = (raw >> hw_bit) & 1;
                        match pattern[bit_idx] {
                            Bit::Zero if bit_val != 0 => return false,
                            Bit::One if bit_val != 1 => return false,
                            _ => {}
                        }
                        bit_idx += 1;
                    }
                }
            }
        }
        true
    }
}

/// Build a `Vec<String>` of length `2^width`. Each entry is the resolved
/// handler expression for that raw value. Returns ambiguity errors if any
/// value resolves to two or more distinct handler strings.
fn build_flat_handler_table(
    src: &FlatTargetSource,
    handler_for: &dyn Fn(&str) -> String,
    invalid_handler: &str,
    span: &Span,
) -> Result<Vec<String>, Vec<Error>> {
    let width = src.width();
    let n: u64 = 1u64 << width;
    let mut table: Vec<String> = vec![invalid_handler.to_string(); n as usize];
    let mut errors: Vec<Error> = Vec::new();

    let n_instrs = src.instruction_count();

    for raw in 0..n {
        let mut matched: Vec<usize> = Vec::new();
        for idx in 0..n_instrs {
            if src.matches(idx, raw) {
                matched.push(idx);
            }
        }
        if matched.is_empty() {
            // already invalid_handler
        } else if matched.len() == 1 {
            table[raw as usize] = handler_for(src.instr_name(matched[0]));
        } else {
            let handlers: Vec<String> = matched
                .iter()
                .map(|i| handler_for(src.instr_name(*i)))
                .collect();
            let first = &handlers[0];
            if handlers.iter().all(|h| h == first) {
                table[raw as usize] = first.clone();
            } else {
                let pairs: Vec<(String, String)> = matched
                    .iter()
                    .zip(handlers.iter())
                    .map(|(i, h)| (src.instr_name(*i).to_string(), h.clone()))
                    .collect();
                errors.push(Error::new(
                    ErrorKind::FlatDispatchAmbiguous {
                        raw,
                        matches: pairs,
                    },
                    span.clone(),
                ));
            }
        }
    }

    if errors.is_empty() {
        Ok(table)
    } else {
        Err(errors)
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_flat_lut(
    out: &mut String,
    src: FlatTargetSource,
    ctx: &mut Ctx,
    fn_name: &str,
    ct: &str,
    it: &str,
    pn: &str,
    re: &str,
) -> Result<(), Vec<Error>> {
    let invalid = ctx
        .invalid_handler
        .map(|s| s.to_string())
        .unwrap_or_else(|| "_unimpl".to_string());

    let handler_for_owned = |name: &str| ctx.handler_for(name);
    let span = Span::new("<flat_lut>", 0, 0, 0);
    let table = build_flat_handler_table(&src, &handler_for_owned, &invalid, &span)?;

    let n = table.len();
    writeln!(out, "pub type Handler = fn(&mut {ct}, {it});").unwrap();
    writeln!(out).unwrap();
    if ctx.invalid_handler.is_none() {
        writeln!(out, "#[cold]").unwrap();
        writeln!(out, "#[inline(never)]").unwrap();
        writeln!(out, "fn _unimpl(_ctx: &mut {ct}, {pn}: {it}) {{").unwrap();
        writeln!(out, "    todo!(\"unimplemented opcode {{:#010x}}\", {re})").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }
    writeln!(out, "static DISPATCH: [Handler; {n}] = [").unwrap();
    for (i, entry) in table.iter().enumerate() {
        writeln!(out, "    {entry}, // {i:#x}").unwrap();
    }
    writeln!(out, "];").unwrap();
    writeln!(out).unwrap();
    writeln!(out, "/// Dispatch via a flat lookup table.").unwrap();
    writeln!(out, "#[inline(always)]").unwrap();
    writeln!(out, "pub fn {fn_name}(ctx: &mut {ct}, {pn}: {it}) {{").unwrap();
    writeln!(out, "    let key = ({re}) as usize;").unwrap();
    writeln!(out, "    DISPATCH[key](ctx, {pn});").unwrap();
    writeln!(out, "}}").unwrap();

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_flat_match(
    out: &mut String,
    src: FlatTargetSource,
    ctx: &mut Ctx,
    fn_name: &str,
    ct: &str,
    it: &str,
    pn: &str,
    re: &str,
) -> Result<(), Vec<Error>> {
    let invalid = ctx
        .invalid_handler
        .map(|s| s.to_string())
        .unwrap_or_else(|| "_unimpl".to_string());

    let handler_for_owned = |name: &str| ctx.handler_for(name);
    let span = Span::new("<flat_match>", 0, 0, 0);
    let table = build_flat_handler_table(&src, &handler_for_owned, &invalid, &span)?;

    if ctx.invalid_handler.is_none() {
        writeln!(out, "#[cold]").unwrap();
        writeln!(out, "#[inline(never)]").unwrap();
        writeln!(out, "fn _unimpl(_ctx: &mut {ct}, {pn}: {it}) {{").unwrap();
        writeln!(out, "    todo!(\"unimplemented opcode {{:#010x}}\", {re})").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }
    writeln!(out, "/// Dispatch via a compressed match.").unwrap();
    writeln!(out, "#[inline(always)]").unwrap();
    writeln!(out, "pub fn {fn_name}(ctx: &mut {ct}, {pn}: {it}) {{").unwrap();
    writeln!(out, "    match ({re}) as u64 {{").unwrap();

    let mut i = 0usize;
    let n = table.len();
    while i < n {
        let current = &table[i];
        let start = i;
        while i < n && table[i] == *current {
            i += 1;
        }
        let end = i - 1;
        let pattern = if start == end {
            format!("{:#x}", start)
        } else {
            format!("{:#x}..={:#x}", start, end)
        };
        writeln!(out, "        {pattern} => {current}(ctx, {pn}),").unwrap();
    }

    writeln!(out, "        _ => {invalid}(ctx, {pn}),").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();
    Ok(())
}

/// Generate a flat dispatch function for a sub-decoder.
/// Uses the explicit `flat_lut` or `flat_match` strategy.
/// Errors on raw-value ambiguity.
#[allow(clippy::too_many_arguments)]
pub fn generate_subdecoder_flat_dispatch(
    sd: &ValidatedSubDecoder,
    handler_mod: &str,
    ctx_type: &str,
    groups: &HashMap<String, String>,
    instr_type: Option<&str>,
    invalid_handler: Option<&str>,
    strategy: crate::Dispatch,
) -> Result<String, Vec<Error>> {
    let snake_name = sd.name.chars().fold(String::new(), |mut acc, c| {
        if c.is_uppercase() && !acc.is_empty() {
            acc.push('_');
        }
        acc.push(c.to_ascii_lowercase());
        acc
    });
    let dispatch_fn = format!("dispatch_{snake_name}");

    let param_type = width_to_type(sd.width);
    let (param_name, param_type_str, raw_expr_str) = if let Some(it) = instr_type {
        ("instr", it.to_string(), "instr.0".to_string())
    } else {
        ("val", param_type.to_string(), "val".to_string())
    };

    // Build a temporary Ctx-like structure for handler resolution. We
    // emulate Ctx::handler_for using the local `groups` map and `handler_mod`.
    let resolve = |name: &str| -> String {
        if let Some(group) = groups.get(name) {
            format!("{}::{group}::<{{ {} }}>", handler_mod, op_const_name(name))
        } else {
            format!("{}::{}", handler_mod, name)
        }
    };
    let invalid = invalid_handler
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!("{}::invalid", handler_mod));

    let span = Span::new("<flat_subdispatch>", 0, 0, 0);
    let table = build_flat_handler_table(&FlatTargetSource::Sub(sd), &resolve, &invalid, &span)?;

    let mut out = String::new();

    // OP_* constants for the sub-decoder's instructions
    writeln!(out, "// Sub-decoder constants for {}", sd.name).unwrap();
    for (i, instr) in sd.instructions.iter().enumerate() {
        writeln!(out, "pub const {}: u32 = {i};", op_const_name(&instr.name)).unwrap();
    }
    writeln!(out).unwrap();

    match strategy {
        crate::Dispatch::FlatLut => {
            let n = table.len();
            writeln!(
                out,
                "pub type SubHandler{name} = fn(&mut {ctx_type}, {param_type_str});",
                name = sd.name,
            )
            .unwrap();
            writeln!(out).unwrap();
            writeln!(
                out,
                "static DISPATCH_{up}: [SubHandler{name}; {n}] = [",
                up = sd.name.to_uppercase(),
                name = sd.name,
            )
            .unwrap();
            for (i, entry) in table.iter().enumerate() {
                writeln!(out, "    {entry}, // {i:#x}").unwrap();
            }
            writeln!(out, "];").unwrap();
            writeln!(out).unwrap();
            writeln!(out, "/// Dispatch a sub-decoder extension opcode.").unwrap();
            writeln!(out, "#[inline(always)]").unwrap();
            writeln!(
                out,
                "pub fn {dispatch_fn}(ctx: &mut {ctx_type}, {param_name}: {param_type_str}) {{"
            )
            .unwrap();
            writeln!(
                out,
                "    DISPATCH_{up}[({raw_expr_str}) as usize](ctx, {param_name});",
                up = sd.name.to_uppercase()
            )
            .unwrap();
            writeln!(out, "}}").unwrap();
        }
        crate::Dispatch::FlatMatch => {
            writeln!(out, "/// Dispatch a sub-decoder extension opcode.").unwrap();
            writeln!(out, "#[inline(always)]").unwrap();
            writeln!(
                out,
                "pub fn {dispatch_fn}(ctx: &mut {ctx_type}, {param_name}: {param_type_str}) {{"
            )
            .unwrap();
            writeln!(out, "    match {raw_expr_str} {{").unwrap();

            let mut i = 0usize;
            let n = table.len();
            while i < n {
                let current = &table[i];
                let start = i;
                while i < n && table[i] == *current {
                    i += 1;
                }
                let end = i - 1;
                let pattern = if start == end {
                    format!("{:#x}", start)
                } else {
                    format!("{:#x}..={:#x}", start, end)
                };
                writeln!(out, "        {pattern} => {current}(ctx, {param_name}),").unwrap();
            }
            writeln!(out, "        _ => {invalid}(ctx, {param_name}),").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        _ => {
            return Err(vec![Error::new(
                ErrorKind::InvalidStrategy(format!("{:?}", strategy)),
                span,
            )]);
        }
    }

    Ok(out)
}
