use std::path::PathBuf;
use std::process::ExitCode;

use clap::{Parser, Subcommand};

use chipi_core::bindings::{self, RunMode, parse_bindings_file, resolve};
use chipi_core::error::Errors;

#[derive(Parser)]
#[command(
    name = "chipi",
    version,
    about = "Code generator for chipi instruction-spec and bindings files"
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate code from a bindings file.
    Generate(BindingsArgs),
    /// Validate a bindings file without generating output.
    Check(BindingsArgs),
    /// Decode a hex opcode and print the matched instruction.
    Explain(ExplainArgs),
    /// Print the resolved bindings configuration without writing files.
    Preview(BindingsArgs),
}

#[derive(Parser)]
struct BindingsArgs {
    /// Path to a `*.bindings.chipi` file.
    bindings: PathBuf,

    /// Target name (rust, cpp, ida, binja). Required when the bindings
    /// file declares more than one target.
    #[arg(short, long)]
    target: Option<String>,

    /// Decoder/dispatch/processor/architecture name. Required when the
    /// selected target has more than one.
    #[arg(short, long)]
    decoder: Option<String>,
}

#[derive(Parser)]
struct ExplainArgs {
    /// Path to a `*.bindings.chipi` file.
    bindings: PathBuf,

    /// Target name. Optional when only one target is present.
    #[arg(short, long)]
    target: Option<String>,

    /// Decoder/sub-decoder to decode against.
    #[arg(short, long)]
    decoder: Option<String>,

    /// Hex opcode word to decode (e.g. `0x38600001`).
    opcode: String,
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Generate(a) => {
            let path = a.bindings.clone();
            run_bindings(a, RunMode::Generate).map(|_| {
                println!("ok: generated from {}", path.display());
            })
        }
        Commands::Check(a) => {
            let path = a.bindings.clone();
            run_bindings(a, RunMode::Check).map(|_| {
                println!("ok: {} parsed and validated cleanly", path.display());
            })
        }
        Commands::Preview(a) => run_bindings(a, RunMode::Preview),
        Commands::Explain(a) => run_explain(a),
    };

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{}", e);
            ExitCode::FAILURE
        }
    }
}

fn run_bindings(args: BindingsArgs, mode: RunMode) -> Result<(), Errors> {
    bindings::run(
        &args.bindings,
        args.target.as_deref(),
        args.decoder.as_deref(),
        mode,
    )
    .map_err(Errors)
}

fn run_explain(args: ExplainArgs) -> Result<(), Errors> {
    let opcode = parse_opcode(&args.opcode).map_err(|e| {
        Errors(vec![chipi_core::error::Error::new(
            chipi_core::error::ErrorKind::BindingsParse(e),
            chipi_core::error::Span::new("<cli>", 1, 1, 0),
        )])
    })?;

    let parsed = parse_bindings_file(&args.bindings).map_err(Errors)?;
    let resolved = resolve(parsed).map_err(Errors)?;
    bindings::validate::validate(&resolved).map_err(Errors)?;

    // Find the decoder (top-level or sub-decoder) by name.
    let decoder_name = args.decoder.as_deref().ok_or_else(|| {
        Errors(vec![chipi_core::error::Error::new(
            chipi_core::error::ErrorKind::BindingsParse(
                "explain requires --decoder NAME".to_string(),
            ),
            chipi_core::error::Span::new("<cli>", 1, 1, 0),
        )])
    })?;

    let (spec, sub) = resolved.find_decoder_or_sub(decoder_name).ok_or_else(|| {
        Errors(vec![chipi_core::error::Error::new(
            chipi_core::error::ErrorKind::UnknownDecoderInBinding {
                name: decoder_name.to_string(),
                suggestion: None,
            },
            chipi_core::error::Span::new("<cli>", 1, 1, 0),
        )])
    })?;

    explain_opcode(
        spec,
        sub,
        opcode,
        &resolved,
        args.target.as_deref(),
        decoder_name,
    );
    Ok(())
}

fn parse_opcode(s: &str) -> Result<u64, String> {
    let s = s.trim();
    if let Some(rest) = s.strip_prefix("0x").or(s.strip_prefix("0X")) {
        u64::from_str_radix(rest, 16).map_err(|e| format!("invalid hex opcode: {}", e))
    } else {
        s.parse::<u64>()
            .map_err(|e| format!("invalid decimal opcode: {}", e))
    }
}

fn explain_opcode(
    spec: &chipi_core::bindings::resolve::ResolvedSpec,
    sub: Option<&chipi_core::types::ValidatedSubDecoder>,
    opcode: u64,
    resolved: &chipi_core::bindings::ResolvedBindings,
    _target: Option<&str>,
    decoder_name: &str,
) {
    use chipi_core::types::Segment;

    // Build a list of (name, segments) candidates from the chosen decoder.
    let candidates: Vec<(String, &[Segment])> = match sub {
        Some(sd) => sd
            .instructions
            .iter()
            .map(|i| (i.name.clone(), i.segments.as_slice()))
            .collect(),
        None => spec
            .def
            .instructions
            .iter()
            .map(|i| (i.name.clone(), i.segments.as_slice()))
            .collect(),
    };

    // Find every candidate whose fixed bits match the opcode at unit 0.
    let mut matches: Vec<&str> = Vec::new();
    for (name, segments) in &candidates {
        if instr_matches(segments, opcode, sub.is_none()) {
            matches.push(name);
        }
    }

    if matches.is_empty() {
        println!("no instruction matches opcode {:#010x}", opcode);
        return;
    }

    // Pick the most specific match. Most fixed bits in unit 0 wins.
    let chosen = matches
        .iter()
        .max_by_key(|name| {
            let segs = candidates
                .iter()
                .find(|(n, _)| n == *name)
                .map(|(_, s)| *s)
                .unwrap();
            count_fixed_bits(segs, sub.is_none())
        })
        .unwrap();

    println!("matched: {}", chosen);

    // Print decoded fields. Prefer resolved_fields for the typed view.
    let chosen_validated = if sub.is_none() {
        spec.def.instructions.iter().find(|i| &i.name == chosen)
    } else {
        None
    };
    let chosen_sub = sub.and_then(|sd| sd.instructions.iter().find(|i| &i.name == chosen));

    if let Some(instr) = chosen_validated {
        let values = field_values_resolved(&instr.resolved_fields, opcode);
        if !values.is_empty() {
            println!();
            println!("fields:");
            for (name, value) in &values {
                println!("  {} = {}", name, value);
            }
        }
        if let Some(disasm) = render_disassembly(instr, opcode) {
            println!();
            println!("disassembly:");
            println!("  {}", disasm);
        }
    } else if let Some(instr) = chosen_sub {
        let values = field_values_resolved(&instr.resolved_fields, opcode);
        if !values.is_empty() {
            println!();
            println!("fields:");
            for (name, value) in &values {
                println!("  {} = {}", name, value);
            }
        }
        if !instr.fragments.is_empty() {
            println!();
            println!("fragments:");
            for frag in &instr.fragments {
                let rendered = render_pieces(&frag.pieces, &values);
                println!("  {} = {}", frag.name, rendered);
            }
        }
    } else {
        // Fallback for very old paths. Decode via raw segments only.
        let fields = decoded_fields(&candidates, chosen, opcode);
        if !fields.is_empty() {
            println!();
            println!("fields:");
            for (name, value) in &fields {
                println!("  {} = {}", name, value);
            }
        }
    }

    // Find a corresponding dispatch binding so we can print the handler.
    // Only Rust targets define dispatches.
    for target in &resolved.file.targets {
        if target.kind != chipi_core::bindings::TargetKind::Rust {
            continue;
        }
        for d in &target.rust_dispatches {
            if d.decoder_name == decoder_name {
                println!();
                println!("dispatch:");
                if let Some(s) = d.strategy {
                    println!("  strategy: {:?}", s);
                }
                let handler_mod = d.handlers.as_deref().unwrap_or("");
                let group_for: Option<&str> = d.handler_groups.iter().find_map(|g| {
                    if g.instructions.iter().any(|(n, _)| n == chosen) {
                        Some(g.handler_name.as_str())
                    } else {
                        None
                    }
                });
                match group_for {
                    Some(group) => println!(
                        "  handler: {}::{}::<{{ OP_{} }}>",
                        handler_mod,
                        group,
                        chosen.to_uppercase()
                    ),
                    None => println!("  handler: {}::{}", handler_mod, chosen),
                }
                break;
            }
        }
    }
}

fn instr_matches(segments: &[chipi_core::types::Segment], raw: u64, only_unit_zero: bool) -> bool {
    use chipi_core::types::{Bit, Segment};
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

fn count_fixed_bits(segments: &[chipi_core::types::Segment], only_unit_zero: bool) -> u32 {
    use chipi_core::types::{Bit, Segment};
    let mut n = 0u32;
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
                for _ in 0..range_width {
                    if bit_idx < pattern.len() {
                        if matches!(pattern[bit_idx], Bit::Zero | Bit::One) {
                            n += 1;
                        }
                        bit_idx += 1;
                    }
                }
            }
        }
    }
    n
}

fn decoded_fields(
    candidates: &[(String, &[chipi_core::types::Segment])],
    chosen: &str,
    raw: u64,
) -> Vec<(String, u64)> {
    use chipi_core::types::Segment;
    let segments = candidates
        .iter()
        .find(|(n, _)| n == chosen)
        .map(|(_, s)| *s)
        .unwrap();

    let mut fields: Vec<(String, u64)> = Vec::new();
    for seg in segments {
        if let Segment::Field { name, ranges, .. } = seg {
            let value = extract_field(ranges, raw);
            fields.push((name.clone(), value));
        }
    }
    fields
}

/// Compute field values for a validated instruction. Applies transforms.
fn field_values_resolved(
    resolved_fields: &[chipi_core::types::ResolvedField],
    raw: u64,
) -> Vec<(String, i64)> {
    let mut out: Vec<(String, i64)> = Vec::new();
    for f in resolved_fields {
        let raw_value = extract_field(&f.ranges, raw);
        let total_width: u32 = f.ranges.iter().map(|r| r.width()).sum();
        let value = apply_transforms(raw_value, total_width, &f.resolved_type.transforms);
        out.push((f.name.clone(), value));
    }
    out
}

/// Extract a field value as an unsigned integer. Concatenates all ranges
/// MSB-first.
fn extract_field(ranges: &[chipi_core::types::BitRange], raw: u64) -> u64 {
    let mut value: u64 = 0;
    let mut total_width: u32 = 0;
    for r in ranges {
        total_width += r.width();
    }
    let mut shift_out: u32 = total_width;
    for r in ranges {
        let w = r.width();
        shift_out -= w;
        let mask = if w == 64 { u64::MAX } else { (1u64 << w) - 1 };
        let part = (raw >> r.end) & mask;
        value |= part << shift_out;
    }
    value
}

fn apply_transforms(raw: u64, width: u32, transforms: &[chipi_core::types::Transform]) -> i64 {
    use chipi_core::types::Transform;
    let mut value: i64 = raw as i64;
    let mut current_width = width;
    for t in transforms {
        match t {
            Transform::SignExtend(to) => {
                let from = current_width;
                if from < 64 {
                    let shift = 64 - from;
                    value = ((value << shift) >> shift) as i64;
                }
                current_width = *to;
            }
            Transform::ZeroExtend(to) => {
                let from = current_width;
                if from < 64 {
                    let mask = if from == 0 { 0 } else { (1u64 << from) - 1 };
                    value = (value as u64 & mask) as i64;
                }
                current_width = *to;
            }
            Transform::ShiftLeft(n) => {
                value = value.wrapping_shl(*n);
                current_width = current_width.saturating_add(*n);
            }
        }
    }
    value
}

/// Render the disassembly for an instruction at the given opcode.
fn render_disassembly(instr: &chipi_core::types::ValidatedInstruction, raw: u64) -> Option<String> {
    let values = field_values_resolved(&instr.resolved_fields, raw);
    let line = pick_format_line(&instr.format_lines, &values)?;
    Some(render_pieces(&line.pieces, &values))
}

/// Pick the first format line whose guard evaluates to true. A line with no
/// guard always passes. Returns `None` if no format lines exist.
fn pick_format_line<'a>(
    lines: &'a [chipi_core::types::FormatLine],
    values: &[(String, i64)],
) -> Option<&'a chipi_core::types::FormatLine> {
    for line in lines {
        match &line.guard {
            None => return Some(line),
            Some(g) => {
                if eval_guard(g, values) {
                    return Some(line);
                }
            }
        }
    }
    lines.last()
}

fn eval_guard(g: &chipi_core::types::Guard, values: &[(String, i64)]) -> bool {
    use chipi_core::types::CompareOp;
    for cond in &g.conditions {
        let l = eval_guard_operand(&cond.left, values);
        let r = eval_guard_operand(&cond.right, values);
        let ok = match cond.op {
            CompareOp::Eq => l == r,
            CompareOp::Ne => l != r,
            CompareOp::Lt => l < r,
            CompareOp::Le => l <= r,
            CompareOp::Gt => l > r,
            CompareOp::Ge => l >= r,
        };
        if !ok {
            return false;
        }
    }
    true
}

fn eval_guard_operand(op: &chipi_core::types::GuardOperand, values: &[(String, i64)]) -> i64 {
    use chipi_core::types::GuardOperand;
    match op {
        GuardOperand::Field(name) => field_lookup(values, name),
        GuardOperand::Literal(n) => *n,
        GuardOperand::Expr { left, op, right } => {
            let l = eval_guard_operand(left, values);
            let r = eval_guard_operand(right, values);
            apply_arith(l, *op, r)
        }
    }
}

fn apply_arith(l: i64, op: chipi_core::types::ArithOp, r: i64) -> i64 {
    use chipi_core::types::ArithOp;
    match op {
        ArithOp::Add => l.wrapping_add(r),
        ArithOp::Sub => l.wrapping_sub(r),
        ArithOp::Mul => l.wrapping_mul(r),
        ArithOp::Div if r != 0 => l.wrapping_div(r),
        ArithOp::Mod if r != 0 => l.wrapping_rem(r),
        _ => 0,
    }
}

fn field_lookup(values: &[(String, i64)], name: &str) -> i64 {
    values
        .iter()
        .find(|(n, _)| n == name)
        .map(|(_, v)| *v)
        .unwrap_or(0)
}

fn render_pieces(pieces: &[chipi_core::types::FormatPiece], values: &[(String, i64)]) -> String {
    use chipi_core::types::FormatPiece;
    let mut out = String::new();
    for p in pieces {
        match p {
            FormatPiece::Literal(s) => out.push_str(s),
            FormatPiece::FieldRef { expr, spec } => {
                let rendered = render_expr(expr, values, spec.as_deref());
                out.push_str(&rendered);
            }
        }
    }
    out
}

fn render_expr(
    expr: &chipi_core::types::FormatExpr,
    values: &[(String, i64)],
    spec: Option<&str>,
) -> String {
    use chipi_core::types::FormatExpr;
    match expr {
        FormatExpr::Field(name) => format_value(field_lookup(values, name), spec),
        FormatExpr::IntLiteral(n) => format_value(*n, spec),
        FormatExpr::Arithmetic { left, op, right } => {
            let l = eval_expr_int(left, values);
            let r = eval_expr_int(right, values);
            format_value(apply_arith(l, *op, r), spec)
        }
        FormatExpr::Ternary {
            field,
            if_nonzero,
            if_zero,
        } => {
            let v = field_lookup(values, field);
            if v != 0 {
                if_nonzero.clone()
            } else {
                if_zero.clone().unwrap_or_default()
            }
        }
        FormatExpr::MapCall { map_name, .. } => format!("<map:{}>", map_name),
        FormatExpr::BuiltinCall { func, .. } => format!("<builtin:{:?}>", func),
        FormatExpr::SubDecoderAccess { field, fragment } => {
            format!("<{}.{}>", field, fragment)
        }
    }
}

fn eval_expr_int(expr: &chipi_core::types::FormatExpr, values: &[(String, i64)]) -> i64 {
    use chipi_core::types::FormatExpr;
    match expr {
        FormatExpr::Field(name) => field_lookup(values, name),
        FormatExpr::IntLiteral(n) => *n,
        FormatExpr::Arithmetic { left, op, right } => {
            let l = eval_expr_int(left, values);
            let r = eval_expr_int(right, values);
            apply_arith(l, *op, r)
        }
        _ => 0,
    }
}

/// Format an integer using a chipi format spec. Examples: `04x`, `x`,
/// `02d`. The spec handles signed-hex prefixes.
fn format_value(v: i64, spec: Option<&str>) -> String {
    let spec = match spec {
        Some(s) => s,
        None => return v.to_string(),
    };

    // Detect a base-and-width pattern like "04x" or "8X".
    let mut width: usize = 0;
    let mut zero_pad = false;
    let mut idx = 0;
    let bytes = spec.as_bytes();
    if bytes.first() == Some(&b'0') {
        zero_pad = true;
        idx = 1;
    }
    while idx < bytes.len() && bytes[idx].is_ascii_digit() {
        width = width * 10 + (bytes[idx] - b'0') as usize;
        idx += 1;
    }
    let kind = bytes.get(idx).copied();

    let body = match kind {
        Some(b'x') => format!("{:x}", v),
        Some(b'X') => format!("{:X}", v),
        Some(b'd') | None => format!("{}", v),
        Some(b'b') => format!("{:b}", v),
        Some(b'o') => format!("{:o}", v),
        Some(_) => format!("{}", v),
    };

    if width > body.len() {
        let pad = if zero_pad { '0' } else { ' ' };
        let mut s = String::with_capacity(width);
        for _ in 0..(width - body.len()) {
            s.push(pad);
        }
        s.push_str(&body);
        s
    } else {
        body
    }
}
