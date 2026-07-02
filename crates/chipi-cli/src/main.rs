//! `chipi`: the command-line interface.
//!
//! Subcommands: explain, asm, stubs, emit, check, dump-ir, dump-tree.

#![forbid(unsafe_code)]

use chipi_core::interp;
use chipi_core::model::Isa;
use chipi_core::{compile, render_diagnostics, Source};
use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let Some(cmd) = args.first() else {
        usage();
        return ExitCode::FAILURE;
    };

    let rest = &args[1..];

    let result = match cmd.as_str() {
        "explain" => cmd_explain(rest),
        "asm" => cmd_asm(rest),
        "stubs" => cmd_stubs(rest),
        "emit" => cmd_emit(rest),
        "check" => cmd_check(rest),
        "dump-ir" => cmd_dump_ir(rest),
        "dump-tree" => cmd_dump_tree(rest),
        "-h" | "--help" | "help" => {
            usage();
            Ok(())
        }
        other => Err(format!("unknown subcommand `{other}` (try `chipi --help`)")),
    };

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(msg) => {
            eprintln!("error: {msg}");
            ExitCode::FAILURE
        }
    }
}

fn usage() {
    eprintln!(
        "chipi: instruction-set decoder, dispatcher, disassembler and encoder generator\n\n\
         USAGE:\n\
         \x20 chipi explain   <spec.chipi> [--mode m=v] -- <word>      explain one fetched word\n\
         \x20 chipi explain   <spec.chipi> --stream -- <b0,b1,...>       decode a prefixed byte stream\n\
         \x20 chipi asm       <spec.chipi> -- <line>                   assemble one instruction line\n\
         \x20 chipi stubs     <spec.chipi> [-o <file>]                 emit editable handler stubs\n\
         \x20 chipi emit      [--target rust|cpp|python] [--style newtype|enum] <spec.chipi> [-o <file>]   emit a decoder\n\
         \x20 chipi check     <spec.chipi> [--roundtrip]               run validation passes\n\
         \x20 chipi dump-ir   <spec.chipi>                             print the resolved IR\n\
         \x20 chipi dump-tree <spec.chipi>                             print the lowered decode tree\n\n\
         <word> accepts 0x.., 0b.., or decimal."
    );
}

// ----- argument handling -----

#[derive(Default)]
struct Args {
    positionals: Vec<String>,
    out: Option<String>,
    target: Option<String>,
    style: Option<String>,
    mode: Option<String>,
    roundtrip: bool,
    stream: bool,
    trailing: Vec<String>,
}

fn parse_args(args: &[String]) -> Result<Args, String> {
    let mut a = Args::default();
    let mut after_dd = false;
    let mut it = args.iter();

    while let Some(arg) = it.next() {
        if after_dd {
            a.trailing.push(arg.clone());
            continue;
        }

        match arg.as_str() {
            "--" => after_dd = true,
            "-o" | "--output" => a.out = Some(it.next().ok_or("`-o` needs a path")?.clone()),
            "--target" => a.target = Some(it.next().ok_or("`--target` needs a value")?.clone()),
            "--style" => a.style = Some(it.next().ok_or("`--style` needs `newtype|enum`")?.clone()),
            "--mode" => a.mode = Some(it.next().ok_or("`--mode` needs `m=v,...`")?.clone()),
            "--roundtrip" => a.roundtrip = true,
            "--stream" => a.stream = true,
            other if other.starts_with('-') => return Err(format!("unknown flag `{other}`")),
            other => a.positionals.push(other.to_string()),
        }
    }

    Ok(a)
}

struct Loaded {
    src: Source,
    isa: Isa,
}

fn load(a: &Args) -> Result<Loaded, String> {
    let path = a.positionals.first().ok_or("missing <spec.chipi> path")?;
    let text = std::fs::read_to_string(path).map_err(|e| format!("cannot read `{path}`: {e}"))?;
    let src = Source::new(path.clone(), text.clone());

    match compile(&text) {
        Ok(isa) => Ok(Loaded { src, isa }),
        Err(errs) => Err(format!(
            "{} error(s) in `{path}`:\n\n{}",
            errs.len(),
            render_diagnostics(&errs, &src)
        )),
    }
}

fn print_warnings(l: &Loaded) {
    for w in &l.isa.warnings {
        eprint!("{}", w.render(&l.src));
    }
}

fn mode_combo(isa: &Isa, spec: Option<&str>) -> usize {
    let mut vals: Vec<u64> = isa.modes.iter().map(|m| m.default).collect();

    if let Some(spec) = spec {
        for kv in spec.split(',') {
            if let Some((k, v)) = kv.split_once('=') {
                if let Some(i) = isa.modes.iter().position(|m| m.name == k.trim()) {
                    vals[i] = v.trim().parse().unwrap_or(0);
                }
            }
        }
    }

    isa.pack_modes(&vals) as usize
}

fn parse_word(s: &str) -> Result<u64, String> {
    let s = s.replace('_', "");

    let parsed = if let Some(h) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        u64::from_str_radix(h, 16)
    } else if let Some(b) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
        u64::from_str_radix(b, 2)
    } else {
        s.parse::<u64>()
    };

    parsed.map_err(|_| format!("`{s}` is not a valid integer word"))
}

fn write_out(out: &Option<String>, content: &str) -> Result<(), String> {
    match out {
        Some(path) => {
            std::fs::write(path, content).map_err(|e| format!("cannot write `{path}`: {e}"))?;
            eprintln!("wrote {path}");
        }
        None => print!("{content}"),
    }
    Ok(())
}

// ----- subcommands -----

fn cmd_explain(args: &[String]) -> Result<(), String> {
    let a = parse_args(args)?;
    let l = load(&a)?;
    let isa = &l.isa;
    let combo = mode_combo(isa, a.mode.as_deref());

    let (word, d, stream_bytes) = if a.stream {
        let bytes: Vec<u8> = a
            .trailing
            .iter()
            .flat_map(|t| t.split(','))
            .filter(|s| !s.trim().is_empty())
            .map(|s| parse_word(s.trim()).map(|v| v as u8))
            .collect::<Result<_, _>>()?;

        let d = interp::decode_stream(isa, &bytes);

        if !d.context.is_empty() || d.prefix_len > 0 {
            let ctx: Vec<String> = d.context.iter().map(|(k, v)| format!("{k}={v}")).collect();
            let ctx = if ctx.is_empty() {
                "-".to_string()
            } else {
                ctx.join(", ")
            };
            println!(
                "stream {:02x?}  (prefix {} byte(s); context: {ctx})",
                bytes, d.prefix_len
            );
        }

        (0u64, d, Some(bytes))
    } else {
        let ws = a
            .trailing
            .first()
            .ok_or("missing word; usage: chipi explain <spec> -- <word>")?;
        let word = parse_word(ws)?;
        (word, interp::decode_mode(isa, combo, word), None)
    };

    println!("decoder {}", isa.decoder.name);

    if !isa.modes.is_empty() {
        let names: Vec<&str> = isa.modes.iter().map(|m| m.name.as_str()).collect();
        println!("modes   [{}]  combo {combo}", names.join(", "));
    }

    println!(
        "word    {word:#x}  ({}bit unit, {:?}, {}byte handle)",
        isa.decoder.unit_bits, isa.decoder.endian, isa.max_len_bytes
    );

    println!("\npath:");
    for step in &d.path {
        println!("    {step}");
    }

    if !d.is_valid() {
        println!("\nresult  decode_invalid (nothing matched)");
        return Ok(());
    }

    println!("\noperands:");
    if d.fields.is_empty() {
        println!("    (none)");
    }
    for f in &d.fields {
        println!(
            "    {:<8} [{:>2}:{:>2}]  raw={:#x}  value={}  -> {}",
            f.name, f.range.hi, f.range.lo, f.raw, f.value, f.rendered
        );
    }

    // For a byte stream on an ISA with `fetch(N)` operands, the plain decode only sees the opcode
    // window (operands read as 0). Run the contextual disassembler over the bytes so the operands
    // show their real values and the reported length includes the fetched bytes.
    let has_fetch = isa
        .instrs
        .iter()
        .any(|i| i.computed.iter().any(|c| interp::is_fetch(&c.expr)));

    if let (Some(bytes), true) = (&stream_bytes, has_fetch && d.is_valid()) {
        struct StreamBytes<'a>(&'a [u8]);
        impl interp::DisasmCtx for StreamBytes<'_> {
            fn read_u8(&self, addr: u64) -> u8 {
                self.0.get(addr as usize).copied().unwrap_or(0)
            }
        }
        let (text, len) = interp::disasm_ctx(isa, 0, &StreamBytes(bytes));
        println!("\nlength  {len} byte(s) (window + fetched operands)");
        println!("disasm  {text}");
    } else {
        println!("\nlength  {} byte(s)", d.len_bytes);
        if let Some(text) = &d.disasm {
            println!("disasm  {text}");
        }
    }

    Ok(())
}

fn cmd_asm(args: &[String]) -> Result<(), String> {
    use chipi_core::assemble::assemble_inst;

    let a = parse_args(args)?;
    let l = load(&a)?;
    let isa = &l.isa;

    if a.trailing.is_empty() {
        return Err("missing line; usage: chipi asm <spec> -- <asm line>".to_string());
    }

    let line = a.trailing.join(" ");
    let asm = assemble_inst(isa, &line).map_err(|e| e.to_string())?;
    let nbytes = isa.max_len_bytes.max(1) as usize;

    println!("{line}");
    println!("    word  {:#0width$x}", asm.word, width = nbytes * 2 + 2);

    let hex: Vec<String> = asm.bytes.iter().map(|b| format!("{b:02x}")).collect();
    println!("    bytes {}  ({} byte(s))", hex.join(" "), asm.bytes.len());

    // Name the leaf the assembler picked. Re-decoding the opcode alone reports the
    // default-mode leaf, which can differ for modal ISAs (for example the m=0/m=1 immediate split).
    println!("    assembled as {}", isa.instrs[asm.instr_index].name);

    Ok(())
}

fn cmd_stubs(args: &[String]) -> Result<(), String> {
    let a = parse_args(args)?;
    let l = load(&a)?;
    print_warnings(&l);
    write_out(&a.out, &chipi_backend_rust::emit_stubs(&l.isa))
}

fn cmd_emit(args: &[String]) -> Result<(), String> {
    let a = parse_args(args)?;
    let l = load(&a)?;
    print_warnings(&l);

    let target = a.target.as_deref().unwrap_or("rust");

    let style_only_rust = || -> Result<(), String> {
        if a.style.is_some() {
            return Err("`--style` only applies to `--target rust`".to_string());
        }
        Ok(())
    };

    let out = match target {
        "rust" => {
            let dispatch = match a.style.as_deref() {
                None | Some("newtype") => chipi_backend_rust::Dispatch::Newtype,
                Some("enum") => chipi_backend_rust::Dispatch::Enum,
                Some(other) => return Err(format!("unknown `--style` `{other}` (newtype|enum)")),
            };
            chipi_backend_rust::emit_decoder_with(
                &l.isa,
                chipi_backend_rust::EmitOptions { dispatch },
            )
        }
        "cpp" | "c++" => {
            style_only_rust()?;
            chipi_backend_cpp::emit_cpp(&l.isa)
        }
        "python" | "ida" | "binja" => {
            style_only_rust()?;
            chipi_backend_python::emit_python(&l.isa)
        }
        other => {
            return Err(format!(
                "unknown target `{other}` (rust|cpp|python|ida|binja)"
            ))
        }
    };

    write_out(&a.out, &out)
}

fn cmd_check(args: &[String]) -> Result<(), String> {
    let a = parse_args(args)?;
    let l = load(&a)?;
    let path = a.positionals.first().unwrap();
    print_warnings(&l);

    println!(
        "ok: `{path}`: {} instruction(s), {} warning(s)",
        l.isa.instrs.len(),
        l.isa.warnings.len()
    );

    if a.roundtrip {
        let scan = roundtrip_scan(&l.isa);
        println!(
            "roundtrip: {}/{} valid encodings re-encode to the same word",
            scan.ok, scan.valid
        );

        report_leaf_status(&l.isa, &scan);

        if scan.ok != scan.valid {
            return Err(format!(
                "{} word(s) failed to round-trip",
                scan.valid - scan.ok
            ));
        }
    }

    Ok(())
}

/// Tallies from one word-sample sweep: the encoder round-trip counts and the per-leaf
/// assembler round-trip counts.
struct ScanStats {
    /// Words that decoded to a leaf and re-encoded (successfully or not to the same word).
    valid: u64,
    /// Words whose re-encode matched under the care mask.
    ok: u64,
    /// Per-leaf: how often the sample decoded to it.
    seen: Vec<u64>,
    /// Per-leaf: how often its disassembly re-assembled to the same word.
    asm_ok: Vec<u64>,
}

/// One sweep over the word sample (every word for windows up to 16bit, otherwise a fixed
/// 200k-word LCG), decoding each word once and feeding both the encoder round-trip tally and
/// the per-leaf assembler tally from that single decode.
fn roundtrip_scan(isa: &Isa) -> ScanStats {
    let n = isa.instrs.len();
    let mut stats = ScanStats {
        valid: 0,
        ok: 0,
        seen: vec![0; n],
        asm_ok: vec![0; n],
    };

    let mut tally = |word: u64| {
        let d = chipi_core::interp::decode(isa, word);
        let Some(idx) = d.instr_index else { return };
        stats.seen[idx] += 1;
        let care = chipi_core::inverse::care_mask(isa, idx);

        // Encoder round-trip, as inverse::roundtrip does it (the decode already happened).
        let values: Vec<(String, i128)> =
            d.fields.iter().map(|f| (f.name.clone(), f.value)).collect();
        if let Ok(re) = chipi_core::inverse::encode(isa, idx, &values) {
            stats.valid += 1;
            if (word & care) == (re & care) {
                stats.ok += 1;
            }
        }

        // Assembler round-trip, as assemble::roundtrip_asm does it.
        if let Some(text) = &d.disasm {
            if let Ok(re) = chipi_core::assemble::assemble_line(isa, text) {
                if (word & care) == (re & care) {
                    stats.asm_ok[idx] += 1;
                }
            }
        }
    };

    let bits = isa.window_bits();
    if bits <= 16 {
        for w in 0..(1u64 << bits) {
            tally(w);
        }
    } else {
        let mask = ((1u128 << bits) - 1) as u64;
        let mut w = 0u64;
        for _ in 0..200_000u64 {
            w = w
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            tally(w & mask);
        }
    }

    stats
}

/// Per-leaf assembler status from the sweep: leaves whose disassembly fails to re-assemble
/// (or that the sample never reached). This is the honest map of what the text assembler can
/// and cannot reverse for this spec.
fn report_leaf_status(isa: &Isa, scan: &ScanStats) {
    let n = isa.instrs.len();
    let clean = (0..n)
        .filter(|&i| scan.seen[i] > 0 && scan.asm_ok[i] == scan.seen[i])
        .count();
    let broken: Vec<usize> = (0..n)
        .filter(|&i| scan.seen[i] > 0 && scan.asm_ok[i] < scan.seen[i])
        .collect();
    let unseen: Vec<usize> = (0..n).filter(|&i| scan.seen[i] == 0).collect();

    println!(
        "assembler: {clean}/{} leaves re-assemble their own disassembly over the sample",
        n
    );
    for &i in &broken {
        println!(
            "  not assemblable: {} ({}/{} sampled words re-assemble)",
            isa.instrs[i].name, scan.asm_ok[i], scan.seen[i]
        );
    }
    if !unseen.is_empty() {
        let names: Vec<&str> = unseen
            .iter()
            .map(|&i| isa.instrs[i].name.as_str())
            .collect();
        println!("  never decoded in the sample: {}", names.join(", "));
    }
}

fn cmd_dump_ir(args: &[String]) -> Result<(), String> {
    let a = parse_args(args)?;
    let isa = load(&a)?.isa;

    println!(
        "decoder {} {{ width = {}, bit_order = {:?}, endian = {:?} }}",
        isa.decoder.name, isa.decoder.unit_bits, isa.decoder.bit_order, isa.decoder.endian
    );
    println!(
        "max_len = {} byte(s), handle = {}",
        isa.max_len_bytes,
        isa.handle_ty()
    );

    if !isa.modes.is_empty() {
        println!("\nmodes:");
        for m in &isa.modes {
            println!(
                "    {} : {:?} cardinality={} default={}",
                m.name, m.kind, m.cardinality, m.default
            );
        }
    }

    println!("\nselectors:");
    for s in &isa.selectors {
        println!("    {} = bits[{}:{}]", s.name, s.range.hi, s.range.lo);
    }

    println!("\ntypes/operands:");
    for t in &isa.types {
        println!(
            "    {} {} = {:?}  xforms={:?}  disp={:?}",
            if t.is_operand { "operand" } else { "type" },
            t.name,
            t.base,
            t.xforms,
            t.disp
        );
    }

    println!("\ninstructions:");
    for inst in &isa.instrs {
        let fixed: Vec<String> = inst
            .fixed
            .iter()
            .map(|c| format!("[{}:{}]={:#x}", c.range.hi, c.range.lo, c.value))
            .collect();
        let fields: Vec<String> = inst
            .fields
            .iter()
            .map(|f| format!("{}:[{}:{}]", f.name, f.range.hi, f.range.lo))
            .collect();

        println!(
            "    {:<8} fixed[{}] fields[{}]",
            inst.name,
            fixed.join(" "),
            fields.join(" ")
        );
    }

    Ok(())
}

fn cmd_dump_tree(args: &[String]) -> Result<(), String> {
    use chipi_core::tree::{Residual, Slot};

    let a = parse_args(args)?;
    let isa = load(&a)?.isa;
    let t = &isa.tree;

    println!(
        "primary: {} bits[{}:{}]  lowering={}  slots={}",
        t.primary.name,
        t.primary.range.hi,
        t.primary.range.lo,
        t.primary_lowering.label(),
        t.slots.len()
    );

    println!("opcodes (id: name):");
    for (id, op) in t.opcodes.iter().enumerate() {
        println!("    {id}: {}", op.name);
    }

    println!("primary slots (mapped):");
    for (v, slot) in t.slots.iter().enumerate() {
        match slot {
            Slot::Leaf(id) => println!("    [{v:#x}] -> {}", t.opcodes[*id].name),
            Slot::Residual(ri) => println!("    [{v:#x}] -> residual #{ri}"),
            Slot::Invalid => {}
        }
    }

    println!("residual tables:");
    for (ri, r) in t.residuals.iter().enumerate() {
        match r {
            Residual::Keyed {
                key,
                lowering,
                arms,
                default,
            } => {
                println!(
                    "    #{ri}: keyed {} bits[{}:{}]  lowering={}  arms={} default={}",
                    key.name,
                    key.range.hi,
                    key.range.lo,
                    lowering.label(),
                    arms.len(),
                    t.opcodes[*default].name
                );

                for (k, id) in arms {
                    println!("         {k:#x} -> {}", t.opcodes[*id].name);
                }
            }
            Residual::Sparse { lowering, arms } => {
                println!(
                    "    #{ri}: sparse  lowering={}  arms={}",
                    lowering.label(),
                    arms.len()
                );

                for arm in arms {
                    println!(
                        "         (word & {:#x}) == {:#x} -> {}",
                        arm.mask, arm.val, t.opcodes[arm.opcode].name
                    );
                }
            }
        }
    }

    println!("unmapped primary keys -> decode_invalid: {}", t.n_invalid);

    Ok(())
}
