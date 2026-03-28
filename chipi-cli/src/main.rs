use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};

use chipi_core::backend;
use chipi_core::config::{self, Dispatch, GenTarget};
use chipi_core::error::Errors;

#[derive(Parser)]
#[command(name = "chipi", version, about = "Code generator for .chipi decoder description files")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate decoder source code
    Gen(GenArgs),
    /// Generate emulator dispatch LUT
    Lut(LutArgs),
    /// Generate stub handler skeletons with todo!() bodies
    Stubs(StubsArgs),
}

#[derive(Parser)]
struct GenArgs {
    /// Input .chipi file (optional if --config specifies it)
    input: Option<String>,

    /// Path to chipi.toml config file
    #[arg(short, long)]
    config: Option<PathBuf>,

    /// Target language (default: rust)
    #[arg(short, long, default_value = "rust")]
    lang: String,

    /// Output file path (overrides config)
    #[arg(short, long)]
    output: Option<String>,

    /// Default dispatch strategy: jump_table, fn_ptr_lut
    #[arg(short, long)]
    dispatch: Option<DispatchArg>,

    /// Per-decoder dispatch override, e.g. --dispatch-for GcDspExt=jump_table
    #[arg(long, value_parser = parse_dispatch_for)]
    dispatch_for: Vec<(String, DispatchArg)>,

    /// Type mapping, e.g. --type-map reg5=crate::dsp::DspReg (repeatable)
    #[arg(short, long, value_parser = parse_key_value)]
    type_map: Vec<(String, String)>,

    /// Run formatter (e.g. rustfmt) on output
    #[arg(long)]
    format: bool,

    /// Print generated source to stdout instead of writing to file
    #[arg(long)]
    dry_run: bool,

    /// Print the parsed Decoder IR and exit
    #[arg(long)]
    dump_ir: bool,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Parser)]
struct LutArgs {
    /// Input .chipi file (optional if --config specifies it)
    input: Option<String>,

    /// Path to chipi.toml config file
    #[arg(short, long)]
    config: Option<PathBuf>,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Parser)]
struct StubsArgs {
    /// Input .chipi file (required unless --config provides it)
    input: Option<String>,

    /// Path to chipi.toml config file (reads \[\[lut\]\] for groups, ctx_type, etc.)
    #[arg(short, long)]
    config: Option<PathBuf>,

    /// Output file path (required unless --config provides it)
    #[arg(short, long)]
    output: Option<String>,

    /// Run formatter on output
    #[arg(long)]
    format: bool,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Clone, Debug)]
enum DispatchArg {
    JumpTable,
    FnPtrLut,
}

impl std::str::FromStr for DispatchArg {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "jump_table" | "jump-table" => Ok(DispatchArg::JumpTable),
            "fn_ptr_lut" | "fn-ptr-lut" => Ok(DispatchArg::FnPtrLut),
            _ => Err(format!(
                "unknown dispatch strategy: '{}' (expected 'jump_table' or 'fn_ptr_lut')",
                s
            )),
        }
    }
}

impl From<DispatchArg> for Dispatch {
    fn from(arg: DispatchArg) -> Dispatch {
        match arg {
            DispatchArg::JumpTable => Dispatch::JumpTable,
            DispatchArg::FnPtrLut => Dispatch::FnPtrLut,
        }
    }
}

fn parse_key_value(s: &str) -> Result<(String, String), String> {
    let pos = s
        .find('=')
        .ok_or_else(|| format!("expected KEY=VALUE, got '{}'", s))?;
    Ok((s[..pos].to_string(), s[pos + 1..].to_string()))
}

fn parse_dispatch_for(s: &str) -> Result<(String, DispatchArg), String> {
    let pos = s
        .find('=')
        .ok_or_else(|| format!("expected DECODER=STRATEGY, got '{}'", s))?;
    let name = s[..pos].to_string();
    let strategy = s[pos + 1..].parse()?;
    Ok((name, strategy))
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Gen(args) => run_gen(args),
        Commands::Lut(args) => run_lut(args),
        Commands::Stubs(args) => run_stubs(args),
    };

    if let Err(e) = result {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}

fn run_gen(args: GenArgs) -> Result<(), Box<dyn std::error::Error>> {
    let config_path = args.config.or_else(|| {
        let default = PathBuf::from("chipi.toml");
        if default.exists() {
            Some(default)
        } else {
            None
        }
    });

    let targets = if let Some(ref config_path) = config_path {
        let cfg = config::load_config(config_path)?;
        let base_dir = config_path.parent().unwrap_or(Path::new("."));
        let mut targets = cfg.targets;
        for t in &mut targets {
            config::resolve_gen_paths(t, base_dir);
        }
        targets
    } else {
        let input = args
            .input
            .as_deref()
            .ok_or("no input file specified (provide <input.chipi> or --config)")?;
        let output = if args.dump_ir || args.dry_run {
            args.output.as_deref().unwrap_or_default()
        } else {
            args.output
                .as_deref()
                .ok_or("no output file specified (provide --output or use --config)")?
        };

        vec![GenTarget::new(input, &args.lang, output)]
    };

    let targets: Vec<GenTarget> = targets
        .into_iter()
        .filter(|t| t.lang == args.lang)
        .map(|mut t| {
            if let Some(ref input) = args.input {
                if config_path.is_some() {
                    t.input.clone_from(input);
                }
            }
            if let Some(ref output) = args.output {
                t.output.clone_from(output);
            }
            if let Some(ref dispatch) = args.dispatch {
                t.dispatch = dispatch.clone().into();
            }
            for (name, strategy) in &args.dispatch_for {
                t.dispatch_overrides
                    .insert(name.clone(), strategy.clone().into());
            }
            for (name, ty) in &args.type_map {
                t.type_map.insert(name.clone(), ty.clone());
            }
            if args.format {
                t.format = true;
            }
            t
        })
        .collect();

    if targets.is_empty() {
        return Err("no matching targets found for language".into());
    }

    for target in &targets {
        let def = chipi_core::parse(&target.input)?;
        let validated = chipi_core::validate::validate(&def)
            .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

        if args.dump_ir {
            println!("Decoder: {}", validated.config.name);
            println!("Width: {}", validated.config.width);
            println!("Instructions: {}", validated.instructions.len());
            for instr in &validated.instructions {
                println!(
                    "  - {} ({} fields)",
                    instr.name,
                    instr.resolved_fields.len()
                );
            }
            println!("Sub-decoders: {}", validated.sub_decoders.len());
            for sd in &validated.sub_decoders {
                println!(
                    "  - {} ({} instructions, fragments: {:?})",
                    sd.name,
                    sd.instructions.len(),
                    sd.fragment_names
                );
            }
            println!("Maps: {}", validated.maps.len());
            for m in &validated.maps {
                println!("  - {} ({} entries)", m.name, m.entries.len());
            }
            continue;
        }

        let backend = backend::get_backend(&target.lang)
            .ok_or_else(|| format!("unknown language: '{}'", target.lang))?;

        if let Some(ref opts) = target.lang_options {
            backend
                .validate_lang_options(opts)
                .map_err(|errs| errs.join("; "))?;
        }

        if args.verbose {
            eprintln!(
                "generating {} from {} -> {}",
                target.lang, target.input, target.output
            );
            eprintln!("  dispatch: {:?}", target.dispatch);
            if !target.dispatch_overrides.is_empty() {
                eprintln!("  dispatch_overrides: {:?}", target.dispatch_overrides);
            }
            if !target.type_map.is_empty() {
                eprintln!("  type_map: {:?}", target.type_map);
            }
        }

        let code = backend.generate(&validated, target)?;

        if args.dry_run {
            print!("{}", code);
        } else {
            std::fs::write(&target.output, &code)?;

            if target.format {
                if let Some(cmd) = backend.formatter_command() {
                    backend::run_formatter(cmd, &target.output);
                }
            }

            if args.verbose {
                eprintln!("  wrote {}", target.output);
            }
        }
    }

    Ok(())
}

fn run_lut(args: LutArgs) -> Result<(), Box<dyn std::error::Error>> {
    let config_path = args.config.or_else(|| {
        let default = PathBuf::from("chipi.toml");
        if default.exists() {
            Some(default)
        } else {
            None
        }
    });

    let config_path = config_path.ok_or("no config file found (provide --config or create chipi.toml)")?;
    let mut cfg = config::load_config(&config_path)?;
    let base_dir = config_path.parent().unwrap_or(Path::new("."));

    if cfg.lut.is_empty() {
        return Err("no [[lut]] targets defined in config".into());
    }

    for target in &mut cfg.lut {
        config::resolve_lut_paths(target, base_dir);

        if args.verbose {
            eprintln!("generating lut from {} -> {}", target.input, target.output);
            eprintln!("  handler_mod: {}", target.handler_mod);
            eprintln!("  ctx_type: {}", target.ctx_type);
            eprintln!("  dispatch: {:?}", target.dispatch);
            if !target.groups.is_empty() {
                eprintln!("  groups: {} defined", target.groups.len());
            }
        }

        chipi_core::LutBuilder::run_target(target)?;

        if args.verbose {
            eprintln!("  wrote {}", target.output);
            if let Some(ref p) = target.instr_type_output {
                eprintln!("  wrote {}", p);
            }
        }
    }

    Ok(())
}

fn run_stubs(args: StubsArgs) -> Result<(), Box<dyn std::error::Error>> {
    // Try to load a [[lut]] target from config for groups/ctx_type/etc.
    let config_path = args.config.or_else(|| {
        let default = PathBuf::from("chipi.toml");
        if default.exists() {
            Some(default)
        } else {
            None
        }
    });

    let lut_target = if let Some(ref config_path) = config_path {
        let mut cfg = config::load_config(config_path)?;
        let base_dir = config_path.parent().unwrap_or(Path::new("."));
        if let Some(target) = cfg.lut.first_mut() {
            config::resolve_lut_paths(target, base_dir);
            Some(target.clone())
        } else {
            None
        }
    } else {
        None
    };

    let (input, output) = if let Some(ref lut) = lut_target {
        (
            args.input.as_deref().unwrap_or(&lut.input).to_string(),
            args.output
                .as_deref()
                .ok_or("no output file specified (provide --output)")?,
        )
    } else {
        (
            args.input
                .as_deref()
                .ok_or("no input file specified (provide <input.chipi> or --config with [[lut]])")?
                .to_string(),
            args.output
                .as_deref()
                .ok_or("no output file specified (provide --output)")?,
        )
    };

    let def = chipi_core::parse(&input)?;
    let validated = chipi_core::validate::validate(&def)
        .map_err(|errs| Box::new(Errors(errs)) as Box<dyn std::error::Error>)?;

    // Build group_to_instrs from lut target config (if available)
    let (ctx_type, group_to_instrs, lut_mod, instr_type) = if let Some(ref lut) = lut_target {
        (
            lut.ctx_type.as_str(),
            &lut.groups,
            lut.lut_mod.as_deref(),
            lut.instr_type.as_deref(),
        )
    } else {
        ("Ctx", &Default::default(), None, None)
    };

    let code = chipi_core::lut_gen::generate_stubs_code(
        &validated,
        ctx_type,
        group_to_instrs,
        lut_mod,
        instr_type,
    );

    std::fs::write(output, &code)?;

    if args.format {
        backend::run_formatter(&["rustfmt"], output);
    }

    if args.verbose {
        if lut_target.is_some() {
            eprintln!(
                "wrote stubs to {} (with {} groups from config)",
                output,
                group_to_instrs.len()
            );
        } else {
            eprintln!("wrote stubs to {} (no groups)", output);
        }
    }
    Ok(())
}
