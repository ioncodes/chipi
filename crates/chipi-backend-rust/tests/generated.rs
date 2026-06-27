//! Compile every example's generated Rust decoder with `rustc` and check that `classify` and the
//! `Display` disassembler match the oracle on a sample of words.

use chipi_core::interp::{decode, decode_mode, Decoded};
use chipi_core::{compile, Isa};
use std::path::PathBuf;
use std::process::Command;

macro_rules! example {
    ($n:literal) => {
        (
            $n,
            include_str!(concat!("../../../examples/", $n, ".chipi")),
        )
    };
}

const VALID_EXAMPLES: &[(&str, &str)] = &[
    example!("mips"),
    example!("gekko"),
    example!("gc_dsp"),
    example!("gb"),
    example!("sparse_demo"),
    example!("tags_demo"),
    example!("cond_demo"),
    example!("names_demo"),
    example!("subdecoder_demo"),
    example!("modes_demo"),
    example!("rv32i"),
    example!("riscv"),
    example!("riscv_rvc"),
    example!("aarch64"),
    example!("gba_arm"),
    example!("x86_prefix"),
    example!("snes_disasm"),
];

fn modal(isa: &Isa) -> bool {
    !isa.modes.is_empty()
}

fn emit_display(isa: &Isa) -> bool {
    !modal(isa)
        && !isa.instrs.iter().any(|i| {
            i.computed
                .iter()
                .any(|c| chipi_core::interp::fetch_width(&c.expr).is_some())
        })
}

fn dec(isa: &Isa, combo: usize, w: u64) -> Decoded {
    if modal(isa) {
        decode_mode(isa, combo, w)
    } else {
        decode(isa, w)
    }
}

/// A handful of valid words plus a couple of invalid ones, picked the same way every run.
fn sample_words(isa: &Isa, combo: usize) -> Vec<u64> {
    let bits = isa.window_bits();
    let (mut valid, mut invalid) = (Vec::new(), Vec::new());

    let consider = |w: u64, valid: &mut Vec<u64>, invalid: &mut Vec<u64>| {
        let d = dec(isa, combo, w);
        if d.is_valid() {
            if valid.len() < 10 {
                valid.push(w);
            }
        } else if invalid.len() < 2 {
            invalid.push(w);
        }
    };

    if bits <= 16 {
        for w in 0..(1u64 << bits) {
            consider(w, &mut valid, &mut invalid);
            if valid.len() >= 10 && invalid.len() >= 2 {
                break;
            }
        }
    } else {
        let mask = ((1u128 << bits) - 1) as u64;
        let mut w = 0u64;
        for _ in 0..200_000u64 {
            w = w
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            consider(w & mask, &mut valid, &mut invalid);
            if valid.len() >= 10 && invalid.len() >= 2 {
                break;
            }
        }
    }

    valid.into_iter().chain(invalid).collect()
}

fn out_dir() -> PathBuf {
    let d = std::env::temp_dir().join("chipi-rustgen-tests");
    std::fs::create_dir_all(&d).unwrap();
    d
}

fn compile_and_run(name: &str, program: &str) -> Vec<String> {
    let dir = out_dir();
    let src = dir.join(format!("{name}.rs"));
    let bin = dir.join(name);
    std::fs::write(&src, program).unwrap();

    let status = Command::new("rustc")
        .args([
            "--edition",
            "2021",
            "--cfg",
            "feature=\"disasm\"",
            "--cap-lints",
            "allow",
            "-o",
        ])
        .arg(&bin)
        .arg(&src)
        .status()
        .expect("rustc should be available");
    assert!(
        status.success(),
        "`{name}` generated decoder failed to compile"
    );

    let output = Command::new(&bin).output().expect("run generated harness");
    assert!(output.status.success(), "`{name}` harness exited non-zero");

    String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect()
}

#[test]
fn generated_decoders_match_oracle() {
    for &(name, src) in VALID_EXAMPLES {
        let isa = compile(src).unwrap_or_else(|_| panic!("`{name}` compile"));
        let combo = isa.default_combo() as usize;
        let words = sample_words(&isa, combo);
        assert!(!words.is_empty(), "`{name}` produced no sample words");
        let disp = emit_display(&isa);

        // Build a harness program: the generated module + a main printing classify (and Display).
        let mut prog = chipi_backend_rust::emit_decoder(&isa);
        prog.push_str("\nfn main() {\n");

        let mut expected = Vec::new();
        for &w in &words {
            let d = dec(&isa, combo, w);
            let call = if modal(&isa) {
                format!("classify({combo}, {w})")
            } else {
                format!("classify({w})")
            };
            if disp {
                prog.push_str(&format!(
                    "    println!(\"{{}}|{{}}\", {call}, format!(\"{{}}\", Instruction({w})));\n"
                ));
                expected.push(format!(
                    "{}|{}",
                    d.opcode_id,
                    d.disasm.clone().unwrap_or_else(|| "(invalid)".into())
                ));
            } else {
                prog.push_str(&format!("    println!(\"{{}}\", {call});\n"));
                expected.push(format!("{}", d.opcode_id));
            }
        }

        prog.push_str("}\n");

        let got = compile_and_run(name, &prog);
        assert_eq!(
            got, expected,
            "`{name}` generated output mismatch vs oracle"
        );
    }
}

#[test]
fn hot_path_is_allocation_free_and_total() {
    let isa = compile(include_str!("../../../examples/mips.chipi")).unwrap();
    let code = chipi_backend_rust::emit_decoder(&isa);
    assert!(
        code.contains("#[cfg(feature = \"disasm\")]"),
        "disasm must be feature-gated"
    );
    assert!(code.contains("pub fn default_table"));
    assert!(code.contains("decode_invalid"));
    assert!(
        !code.contains("Option<usize>"),
        "classification must be total"
    );
    assert!(
        code.contains(">>") && code.contains('&'),
        "accessors are shift/mask"
    );
    assert!(code.contains("#[inline(always)]"));

    let hot = code.split("#[cfg(feature = \"disasm\")]").next().unwrap();
    for bad in ["Vec<", "String", "Box::", ".to_string()", "format!", "vec!"] {
        assert!(!hot.contains(bad), "hot path contains `{bad}`");
    }
}

#[test]
fn stubs_emit() {
    let isa = compile(include_str!("../../../examples/mips.chipi")).unwrap();
    let stubs = chipi_backend_rust::emit_stubs(&isa);
    assert!(stubs.contains("impl Ops for MyCpu"));
    assert!(stubs.contains("fn add(&mut self, inst: Instruction)"));
}

/// The generated contextual disassembler (`fetch(N)` operands + `:sym`) must match the oracle.
#[test]
fn generated_disasm_ctx_matches_oracle() {
    let isa = compile(include_str!("../../../examples/snes_disasm.chipi")).unwrap();
    let pcs = [0u64, 1, 4];

    // A small program image: nop; lda #$1234; jmp $9000.
    struct Mem;
    impl chipi_core::interp::DisasmCtx for Mem {
        fn read_u8(&self, addr: u64) -> u8 {
            [0xEAu8, 0xA9, 0x34, 0x12, 0x4C, 0x00, 0x90]
                .get(addr as usize)
                .copied()
                .unwrap_or(0)
        }
    }

    let expected: Vec<String> = pcs
        .iter()
        .map(|&pc| {
            let (text, len) = chipi_core::interp::disasm_ctx(&isa, pc, &Mem);
            format!("{text}|{len}")
        })
        .collect();

    let mut prog = chipi_backend_rust::emit_decoder(&isa);
    prog.push_str(
        r#"
struct Mem;
impl DisasmCtx for Mem {
    fn read_u8(&self, addr: u64) -> u8 {
        [0xEAu8, 0xA9, 0x34, 0x12, 0x4C, 0x00, 0x90].get(addr as usize).copied().unwrap_or(0)
    }
}
fn main() {
    for pc in [0u64, 1, 4] {
        let (text, len) = disasm_ctx(pc, &Mem);
        println!("{}|{}", text, len);
    }
}
"#,
    );

    let got = compile_and_run("snes_ctx", &prog);
    assert_eq!(
        got, expected,
        "generated disasm_ctx output mismatch vs oracle"
    );
}

/// Exercises how `disasm_ctx` delegates to `Display`. The spec emits both renderers (it has `:sym`
/// but no `fetch`), so plain arms delegate to `Display`. The `:sym` arm and the
/// signed-explicit-hex arm keep their own renderers. All paths must still match the oracle.
#[test]
fn disasm_ctx_delegation_matches_oracle() {
    const SPEC: &str = r#"
decoder T { width = 16 bit_order = lsb0 endian = little }
selector op [12:15]
operand reg = u4 { display("r{}") }
type s8 = i8 { sign_extend(8) }
nop  op=0 | "nop"
mov  op=1 d:reg[8:11] s:reg[4:7] | "mov {d}, {s}"
jmp  op=2 t:u8[0:7] | "jmp {t:sym}"
addx op=3 v:s8[0:7] | "addx {v:x}"
ld   op=4 a:u8[0:7] | "ld {a:x}"
"#;
    let isa = compile(SPEC).expect("synthetic spec compiles");

    // nop(deleg), mov(deleg), jmp(:sym, custom), addx v=-1 and v=127 (signed-hex, custom),
    // ld(deleg) and an invalid opcode.
    let words: [u16; 7] = [0x0000, 0x1230, 0x2042, 0x30FF, 0x307F, 0x40AB, 0xF000];

    struct Mem(u16);
    impl chipi_core::interp::DisasmCtx for Mem {
        fn read_u8(&self, addr: u64) -> u8 {
            self.0
                .to_le_bytes()
                .get(addr as usize)
                .copied()
                .unwrap_or(0)
        }
    }
    let expected: Vec<String> = words
        .iter()
        .map(|&w| {
            let (text, len) = chipi_core::interp::disasm_ctx(&isa, 0, &Mem(w));
            format!("{text}|{len}")
        })
        .collect();

    let mut prog = chipi_backend_rust::emit_decoder(&isa);
    prog.push_str(
        r#"
struct Mem(u16);
impl DisasmCtx for Mem {
    fn read_u8(&self, addr: u64) -> u8 { self.0.to_le_bytes().get(addr as usize).copied().unwrap_or(0) }
}
fn main() {
    for w in [0x0000u16, 0x1230, 0x2042, 0x30FF, 0x307F, 0x40AB, 0xF000] {
        let (text, len) = disasm_ctx(0, &Mem(w));
        println!("{}|{}", text, len);
    }
}
"#,
    );

    let got = compile_and_run("deleg_ctx", &prog);
    assert_eq!(
        got, expected,
        "disasm_ctx delegation/guard mismatch vs oracle"
    );
}

fn emit_enum(isa: &Isa) -> String {
    chipi_backend_rust::emit_decoder_with(
        isa,
        chipi_backend_rust::EmitOptions {
            dispatch: chipi_backend_rust::Dispatch::Enum,
        },
    )
}

/// The nested-enum decoder's eager `decode` + `render` must match the oracle's contextual
/// disassembler on a fetch/`:sym` spec. Operands are extracted once at decode, then the variant is
/// rendered from those pre-extracted fields.
#[test]
fn enum_decode_render_matches_oracle() {
    use chipi_core::interp::DisasmCtx as _;

    let isa = compile(include_str!("../../../examples/snes_disasm.chipi")).unwrap();
    let pcs = [0u64, 1, 4];

    struct Mem;
    impl chipi_core::interp::DisasmCtx for Mem {
        fn read_u8(&self, addr: u64) -> u8 {
            [0xEAu8, 0xA9, 0x34, 0x12, 0x4C, 0x00, 0x90]
                .get(addr as usize)
                .copied()
                .unwrap_or(0)
        }
    }

    let expected: Vec<String> = pcs
        .iter()
        .map(|&pc| {
            let word = Mem.read_u8(pc) as u64;
            let name = chipi_core::interp::decode(&isa, word).opcode_name;
            let (text, len) = chipi_core::interp::disasm_ctx(&isa, pc, &Mem);
            format!("{name}|{len}|{text}")
        })
        .collect();

    let mut prog = emit_enum(&isa);
    prog.push_str(
        r#"
struct Mem;
impl DisasmCtx for Mem {
    fn read_u8(&self, addr: u64) -> u8 {
        [0xEAu8, 0xA9, 0x34, 0x12, 0x4C, 0x00, 0x90].get(addr as usize).copied().unwrap_or(0)
    }
}
fn main() {
    for pc in [0u64, 1, 4] {
        let (inst, len) = decode(pc, &Mem);
        println!("{}|{}|{}", inst.name(), len, inst.render(pc, &Mem));
    }
}
"#,
    );

    let got = compile_and_run("snes_enum_ctx", &prog);
    assert_eq!(got, expected, "enum decode/render mismatch vs oracle");
}

/// Regression: an operand named `r` must not shadow the enum renderer's output `String`. `sparse_demo`
/// binds `r:reg`, which previously collided with a hardcoded `r` accumulator. Compile the emitted enum
/// module as a library (a pure compile check).
#[test]
fn enum_render_survives_operand_named_r() {
    let isa = compile(include_str!("../../../examples/sparse_demo.chipi")).unwrap();
    let prog = emit_enum(&isa);
    assert!(
        prog.contains("Instruction::A { r }"),
        "expected an `r` operand binding in the generated enum:\n{prog}"
    );

    let dir = out_dir();
    let src = dir.join("sparse_enum_lib.rs");
    std::fs::write(&src, &prog).unwrap();

    let status = Command::new("rustc")
        .args([
            "--edition",
            "2021",
            "--crate-type",
            "lib",
            "--cfg",
            "feature=\"disasm\"",
            "--cap-lints",
            "allow",
            "-o",
        ])
        .arg(dir.join("libsparse_enum.rlib"))
        .arg(&src)
        .status()
        .expect("rustc should be available");
    assert!(
        status.success(),
        "sparse_demo enum (operand named `r`) must compile"
    );
}

/// A modal, grouped spec exercising the full enum surface: dispatch groups (sub-enums), a no-operand
/// leaf, a signed `:rel` branch, an absolute fetch, a two-field op and a mode-split immediate.
/// Render is checked at the default combo (the oracle's `disasm_ctx` decodes mode-agnostically),
/// and the mode-split leaf is checked under both combos against `decode_mode`.
#[test]
fn enum_modal_grouped_matches_oracle() {
    const SPEC: &str = r#"
decoder T {
    width = 8
    bit_order = lsb0
    endian = little
    mode m: bool = 1
}
selector op [0:7]

nop       op=0x00 | "nop"
inx       op=0x01 | "inx"
dex       op=0x02 | "dex"
beq_rel   op=0x10 disp:i8 = fetch(8) | "beq ${disp:rel}"
bne_rel   op=0x11 disp:i8 = fetch(8) | "bne ${disp:rel}"
lda_imm8  m=1 op=0x20 imm:u8  = fetch(8)  | "lda #${imm:02x}"
lda_imm16 m=0 op=0x20 imm:u16 = fetch(16) | "lda #${imm:04x}"
sta_abs   op=0x30 addr:u16 = fetch(16) | "sta ${addr:04x}"
mvn_bm    op=0x40 dst:u8 = fetch(8) src:u8 = fetch(8) | "mvn ${src:02x},${dst:02x}"

dispatch reg_step { inx, dex }
dispatch branches { beq_rel, bne_rel }
"#;
    let isa = compile(SPEC).expect("synthetic modal spec compiles");
    let default_combo = isa.default_combo() as usize;

    // 0:nop 1:inx 2:beq +0x7f 4:lda #$34 6:sta $9000 9:mvn src=2,dst=1
    const IMAGE: [u8; 12] = [
        0x00, 0x01, 0x10, 0x7F, 0x20, 0x34, 0x30, 0x00, 0x90, 0x40, 0x01, 0x02,
    ];
    let render_pcs = [0u64, 1, 2, 4, 6, 9];

    let _ = IMAGE;

    // The render text (including `:rel` target resolution, which the pure oracle lacks) must match
    // the shipped newtype backend exactly, so switching a consumer to enum dispatch keeps
    // disassembly output unchanged. Both decode mode-agnostically here. The newtype `disasm_ctx`
    // reads `ctx.mode()`, the enum is given the default combo.
    let mut nt = chipi_backend_rust::emit_decoder(&isa);
    nt.push_str(
        r#"
struct Mem;
impl DisasmCtx for Mem {
    fn read_u8(&self, addr: u64) -> u8 {
        const IMAGE: [u8; 12] = [0x00, 0x01, 0x10, 0x7F, 0x20, 0x34, 0x30, 0x00, 0x90, 0x40, 0x01, 0x02];
        IMAGE.get(addr as usize).copied().unwrap_or(0)
    }
    fn mode(&self, name: &str) -> u64 { if name == "m" { 1 } else { 0 } }
}
fn main() {
    for pc in [0u64, 1, 2, 4, 6, 9] {
        let (text, len) = disasm_ctx(pc, &Mem);
        println!("R|{}|{}", len, text);
    }
}
"#,
    );
    let expected_render = compile_and_run("enum_modal_nt", &nt);

    // The mode-split leaf must classify per combo exactly like the oracle's `decode_mode`.
    let oracle_modes: Vec<String> = [0usize, 1]
        .iter()
        .map(|&combo| {
            let name = chipi_core::interp::decode_mode(&isa, combo, IMAGE[4] as u64).opcode_name;
            format!("M|{combo}|{name}")
        })
        .collect();

    let mut en = emit_enum(&isa);
    en.push_str(&format!(
        r#"
struct Mem;
impl DisasmCtx for Mem {{
    fn read_u8(&self, addr: u64) -> u8 {{
        const IMAGE: [u8; 12] = [0x00, 0x01, 0x10, 0x7F, 0x20, 0x34, 0x30, 0x00, 0x90, 0x40, 0x01, 0x02];
        IMAGE.get(addr as usize).copied().unwrap_or(0)
    }}
}}
fn main() {{
    for pc in [0u64, 1, 2, 4, 6, 9] {{
        let (inst, len) = decode({default_combo}, pc, &Mem);
        println!("R|{{}}|{{}}", len, inst.render(pc, &Mem));
    }}
    for combo in [0usize, 1] {{
        let (inst, _) = decode(combo, 4, &Mem);
        println!("M|{{}}|{{}}", combo, inst.name());
    }}
}}
"#
    ));
    let got = compile_and_run("enum_modal_en", &en);

    assert_eq!(
        &got[..render_pcs.len()],
        &expected_render[..],
        "enum render must match the newtype backend"
    );
    assert_eq!(
        &got[render_pcs.len()..],
        &oracle_modes[..],
        "enum mode-split decode must match the oracle"
    );
}

/// Unsupported spec shapes must produce a clear `compile_error!`, not silently wrong output. The
/// `x86_prefix` example carries a `prefix` scan, which the single-window enum decoder cannot model.
#[test]
fn enum_gates_unsupported_specs() {
    let isa = compile(include_str!("../../../examples/x86_prefix.chipi")).unwrap();
    assert!(isa.prefix.is_some(), "x86_prefix should have a prefix scan");
    let out = emit_enum(&isa);
    assert!(
        out.contains("compile_error!"),
        "prefix spec should be gated in enum mode"
    );
}
