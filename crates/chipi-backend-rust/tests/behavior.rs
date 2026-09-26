//! Behavioral coverage for the dispatch layers that `generated.rs` only string-smoke-checks:
//! the Handler fn-pointer table (`default_table`/`dispatch`/`run`), the Ops trait layer
//! (`dispatch_ops`/`run_ops`, including grouped handlers), the prefix scan
//! (`scan_prefixes`/`decode_stream`) and the enum-style backend's `decode`.
//!
//! Each test emits the module for a real example spec, appends a recording harness, compiles it
//! with `rustc` and compares the observed handler sequence and operand values against
//! `chipi_core::interp`, the oracle.

mod common;

use chipi_core::interp::{decode, decode_stream, Decoded};
use chipi_core::{compile, Isa};
use common::{compile_and_run, pascal};
use std::fmt::Write as _;

/// Deterministic sample words: every leaf's fixed bits under three don't-care fills (kept only
/// when the oracle still routes the word to that leaf), plus two invalid words. Asserts that
/// every instruction of the spec is represented, so no handler goes unexercised.
fn sample_words(isa: &Isa) -> Vec<u64> {
    let mask = ((1u128 << isa.window_bits()) - 1) as u64;
    let mut words = Vec::new();
    let mut covered = vec![false; isa.instrs.len()];

    for (idx, inst) in isa.instrs.iter().enumerate() {
        let (fixed_mask, fixed_val) = inst.fixed_mask_val();
        for fill in [0u64, 0xAAAA_AAAA_AAAA_AAAA, u64::MAX] {
            let w = (fixed_val | (fill & !fixed_mask)) & mask;
            if decode(isa, w).opcode_name == inst.name {
                covered[idx] = true;
                if !words.contains(&w) {
                    words.push(w);
                }
            }
        }
    }
    assert!(
        covered.iter().all(|&c| c),
        "sample_words missed an instruction"
    );

    // Two invalid words, found with the same LCG walk the smoke tests use.
    let mut found = 0;
    let mut w = 1u64;
    for _ in 0..200_000u32 {
        w = w
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        let cand = w & mask;
        if !decode(isa, cand).is_valid() && !words.contains(&cand) {
            words.push(cand);
            found += 1;
            if found == 2 {
                break;
            }
        }
    }
    assert_eq!(found, 2, "no invalid sample words found");

    words
}

/// The dispatch group a leaf belongs to, as `(group name, Kind variant)`.
fn group_of<'a>(isa: &'a Isa, leaf: &str) -> Option<(&'a str, String)> {
    for g in &isa.groups {
        if g.members.iter().any(|m| m == leaf) {
            return Some((g.name.as_str(), pascal(leaf)));
        }
    }
    None
}

/// `"add rd=3 rs=1 rt=2"`: the leaf name and its bound operands in declaration order, with the
/// oracle's post-transform values. The recording harnesses print the exact same shape from the
/// generated accessors (newtype) or variant payloads (enum).
fn field_line(isa: &Isa, d: &Decoded) -> String {
    let inst = &isa.instrs[d.instr_index.expect("valid decode")];
    let mut line = d.opcode_name.clone();
    for f in &inst.fields {
        let fv = d
            .fields
            .iter()
            .find(|fv| fv.name == f.name)
            .expect("oracle binds every declared operand");
        let _ = write!(line, " {}={}", f.name, fv.value);
    }
    line
}

/// A recording `impl Ops for Rec`: every instruction handler prints its name and operand values
/// via the generated accessors, every dispatch-group handler prints the group entry and then
/// forwards to the member handler (exercising the grouped `fn alu(op: AluKind, inst)` path).
fn recording_ops_impl(isa: &Isa) -> String {
    let mut s = String::from("\nstruct Rec;\n\nimpl Ops for Rec {\n");

    for g in &isa.groups {
        let kind = format!("{}Kind", pascal(&g.name));
        let _ = writeln!(
            s,
            "    fn {}(&mut self, op: {kind}, inst: Instruction) {{",
            g.name
        );
        let _ = writeln!(s, "        println!(\"group {} {{:?}}\", op);", g.name);
        s.push_str("        match op {\n");
        for m in &g.members {
            let _ = writeln!(s, "            {kind}::{} => self.{}(inst),", pascal(m), m);
        }
        s.push_str("        }\n    }\n\n");
    }

    for inst in &isa.instrs {
        let mut fmt = inst.name.clone();
        let mut args = String::new();
        for f in &inst.fields {
            let _ = write!(fmt, " {}={{}}", f.name);
            let _ = write!(args, ", inst.{}()", f.name);
        }
        let _ = writeln!(s, "    fn {}(&mut self, inst: Instruction) {{", inst.name);
        let _ = writeln!(s, "        println!(\"{fmt}\"{args});");
        s.push_str("    }\n\n");
    }

    s.push_str("    fn on_invalid(&mut self, _inst: Instruction) {\n");
    s.push_str("        println!(\"invalid\");\n");
    s.push_str("    }\n}\n");
    s
}

fn word_list(words: &[u64]) -> String {
    words
        .iter()
        .map(|w| format!("{w:#x}"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// What the oracle says `run_ops` should print for one word: the group entry (if the leaf is a
/// dispatch-group member), the leaf handler line, then the length returned by `run_ops`.
fn expected_ops_lines(isa: &Isa, words: &[u64]) -> Vec<String> {
    let mut expected = Vec::new();
    for &w in words {
        let d = decode(isa, w);
        if d.is_valid() {
            if let Some((gname, variant)) = group_of(isa, &d.opcode_name) {
                expected.push(format!("group {gname} {variant}"));
            }
            expected.push(field_line(isa, &d));
        } else {
            expected.push("invalid".to_string());
        }
        expected.push(format!("len={}", d.len_bytes));
    }
    expected
}

fn run_ops_case(name: &str, src: &str) {
    let isa = compile(src).unwrap_or_else(|_| panic!("`{name}` compile"));
    let words = sample_words(&isa);
    let expected = expected_ops_lines(&isa, &words);

    let mut prog = chipi_backend_rust::emit_decoder(&isa);
    prog.push_str(&recording_ops_impl(&isa));
    prog.push_str(&format!(
        r#"
fn main() {{
    let mut rec = Rec;

    for w in [{}] {{
        let len = run_ops(&mut rec, w);
        println!("len={{}}", len);
    }}
}}
"#,
        word_list(&words)
    ));

    let got = compile_and_run(name, &prog);
    assert_eq!(got, expected, "`{name}` Ops dispatch mismatch vs oracle");
}

/// Ops trait dispatch on a plain spec: every word routes to the handler and operand values the
/// oracle decodes, invalid words route to `on_invalid`.
#[test]
fn ops_dispatch_matches_oracle() {
    run_ops_case("ops_mips", include_str!("../../../examples/mips.chipi"));
}

/// Same, on a spec with a dispatch group: `add`/`sub` arrive through the grouped
/// `fn alu(op: AluKind, inst)` entry point with the right `AluKind`, `ori` stays individual.
#[test]
fn ops_dispatch_groups_match_oracle() {
    let isa = compile(include_str!("../../../examples/tags_demo.chipi")).unwrap();
    assert!(
        !isa.groups.is_empty(),
        "tags_demo should carry a dispatch group"
    );
    run_ops_case(
        "ops_tags",
        include_str!("../../../examples/tags_demo.chipi"),
    );
}

/// The Handler fn-pointer table: `default_table` is inert (every slot is the no-op
/// `decode_invalid`), and once slots are overridden with recording handlers, `run` and `dispatch`
/// both route every word to the handler and operands the oracle decodes, with invalid words
/// landing in the overridden slot 0.
#[test]
fn handler_table_routes_like_oracle() {
    let isa = compile(include_str!("../../../examples/mips.chipi")).unwrap();
    let words = sample_words(&isa);

    // One routed line per word, and the whole word set replayed twice: once through `run`,
    // once through `dispatch`.
    let mut routed = Vec::new();
    for &w in &words {
        let d = decode(&isa, w);
        if d.is_valid() {
            routed.push(field_line(&isa, &d));
        } else {
            routed.push("invalid".to_string());
        }
    }
    let mut expected = vec!["default-table hits: 0".to_string()];
    expected.extend(routed.iter().cloned());
    expected.extend(routed);

    let mut prog = chipi_backend_rust::emit_decoder(&isa);

    // A recording handler per instruction, mirroring the oracle's field lines.
    for inst in &isa.instrs {
        let mut fmt = inst.name.clone();
        let mut args = String::new();
        for f in &inst.fields {
            let _ = write!(fmt, " {}={{}}", f.name);
            let _ = write!(args, ", inst.{}()", f.name);
        }
        let _ = write!(
            prog,
            r#"
fn h_{}(hits: &mut Vec<String>, inst: Instruction) {{
    hits.push(format!("{fmt}"{args}));
}}
"#,
            inst.name
        );
    }
    prog.push_str(
        r#"
fn h_invalid(hits: &mut Vec<String>, _inst: Instruction) {
    hits.push("invalid".to_string());
}
"#,
    );

    let mut overrides = String::new();
    for inst in &isa.instrs {
        let _ = writeln!(
            overrides,
            "    table[opcode::{}] = h_{};",
            inst.name.to_uppercase(),
            inst.name
        );
    }

    prog.push_str(&format!(
        r#"
fn main() {{
    let words = [{}];

    // The default table must be inert: every slot is the no-op `decode_invalid`.
    let inert = default_table::<Vec<String>>();
    let mut hits: Vec<String> = Vec::new();
    for &w in &words {{
        let len = run(&inert, &mut hits, w);
        assert_eq!(len, LEN);
    }}
    println!("default-table hits: {{}}", hits.len());

    // Overridden table: every opcode slot records, including the invalid slot.
    let mut table = default_table::<Vec<String>>();
    table[opcode::INVALID] = h_invalid;
{overrides}
    for &w in &words {{
        run(&table, &mut hits, w);
    }}
    for &w in &words {{
        dispatch(&table, &mut hits, Instruction(w));
    }}

    for line in hits {{
        println!("{{}}", line);
    }}
}}
"#,
        word_list(&words)
    ));

    let got = compile_and_run("table_mips", &prog);
    assert_eq!(got, expected, "handler table routing mismatch vs oracle");
}

/// `scan_prefixes` and `decode_stream` against the oracle's byte-stream decode, covering all
/// three prefix terminators: 0x66 keeps scanning (Continue), a REX byte consumes and stops
/// (Finish), a non-prefix byte stops without consuming (Done). Also checks a prefix at the end
/// of the stream and the empty stream.
#[test]
fn prefix_scan_matches_stream_oracle() {
    let isa = compile(include_str!("../../../examples/x86_prefix.chipi")).unwrap();
    assert!(isa.prefix.is_some(), "x86_prefix should have a prefix scan");

    let streams: &[&[u8]] = &[
        &[0x90],             // Done immediately: plain nop
        &[0x50],             // Done immediately: push rax
        &[0xC3],             // Done immediately: ret
        &[0x66, 0x50],       // Continue then Done: push16
        &[0x66, 0x66, 0x50], // repeated Continue
        &[0x49, 0x90],       // Finish: REX.B flips nop to xchg_r8
        &[0x48, 0x90],       // Finish: REX.W only, still nop
        &[0x66, 0x49, 0x90], // Continue then Finish
        &[0x41],             // Finish with nothing after: decodes the zero word (invalid)
        &[],                 // empty stream: defaults, invalid
    ];

    // "consumed|f1=v1 f2=v2 ...|total_len|name", with context fields in declaration order.
    let ctx_names: Vec<&str> = isa
        .decoder
        .context
        .iter()
        .map(|c| c.name.as_str())
        .collect();
    let mut expected = Vec::new();
    for bytes in streams {
        let d = decode_stream(&isa, bytes);
        let ctx = ctx_names
            .iter()
            .map(|n| {
                let v = d.context.iter().find(|(cn, _)| cn == n).map(|(_, v)| *v);
                format!("{n}={}", v.expect("context field present"))
            })
            .collect::<Vec<_>>()
            .join(" ");
        expected.push(format!(
            "{}|{ctx}|{}|{}",
            d.prefix_len, d.len_bytes, d.opcode_name
        ));
    }

    let ctx_fmt = ctx_names
        .iter()
        .map(|n| format!("{n}={{}}"))
        .collect::<Vec<_>>()
        .join(" ");
    let ctx_args = ctx_names
        .iter()
        .map(|n| format!("ctx.{n}"))
        .collect::<Vec<_>>()
        .join(", ");
    let stream_lits = streams
        .iter()
        .map(|bytes| {
            let inner = bytes
                .iter()
                .map(|b| format!("{b:#04x}"))
                .collect::<Vec<_>>()
                .join(", ");
            format!("&[{inner}]")
        })
        .collect::<Vec<_>>()
        .join(",\n        ");

    let mut prog = chipi_backend_rust::emit_decoder(&isa);
    prog.push_str(&format!(
        r#"
fn main() {{
    let streams: [&[u8]; {}] = [
        {stream_lits},
    ];

    for bytes in streams {{
        let (consumed, ctx) = scan_prefixes(bytes);
        let (inst, len, stream_ctx) = decode_stream(bytes);
        assert_eq!(ctx, stream_ctx, "decode_stream must reuse the scan context");
        println!("{{}}|{ctx_fmt}|{{}}|{{}}", consumed, {ctx_args}, len, inst.opcode_name_with(&ctx));
    }}
}}
"#,
        streams.len()
    ));

    let got = compile_and_run("stream_x86", &prog);
    assert_eq!(
        got, expected,
        "prefix scan mismatch vs decode_stream oracle"
    );
}

/// The enum-style backend's `decode`: variant identity and payload field values must match the
/// oracle for every sample word, and invalid words must land in `Instruction::Invalid`.
#[test]
fn enum_decode_payloads_match_oracle() {
    let isa = compile(include_str!("../../../examples/mips.chipi")).unwrap();
    let words = sample_words(&isa);

    let mut expected = Vec::new();
    let mut image: Vec<u8> = Vec::new();
    for &w in &words {
        image.extend_from_slice(&(w as u32).to_le_bytes());
        let d = decode(&isa, w);
        let line = if d.is_valid() {
            field_line(&isa, &d)
        } else {
            "invalid".to_string()
        };
        expected.push(format!("{}|{line}|{}", d.opcode_name, d.len_bytes));
    }

    // A match arm per instruction, printing the variant's payload fields by name.
    let mut arms = String::new();
    for inst in &isa.instrs {
        let names: Vec<&str> = inst.fields.iter().map(|f| f.name.as_str()).collect();
        let mut fmt = inst.name.clone();
        for n in &names {
            let _ = write!(fmt, " {n}={{}}");
        }
        let _ = writeln!(
            arms,
            "            Instruction::{} {{ {} }} => format!(\"{fmt}\", {}),",
            pascal(&inst.name),
            names.join(", "),
            names.join(", ")
        );
    }
    arms.push_str("            Instruction::Invalid { .. } => \"invalid\".to_string(),");

    let image_lit = image
        .iter()
        .map(|b| format!("{b:#04x}"))
        .collect::<Vec<_>>()
        .join(", ");

    let mut prog = chipi_backend_rust::emit_decoder_with(
        &isa,
        chipi_backend_rust::EmitOptions {
            dispatch: chipi_backend_rust::Dispatch::Enum,
        },
    );
    prog.push_str(&format!(
        r#"
static IMAGE: [u8; {}] = [{image_lit}];

struct Mem;

impl DisasmCtx for Mem {{
    fn read_u8(&self, addr: u64) -> u8 {{
        IMAGE.get(addr as usize).copied().unwrap_or(0)
    }}
}}

fn main() {{
    for i in 0..{} {{
        let (inst, len) = decode((i * 4) as u64, &Mem);
        let line = match inst {{
{arms}
        }};
        println!("{{}}|{{}}|{{}}", inst.name(), line, len);
    }}
}}
"#,
        image.len(),
        words.len()
    ));

    let got = compile_and_run("enum_mips_behavior", &prog);
    assert_eq!(got, expected, "enum decode payload mismatch vs oracle");
}
