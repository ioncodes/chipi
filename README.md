# chipi

[![CI](https://github.com/ioncodes/chipi/actions/workflows/ci.yml/badge.svg)](https://github.com/ioncodes/chipi/actions/workflows/ci.yml)

chipi generates instruction decoders from a small spec. You describe a CPUs encoding once in a
`.chipi` file and chipi builds a decoder, disassembler and dispatcher for it, in Rust, C++ and
Python. A reference encoder and text assembler live in the core library and the CLI.

## Minimal Example

This is a minimal MIPS example:

```text
decoder Mips {
    width = 32
    bit_order = lsb0
    endian = little
}

selector op    [31:26]
selector funct [5:0]

operand greg = u5  { display("$r{}") }
type simm16  = i32 { sign_extend(16), display(signed_hex) }

add   op=0        funct=0b100000 rd:greg[15:11] rs:greg[25:21] rt:greg[20:16] | "add {rd}, {rs}, {rt}"
addiu op=0b001001 rt:greg[20:16] rs:greg[25:21] imm:simm16[15:0]              | "addiu {rt}, {rs}, {imm}"
lw    op=0b100011 rt:greg[20:16] rs:greg[25:21] off:simm16[15:0]              | "lw {rt}, {off}({rs})"
```

From this spec, chipi builds:

- a decoder that turns a word into the matched instruction and its operands
- a disassembler that renders the display text, with conditionals and symbol lookups
- a dispatcher with one handler per instruction, called through a small trait
- identity metadata: opcode ids, names, tags, and (for dotted leaf names) mnemonic/form enums

The encoder and text assembler are part of `chipi-core` and the CLI (`chipi asm`,
`chipi check --roundtrip`); no backend emits encoding into generated code.

## Advanced Features

There's a lot of advanced features, here's a short list of few:

**Decode variables.** Host `mode`s and prefix-assigned `context` fields are one concept: decode
variables. Guards and `length` arms read them, `fetch` widths compute from them, and prefixes can
actually change what decodes. Word-level entry points (`classify(word)`, `disasm(word)`) fold every
variable to its declared default, matching `decode(word)` in the reference evaluator:

```text
decoder MX {
    width = 8
    mode m: bool = 1
}

lda8  m=1 op=0xA9 | "lda #imm8"       # per-combination tables, host picks via pack_modes(m)
lda16 m=0 op=0xA9 | "lda #imm16"
```

```text
context { osize:u1 = 0 }
prefix scan { 0x66 => osize = 1  _ => done }

push16 op=0x50 when osize == 1 | "push ax"    # the 0x66 prefix flips what 0x50 decodes to
push   op=0x50                 | "push rax"
```

Each mode combination gets its own decode table (capped at 256 combinations), so modes are for
small host state, not wide values.

**Expression fetch widths.** A stream operand's width can depend on host modes, collapsing the
65816-style m8/m16 leaf duplication into one leaf:

```text
lda_imm op=0xA9 imm:u16 = fetch(m ? 8 : 16) | "lda #{m?${imm:02x}:${imm:04x}}"
```

**Identity axes.** A dotted leaf name (`lda.dpx`) names the leaf on two axes. Codegen derives
`Mnemonic` and `Form` enums, `mnemonic()`/`form()` accessors and name tables; leaves of one form
must bind the same operand shape, so a consumer writes one match over the form axis. Dispatch
groups accept `lda.*` patterns:

```text
lda.imm op=0xA9 v:u8[0:7]  | "lda #${v:02x}"
lda.dp  op=0xA5 dp:u8[0:7] | "lda ${dp:02x}"

dispatch load { lda.*, ldx.* }
```

**Indexed families.** `for` blocks expand SPC700-style opcode arithmetic, with the index usable in
the name, constraints and template:

```text
for n in 0..8 {
    bbs_b{n} op = 0x03 + n * 0x20 dp:u8[0:7] | "bbs ${dp:02x}.{n}"
}
```

**Guard-decided leaves.** Two leaves may share every fixed bit when guards can decide between them
(at most one unguarded fallback); a failed guard falls through to the next candidate:

```text
eq  op=0 a:reg[3:0] b:reg[7:4] when a == b | "eq {a}"
mov op=0 a:reg[3:0] b:reg[7:4]             | "mov {a}, {b}"
```

**Computed operands.** Scatter or gather bits with `assemble`, declared once and reused across a
whole format; the encoder inverts the same map:

```text
operand bimm = i13 assemble 13 {
    [12]   = word[31]
    [10:5] = word[30:25]
    [4:1]  = word[11:8]
    [11]   = word[7]
    [0]    = 0
} sign_extend

beq op=0b1100011 rs1:greg[19:15] rs2:greg[24:20] off:bimm | "beq {rs1}, {rs2}, {off}"
```

**Functions and guards.** Small pure `fn`s with a few builtins (`concat`, `replicate`,
`rotate_right`, `ones` and friends) and `when` guards cover what plain field extraction cannot:

```text
fn arm_bitmask(n:u1, immr:u6, imms:u6) -> u64 {
    let len   = bit_width(concat(n, ~imms)) - 1
    let welem = ones((imms & ((1 << len) - 1)) + 1)
    return replicate(rotate_right(welem, immr, 1 << len), 1 << len, 64)
}

and_imm op=0b00100100 n:u1[22] immr:u6[16:21] imms:u6[10:15] rn:gpr[5:9] rd:gpr[0:4]
        when valid_bitmask(n, imms)
        imm:u64 = arm_bitmask(n, immr, imms)
        | "and {rd}, {rn}, #{imm:#x}"
```

**Dispatch groups.** Fold several opcodes into one grouped handler while keeping the per-instruction
handlers:

```
dispatch alu { add, sub }     # add/sub fold into one fn alu(op, inst); ori stays on its own
```

**Display templates.** Conditionals (`{flag?suffix}`, `{cond?a:b}`) and symbol or PC-relative
lookups (`{x:sym}`, `{x:rel}`) shape the rendered text:

```text
add     op=0 rc:u1[0] oe:u1[10] rd:greg[20:23] ra:greg[16:19] rb:greg[12:15]
        | "add{oe?o}{rc?.} {rd}, {ra}, {rb}"
jmp_abs op=0x4C target:fetch16 | "jmp {target:sym}"
```

**Named values.** An operand can render through a `names { ... }` table instead of a numeric
pattern, falling back to a literal or a `dec`/`hex` hint:

```text
operand cc = u4 { display(names { 0 => "eq", 1 => "ne", 2 => "lt", _ => "gt" }) }
```

**Subdecoders.** A `subdecoder` decodes a bound sub-field into named string `outputs`, spliced into
a template as `{field.output}`. The field still decodes and encodes as plain bits; only its
rendering runs the subdecoder (used, for example, by the GameCube DSP's packed parallel-move byte).
All three backends emit it as a per-output render function:

```text
subdecoder Ext { width = 8 bit_order = msb0 outputs { mnemonic, operands }
    e_dr [0:5]=0b000001 r:n2[6:7] | mnemonic = "'DR" | operands = " : $ar{r}" }
add [0:7]=0b01000000 ext:Ext[8:15] | "add{ext.mnemonic} $ac0{ext.operands}"
```

## CLI Usage

```bash
# Install it
cargo install chipi-cli

# Decode one word and show how it matched
chipi explain examples/mips.chipi -- 0x00851020

# Assemble one line back into bytes
chipi asm examples/mips.chipi -- 'add $r2, $r4, $r5'

# Generate a decoder (rust, cpp or python)
chipi emit --target rust examples/mips.chipi -o mips_decoder.rs

# Check a spec, with per-leaf encoder/assembler status
chipi check --roundtrip examples/mips.chipi
```

There are a few more: `stubs` writes handler skeletons, `dump-ir` and `dump-tree` print the
resolved spec and the decode tree. Run `chipi --help` for the full list.

## Rust

The `isa!` macro expands a spec into a module at compile time, with no build script:

```rust
chipi_macros::isa!("examples/mips.chipi");

let (inst, len) = Mips::decode(0x0085_1020);
assert_eq!(inst.opcode_name(), "add");
assert_eq!(inst.rd(), 2);
```

## Examples

The `examples/` folder has one spec per feature. Every working spec runs through the whole pipeline
in the test suite.

| File                    | Shows                                          |
| ----------------------- | ---------------------------------------------- |
| `mips.chipi`            | dense table, `funct` residual, encoder         |
| `rv32i.chipi`           | RV32I and every immediate shape                |
| `riscv.chipi`           | `assemble` scatter and gather                  |
| `riscv_rvc.chipi`       | a `length` window: 16bit or 32bit              |
| `x86_prefix.chipi`      | prefix scan; context read back by guards       |
| `aarch64.chipi`         | `fn`, builtins, a `when` guard                 |
| `gekko.chipi`           | 32bit `msb0`, a `form`, residual `xo`          |
| `gba_arm.chipi`         | ARM7TDMI data processing and branch            |
| `gb.chipi`              | 8bit opcodes, specific leaf beats general      |
| `gc_dsp.chipi`          | 16bit fixed words                              |
| `modes_demo.chipi`      | host modes and the decode tree cross product   |
| `mode_guard.chipi`      | guards reading a host mode                     |
| `guard_chain.chipi`     | guard-decided leaves sharing one slot          |
| `fetch_expr.chipi`      | mode-dependent `fetch` widths                  |
| `axes_demo.chipi`       | identity axes and `lda.*` dispatch patterns    |
| `for_demo.chipi`        | `for` expansion of indexed families            |
| `cond_demo.chipi`       | conditionals in the display template           |
| `names_demo.chipi`      | `names { ... }` value-to-string display tables |
| `subdecoder_demo.chipi` | a `subdecoder` spliced in via `{field.output}` |
| `tags_demo.chipi`       | instruction tags and folded dispatch groups    |
| `sparse_demo.chipi`     | the sparse residual matcher                    |
| `snes_disasm.chipi`     | `fetch(N)` operands and `{x:sym}`              |
| `fn_let_width.chipi`    | `let` width inference inside `fn` bodies       |

## Build and test

```bash
cargo build
cargo test
cargo fmt --all --check
cargo clippy --all-targets --all-features -- -D warnings
```

The C++ and Python tests need `g++` (C++17) and `python3` in your `PATH`.

## Editor support

There is a VS Code extension in [`editors/vscode`](editors/vscode) with syntax highlighting,
snippets and a `.chipi` file icon.

## License

MIT or Apache-2.0, your choice. See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE).
