# chipi

[![CI](https://github.com/ioncodes/chipi/actions/workflows/ci.yml/badge.svg)](https://github.com/ioncodes/chipi/actions/workflows/ci.yml)

chipi generates instruction decoders, disassemblers and dispatchers in Rust, C++
and Python. You write the bit layouts, operands and display text once in a
`.chipi` file. The generated code handles the matching and extraction.

It also has a reference encoder and text assembler in the core library and CLI.
These are useful for checking a spec and assembling individual instructions;
they are not emitted into generated decoders.

## Get started

Install the released tools from crates.io with Rust 1.74 or newer:

```sh
cargo install chipi-cli --locked
cargo install chipi-lsp --locked
```

The first command installs `chipi`. The second installs the language server used
by the [VS Code extension](#editor-support) and other LSP clients.

With the specs in this repository's `examples/` directory, check a spec, decode a
word and generate a decoder:

```sh
chipi check examples/mips.chipi
chipi explain examples/mips.chipi -- 0x00851020
chipi emit --target rust examples/mips.chipi -o mips_decoder.rs
```

Use `--target cpp` or `--target python` for the other backends. Generated decoders
are self-contained; they do not require chipi at runtime.

## A small spec

This describes three MIPS instructions:

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

Selectors name the bits used to identify an instruction. Operands name the bits
to extract and how to display them. Each instruction fixes its selector values,
binds its operands and gives its assembly text after `|`.

From this, chipi produces classification and decoding functions, operand accessors,
a disassembler and dispatch handlers. It also exposes opcode names and ids. Dotted
instruction names such as `lda.imm` add separate mnemonic and form metadata.

The specs in [`examples/`](examples) demonstrate guards, computed operands,
mode-dependent fetches, prefix scanning, indexed instruction families, display
templates and subdecoders.

## Use it from Rust

Add `chipi-macros` to your project and put the spec beside your source. The `isa!`
macro generates a module at compile time, without a build script:

```rust
chipi_macros::isa!("isa/mips.chipi");

let (inst, _len) = Mips::decode(0x0085_1020);
assert_eq!(inst.opcode_name(), "add");
assert_eq!(inst.rd(), 2);
```

The path is relative to your crate's `Cargo.toml`; the module name comes from the
spec's `decoder` declaration. Changes to the spec trigger recompilation. Generated
Rust disassembly is behind the consuming crate's `disasm` feature, so declare and
enable that feature if you need it.

The default representation is a small instruction value with lazy operand
accessors. For a simple ISA, `isa!("isa/cpu.chipi", style = enum)` generates an enum
with eagerly decoded operand fields instead. See the limitations below before
choosing it.

## CLI

```sh
# Assemble one instruction into a word and bytes.
chipi asm examples/mips.chipi -- 'add $r2, $r4, $r5'

# Check encoder round trips and report assembler coverage per instruction.
chipi check --roundtrip examples/mips.chipi

# Decode with a host mode, or inspect a prefixed byte stream.
chipi explain examples/fetch_expr.chipi --mode m=0 -- 0xA9
chipi explain examples/x86_prefix.chipi --stream -- 0x66,0x48,0x90

# Generate editable handler skeletons or inspect the compiler's output.
chipi stubs examples/mips.chipi -o handlers.rs
chipi dump-ir examples/mips.chipi
chipi dump-tree examples/mips.chipi
```

`--mode` takes numeric values as `name=value`, with commas between assignments.
It applies to word decoding; stream decoding starts from the spec's defaults and
applies its prefixes. Run `chipi --help` for command syntax or `chipi --version`
to check the installed version.

## Editor support

The [VS Code extension](editors/vscode) provides autocomplete, errors and warnings
as you type, hover information, go-to-definition, code references, outline symbols,
folding and document formatting. It also includes syntax highlighting and snippets.

After installing `chipi-lsp`, build and install the extension with Node.js 22 or newer:

```sh
cd editors/vscode
npm ci
npm run package
code --install-extension chipi-1.0.0.vsix
```

Open a `.chipi` file to start the server. If VS Code cannot find it on `PATH`, set
`chipi.serverPath` to the full path of the `chipi-lsp` executable. The command
**chipi: Restart Language Server** restarts it after configuration changes or an update.

Formatting adjusts spacing and indentation while keeping comments, string contents
and line breaks. To format on save:

```json
"[chipi]": {
  "editor.defaultFormatter": "ioncodes.chipi",
  "editor.formatOnSave": true
}
```

Other editors can launch `chipi-lsp --stdio` for `.chipi` files. See the
[editor guide](editors/vscode/README.md) for configuration and current LSP scope.

## Supported features and limits

| Feature                                      | Rust (default) | C++ | Python |
| -------------------------------------------- | -------------- | --- | ------ |
| Decode, operand accessors and guards         | Yes            | Yes | Yes    |
| Disassembly and grouped dispatch             | Yes            | Yes | Yes    |
| Modes, prefixes and context                  | Yes            | Yes | Yes    |
| Fetched operands and contextual disassembly  | Yes            | Yes | Yes    |
| Tags, mnemonic/form metadata and subdecoders | Yes            | Yes | Yes    |
| Function-pointer handler table               | Yes            | No  | No     |
| Generated encoder or assembler               | No             | No  | No     |

The compiler and generated code have a few boundaries to keep in mind:

- Host modes have a maximum of 256 value combinations. Combinations selecting the
  same instructions share a decode table. Word-level calls use the declared defaults;
  use the generated mode-aware or contextual entry points to supply runtime state.
- `fetch(expr)` can depend on host modes and must yield a width from 1 to 64 bits
  in every combination. Prefix-assigned context cannot set a fetch width.
- Generated backends reject `length` arms that read decode variables. Display
  conditions reading those variables require the contextual disassembler path.
- The Rust enum backend supports fixed windows and fixed-size fetched operands.
  It rejects `length`, prefixes, subdecoders and expression-width fetches.
- The reference assembler handles reversible display forms. Numeric fallbacks for
  symbol and relative operands work; symbol names need context and subdecoder
  output text is not generally reversible. Use `check --roundtrip` to see coverage
  for your spec. Function inversion may fall back to a bounded search.

## Examples and tests

Start with [`mips.chipi`](examples/mips.chipi) for a conventional fixed-width ISA,
[`rv32i.chipi`](examples/rv32i.chipi) for scattered immediates, or
[`x86_prefix.chipi`](examples/x86_prefix.chipi) for prefixes and context.
[`fetch_expr.chipi`](examples/fetch_expr.chipi),
[`axes_demo.chipi`](examples/axes_demo.chipi) and
[`for_demo.chipi`](examples/for_demo.chipi) each demonstrate one of the larger
language features.

The four production specs in [`corpus/`](corpus) cover the Ricoh 5A22, SPC700,
Gekko and GameCube DSP. Tests compare generated Rust, C++ and Python code against
the reference interpreter and check the corpus against saved decode transcripts.
Everything needed is in this checkout.

To work on chipi itself, run the tools from the repository with
`cargo run -p chipi-cli -- --help` or `cargo run -p chipi-lsp -- --stdio`.
To install your local changes, use `cargo install --path crates/chipi-cli --locked`
and `cargo install --path crates/chipi-lsp --locked`.

```sh
cargo test --workspace --locked
cargo fmt --all --check
cargo clippy --workspace --all-targets --all-features --locked -- -D warnings
```

Backend tests need `g++` with C++17 support and `python3` on `PATH`. The
[editor guide](editors/vscode/README.md) covers building and testing the VS Code
extension.

## License

MIT or Apache-2.0, your choice. See [LICENSE-MIT](LICENSE-MIT) and
[LICENSE-APACHE](LICENSE-APACHE).
