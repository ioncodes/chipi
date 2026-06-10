# chipi

A declarative instruction decoder generator. You define a CPUs encoding in
a portable `.chipi` DSL file. You then describe per-project codegen choices
in a `*.bindings.chipi` file. chipi produces decoders, disassemblers and
emulator dispatch code.

`.chipi` files are language-agnostic. They describe bit patterns, field
extractions and display formats. They contain no language-specific
information.

`*.bindings.chipi` files are project-specific. They pick which decoders to
lower, pick which language/processor to target (Rust, C++, IDA, Binary Ninja)
and much more.

## Backends

| Backend | Output                                                             |
| ------- | ------------------------------------------------------------------ |
| `rust`  | Decoder enum + `decode()` + `Display`. Optional emulator dispatch. |
| `cpp`   | Single-header decoder with `std::format`.                          |
| `ida`   | IDA Pro 9.x processor module (Python).                             |
| `binja` | Binary Ninja Architecture plugin (Python).                         |

The IDA and Binary Ninja outputs are experimental. They do not replace
hand-written processor modules.

## Architecture

```
*.chipi --------------+
                      |
*.bindings.chipi -----+--> bindings parser/lower --> codegen
                      |
                      +--> chipi-cli / chipi-build
```

Three crates:

| Crate             | Purpose                                                      |
| ----------------- | ------------------------------------------------------------ |
| **`chipi-core`**  | Parser, IR, validation, bindings frontend, codegen backends. |
| **`chipi-cli`**   | CLI: `generate`, `check`, `explain`, `preview`.              |
| **`chipi-build`** | `build.rs` helper for Rust projects.                         |

## Quick start

A `.chipi` instruction spec describes the encoding:

```text
decoder Gekko {
    width = 32
    bit_order = msb0
}

addi   [0:5]=001110 rd:u5[6:10] ra:u5[11:15] simm:s16[16:31]
       | "addi r{rd}, r{ra}, {simm}"

ori    [0:5]=011000 rs:u5[6:10] ra:u5[11:15] uimm:u16[16:31]
       | "ori r{ra}, r{rs}, 0x{uimm:04x}"
```

A `*.bindings.chipi` file picks which decoder/dispatch to generate:

```text
include "gekko.chipi"

target rust {
    decoder Gekko {
        output "$OUT_DIR/gekko.rs"

        type gpr = crate::cpu::Gpr
        type simm16 = i32
    }

    dispatch Gekko {
        output "$OUT_DIR/gekko_dispatch.rs"

        context crate::Cpu
        handlers crate::cpu::interpreter
        strategy fn_ptr_lut

        invalid_handler crate::cpu::interpreter::invalid

        instruction_type crate::cpu::Instruction {
            output "$OUT_DIR/gekko_instr.rs"
        }

        handler alu<const OP> {
            addi
            ori
        }
    }
}
```

Generate everything:

```bash
chipi generate specs/gekko.bindings.chipi
```

## CLI

```bash
# Run all targets in the file.
chipi generate <bindings>
chipi generate <bindings> --target rust
chipi generate <bindings> --target rust --decoder Gekko

# Validate without writing.
chipi check <bindings>

# Decode an opcode and print the match.
chipi explain <bindings> --decoder Gekko 0x38600001

# Print the lowered configuration.
chipi preview <bindings>
chipi preview <bindings> --target rust
```

A bindings file may contain multiple targets. If `--target` is omitted in
that case, chipi reports an error. The same applies to `--decoder` when
more than one decoder or dispatch is reachable.

## `build.rs`

Drive codegen from `build.rs` via `chipi-build`:

```rust
// build.rs
fn main() {
    chipi_build::generate_bindings("specs/gekko.bindings.chipi")
        .expect("chipi codegen failed");
}
```

To select a single target or decoder:

```rust
chipi_build::generate_bindings_target("specs/gekko.bindings.chipi", "rust")?;
chipi_build::generate_bindings_decoder("specs/dsp.bindings.chipi", "rust", "GcDsp")?;
```

`chipi-build` automatically emits `cargo:rerun-if-changed` for the bindings
file. It also emits one for every transitively included bindings file and one
for every included `.chipi` spec.

## Bindings reference

### Targets

A bindings file contains one or more `target <kind> { ... }` blocks:

- `target rust`: Supports `decoder` and `dispatch` blocks.
- `target cpp`: Supports `decoder` blocks.
- `target ida`: Supports `processor` blocks.
- `target binja`: Supports `architecture` blocks.

`include "*.chipi"` brings in an instruction spec. `include "*.bindings.chipi"`
recursively merges another bindings file's targets.
The latter is useful for combining multiple CPUs in one project.

### Rust decoders

```text
target rust {
    decoder Gekko {
        output "$OUT_DIR/gekko.rs"

        type gpr = crate::cpu::Gpr
        type fpr = crate::cpu::Fpr
        type simm16 = i32

        subdecoder GekkoExt {
            output "$OUT_DIR/gekko_ext.rs"
        }
    }
}
```

Type aliases declared in the `.chipi` file map to the Rust paths listed in
`type ... = ...`. Sub-decoder blocks share the same set of options.

### Rust dispatches

```text
target rust {
    dispatch Gekko {
        output "$OUT_DIR/gekko_dispatch.rs"

        context crate::Cpu
        handlers crate::cpu::interpreter
        strategy fn_ptr_lut

        invalid_handler crate::cpu::interpreter::invalid

        instruction_type crate::cpu::Instruction {
            output "$OUT_DIR/gekko_instr.rs"
        }

        handler alu<const OP> {
            addi
            ori
        }
    }
}
```

If no `handler` blocks are present, every instruction maps to a same-named
function under the `handlers` module path:

- `addi` -> `crate::cpu::interpreter::addi`
- `ori`  -> `crate::cpu::interpreter::ori`

Each `handler <name><const OP> { ... }` block groups the listed
instructions under one const-generic handler:

- `addi` -> `crate::cpu::interpreter::alu::<{ OP_ADDI }>`
- `ori`  -> `crate::cpu::interpreter::alu::<{ OP_ORI }>`

The `OP_*` constants are emitted into the generated dispatch file.

### Dispatch strategies

- `fn_ptr_lut`. Static `[Handler; N]` arrays per decision-tree branch.
- `jump_table`. One `#[inline(always)]` function with nested matches.
- `flat_lut`. Full-width function-pointer table indexed by raw decoder value.
- `flat_match`. Full-width match with adjacent equal handlers compressed
  into ranges.

`flat_lut` and `flat_match` enumerate the entire `2^width` key space. They
are best suited to small decoders or sub-decoders. chipi does not cap the
width for you. It will happily generate gigantic outputs if asked, so
make sure you don't run this against something huge.

### Subdecoder / subdispatch

```text
target rust {
    decoder GcDsp {
        output "$OUT_DIR/dsp.rs"

        type reg5 = crate::dsp::Register

        subdecoder GcDspExt {
            output "$OUT_DIR/dsp_ext.rs"
        }
    }

    dispatch GcDsp {
        output "$OUT_DIR/dsp_dispatch.rs"

        context crate::dsp::Dsp
        handlers crate::dsp::interpreter
        strategy fn_ptr_lut
        invalid_handler crate::dsp::interpreter::invalid

        subdispatch GcDspExt {
            handlers crate::dsp::interpreter::ext
            strategy flat_lut
            invalid_handler crate::dsp::interpreter::invalid_ext
        }
    }
}
```

A `subdispatch` inherits `context`, `strategy`, and `invalid_handler` from
its parent unless overridden. `handlers` and `instruction_type` may also
be overridden.

### IDA processor

```text
target ida {
    processor GcDsp {
        output "plugins/ida/dsp_proc.py"

        name "gcdsp"
        long_name "GameCube DSP"
        id 0x8002

        address_size 16
        bytes_per_unit 2

        registers {
            ar0
            ar1
            ar2
            ar3
            CS
            DS
        }

        segment_registers {
            CS
            DS
        }

        flow {
            calls {
                callcc
            }
            returns {
                retcc
            }
            stops {
                halt
            }
        }
    }
}
```

`segment_registers` must be a subset of `registers`. Instruction names in
`flow` must exist in the decoder.

### Binary Ninja architecture

```text
target binja {
    architecture GcDsp {
        output "plugins/binja/dsp_arch.py"

        name "gcdsp"

        address_size 2
        default_int_size 2
        endianness big

        registers {
            ar0
            ar1
            ar2
            ar3
        }
    }
}
```

`endianness` must be `big` or `little`.

## Diagnostics

chipi reports validation errors with a span and an optional `did you mean`
suggestion. For example:

```
error: unknown instruction 'halttt' in handler group
 --> test_grouped.bindings.chipi:14
 = help: did you mean "halt"?
```

`flat_lut` / `flat_match` ambiguity reports each conflicting instruction.
This happens when one raw value matches two distinct handlers:

```
error: flat dispatch cannot resolve raw opcode 0x0000007c
   matched instructions:
     add  -> crate::cpu::interpreter::add
     addx -> crate::cpu::interpreter::addx
   flat dispatch requires each raw value to resolve to exactly one handler.
```

## Examples

| Project                                                        | Description                                    |
| -------------------------------------------------------------- | ---------------------------------------------- |
| [chipi-gekko](https://github.com/ioncodes/chipi-gekko)         | GameCube CPU & DSP disassembler (Rust)         |
| [chipi-gekko-cpp](https://github.com/ioncodes/chipi-gekko-cpp) | GameCube CPU disassembler (C++)                |
| [gc-dsp-ida](https://github.com/ioncodes/gc-dsp-ida)           | GameCube DSP processor plugin for IDA Pro 9.x  |
| [gc-dsp-binja](https://github.com/ioncodes/gc-dsp-binja)       | GameCube DSP processor plugin for Binary Ninja |
| [chipi-spec](https://github.com/ioncodes/chipi-spec)           | Reusable `.chipi` specs                        |
| [chipi-vscode](https://github.com/ioncodes/chipi-vscode)       | VS Code syntax highlighting for `.chipi` files |
