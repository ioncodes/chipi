# chipi

A declarative instruction decoder generator. Define your CPU's instruction encoding in a portable `.chipi` DSL file, and chipi generates decoders, disassemblers, and emulator dispatch code.

`.chipi` files are language-agnostic: they describe bit patterns, field extractions, and display formats with zero language-specific information. All language-specific configuration (type mappings, output paths, dispatch strategies) lives in a project-level `chipi.toml` or is passed as CLI arguments.

## Backends

| Backend | Lang   | Output                                   | Usage             |
| ------- | ------ | ---------------------------------------- | ----------------- |
| `rust`  | Rust   | Decoder enum + `decode()` + `Display`    | `build.rs` or CLI |
| `cpp`   | C++    | Single-header decoder with `std::format` | CLI or CMake      |
| `ida`   | Python | IDA Pro 9.x processor module             | CLI               |
| `binja` | Python | Binary Ninja Architecture plugin         | CLI               |

## Examples

| Project                                                        | Description                                    |
| -------------------------------------------------------------- | ---------------------------------------------- |
| [chipi-gekko](https://github.com/ioncodes/chipi-gekko)         | GameCube CPU & DSP disassembler (Rust)         |
| [chipi-gekko-cpp](https://github.com/ioncodes/chipi-gekko-cpp) | GameCube CPU disassembler (C++)                |
| [gc-dsp-ida](https://github.com/ioncodes/gc-dsp-ida)           | GameCube DSP processor plugin for IDA Pro 9.x  |
| [gc-dsp-binja](https://github.com/ioncodes/gc-dsp-binja)       | GameCube DSP processor plugin for Binary Ninja |
| [chipi-spec](https://github.com/ioncodes/chipi-spec)           | Reusable `.chipi` specs                        |
| [chipi-vscode](https://github.com/ioncodes/chipi-vscode)       | VS Code syntax highlighting for `.chipi` files |

Note: The IDA and Binary Ninja generators backends are highly experimental and do *not* replace fully fledged processor modules.

## Architecture

```
.chipi files --> Parser --> Decoder IR --> Backend --> generated source
                                             ^
               chipi.toml + CLI args --------+
```

Three crates:

| Crate             | Purpose                                                  |
| ----------------- | -------------------------------------------------------- |
| **`chipi-core`**  | Parser, IR, validation, code generation backends         |
| **`chipi-cli`**   | Standalone CLI (`chipi gen`, `chipi lut`, `chipi stubs`) |
| **`chipi-build`** | `build.rs` helper for Rust projects                      |

## CLI

```bash
# Install
cargo install chipi-cli

# Generate decoder/disassembler (Rust, C++, IDA, or Binary Ninja)
chipi gen --config chipi.toml
chipi gen --lang cpp --config chipi.toml
chipi gen --lang ida --config chipi.toml

# Override dispatch for a sub-decoder
chipi gen --config chipi.toml --dispatch-for GcDspExt=jump_table

# Dry run (print to stdout)
chipi gen --dry-run src/dsp.chipi

# Dump parsed IR for debugging
chipi gen --dump-ir src/dsp.chipi

# Generate emulator dispatch LUT from config
chipi lut --config chipi.toml

# Generate handler stubs
chipi stubs --output src/handlers.rs src/cpu.chipi
```

### `chipi.toml`

A single config file can define multiple targets across backends.
Paths support `$VAR` environment variable expansion (e.g. `$OUT_DIR/lut.rs`).
Relative paths are resolved from the TOML file's directory.

```toml
# Rust decoder
[[gen]]
input = "specs/dsp.chipi"
lang = "rust"
output = "$OUT_DIR/dsp.rs"

[gen.dispatch_overrides]
GcDspExt = "jump_table"

[gen.type_map]
reg5 = "crate::dsp::DspReg"

# C++ decoder (single-header)
[[gen]]
input = "specs/gekko.chipi"
lang = "cpp"
output = "generated/gekko.hpp"

[gen.lang_options]
includes = ["types.hpp"]

[gen.type_map]
gpr = "Gpr"
fpr = "Fpr"

# IDA Pro processor module
[[gen]]
input = "specs/dsp.chipi"
lang = "ida"
output = "dsp_proc.py"

[gen.lang_options]
processor_name = "gcdsp"
processor_long_name = "GameCube DSP"
processor_id = 0x8002
address_size = 16
bytes_per_unit = 2
register_names = ["ar0", "ar1", "ar2", "ar3", "CS", "DS"]
segment_registers = ["CS", "DS"]

[gen.lang_options.flow]
calls = ["callcc"]
returns = ["retcc"]
stops = ["halt"]

# Binary Ninja architecture plugin
[[gen]]
input = "specs/dsp.chipi"
lang = "binja"
output = "dsp_arch.py"

[gen.lang_options]
architecture_name = "gcdsp"
address_size = 2
default_int_size = 2
endianness = "BigEndian"
register_names = ["ar0", "ar1", "ar2", "ar3"]

# Emulator dispatch LUT (Rust only)
[[lut]]
input = "specs/gekko.chipi"
output = "$OUT_DIR/gekko_lut.rs"
handler_mod = "crate::cpu::interpreter"
ctx_type = "crate::Cpu"
instr_type = "crate::cpu::Instruction"
lut_mod = "crate::cpu::lut"
instr_type_output = "$OUT_DIR/gekko_instr.rs"

[lut.groups]
branch = ["bx", "bcx", "bclrx", "bcctrx"]
alu = ["addi", "addis", "ori", "oris"]
```

## Rust `build.rs` usage

For Rust projects, use `chipi-build` in `build.rs`:

```toml
[build-dependencies]
chipi-build = { path = "path/to/chipi/chipi-build" }
```

### Config-driven

```rust
// build.rs: processes all [[gen]] and [[lut]] targets from chipi.toml
fn main() {
    chipi_build::run_config("chipi.toml").expect("chipi codegen failed");
}
```

### Programmatic

```rust
// build.rs: decoder/disassembler generation
fn main() {
    chipi_build::generate("src/dsp/gcdsp.chipi")
        .type_map("reg5", "crate::dsp::DspReg")
        .dispatch_default(chipi_build::Dispatch::FnPtrLut)
        .dispatch_for("GcDspExt", chipi_build::Dispatch::JumpTable)
        .output("src/dsp/generated/gcdsp.rs")
        .run()
        .expect("chipi codegen failed");
}
```

```rust
// build.rs: emulator dispatch LUT generation
fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    chipi_build::lut("cpu.chipi")
        .handler_mod("crate::cpu::interpreter")
        .ctx_type("crate::Cpu")
        .instr_type("crate::cpu::Instruction")
        .lut_mod("crate::cpu::lut")
        .group("alu", ["addi", "addis", "ori", "oris"])
        .group("branch", ["bx", "bcx"])
        .output(format!("{out_dir}/cpu_lut.rs"))
        .instr_type_output(format!("{out_dir}/cpu_instr.rs"))
        .run()
        .expect("chipi lut failed");
}
```

`chipi-build` automatically emits `cargo:rerun-if-changed` for all input files and their includes.

### Using generated code

```rust
mod dsp {
    include!(concat!(env!("OUT_DIR"), "/gcdsp.rs"));
}

match dsp::GcDspInstruction::decode(&data[offset..]) {
    Some((instr, bytes)) => {
        println!("{}", instr);
        offset += bytes;
    }
    None => println!("invalid instruction"),
}
```

## C++ backend

The C++ backend generates a single header with `enum class Opcode`, a tagged `Instruction` struct, `decode()`, and `format()` (using `std::format`).

Type aliases with `display(hex)` or `display(signed_hex)` automatically generate wrapper types (`Hex`, `SignedHex`) with `std::formatter` specializations. Custom types like registers are provided by the user via `type_map` and a user-written header with `std::formatter` specializations.

```toml
[gen.type_map]
gpr = "Gpr"    # user provides Gpr type with std::formatter<Gpr>
fpr = "Fpr"

[gen.lang_options]
includes = ["types.hpp"]   # user's type definitions
```

```cpp
// types.hpp (provided by user)
struct Gpr {
    uint8_t value;
    Gpr() = default;
    constexpr Gpr(uint8_t v) : value(v) {}
    bool operator==(const Gpr&) const = default;
    bool operator==(int other) const { return value == other; }
};

template <> struct std::formatter<Gpr> : std::formatter<std::string> {
    auto format(Gpr g, auto& ctx) const {
        return std::formatter<std::string>::format(std::format("r{}", g.value), ctx);
    }
};
```

## IDA backend

Generates a self-contained IDA Pro 9.x processor module (Python). Requires `lang_options` with processor metadata, register names, and optional flow analysis hints. See the `chipi.toml` example above.

## Binary Ninja backend

Generates a Binary Ninja Architecture plugin (Python). Implements `get_instruction_info`, `get_instruction_text`, and a stub `get_instruction_low_level_il`.

## DSL

### Decoder block

```chipi
decoder Ppc {
    width = 32        # bits per instruction unit
    bit_order = msb0  # msb0 or lsb0
    endian = big      # big or little
    max_units = 1     # optional safety guard
}
```

### Instructions

```chipi
addi [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:simm16[16:31]
     | ra == 0: "li {rd}, {simm}"
     | "addi {rd}, {ra}, {simm}"
```

- Fixed bits: `[range]=pattern` (use `?` for wildcards)
- Fields: `name:type[range]`
- Format lines: `| [guard:] "template"`

### Custom types

```chipi
type simm16 = i32 { sign_extend(16), display(signed_hex) }
type addr = u32 { shift_left(2), zero_extend(32) }
type gpr = u8
```

**Builtin types:** `bool`, `u1`-`u7`, `u8`, `u16`, `u32`, `i8`, `i16`, `i32`

**Transforms:** `sign_extend(n)`, `zero_extend(n)`, `shift_left(n)`

**Display hints:** `display(hex)`, `display(signed_hex)`

### Format strings

- Field references: `{field}`, `{field:#x}`
- Ternary: `{flag ? yes : no}`
- Arithmetic: `{a + b * 4}`, `{-field}`
- Map calls: `{spr_name(spr)}`
- Builtins: `{rotate_right(val, amt)}`, `{rotate_left(val, amt)}`
- Sub-decoder fragments: `{ext.mnemonic}`
- Guards: `| ra == 0: "li {rd}, {simm}"` (supports `==`, `!=`, `<`, `<=`, `>`, `>=`)
- Escapes: `\{`, `\}`, `\?`, `\:`

### Maps

```chipi
map spr_name(spr) {
    1 => "xer"
    8 => "lr"
    9 => "ctr"
    _ => "???"
}
```

### Includes

```chipi
include "dsp_ext.chipi"
```

Resolved relative to the including file. Circular includes are detected and rejected.

### Variable-length instructions

Bit positions beyond `width - 1` reference subsequent units:

```chipi
decoder GcDsp {
    width = 16
    bit_order = msb0
    endian = big
    max_units = 2
}

# 2-unit instruction: bits [16:31] are in the second unit
lri [0:10]=00000010000 rd:u5[11:15] imm:u16[16:31]
    | "lri r{rd}, #0x{imm:04x}"
```

### Sub-decoders

Sub-decoders decode a bit-field within a parent instruction using a separate dispatch table:

```chipi
subdecoder GcDspExt {
    width = 8
    bit_order = msb0
}

ext_dr  [0:5]=000001 r:u8[6:7]
        | .mnemonic = "'DR"
        | .operands = " : $ar{r}"

ext_nop [0:5]=000000 [6:7]=??
        | .mnemonic = ""
        | .operands = ""
```

Reference from a parent instruction:

```chipi
addr [0:3]=0100 [4]=0 ss:u8[5:6] d:u8[7] ext:GcDspExt[8:15]
     | "ADDR{ext.mnemonic} $ac{d}, ${ax2_name(ss)}{ext.operands}"
```

### Formatting trait (Rust)

The generated trait allows per-instruction display overrides:

```rust
struct MyFormat;
impl ppc::PpcFormat for MyFormat {
    fn fmt_addi(rd: u8, ra: u8, simm: i32, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ADDI r{}, r{}, {}", rd, ra, simm)
    }
}

println!("{}", instr.display::<MyFormat>());
```

## Dispatch strategies

| Strategy     | Rust                               |
| ------------ | ---------------------------------- |
| `fn_ptr_lut` | `static [Option<fn>; N]` (default) |
| `jump_table` | `#[inline(always)]` fn + `match`   |

## Handler stubs

Stubs are generated via the CLI only. They are one-shot scaffolding meant to be edited by hand:

```bash
chipi stubs --output src/cpu/interpreter.rs cpu.chipi
```

This produces:

```rust
pub fn addi(_ctx: &mut Ctx, _opcode: u32) { todo!("addi") }
pub fn lwz(_ctx: &mut Ctx, _opcode: u32) { todo!("lwz") }
// ... one stub per instruction
```

## Grouped handlers with const generics

`.group()` (in config or builder) folds multiple instructions into one handler via `const OP: u32`:

```rust
// Generated LUT entry
crate::cpu::interpreter::alu::<{ OP_ADDI }>

// Expected handler signature
pub fn alu<const OP: u32>(ctx: &mut crate::Cpu, instr: crate::cpu::Instruction) {
    match OP {
        OP_ADDI  => { /* ... */ },
        OP_ADDIS => { /* ... */ },
        _ => unreachable!(),
    }
}
```

## Instruction type generation

When `instr_type` and `instr_type_output` are set (in config or builder), chipi generates a newtype with field accessors:

```rust
pub struct Instruction(pub u32);
impl Instruction {
    #[inline] pub fn rd(&self) -> u8 { ((self.0 >> 21) & 0x1f) as u8 }
    #[inline] pub fn ra(&self) -> u8 { ((self.0 >> 16) & 0x1f) as u8 }
    #[inline] pub fn simm(&self) -> i32 { (((((self.0 & 0xffff) as i32) << 16) >> 16)) as i32 }
}
```