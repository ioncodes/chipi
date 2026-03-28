# chipi

A declarative instruction decoder generator using a custom DSL. Define your CPU's instruction encoding in a `.chipi` file, and chipi generates a decoder and disassembler for you.

An example disassembler for GameCube CPU and DSP can be found [here](https://github.com/ioncodes/chipi-gekko).

## Usage

Add to your `Cargo.toml`:

```toml
[build-dependencies]
chipi = "0.6.0"
```

In `build.rs`:

```rs
use std::env;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    chipi::generate("ppc.chipi", out_dir.join("ppc.rs").to_str().unwrap())
        .expect("failed to generate decoder");
    println!("cargo:rerun-if-changed=ppc.chipi");
}
```

Then use it:

```rs
mod ppc {
    include!(concat!(env!("OUT_DIR"), "/ppc.rs"));
}

match ppc::PpcInstruction::decode(&data[offset..]) {
    Some((instr, bytes)) => {
        println!("{}", instr);  // uses generated Display impl
        offset += bytes;
    }
    None => {
        println!(".long {:#010x}", raw);
        offset += 4;
    }
};
```

The generated `decode()` has the signature `pub fn decode(data: &[u8]) -> Option<(Self, usize)>`, where `usize` is the number of bytes consumed.

The generated `Display` impl uses format lines defined in the DSL. You can also override formatting per-instruction by implementing the generated trait:

```rs
struct MyFormat;
impl ppc::PpcFormat for MyFormat {
    fn fmt_bx(li: i32, aa: bool, lk: bool, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "BRANCH {:#x}", li)
    }
}

// Use custom formatting
println!("{}", instr.display::<MyFormat>());
```

## DSL

Create a `.chipi` file describing your instruction set:

```chipi
decoder Ppc {
    width = 32
    bit_order = msb0
    endian = big
}

type simm16 = i32 { sign_extend(16) }
type simm24 = i32 { sign_extend(24), shift_left(2) }

# Branch
bx      [0:5]=010010 li:simm24[6:29] aa:bool[30] lk:bool[31]
        | "b{lk ? l}{aa ? a} {li:#x}"

# Arithmetic
addi    [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:simm16[16:31]
        | "addi {rd}, {ra}, {simm}"
```

- `decoder` block sets the instruction width (in bits), bit ordering, and byte endianness
- Each line defines an instruction: a name, fixed-bit patterns for matching, and named fields to extract
- Fields have a name, a type (`u8`, `u16`, ...), and a bit range
- Fixed bits use `[range]=value` syntax
  - Use `0` or `1` for bits that must match exactly
  - Use `?` for wildcard bits
- Format lines start with `|` and define disassembly output
- Comments start with `#`

### Bit ordering

- `msb0`: position 0 is the most significant bit
- `lsb0`: position 0 is the least significant bit

### Wildcard bits

Use `?` in fixed bit patterns to indicate "don't care" bits that can be any value. This is useful when certain bit positions are reserved, unused, or ignored by the hardware:

```chipi
# Match when bits [15:8] are 0x8c, bits [7:0] can be anything
clr15   [15:0]=10001100????????
        | "CLR15"

# Mix wildcards with specific bits
nop     [7:4]=0000 [3:0]=????
        | "nop"
```

Wildcard bits are excluded from the matching mask, meaning instructions will match regardless of the values in those positions. This allows you to accurately represent instruction encodings where certain bits are architecturally undefined or reserved.

### Variable-Length Instructions

Chipi supports variable-length instructions. When a bit position exceeds `width - 1`, it implicitly references subsequent units (1 unit = `width` bits). The unit index is automatically computed as `bit_position / width`.

```chipi
decoder GcDsp {
    width = 16
    bit_order = msb0
    endian = big          # byte endianness (big or little, default: big)
    max_units = 2         # optional safety check
}

# 1-unit instruction: all bits within [0:15]
nop     [0:15]=0000000000000000
        | "nop"

# 2-unit instruction: bits [16:31] are in the second unit
lri     [0:10]=00000010000 rd:u5[11:15] imm:u16[16:31]
        | "lri r{rd}, #0x{imm:04x}"

call    [0:15]=0000001010111111 addr:u16[16:31]
        | "call 0x{addr:04x}"
```

The generated decoder always accepts `&[u8]` and returns bytes consumed. For single-unit instructions, it returns `width / 8` bytes; for multi-unit instructions, it returns `unit_count * (width / 8)` bytes.

### Optional Safety Guard: `max_units`

The `max_units` decoder option acts as a compile-time safety net:

```chipi
decoder GcDsp {
    width = 16
    bit_order = msb0
    endian = big
    max_units = 2       # enforce maximum instruction length
}
```

It ensures at compile-time that bitranges do not exceed `max_units * width`. Helps with catching typos.

### Custom types

Use `type` to create type aliases with optional transformations:

```chipi
# Simple alias
type byte = u8

# With transformation
type simm16 = i32 { sign_extend(16) }

# Multiple transformations (comma-separated)
type addr = u32 { shift_left(2), zero_extend(32) }

# With display format hint
type simm16 = i32 { sign_extend(16), display(signed_hex) }
type uimm = u16 { display(hex) }
```

To map a chipi type to a Rust wrapper type (e.g. a newtype for registers), use `CodegenBuilder::type_map()` in `build.rs` instead of declaring it in the DSL. This keeps the `.chipi` file language-agnostic:

```rs
chipi::CodegenBuilder::new("cpu.chipi")
    .type_map("gpr", "crate::cpu::Gpr")    // gpr fields -> Gpr in generated code
    .type_map("fpr", "crate::cpu::Fpr")
    .output(out_dir.join("cpu.rs").to_str().unwrap())
    .run()?;
```

**Builtin types:**
- `bool`: Converts extracted bit to `bool`
- `u1` through `u7`: Extracted as `u8` in Rust
- `u8`, `u16`, `u32`: Unsigned integer types
- `i8`, `i16`, `i32`: Signed integer types

**Builtin transformations:**
- `sign_extend(n)`: Sign-extends the extracted value from n bits
- `zero_extend(n)`: Zero-extends the extracted value from n bits
- `shift_left(n)`: Shifts the value left by n bits

**Display formats:**
- `display(signed_hex)`: Formats as signed hex (`0x1A`, `-0x1A`, `0`)
- `display(hex)`: Formats as unsigned hex (`0x1A`, `0`)

### Includes

Use `include` to pull in definitions from other `.chipi` files:

```chipi
include "dsp_ext.chipi"

decoder GcDsp {
    width = 16
    bit_order = msb0
    max_units = 2
}
```

Includes are resolved relative to the including file's directory. The included file can contain `subdecoder` declarations, maps, types, or any other top-level construct. Circular includes are detected and rejected.

### Sub-decoders

Sub-decoders let one decoder reference another inline, so that a bit-field within an instruction is decoded by a separate dispatch table and its result is embedded into the parent's output.

This is useful when an ISA packs multiple operations into a single instruction word (e.g., the GameCube DSP packs a main opcode and an extension opcode into one 16-bit word).

#### Declaring a sub-decoder

Use `subdecoder` (instead of `decoder`) to define a sub-decoder. Sub-decoders only support `width` and `bit_order`:

```chipi
subdecoder GcDspExt {
    width = 8
    bit_order = msb0
}
```

#### Sub-decoder instructions

Instructions within a sub-decoder use **named fragment lines** instead of regular format strings. Each fragment is a `| .name = "template"` line:

```chipi
ext_dr      [0:5]=000001 r:u8[6:7]
            | .mnemonic = "'DR"
            | .operands = " : $ar{r}"

ext_nop     [0:5]=000000 [6:7]=??
            | .mnemonic = ""
            | .operands = ""
```

All instructions in a sub-decoder must declare the **same set of fragment names**. The names are arbitrary and chosen by the DSL author.

#### Referencing a sub-decoder

In a parent instruction, use the sub-decoder name as the field type:

```chipi
addr  [0:3]=0100 [4]=0 ss:u8[5:6] d:u8[7] ext:GcDspExt[8:15]
      | "ADDR{ext.mnemonic} $ac{d}, ${ax2_name(ss)}{ext.operands}"
```

Access fragment values with dotted syntax: `{ext.mnemonic}`, `{ext.operands}`. Interpolation is pure verbatim string substitution.

When the field bit-range is narrower than the sub-decoder's declared width (e.g., 7 bits for an 8-bit sub-decoder), the extracted value is zero-extended before dispatch.

#### Sub-decoder dispatch strategy

By default, sub-decoders use `Dispatch::FnPtrLut` (no inlining). For emulator hot paths, opt into `Dispatch::JumpTable` (`#[inline(always)]`) via `build.rs`:

```rs
chipi::CodegenBuilder::new("dsp.chipi")
    .decoder_dispatch("GcDspExt", chipi::Dispatch::JumpTable)
    .output(out_dir.join("dsp.rs").to_str().unwrap())
    .run()?;
```

### Format lines

Format lines follow an instruction definition and control how it is displayed. They start with `|` and contain a quoted format string:

```chipi
bx  [0:5]=010010 li:simm24[6:29] aa:bool[30] lk:bool[31]
    | "b{lk ? l}{aa ? a} {li:#x}"
```

This produces `b 0x100`, `bl 0x100`, `ba 0x100`, or `bla 0x100` depending on the flag fields.

**Field references:** `{field}` inserts a field value. Add a format specifier with `{field:#x}` (hex), `{field:#b}` (binary), etc.

**Ternary expressions:** `{field ? text}` emits `text` if the field is nonzero, nothing otherwise. `{field ? yes : no}` provides an else branch.

**Arithmetic:** `{a + b * 4}` evaluates inline arithmetic (`+`, `-`, `*`, `/`, `%`).

**Unary negation:** `{-field}` negates a field value.

```chipi
addi  [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:simm16[16:31]
      | simm < 0 : "subi {rd}, {ra}, {-simm}"
      | "addi {rd}, {ra}, {simm}"
```

**Builtin functions:** `{rotate_right(val, amt)}` and `{rotate_left(val, amt)}`.

**Sub-decoder fragment access:** `{ext.mnemonic}` inserts a named fragment from a sub-decoder field verbatim.

**Guards:** Multiple format lines can be used with guard conditions to select different output based on field values:

```chipi
addi  [0:5]=001110 rd:u8[6:10] ra:u8[11:15] simm:simm16[16:31]
      | ra == 0: "li {rd}, {simm}"
      | "addi {rd}, {ra}, {simm}"
```

Guard conditions support `==`, `!=`, `<`, `<=`, `>`, `>=` and can be joined with `,` or `&&`. Guard operands can be field names, integer literals, or arithmetic expressions (`sh == 32 - mb`). The last format line may omit the guard (acts as the default).

**Escapes:** Use `\{`, `\}`, `\?`, `\:` to emit literal characters.

### Maps

Maps define lookup tables for use in format strings:

```chipi
map spr_name(spr) {
    1 => "xer"
    8 => "lr"
    9 => "ctr"
    _ => "???"
}

mtspr  [0:5]=011111 rs:u8[6:10] spr:u16[11:20] [21:30]=0111010011 [31]=0
       | "mtspr {spr_name(spr)}, {rs}"
```

Map parameters can also use `{param}` interpolation in the output, in which case the map returns a `String` instead of `&'static str`:

```chipi
map ea(mode, reg) {
    0, _ => "d{reg}"
    1, _ => "a{reg}"
    _ => "???"
}
```

Maps can be declared inside a `subdecoder` block to scope them to that sub-decoder.

### Formatting trait

chipi generates a trait (e.g. `PpcFormat`) with one method per instruction. Each method has a default implementation from the format lines. To override specific instructions, implement the trait on your own struct:

```rs
struct MyFormat;
impl ppc::PpcFormat for MyFormat {
    // Override just this one; all others keep their defaults
    fn fmt_addi(rd: u8, ra: u8, simm: i32,
                f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ADDI r{}, r{}, {}", rd, ra, simm)
    }
}

println!("{}", instr.display::<MyFormat>());
```

Instructions without format lines get a raw fallback: `instr_name field1, field2, ...`.

## CodegenBuilder

`CodegenBuilder` provides fine-grained control over code generation, including type mappings and dispatch strategies:

```rs
chipi::CodegenBuilder::new("dsp.chipi")
    .type_map("reg5", "crate::dsp::DspReg")          // map chipi type -> Rust type
    .decoder_dispatch("GcDspExt", chipi::Dispatch::JumpTable)  // inline sub-decoder
    .output(out_dir.join("dsp.rs").to_str().unwrap())
    .run()?;
```

### Type mappings

`.type_map(chipi_type, rust_path)` maps a chipi type alias to a Rust wrapper type. The generated code emits `use` statements for paths containing `::` and wraps extracted values with `Type::from(val)`.

### Dispatch strategies

`.decoder_dispatch(name, strategy)` controls how a sub-decoder dispatches:

- `Dispatch::FnPtrLut` (default): `static [Option<fn>; N]` array with indirect calls. Fast to compile.
- `Dispatch::JumpTable`: `#[inline(always)]` match statement. The compiler can inline the entire decode chain. Good for emulator hot paths, but can cause slow compilation with large sub-decoders.

## Instruction Type Generation

chipi can generate a newtype wrapper with field accessor methods, eliminating the need to hand-write bit extraction code. This is useful in cases where a thin wrapper for decoding is prefered (e.g. emulation).

### Usage

In `build.rs`:

```rs
let builder = chipi::LutBuilder::new("cpu.chipi")
    .instr_type("crate::cpu::Instruction");

// Generate the instruction type with accessor methods
builder
    .build_instr_type(out_dir.join("cpu_instr.rs").to_str().unwrap())
    .expect("failed to generate instruction type");
```

Or use the standalone function:

```rs
chipi::generate_instr_type("cpu.chipi", "out/instruction.rs", "Instruction")?;
```

This generates:

```rs
// Auto-generated by chipi. Do not edit.

pub struct Instruction(pub u32);

#[rustfmt::skip]
impl Instruction {
    #[inline] pub fn rd(&self) -> u8 { ((self.0 >> 21) & 0x1f) as u8 }
    #[inline] pub fn ra(&self) -> u8 { ((self.0 >> 16) & 0x1f) as u8 }
    #[inline] pub fn simm(&self) -> i32 { (((((self.0 & 0xffff) as i32) << 16) >> 16)) as i32 }
    #[inline] pub fn rc(&self) -> bool { (self.0 & 0x1) != 0 }
    // ... one accessor per unique field
}
```

### Field Deduplication and Conflict Resolution

chipi collects all unique fields across all instructions. Fields with the same name and bit range are deduplicated. Fields with the same name but different bit ranges or types generate separate accessors with bit range suffixes.

For example, if your spec has:
- `d:disp[16:31]` in load/store instructions (16-bit displacement)
- `d:simm12[20:31]` in paired-single instructions (12-bit signed immediate)

chipi will generate both `d_16_31()` and `d_20_31()` accessors. You can add convenience aliases in a separate `impl` block:

```rs
impl Instruction {
    pub fn d(&self) -> i32 { self.d_16_31() }  // Most common variant
    pub fn d_psq(&self) -> i32 { self.d_20_31() }  // PSQ-specific
}
```

## Emulator Dispatch

In addition to the decoder/disassembler output, chipi can generate dispatch code for emulator interpreters. Two strategies are available:

- `Dispatch::FnPtrLut` (default): Static `[Handler; N]` function pointer arrays, one per tree level. Each opcode is routed via indirect call. Good general-purpose default.
- `Dispatch::JumpTable`: A single `#[inline(always)]` function with nested match statements that call handlers directly by name. When handlers are also `#[inline(always)]`, the compiler can flatten the entire dispatch + execution chain (hyperinlining).

Recommended to use along with the newtype generator.

### build.rs

Use `LutBuilder` to configure dispatch and stubs together:

```rs
use std::env;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let manifest = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let spec = "cpu.chipi";

    let builder = chipi::LutBuilder::new(spec)
        .handler_mod("crate::cpu::interpreter")
        .ctx_type("crate::Cpu")
        // .dispatch(chipi::Dispatch::JumpTable)  // opt into hyperinlining
        ;

    // Always regenerate the LUT tables from the spec.
    builder
        .build_lut(out_dir.join("cpu_lut.rs").to_str().unwrap())
        .expect("failed to generate LUT");

    // Generate handler stubs the first time only, never overwritten after that.
    let stubs = manifest.join("src/cpu/interpreter.rs");
    if !stubs.exists() {
        builder.build_stubs(stubs.to_str().unwrap())
            .expect("failed to generate stubs");
    }

    println!("cargo:rerun-if-changed={spec}");
}
```

### Include the LUT

```rs
// src/cpu.rs
#[allow(dead_code, non_upper_case_globals)]
pub mod lut {
    include!(concat!(env!("OUT_DIR"), "/cpu_lut.rs"));
}
```

### Dispatch

```rs
// fetch -> dispatch
let opcode = mem.read_u32(pc);
crate::cpu::lut::dispatch(&mut ctx, opcode);
```

### Handler functions

`build_stubs` writes `src/cpu/interpreter.rs` on the first build with `todo!()` bodies. The second parameter type is derived from the spec's `width`: `u8` (8-bit), `u16` (16-bit), or `u32` (32-bit).

```rs
pub fn addi(_ctx: &mut crate::Cpu, _opcode: u32) { todo!("addi") }
pub fn lwz(_ctx: &mut crate::Cpu, _opcode: u32) { todo!("lwz")  }
// ... one stub per instruction in the spec
```

Replace each `todo!()` with a real implementation as you go. The file is never regenerated, so hand-edits are preserved.

### Grouped handlers with const generics

Use `.group()` to fold multiple instructions into one handler function via a `const OP: u32` generic parameter. The compiler monomorphizes each entry.

Provide `.lut_mod()` so the generated stubs can `use` the `OP_*` constants:

```rs
chipi::LutBuilder::new("cpu.chipi")
    .handler_mod("crate::cpu::interpreter")
    .ctx_type("crate::Cpu")
    .lut_mod("crate::cpu::lut")
    .group("alu", ["addi", "addis", "ori", "oris"])
    .build_lut(out_dir.join("cpu_lut.rs").to_str().unwrap())?;
```

Generated LUT entry for `addi`:

```rs
crate::cpu::interpreter::alu::<{ OP_ADDI }>
```

Generated stub:

```rs
pub fn alu<const OP: u32>(_ctx: &mut crate::Cpu, _opcode: u32) {
    match OP {
        OP_ADDI  => todo!("addi"),
        OP_ADDIS => todo!("addis"),
        _ => unreachable!(),
    }
}
```

### Custom instruction wrapper type

Use `.instr_type()` to replace the raw integer with a richer type. chipi uses it in the generated `Handler` alias and all stub signatures. `.raw_expr()` specifies how to extract the underlying integer for table indexing; it defaults to `instr.0` for newtype wrappers.

```rs
chipi::LutBuilder::new("cpu.chipi")
    .handler_mod("crate::cpu::interpreter")
    .ctx_type("crate::Cpu")
    .instr_type("crate::cpu::Instruction")  // struct Instruction(pub u32)
    // .raw_expr("instr.0")                 // default for newtype wrappers
    .build_lut(out_dir.join("cpu_lut.rs").to_str().unwrap())?;
```

Generated `Handler` type and stub signature:

```rs
pub type Handler = fn(&mut crate::Cpu, crate::cpu::Instruction);

pub fn addi(_ctx: &mut crate::Cpu, _instr: crate::cpu::Instruction) { todo!("addi") }
```

### Generated output

The LUT file contains:

- `pub const OP_*: u32`: one constant per instruction, usable as const-generic arguments
- `pub type Handler = fn(&mut Ctx, T)`: handler function pointer type (`T` is `u8`/`u16`/`u32` or your wrapper)
- `static _T0: [Handler; 64]`: one table per dispatch level, sized to the bit range (e.g. 64 entries for a 6-bit primary opcode, 1024 for a 10-bit secondary)
- Inline `_d*` dispatch functions that index into each table
- `pub fn dispatch(ctx: &mut Ctx, opcode: T)`: the public entry point

Overlapping patterns (wildcards vs. specific) are handled by generated `_priority_*` functions that check bitmask guards in priority order.

## Syntax Highlighting

[vscode](https://github.com/ioncodes/chipi-vscode)
