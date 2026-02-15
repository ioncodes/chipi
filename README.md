# chipi

A declarative instruction decoder generator using a custom DSL. Define your CPUs instruction encoding in a `.chipi` file, and chipi generates a decoder and disassembler for you. Seemless interaction with Rust types.  

An example disassembler for GameCube CPU and DSP can be found [here](https://github.com/ioncodes/chipi-gekko).

## Usage

Add to your `Cargo.toml`:

```toml
[build-dependencies]
chipi = "0.1"
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

match ppc::PpcInstruction::decode(raw) {
    Some(i) => println!("{}", i),    // uses generated Display impl
    None => println!(".long {:#010x}", raw),
};
```

Note: The generated `decode()` function signature changes depending on amount of units (e.g. if the architecture is of variable length)! Example:
- Single-unit: `pub fn decode(opcode: u16) -> Option<Self>`
  - Returns `instruction` only
- Multi-unit: `pub fn decode(units: &[u16]) -> Option<(Self, usize)>`
  - Returns `(instruction, unit_count)` tuple, `unit_count` indicating the amount of units consumed

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
import crate::cpu::Register

decoder Ppc {
    width = 32
    bit_order = msb0
}

type reg = u8 as Register
type simm16 = i32 { sign_extend(16) }
type simm24 = i32 { sign_extend(24), shift_left(2) }

# Branch
bx      [0:5]=010010 li:simm24[6:29] aa:bool[30] lk:bool[31]
        | "b{lk ? l}{aa ? a} {li:#x}"

# Arithmetic
addi    [0:5]=001110 rd:reg[6:10] ra:reg[11:15] simm:simm16[16:31]
        | "addi {rd}, {ra}, {simm}"
```

- `decoder` block sets the instruction width (in bits) and bit ordering
- Each line defines an instruction: a name, fixed-bit patterns for matching, and named fields to extract
- Fields have a name, a type (`u8`, `u16`, ...), and a bit range
- Fixed bits use `[range]=value` syntax
- Format lines start with `|` and define disassembly output
- Comments start with `#`

### Bit ordering

- `msb0`: position 0 is the most significant bit
- `lsb0`: position 0 is the least significant bit

### Variable-Length Instructions

Chipi supports variable-length instructions. When a bit position exceeds `width - 1`, it implicitly references subsequent units (1 unit = `width` bits). The unit index is automatically computed as `bit_position / width`.

```chipi
decoder GcDsp {
    width = 16
    bit_order = msb0
    max_units = 2       # optional safety guard
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

### Optional Safety Guard: `max_units`

The `max_units` decoder option acts as a compile-time safety net:

```chipi
decoder GcDsp {
    width = 16
    bit_order = msb0
    max_units = 2       # enforce maximum instruction length
}
```

It ensures at compile-time that bitranges do not exceed `max_units * width`. Helps with catching typos.

### Custom types

Use `type` to create type aliases with optional transformations or wrappers:

```chipi
# Simple alias
type byte = u8

# With transformation
type simm16 = i32 { sign_extend(16) }

# With custom wrapper (must be imported)
type reg = u8 as Register

# Multiple transformations (comma-separated)
type addr = u32 { shift_left(2), zero_extend(32) }

# With display format hint
type simm16 = i32 { sign_extend(16), display(signed_hex) }
type uimm = u16 { display(hex) }
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
addi  [0:5]=001110 rd:reg[6:10] ra:reg[11:15] simm:simm16[16:31]
      | simm < 0 : "subi {rd}, {ra}, {-simm}"
      | "addi {rd}, {ra}, {simm}"
```

**Builtin functions:** `{rotate_right(val, amt)}` and `{rotate_left(val, amt)}`.

**Guards:** Multiple format lines can be used with guard conditions to select different output based on field values:

```chipi
addi  [0:5]=001110 rd:reg[6:10] ra:reg[11:15] simm:simm16[16:31]
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

mtspr  [0:5]=011111 rs:reg[6:10] spr:u16[11:20] [21:30]=0111010011 [31]=0
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

### Formatting trait

chipi generates a trait (e.g. `PpcFormat`) with one method per instruction. Each method has a default implementation from the format lines. To override specific instructions, implement the trait on your own struct:

```rs
struct MyFormat;
impl ppc::PpcFormat for MyFormat {
    // Override just this one; all others keep their defaults
    fn fmt_addi(rd: &Register, ra: &Register, simm: i32,
                f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ADDI r{}, r{}, {}", rd, ra, simm)
    }
}

println!("{}", instr.display::<MyFormat>());
```

Instructions without format lines get a raw fallback: `instr_name field1, field2, ...`.

## Syntax Highlighting

[vscode](https://github.com/ioncodes/chipi-vscode)
