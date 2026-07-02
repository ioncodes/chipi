//! Length-window validation: a leaf whose encoding selects a short `length` window must not
//! bind fields, fix bits or be classified through selector bits outside that window.

use chipi_core::compile;

fn codes(src: &str) -> Vec<&'static str> {
    match compile(src) {
        Ok(_) => Vec::new(),
        Err(e) => e.iter().map(|d| d.code).collect(),
    }
}

fn expect_length_window(src: &str) {
    let cs = codes(src);
    assert!(
        cs.contains(&"LengthWindow"),
        "expected `LengthWindow`, got {cs:?} for spec:\n{src}"
    );
}

fn expect_compiles(name: &str, src: &str) {
    if let Err(e) = compile(src) {
        let src_obj = chipi_core::Source::new(name, src);
        panic!(
            "`{name}` should compile:\n{}",
            chipi_core::render_diagnostics(&e, &src_obj)
        );
    }
}

// A dsp-shaped 16/32 spec: msb0, big endian, the opcode word in the numeric high half. The
// short leaf's window is numeric bits [16:31]; source bit 20 lands at numeric bit 11.
#[test]
fn short_leaf_binding_bit_20_is_an_error() {
    expect_length_window(
        "decoder D { width = 32 bit_order = msb0 endian = big }\n\
         length =\n\
         \x20 | word[31:26] == 0b000100 : 32\n\
         \x20 | else : 16\n\
         selector op [0:5]\n\
         short op=0b000000 [6:15]=0 f:u5[16:20] | \"short {f}\"\n\
         long op=0b000100 imm:u16[16:31] | \"long {imm}\"",
    );
}

// Same shape, but the short leaf fixes bits in the unfetched half instead of binding them.
#[test]
fn short_leaf_fixing_out_of_window_bits_is_an_error() {
    expect_length_window(
        "decoder D { width = 32 bit_order = msb0 endian = big }\n\
         length =\n\
         \x20 | word[31:26] == 0b000100 : 32\n\
         \x20 | else : 16\n\
         selector op [0:5]\n\
         short op=0b000000 [6:15]=0 [16:20]=0 | \"short\"\n\
         long op=0b000100 imm:u16[16:31] | \"long {imm}\"",
    );
}

// Little-endian orientation: the short window is the numeric low half, so a field past bit 15
// on a 16-bit leaf reads the next instruction's bytes.
#[test]
fn short_leaf_out_of_window_field_little_endian() {
    expect_length_window(
        "decoder D { width = 32 bit_order = lsb0 endian = little }\n\
         length =\n\
         \x20 | word[1:0] != 0b11 : 16\n\
         \x20 | else : 32\n\
         selector op [6:0]\n\
         short op=0b0000001 [15:7]=0 f:u5[24:20] | \"short {f}\"\n\
         long op=0b0010011 rd:u5[11:7] imm:i12[31:20] [19:12]=0 | \"long {rd}, {imm}\"",
    );
}

// The primary selector straddles the short window: `sel` reads bits [12:17], but leaf `a`
// selects a 16-bit window, so classification would key on unfetched bits 16 and 17. Leaf `a`
// itself only fixes in-window bits; the error comes from the classification check.
#[test]
fn primary_selector_straddling_short_window_is_an_error() {
    expect_length_window(
        "decoder D { width = 32 bit_order = lsb0 endian = little }\n\
         length =\n\
         \x20 | word[1:0] != 0b11 : 16\n\
         \x20 | else : 32\n\
         selector sel [17:12]\n\
         a [1:0]=0b01 [15:12]=0b0000 | \"a\"\n\
         b [1:0]=0b11 sel=0b101111 | \"b\"",
    );
}

// A leaf whose free bits feed the length condition has no single determined window; the
// probe (free bits at 0 and at 1) disagrees, so the check conservatively stays silent even
// though some matching words select the short window.
#[test]
fn undetermined_window_is_conservatively_accepted() {
    expect_compiles(
        "undetermined",
        "decoder D { width = 32 bit_order = lsb0 endian = little }\n\
         length =\n\
         \x20 | word[8:8] == 1 : 32\n\
         \x20 | else : 16\n\
         selector op [3:0]\n\
         x op=0b0001 [7:4]=0 amb:u1[8:8] f:u4[19:16] | \"x {amb} {f}\"",
    );
}

// A well-formed 16/32 spec where every leaf stays inside its own window still compiles.
#[test]
fn in_window_short_leaves_compile() {
    expect_compiles(
        "in_window",
        "decoder D { width = 32 bit_order = msb0 endian = big }\n\
         length =\n\
         \x20 | word[31:26] == 0b000100 : 32\n\
         \x20 | else : 16\n\
         selector op [0:5]\n\
         short op=0b000000 f:u8[8:15] [6:7]=0 | \"short {f}\"\n\
         long op=0b000100 imm:u16[16:31] [6:15]=0 | \"long {imm}\"",
    );
}

// The tracked variable-length examples keep compiling under the new pass.
#[test]
fn examples_still_compile() {
    expect_compiles(
        "riscv_rvc",
        include_str!("../../../examples/riscv_rvc.chipi"),
    );
    expect_compiles("gc_dsp", include_str!("../../../examples/gc_dsp.chipi"));
}
