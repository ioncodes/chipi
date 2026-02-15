use chipi::*;

#[test]
fn test_variable_length_dsp() {
    let source = r#"
decoder TestDsp {
    width = 16
    bit_order = msb0
    max_units = 2
}

nop     [0:15]=0000000000000000
        | "nop"

halt    [0:15]=0000000000100001
        | "halt"

lri     [0:10]=00000010000 rd:u5[11:15] imm:u16[16:31]
        | "lri r{rd}, #0x{imm:04x}"

call    [0:15]=0000001010111111 addr:u16[16:31]
        | "call 0x{addr:04x}"
"#;

    let result = generate_from_str(source, "test_dsp.chipi");
    match result {
        Ok(code) => {
            // Should generate variable-length decode function
            assert!(code.contains("pub fn decode(units: &[u16]) -> Option<(Self, usize)>"));
            // Should have bounds checks for 2-unit instructions
            assert!(code.contains("units.len() >= 2"));
            // Should extract from units[1]
            assert!(code.contains("units[1]"));
            println!("Generated code:\n{}", code);
        }
        Err(e) => {
            panic!("Failed to generate code: {}", e);
        }
    }
}

#[test]
fn test_backward_compatibility_single_unit() {
    let source = r#"
decoder TestCpu {
    width = 32
    bit_order = msb0
}

nop     [0:31]=00000000000000000000000000000000
        | "nop"

add     [0:5]=000001 rd:u5[6:10] rs:u5[11:15] rt:u5[16:20] [21:31]=00000000000
        | "add r{rd}, r{rs}, r{rt}"
"#;

    let result = generate_from_str(source, "test_dsp.chipi");
    match result {
        Ok(code) => {
            // Should generate single-unit decode function (backward compatible)
            assert!(code.contains("pub fn decode(opcode: u32) -> Option<Self>"));
            // Should NOT have variable-length features
            assert!(!code.contains("units: &["));
            assert!(!code.contains("units.len()"));
            println!("Generated code:\n{}", code);
        }
        Err(e) => {
            panic!("Failed to generate code: {}", e);
        }
    }
}

#[test]
fn test_max_units_validation() {
    let source = r#"
decoder TestDsp {
    width = 16
    bit_order = msb0
    max_units = 1
}

# This should fail validation - requires 2 units but max_units is 1
lri     [0:10]=00000010000 rd:u5[11:15] imm:u16[16:31]
        | "lri r{rd}, #0x{imm:04x}"
"#;

    let result = generate_from_str(source, "test_dsp.chipi");
    assert!(result.is_err());
    let err_str = result.unwrap_err().to_string();
    assert!(err_str.contains("max_units"));
}

#[test]
fn test_cross_unit_field_extraction() {
    let source = r#"
decoder TestDsp {
    width = 16
    bit_order = msb0
    max_units = 2
}

# Field spanning units is now SUPPORTED
# In MSB0: bits 14-31 spans from unit 0 (bits 14-15) into unit 1 (bits 0-15)
cross_unit [0:13]=00000000000000 field:u32[14:31]
           | "cross {field:#x}"
"#;

    let result = generate_from_str(source, "test_dsp.chipi");
    match result {
        Ok(code) => {
            // Should generate variable-length decode function
            assert!(code.contains("pub fn decode(units: &[u16]) -> Option<(Self, usize)>"));
            // Should extract from both unit 0 and unit 1
            assert!(code.contains("units[1]"));
            // Should have multi-range extraction (combining bits from multiple units)
            println!("Generated code:\n{}", code);
        }
        Err(e) => {
            panic!("Cross-unit fields should now be supported! Error: {}", e);
        }
    }
}

#[test]
fn test_unit1_pattern_disambiguation() {
    let source = r#"
decoder TestDsp {
    width = 16
    bit_order = msb0
    max_units = 2
}

# Two instructions with same unit 0 pattern but different unit 1 patterns
# This tests that unit 1+ fixed bits are properly checked
instr_a [0:15]=0000000100000000 [16:31]=0000000000000000
        | "instr_a"

instr_b [0:15]=0000000100000000 [16:31]=1111111111111111
        | "instr_b"
"#;

    let result = generate_from_str(source, "test_dsp.chipi");
    match result {
        Ok(code) => {
            // Should generate variable-length decode
            assert!(code.contains("pub fn decode(units: &[u16]) -> Option<(Self, usize)>"));
            // Should check unit 1 patterns to distinguish instructions
            assert!(code.contains("units[1]"));
            // Should have guards checking unit 1 bits
            println!("Generated code:\n{}", code);
        }
        Err(e) => {
            panic!("Should support unit 1 pattern disambiguation! Error: {}", e);
        }
    }
}
