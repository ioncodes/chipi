use chipi::*;

#[test]
fn test_overlapping_patterns() {
    let source = r#"
decoder Test {
    width = 8
    bit_order = msb0
}

# Generic - matches 0x1X (any X in bits 4-7)
generic [0:3]=0001 val:u4[4:7]
        | "generic {val}"

# Specific - matches only 0x1A
specific [0:3]=0001 [4:7]=1010
         | "specific"
"#;

    let result = generate_from_str(source, "test.chipi");
    match result {
        Ok(code) => {
            // Should have both instructions in the enum
            assert!(code.contains("Generic { val: u8 }"));
            assert!(code.contains("Specific"));

            // Should have both instructions in decode logic
            // The specific pattern should be checked first (more restrictive guard)
            assert!(code.contains("0xff == 0x1a")); // Specific pattern check

            // The generic pattern should be a fallback
            assert!(code.contains("0xf0 == 0x10")); // Generic pattern check (upper 4 bits)
        }
        Err(e) => panic!("Should support overlapping patterns: {}", e),
    }
}

#[test]
fn test_multiple_overlapping_patterns() {
    let source = r#"
decoder Test {
    width = 8
    bit_order = msb0
}

# Most generic - matches 0xXX (any value)
catch_all val:u8[0:7]
          | "unknown {val:#x}"

# Semi-specific - matches 0x1X
semi [0:3]=0001 data:u4[4:7]
     | "semi {data}"

# Most specific - matches only 0x1A
specific [0:3]=0001 [4:7]=1010
         | "specific"
"#;

    let result = generate_from_str(source, "test.chipi");
    match result {
        Ok(code) => {
            // All three instructions should exist
            assert!(code.contains("CatchAll { val: u8 }"));
            assert!(code.contains("Semi { data: u8 }"));
            assert!(code.contains("Specific"));

            // Should check in order: specific -> semi -> catch_all
            // Most specific pattern should be checked first
            println!("Generated code:\n{}", code);
        }
        Err(e) => panic!("Should support multiple overlapping patterns: {}", e),
    }
}

#[test]
fn test_overlapping_with_multi_unit() {
    let source = r#"
decoder Test {
    width = 16
    bit_order = msb0
    max_units = 2
}

# Generic - unit 0 fixed, unit 1 is wildcard
generic [0:15]=0000000100000000 data:u16[16:31]
        | "load {data:#x}"

# Specific - both units fully fixed
specific [0:15]=0000000100000000 [16:31]=1111111111111111
         | "load_max"
"#;

    let result = generate_from_str(source, "test.chipi");
    match result {
        Ok(code) => {
            // Should generate variable-length decode
            assert!(code.contains("pub fn decode(units: &[u16]) -> Option<(Self, usize)>"));

            // Both instructions should exist
            assert!(code.contains("Generic { data: u16 }"));
            assert!(code.contains("Specific"));

            // Specific should check both unit 0 and unit 1
            assert!(code.contains("units[1]"));

            // Should check specific first (both units fixed) then generic (unit 1 wildcard)
            println!("Generated code:\n{}", code);
        }
        Err(e) => panic!("Should support multi-unit overlapping patterns: {}", e),
    }
}
