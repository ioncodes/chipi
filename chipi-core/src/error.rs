//! Error types and reporting for parsing and validation.

use std::fmt;

/// Source location information for error reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file: String,
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

impl Span {
    /// Create a new span with file and position information.
    pub fn new(file: &str, line: usize, col: usize, len: usize) -> Self {
        Span {
            file: file.to_string(),
            line,
            col,
            len,
        }
    }
}

/// Kinds of errors that can occur during parsing and validation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    // Lexer errors
    /// Unexpected character in input
    UnexpectedChar(char),

    // Parser errors
    /// Unexpected token
    UnexpectedToken(String),
    /// Invalid bit pattern syntax
    InvalidBitPattern(String),
    /// Expected token missing
    ExpectedToken(String),
    /// Invalid bit range specification
    InvalidRange,
    /// Invalid decoder width value
    InvalidWidth(u32),
    /// Missing decoder block in definition
    MissingDecoderBlock,

    // Validation errors
    /// Two instructions have the same name
    DuplicateInstructionName(String),
    /// Two type aliases have the same name
    DuplicateTypeAlias(String),
    /// Instruction doesn't specify bits for all positions
    BitCoverageGap {
        instruction: String,
        missing_bits: Vec<u32>,
    },
    /// Instruction specifies overlapping bits
    OverlappingBits { instruction: String, bit: u32 },
    /// Field references an undefined type
    UnresolvedType(String),
    /// Two instructions have the same fixed bit pattern
    PatternConflict { a: String, b: String },
    /// Fixed bit pattern length doesn't match range width
    PatternLengthMismatch {
        instruction: String,
        expected: u32,
        got: u32,
    },
    /// An import statement is unused
    UnusedImport(String),

    // Format/map errors
    /// Invalid format string syntax
    InvalidFormatString(String),
    /// Invalid guard condition syntax
    InvalidGuard(String),
    /// Format string references undefined field
    UndefinedFieldInFormat { instruction: String, field: String },
    /// Guard references undefined field
    UndefinedFieldInGuard { instruction: String, field: String },
    /// Map call references undefined map
    UndefinedMap(String),
    /// Map call has wrong number of arguments
    MapArgCountMismatch {
        map: String,
        expected: usize,
        got: usize,
    },
    /// Duplicate entry in a map
    DuplicateMapEntry { map: String },
    /// Duplicate map name
    DuplicateMapName(String),
    /// Non-last format line without a guard condition
    UnguardedNonLastFormatLine { instruction: String },
    /// Unknown builtin function name
    UnknownBuiltinFunction(String),

    // Variable-length instruction errors
    /// A bit range spans across unit boundaries
    CrossUnitBoundary {
        instruction: String,
        range_start: u32,
        range_end: u32,
        width: u32,
    },
    /// Instruction requires more units than max_units allows
    ExceedsMaxUnits {
        instruction: String,
        required: u32,
        max_units: u32,
    },

    // Sub-decoder errors
    /// Fragment names differ between instructions in the same sub-decoder
    InconsistentFragmentNames {
        subdecoder: String,
        instruction: String,
        expected: Vec<String>,
        got: Vec<String>,
    },
    /// Field bit-width exceeds sub-decoder's declared width
    SubDecoderFieldTooWide {
        field: String,
        field_width: u32,
        subdecoder: String,
        subdecoder_width: u32,
    },
    /// Referenced sub-decoder doesn't exist
    UndefinedSubDecoder(String),
    /// Dotted access to a non-existent fragment name
    UndefinedFragment {
        subdecoder: String,
        fragment: String,
    },
    /// Circular include detected
    CircularInclude(String),
    /// Included file not found
    IncludeNotFound(String),

    // --- Bindings-specific errors ---
    /// Generic parse error in a `*.bindings.chipi` file
    BindingsParse(String),
    /// Unknown target kind (e.g. `target foo`)
    UnknownTargetKind(String),
    /// Unknown strategy keyword (`strategy frob`)
    InvalidStrategy(String),
    /// Required field missing in a bindings block
    MissingBindingsField { block: String, field: String },
    /// Endianness value not in {big, little}
    InvalidEndianness(String),
    /// Multiple `target` blocks present and CLI did not pick one
    MultipleTargetsAmbiguous(Vec<String>),
    /// Multiple decoders/dispatches/processors/architectures present and
    /// CLI did not pick one with `--decoder`
    MultipleDecodersAmbiguous(Vec<String>),
    /// Decoder/dispatch/processor/architecture refers to a name that
    /// isn't defined in any included spec
    UnknownDecoderInBinding {
        name: String,
        suggestion: Option<String>,
    },
    /// Handler group references an instruction not present in the resolved
    /// decoder
    UnknownInstructionInGroup {
        instruction: String,
        suggestion: Option<String>,
    },
    /// IDA flow / Binja flow references an instruction not in the decoder
    UnknownInstructionInFlow {
        instruction: String,
        suggestion: Option<String>,
    },
    /// `segment_registers` references a name not declared in `registers`
    SegmentRegisterNotDeclared(String),
    /// Bindings dispatch missing required `invalid_handler`
    MissingInvalidHandler(String),
    /// Bindings include cycle
    BindingsCircularInclude(String),
    /// `flat_*` strategy: a raw value matches multiple instructions that
    /// resolve to different handlers
    FlatDispatchAmbiguous {
        raw: u64,
        matches: Vec<(String, String)>,
    },
}

/// An error with location and optional help text.
#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
    pub help: Option<String>,
}

impl Error {
    /// Create a new error with a kind and span.
    pub fn new(kind: ErrorKind, span: Span) -> Self {
        Error {
            kind,
            span,
            help: None,
        }
    }

    /// Add a help message to the error.
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match &self.kind {
            ErrorKind::UnexpectedChar(c) => format!("unexpected character '{}'", c),
            ErrorKind::UnexpectedToken(t) => format!("unexpected token '{}'", t),
            ErrorKind::InvalidBitPattern(p) => format!("invalid bit pattern '{}'", p),
            ErrorKind::ExpectedToken(t) => format!("expected {}", t),
            ErrorKind::InvalidRange => "invalid bit range".to_string(),
            ErrorKind::InvalidWidth(w) => format!("invalid decoder width: {}", w),
            ErrorKind::MissingDecoderBlock => "missing decoder block".to_string(),
            ErrorKind::DuplicateInstructionName(n) => {
                format!("duplicate instruction name '{}'", n)
            }
            ErrorKind::DuplicateTypeAlias(n) => format!("duplicate type alias '{}'", n),
            ErrorKind::BitCoverageGap {
                instruction,
                missing_bits,
            } => {
                format!(
                    "instruction '{}' has uncovered bits: {:?}",
                    instruction, missing_bits
                )
            }
            ErrorKind::OverlappingBits { instruction, bit } => {
                format!(
                    "instruction '{}' has overlapping coverage at bit {}",
                    instruction, bit
                )
            }
            ErrorKind::UnresolvedType(t) => format!("unresolved type '{}'", t),
            ErrorKind::PatternConflict { a, b } => {
                format!(
                    "instructions '{}' and '{}' have conflicting fixed bit patterns",
                    a, b
                )
            }
            ErrorKind::PatternLengthMismatch {
                instruction,
                expected,
                got,
            } => {
                format!(
                    "instruction '{}': fixed pattern length {} doesn't match range width {}",
                    instruction, got, expected
                )
            }
            ErrorKind::UnusedImport(path) => format!("unused import '{}'", path),
            ErrorKind::InvalidFormatString(msg) => format!("invalid format string: {}", msg),
            ErrorKind::InvalidGuard(msg) => format!("invalid guard condition: {}", msg),
            ErrorKind::UndefinedFieldInFormat { instruction, field } => {
                format!(
                    "format string in '{}' references undefined field '{}'",
                    instruction, field
                )
            }
            ErrorKind::UndefinedFieldInGuard { instruction, field } => {
                format!(
                    "guard in '{}' references undefined field '{}'",
                    instruction, field
                )
            }
            ErrorKind::UndefinedMap(name) => format!("undefined map '{}'", name),
            ErrorKind::MapArgCountMismatch { map, expected, got } => {
                format!(
                    "map '{}' expects {} arguments but got {}",
                    map, expected, got
                )
            }
            ErrorKind::DuplicateMapEntry { map } => {
                format!("duplicate entry in map '{}'", map)
            }
            ErrorKind::DuplicateMapName(name) => format!("duplicate map name '{}'", name),
            ErrorKind::UnguardedNonLastFormatLine { instruction } => {
                format!(
                    "non-last format line in '{}' must have a guard condition",
                    instruction
                )
            }
            ErrorKind::UnknownBuiltinFunction(name) => {
                format!("unknown builtin function '{}'", name)
            }
            ErrorKind::CrossUnitBoundary {
                instruction,
                range_start,
                range_end,
                width,
            } => {
                format!(
                    "instruction '{}': bit range [{}:{}] spans across unit boundary (width={})",
                    instruction, range_start, range_end, width
                )
            }
            ErrorKind::ExceedsMaxUnits {
                instruction,
                required,
                max_units,
            } => {
                format!(
                    "instruction '{}' requires {} units but decoder max_units is {}",
                    instruction, required, max_units
                )
            }
            ErrorKind::InconsistentFragmentNames {
                subdecoder,
                instruction,
                expected,
                got,
            } => {
                format!(
                    "sub-decoder '{}': instruction '{}' has fragments {:?} but expected {:?}",
                    subdecoder, instruction, got, expected
                )
            }
            ErrorKind::SubDecoderFieldTooWide {
                field,
                field_width,
                subdecoder,
                subdecoder_width,
            } => {
                format!(
                    "field '{}' is {} bits wide but sub-decoder '{}' is only {} bits",
                    field, field_width, subdecoder, subdecoder_width
                )
            }
            ErrorKind::UndefinedSubDecoder(name) => {
                format!("undefined sub-decoder '{}'", name)
            }
            ErrorKind::UndefinedFragment {
                subdecoder,
                fragment,
            } => {
                format!(
                    "sub-decoder '{}' has no fragment named '{}'",
                    subdecoder, fragment
                )
            }
            ErrorKind::CircularInclude(path) => {
                format!("circular include detected: '{}'", path)
            }
            ErrorKind::IncludeNotFound(path) => {
                format!("included file not found: '{}'", path)
            }

            // --- Bindings-specific ---
            ErrorKind::BindingsParse(msg) => msg.clone(),
            ErrorKind::UnknownTargetKind(name) => {
                format!(
                    "unknown target kind '{}': expected one of rust, cpp, ida, binja",
                    name
                )
            }
            ErrorKind::InvalidStrategy(name) => {
                format!(
                    "unknown dispatch strategy '{}': expected one of fn_ptr_lut, jump_table, flat_lut, flat_match",
                    name
                )
            }
            ErrorKind::MissingBindingsField { block, field } => {
                format!("{} is missing required field '{}'", block, field)
            }
            ErrorKind::InvalidEndianness(value) => {
                format!("invalid endianness '{}': expected 'big' or 'little'", value)
            }
            ErrorKind::MultipleTargetsAmbiguous(names) => {
                let mut s = String::from("multiple targets found:");
                for name in names {
                    s.push_str("\n   ");
                    s.push_str(name);
                }
                s.push_str("\npass one explicitly with --target");
                s
            }
            ErrorKind::MultipleDecodersAmbiguous(names) => {
                let mut s = String::from("multiple dispatch targets found:");
                for name in names {
                    s.push_str("\n   ");
                    s.push_str(name);
                }
                s.push_str("\npass one explicitly with --decoder");
                s
            }
            ErrorKind::UnknownDecoderInBinding { name, .. } => {
                format!("unknown decoder '{}' in bindings block", name)
            }
            ErrorKind::UnknownInstructionInGroup { instruction, .. } => {
                format!("unknown instruction '{}' in handler group", instruction)
            }
            ErrorKind::UnknownInstructionInFlow { instruction, .. } => {
                format!("unknown instruction '{}' in flow block", instruction)
            }
            ErrorKind::SegmentRegisterNotDeclared(name) => {
                format!(
                    "segment_registers entry '{}' is not declared in registers",
                    name
                )
            }
            ErrorKind::MissingInvalidHandler(decoder) => {
                format!(
                    "dispatch '{}' requires an `invalid_handler` directive",
                    decoder
                )
            }
            ErrorKind::BindingsCircularInclude(path) => {
                format!("circular bindings include detected: '{}'", path)
            }
            ErrorKind::FlatDispatchAmbiguous { raw, matches } => {
                let mut s = format!(
                    "flat dispatch cannot resolve raw opcode {:#010x}\n   matched instructions:",
                    raw
                );
                for (instr, handler) in matches {
                    s.push_str(&format!("\n     {} -> {}", instr, handler));
                }
                s.push_str(
                    "\n   flat dispatch requires each raw value to resolve to exactly one handler.",
                );
                s
            }
        };

        write!(f, "error: {}", msg)?;
        write!(f, "\n --> {}:{}", self.span.file, self.span.line)?;

        // Auto-emit `did you mean` from suggestion variants if the user
        // didn't already attach a help message.
        let auto_help = if self.help.is_none() {
            match &self.kind {
                ErrorKind::UnknownDecoderInBinding {
                    suggestion: Some(s),
                    ..
                }
                | ErrorKind::UnknownInstructionInGroup {
                    suggestion: Some(s),
                    ..
                }
                | ErrorKind::UnknownInstructionInFlow {
                    suggestion: Some(s),
                    ..
                } => Some(format!("did you mean \"{}\"?", s)),
                _ => None,
            }
        } else {
            None
        };

        if let Some(help) = self.help.as_ref().or(auto_help.as_ref()) {
            write!(f, "\n = help: {}", help)?;
        }

        Ok(())
    }
}

impl std::error::Error for Error {}

/// Multiple errors collected from parsing or validation.
#[derive(Debug)]
pub struct Errors(pub Vec<Error>);

impl fmt::Display for Errors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, err) in self.0.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", err)?;
        }
        Ok(())
    }
}

impl std::error::Error for Errors {}
