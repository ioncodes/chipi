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
    OverlappingBits {
        instruction: String,
        bit: u32,
    },
    /// Field references an undefined type
    UnresolvedType(String),
    /// Two instructions have the same fixed bit pattern
    PatternConflict {
        a: String,
        b: String,
    },
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
    MapArgCountMismatch { map: String, expected: usize, got: usize },
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
        };

        write!(f, "error: {}", msg)?;
        write!(
            f,
            "\n --> {}:{}",
            self.span.file, self.span.line
        )?;

        if let Some(help) = &self.help {
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
