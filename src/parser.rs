//! DSL parsing for instruction definitions.
//!
//! This module parses `.chipi` files (or source strings) into an intermediate representation.
//! The parser handles:
//! - Decoder configuration (name, width, bit order)
//! - Type aliases for custom field types
//! - Instruction definitions with fixed bit patterns and variable fields
//!
//! Bit ranges are converted from DSL notation (where the order depends on bit_order config)
//! to hardware notation (LSB=0) during validation.

use crate::error::{Error, ErrorKind, Span};
use crate::format_parser::parse_format_string;
use crate::types::*;

/// Parse DSL source text into a decoder definition.
///
/// # Arguments
///
/// * `source` - DSL source code
/// * `filename` - Name for error reporting (e.g., "my_arch.chipi")
///
/// # Returns
///
/// A `DecoderDef` on success, or a vector of parse errors.
pub fn parse(source: &str, filename: &str) -> Result<DecoderDef, Vec<Error>> {
    let mut parser = Parser::new(source, filename);
    parser.parse_file()
}

struct Parser<'a> {
    _source: &'a str,
    filename: String,
    lines: Vec<&'a str>,
    line_idx: usize,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, filename: &str) -> Self {
        let lines: Vec<&str> = source.lines().collect();
        Parser {
            _source: source,
            filename: filename.to_string(),
            lines,
            line_idx: 0,
            errors: Vec::new(),
        }
    }

    fn span(&self, col: usize, len: usize) -> Span {
        Span::new(&self.filename, self.line_idx + 1, col + 1, len)
    }

    fn advance(&mut self) {
        self.line_idx += 1;
    }

    fn parse_file(&mut self) -> Result<DecoderDef, Vec<Error>> {
        let mut imports = Vec::new();
        let mut config: Option<DecoderConfig> = None;
        let mut type_aliases = Vec::new();
        let mut maps = Vec::new();
        let mut instructions = Vec::new();

        while self.line_idx < self.lines.len() {
            let line = self.lines[self.line_idx];
            let trimmed = line.trim();

            if trimmed.is_empty() || trimmed.starts_with('#') {
                self.advance();
                continue;
            }

            if trimmed.starts_with("import ") {
                match self.parse_import(trimmed) {
                    Ok(imp) => imports.push(imp),
                    Err(e) => self.errors.push(e),
                }
                self.advance();
            } else if trimmed.starts_with("decoder ") {
                match self.parse_decoder_block() {
                    Ok(cfg) => config = Some(cfg),
                    Err(e) => self.errors.push(e),
                }
            } else if trimmed.starts_with("type ") {
                match self.parse_type_alias(trimmed) {
                    Ok(ta) => type_aliases.push(ta),
                    Err(e) => self.errors.push(e),
                }
                self.advance();
            } else if trimmed.starts_with("map ") {
                match self.parse_map_block() {
                    Ok(map) => maps.push(map),
                    Err(e) => self.errors.push(e),
                }
            } else {
                // Instruction line
                match self.parse_instruction(trimmed) {
                    Ok(instr) => instructions.push(instr),
                    Err(e) => self.errors.push(e),
                }
                self.advance();
                // Consume format lines (| ...)
                let format_lines = self.parse_format_lines();
                if let Some(last) = instructions.last_mut() {
                    last.format_lines = format_lines;
                }
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        let config = match config {
            Some(c) => c,
            None => {
                return Err(vec![Error::new(
                    ErrorKind::MissingDecoderBlock,
                    Span::new(&self.filename, 1, 1, 0),
                )]);
            }
        };

        Ok(DecoderDef {
            imports,
            config,
            type_aliases,
            maps,
            instructions,
        })
    }

    fn parse_import(&self, line: &str) -> Result<Import, Error> {
        let rest = line.strip_prefix("import ").unwrap().trim();
        if rest.is_empty() {
            return Err(Error::new(
                ErrorKind::ExpectedToken("import path".to_string()),
                self.span(7, 1),
            ));
        }
        Ok(Import {
            path: rest.to_string(),
            span: self.span(0, line.len()),
        })
    }

    fn parse_decoder_block(&mut self) -> Result<DecoderConfig, Error> {
        let first_line = self.lines[self.line_idx].trim();
        let block_start_line = self.line_idx;

        let rest = first_line
            .strip_prefix("decoder ")
            .unwrap()
            .trim();
        let name = rest
            .strip_suffix('{')
            .map(|s| s.trim())
            .unwrap_or(rest)
            .to_string();

        if name.is_empty() {
            return Err(Error::new(
                ErrorKind::ExpectedToken("decoder name".to_string()),
                self.span(8, 1),
            ));
        }

        self.advance();

        let mut width: Option<u32> = None;
        let mut bit_order: Option<BitOrder> = None;
        let mut max_units: Option<u32> = None;

        // Parse body lines until '}'
        while self.line_idx < self.lines.len() {
            let line = self.lines[self.line_idx].trim();

            if line == "}" {
                self.advance();
                break;
            }

            if line.is_empty() || line.starts_with('#') {
                self.advance();
                continue;
            }

            if let Some(val) = line.strip_prefix("width") {
                let val = val.trim().strip_prefix('=').map(|s| s.trim()).unwrap_or(val.trim());
                match val.parse::<u32>() {
                    Ok(w) if w == 8 || w == 16 || w == 32 => width = Some(w),
                    Ok(w) => {
                        return Err(Error::new(
                            ErrorKind::InvalidWidth(w),
                            self.span(0, line.len()),
                        ));
                    }
                    Err(_) => {
                        return Err(Error::new(
                            ErrorKind::ExpectedToken("integer width (8, 16, or 32)".to_string()),
                            self.span(0, line.len()),
                        ));
                    }
                }
            } else if let Some(val) = line.strip_prefix("bit_order") {
                let val = val.trim().strip_prefix('=').map(|s| s.trim()).unwrap_or(val.trim());
                match val {
                    "msb0" => bit_order = Some(BitOrder::Msb0),
                    "lsb0" => bit_order = Some(BitOrder::Lsb0),
                    _ => {
                        return Err(Error::new(
                            ErrorKind::ExpectedToken("msb0 or lsb0".to_string()),
                            self.span(0, line.len()),
                        ));
                    }
                }
            } else if let Some(val) = line.strip_prefix("max_units") {
                let val = val.trim().strip_prefix('=').map(|s| s.trim()).unwrap_or(val.trim());
                match val.parse::<u32>() {
                    Ok(m) if m > 0 => max_units = Some(m),
                    Ok(_) => {
                        return Err(Error::new(
                            ErrorKind::ExpectedToken("positive integer for max_units".to_string()),
                            self.span(0, line.len()),
                        ));
                    }
                    Err(_) => {
                        return Err(Error::new(
                            ErrorKind::ExpectedToken("positive integer for max_units".to_string()),
                            self.span(0, line.len()),
                        ));
                    }
                }
            }

            self.advance();
        }

        let width = width.unwrap_or(32);
        let bit_order = bit_order.unwrap_or(BitOrder::Msb0);

        Ok(DecoderConfig {
            name,
            width,
            bit_order,
            max_units,
            span: Span::new(&self.filename, block_start_line + 1, 1, 0),
        })
    }

    fn parse_type_alias(&self, line: &str) -> Result<TypeAlias, Error> {
        let rest = line.strip_prefix("type ").unwrap().trim();

        let eq_pos = rest.find('=').ok_or_else(|| {
            Error::new(
                ErrorKind::ExpectedToken("'=' in type alias".to_string()),
                self.span(5, rest.len()),
            )
        })?;

        let name = rest[..eq_pos].trim().to_string();
        let rhs = rest[eq_pos + 1..].trim();

        let (base_and_wrapper, transforms, display_format) = if let Some(brace_pos) = rhs.find('{') {
            let close = rhs.rfind('}').ok_or_else(|| {
                Error::new(
                    ErrorKind::ExpectedToken("closing '}'".to_string()),
                    self.span(0, line.len()),
                )
            })?;
            let transforms_str = &rhs[brace_pos + 1..close];
            let (transforms, display_format) = self.parse_transforms(transforms_str)?;
            (rhs[..brace_pos].trim(), transforms, display_format)
        } else {
            (rhs, Vec::new(), None)
        };

        let (base_type, wrapper_type) = if let Some(as_pos) = base_and_wrapper.find(" as ") {
            let base = base_and_wrapper[..as_pos].trim().to_string();
            let wrapper = base_and_wrapper[as_pos + 4..].trim().to_string();
            (base, Some(wrapper))
        } else {
            (base_and_wrapper.to_string(), None)
        };

        Ok(TypeAlias {
            name,
            base_type,
            wrapper_type,
            transforms,
            display_format,
            span: self.span(0, line.len()),
        })
    }

    fn parse_transforms(&self, s: &str) -> Result<(Vec<Transform>, Option<DisplayFormat>), Error> {
        let mut transforms = Vec::new();
        let mut display_format = None;
        for part in s.split(',') {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            if let Some(inner) = part.strip_prefix("sign_extend(").and_then(|s| s.strip_suffix(')'))
            {
                let n: u32 = inner.trim().parse().map_err(|_| {
                    Error::new(
                        ErrorKind::ExpectedToken("integer for sign_extend".to_string()),
                        self.span(0, 0),
                    )
                })?;
                transforms.push(Transform::SignExtend(n));
            } else if let Some(inner) =
                part.strip_prefix("zero_extend(").and_then(|s| s.strip_suffix(')'))
            {
                let n: u32 = inner.trim().parse().map_err(|_| {
                    Error::new(
                        ErrorKind::ExpectedToken("integer for zero_extend".to_string()),
                        self.span(0, 0),
                    )
                })?;
                transforms.push(Transform::ZeroExtend(n));
            } else if let Some(inner) =
                part.strip_prefix("shift_left(").and_then(|s| s.strip_suffix(')'))
            {
                let n: u32 = inner.trim().parse().map_err(|_| {
                    Error::new(
                        ErrorKind::ExpectedToken("integer for shift_left".to_string()),
                        self.span(0, 0),
                    )
                })?;
                transforms.push(Transform::ShiftLeft(n));
            } else if let Some(inner) =
                part.strip_prefix("display(").and_then(|s| s.strip_suffix(')'))
            {
                let fmt = match inner.trim() {
                    "signed_hex" => DisplayFormat::SignedHex,
                    "hex" => DisplayFormat::Hex,
                    other => {
                        return Err(Error::new(
                            ErrorKind::UnexpectedToken(format!("unknown display format: {}", other)),
                            self.span(0, part.len()),
                        ));
                    }
                };
                display_format = Some(fmt);
            } else {
                return Err(Error::new(
                    ErrorKind::UnexpectedToken(part.to_string()),
                    self.span(0, part.len()),
                ));
            }
        }
        Ok((transforms, display_format))
    }

    fn parse_instruction(&self, line: &str) -> Result<InstructionDef, Error> {
        // Example: add rd:reg[6:10] ra:reg[11:15] rb:reg[16:20] [21:30]=0100001010 rc:u1[31]
        // First token is the instruction name
        let name_end = line
            .find(|c: char| c.is_whitespace())
            .unwrap_or(line.len());
        let name = line[..name_end].to_string();
        let rest = line[name_end..].trim();

        let segments = self.parse_segments(rest, line)?;

        Ok(InstructionDef {
            name,
            segments,
            format_lines: Vec::new(),
            span: self.span(0, line.len()),
        })
    }

    fn parse_segments(&self, input: &str, _full_line: &str) -> Result<Vec<Segment>, Error> {
        let mut segments = Vec::new();
        let mut pos = 0;
        let bytes = input.as_bytes();

        while pos < input.len() {
            while pos < input.len() && input.as_bytes()[pos].is_ascii_whitespace() {
                pos += 1;
            }
            if pos >= input.len() {
                break;
            }

            if bytes[pos] == b'[' {
                let seg = self.parse_fixed_segment(input, &mut pos)?;
                segments.push(seg);
            } else {
                // Field segment: name:type[start:end]
                let seg = self.parse_field_segment(input, &mut pos)?;
                segments.push(seg);
            }
        }

        Ok(segments)
    }

    fn parse_fixed_segment(&self, input: &str, pos: &mut usize) -> Result<Segment, Error> {
        let (range, _) = self.parse_bit_range(input, pos)?;

        // Expect '='
        if *pos >= input.len() || input.as_bytes()[*pos] != b'=' {
            return Err(Error::new(
                ErrorKind::ExpectedToken("'=' after bit range for fixed segment".to_string()),
                self.span(*pos, 1),
            ));
        }
        *pos += 1;

        // Parse binary pattern (0, 1, or ?)
        let start = *pos;
        while *pos < input.len() && (input.as_bytes()[*pos] == b'0' || input.as_bytes()[*pos] == b'1' || input.as_bytes()[*pos] == b'?')
        {
            *pos += 1;
        }
        let pattern_str = &input[start..*pos];
        if pattern_str.is_empty() {
            return Err(Error::new(
                ErrorKind::InvalidBitPattern("empty pattern".to_string()),
                self.span(start, 1),
            ));
        }

        let pattern: Vec<Bit> = pattern_str
            .chars()
            .map(|c| match c {
                '0' => Bit::Zero,
                '1' => Bit::One,
                '?' => Bit::Wildcard,
                _ => unreachable!(),
            })
            .collect();

        Ok(Segment::Fixed {
            ranges: vec![range],  // Single-unit range for now
            pattern,
            span: self.span(start, pattern_str.len()),
        })
    }

    fn parse_field_segment(&self, input: &str, pos: &mut usize) -> Result<Segment, Error> {
        // name:type[range] or name:type{transforms}[range]
        let name_start = *pos;

        // Parse field name (up to ':')
        while *pos < input.len() && input.as_bytes()[*pos] != b':' {
            *pos += 1;
        }
        let name = input[name_start..*pos].to_string();

        if *pos >= input.len() || input.as_bytes()[*pos] != b':' {
            return Err(Error::new(
                ErrorKind::ExpectedToken("':' after field name".to_string()),
                self.span(*pos, 1),
            ));
        }
        *pos += 1; // skip ':'

        // Parse type (up to '[' or '{')
        let type_start = *pos;
        while *pos < input.len()
            && input.as_bytes()[*pos] != b'['
            && input.as_bytes()[*pos] != b'{'
        {
            *pos += 1;
        }
        let type_name = input[type_start..*pos].trim().to_string();

        // Check for inline transforms
        let transforms = if *pos < input.len() && input.as_bytes()[*pos] == b'{' {
            *pos += 1; // skip '{'
            let brace_start = *pos;
            while *pos < input.len() && input.as_bytes()[*pos] != b'}' {
                *pos += 1;
            }
            let transforms_str = &input[brace_start..*pos];
            if *pos < input.len() {
                *pos += 1; // skip '}'
            }
            Some(self.parse_transforms(transforms_str)?.0)
        } else {
            None
        };

        // Parse bit range
        let (range, _) = self.parse_bit_range(input, pos)?;

        let field_type = match transforms {
            Some(t) => FieldType::Inline {
                base_type: type_name,
                transforms: t,
            },
            None => {
                // Could be an alias or a builtin inline type
                if is_builtin_type(&type_name) {
                    FieldType::Inline {
                        base_type: type_name,
                        transforms: Vec::new(),
                    }
                } else {
                    FieldType::Alias(type_name)
                }
            }
        };

        Ok(Segment::Field {
            name,
            field_type,
            ranges: vec![range],  // Single-unit range for now
            span: self.span(name_start, *pos - name_start),
        })
    }

    /// Parse a `map name(params) { ... }` block.
    fn parse_map_block(&mut self) -> Result<MapDef, Error> {
        let first_line = self.lines[self.line_idx].trim();
        let block_start_line = self.line_idx;

        let rest = first_line.strip_prefix("map ").unwrap().trim();

        // Parse: name(param1, param2) {
        let paren_pos = rest.find('(').ok_or_else(|| {
            Error::new(
                ErrorKind::ExpectedToken("'(' after map name".to_string()),
                self.span(4, rest.len()),
            )
        })?;
        let name = rest[..paren_pos].trim().to_string();

        let close_paren = rest.find(')').ok_or_else(|| {
            Error::new(
                ErrorKind::ExpectedToken("')' in map definition".to_string()),
                self.span(0, rest.len()),
            )
        })?;

        let params_str = &rest[paren_pos + 1..close_paren];
        let params: Vec<String> = params_str
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect();

        // Expect `{` after params
        let after_paren = rest[close_paren + 1..].trim();
        if !after_paren.starts_with('{') {
            return Err(Error::new(
                ErrorKind::ExpectedToken("'{' after map parameters".to_string()),
                self.span(0, first_line.len()),
            ));
        }

        self.advance();

        let mut entries = Vec::new();

        while self.line_idx < self.lines.len() {
            let line = self.lines[self.line_idx].trim();

            if line == "}" {
                self.advance();
                break;
            }

            if line.is_empty() || line.starts_with('#') {
                self.advance();
                continue;
            }

            // Parse: key1, key2 => output_text
            let arrow_pos = line.find("=>").ok_or_else(|| {
                Error::new(
                    ErrorKind::ExpectedToken("'=>' in map entry".to_string()),
                    self.span(0, line.len()),
                )
            })?;

            let keys_str = &line[..arrow_pos];
            let output_raw = line[arrow_pos + 2..].trim();

            // Strip surrounding quotes from map output if present
            let output_str = extract_quoted_string(output_raw).unwrap_or(output_raw);

            let keys: Vec<MapKey> = keys_str
                .split(',')
                .map(|s| {
                    let s = s.trim();
                    if s == "_" {
                        MapKey::Wildcard
                    } else if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X"))
                    {
                        MapKey::Value(i64::from_str_radix(hex, 16).unwrap_or(0))
                    } else {
                        MapKey::Value(s.parse::<i64>().unwrap_or(0))
                    }
                })
                .collect();

            let entry_span = self.span(0, line.len());
            let output = parse_format_string(output_str, &entry_span)?;

            entries.push(MapEntry {
                keys,
                output,
                span: entry_span,
            });

            self.advance();
        }

        Ok(MapDef {
            name,
            params,
            entries,
            span: Span::new(&self.filename, block_start_line + 1, 1, 0),
        })
    }

    /// Parse format lines (`| ...`) following an instruction.
    fn parse_format_lines(&mut self) -> Vec<FormatLine> {
        let mut format_lines = Vec::new();

        while self.line_idx < self.lines.len() {
            let line = self.lines[self.line_idx];
            let trimmed = line.trim();

            if !trimmed.starts_with('|') {
                break;
            }

            let content = trimmed[1..].trim();
            let span = self.span(0, line.len());

            match self.parse_single_format_line(content, &span) {
                Ok(fl) => format_lines.push(fl),
                Err(e) => self.errors.push(e),
            }

            self.advance();
        }

        format_lines
    }

    /// Parse a single format line content (after stripping the leading `|`).
    fn parse_single_format_line(&self, content: &str, span: &Span) -> Result<FormatLine, Error> {
        // Check if there's a guard: content before first `"` that contains a `:`
        // Format: `guard_expr: "format string"` or just `"format string"`
        if let Some(quote_pos) = content.find('"') {
            let before_quote = &content[..quote_pos];
            let after_quote = &content[quote_pos..];

            // Extract the quoted format string
            let fmt_str = extract_quoted_string(after_quote).ok_or_else(|| {
                Error::new(
                    ErrorKind::InvalidFormatString("unclosed quote in format line".to_string()),
                    span.clone(),
                )
            })?;

            let guard = if before_quote.trim().is_empty() {
                None
            } else {
                // Strip trailing `:` from guard
                let guard_str = before_quote.trim().trim_end_matches(':').trim();
                Some(parse_guard(guard_str, span)?)
            };

            let pieces = parse_format_string(fmt_str, span)?;

            Ok(FormatLine {
                guard,
                pieces,
                span: span.clone(),
            })
        } else {
            Err(Error::new(
                ErrorKind::InvalidFormatString(
                    "format line must contain a quoted string".to_string(),
                ),
                span.clone(),
            ))
        }
    }

    /// Parse `[start:end]` or `[N]` bit range notation.
    fn parse_bit_range(&self, input: &str, pos: &mut usize) -> Result<(BitRange, (u32, u32)), Error> {
        if *pos >= input.len() || input.as_bytes()[*pos] != b'[' {
            return Err(Error::new(
                ErrorKind::ExpectedToken("'[' for bit range".to_string()),
                self.span(*pos, 1),
            ));
        }
        *pos += 1;

        let num1_start = *pos;
        while *pos < input.len()
            && input.as_bytes()[*pos] != b':'
            && input.as_bytes()[*pos] != b']'
        {
            *pos += 1;
        }
        let num1: u32 = input[num1_start..*pos].trim().parse().map_err(|_| {
            Error::new(
                ErrorKind::InvalidRange,
                self.span(num1_start, *pos - num1_start),
            )
        })?;

        let (dsl_start, dsl_end) = if *pos < input.len() && input.as_bytes()[*pos] == b':' {
            *pos += 1;
            let num2_start = *pos;
            while *pos < input.len() && input.as_bytes()[*pos] != b']' {
                *pos += 1;
            }
            let num2: u32 = input[num2_start..*pos].trim().parse().map_err(|_| {
                Error::new(
                    ErrorKind::InvalidRange,
                    self.span(num2_start, *pos - num2_start),
                )
            })?;
            (num1, num2)
        } else {
            (num1, num1)
        };

        if *pos < input.len() && input.as_bytes()[*pos] == b']' {
            *pos += 1;
        } else {
            return Err(Error::new(
                ErrorKind::ExpectedToken("']'".to_string()),
                self.span(*pos, 1),
            ));
        }

        // Conversion from DSL to hardware notation happens in validate() where width is known
        // Use BitRange::new() which defaults to unit 0
        Ok((BitRange::new(dsl_start, dsl_end), (dsl_start, dsl_end)))
    }
}

/// Extract contents of a quoted string (strips surrounding `"`).
fn extract_quoted_string(s: &str) -> Option<&str> {
    let s = s.trim();
    if s.starts_with('"') {
        let inner = &s[1..];
        // Find closing quote (not escaped)
        let mut i = 0;
        let chars: Vec<char> = inner.chars().collect();
        while i < chars.len() {
            if chars[i] == '\\' {
                i += 2;
            } else if chars[i] == '"' {
                let byte_pos: usize = inner[..].char_indices().nth(i).map(|(p, _)| p).unwrap_or(inner.len());
                return Some(&inner[..byte_pos]);
            } else {
                i += 1;
            }
        }
    }
    None
}

/// Parse a guard condition string like `ra == 0` or `ra == 0, lk == 1`.
fn parse_guard(s: &str, span: &Span) -> Result<Guard, Error> {
    let mut conditions = Vec::new();

    // Split on `,` or `&&`
    let parts: Vec<&str> = if s.contains("&&") {
        s.split("&&").collect()
    } else {
        s.split(',').collect()
    };

    for part in parts {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }

        let cond = parse_guard_condition(part, span)?;
        conditions.push(cond);
    }

    if conditions.is_empty() {
        return Err(Error::new(
            ErrorKind::InvalidGuard("empty guard".to_string()),
            span.clone(),
        ));
    }

    Ok(Guard { conditions })
}

fn parse_guard_condition(s: &str, span: &Span) -> Result<GuardCondition, Error> {
    // Try each operator from longest to shortest
    let ops: &[(&str, CompareOp)] = &[
        ("!=", CompareOp::Ne),
        ("<=", CompareOp::Le),
        (">=", CompareOp::Ge),
        ("==", CompareOp::Eq),
        ("<", CompareOp::Lt),
        (">", CompareOp::Gt),
    ];

    for &(op_str, ref op) in ops {
        if let Some(pos) = s.find(op_str) {
            let left = s[..pos].trim();
            let right = s[pos + op_str.len()..].trim();

            return Ok(GuardCondition {
                left: parse_guard_operand(left, span)?,
                op: op.clone(),
                right: parse_guard_operand(right, span)?,
            });
        }
    }

    Err(Error::new(
        ErrorKind::InvalidGuard(format!("no operator found in '{}'", s)),
        span.clone(),
    ))
}

fn parse_guard_operand(s: &str, span: &Span) -> Result<GuardOperand, Error> {
    let s = s.trim();
    if s.is_empty() {
        return Err(Error::new(
            ErrorKind::InvalidGuard("empty operand".to_string()),
            span.clone(),
        ));
    }

    // Try arithmetic: +, - at top level
    if let Some(op_pos) = find_guard_arith_op(s, &['+', '-']) {
        let left = s[..op_pos].trim();
        let op_char = s.as_bytes()[op_pos] as char;
        let right = s[op_pos + 1..].trim();
        let op = match op_char {
            '+' => ArithOp::Add,
            '-' => ArithOp::Sub,
            _ => unreachable!(),
        };
        return Ok(GuardOperand::Expr {
            left: Box::new(parse_guard_operand(left, span)?),
            op,
            right: Box::new(parse_guard_operand(right, span)?),
        });
    }

    // Try arithmetic: *, /, % at top level
    if let Some(op_pos) = find_guard_arith_op(s, &['*', '/', '%']) {
        let left = s[..op_pos].trim();
        let op_char = s.as_bytes()[op_pos] as char;
        let right = s[op_pos + 1..].trim();
        let op = match op_char {
            '*' => ArithOp::Mul,
            '/' => ArithOp::Div,
            '%' => ArithOp::Mod,
            _ => unreachable!(),
        };
        return Ok(GuardOperand::Expr {
            left: Box::new(parse_guard_operand(left, span)?),
            op,
            right: Box::new(parse_guard_operand(right, span)?),
        });
    }

    // Try integer literal
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        if let Ok(val) = i64::from_str_radix(hex, 16) {
            return Ok(GuardOperand::Literal(val));
        }
    }
    if let Ok(val) = s.parse::<i64>() {
        return Ok(GuardOperand::Literal(val));
    }

    // Must be a field reference
    Ok(GuardOperand::Field(s.to_string()))
}

/// Find rightmost top-level arithmetic operator in a guard operand string.
fn find_guard_arith_op(s: &str, ops: &[char]) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut last = None;
    for (i, &b) in bytes.iter().enumerate() {
        if i > 0 && ops.contains(&(b as char)) {
            last = Some(i);
        }
    }
    last
}

fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "u1" | "u2" | "u3" | "u4" | "u5" | "u6" | "u7" | "u8" | "u16" | "u32" | "i8" | "i16" | "i32" | "bool"
    )
}

/// Convert DSL bit positions to hardware (LSB=0) positions with unit support.
///
/// For variable-length instructions, bit positions beyond width-1 automatically
/// refer to subsequent units. The unit index is computed as bit_position / width.
///
/// For cross-unit ranges (e.g., `\[8:31\]` with width=16), this splits the range into
/// multiple BitRange objects, one for each unit spanned.
///
/// Returns a Vec of BitRange objects ordered by unit index.
pub fn dsl_to_hardware(dsl_start: u32, dsl_end: u32, width: u32, order: BitOrder) -> Vec<BitRange> {
    let (dsl_lo, dsl_hi) = (dsl_start.min(dsl_end), dsl_start.max(dsl_end));

    let start_unit = dsl_lo / width;
    let end_unit = dsl_hi / width;

    let mut ranges = Vec::new();

    for unit in start_unit..=end_unit {
        let unit_dsl_start = unit * width;
        let unit_dsl_end = (unit + 1) * width - 1;

        // Calculate which bits from this unit are included
        let range_start_in_unit = if unit == start_unit { dsl_lo } else { unit_dsl_start };
        let range_end_in_unit = if unit == end_unit { dsl_hi } else { unit_dsl_end };

        // Convert to local positions within the unit
        let local_start = range_start_in_unit % width;
        let local_end = range_end_in_unit % width;

        // Convert to hardware notation
        let (hw_start, hw_end) = match order {
            BitOrder::Msb0 => {
                let hw_a = width - 1 - local_start;
                let hw_b = width - 1 - local_end;
                (std::cmp::max(hw_a, hw_b), std::cmp::min(hw_a, hw_b))
            }
            BitOrder::Lsb0 => {
                (std::cmp::max(local_start, local_end), std::cmp::min(local_start, local_end))
            }
        };

        ranges.push(BitRange::new_in_unit(unit, hw_start, hw_end));
    }

    ranges
}
