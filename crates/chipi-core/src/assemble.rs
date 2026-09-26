//! The text assembler. Built on the encoder ([`crate::inverse`]):
//!
//! * reverse a display template into a per-instruction line parser,
//! * select among candidate templates (the most specific that encodes wins),
//! * drive a two-pass assembler with labels and `.org`/`.byte`/`.word`/`.align` directives.
//!
//! `assemble_line(disasm(w)) == w` for every reversibly-displayed leaf.

use crate::model::{Disp, DispHint, Endian, FieldTy, Insn, Isa, NameDefault};
use crate::render::{FmtSpec, Seg};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsmError {
    /// No instruction template matched the line.
    NoMatch(String),
    /// A template matched but the operand values could not be encoded.
    NotEncodable(String),
}

impl std::fmt::Display for AsmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmError::NoMatch(l) => write!(f, "no instruction matches `{l}`"),
            AsmError::NotEncodable(l) => write!(f, "`{l}` matched a template but is not encodable"),
        }
    }
}

/// One assembled instruction: the decode-window `word`, the full instruction `bytes` (the window
/// followed by any `fetch(N)` operand bytes) and the matched leaf.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assembled {
    pub instr_index: usize,
    pub word: u64,
    pub bytes: Vec<u8>,
}

/// Assemble one instruction line to its decode-window word. See [`assemble_inst`] for the full byte
/// stream (which is what you want for variable-length / `fetch(N)` ISAs).
pub fn assemble_line(isa: &Isa, line: &str) -> Result<u64, AsmError> {
    assemble_inst(isa, line).map(|a| a.word)
}

/// Assemble one instruction line to its full byte stream. Among all templates that match, parse,
/// fit their operand widths and encode, the one with the most literal characters wins; ties favour
/// the earliest instruction in spec order.
pub fn assemble_inst(isa: &Isa, line: &str) -> Result<Assembled, AsmError> {
    let line = line.trim();
    let mut matched = false;
    let mut best: Option<(usize, Assembled)> = None; // (specificity, result)

    for (idx, inst) in isa.instrs.iter().enumerate() {
        for arm in &inst.display {
            // Arms without in-template conditionals match against the borrowed segments
            // directly; only conditional arms pay for `expand_conds`' cloned alternatives,
            // and the first alternative that parses wins.
            let values = if arm.segs.iter().any(|s| matches!(s, Seg::Cond { .. })) {
                expand_conds(&arm.segs)
                    .into_iter()
                    .find_map(|segs| match_template(inst, &segs, line))
            } else {
                match_template(inst, &arm.segs, line)
            };
            let Some(values) = values else {
                continue;
            };
            matched = true;

            // Every operand must fit its declared width. A `fetch(N)` operand has no in-window bits,
            // so the encoder cannot reject an over-wide value (it ignores it). Without this
            // check `lda $1234` would match the 8bit `lda_dp` rather than `lda_abs`.
            if !operands_fit(inst, &values) {
                continue;
            }
            let Ok(word) = crate::inverse::encode(isa, idx, &values) else {
                continue;
            };
            let Some(bytes) = build_bytes(isa, inst, word, &values) else {
                continue;
            };

            let specificity: usize = arm
                .segs
                .iter()
                .map(|s| match s {
                    Seg::Lit(l) => l.len(),
                    Seg::Field { .. } | Seg::Cond { .. } | Seg::SubField { .. } => 0,
                })
                .sum();

            if best.as_ref().map(|(s, _)| specificity > *s).unwrap_or(true) {
                best = Some((
                    specificity,
                    Assembled {
                        instr_index: idx,
                        word,
                        bytes,
                    },
                ));
            }
        }
    }

    match best {
        Some((_, a)) => Ok(a),
        None if matched => Err(AsmError::NotEncodable(line.to_string())),
        None => Err(AsmError::NoMatch(line.to_string())),
    }
}

/// Every supplied operand value fits its operand's declared value width. (The encoder rejects
/// misfits too; this pre-filter just keeps template selection cheap.)
fn operands_fit(inst: &Insn, values: &[(String, i128)]) -> bool {
    values.iter().all(|(n, v)| match field_ty(inst, n) {
        Some(ty) => crate::inverse::value_fits(*v, ty.value_width),
        None => true,
    })
}

/// The full byte stream of an assembled instruction: the decode-window word first, then each
/// `fetch(N)` operand value as little/big-endian bytes in declaration order (mirroring how decode
/// reads the stream past the opcode window). `None` if a fetched operand has no supplied value.
fn build_bytes(isa: &Isa, inst: &Insn, word: u64, values: &[(String, i128)]) -> Option<Vec<u8>> {
    let little = matches!(isa.decoder.endian, Endian::Little);
    let wb = isa.max_len_bytes.max(1) as usize;

    let mut bytes = Vec::with_capacity(wb);
    for i in 0..wb {
        let sh = if little { i } else { wb - 1 - i };
        bytes.push((word >> (sh * 8)) as u8);
    }

    for c in &inst.computed {
        // The text assembler works word-level: expression fetch widths resolve at the decode
        // variables' defaults, the same environment `decode(word)` uses.
        let Some(bits) = crate::interp::fetch_width_vars(&c.expr, &isa.vars_default) else {
            continue;
        };
        let v = values.iter().find(|(n, _)| n == &c.name).map(|(_, v)| *v)?;
        let nb = (bits as usize).div_ceil(8);
        let masked = (v as u128) & crate::compute::mask128(bits);
        for i in 0..nb {
            let sh = if little { i } else { nb - 1 - i };
            bytes.push((masked >> (sh * 8)) as u8);
        }
    }

    Some(bytes)
}

/// Round-trip an encoded word through disassembly and back.
pub fn roundtrip_asm(isa: &Isa, word: u64) -> Option<bool> {
    let d = crate::interp::decode(isa, word);
    if !d.is_valid() {
        return None;
    }
    let text = d.disasm?;

    let re = assemble_line(isa, &text).ok()?;
    let care = crate::inverse::care_mask(isa, d.instr_index?);
    Some((word & care) == (re & care))
}

/// Expand in-template conditionals into flat alternatives: each `{cond ? a : b}` doubles the
/// candidate list (then-branch first). Capped at 16 alternatives; templates beyond that are not
/// assembled. The parsed values need not satisfy the condition; the caller's encode step and the
/// roundtrip check are the arbiters.
fn expand_conds(segs: &[Seg]) -> Vec<Vec<Seg>> {
    let mut out: Vec<Vec<Seg>> = vec![Vec::new()];
    for seg in segs {
        match seg {
            Seg::Cond { then, els, .. } => {
                if out.len() * 2 > 16 {
                    return Vec::new();
                }
                let mut next = Vec::new();
                for prefix in &out {
                    for branch in [then, els] {
                        for expanded in expand_conds(branch) {
                            let mut v = prefix.clone();
                            v.extend(expanded);
                            next.push(v);
                        }
                    }
                }
                out = next;
            }
            other => {
                for v in &mut out {
                    v.push(other.clone());
                }
            }
        }
    }
    out
}

/// Match a template against `line`, returning `(operand, value)` pairs, or `None` if a literal fails
/// to match or a field token cannot be parsed. The whole line must be consumed.
fn match_template(inst: &Insn, segs: &[Seg], line: &str) -> Option<Vec<(String, i128)>> {
    let mut rest = line;
    let mut values = Vec::new();
    for (i, seg) in segs.iter().enumerate() {
        match seg {
            Seg::Lit(l) => rest = rest.strip_prefix(l.as_str())?,
            // Conditionals were expanded away by `expand_conds`; a subdecoder output renders
            // arbitrary text and stays unassemblable.
            Seg::Cond { .. } | Seg::SubField { .. } => return None,
            Seg::Field { name, fmt } => {
                let next_lit = match segs.get(i + 1) {
                    Some(Seg::Lit(l)) => Some(l.as_str()),
                    _ => None,
                };
                let token = match next_lit {
                    Some(lit) => {
                        let end = rest.find(lit)?;
                        let (tok, r) = rest.split_at(end);
                        rest = r;
                        tok
                    }
                    None => {
                        let tok = rest;
                        rest = "";
                        tok
                    }
                };
                let ty = field_ty(inst, name)?;
                values.push((name.clone(), parse_field_token(token, ty, fmt)?));
            }
        }
    }
    rest.is_empty().then_some(values)
}

fn field_ty<'a>(inst: &'a Insn, name: &str) -> Option<&'a FieldTy> {
    inst.fields
        .iter()
        .find(|f| f.name == name)
        .map(|f| &f.ty)
        .or_else(|| inst.computed.iter().find(|c| c.name == name).map(|c| &c.ty))
}

/// Strip the operand's display pattern (like `$r{}` or `#{}`) and read the inner number, honouring the
/// template's format spec: a `:x` (hex) field is read in base 16 even without a `0x` prefix, which is
/// how chipi renders it. `:rel`/`:sym` are modifiers whose word-level rendering falls back to the
/// numeric form, so a numeric token parses; an unresolved symbol NAME does not (loud NoMatch).
fn parse_field_token(token: &str, ty: &FieldTy, fmt: &FmtSpec) -> Option<i128> {
    let token = token.trim();

    // A `names { ... }` operand reverses by matching the token against the table, then falling back
    // to a numeric parse when the default is numeric. A `:x`/`:d` format spec overrides the table.
    if let Disp::Names(t) = &ty.disp {
        if !fmt.hex && !fmt.dec {
            if let Some((k, _)) = t.entries.iter().find(|(_, s)| s.as_str() == token) {
                return Some(*k as i128);
            }
            return match &t.default {
                NameDefault::Hint(DispHint::Hex) => parse_radix(token, 16),
                NameDefault::Hint(_) => parse_number(token),
                NameDefault::Str(_) => None,
            };
        }
    }

    let inner = match &ty.disp {
        Disp::Pattern(p) => {
            let (pre, post) = p.split_once("{}").unwrap_or((p.as_str(), ""));
            token.strip_prefix(pre)?.strip_suffix(post)?
        }
        _ => token,
    };
    if fmt.hex {
        let v = parse_radix(inner, 16)?;

        // The renderer zero-pads hex to exactly max(pad, needed) digits. Accept only that
        // form, so a 16bit render like `$0000` never matches an 8bit `02x` template (which
        // would win on spec order and re-encode to the wrong leaf).
        if fmt.zero_pad > 0 && v >= 0 {
            let digits = inner
                .trim()
                .strip_prefix("0x")
                .or_else(|| inner.trim().strip_prefix("0X"))
                .unwrap_or(inner.trim());
            let needed = format!("{:x}", v as u128).len();
            if digits.len() != needed.max(fmt.zero_pad) {
                return None;
            }
        }
        Some(v)
    } else {
        parse_number(inner)
    }
}

/// Parse an integer in `radix`, tolerating a leading sign and an optional `0x`/`0X` prefix.
fn parse_radix(s: &str, radix: u32) -> Option<i128> {
    let s = s.trim();
    let (neg, s) = match s.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, s),
    };
    let s = s
        .strip_prefix("0x")
        .or_else(|| s.strip_prefix("0X"))
        .unwrap_or(s);
    let mag = i128::from_str_radix(s, radix).ok()?;
    Some(if neg { -mag } else { mag })
}

// ---------------------------------------------------------------- two-pass driver

/// The assembled image (stream order) plus the resolved label table.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AsmImage {
    pub bytes: Vec<u8>,
    pub labels: Vec<(String, u64)>,
    /// The lowest written address (the image origin).
    pub origin: u64,
}

/// Assemble a multi-line program with labels and `.org`/`.byte`/`.word`/`.align` directives.
///
/// Each instruction is emitted as its full byte stream (window plus fetched operands), so
/// variable-length ISAs assemble correctly. Pass 1 sizes each instruction by assembling it with the
/// labels seen so far; a line that can't yet be sized (a forward reference whose encoded length
/// depends on the target's magnitude) falls back to the maximum length. Fixed-length ISAs are always
/// exact.
pub fn assemble_program(isa: &Isa, source: &str) -> Result<AsmImage, AsmError> {
    let width = isa.max_len_bytes.max(1) as u64;
    let little = matches!(isa.decoder.endian, Endian::Little);

    // pass 1: walk the source, assigning each label the address it falls on.
    let mut labels: Vec<(String, u64)> = Vec::new();
    let mut pc: u64 = 0;
    for raw in source.lines() {
        let line = strip_comment(raw);
        if line.is_empty() {
            continue;
        }
        let (label, rest) = split_label(line);
        if let Some(l) = label {
            labels.push((l.to_string(), pc));
        }
        if rest.is_empty() {
            continue;
        }
        if let Some(d) = rest.strip_prefix('.') {
            let mut sink = Vec::new();
            pc = emit_directive(d, pc, width, little, &mut sink)?;
        } else {
            let resolved = substitute_labels(rest, &labels);
            let len = assemble_inst(isa, &resolved)
                .map(|a| a.bytes.len() as u64)
                .unwrap_or(width);
            pc += len;
        }
    }

    // pass 2: emit
    let mut image: Vec<(u64, u8)> = Vec::new();
    pc = 0;
    for raw in source.lines() {
        let line = strip_comment(raw);
        if line.is_empty() {
            continue;
        }
        let (_, rest) = split_label(line);
        if rest.is_empty() {
            continue;
        }
        if let Some(d) = rest.strip_prefix('.') {
            pc = emit_directive(d, pc, width, little, &mut image)?;
            continue;
        }
        let resolved = substitute_labels(rest, &labels);
        let asm = assemble_inst(isa, &resolved)?;
        for (i, b) in asm.bytes.iter().enumerate() {
            image.push((pc + i as u64, *b));
        }
        pc += asm.bytes.len() as u64;
    }

    if image.is_empty() {
        return Ok(AsmImage::default());
    }

    // flatten the sparse (addr, byte) list into a contiguous image from the lowest address.
    let origin = image.iter().map(|(a, _)| *a).min().unwrap();
    let end = image.iter().map(|(a, _)| *a).max().unwrap();
    let mut bytes = vec![0u8; (end - origin + 1) as usize];
    for (a, b) in image {
        bytes[(a - origin) as usize] = b;
    }

    Ok(AsmImage {
        bytes,
        labels,
        origin,
    })
}

fn strip_comment(line: &str) -> &str {
    let cut = line.find(['#', ';']).unwrap_or(line.len());
    line[..cut].trim()
}

/// Split a leading `label:` off the line.
fn split_label(line: &str) -> (Option<&str>, &str) {
    if let Some(colon) = line.find(':') {
        let head = line[..colon].trim();
        if !head.is_empty()
            && head.bytes().all(|c| c.is_ascii_alphanumeric() || c == b'_')
            && head
                .bytes()
                .next()
                .map(|c| !c.is_ascii_digit())
                .unwrap_or(false)
        {
            return (Some(head), line[colon + 1..].trim());
        }
    }
    (None, line)
}

fn emit_directive(
    d: &str,
    pc: u64,
    width: u64,
    little: bool,
    image: &mut Vec<(u64, u8)>,
) -> Result<u64, AsmError> {
    let name = d.split_whitespace().next().unwrap_or("");
    let argline = d[name.len()..].trim();
    let args: Vec<&str> = argline.split(',').map(str::trim).collect();
    match name {
        "org" => {
            let v = parse_number(args.first().copied().unwrap_or(""))
                .ok_or_else(|| AsmError::NotEncodable(format!(".org {argline}")))?;
            Ok(v as u64)
        }
        "align" => {
            let a = parse_number(args.first().copied().unwrap_or(""))
                .filter(|a| *a > 0)
                .ok_or_else(|| AsmError::NotEncodable(format!(".align {argline}")))?
                as u64;
            Ok(pc.div_ceil(a) * a)
        }
        "byte" => {
            let mut cur = pc;
            for tok in &args {
                let v = parse_number(tok)
                    .ok_or_else(|| AsmError::NotEncodable(format!(".byte {argline}")))?;
                image.push((cur, v as u8));
                cur += 1;
            }
            Ok(cur)
        }
        "word" => {
            let mut cur = pc;
            for tok in &args {
                let v = parse_number(tok)
                    .ok_or_else(|| AsmError::NotEncodable(format!(".word {argline}")))?;
                push_word(image, cur, v as u64, width, little);
                cur += width;
            }
            Ok(cur)
        }
        other => Err(AsmError::NotEncodable(format!(
            "unknown directive `.{other}`"
        ))),
    }
}

fn push_word(image: &mut Vec<(u64, u8)>, addr: u64, word: u64, width: u64, little: bool) {
    for i in 0..width {
        let shift = if little { i } else { width - 1 - i };
        image.push((addr + i, (word >> (shift * 8)) as u8));
    }
}

/// Replace bare label tokens with their absolute address (decimal).
fn substitute_labels(line: &str, labels: &[(String, u64)]) -> String {
    let mut out = String::new();
    let mut tok = String::new();
    let flush = |tok: &mut String, out: &mut String| {
        match labels.iter().find(|(n, _)| n == tok) {
            Some((_, addr)) => out.push_str(&addr.to_string()),
            None => out.push_str(tok),
        }
        tok.clear();
    };
    for c in line.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            tok.push(c);
        } else {
            flush(&mut tok, &mut out);
            out.push(c);
        }
    }
    flush(&mut tok, &mut out);
    out
}

fn parse_number(s: &str) -> Option<i128> {
    let s = s.trim();
    let (neg, s) = match s.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, s),
    };
    let mag: i128 = if let Some(h) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        i128::from_str_radix(h, 16).ok()?
    } else {
        s.parse().ok()?
    };
    Some(if neg { -mag } else { mag })
}
