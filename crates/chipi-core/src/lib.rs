//! `chipi-core` resolves the AST to IR, runs validation passes, builds and
//! lowers the decode tree and exposes the reference evaluator, the encoder, the text assembler and
//! the backend-neutral program model.
//!
//! Depends only on `chipi-syntax`; free of `unsafe`.

#![forbid(unsafe_code)]

pub mod accessor;
pub mod assemble;
pub mod check;
pub mod compute;
pub mod interp;
pub mod inverse;
pub mod lower;
pub mod model;
pub mod render;
pub mod tree;

pub use chipi_syntax::{Diag, Source};
pub use model::Isa;

/// Compile spec source all the way to a resolved, validated, lowered [`Isa`].
///
/// On success the program's `warnings` field carries non-fatal diagnostics. On failure every error
/// diagnostic collected across the pipeline is returned.
pub fn compile(src: &str) -> Result<Isa, Vec<Diag>> {
    let spec = chipi_syntax::parse(src).map_err(|d| vec![d])?;
    let resolved = lower::resolve(&spec)?;

    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    let (verr, vwarn) = check::validate(&resolved);
    errors.extend(verr);
    warnings.extend(vwarn);

    let built = tree::build(&resolved);
    errors.extend(built.errors);
    warnings.extend(built.warnings);

    if !errors.is_empty() {
        return Err(errors);
    }

    // The handle must hold the widest window: the fetch unit, or the widest `length` arm.
    let max_bits = resolved
        .length
        .as_ref()
        .map(|l| l.max_bits())
        .unwrap_or(0)
        .max(resolved.decoder.unit_bits as u16);
    let max_len_bytes = max_bits.div_ceil(8) as u8;

    // default-mode tree index (mixed radix over mode defaults)
    let default_combo = model::default_combo(&resolved.modes) as usize;

    let mode_trees = built.trees;
    let tree = mode_trees[default_combo.min(mode_trees.len() - 1)].clone();

    Ok(Isa {
        decoder: resolved.decoder,
        selectors: resolved.selectors,
        types: resolved.types,
        forms: resolved.forms,
        fns: resolved.fns,
        modes: resolved.modes,
        instrs: resolved.instrs,
        tags: resolved.tags,
        groups: resolved.groups,
        subdecoders: resolved.subdecoders,
        length: resolved.length,
        prefix: resolved.prefix,
        tree,
        mode_trees,
        max_len_bytes,
        warnings,
    })
}

/// Render diagnostics against a named source into one string.
pub fn render_diagnostics(diags: &[Diag], src: &Source) -> String {
    diags
        .iter()
        .map(|d| d.render(src))
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interp::{self, Decoded};
    use crate::tree::Residual;

    const MIPS: &str = include_str!("../../../examples/mips.chipi");

    fn isa() -> Isa {
        match compile(MIPS) {
            Ok(p) => p,
            Err(errs) => {
                let src = Source::new("mips.chipi", MIPS);
                panic!("compile failed:\n{}", render_diagnostics(&errs, &src));
            }
        }
    }

    #[test]
    fn mips_shape() {
        let p = isa();
        assert_eq!(p.decoder.unit_bits, 32);
        assert_eq!(p.max_len_bytes, 4);
        assert_eq!(p.handle_ty(), "u32");
        assert_eq!(p.instrs.len(), 11);
        assert_eq!(p.tree.primary.name, "op");
        assert_eq!(p.tree.opcode_count(), 12); // 11 leaves + Invalid
        assert_eq!(p.tree.residuals.len(), 1);
        match &p.tree.residuals[0] {
            Residual::Keyed { key, arms, .. } => {
                assert_eq!(key.name, "funct");
                assert_eq!(arms.len(), 5);
            }
            Residual::Sparse { .. } => panic!("MIPS funct should be a keyed residual"),
        }
    }

    fn dec(p: &Isa, word: u64) -> Decoded {
        interp::decode(p, word)
    }

    #[test]
    fn mips_decode_vectors() {
        let p = isa();
        let field = |d: &Decoded, n: &str| d.fields.iter().find(|f| f.name == n).unwrap().value;

        let d = dec(&p, 0x0085_1020);
        assert_eq!(d.opcode_name, "add");
        assert_eq!(d.len_bytes, 4);
        assert_eq!(field(&d, "rd"), 2);
        assert_eq!(field(&d, "rs"), 4);
        assert_eq!(field(&d, "rt"), 5);
        assert_eq!(d.disasm.as_deref(), Some("add $r2, $r4, $r5"));

        let d = dec(&p, 0x2482_0010);
        assert_eq!(d.opcode_name, "addiu");
        assert_eq!(field(&d, "imm"), 0x10);

        let d = dec(&p, 0x8FA2_0004);
        assert_eq!(d.opcode_name, "lw");
        assert_eq!(field(&d, "rs"), 29);
        assert_eq!(field(&d, "off"), 4);

        let d = dec(&p, 0x0800_0100);
        assert_eq!(d.opcode_name, "j");
        let t = d.fields.iter().find(|f| f.name == "target").unwrap();
        assert_eq!(t.raw, 0x100);
        assert_eq!(t.value, 0x400);

        let d = dec(&p, 0x0000_0000);
        assert_eq!(d.opcode_name, "sll");

        let d = dec(&p, 0xFFFF_FFFF);
        assert!(!d.is_valid());
        assert_eq!(d.opcode_name, "Invalid");
    }

    #[test]
    fn coverage_warns_but_compiles() {
        let p = isa();
        assert!(p
            .warnings
            .iter()
            .any(|w| w.code == "IncompleteCoverage" && w.message.contains("add")));
    }

    #[test]
    fn mips_roundtrips() {
        let p = isa();
        for w in [
            0x0085_1020u64,
            0x2482_0010,
            0x8FA2_0004,
            0x0800_0100,
            0x0000_0000,
        ] {
            assert_eq!(inverse::roundtrip(&p, w), Some(true), "word {w:#x}");
        }
    }
}
