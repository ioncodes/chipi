//! Decode-tree construction and per-node lowering.
//!
//! The tree picks a primary selector. This is the widest selector constrained by every leaf, or a
//! synthesised full-unit key for small windows. It builds a dense table over the selector's value
//! space, then resolves each slot to a leaf, an invalid slot, or a residual matcher over the
//! discriminating bits. Ambiguity (two leaves that match the same word with no priority) and table
//! holes are reported here. One tree is produced per mode combination.

use crate::lower::Resolved;
use crate::model::{BitRange, Mode, Selector};
use chipi_syntax::{Diag, Span};

/// A named contiguous key over the window.
#[derive(Clone, Debug)]
pub struct SelKey {
    pub name: String,
    pub range: BitRange,
}

/// How a table node is realised in generated code. Cosmetic in dumps, informs backends.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Lowering {
    Dense,
    Residual,
    Inline,
    Sparse,
}

impl Lowering {
    pub fn label(self) -> &'static str {
        match self {
            Lowering::Dense => "dense-table",
            Lowering::Residual => "residual-match",
            Lowering::Inline => "inline",
            Lowering::Sparse => "sparse-match",
        }
    }
}

/// A primary-table slot.
#[derive(Clone, Debug)]
pub enum Slot {
    Invalid,
    Leaf(usize),
    Residual(usize),
}

impl Slot {
    /// The dense-table entry for this slot. `base` is where residual ids start (one past the last
    /// opcode id), so a residual `ri` routes to `base + ri`.
    pub fn table_value(&self, base: usize) -> usize {
        match self {
            Slot::Invalid => 0,
            Slot::Leaf(id) => *id,
            Slot::Residual(ri) => base + ri,
        }
    }
}

/// One arm of a sparse verify chain: route to `opcode` when `(word & mask) == val`.
#[derive(Clone, Debug)]
pub struct SparseArm {
    pub mask: u64,
    pub val: u64,
    pub opcode: usize,
}

#[derive(Clone, Debug)]
pub enum Residual {
    /// Every candidate fixes one shared contiguous key with a distinct value.
    Keyed {
        key: SelKey,
        lowering: Lowering,
        /// `(key value, opcode id)`, sorted by key value.
        arms: Vec<(u64, usize)>,
        default: usize,
    },
    /// An ordered, most-specific-first mask/compare chain falling through to invalid.
    Sparse {
        lowering: Lowering,
        arms: Vec<SparseArm>,
    },
}

#[derive(Clone, Debug)]
pub struct Opcode {
    pub name: String,
    /// Index into `Isa.instrs`, or `usize::MAX` for the reserved `Invalid` slot 0.
    pub instr: usize,
}

#[derive(Clone, Debug)]
pub struct Tree {
    pub primary: SelKey,
    pub primary_lowering: Lowering,
    pub slots: Vec<Slot>,
    pub residuals: Vec<Residual>,
    pub opcodes: Vec<Opcode>,
    pub n_invalid: usize,
}

impl Tree {
    pub fn opcode_count(&self) -> usize {
        self.opcodes.len()
    }
}

/// The cross-product build output: one tree per mode combination.
pub struct Built {
    pub trees: Vec<Tree>,
    pub errors: Vec<Diag>,
    pub warnings: Vec<Diag>,
}

struct Leaf {
    id: usize,
    fixed_mask: u64,
    fixed_val: u64,
    name: String,
    span: Span,
    mode_constraints: Vec<(usize, u64)>,
}

const MAX_PRIMARY_BITS: u16 = 16;

pub fn build(r: &Resolved) -> Built {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Shared opcode ids: 0 = Invalid, then instructions sorted by name for a stable order.
    let mut order: Vec<usize> = (0..r.instrs.len()).collect();
    order.sort_by(|&a, &b| r.instrs[a].name.cmp(&r.instrs[b].name));

    let mut opcodes = vec![Opcode {
        name: "Invalid".to_string(),
        instr: usize::MAX,
    }];
    let mut id_of = vec![0usize; r.instrs.len()];
    for &idx in &order {
        id_of[idx] = opcodes.len();
        opcodes.push(Opcode {
            name: r.instrs[idx].name.clone(),
            instr: idx,
        });
    }

    let leaves: Vec<Leaf> = r
        .instrs
        .iter()
        .enumerate()
        .map(|(idx, inst)| {
            let (mask, val) = inst.fixed_mask_val();
            Leaf {
                id: id_of[idx],
                fixed_mask: mask,
                fixed_val: val,
                name: inst.name.clone(),
                span: inst.span,
                mode_constraints: inst.mode_constraints.clone(),
            }
        })
        .collect();

    let window = r.decoder.unit_bits as u16;
    let primary = match pick_primary(r, &leaves, window) {
        Ok(p) => p,
        Err(e) => {
            errors.push(e);
            return Built {
                trees: vec![empty_tree(opcodes)],
                errors,
                warnings,
            };
        }
    };

    let combos = r
        .modes
        .iter()
        .map(|m| m.cardinality)
        .product::<u64>()
        .max(1);

    if combos > 256 {
        errors.push(Diag::error(
            "ModeBudgetExceeded",
            format!(
                "mode cross-product is {combos} combinations (>256); reduce decode-affecting modes"
            ),
            r.instrs.first().map(|i| i.span).unwrap_or(Span::at(0)),
        ));
        return Built {
            trees: vec![],
            errors,
            warnings,
        };
    }

    let mut trees = Vec::new();
    for combo in 0..combos {
        let scope: Vec<&Leaf> = leaves
            .iter()
            .filter(|l| {
                l.mode_constraints
                    .iter()
                    .all(|&(mi, v)| mode_value(&r.modes, combo, mi) == v)
            })
            .collect();

        let (slots, residuals, n_invalid) =
            build_slots(r, &primary, &scope, &mut errors, &mut warnings);

        trees.push(Tree {
            primary: primary.clone(),
            primary_lowering: Lowering::Dense,
            slots,
            residuals,
            opcodes: opcodes.clone(),
            n_invalid,
        });
    }

    Built {
        trees,
        errors,
        warnings,
    }
}

fn empty_tree(opcodes: Vec<Opcode>) -> Tree {
    Tree {
        primary: SelKey {
            name: "<none>".into(),
            range: BitRange { lo: 0, hi: 0 },
        },
        primary_lowering: Lowering::Dense,
        slots: vec![Slot::Invalid],
        residuals: Vec::new(),
        opcodes,
        n_invalid: 1,
    }
}

/// Value of mode `mi` within combination `combo` (mixed radix over cardinalities).
fn mode_value(modes: &[Mode], combo: u64, mi: usize) -> u64 {
    let mut radix = 1u64;
    for m in &modes[..mi] {
        radix *= m.cardinality;
    }
    (combo / radix) % modes[mi].cardinality
}

fn build_slots(
    r: &Resolved,
    primary: &SelKey,
    leaves: &[&Leaf],
    errors: &mut Vec<Diag>,
    warnings: &mut Vec<Diag>,
) -> (Vec<Slot>, Vec<Residual>, usize) {
    let p_mask = primary.range.mask();
    let p_lo = primary.range.lo;
    let n_slots = 1usize << primary.range.width();

    let mut slots = Vec::with_capacity(n_slots);
    let mut residuals: Vec<Residual> = Vec::new();
    let mut n_invalid = 0usize;

    for v in 0..n_slots as u64 {
        let v_window = v << p_lo;
        let cands: Vec<&Leaf> = leaves
            .iter()
            .copied()
            .filter(|l| {
                let m = l.fixed_mask & p_mask;
                (l.fixed_val & m) == (v_window & m)
            })
            .collect();

        if cands.is_empty() {
            slots.push(Slot::Invalid);
            n_invalid += 1;
            continue;
        }

        let extra = cands
            .iter()
            .fold(0u64, |acc, l| acc | (l.fixed_mask & !p_mask));
        if extra == 0 {
            // The primary key fully determines the slot: pick the unique dominator.
            let dominators: Vec<&Leaf> = cands
                .iter()
                .copied()
                .filter(|win| {
                    cands
                        .iter()
                        .all(|c| (win.fixed_mask & c.fixed_mask) == c.fixed_mask)
                })
                .collect();
            if dominators.len() == 1 {
                slots.push(Slot::Leaf(dominators[0].id));
            } else {
                // No single dominator: the slot is ambiguous; report and pick most-specific.
                let (a, b) = incomparable(&cands);
                errors.push(
                    Diag::error(
                        "Ambiguous",
                        format!(
                            "`{}` and `{}` match the same encoding with no distinguishing bit",
                            a.name, b.name
                        ),
                        a.span,
                    )
                    .label(b.span, "also matches here"),
                );
                let pick = cands
                    .iter()
                    .max_by_key(|l| l.fixed_mask.count_ones())
                    .unwrap();
                slots.push(Slot::Leaf(pick.id));
            }
            continue;
        }

        match build_residual(r, &cands, p_mask) {
            Ok(table) => {
                residuals.push(table);
                slots.push(Slot::Residual(residuals.len() - 1));
            }
            Err(mut e) => {
                errors.append(&mut e);
                slots.push(Slot::Invalid);
                n_invalid += 1;
            }
        }
    }

    if n_invalid > 0 {
        warnings.push(Diag::warning(
            "IncompleteTable",
            format!(
                "primary table `{}` has {n_invalid}/{n_slots} unmapped key(s) routed to decode_invalid",
                primary.name
            ),
            r.instrs.first().map(|i| i.span).unwrap_or(Span::at(0)),
        ));
    }

    (slots, residuals, n_invalid)
}

fn build_residual(r: &Resolved, cands: &[&Leaf], p_mask: u64) -> Result<Residual, Vec<Diag>> {
    let union = cands.iter().fold(0u64, |a, l| a | (l.fixed_mask & !p_mask));

    let lo = union.trailing_zeros() as u16;
    let hi = (63 - union.leading_zeros()) as u16;
    let key_range = BitRange { lo, hi };
    let key_mask = key_range.mask();

    // Keyed fast path: every candidate fixes exactly the contiguous key.
    if cands.iter().all(|l| (l.fixed_mask & !p_mask) == key_mask) {
        let key_name = selector_named(r, key_range).unwrap_or_else(|| format!("bits[{hi}:{lo}]"));
        let mut errs = Vec::new();
        let mut arms: Vec<(u64, usize)> = Vec::new();
        for l in cands {
            let key = (l.fixed_val & key_mask) >> lo;
            if let Some(&(_, other)) = arms.iter().find(|(k, _)| *k == key) {
                let other_name = cands
                    .iter()
                    .find(|c| c.id == other)
                    .map(|c| c.name.as_str())
                    .unwrap_or("?");
                errs.push(Diag::error(
                    "Ambiguous",
                    format!(
                        "`{}` collides with `{}` on residual key {key_name} = {key:#x}",
                        l.name, other_name
                    ),
                    l.span,
                ));
            } else {
                arms.push((key, l.id));
            }
        }
        arms.sort_by_key(|(k, _)| *k);

        if !errs.is_empty() {
            return Err(errs);
        }

        let lowering = residual_lowering(arms.len(), key_range.width());
        return Ok(Residual::Keyed {
            key: SelKey {
                name: key_name,
                range: key_range,
            },
            lowering,
            arms,
            default: 0,
        });
    }

    build_sparse(cands, p_mask)
}

fn build_sparse(cands: &[&Leaf], p_mask: u64) -> Result<Residual, Vec<Diag>> {
    let mut errs = Vec::new();

    // detect any pair that can match the same word with no strict-superset relationship.
    for (i, a) in cands.iter().enumerate() {
        let am = a.fixed_mask & !p_mask;
        for b in &cands[i + 1..] {
            let bm = b.fixed_mask & !p_mask;
            let shared = am & bm;
            // can they match a common word? (no fixed bit disagrees on the shared region)
            if (shared & (a.fixed_val ^ b.fixed_val)) != 0 {
                continue;
            }
            let a_super = (am & bm) == bm;
            let b_super = (am & bm) == am;
            let strict = (a_super && am != bm) || (b_super && am != bm);
            if !strict {
                errs.push(
                    Diag::error(
                        "Ambiguous",
                        format!(
                            "`{}` and `{}` can match the same encoding with no distinguishing bit",
                            a.name, b.name
                        ),
                        a.span,
                    )
                    .label(b.span, "also matches here"),
                );
            }
        }
    }

    // most fixed bits first; tie-break by name for a stable order
    let mut ordered: Vec<&&Leaf> = cands.iter().collect();
    ordered.sort_by(|a, b| {
        (b.fixed_mask & !p_mask)
            .count_ones()
            .cmp(&(a.fixed_mask & !p_mask).count_ones())
            .then(a.name.cmp(&b.name))
    });
    let arms = ordered
        .iter()
        .map(|l| SparseArm {
            mask: l.fixed_mask & !p_mask,
            val: l.fixed_val & !p_mask,
            opcode: l.id,
        })
        .collect();

    if errs.is_empty() {
        Ok(Residual::Sparse {
            lowering: Lowering::Sparse,
            arms,
        })
    } else {
        Err(errs)
    }
}

/// Two candidates with no strict-superset relationship (the pair to blame for an ambiguous slot).
fn incomparable<'a>(cands: &[&'a Leaf]) -> (&'a Leaf, &'a Leaf) {
    for (i, a) in cands.iter().enumerate() {
        for b in &cands[i + 1..] {
            let (am, bm) = (a.fixed_mask, b.fixed_mask);
            let a_super = (am & bm) == bm && am != bm;
            let b_super = (am & bm) == am && am != bm;
            if !a_super && !b_super {
                return (a, b);
            }
        }
    }
    (cands[0], cands[cands.len() - 1])
}

fn residual_lowering(live: usize, key_width: u16) -> Lowering {
    let n_keys = 1u64 << key_width.min(20);
    if live <= 4 {
        Lowering::Inline
    } else if key_width <= 12 && (live as f64) / (n_keys as f64) >= 0.40 {
        Lowering::Dense
    } else {
        Lowering::Residual
    }
}

fn selector_named(r: &Resolved, range: BitRange) -> Option<String> {
    r.selectors
        .iter()
        .find(|s| s.range == range)
        .map(|s| s.name.clone())
}

fn pick_primary(r: &Resolved, leaves: &[Leaf], window: u16) -> Result<SelKey, Diag> {
    // declared selectors constrained by *every* leaf
    let qualifying: Vec<&Selector> = r
        .selectors
        .iter()
        .filter(|s| {
            let m = s.range.mask();
            leaves.iter().all(|l| (l.fixed_mask & m) != 0)
        })
        .collect();

    // among those, the ones narrow enough for a dense primary table.
    let mut narrow: Vec<&&Selector> = qualifying
        .iter()
        .filter(|s| s.range.width() <= MAX_PRIMARY_BITS)
        .collect();

    if !narrow.is_empty() {
        // widest, then most-significant (higher lo), then name
        narrow.sort_by(|a, b| {
            b.range
                .width()
                .cmp(&a.range.width())
                .then(b.range.lo.cmp(&a.range.lo))
                .then(a.name.cmp(&b.name))
        });
        let s = narrow[0];
        return Ok(SelKey {
            name: s.name.clone(),
            range: s.range,
        });
    }

    if let Some(wide) = qualifying.first() {
        return Err(Diag::error(
            "NoPrimarySelector",
            format!(
                "the only selector(s) constrained by every instruction are wider than \
                 {MAX_PRIMARY_BITS} bits (e.g. `{}` is {} bits); a dense primary table would be too \
                 large. Declare a narrower discriminating selector.",
                wide.name,
                wide.range.width()
            ),
            wide.span,
        ));
    }

    if window <= 12 {
        Ok(SelKey {
            name: "word".to_string(),
            range: BitRange {
                lo: 0,
                hi: window - 1,
            },
        })
    } else {
        Err(Diag::error(
            "NoPrimarySelector",
            "cannot infer a primary selector: no declared selector is constrained by every \
             instruction, and the window is too wide for a full-unit key. Declare a `selector` that \
             every instruction constrains.",
            r.instrs.first().map(|i| i.span).unwrap_or(Span::at(0)),
        ))
    }
}
