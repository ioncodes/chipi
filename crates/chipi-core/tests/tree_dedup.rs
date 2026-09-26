//! Mode combinations whose constraints filter to the same leaf set must share one decode tree
//! (and every tree of a build must share one opcode-table allocation). These are the size
//! regressions for the per-combination tree build.

use chipi_core::interp::decode_mode;
use chipi_core::{compile, Isa};
use std::sync::Arc;

/// Two bool modes (4 combinations), but only `a` constrains any leaf: the build must produce
/// exactly 2 distinct trees, with both `b` values of each `a` mapped to the same tree.
const TWO_MODES_ONE_LIVE: &str = r#"
decoder DD {
    width = 8
    bit_order = lsb0
    endian = little
    mode a: bool = 0
    mode b: bool = 0
}

selector op [0:7]

one_a0 a=0 op=0x10 | "one_a0"
one_a1 a=1 op=0x10 | "one_a1"
shared op=0x20 | "shared"
"#;

/// The same instruction set with only the live mode, as the non-deduped expectation: decoding
/// combo `a + 2*b` through the deduped build must agree with decoding `a` here, word for word.
const ONE_MODE_REF: &str = r#"
decoder DD {
    width = 8
    bit_order = lsb0
    endian = little
    mode a: bool = 0
}

selector op [0:7]

one_a0 a=0 op=0x10 | "one_a0"
one_a1 a=1 op=0x10 | "one_a1"
shared op=0x20 | "shared"
"#;

fn build(src: &str) -> Isa {
    compile(src).expect("spec compiles")
}

#[test]
fn identical_leaf_sets_share_a_tree() {
    let isa = build(TWO_MODES_ONE_LIVE);

    assert_eq!(isa.mode_combos(), 4);
    assert_eq!(
        isa.mode_trees.len(),
        2,
        "only `a` affects leaf presence, so 4 combos must dedupe to 2 trees"
    );

    // Combos are mixed radix over declaration order (`a` least significant), so `b` flips
    // between combo pairs (0,2) and (1,3) without changing the leaf set.
    assert_eq!(isa.combo_tree, vec![0, 1, 0, 1]);

    // The slot allocation scales with distinct trees, not combinations: 2 x 256, not 4 x 256.
    let total_slots: usize = isa.mode_trees.iter().map(|t| t.slots.len()).sum();
    assert_eq!(total_slots, 2 * 256);

    // `tree_for` resolves through the map.
    for combo in 0..4 {
        assert!(std::ptr::eq(
            isa.tree_for(combo),
            &isa.mode_trees[isa.combo_tree[combo]]
        ));
    }
}

#[test]
fn opcode_table_is_one_shared_allocation() {
    let isa = build(TWO_MODES_ONE_LIVE);

    for t in &isa.mode_trees {
        assert!(
            Arc::ptr_eq(&t.opcodes, &isa.mode_trees[0].opcodes),
            "all trees must share one opcode table"
        );
    }
    assert!(
        Arc::ptr_eq(&isa.tree.opcodes, &isa.mode_trees[0].opcodes),
        "the default-mode tree clone must share it too"
    );
}

#[test]
fn deduped_decode_matches_non_deduped_expectation() {
    let isa = build(TWO_MODES_ONE_LIVE);
    let reference = build(ONE_MODE_REF);

    // The reference build has one combination per `a` value and no dedupe opportunity, so it
    // stands in for the pre-dedupe result. Every combo and word must agree.
    for a in 0..2usize {
        for b in 0..2usize {
            let combo = a + 2 * b;
            for w in 0u64..256 {
                let got = decode_mode(&isa, combo, w);
                let want = decode_mode(&reference, a, w);
                assert_eq!(
                    got.opcode_name, want.opcode_name,
                    "combo {combo} (a={a}, b={b}) word {w:#x}"
                );
                assert_eq!(got.disasm, want.disasm, "combo {combo} word {w:#x}");
            }
        }
    }
}

/// modes_demo splits leaves on its one mode, so both combos keep distinct trees; mode_guard
/// separates leaves by guards only, so both combos share one tree while per-combination guard
/// folding still tells them apart.
#[test]
fn example_specs_dedupe_as_expected() {
    let modes_demo = build(include_str!("../../../examples/modes_demo.chipi"));
    assert_eq!(modes_demo.mode_trees.len(), 2);
    assert_eq!(modes_demo.combo_tree, vec![0, 1]);

    let mode_guard = build(include_str!("../../../examples/mode_guard.chipi"));
    assert_eq!(
        mode_guard.mode_trees.len(),
        1,
        "guard-separated combos have identical leaf sets and must share one tree"
    );
    assert_eq!(mode_guard.combo_tree, vec![0, 0]);

    // The shared tree must not blur the guard outcomes: `m` still folds per combination.
    assert_eq!(decode_mode(&mode_guard, 0, 0x10ab).opcode_name, "wide");
    assert_eq!(decode_mode(&mode_guard, 1, 0x10ab).opcode_name, "narrow");
}
