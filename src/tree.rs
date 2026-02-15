//! Decision tree construction for optimal instruction dispatch.
//!
//! Builds a compact decision tree that efficiently dispatches on bit patterns
//! to select the correct instruction. The tree minimizes the maximum partition
//! size and balances splits to create an efficient decoder.

use std::collections::BTreeMap;

use crate::types::*;

/// A node in the dispatch/decision tree.
#[derive(Debug, Clone)]
pub enum DecodeNode {
    /// Branch on a range of bits, with arms for each value
    Branch {
        range: BitRange,
        arms: BTreeMap<u64, DecodeNode>,
        default: Box<DecodeNode>,
    },
    /// Leaf: matched this instruction
    Leaf { instruction_index: usize },
    /// No instruction matches
    Fail,
}

/// Build an optimal dispatch tree from validated instructions.
///
/// The tree is constructed recursively, choosing bit ranges that best
/// partition the instruction candidates at each level.
pub fn build_tree(def: &ValidatedDef) -> DecodeNode {
    let candidates: Vec<usize> = (0..def.instructions.len()).collect();
    build_node(&def.instructions, &candidates, def.config.width)
}

fn build_node(
    instructions: &[ValidatedInstruction],
    candidates: &[usize],
    width: DecoderWidth,
) -> DecodeNode {
    match candidates.len() {
        0 => DecodeNode::Fail,
        1 => DecodeNode::Leaf {
            instruction_index: candidates[0],
        },
        _ => {
            // Find the best bit range to split on
            let groups = find_useful_bit_groups(instructions, candidates, width);

            if groups.is_empty() {
                // No bits can distinguish candidates.
                // Pick the most specific instruction (most fixed bits).
                let best = candidates
                    .iter()
                    .copied()
                    .max_by_key(|&idx| instructions[idx].fixed_bits().len())
                    .unwrap();
                return DecodeNode::Leaf {
                    instruction_index: best,
                };
            }

            // Try all candidate ranges and pick the best
            let mut best_range: Option<BitRange> = None;
            let mut best_score: Option<(usize, usize, u32)> = None;

            for group in &groups {
                let ranges_to_try = generate_sub_ranges(group);
                for range in ranges_to_try {
                    let partitions = partition_by_range(instructions, candidates, range);

                    // Count effective partitions (values that actually split)
                    let num_values = partitions.len();
                    if num_values <= 1 {
                        continue;
                    }

                    let max_part = partitions.values().map(|v| v.len()).max().unwrap_or(0);

                    // If this split doesn't actually reduce any partition, skip
                    if max_part >= candidates.len() {
                        continue;
                    }

                    // Score: (max_partition, inv_num_partitions, inv_width) - lower is better
                    let score = (max_part, usize::MAX - num_values, u32::MAX - range.width());

                    let is_better = match &best_score {
                        None => true,
                        Some(prev) => score < *prev,
                    };

                    if is_better {
                        best_score = Some(score);
                        best_range = Some(range);
                    }
                }
            }

            let range = match best_range {
                Some(r) => r,
                None => {
                    // Can't split further, pick most specific
                    let best = candidates
                        .iter()
                        .copied()
                        .max_by_key(|&idx| instructions[idx].fixed_bits().len())
                        .unwrap();
                    return DecodeNode::Leaf {
                        instruction_index: best,
                    };
                }
            };

            let partitions = partition_by_range(instructions, candidates, range);

            // Collect wildcards: candidates that don't have all fixed bits in range
            let wildcards: Vec<usize> = candidates
                .iter()
                .copied()
                .filter(|&idx| !has_all_fixed_at(&instructions[idx], range))
                .collect();

            let mut arms = BTreeMap::new();
            for (value, sub_candidates) in partitions {
                // Guard: if partition didn't reduce candidate count, avoid infinite recursion
                if sub_candidates.len() >= candidates.len() {
                    // No progress, pick the most specific instruction from this set
                    let best = sub_candidates
                        .iter()
                        .copied()
                        .max_by_key(|&idx| instructions[idx].fixed_bits().len())
                        .unwrap();
                    arms.insert(value, DecodeNode::Leaf { instruction_index: best });
                } else {
                    let child = build_node(instructions, &sub_candidates, width);
                    arms.insert(value, child);
                }
            }

            // Default arm for values not explicitly matched
            let default = if wildcards.is_empty() {
                Box::new(DecodeNode::Fail)
            } else {
                Box::new(build_node(instructions, &wildcards, width))
            };

            DecodeNode::Branch {
                range,
                arms,
                default,
            }
        }
    }
}

/// Check if an instruction has fixed bits at ALL positions in a range.
fn has_all_fixed_at(instr: &ValidatedInstruction, range: BitRange) -> bool {
    range.bits().all(|bit| instr.fixed_bit_at(bit).is_some())
}

/// Find contiguous bit groups useful for splitting.
/// A bit position is useful if at least 2 candidates have variation at that position.
fn find_useful_bit_groups(
    instructions: &[ValidatedInstruction],
    candidates: &[usize],
    width: DecoderWidth,
) -> Vec<BitRange> {
    let mut useful = vec![false; width.max_bits() as usize];

    for bit in 0..width.max_bits() {
        // Collect fixed values at this bit (skip candidates without fixed bits here)
        let fixed_values: Vec<Bit> = candidates
            .iter()
            .filter_map(|&idx| instructions[idx].fixed_bit_at(bit))
            .collect();

        // Need at least 2 fixed values with variation
        if fixed_values.len() >= 2 {
            let has_variation = fixed_values.iter().any(|&v| v != fixed_values[0]);
            useful[bit as usize] = has_variation;
        }
    }

    // Group contiguous useful bits into ranges
    let mut groups = Vec::new();
    let mut i = 0u32;
    while i < width.max_bits() {
        if useful[i as usize] {
            let start = i;
            while i < width.max_bits() && useful[i as usize] {
                i += 1;
            }
            let end = i - 1;
            // BitRange: start is MSB, end is LSB. Iteration goes from LSB to MSB.
            groups.push(BitRange::new(end, start));
        } else {
            i += 1;
        }
    }

    groups
}

/// Generate sub-ranges from a full range for finer-grained splitting.
fn generate_sub_ranges(range: &BitRange) -> Vec<BitRange> {
    let mut ranges = vec![*range];

    let w = range.width();
    if w > 1 {
        if w <= 10 {
            for sub_width in (1..w).rev() {
                for start_offset in 0..=(w - sub_width) {
                    let sub_start = range.start.saturating_sub(start_offset);
                    let sub_end_needed = sub_start + 1 - sub_width;
                    if sub_end_needed >= range.end && sub_start >= sub_end_needed {
                        let sub = BitRange::new(sub_start, sub_end_needed);
                        if sub != *range && !ranges.contains(&sub) {
                            ranges.push(sub);
                        }
                    }
                }
            }
        } else {
            let mid = range.end + w / 2;
            ranges.push(BitRange::new(range.start, mid));
            if mid > range.end {
                ranges.push(BitRange::new(mid - 1, range.end));
            }

            for chunk_size in [4u32, 6, 8] {
                if chunk_size < w {
                    if range.start >= chunk_size - 1 {
                        let sub = BitRange::new(range.start, range.start - chunk_size + 1);
                        if !ranges.contains(&sub) {
                            ranges.push(sub);
                        }
                    }
                    let sub = BitRange::new(range.end + chunk_size - 1, range.end);
                    if !ranges.contains(&sub) {
                        ranges.push(sub);
                    }
                }
            }
        }
    }

    ranges
}

/// Partition candidates by fixed bit values at a given range.
/// Instructions with all fixed bits at the range go in their specific bucket.
/// Instructions with any non-fixed bits (wildcards) go in every bucket.
fn partition_by_range(
    instructions: &[ValidatedInstruction],
    candidates: &[usize],
    range: BitRange,
) -> BTreeMap<u64, Vec<usize>> {
    let mut fixed_map: BTreeMap<u64, Vec<usize>> = BTreeMap::new();
    let mut wildcards: Vec<usize> = Vec::new();

    for &idx in candidates {
        if has_all_fixed_at(&instructions[idx], range) {
            let value = extract_fixed_value(&instructions[idx], range);
            fixed_map.entry(value).or_default().push(idx);
        } else {
            wildcards.push(idx);
        }
    }

    // Add wildcards to every partition
    if !wildcards.is_empty() {
        for bucket in fixed_map.values_mut() {
            bucket.extend_from_slice(&wildcards);
        }
    }

    fixed_map
}

/// Extract the fixed bit value of an instruction at a given range.
fn extract_fixed_value(instr: &ValidatedInstruction, range: BitRange) -> u64 {
    let mut value: u64 = 0;
    for bit_pos in range.bits() {
        value <<= 1;
        if let Some(Bit::One) = instr.fixed_bit_at(bit_pos) {
            value |= 1;
        }
    }
    value
}
