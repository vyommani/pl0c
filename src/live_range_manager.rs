use std::collections::HashMap;

// Manages live range information for virtual registers
pub struct LiveRangeManager {
    pub ranges: HashMap<usize, (i32, i32)>,
    pub current_instruction: i32,
}

impl LiveRangeManager {
    // Creates a new live range manager with no ranges and instruction index 0.
    pub fn new() -> Self {
        LiveRangeManager {
            ranges: HashMap::with_capacity(16),
            current_instruction: 0,
        }
    }

    // Sets the current instruction index for filtering next uses.
    pub fn set_instruction_index(&mut self, index: i32) {
        self.current_instruction = index;
    }

    // Sets the live range for a virtual register with validation.
    pub fn set_range(&mut self, v_reg: usize, start: i32, end: i32) {
        // Validate range
        if start > end {
            eprintln!("Warning: Invalid live range for vreg {}: start {} > end {}", v_reg, start, end);
            return;
        }
        self.ranges.insert(v_reg, (start, end));
    }

    // Removes the live range for a virtual register.
    pub fn remove_range(&mut self, v_reg: usize) {
        self.ranges.remove(&v_reg);
    }

    // Checks if two virtual registers have overlapping live ranges (interfere).
    pub fn interfere(&self, v_reg: usize, other_v_reg: usize) -> bool {
        if v_reg == other_v_reg {
            return false;
        }

        // If either register has no range info, assume they don't interfere
        let (start1, end1) = match self.ranges.get(&v_reg) {
            Some(&range) => range,
            None => return false,
        };

        let (start2, end2) = match self.ranges.get(&other_v_reg) {
            Some(&range) => range,
            None => return false,
        };

        // Ranges overlap if: start1 <= end2 && start2 <= end1
        start1 <= end2 && start2 <= end1
    }

    // Checks if two virtual registers can be coalesced.
    // For proper coalescing, registers should not interfere.
    // NOTE: This is a simplified version - real coalescing also requires
    // that the registers are connected by a move instruction.
    pub fn can_coalesce(&self, v_reg: usize, other_v_reg: usize) -> bool {
        if v_reg == other_v_reg {
            return false;
        }

        // Can only coalesce if both registers have range information
        if !self.ranges.contains_key(&v_reg) || !self.ranges.contains_key(&other_v_reg) {
            return false;
        }

        // Can coalesce if they don't interfere
        !self.interfere(v_reg, other_v_reg)
    }

    // Filters next uses to include only those at or after the current instruction.
    pub fn filter_next_uses(&self, next_uses: &[i32]) -> Vec<i32> {
        next_uses
            .iter()
            .filter(|&&use_pos| use_pos >= self.current_instruction)
            .copied()
            .collect()
    }

    // Checks if a virtual register is live at a specific instruction.
    pub fn is_live_at(&self, v_reg: usize, instruction: i32) -> bool {
        if let Some(&(start, end)) = self.ranges.get(&v_reg) {
            instruction >= start && instruction <= end
        } else {
            false
        }
    }

    // Checks if a virtual register is live at the current instruction.
    pub fn is_currently_live(&self, v_reg: usize) -> bool {
        self.is_live_at(v_reg, self.current_instruction)
    }

    // Gets the live range for a virtual register.
    pub fn get_range(&self, v_reg: usize) -> Option<(i32, i32)> {
        self.ranges.get(&v_reg).copied()
    }

    // Extends the live range of a virtual register to include a new instruction.
    pub fn extend_range(&mut self, v_reg: usize, instruction: i32) {
        if let Some(&mut (ref mut start, ref mut end)) = self.ranges.get_mut(&v_reg) {
            *start = (*start).min(instruction);
            *end = (*end).max(instruction);
        } else {
            // If no range exists, create a point range
            self.set_range(v_reg, instruction, instruction);
        }
    }

    // Merges the live ranges of two virtual registers.
    // Useful when coalescing registers.
    pub fn merge_ranges(&mut self, target_vreg: usize, source_vreg: usize) -> bool {
        let source_range = match self.ranges.get(&source_vreg).copied() {
            Some(range) => range,
            None => return false,
        };

        if let Some(&(target_start, target_end)) = self.ranges.get(&target_vreg) {
            // Merge the ranges
            let new_start = target_start.min(source_range.0);
            let new_end = target_end.max(source_range.1);
            self.ranges.insert(target_vreg, (new_start, new_end));
        } else {
            // Target has no range, just copy source range
            self.ranges.insert(target_vreg, source_range);
        }

        // Remove source range
        self.ranges.remove(&source_vreg);
        true
    }

    // Gets all virtual registers that are live at a specific instruction.
    pub fn get_live_at(&self, instruction: i32) -> Vec<usize> {
        self.ranges
            .iter()
            .filter_map(|(&v_reg, &(start, end))| {
                if instruction >= start && instruction <= end {
                    Some(v_reg)
                } else {
                    None
                }
            })
            .collect()
    }

    // Returns the number of virtual registers with live range information.
    pub fn range_count(&self) -> usize {
        self.ranges.len()
    }
}
