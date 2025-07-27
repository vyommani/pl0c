use std::collections::HashMap;

// Manages live range information for virtual registers
pub struct LiveRangeManager {
    pub ranges: HashMap<usize, (i32, i32)>,
    pub current_instruction: i32,
}

impl LiveRangeManager {
    /// Creates a new live range manager with no ranges and instruction index 0.
    pub fn new() -> Self {
        LiveRangeManager {
            ranges: HashMap::with_capacity(16),
            current_instruction: 0,
        }
    }

    /// Sets the current instruction index for filtering next uses.
    pub fn set_instruction_index(&mut self, index: i32) {
        self.current_instruction = index;
    }

    /// Sets the live range for a virtual register.
    pub fn set_range(&mut self, v_reg: usize, start: i32, end: i32) {
        self.ranges.insert(v_reg, (start, end));
    }

    /// Removes the live range for a virtual register.
    pub fn remove_range(&mut self, v_reg: usize) {
        self.ranges.remove(&v_reg);
    }

    /// Checks if two virtual registers can be coalesced based on non-overlapping live ranges.
    pub fn can_coalesce(&self, v_reg: usize, other_v_reg: usize) -> bool {
        if v_reg == other_v_reg {
            return false;
        }
        let (start1, end1) = self.ranges.get(&v_reg).copied().unwrap_or((0, i32::MAX));
        let (start2, end2) = self
            .ranges
            .get(&other_v_reg)
            .copied()
            .unwrap_or((0, i32::MAX));
        end1 < start2 || end2 < start1
    }

    /// Filters next uses to include only those at or after the current instruction.
    pub fn filter_next_uses(&self, next_uses: &[i32]) -> Vec<i32> {
        next_uses
            .iter()
            .filter(|&&use1| use1 >= self.current_instruction)
            .copied()
            .collect()
    }
}
