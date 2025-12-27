use std::collections::{VecDeque, HashSet};

pub struct RegisterPool {
    caller_saved: VecDeque<usize>,
    callee_saved: VecDeque<usize>,
    allocated_caller_saved: HashSet<usize>,
    allocated_callee_saved: HashSet<usize>,
}

// ARM64-specific register classification
const CALLER_SAVED_INDICES: &[usize] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15];

const CALLEE_SAVED_INDICES: &[usize] = &[19, 20, 21, 22, 23, 24, 25, 26, 27, 28];

impl RegisterPool {
    pub fn new() -> Self {
        let caller_saved = CALLER_SAVED_INDICES.iter().copied().collect();
        let callee_saved = CALLEE_SAVED_INDICES.iter().copied().collect();
        RegisterPool {
            caller_saved,
            callee_saved,
            allocated_caller_saved: HashSet::new(),
            allocated_callee_saved: HashSet::new(),
        }
    }

    pub fn allocate_caller_saved(&mut self) -> Option<usize> {
        if let Some(reg) = self.caller_saved.pop_front() {
            self.allocated_caller_saved.insert(reg);
            Some(reg)
        } else {
            None
        }
    }

    pub fn allocate_callee_saved(&mut self) -> Option<usize> {
        if let Some(reg) = self.callee_saved.pop_front() {
            self.allocated_callee_saved.insert(reg);
            Some(reg)
        } else {
            None
        }
    }

    pub fn free(&mut self, p_reg: usize) {
        if self.allocated_caller_saved.remove(&p_reg) {
            if !self.caller_saved.contains(&p_reg) {
                self.caller_saved.push_back(p_reg);
            }
            return;
        }
        if self.allocated_callee_saved.remove(&p_reg) {
            if !self.callee_saved.contains(&p_reg) {
                self.callee_saved.push_back(p_reg);
            }
            return;
        }
    }

    pub fn is_caller_saved(p_reg: usize) -> bool {
        CALLER_SAVED_INDICES.contains(&p_reg)
    }

    pub fn is_callee_saved(p_reg: usize) -> bool {
        CALLEE_SAVED_INDICES.contains(&p_reg)
    }

    pub fn available_caller_saved(&self) -> usize {
        self.caller_saved.len()
    }

    pub fn available_callee_saved(&self) -> usize {
        self.callee_saved.len()
    }

    pub fn total_allocated(&self) -> usize {
        self.allocated_caller_saved.len() + self.allocated_callee_saved.len()
    }
}
