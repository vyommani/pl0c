use std::collections::VecDeque;

// Manages free caller-saved and callee-saved registers
pub struct RegisterPool {
    caller_saved: VecDeque<usize>,
    callee_saved: VecDeque<usize>,
}

const NUM_REGS: usize = 32; // X0-X30, SP
const ALLOCATABLE_INDICES: &[usize] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28];

impl RegisterPool {
    /// Creates a new register pool, populating caller-saved and callee-saved registers.
    pub fn new() -> Self {
        let mut caller_saved = VecDeque::new();
        let mut callee_saved = VecDeque::new();
        for &i in ALLOCATABLE_INDICES {
            if i <= 15 {
                caller_saved.push_back(i);
            } else {
                callee_saved.push_back(i);
            }
        }
        RegisterPool { caller_saved, callee_saved }
    }

    /// Allocates a caller-saved register, returning its index if available.
    pub fn allocate_caller_saved(&mut self) -> Option<usize> {
        self.caller_saved.pop_front()
    }

    /// Allocates a callee-saved register, returning its index if available.
    pub fn allocate_callee_saved(&mut self) -> Option<usize> {
        self.callee_saved.pop_front()
    }

    /// Frees a register, returning it to the appropriate pool.
    pub fn free(&mut self, p_reg: usize) {
        if ALLOCATABLE_INDICES.contains(&p_reg) {
            if p_reg <= 15 {
                self.caller_saved.push_back(p_reg);
            } else {
                self.callee_saved.push_back(p_reg);
            }
        }
    }
}
