use std::{cmp::Reverse, collections::BinaryHeap};

// Manages spill slot allocation and freeing
pub struct SpillManager {
    offset: i32,
    free_slots: BinaryHeap<Reverse<i32>>,
}

impl SpillManager {
    /// Creates a new spill manager with no allocated slots.
    pub fn new() -> Self {
        SpillManager {
            offset: 0,
            free_slots: BinaryHeap::new(),
        }
    }

    /// Allocates a spill slot, reusing freed slots or allocating a new one.
    pub fn allocate_slot(&mut self) -> i32 {
        if let Some(Reverse(slot)) = self.free_slots.pop() {
            slot
        } else {
            self.offset += 8;
            if self.offset % 16 != 0 {
                self.offset += 8;
            }
            self.offset
        }
    }

    /// Frees a spill slot, making it available for reuse.
    pub fn free_slot(&mut self, slot: i32) {
        self.free_slots.push(Reverse(slot));
    }

    /// Returns the total spill space needed.
    pub fn total_space(&self) -> i32 {
        self.offset
    }
}
