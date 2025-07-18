use std::collections::{VecDeque, HashMap, BinaryHeap};
use std::fmt::{self, Write as FmtWrite};
use std::cmp::Reverse;
use crate::code_emitter::CodeEmitter;
use crate::code_emitter::StringCodeEmitter;

use crate::{
    assembly_generator::RegisterAllocator,
    register_allocator_common::{Register, RegisterConstraints, RegisterError},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegisterName {
    X0 = 0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15,
    X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, SP,
}

impl fmt::Display for RegisterName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegisterName::SP => write!(f, "sp"),
            RegisterName::X29 => write!(f, "fp"),
            RegisterName::X30 => write!(f, "lr"),
            reg if (0..=28).contains(&reg.index()) => write!(f, "x{}", reg.index()),
            _ => write!(f, "{:?}", self),
        }
    }
}

const ALL_REGS: [RegisterName; 32] = [
    RegisterName::X0, RegisterName::X1, RegisterName::X2, RegisterName::X3,
    RegisterName::X4, RegisterName::X5, RegisterName::X6, RegisterName::X7,
    RegisterName::X8, RegisterName::X9, RegisterName::X10, RegisterName::X11,
    RegisterName::X12, RegisterName::X13, RegisterName::X14, RegisterName::X15,
    RegisterName::X16, RegisterName::X17, RegisterName::X18, RegisterName::X19,
    RegisterName::X20, RegisterName::X21, RegisterName::X22, RegisterName::X23,
    RegisterName::X24, RegisterName::X25, RegisterName::X26, RegisterName::X27,
    RegisterName::X28, RegisterName::X29, RegisterName::X30, RegisterName::SP,
];

impl RegisterName {
    fn index(&self) -> usize {
        self.clone() as usize
    }

    pub fn from_index(i: usize) -> Option<Self> {
        ALL_REGS.get(i).cloned()
    }

    pub fn get_constraints(&self) -> &'static RegisterConstraints {
        use RegisterName::*;
        match self {
            X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 => &RegisterConstraints { can_allocate: true, can_spill: true, special_purpose: "Function arguments/return value" },
            X8 => &RegisterConstraints { can_allocate: true, can_spill: true, special_purpose: "Indirect result, temporary" },
            X9 | X10 | X11 | X13 | X14 | X15 => &RegisterConstraints { can_allocate: true, can_spill: true, special_purpose: "Caller-saved temporary" },
            X12 => &RegisterConstraints { can_allocate: false, can_spill: false, special_purpose: "Reserved (e.g., for address calculation)" },
            X16 | X17 => &RegisterConstraints { can_allocate: false, can_spill: false, special_purpose: "Intra-procedure call temporary" },
            X18 => &RegisterConstraints { can_allocate: false, can_spill: false, special_purpose: "Platform register (OS-specific)" },
            X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28 => &RegisterConstraints { can_allocate: true, can_spill: true, special_purpose: "Callee-saved" },
            X29 => &RegisterConstraints { can_allocate: false, can_spill: false, special_purpose: "Frame pointer" },
            X30 => &RegisterConstraints { can_allocate: false, can_spill: false, special_purpose: "Link register" },
            SP => &RegisterConstraints { can_allocate: false, can_spill: false, special_purpose: "Stack pointer" },
        }
    }
}

const NUM_REGS: usize = 32; // X0-X30, SP
const ALLOCATABLE_INDICES: &[usize] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28];

pub struct Arm64RegisterAllocator {
    free_caller_saved: VecDeque<usize>, // X0-X15 (excluding X12)
    free_callee_saved: VecDeque<usize>, // X19-X28
    reg_map: [Option<Register<RegisterName>>; NUM_REGS],
    pub vreg_map: HashMap<String, Register<RegisterName>>,
    spill_offset: i32,
    free_spill_slots: BinaryHeap<Reverse<i32>>, // Track freed spill slots
    live_ranges: HashMap<usize, (i32, i32)>, // v_reg ID -> (start, end) instruction indices
    current_instruction: i32, // Track current instruction index
    pub used_callee_saved: std::collections::HashSet<usize>, // Track used callee-saved registers
}

impl Arm64RegisterAllocator {
    pub fn new() -> Self {
        let mut free_caller_saved = VecDeque::new();
        let mut free_callee_saved = VecDeque::new();
        let mut reg_map: [Option<Register<RegisterName>>; NUM_REGS] = Default::default();
        let vreg_map = HashMap::with_capacity(16);
        let live_ranges = HashMap::with_capacity(16);
        let free_spill_slots = BinaryHeap::new();
        let used_callee_saved = std::collections::HashSet::new();

        for i in 0..NUM_REGS {
            if let Some(name) = RegisterName::from_index(i) {
                reg_map[i] = Some(Register {
                    p_reg: i,
                    v_reg: usize::MAX,
                    name,
                    next_uses: vec![],
                    address: 0,
                    spill_offset: None,
                    live_across_call: false,
                });
                if ALLOCATABLE_INDICES.contains(&i) {
                    if i <= 15 {
                        free_caller_saved.push_back(i);
                    } else {
                        free_callee_saved.push_back(i);
                    }
                }
            }
        }

        Self {
            free_caller_saved,
            free_callee_saved,
            reg_map,
            vreg_map,
            spill_offset: 0,
            free_spill_slots,
            live_ranges,
            current_instruction: 0,
            used_callee_saved,
        }
    }

    pub fn set_instruction_index(&mut self, index: i32) {
        self.current_instruction = index;
    }

    pub fn set_live_range(&mut self, v_reg: usize, start: i32, end: i32) {
        self.live_ranges.insert(v_reg, (start, end));
    }

    fn can_coalesce(&self, v_reg: usize, other_v_reg: usize) -> bool {
        if v_reg == other_v_reg {
            return false;
        }
        let (start1, end1) = self.live_ranges.get(&v_reg).copied().unwrap_or((0, i32::MAX));
        let (start2, end2) = self.live_ranges.get(&other_v_reg).copied().unwrap_or((0, i32::MAX));
        end1 < start2 || end2 < start1
    }

    fn allocate_spill_slot(&mut self) -> i32 {
        if let Some(Reverse(slot)) = self.free_spill_slots.pop() {
            slot // Reuse freed slot
        } else {
            self.spill_offset += 8;
            if self.spill_offset % 16 != 0 {
                self.spill_offset += 8; // Ensure 16-byte alignment
            }
            self.spill_offset
        }
    }

    pub fn free(&mut self, p_reg: usize) {
        if p_reg >= NUM_REGS {
            return;
        }
        if let Some(reg) = self.reg_map[p_reg].as_mut() {
            let v_reg = reg.v_reg;
            if let Some(spill_offset) = reg.spill_offset {
                self.free_spill_slots.push(Reverse(spill_offset));
            }
            reg.v_reg = usize::MAX;
            reg.next_uses.clear();
            reg.address = 0;
            reg.spill_offset = None;
            // Return to free pool
            if ALLOCATABLE_INDICES.contains(&p_reg) {
                if p_reg <= 15 {
                    self.free_caller_saved.push_back(p_reg);
                } else {
                    self.free_callee_saved.push_back(p_reg);
                }
            }
            self.vreg_map.remove(&v_reg.to_string()); // Clear vreg_map
            self.live_ranges.remove(&v_reg);
        }
    }

    pub fn alloc(
         &mut self,
         v_reg: &Register<RegisterName>,
         vreg_name: &str,
         emitter: &mut dyn CodeEmitter,
    ) -> Result<Register<RegisterName>, RegisterError> {
        // Update next_uses based on current instruction
        let mut next_uses = v_reg.next_uses.clone();
        next_uses.retain(|&use1| use1 >= self.current_instruction);

        // Coalescing
        let coalesce_candidates: Vec<(usize, usize)> = self
            .reg_map
            .iter()
            .enumerate()
            .filter_map(|(i, r)| {
                r.as_ref().filter(|reg| {
                    reg.v_reg != usize::MAX
                        && reg.spill_offset.is_none()
                        && self.can_coalesce(v_reg.v_reg, reg.v_reg)
                }).map(|reg| (i, reg.v_reg))
            })
            .collect();

        for (p_reg, _) in coalesce_candidates {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = next_uses.clone();
                reg.address = v_reg.address;
                reg.spill_offset = None;
                self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                self.live_ranges.insert(v_reg.v_reg, self.live_ranges.get(&v_reg.v_reg).copied().unwrap_or((0, i32::MAX)));
                return Ok(reg.clone());
            }
        }

        // Use only callee-saved for vregs live across calls
        if v_reg.live_across_call {
            if !self.free_callee_saved.is_empty() {
                let p_reg = self.free_callee_saved.pop_front().unwrap();
                if let Some(reg) = self.reg_map[p_reg].as_mut() {
                    reg.v_reg = v_reg.v_reg;
                    reg.next_uses = next_uses.clone();
                    reg.address = v_reg.address;
                    reg.spill_offset = None;
                    self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                    self.live_ranges.insert(v_reg.v_reg, self.live_ranges.get(&v_reg.v_reg).copied().unwrap_or((0, i32::MAX)));
                    if p_reg >= 19 && p_reg <= 28 {
                        self.used_callee_saved.insert(p_reg); // Track usage
                    }
                    return Ok(reg.clone());
                }
            }
        } else {
            // Otherwise, use caller-saved
            if !self.free_caller_saved.is_empty() {
                let p_reg = self.free_caller_saved.pop_front().unwrap();
                if let Some(reg) = self.reg_map[p_reg].as_mut() {
                    reg.v_reg = v_reg.v_reg;
                    reg.next_uses = next_uses.clone();
                    reg.address = v_reg.address;
                    reg.spill_offset = None;
                    self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                    self.live_ranges.insert(v_reg.v_reg, self.live_ranges.get(&v_reg.v_reg).copied().unwrap_or((0, i32::MAX)));
                    return Ok(reg.clone());
                }
            }
        }

        // Callee-saved fallback (if not live_across_call but caller-saved exhausted)
        if !self.free_callee_saved.is_empty() {
            let p_reg = self.free_callee_saved.pop_front().unwrap();
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = next_uses.clone();
                reg.address = v_reg.address;
                reg.spill_offset = None;
                self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                self.live_ranges.insert(v_reg.v_reg, self.live_ranges.get(&v_reg.v_reg).copied().unwrap_or((0, i32::MAX)));
                if p_reg >= 19 && p_reg <= 28 {
                    self.used_callee_saved.insert(p_reg); // Track usage
                }
                return Ok(reg.clone());
            }
        }

        // Spill
        let mut spill_candidates = BinaryHeap::new();
        for (i, r) in self.reg_map.iter().enumerate() {
            if let Some(reg) = r.as_ref() {
                if reg.name.get_constraints().can_spill && reg.v_reg != usize::MAX {
                    let next_use = reg.next_uses.first().cloned().unwrap_or(i32::MAX);
                    spill_candidates.push(Reverse((next_use, i)));
                }
            }
        }

        let spill_idx = spill_candidates
            .pop()
            .map(|Reverse((_, i))| i)
            .ok_or(RegisterError::NoRegistersAvailable)?;

        let reg_name = self.reg_map[spill_idx]
            .as_ref()
            .map(|reg| reg.name.clone())
            .ok_or(RegisterError::NoRegistersAvailable)?;
        let spill_offset = self.allocate_spill_slot();
        emitter.emit(&format!("str {}, [sp, -{}]", reg_name, spill_offset))?;

        if let Some(reg) = self.reg_map[spill_idx].as_mut() {
            let old_v_reg = reg.v_reg;
            reg.v_reg = v_reg.v_reg;
            reg.next_uses = next_uses;
            reg.address = v_reg.address;
            reg.spill_offset = Some(spill_offset);
            self.vreg_map.insert(vreg_name.to_string(), reg.clone());
            self.live_ranges.insert(v_reg.v_reg, self.live_ranges.get(&v_reg.v_reg).copied().unwrap_or((0, i32::MAX)));
            self.live_ranges.remove(&old_v_reg);
            Ok(reg.clone())
        } else {
            Err(RegisterError::NoRegistersAvailable)
        }
    }

    // Add a method to get the used callee-saved registers
    pub fn get_used_callee_saved(&self) -> &std::collections::HashSet<usize> {
        &self.used_callee_saved
    }
}

impl RegisterAllocator for Arm64RegisterAllocator {
    fn free(&mut self, p_reg: usize) {
        self.free(p_reg);
    }

    fn alloc(&mut self, v_reg: &str, output: &mut String) -> Result<usize, RegisterError> {
        if let Some(reg) = self.vreg_map.get(v_reg).cloned() {
            let mut emitter = StringCodeEmitter::new(output);
            let allocated = self.alloc(&reg, v_reg, &mut emitter)?;
            emitter.flush()?;
            Ok(allocated.p_reg)
        } else {
            Err(RegisterError::UnknownRegister(v_reg.to_string()))
        }
    }

    fn ensure(&mut self, v_reg: &str, output: &mut String) -> Result<usize, RegisterError> {
        if let Some(reg) = self.vreg_map.get(v_reg) {
            for mapped_reg in self.reg_map.iter().flatten() {
                if mapped_reg.v_reg == reg.v_reg && mapped_reg.spill_offset.is_none() {
                    return Ok(mapped_reg.p_reg);
                }
            }
        }
        if let Some(vreg) = self.vreg_map.get(v_reg).cloned() {
            let mut emitter = StringCodeEmitter::new(output);
            let allocated = self.alloc(&vreg, v_reg, &mut emitter)?;
            if let Some(spill_offset) = vreg.spill_offset {
                emitter.emit_ldr(&allocated.name, spill_offset)?;
                if let Some(reg) = self.reg_map[allocated.p_reg].as_mut() {
                    reg.spill_offset = None;
                    reg.next_uses.retain(|&use1| use1 >= self.current_instruction);
                }
            }
            emitter.flush()?;
            Ok(allocated.p_reg)
        } else {
            Err(RegisterError::UnknownRegister(v_reg.to_string()))
        }
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn get_vreg(&mut self, vreg: &str) -> Option<&dyn std::any::Any> {
        self.vreg_map.get(vreg).map(|reg| reg as &dyn std::any::Any)
    }
}