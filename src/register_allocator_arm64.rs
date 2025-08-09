use crate::code_emitter::CodeEmitter;
use crate::code_emitter::StringCodeEmitter;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::fmt::{self, Write as FmtWrite};

use crate::live_range_manager::LiveRangeManager;
use crate::register_pool::RegisterPool;
use crate::spill_manager::SpillManager;
use crate::{
    assembly_generator::RegisterAllocator,
    register_allocator_common::{Register, RegisterConstraints},
};
use crate::errors::Pl0Result;
use crate::errors::Pl0Error;

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

const ALL_REGS: [RegisterName; 32] = [ RegisterName::X0, RegisterName::X1, RegisterName::X2, RegisterName::X3, RegisterName::X4,
    RegisterName::X5, RegisterName::X6, RegisterName::X7, RegisterName::X8, RegisterName::X9, RegisterName::X10,RegisterName::X11,
    RegisterName::X12, RegisterName::X13, RegisterName::X14, RegisterName::X15, RegisterName::X16, RegisterName::X17, RegisterName::X18,
    RegisterName::X19, RegisterName::X20, RegisterName::X21, RegisterName::X22, RegisterName::X23, RegisterName::X24,RegisterName::X25,
    RegisterName::X26, RegisterName::X27, RegisterName::X28, RegisterName::X29, RegisterName::X30, RegisterName::SP,
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
            X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 => &RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Function arguments/return value",
            },
            X8 => &RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Indirect result, temporary",
            },
            X9 | X10 | X11 | X13 | X14 | X15 => &RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Caller-saved temporary",
            },
            X12 => &RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Reserved (e.g., for address calculation)",
            },
            X16 | X17 => &RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Intra-procedure call temporary",
            },
            X18 => &RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Platform register (OS-specific)",
            },
            X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28 => &RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Callee-saved",
            },
            X29 => &RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Frame pointer",
            },
            X30 => &RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Link register",
            },
            SP => &RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Stack pointer",
            },
        }
    }
}

const NUM_REGS: usize = 32; // X0-X30, SP
const ALLOCATABLE_INDICES: &[usize] = &[
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
];

pub struct Arm64RegisterAllocator {
    pool: RegisterPool,
    reg_map: [Option<Register<RegisterName>>; NUM_REGS],
    pub vreg_map: HashMap<String, Register<RegisterName>>,
    spill_manager: SpillManager,
    live_range_manager: LiveRangeManager,
    current_instruction: i32, // Track current instruction index
    pub used_callee_saved: std::collections::HashSet<usize>, // Track used callee-saved registers
}

impl Arm64RegisterAllocator {
    pub fn new() -> Self {
        let pool = RegisterPool::new();
        let mut reg_map: [Option<Register<RegisterName>>; NUM_REGS] = Default::default();
        let vreg_map = HashMap::with_capacity(16);
        let live_range_manager = LiveRangeManager::new();
        let spill_manager = SpillManager::new();
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
            }
        }

        Self {
            pool,
            reg_map,
            vreg_map,
            spill_manager,
            live_range_manager,
            current_instruction: 0,
            used_callee_saved,
        }
    }

    pub fn set_instruction_index(&mut self, index: i32) {
        self.live_range_manager.set_instruction_index(index);
    }

    pub fn set_live_range(&mut self, v_reg: usize, start: i32, end: i32) {
        self.live_range_manager.set_range(v_reg, start, end);
    }

    fn can_coalesce(&self, v_reg: usize, other_v_reg: usize) -> bool {
        self.live_range_manager.can_coalesce(v_reg, other_v_reg)
    }

    pub fn free(&mut self, p_reg: usize) {
        if p_reg >= NUM_REGS {
            return;
        }
        if let Some(reg) = self.reg_map[p_reg].as_mut() {
            let v_reg = reg.v_reg;
            if let Some(spill_offset) = reg.spill_offset {
                self.spill_manager.free_slot(spill_offset);
            }
            reg.v_reg = usize::MAX;
            reg.next_uses.clear();
            reg.address = 0;
            reg.spill_offset = None;
            self.pool.free(p_reg);
            self.vreg_map.remove(&v_reg.to_string()); // Clear vreg_map
            self.live_range_manager.remove_range(v_reg);
        }
    }

    pub fn alloc(&mut self, v_reg: &Register<RegisterName>, vreg_name: &str, emitter: &mut dyn CodeEmitter) -> Pl0Result<Register<RegisterName>> {
        // Update next_uses based on current instruction
        let next_uses: Vec<i32> = v_reg.next_uses.iter()
        .copied()
        .filter(|&use_pos| use_pos >= self.current_instruction)
        .collect();

        // Try caller-saved registers first
        if let Some(p_reg) = self.pool.allocate_caller_saved() {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = next_uses;
                reg.address = v_reg.address;
                reg.spill_offset = None;
                self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                return Ok(reg.clone());
            }
        }

        // Callee-saved fallback
        if let Some(p_reg) = self.pool.allocate_callee_saved() {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = next_uses;
                reg.address = v_reg.address;
                reg.spill_offset = None;
                self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                if p_reg >= 19 && p_reg <= 28 {
                    self.used_callee_saved.insert(p_reg);
                }
                return Ok(reg.clone());
            }
        }

        // Spill: Select register with furthest next use
        let (spill_idx, spilled) = self.reg_map.iter().enumerate()
            .filter_map(|(i, r)| r.as_ref().map(|reg| (i, reg)))
            .filter(|(_, reg)| reg.name.get_constraints().can_spill && reg.v_reg != usize::MAX)
            .max_by_key(|(_, reg)| reg.next_uses.first().copied().unwrap_or(i32::MAX))
            .ok_or(Pl0Error::NoRegistersAvailable)?;

        // Find vreg name for spilled register
        let spilled_vreg = self.vreg_map.iter().find_map(|(name, reg)| (reg.v_reg == spilled.v_reg).then(|| name.clone()))
            .ok_or_else(|| Pl0Error::RegisterAllocationError {message: format!("Spilled register {} has no vreg mapping", spill_idx)})?;

        // Emit spill code
        let spill_offset = self.spill_manager.allocate_slot();
        let spill_reg_name = RegisterName::from_index(spill_idx)
            .ok_or_else(|| Pl0Error::RegisterAllocationError {message: format!("Invalid register index: {}", spill_idx)})?;
        emitter.emit(&format!("str {}, [sp, -{}]", spill_reg_name, spill_offset))?;

        // Update vreg_map for spilled register
        if let Some(spilled_reg) = self.reg_map[spill_idx].as_mut() {
            spilled_reg.spill_offset = Some(spill_offset);
            self.vreg_map.insert(spilled_vreg.clone(), spilled_reg.clone());
        }

        // Update reg_map for new allocation
        if let Some(reg) = self.reg_map[spill_idx].as_mut() {
            reg.v_reg = v_reg.v_reg;
            reg.next_uses = next_uses;
            reg.address = v_reg.address;
            reg.spill_offset = Some(spill_offset);
            self.vreg_map.insert(vreg_name.to_string(), reg.clone());
            if (19..=28).contains(&spill_idx) {
                self.used_callee_saved.insert(spill_idx);
            }
            Ok(reg.clone())
        } else {
            Err(Pl0Error::NoRegistersAvailable)
        }
    }

    // Add a method to get the used callee-saved registers
    pub fn get_used_callee_saved(&self) -> &std::collections::HashSet<usize> {
        &self.used_callee_saved
    }

    // Add a method to get the total spill space needed
    pub fn get_spill_space_needed(&self) -> i32 {
        self.spill_manager.total_space()
    }
}

impl RegisterAllocator for Arm64RegisterAllocator {
    fn free(&mut self, p_reg: usize) {
        self.free(p_reg);
    }

    fn alloc(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize> {
        if let Some(reg) = self.vreg_map.get(v_reg).cloned() {
            let mut emitter = StringCodeEmitter::new(output);
            let allocated = self.alloc(&reg, v_reg, &mut emitter)?;
            emitter.flush()?;
            Ok(allocated.p_reg)
        } else {
            Err(Pl0Error::RegisterAllocationError { message: format!("Unknown register: {}", v_reg) })
        }
    }

    fn ensure(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize> {
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
                    reg.next_uses
                        .retain(|&use1| use1 >= self.current_instruction);
                }
            }
            emitter.flush()?;
            Ok(allocated.p_reg)
        } else {
            Err(Pl0Error::RegisterAllocationError { message: format!("Unknown register: {}", v_reg) })
        }
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn get_vreg(&mut self, vreg: &str) -> Option<&dyn std::any::Any> {
        self.vreg_map.get(vreg).map(|reg| reg as &dyn std::any::Any)
    }
}
