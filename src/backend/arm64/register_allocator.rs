use crate::ir::code_emitter::CodeEmitter;
use crate::ir::code_emitter::StringCodeEmitter;
use std::collections::{HashMap, HashSet};
use std::fmt::{self};

use crate::backend::common::live_range_manager::LiveRangeManager;
use crate::backend::common::register_pool::RegisterPool;
use crate::backend::common::spill_manager::SpillManager;
use crate::{
    backend::assembly_generator::RegisterAllocator,
    backend::common::register_allocator_common::{Register, RegisterConstraints},
};
use crate::utils::errors::Pl0Result;
use crate::utils::errors::Pl0Error;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegisterName {
    X0 = 0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15,
    X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, SP,None,
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

const ALL_REGS: [RegisterName; 33] = [ RegisterName::X0, RegisterName::X1, RegisterName::X2, RegisterName::X3, RegisterName::X4,
    RegisterName::X5, RegisterName::X6, RegisterName::X7, RegisterName::X8, RegisterName::X9, RegisterName::X10,RegisterName::X11,
    RegisterName::X12, RegisterName::X13, RegisterName::X14, RegisterName::X15, RegisterName::X16, RegisterName::X17, RegisterName::X18,
    RegisterName::X19, RegisterName::X20, RegisterName::X21, RegisterName::X22, RegisterName::X23, RegisterName::X24,RegisterName::X25,
    RegisterName::X26, RegisterName::X27, RegisterName::X28, RegisterName::X29, RegisterName::X30, RegisterName::SP,RegisterName::None,
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
            X9 | X10 | X13 | X14 | X15 => &RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Caller-saved temporary",
            },
            X11 => &RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Reserved for temporary computations (emit_mod)",
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
            None => &RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Invalid register",
            },
        }
    }
}

const NUM_REGS: usize = 32; // X0-X30, SP

pub struct Arm64RegisterAllocator {
    pool: RegisterPool,
    reg_map: [Option<Register<RegisterName>>; NUM_REGS],
    pub vreg_map: HashMap<String, Register<RegisterName>>,
    preg_vreg_map: HashMap<usize, String>,
    spill_manager: SpillManager,
    live_range_manager: LiveRangeManager,
    pub used_callee_saved: HashSet<usize>, // Track used callee-saved registers
    // Track which registers were allocated from the pool vs pre-existing
    allocated_from_pool: HashSet<usize>,
}

impl Arm64RegisterAllocator {
    pub fn new() -> Self {
        let pool = RegisterPool::new();
        let mut reg_map: [Option<Register<RegisterName>>; NUM_REGS] = Default::default();
        let vreg_map = HashMap::with_capacity(16);
        let preg_vreg_map = HashMap::with_capacity(16);
        let live_range_manager = LiveRangeManager::new();
        let spill_manager = SpillManager::new();
        let used_callee_saved = HashSet::new();
        let allocated_from_pool = HashSet::new();

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
            preg_vreg_map,
            spill_manager,
            live_range_manager,
            used_callee_saved,
            allocated_from_pool,
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
            // Free spill slot if allocated
            if let Some(spill_offset) = reg.spill_offset {
                self.spill_manager.free_slot(spill_offset);
            }
            // Reset register state
            reg.v_reg = usize::MAX;
            reg.next_uses.clear();
            reg.address = 0;
            reg.spill_offset = None;
            // Only free from pool if it was allocated from pool
            if self.allocated_from_pool.contains(&p_reg) {
                self.pool.free(p_reg);
                self.allocated_from_pool.remove(&p_reg);
            }
            if let Some(vreg_name) = self.preg_vreg_map.get(&p_reg){
                self.vreg_map.remove(vreg_name);
            }
            self.preg_vreg_map.remove(&p_reg);
        }
    }

    pub fn alloc(&mut self, v_reg: &Register<RegisterName>, vreg_name: &str, emitter: &mut dyn CodeEmitter) -> Pl0Result<Register<RegisterName>> {
        let next_uses = self.live_range_manager.filter_next_uses(&v_reg.next_uses);
        // Try caller-saved registers first
        if let Some(p_reg) = self.pool.allocate_caller_saved() {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = next_uses;
                reg.address = v_reg.address;
                reg.spill_offset = None;
                self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                self.preg_vreg_map.insert(p_reg, vreg_name.to_string());
                self.allocated_from_pool.insert(p_reg);
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
                self.preg_vreg_map.insert(p_reg, vreg_name.to_string());
                self.allocated_from_pool.insert(p_reg);
                if p_reg >= 19 && p_reg <= 28 {
                    self.used_callee_saved.insert(p_reg);
                }
                return Ok(reg.clone());
            }
        }

        // Spill: Select register with furthest next use
        let (spill_idx, _spilled) = self.reg_map.iter().enumerate()
            .filter_map(|(i, r)| r.as_ref().map(|reg| (i, reg)))
            .filter(|(_, reg)| reg.name.get_constraints().can_spill && reg.v_reg != usize::MAX)
            .max_by_key(|(_, reg)| reg.next_uses.first().copied().unwrap_or(i32::MAX))
            .ok_or(Pl0Error::NoRegistersAvailable)?;

        // Find vreg name for spilled register
        let spilled_vreg = self.vreg_map.iter()
            .find_map(|(name, reg)| (reg.p_reg == spill_idx).then(|| name.clone()))
            .ok_or_else(|| Pl0Error::RegisterAllocationError {
                message: format!("Spilled register {} has no vreg mapping", spill_idx)
            })?;

        // Allocate spill slot and emit spill code
        let spill_offset = self.spill_manager.allocate_slot();
        let spill_reg_name = RegisterName::from_index(spill_idx).ok_or_else(|| Pl0Error::RegisterAllocationError {message: format!("Invalid register index: {}", spill_idx)})?;
        emitter.emit(&format!("str {}, [sp, -{}]", spill_reg_name, spill_offset))?;

        // Update spilled register's state in vreg_map
        if let Some(spilled_reg) = self.vreg_map.get_mut(&spilled_vreg) {
            spilled_reg.spill_offset = Some(spill_offset);
        }

        // Allocate the register to new vreg
        if let Some(reg) = self.reg_map[spill_idx].as_mut() {
            reg.v_reg = v_reg.v_reg;
            reg.next_uses = next_uses;
            reg.address = v_reg.address;
            reg.spill_offset = None;
            let new_reg = reg.clone();
            self.vreg_map.insert(vreg_name.to_string(), new_reg.clone());
            self.preg_vreg_map.insert(spill_idx, vreg_name.to_string());
            if (19..=28).contains(&spill_idx) {
                self.used_callee_saved.insert(spill_idx);
            }
            Ok(new_reg)
        } else {
            Err(Pl0Error::NoRegistersAvailable)
        }
    }

    pub fn get_used_callee_saved(&self) -> &HashSet<usize> {
        &self.used_callee_saved
    }

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
        // First, check if register is already allocated and not spilled
        if let Some(reg) = self.vreg_map.get(v_reg) {
            // If register is allocated and not spilled, return it
            if reg.spill_offset.is_none() {
                return Ok(reg.p_reg);
            }
            // If register is spilled, we need to reload it
            if let Some(spill_offset) = reg.spill_offset {
                // Try to allocate a new physical register
                let vreg_copy = reg.clone();
                let mut emitter = StringCodeEmitter::new(output);
                let allocated = self.alloc(&vreg_copy, v_reg, &mut emitter)?;
                // Emit load from spill location
                emitter.emit(&format!("ldr {}, [sp, -{}]", allocated.name, spill_offset))?;
                // Update register state - no longer spilled
                if let Some(updated_reg) = self.vreg_map.get_mut(v_reg) {
                    updated_reg.spill_offset = None;
                    updated_reg.p_reg = allocated.p_reg;
                }
                self.preg_vreg_map.insert(allocated.p_reg, v_reg.to_string());
                emitter.flush()?;
                return Ok(allocated.p_reg);
            }
        }
        // If register doesn't exist in vreg_map, it's an error
        Err(Pl0Error::RegisterAllocationError { message: format!("Unknown register: {}", v_reg) })
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn get_vreg(&mut self, vreg: &str) -> Option<&dyn std::any::Any> {
        self.vreg_map.get(vreg).map(|reg| reg as &dyn std::any::Any)
    }
}
