use std::collections::{BinaryHeap, HashMap};
use std::fmt::{self, Write as FmtWrite};
use std::cmp::Reverse;

use crate::{
    assembly_generator::RegisterAllocator,
    register_allocator_common::{Register, RegisterConstraints, RegisterError},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegisterName {
    X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15,
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

impl RegisterName {
    fn index(&self) -> usize {
        match self {
            RegisterName::X0 => 0, RegisterName::X1 => 1, RegisterName::X2 => 2, RegisterName::X3 => 3,
            RegisterName::X4 => 4, RegisterName::X5 => 5, RegisterName::X6 => 6, RegisterName::X7 => 7,
            RegisterName::X8 => 8, RegisterName::X9 => 9, RegisterName::X10 => 10, RegisterName::X11 => 11,
            RegisterName::X12 => 12, RegisterName::X13 => 13, RegisterName::X14 => 14, RegisterName::X15 => 15,
            RegisterName::X16 => 16, RegisterName::X17 => 17, RegisterName::X18 => 18, RegisterName::X19 => 19,
            RegisterName::X20 => 20, RegisterName::X21 => 21, RegisterName::X22 => 22, RegisterName::X23 => 23,
            RegisterName::X24 => 24, RegisterName::X25 => 25, RegisterName::X26 => 26, RegisterName::X27 => 27,
            RegisterName::X28 => 28, RegisterName::X29 => 29, RegisterName::X30 => 30, RegisterName::SP => 31,
        }
    }

    pub fn from_index(i: usize) -> Option<Self> {
        match i {
            0 => Some(RegisterName::X0), 1 => Some(RegisterName::X1), 2 => Some(RegisterName::X2),
            3 => Some(RegisterName::X3), 4 => Some(RegisterName::X4), 5 => Some(RegisterName::X5),
            6 => Some(RegisterName::X6), 7 => Some(RegisterName::X7), 8 => Some(RegisterName::X8),
            9 => Some(RegisterName::X9), 10 => Some(RegisterName::X10), 11 => Some(RegisterName::X11),
            12 => Some(RegisterName::X12), 13 => Some(RegisterName::X13), 14 => Some(RegisterName::X14),
            15 => Some(RegisterName::X15), 16 => Some(RegisterName::X16), 17 => Some(RegisterName::X17),
            18 => Some(RegisterName::X18), 19 => Some(RegisterName::X19), 20 => Some(RegisterName::X20),
            21 => Some(RegisterName::X21), 22 => Some(RegisterName::X22), 23 => Some(RegisterName::X23),
            24 => Some(RegisterName::X24), 25 => Some(RegisterName::X25), 26 => Some(RegisterName::X26),
            27 => Some(RegisterName::X27), 28 => Some(RegisterName::X28), 29 => Some(RegisterName::X29),
            30 => Some(RegisterName::X30), 31 => Some(RegisterName::SP), _ => None,
        }
    }

    pub fn get_constraints(&self) -> RegisterConstraints {
        match self {
            RegisterName::SP => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Stack pointer".to_string(),
            },
            RegisterName::X29 => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Frame pointer".to_string(),
            },
            RegisterName::X30 => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Link register".to_string(),
            },
            RegisterName::X12 => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Reserved (e.g., for address calculation)".to_string(),
            },
            reg if (0..=7).contains(&reg.index()) => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Function arguments/return value".to_string(),
            },
            RegisterName::X8 => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Indirect result, temporary".to_string(),
            },
            reg if (9..=15).contains(&reg.index()) => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Caller-saved temporary".to_string(),
            },
            reg if (16..=17).contains(&reg.index()) => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Intra-procedure call temporary".to_string(),
            },
            RegisterName::X18 => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Platform register (OS-specific)".to_string(),
            },
            reg if (19..=28).contains(&reg.index()) => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Callee-saved".to_string(),
            },
            _ => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Unknown register".to_string(),
            },
        }
    }
}

const NUM_REGS: usize = 32; // X0-X30, SP
const ALLOCATABLE_INDICES: &[usize] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28];

pub struct Arm64RegisterAllocator {
    free_caller_saved: BinaryHeap<Reverse<usize>>, // X0-X15
    free_callee_saved: BinaryHeap<Reverse<usize>>, // X19-X28
    reg_map: [Option<Register<RegisterName>>; NUM_REGS],
    pub vreg_map: HashMap<String, Register<RegisterName>>,
    spill_offset: i32,
    live_ranges: HashMap<usize, (i32, i32)>, // v_reg ID -> (start, end) instruction indices
}

impl Arm64RegisterAllocator {
    pub fn new() -> Self {
        let mut free_caller_saved = BinaryHeap::new();
        let mut free_callee_saved = BinaryHeap::new();
        let mut reg_map: [Option<Register<RegisterName>>; NUM_REGS] = Default::default();
        let vreg_map = HashMap::with_capacity(16);
        let live_ranges = HashMap::with_capacity(16);

        for i in 0..NUM_REGS {
            if let Some(name) = RegisterName::from_index(i) {
                reg_map[i] = Some(Register {
                    p_reg: i,
                    v_reg: usize::MAX,
                    name,
                    next_uses: vec![],
                    address: 0,
                    spill_offset: None,
                });
                if ALLOCATABLE_INDICES.contains(&i) {
                    if i <= 15 {
                        free_caller_saved.push(Reverse(i));
                    } else {
                        free_callee_saved.push(Reverse(i));
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
            live_ranges,
        }
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
        end1 < start2 || end2 < start1 // No overlap in live ranges
    }

    fn allocate_spill_slot(&mut self) -> i32 {
        self.spill_offset += 8;
        if self.spill_offset % 16 != 0 {
            self.spill_offset += 8; // Ensure 16-byte alignment
        }
        self.spill_offset
    }

    pub fn free(&mut self, p_reg: usize) {
        if p_reg >= NUM_REGS {
            return;
        }
        if let Some(reg) = self.reg_map[p_reg].as_mut() {
            let v_reg = reg.v_reg;
            reg.v_reg = usize::MAX;
            reg.next_uses.clear();
            reg.address = 0;
            reg.spill_offset = None;
            if ALLOCATABLE_INDICES.contains(&p_reg) {
                if p_reg <= 15 {
                    self.free_caller_saved.push(Reverse(p_reg));
                } else {
                    self.free_callee_saved.push(Reverse(p_reg));
                }
            }
            self.live_ranges.remove(&v_reg);
        }
    }

    pub fn alloc(
        &mut self,
        v_reg: &Register<RegisterName>,
        vreg_name: &str,
        output: &mut String,
    ) -> Result<Register<RegisterName>, RegisterError> {
        // Collect coalescing candidates to avoid borrowing conflicts
        let coalesce_candidates: Vec<(usize, usize)> = self
            .reg_map
            .iter()
            .enumerate()
            .filter_map(|(i, r)| r.as_ref().map(|reg| (i, reg.v_reg)))
            .filter(|(i, v_reg)| *v_reg != usize::MAX && self.reg_map[*i].as_ref().unwrap().spill_offset.is_none())
            .filter(|(i, other_v_reg)| self.can_coalesce(v_reg.v_reg, *other_v_reg))
            .collect();

        for (p_reg, _) in coalesce_candidates {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                if !reg.name.get_constraints().can_allocate {
                    continue;
                }
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = v_reg.next_uses.clone();
                reg.address = v_reg.address;
                reg.spill_offset = None;
                self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                self.live_ranges.insert(v_reg.v_reg, (0, i32::MAX)); // Update with actual range
                return Ok(reg.clone());
            }
        }

        // Try caller-saved first
        if let Some(Reverse(p_reg)) = self.free_caller_saved.pop() {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                if !reg.name.get_constraints().can_allocate {
                    self.free_caller_saved.push(Reverse(p_reg));
                    return Err(RegisterError::NoRegistersAvailable);
                }
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = v_reg.next_uses.clone();
                reg.address = v_reg.address;
                reg.spill_offset = None;
                self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                self.live_ranges.insert(v_reg.v_reg, (0, i32::MAX));
                return Ok(reg.clone());
            }
        }
        // Try callee-saved
        if let Some(Reverse(p_reg)) = self.free_callee_saved.pop() {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                if !reg.name.get_constraints().can_allocate {
                    self.free_callee_saved.push(Reverse(p_reg));
                    return Err(RegisterError::NoRegistersAvailable);
                }
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = v_reg.next_uses.clone();
                reg.address = v_reg.address;
                reg.spill_offset = None;
                self.vreg_map.insert(vreg_name.to_string(), reg.clone());
                self.live_ranges.insert(v_reg.v_reg, (0, i32::MAX));
                if p_reg >= 19 && p_reg <= 28 {
                    write!(output, "    stp {}, x29, [sp, -16]!\n", reg.name)
                        .map_err(|_| RegisterError::NoRegistersAvailable)?;
                }
                return Ok(reg.clone());
            }
        }
        // Spill a register
        let spill_candidates: Vec<(usize, i32)> = self
            .reg_map
            .iter()
            .enumerate()
            .filter_map(|(i, r)| {
                r.as_ref().map(|reg| {
                    if reg.name.get_constraints().can_spill && reg.v_reg != usize::MAX {
                        Some((i, reg.next_uses.first().cloned().unwrap_or(i32::MAX)))
                    } else {
                        None
                    }
                }).flatten()
            })
            .collect();

        let spill_idx = spill_candidates
            .into_iter()
            .max_by_key(|(_, next_use)| *next_use)
            .map(|(i, _)| i)
            .ok_or(RegisterError::NoRegistersAvailable)?;

        let reg_name = self.reg_map[spill_idx]
            .as_ref()
            .map(|reg| reg.name.clone())
            .ok_or(RegisterError::NoRegistersAvailable)?;
        let spill_offset = self.allocate_spill_slot();
        write!(output, "    str {}, [sp, -{}]\n", reg_name, spill_offset)
            .map_err(|_| RegisterError::NoRegistersAvailable)?;

        if let Some(reg) = self.reg_map[spill_idx].as_mut() {
            let old_v_reg = reg.v_reg;
            reg.v_reg = v_reg.v_reg;
            reg.next_uses = v_reg.next_uses.clone();
            reg.address = v_reg.address;
            reg.spill_offset = Some(spill_offset);
            self.vreg_map.insert(vreg_name.to_string(), reg.clone());
            self.live_ranges.insert(v_reg.v_reg, (0, i32::MAX));
            self.live_ranges.remove(&old_v_reg);
            Ok(reg.clone())
        } else {
            Err(RegisterError::NoRegistersAvailable)
        }
    }
}

impl RegisterAllocator for Arm64RegisterAllocator {
    fn free(&mut self, p_reg: usize) {
        self.free(p_reg);
    }

    fn alloc(&mut self, v_reg: &str, output: &mut String) -> Result<usize, RegisterError> {
        if let Some(reg) = self.vreg_map.get(v_reg).cloned() {
            let allocated = self.alloc(&reg, v_reg, output)?;
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
            let allocated = self.alloc(&vreg, v_reg, output)?;
            if let Some(spill_offset) = vreg.spill_offset {
                write!(output, "    ldr {}, [sp, -{}]\n", allocated.name, spill_offset)
                    .map_err(|_| RegisterError::NoRegistersAvailable)?;
                if let Some(reg) = self.reg_map[allocated.p_reg].as_mut() {
                    reg.spill_offset = None;
                }
            }
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