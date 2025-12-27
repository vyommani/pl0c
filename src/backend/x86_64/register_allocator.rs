use crate::assembly_generator::RegisterAllocator;
use crate::backend::common::register_allocator_common::{Register, RegisterConstraints};
use std::collections::HashMap;
use std::fmt;
use crate::errors::{Pl0Result, Pl0Error};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegisterName {
    RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15, None,
}

impl fmt::Display for RegisterName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            RegisterName::RAX => "rax",
            RegisterName::RBX => "rbx",
            RegisterName::RCX => "rcx",
            RegisterName::RDX => "rdx",
            RegisterName::RSI => "rsi",
            RegisterName::RDI => "rdi",
            RegisterName::RBP => "rbp",
            RegisterName::RSP => "rsp",
            RegisterName::R8 => "r8",
            RegisterName::R9 => "r9",
            RegisterName::R10 => "r10",
            RegisterName::R11 => "r11",
            RegisterName::R12 => "r12",
            RegisterName::R13 => "r13",
            RegisterName::R14 => "r14",
            RegisterName::R15 => "r15",
            RegisterName::None => "none",
        };
        write!(f, "{}", s)
    }
}

impl RegisterName {
    pub fn from_usize(i: usize) -> Option<Self> {
        match i {
            0 => Some(RegisterName::RAX),
            1 => Some(RegisterName::RBX),
            2 => Some(RegisterName::RCX),
            3 => Some(RegisterName::RDX),
            4 => Some(RegisterName::RSI),
            5 => Some(RegisterName::RDI),
            6 => Some(RegisterName::RBP),
            7 => Some(RegisterName::RSP),
            8 => Some(RegisterName::R8),
            9 => Some(RegisterName::R9),
            10 => Some(RegisterName::R10),
            11 => Some(RegisterName::R11),
            12 => Some(RegisterName::R12),
            13 => Some(RegisterName::R13),
            14 => Some(RegisterName::R14),
            15 => Some(RegisterName::R15),
            _ => None,
        }
    }

    pub fn is_special_purpose(&self) -> bool {
        matches!(
            self,
            RegisterName::RSP
                | RegisterName::RBP
                | RegisterName::RAX
                | RegisterName::RCX
                | RegisterName::RDX
                | RegisterName::RSI
                | RegisterName::RDI
                | RegisterName::R12 // Reserve r12 for static links
        )
    }

    pub fn get_constraints(&self) -> RegisterConstraints {
        match self {
            RegisterName::RSP => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Stack pointer",
            },
            RegisterName::RBP => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Base pointer",
            },
            RegisterName::RAX => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Return value, some special instructions",
            },
            RegisterName::RCX => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Loop counter, string operations",
            },
            RegisterName::RDX => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "I/O operations, some special instructions",
            },
            RegisterName::RSI | RegisterName::RDI => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "String operations",
            },
            RegisterName::R12 => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Static link for nested procedures",
            },
            RegisterName::None => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Invalid register",
            },
            _ => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "General purpose",
            },
        }
    }
}

const NUM_REGS: usize = 16;
const UNALLOCATED_VREG: usize = usize::MAX;

pub struct X86_64RegisterAllocator {
    free_list: Vec<usize>,
    reg_map: [Option<Register<RegisterName>>; NUM_REGS],
    pub vreg_map: HashMap<String, Register<RegisterName>>,
    spill_slots: HashMap<String, u32>, // Maps vregs to stack offsets
    next_spill_offset: u32, // Tracks next available stack slot
}

impl X86_64RegisterAllocator {
    pub fn new() -> Self {
        let mut free_list = Vec::with_capacity(NUM_REGS);
        let mut reg_map: [Option<Register<RegisterName>>; NUM_REGS] = Default::default();
        let vreg_map = HashMap::new();
        let spill_slots = HashMap::new();
        const NAMES: [RegisterName; 16] = [
            RegisterName::RAX, RegisterName::RBX, RegisterName::RCX, RegisterName::RDX,
            RegisterName::RSI, RegisterName::RDI, RegisterName::RBP, RegisterName::RSP,
            RegisterName::R8, RegisterName::R9, RegisterName::R10, RegisterName::R11,
            RegisterName::R12, RegisterName::R13, RegisterName::R14, RegisterName::R15,
        ];
        for i in 0..NUM_REGS {
            if NAMES[i].get_constraints().can_allocate {
                free_list.push(i);
            }
            reg_map[i] = Some(Register::new(i, UNALLOCATED_VREG, NAMES[i].clone(), vec![], 0));
        }
        Self {
            free_list,
            reg_map,
            vreg_map,
            spill_slots,
            next_spill_offset: 16, // Start after static link (if any) at [rbp - 8]
        }
    }

    pub fn free(&mut self, p_reg: usize) {
        if let Some(reg) = self.reg_map[p_reg].as_mut() {
            reg.v_reg = UNALLOCATED_VREG;
            reg.next_uses.clear();
            self.free_list.push(p_reg);
        }
    }

    pub fn alloc(
        &mut self,
        v_reg: &Register<RegisterName>,
        output: &mut String,
    ) -> Pl0Result<Register<RegisterName>> {
        // Try to allocate a free register
        if let Some(p_reg) = self.free_list.pop() {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                if !reg.name.get_constraints().can_allocate {
                    self.free_list.push(p_reg);
                    return Err(Pl0Error::NoRegistersAvailable);
                }
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = v_reg.next_uses.clone();
                reg.address = v_reg.address;
                return Ok(reg.clone());
            } else {
                return Err(Pl0Error::RegisterAllocationError {message: "Invalid register state".to_string(),});
            }
        }

        // No free register: spill one
        let (spill_idx, spilled) = self
            .reg_map
            .iter()
            .enumerate()
            .filter_map(|(i, r)| r.as_ref().map(|reg| (i, reg)))
            .filter(|(_, reg)| reg.name.get_constraints().can_spill && reg.v_reg != UNALLOCATED_VREG)
            .max_by_key(|(_, reg)| reg.next_uses.first().cloned().unwrap_or(i32::MAX))
            .ok_or(Pl0Error::NoRegistersAvailable)?;

        // Find vreg name for spilled register
        let spilled_vreg = self
            .vreg_map
            .iter()
            .find_map(|(name, reg)| {
                if reg.v_reg == spilled.v_reg {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .ok_or_else(|| Pl0Error::RegisterAllocationError {message: format!("Spilled register {} has no vreg mapping", spill_idx),})?;

        // Allocate stack slot for spilled register
        let spill_offset = if let Some(offset) = self.spill_slots.get(&spilled_vreg) {
            *offset
        } else {
            let offset = self.next_spill_offset;
            self.spill_slots.insert(spilled_vreg.clone(), offset);
            self.next_spill_offset += 8; // 8-byte slot for 64-bit values
            offset
        };

        // Emit spill code: mov [rbp - offset], rX
        let spill_reg_name = RegisterName::from_usize(spill_idx)
            .ok_or_else(|| Pl0Error::RegisterAllocationError {message: format!("Invalid register index: {}", spill_idx),})?;
        output.push_str(&format!("    mov [rbp - {}], {}\n", spill_offset, spill_reg_name));

        // Update reg_map for new allocation
        if let Some(reg) = self.reg_map[spill_idx].as_mut() {
            reg.v_reg = v_reg.v_reg;
            reg.next_uses = v_reg.next_uses.clone();
            reg.address = v_reg.address;
            Ok(reg.clone())
        } else {
            Err(Pl0Error::RegisterAllocationError {message: "Invalid register state after spill".to_string(),})
        }
    }

    pub fn ensure(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize> {
        // Check if vreg is already in a physical register
        if let Some(reg) = self.vreg_map.get(v_reg) {
            for mapped_reg in self.reg_map.iter().flatten() {
                if mapped_reg.v_reg == reg.v_reg {
                    return Ok(mapped_reg.p_reg);
                }
            }
        }

        // Check if vreg is spilled
        if let Some(&spill_offset) = self.spill_slots.get(v_reg) {
            let temp_reg = Register::new(
                0,
                self.vreg_map.get(v_reg).map_or(UNALLOCATED_VREG, |r| r.v_reg),
                RegisterName::RAX,
                self.vreg_map.get(v_reg).map_or(vec![], |r| r.next_uses.clone()),
                0,
            );
            let allocated_reg = self.alloc(&temp_reg, output)?;
            let p_reg = allocated_reg.p_reg;
            output.push_str(&format!("    mov {}, [rbp - {}]\n", allocated_reg.name, spill_offset));
            self.vreg_map.insert(v_reg.to_string(), allocated_reg.clone());
            return Ok(p_reg);
        }

        // Allocate new register if vreg exists
        if let Some(vreg) = self.vreg_map.get(v_reg) {
            let vreg_cloned = vreg.clone();
            let allocated_reg = self.alloc(&vreg_cloned, output)?;
            self.vreg_map.insert(v_reg.to_string(), allocated_reg.clone());
            Ok(allocated_reg.p_reg)
        } else {
            Err(Pl0Error::RegisterAllocationError {message: format!("Unknown register: {}", v_reg),})
        }
    }
}

impl RegisterAllocator for X86_64RegisterAllocator {
    fn free(&mut self, p_reg: usize) {
        self.free(p_reg);
    }

    fn alloc(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize> {
        if let Some(reg) = self.vreg_map.get(v_reg) {
            for mapped_reg in self.reg_map.iter().flatten() {
                if mapped_reg.v_reg == reg.v_reg {
                    return Ok(mapped_reg.p_reg);
                }
            }
            let reg_cloned = reg.clone();
            let allocated_reg = self.alloc(&reg_cloned, output)?;
            self.vreg_map.insert(v_reg.to_string(), allocated_reg.clone());
            Ok(allocated_reg.p_reg)
        } else {
            Err(Pl0Error::RegisterAllocationError {message: format!("Unknown register: {}", v_reg),})
        }
    }

    fn ensure(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize> {
        self.ensure(v_reg, output)
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn get_vreg(&mut self, vreg: &str) -> Option<&dyn std::any::Any> {
        self.vreg_map.get(vreg).map(|reg| reg as &dyn std::any::Any)
    }
}