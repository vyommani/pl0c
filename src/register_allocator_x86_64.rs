use std::collections::HashMap;
use std::fmt;
use crate::register_allocator_common::{RegisterConstraints, RegisterError, Register};
use crate::assembly_generator::RegisterAllocator;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegisterName {
    RAX, RBX, RCX, RDX,
    RSI, RDI, RBP, RSP,
    R8, R9, R10, R11, R12, R13, R14, R15,
}

impl fmt::Display for RegisterName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            RegisterName::RAX => "rax", RegisterName::RBX => "rbx", RegisterName::RCX => "rcx",
            RegisterName::RDX => "rdx", RegisterName::RSI => "rsi", RegisterName::RDI => "rdi",
            RegisterName::RBP => "rbp", RegisterName::RSP => "rsp", RegisterName::R8  => "r8",
            RegisterName::R9  => "r9", RegisterName::R10 => "r10", RegisterName::R11 => "r11",
            RegisterName::R12 => "r12", RegisterName::R13 => "r13", RegisterName::R14 => "r14",
            RegisterName::R15 => "r15",
        };
        write!(f, "{}", s)
    }
}

impl RegisterName {
    pub fn is_special_purpose(&self) -> bool {
        matches!(self, 
            RegisterName::RSP | RegisterName::RBP | 
            RegisterName::RAX | RegisterName::RCX | 
            RegisterName::RDX | RegisterName::RSI | 
            RegisterName::RDI
        )
    }

    pub fn get_constraints(&self) -> RegisterConstraints {
        match self {
            RegisterName::RSP => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Stack pointer".to_string(),
            },
            RegisterName::RBP => RegisterConstraints {
                can_allocate: false,
                can_spill: false,
                special_purpose: "Base pointer".to_string(),
            },
            RegisterName::RAX => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Return value, some special instructions".to_string(),
            },
            RegisterName::RCX => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "Loop counter, string operations".to_string(),
            },
            RegisterName::RDX => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "I/O operations, some special instructions".to_string(),
            },
            RegisterName::RSI | RegisterName::RDI => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "String operations".to_string(),
            },
            _ => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "General purpose".to_string(),
            },
        }
    }
}

const NUM_REGS: usize = 16; // for x86-64

pub struct X86_64RegisterAllocator {
    free_list: Vec<usize>,
    reg_map: [Option<Register<RegisterName>>; NUM_REGS],
    pub vreg_map: HashMap<String, Register<RegisterName>>,
}

impl X86_64RegisterAllocator {
    pub fn new() -> Self {
        let mut free_list = Vec::with_capacity(NUM_REGS);
        let mut reg_map: [Option<Register<RegisterName>>; NUM_REGS] = Default::default();
        let vreg_map = HashMap::new();
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
            reg_map[i] = Some(Register::new(i, usize::MAX, NAMES[i].clone(), vec![], 0));
        }
        Self {
            free_list,
            reg_map,
            vreg_map,
        }
    }

    pub fn free(&mut self, p_reg: usize) {
        if let Some(reg) = self.reg_map[p_reg].as_mut() {
            reg.v_reg = usize::MAX;
        }
        self.free_list.push(p_reg);
    }

    pub fn alloc(&mut self, v_reg: &Register<RegisterName>) -> Result<Register<RegisterName>, RegisterError> {
        // Try to allocate a free register
        if let Some(p_reg) = self.free_list.pop() {
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                if !reg.name.get_constraints().can_allocate {
                    self.free_list.push(p_reg);
                    return Err(RegisterError::NoRegistersAvailable);
                }
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = v_reg.next_uses.clone();
                reg.address = v_reg.address;
                return Ok(reg.clone());
            } else {
                return Err(RegisterError::NoRegistersAvailable);
            }
        }
        // No free register: spill one
        let (spill_idx, _spilled) = self.reg_map.iter().enumerate()
            .filter_map(|(i, r)| r.as_ref().map(|reg| (i, reg)))
            .filter(|(_, reg)| reg.name.get_constraints().can_spill)
            .max_by_key(|(_, reg)| reg.next_uses.first().cloned().unwrap_or(i32::MAX))
            .ok_or(RegisterError::NoRegistersAvailable)?;

        // TODO: Emit spill code for _spilled if needed

        if let Some(reg) = self.reg_map[spill_idx].as_mut() {
            reg.v_reg = v_reg.v_reg;
            reg.next_uses = v_reg.next_uses.clone();
            reg.address = v_reg.address;
            Ok(reg.clone())
        } else {
            Err(RegisterError::NoRegistersAvailable)
        }
    }

    pub fn ensure(&mut self, v_reg: &Register<RegisterName>) -> Result<usize, RegisterError>{
        // Use the virtual register name as the key
        let vreg_key = v_reg.name.to_string();
        if let Some(reg) = self.vreg_map.get(&vreg_key) {
            for mapped_reg in self.reg_map.iter().flatten() {
                if mapped_reg.v_reg == reg.v_reg {
                    return Ok(mapped_reg.p_reg);
                }
            }
        }
        if let Some(vreg) = self.vreg_map.get(&vreg_key) {
            let vreg_cloned = vreg.clone();
            let reg = self.alloc(&vreg_cloned)?;
            return Ok(reg.p_reg);
        } else {
            Err(RegisterError::UnknownRegister(vreg_key))
        }
    }
}

impl RegisterAllocator for X86_64RegisterAllocator {

    fn free(&mut self, p_reg: usize) {
        X86_64RegisterAllocator::free(self, p_reg);
    }

    fn alloc(&mut self, v_reg: &str) -> Result<usize, RegisterError> {
        // You need to parse v_reg (e.g., "v1") to an index or use your vreg_map
        if let Some(reg) = self.vreg_map.get(v_reg) {
            let reg = reg.clone();
            let reg = self.alloc(&reg)?;
            Ok(reg.p_reg)
        } else {
            Err(RegisterError::UnknownRegister(v_reg.to_string()))
        }
    }

    fn ensure(&mut self, v_reg: &str) -> Result<usize, RegisterError> {
        if let Some(reg) = self.vreg_map.get(v_reg) {
            for mapped_reg in self.reg_map.iter().flatten() {
                if mapped_reg.v_reg == reg.v_reg {
                    return Ok(mapped_reg.p_reg);
                }
            }
        }
        if let Some(vreg) = self.vreg_map.get(v_reg) {
            let vreg_cloned = vreg.clone();
            let reg = self.alloc(&vreg_cloned)?;
            return Ok(reg.p_reg);
        } else {
            Err(RegisterError::UnknownRegister(v_reg.to_string()))
        }
    }
}