use std::collections::HashMap;
use std::fmt;

use crate::{assembly_generator::RegisterAllocator, register_allocator_common::{Register, RegisterConstraints, RegisterError}};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegisterName {
    X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15,
    X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, SP,
}

impl fmt::Display for RegisterName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl RegisterName {
    pub fn is_special_purpose(&self) -> bool {
        matches!(self, RegisterName::SP | RegisterName::X29 | RegisterName::X30)
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
            _ => RegisterConstraints {
                can_allocate: true,
                can_spill: true,
                special_purpose: "General purpose".to_string(),
            },
        }
    }
}


const NUM_REGS: usize = 31; // for ARM64 (X0-X30)

pub struct Arm64RegisterAllocator {
    free_list: Vec<usize>,
    reg_map: [Option<Register<RegisterName>>; NUM_REGS],
    vreg_map: HashMap<String, Register<RegisterName>>,
}

impl Arm64RegisterAllocator {

    pub fn new() -> Self {
        let mut free_list = Vec::with_capacity(NUM_REGS);
        let mut reg_map: [Option<Register<RegisterName>>; NUM_REGS] = Default::default();
        let vreg_map = HashMap::new();
        let names = [
            RegisterName::X0, RegisterName::X1, RegisterName::X2, RegisterName::X3, RegisterName::X4, RegisterName::X5, RegisterName::X6, RegisterName::X7,
            RegisterName::X8, RegisterName::X9, RegisterName::X10, RegisterName::X11, RegisterName::X12, RegisterName::X13, RegisterName::X14, RegisterName::X15,
            RegisterName::X16, RegisterName::X17, RegisterName::X18, RegisterName::X19, RegisterName::X20, RegisterName::X21, RegisterName::X22, RegisterName::X23,
            RegisterName::X24, RegisterName::X25, RegisterName::X26, RegisterName::X27, RegisterName::X28, RegisterName::X29, RegisterName::X30,
        ];
        for i in 0..NUM_REGS {
            if names[i].get_constraints().can_allocate {
                free_list.push(i);
            }
            reg_map[i] = Some(Register::new(i, usize::MAX, names[i].clone(), vec![], 0));
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
}

impl RegisterAllocator for Arm64RegisterAllocator {
    fn free(&mut self, p_reg: usize) {
        self.free(p_reg);
    }

    fn alloc(&mut self, v_reg: &str) -> Result<usize, RegisterError> {
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