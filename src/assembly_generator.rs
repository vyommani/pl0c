
use std::collections::HashMap;
use std::fmt;
use regex::Regex;


/// Represents x86-64 register names
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegisterName {
    RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15,
}

impl fmt::Display for RegisterName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
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

#[derive(Debug)]
pub struct RegisterConstraints {
    pub can_allocate: bool,
    pub can_spill: bool,
    pub special_purpose: String,
}

#[derive(Debug)]
pub enum RegisterError {
    UnknownRegister(String),
    InvalidInstruction(String),
    NoRegistersAvailable,
    SpillFailed,
    RegisterConstraintViolation(String),
    StackFrameError(String),
}

impl fmt::Display for RegisterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegisterError::UnknownRegister(reg) => write!(f, "Unknown register: {}", reg),
            RegisterError::InvalidInstruction(inst) => write!(f, "Invalid instruction: {}", inst),
            RegisterError::NoRegistersAvailable => write!(f, "No registers available for allocation"),
            RegisterError::SpillFailed => write!(f, "Failed to spill register to memory"),
            RegisterError::RegisterConstraintViolation(msg) => write!(f, "Register constraint violation: {}", msg),
            RegisterError::StackFrameError(msg) => write!(f, "Stack frame error: {}", msg),
        }
    }
}

/// Represents a register with its current state
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Register {
    pub p_reg: usize,
    pub v_reg: usize,
    pub name: RegisterName,
    pub next_uses: Vec<i32>,
    pub address: i64,
}

impl Register {
    pub fn new(p_reg: usize, v_reg: usize, name: RegisterName, next_uses: Vec<i32>, address: i64) -> Self {
        Self { p_reg, v_reg, name, next_uses, address }
    }

    pub fn is_free(&self) -> bool {
        self.v_reg == usize::MAX
    }
}

const NUM_REGS: usize = 16; // for x86-64

/// Main assembly generator that handles register allocation and code generation
pub struct AssemblyGenerator {
    free_list: Vec<usize>,
    reg_map: [Option<Register>; NUM_REGS],
    vreg_map: HashMap<String, Register>,
    assembly_output: String,
}

impl AssemblyGenerator {
    pub fn new() -> Self {
        let mut free_list = Vec::with_capacity(NUM_REGS);
        let mut reg_map: [Option<Register>; NUM_REGS] = Default::default();
        let vreg_map = HashMap::new();
        let names = [
            RegisterName::RAX, RegisterName::RBX, RegisterName::RCX, RegisterName::RDX,
            RegisterName::RSI, RegisterName::RDI, RegisterName::RBP, RegisterName::RSP,
            RegisterName::R8, RegisterName::R9, RegisterName::R10, RegisterName::R11,
            RegisterName::R12, RegisterName::R13, RegisterName::R14, RegisterName::R15,
        ];
        for i in 0..NUM_REGS {
            // Only add allocatable registers to free list
            if names[i].get_constraints().can_allocate {
                free_list.push(i);
            }
            reg_map[i] = Some(Register::new(i, usize::MAX, names[i].clone(), vec![], 0));
        }
        Self {
            free_list,
            reg_map,
            vreg_map,
            assembly_output: String::new(),
        }
    }

    pub fn free(&mut self, p_reg: usize) {
        if let Some(reg) = self.reg_map[p_reg].as_mut() {
            reg.v_reg = usize::MAX; // Mark as unmapped
        }
        self.free_list.push(p_reg);
    }

    pub fn get_output(&self) -> String {
        self.assembly_output.clone()
    }

    pub fn alloc(&mut self, v_reg: &Register) -> Result<Register, RegisterError> {
        if let Some(p_reg) = self.free_list.pop() {
            // Update the existing Register in reg_map
            if let Some(reg) = self.reg_map[p_reg].as_mut() {
                // Check if register is allocatable
                if !reg.name.get_constraints().can_allocate {
                    self.free_list.push(p_reg);
                    return Err(RegisterError::NoRegistersAvailable);
                }
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = v_reg.next_uses.clone();
                reg.address = v_reg.address;
                Ok(reg.clone())
            } else {
                Err(RegisterError::NoRegistersAvailable)
            }
        } else {
            // Spill: find the register with the furthest next use
            let (spill_idx, spilled) = self.reg_map.iter().enumerate()
                .filter_map(|(i, r)| r.as_ref().map(|reg| (i, reg)))
                .filter(|(_, reg)| reg.name.get_constraints().can_spill)
                .max_by_key(|(_, reg)| reg.next_uses.first().cloned().unwrap_or(i32::MAX))
                .ok_or(RegisterError::NoRegistersAvailable)?;

            // Emit spill code if needed
            if spilled.next_uses.iter().any(|&use_idx| use_idx > 0) {
                self.assembly_output.push_str(&format!(
                    "    mov [{}], {}\n", spilled.address, spilled.name
                ));
            }

            // Update the Register in reg_map for the new v_reg
            if let Some(reg) = self.reg_map[spill_idx].as_mut() {
                reg.v_reg = v_reg.v_reg;
                reg.next_uses = v_reg.next_uses.clone();
                reg.address = v_reg.address;
                Ok(reg.clone())
            } else {
                Err(RegisterError::SpillFailed)
            }
        }
    }

    pub fn ensure(&mut self, v_reg: &Register) -> Result<Register, RegisterError> {
        // If already mapped, return it
        if let Some(reg) = self.vreg_map.get(&format!("v{}", v_reg.v_reg)) {
            if reg.p_reg != usize::MAX {
                return Ok(self.reg_map[reg.p_reg].as_ref().unwrap().clone());
            }
        }
        // Otherwise, allocate and emit load
        let reg = self.alloc(v_reg)?;
        self.assembly_output.push_str(&format!(
            "    mov {}, [{}]\n", reg.name, reg.address
        ));
        Ok(reg)
    }

    /// Analyzes IR code to build a map of virtual registers and their usage information.
    /// This includes tracking when registers are used and building the virtual register map.
    pub fn analyze_register_usage(&mut self, ir_lines: &[String]) {
        // Clear any old next_uses
        for reg in self.vreg_map.values_mut() {
            reg.next_uses.clear();
        }
        for (idx, line) in ir_lines.iter().enumerate() {
            for word in line.split(|c: char| !c.is_alphanumeric()) {
                if word.starts_with('v') && word[1..].chars().all(|c| c.is_digit(10)) {
                    let reg = self.vreg_map.entry(word.to_string()).or_insert_with(|| {
                        Register::new(
                            usize::MAX, // not mapped yet
                            word[1..].parse().unwrap_or(usize::MAX),
                            RegisterName::RAX, // placeholder
                            vec![],
                            0,
                        )
                    });
                    reg.next_uses.push(idx as i32);
                }
            }
        }
    }

    /// Generates x86-64 assembly code from IR with register allocation
    pub fn generate_assembly(&mut self, ir: &str) -> Result<(), RegisterError> {

        // Define regex patterns as static constants for IR parsing
        let binary_op_pattern = Regex::new(r"(\w+)\s+(\w+),\s*(\w+)").unwrap();
        let unary_op_pattern =  Regex::new(r"(\w+)\s+(\w+)").unwrap();
        let section_pattern = Regex::new(r"\.section\s+(\w+)").unwrap();
        let global_pattern = Regex::new(r"global\s+(\w+)").unwrap();
        let label_pattern = Regex::new(r"(\w+):").unwrap();

        // Split input string into lines and filter out empty lines
        let ir_lines: Vec<String> = ir.lines()
            .map(|s| s.trim().to_string())
            .collect();
        
        // Analyze register usage and build virtual register map
        self.analyze_register_usage(&ir_lines);
        
        for (i, line) in ir_lines.iter().enumerate() {
            let line = line.trim();
            
            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Handle section declarations
            if let Some(cap) = section_pattern.captures(line) {
                self.assembly_output.push_str(&format!("section .{}\n", &cap[1]));
                continue;
            }

            // Handle global declarations
            if let Some(cap) = global_pattern.captures(line) {
                self.assembly_output.push_str(&format!("    global {}\n", &cap[1]));
                continue;
            }

            // Handle label declarations
            if let Some(cap) = label_pattern.captures(line) {
                self.assembly_output.push_str(&format!("{}:\n", &cap[1]));
                continue;
            }

            // Try binary operation first
            if let Some(cap) = binary_op_pattern.captures(line) {
                let op = &cap[1];
                let dst = &cap[2];
                let src = &cap[3];

                let src_reg = self.vreg_map.get(src)
                    .ok_or_else(|| RegisterError::UnknownRegister(src.to_string()))?
                    .clone();
                let dst_reg = self.vreg_map.get(dst)
                    .ok_or_else(|| RegisterError::UnknownRegister(dst.to_string()))?
                    .clone();

                let psrc = self.ensure(&src_reg)?;
                let pdst = self.ensure(&dst_reg)?;

                // Emit instruction
                self.assembly_output.push_str(&format!(
                    "    {} {}, {}\n", op, pdst.name, psrc.name
                ));

                self.update_register_lifetimes(src, dst, i.try_into().unwrap())?;
            }
            // Try unary operation
            else if let Some(cap) = unary_op_pattern.captures(line) {
                let op = &cap[1];
                let dst = &cap[2];

                let dst_reg = self.vreg_map.get(dst)
                    .ok_or_else(|| RegisterError::UnknownRegister(dst.to_string()))?
                    .clone();

                let pdst = self.ensure(&dst_reg)?;

                self.assembly_output.push_str(&format!(
                    "    {} {}\n", op, pdst.name
                ));

                if let Some(reg) = self.vreg_map.get_mut(dst) {
                    reg.next_uses.retain(|&use_idx| use_idx > i as i32);
                    if reg.next_uses.is_empty() {
                        self.free(pdst.p_reg);
                    }
                }
            }
            else {
                // Pass through unrecognized instructions
                self.assembly_output.push_str(&format!("    {}\n", line));
            }
        }
        Ok(())
    }

    fn update_register_lifetimes(&mut self, src: &str, dst: &str, current_idx: i32) -> Result<(), RegisterError> {
        // Collect registers to free
        let mut regs_to_free = Vec::new();

        // Update source register lifetime
        if let Some(src_reg) = self.vreg_map.get_mut(src) {
            src_reg.next_uses.retain(|&use_idx| use_idx > current_idx);
            if src_reg.next_uses.is_empty() && src_reg.p_reg != usize::MAX {
                regs_to_free.push(src_reg.p_reg);
                src_reg.p_reg = usize::MAX;
            }
        }

        // Update destination register lifetime
        if let Some(dst_reg) = self.vreg_map.get_mut(dst) {
            dst_reg.next_uses.retain(|&use_idx| use_idx > current_idx);
            if dst_reg.next_uses.is_empty() && dst_reg.p_reg != usize::MAX {
                regs_to_free.push(dst_reg.p_reg);
                dst_reg.p_reg = usize::MAX;
            }
        }

        // Free registers after dropping the mutable borrows
        for reg in regs_to_free {
            self.free(reg);
        }

        Ok(())
    }
}