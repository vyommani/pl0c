use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RegisterName {
    // x86-64
    RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15,
    // ARM
    R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, SP, LR, PC,
}

// Represents a basic block: a vector of IR lines.
#[derive(Debug)]
pub struct BasicBlock {
    pub id: usize,
    pub lines: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Register {
    pub pReg: i32,
    pub vReg: i32,
    pub name: RegisterName,
    pub nextUses: Vec<i32>,
    pub address: i64,
}

impl Register {
    pub fn new(pReg: i32, vReg: i32, name: RegisterName, nextUses: Vec<i32>, address: i64) -> Self {
        Self {
            pReg,
            vReg,
            name,
            nextUses,
            address,
        }
    }
}

pub struct RegisterAllocator {
    freeList: Vec<Register>,
    vRegList: Vec<Register>,
}

impl RegisterAllocator {
    pub fn with_arch(arch: &str) -> Self {
        let mut freeList = Vec::new();
        match arch {
            "x86-64" => {
                let names = vec![
                    RegisterName::RAX, RegisterName::RBX, RegisterName::RCX, RegisterName::RDX,
                    RegisterName::RSI, RegisterName::RDI, RegisterName::RBP, RegisterName::RSP,
                    RegisterName::R8, RegisterName::R9, RegisterName::R10, RegisterName::R11,
                    RegisterName::R12, RegisterName::R13, RegisterName::R14, RegisterName::R15,
                ];
                for (i, name) in names.into_iter().enumerate() {
                    freeList.push(Register::new(i as i32, -1, name, vec![], 0));
                }
            }
            "arm" => {
                let names = vec![
                    RegisterName::R0, RegisterName::R1, RegisterName::R2, RegisterName::R3,
                    RegisterName::R4, RegisterName::R5, RegisterName::R6, RegisterName::R7,
                    RegisterName::R8, RegisterName::R9, RegisterName::R10, RegisterName::R11,
                    RegisterName::R12,
                ];
                for (i, name) in names.into_iter().enumerate() {
                    freeList.push(Register::new(i as i32, -1, name, vec![], 0));
                }
            }
            _ => {}
        }
        Self {
            freeList,
            vRegList: Vec::new(),
        }
    }

    pub fn alloc(&mut self) -> Result<i32, &'static str> {
        if self.freeList.is_empty() {
            return Err("No free registers available");
        }
        let reg = self.freeList.pop().unwrap();
        let pReg = reg.pReg;
        self.vRegList.push(reg);
        Ok(pReg)
    }

    pub fn free(&mut self, reg: i32) -> Result<(), &'static str> {
        if let Some(pos) = self.vRegList.iter().position(|r| r.pReg == reg) {
            let reg = self.vRegList.remove(pos);
            self.freeList.push(reg);
            Ok(())
        } else {
            Err("Register not found in allocated registers")
        }
    }

    fn build_next_use(ir_lines: &[String]) -> HashMap<String, VecDeque<usize>> {
        let mut uses: HashMap<String, VecDeque<usize>> = HashMap::new();
        for (idx, line) in ir_lines.iter().enumerate() {
            for word in line.split(|c: char| !c.is_alphanumeric()) {
                if word.starts_with('v') && word[1..].chars().all(|c| c.is_digit(10)) {
                    uses.entry(word.to_string()).or_default().push_back(idx);
                }
            }
        }
        uses
    }

    pub fn divide_into_basic_blocks(ir_lines: &[String]) -> Vec<BasicBlock> {
        let mut leaders = HashSet::new();
        let mut label_to_index = HashMap::new();

        // 1. First instruction is a leader
        if !ir_lines.is_empty() {
            leaders.insert(0);
        }

        // 2. Find all labels and jump targets
        for (i, line) in ir_lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.ends_with(':') {
                label_to_index.insert(trimmed.trim_end_matches(':'), i);
                leaders.insert(i);
            }
        }

        // 3. Find leaders after jumps/calls/returns
        for (i, line) in ir_lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("jmp")
                || trimmed.starts_with("je")
                || trimmed.starts_with("jne")
                || trimmed.starts_with("jg")
                || trimmed.starts_with("jl")
                || trimmed.starts_with("call")
                || trimmed.starts_with("ret")
            {
                // Next instruction is a leader
                if i + 1 < ir_lines.len() {
                    leaders.insert(i + 1);
                }
                // Target label is a leader (already handled above)
            }
        }

        // 4. Sort leaders and build blocks
        let mut leaders: Vec<usize> = leaders.into_iter().collect();
        leaders.sort_unstable();

        let mut blocks = Vec::new();
        for (i, &start) in leaders.iter().enumerate() {
            let end = if i + 1 < leaders.len() {
                leaders[i + 1]
            } else {
                ir_lines.len()
            };
            let lines = ir_lines[start..end].to_vec();
            blocks.push(BasicBlock { id: i, lines });
        }

        blocks
    }
}