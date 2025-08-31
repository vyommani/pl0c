use crate::assembly_emitter_arm64::Arm64AssemblyEmitter;
use crate::assembly_emitter_x86_64::X86_64AssemblyEmitter;
use crate::register_allocator_arm64::Arm64RegisterAllocator;
use crate::register_allocator_x86_64::X86_64RegisterAllocator;
use crate::utils::string_utils::write_line;
use crate::errors::Pl0Result;
use crate::errors::Pl0Error;
use std::any::Any;
use std::collections::{HashMap, HashSet};
use regex::Regex;

pub struct RuntimeNeeds {
    pub write_int: bool,
    pub read_int: bool,
    pub write_str: bool,
}
impl RuntimeNeeds {
    pub fn new() -> Self {
        Self {
            write_int: false,
            read_int: false,
            write_str: false,
        }
    }
}

pub struct DataInfo {
    pub variables: HashSet<String>,
    pub constants: HashMap<String, String>,
    pub runtime_needs: RuntimeNeeds,
    pub strings: Vec<String>,
}

impl DataInfo {
    pub fn new() -> Self {
        Self {
            variables: HashSet::new(),
            constants: HashMap::new(),
            runtime_needs: RuntimeNeeds::new(),
            strings: Vec::new(),
        }
    }

    pub fn from_ir(ir: &[String]) -> Pl0Result<Self> {
        let mut data_info = Self::new();
        let const_regex = Regex::new(r"const\s+([^=]+)=(.+)").map_err(|e| Pl0Error::CodeGenError {
            message: format!("Invalid const regex: {}", e),
            line: None,
        })?;

        for line in ir {
            let line = line.trim();
            if line.starts_with("var ") {
                let vars = line[4..].split(',').map(|v| v.trim()).filter(|v| !v.is_empty());
                for v in vars {
                    data_info.variables.insert(v.to_string());
                }
            } else if line.starts_with("const ") {
                if let Some(captures) = const_regex.captures(line) {
                    let name = captures.get(1).map_or("", |m| m.as_str()).trim();
                    let value = captures.get(2).map_or("", |m| m.as_str()).trim();
                    if name.is_empty() || value.is_empty() {
                        return Err(Pl0Error::CodeGenError {
                            message: "Invalid const format".to_string(),line: None});
                    }
                    data_info.constants.insert(name.to_string(), value.to_string());
                } else {
                    return Err(Pl0Error::CodeGenError {message: "Invalid const syntax".to_string(), line: None});
                }
            } else if line.contains("write_int") {
                data_info.runtime_needs.write_int = true;
            } else if line.contains("read_int") {
                data_info.runtime_needs.read_int = true;
            } else if line.contains("write_str") {
                data_info.runtime_needs.write_str = true;
                let string = line[10..].trim_matches(|c| c == '"' || c == '\'').to_string();
                if !string.is_empty() && !data_info.strings.contains(&string) {
                    data_info.strings.push(string);
                }
            }
        }
        Ok(data_info)
    }

    pub fn emit_data_section(&self, output: &mut String, target: TargetArch) -> Pl0Result<()> {
        match target {
            TargetArch::ARM64 => {
                for (name, value) in &self.constants {
                    write_line(output, format_args!(".equ {}, {}\n", name, value))?;
                }
                if !self.variables.is_empty() {
                    write_line(output, format_args!(".section __DATA,__bss\n"))?;
                    write_line(output, format_args!(".align 3\n"))?;
                    for v in &self.variables {
                        write_line(output, format_args!("{}:\n", v))?;
                        write_line(output, format_args!("    .skip 8\n"))?;
                    }
                }
            }
            TargetArch::X86_64 => {
                let mut used_constants = HashSet::new();
                for (name, _) in &self.constants {
                    if output.contains(name) {
                        used_constants.insert(name.clone());
                    }
                }
                output.push_str("section .data\n");
                if !self.strings.is_empty() || self.runtime_needs().write_int || self.runtime_needs().write_str {
                    output.push_str("    newline db 0xA\n");
                    for (i, s) in self.strings.iter().enumerate() {
                        output.push_str(&format!("    string_{} db \"{}\", 0\n", i, s));
                    }
                }
                if self.runtime_needs().write_int {
                    output.push_str("    digitSpace times 20 db 0\n");
                }
                for (name, value) in &self.constants {
                    if used_constants.contains(name) {
                        output.push_str(&format!("    {} dq {}\n", name, value));
                    }
                }
                if !self.variables.is_empty() {
                    output.push_str("section .bss\n");
                    output.push_str("    align 8\n");
                    for v in &self.variables {
                        output.push_str(&format!("    {} resq 1\n", v));
                    }
                }
            }
        }
        Ok(())
    }

    pub fn runtime_needs(&self) -> &RuntimeNeeds {
        &self.runtime_needs
    }
}

pub enum ProcContext {
    Main,
    Procedure {
        name: String,
        stack: Vec<String>,
        stack_sizes: Vec<usize>,
    },
}

impl ProcContext {
    pub fn new() -> Self {
        ProcContext::Main
    }

    pub fn is_in_proc(&self) -> bool {
        matches!(self, ProcContext::Procedure { .. })
    }

    pub fn current_proc(&self) -> Option<&str> {
        match self {
            ProcContext::Procedure { name, .. } => Some(name),
            ProcContext::Main => None,
        }
    }

    pub fn push_proc(&mut self, proc_name: String) {
        match self {
            ProcContext::Main => {
                *self = ProcContext::Procedure {
                    name: proc_name,
                    stack: vec![],
                    stack_sizes: vec![],
                };
            }
            ProcContext::Procedure { name, stack, stack_sizes } => {
                stack.push(name.clone());
                *self = ProcContext::Procedure {
                    name: proc_name,
                    stack: stack.clone(),
                    stack_sizes: stack_sizes.clone(),
                };
            }
        }
    }

    pub fn pop_proc(&mut self) {
        match self {
            ProcContext::Procedure { stack, stack_sizes, .. } => {
                if let Some(last) = stack.pop() {
                    *self = ProcContext::Procedure {
                        name: last,
                        stack: stack.clone(),
                        stack_sizes: stack_sizes.clone(),
                    };
                } else {
                    *self = ProcContext::Main;
                }
            }
            ProcContext::Main => {}
        }
    }

    pub fn push_stack_size(&mut self, size: usize) {
        match self {
            ProcContext::Procedure { stack_sizes, .. } => {
                stack_sizes.push(size);
            }
            ProcContext::Main => {}
        }
    }

    pub fn pop_stack_size(&mut self) -> Option<usize> {
        match self {
            ProcContext::Procedure { stack_sizes, .. } => stack_sizes.pop(),
            ProcContext::Main => None,
        }
    }
}

pub enum TargetArch {
    X86_64,
    ARM64,
}

pub trait RegisterAllocator {
    fn free(&mut self, p_reg: usize);
    fn alloc(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize>;
    fn ensure(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize>;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn get_vreg(&mut self, vreg: &str) -> Option<&dyn std::any::Any>;
}

pub trait AssemblyEmitter {
    fn emit(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()>;
    fn compute_vreg_next_uses(&self, ir: &[String], allocator: &mut dyn RegisterAllocator) -> Pl0Result<()>;
}

#[derive()]
pub struct AssemblyGenerator {
    allocator: Box<dyn RegisterAllocator>,
    emitter: Box<dyn AssemblyEmitter>,
    assembly_output: String,
    target: TargetArch,
}

impl AssemblyGenerator {
    pub fn new(target: TargetArch) -> Self {
        let allocator: Box<dyn RegisterAllocator> = match target {
            TargetArch::X86_64 => Box::new(X86_64RegisterAllocator::new()),
            TargetArch::ARM64 => Box::new(Arm64RegisterAllocator::new()),
        };
        let emitter: Box<dyn AssemblyEmitter> = match target {
            TargetArch::X86_64 => Box::new(X86_64AssemblyEmitter),
            TargetArch::ARM64 => Box::new(Arm64AssemblyEmitter),
        };
        Self {
            target,
            allocator,
            emitter,
            assembly_output: String::new(),
        }
    }

    pub fn free(&mut self, p_reg: usize) {
        self.allocator.free(p_reg);
    }

    pub fn get_output(&self) -> &str {
        &self.assembly_output
    }

    pub fn alloc(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize> {
        self.allocator.alloc(v_reg, output)
    }

    pub fn ensure(&mut self, v_reg: &str, output: &mut String) -> Pl0Result<usize> {
        self.allocator.ensure(v_reg, output)
    }

    pub fn emit_assembly(&mut self, ir: &str) -> Pl0Result<()> {
        self.emitter.compute_vreg_next_uses(&ir.lines().map(|line| line.to_string()).collect::<Vec<String>>(), &mut *self.allocator)?;
        self.emitter.emit(&ir.lines().map(|line| line.to_string()).collect::<Vec<String>>(), &mut *self.allocator, &mut self.assembly_output)?;
        Ok(())
    }
}
