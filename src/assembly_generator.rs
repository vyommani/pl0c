use crate::assembly_emitter_arm64::Arm64AssemblyEmitter;
use crate::assembly_emitter_x86_64::X86_64AssemblyEmitter;
use crate::register_allocator_arm64::Arm64RegisterAllocator;
use crate::register_allocator_x86_64::X86_64RegisterAllocator;
use crate::errors::Pl0Result;
use std::any::Any;
use std::collections::HashSet;
use std::collections::HashMap;

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

struct DataInfo {
    variables: HashSet<String>,
    constants: HashMap<String, String>,
    runtime_needs: RuntimeNeeds,
    strings: Vec<String>,
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
