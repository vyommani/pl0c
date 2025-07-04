use crate::register_allocator_x86_64::X86_64RegisterAllocator;
use crate::register_allocator_arm64::Arm64RegisterAllocator;
use crate::register_allocator_common::{Register, RegisterError};
use crate::assembly_emitter_x86_64::X86_64AssemblyEmitter;
use crate::assembly_emitter_arm64::Arm64AssemblyEmitter;
use std::io;
use std::any::Any;

pub enum TargetArch {
    X86_64,
    ARM64,
}

pub trait RegisterAllocator {
    fn free(&mut self, p_reg: usize);
    fn alloc(&mut self, v_reg: &str) -> Result<usize, RegisterError>;
    fn ensure(&mut self, v_reg: &str) -> Result<usize, RegisterError>;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn get_vreg(&mut self, vreg: &str) -> Option<&dyn std::any::Any>;
}

pub trait AssemblyEmitter {
    fn emit(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, output: &mut dyn io::Write) -> Result<(), io::Error>;
    fn analyze_register_usage(&self, ir: &[String], allocator: &mut dyn RegisterAllocator) -> Result<(), RegisterError>;
}

#[derive()]
pub struct AssemblyGenerator {
    allocator: Box<dyn RegisterAllocator>,
    emitter: Box<dyn AssemblyEmitter>,
    assembly_output: Vec<u8>,
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
            assembly_output: Vec::new(),
        }
    }

    pub fn free(&mut self, p_reg: usize) {
        self.allocator.free(p_reg);
    }

    pub fn get_output(&self) -> &str {
        std::str::from_utf8(&self.assembly_output).unwrap_or("")
    }

    pub fn alloc(&mut self, v_reg: &str) -> Result<usize, RegisterError> {
        self.allocator.alloc(v_reg)
    }

    pub fn ensure(&mut self, v_reg: &str) -> Result<usize, RegisterError> {
        self.allocator.ensure(v_reg)
    }

    pub fn emit_assembly(&mut self, ir: &str) -> Result<(), Box<dyn std::error::Error>> {
        self.emitter.analyze_register_usage(
            &ir.lines().map(|line| line.to_string()).collect::<Vec<String>>(),
            &mut *self.allocator,
        )?;
        self.emitter.emit(
            &ir.lines().map(|line| line.to_string()).collect::<Vec<String>>(),
            &mut *self.allocator,
            &mut self.assembly_output,
        )?;
        Ok(())
    }
}