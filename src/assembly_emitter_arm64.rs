use crate::assembly_generator::{AssemblyEmitter, RegisterAllocator};
use std::io::{self, Write};

pub struct Arm64AssemblyEmitter;

impl AssemblyEmitter for Arm64AssemblyEmitter {
    fn emit(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> Result<(), io::Error> {
      for line in ir {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
           //Todo: Implement the actual assembly emission logic here
        }
        Ok(())
    }
}