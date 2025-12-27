// Architecture-specific modules
pub mod x86_64;
pub mod arm64;

// Shared backend infrastructure
pub mod common;

// Re-export commonly used types
pub use common::*;
pub mod assembly_generator;

pub use assembly_generator::{
    AssemblyGenerator,
    TargetArch,
    RegisterAllocator,
    AssemblyEmitter,
    DataInfo,
    RuntimeNeeds,
    ProcContext,
};
