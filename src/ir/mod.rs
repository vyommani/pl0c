pub mod generator;
pub mod visitor;
pub mod expression_handlers;
pub mod statement_handlers;
pub mod procedure_handlers;
pub mod symbol_helpers;
pub mod ir_emitter;
pub mod stack_manager;
pub mod ir_dispatch;
pub mod code_emitter;

pub use generator::IRGenerator;
pub use code_emitter::{CodeEmitter, StringCodeEmitter};
pub use ir_dispatch::*;
