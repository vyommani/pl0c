use super::IRGenerator;
use super::ir_emitter;
use super::tac::MemLoc;
use crate::{
    ast::Block,
    utils::errors::Pl0Result,
    semantic::symboltable::SymbolLocation,
};

pub fn initialize_local_variables(gen: &mut IRGenerator, block: &Block) -> Pl0Result<()> {
    if !block.var_decl.var_decl.is_empty() {
        // Collect all the variables and their offsets first
        gen.symbol_table.enter_scope(gen.scope.level());
        let mut var_offsets = Vec::new();
        for var_name in &block.var_decl.var_decl {
            if let Some(symbol) = gen.symbol_table.get(var_name) {
                if let SymbolLocation::StackOffset(offset) = symbol.location {
                    var_offsets.push(offset);
                }
            }
        }
        // Now emit the initialization code
        for offset in var_offsets {
            let zero_reg = gen.allocate_virtual_register();
            ir_emitter::emit_li(gen, &zero_reg, 0)?;
            ir_emitter::emit_st(gen, MemLoc::Local { offset }, &zero_reg)?;
        }
    }
    Ok(())
}