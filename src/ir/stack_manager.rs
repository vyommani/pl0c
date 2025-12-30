use super::IRGenerator;
use crate::{
    ast::Block,
    utils::errors::Pl0Result,
    semantic::symboltable::SymbolLocation,
    ir::code_emitter::{CodeEmitter, StringCodeEmitter},
};

pub fn initialize_local_variables(gen: &mut IRGenerator, block: &Block) -> Pl0Result<()> {
    if !block.var_decl.var_decl.is_empty() {
        // Collect all the variables and their offsets first
        let mut var_offsets = Vec::new();
        for var_name in &block.var_decl.var_decl {
            if let Some(symbol) = gen.symbol_table.get_at_level(var_name, gen.scope.level()) {
                if let SymbolLocation::StackOffset(offset) = symbol.location {
                    var_offsets.push(offset);
                }
            }
        }
        // Now emit the initialization code
        for offset in var_offsets {
            let zero_reg = gen.allocate_virtual_register();
            let mut emitter = StringCodeEmitter::new(&mut gen.code);
            emitter.emit_li(&zero_reg, "0")?;
            emitter.emit_st(&format!("bp-{}", offset), &zero_reg)?;
        }
    }
    Ok(())
}