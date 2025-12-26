use super::IRGenerator;
use crate::{
    ast::{Node, Block, ProcDecl},
    errors::{Pl0Result, Pl0Error},
    code_emitter::{CodeEmitter, StringCodeEmitter},
    config::codegen::{WORD_SIZE, STACK_ALIGNMENT},
};

pub fn handle_proc_decl(gen: &mut IRGenerator, expr: &ProcDecl) -> Pl0Result<()> {
    // Only emit procedures if we're not already in a procedure
    if !gen.scope.in_procedure() && !gen.procedures_emitted {
        for (name, proc_block) in &expr.procedurs {
            emit_single_procedure(gen, name, proc_block)?;
        }
    }
    Ok(())
}

pub fn handle_block(gen: &mut IRGenerator, block: &Block) -> Pl0Result<()> {
    use super::ir_emitter;
    use super::stack_manager;
    
    // Process constants first
    if !block.const_decl.const_decl.is_empty() {
        block.const_decl.accept(gen)?;
    }

    // Process variable declarations
    if !block.var_decl.var_decl.is_empty() {
        block.var_decl.accept(gen)?;
    }

    // Emit all procedures at the top level (only once)
    if !gen.scope.in_procedure() && !gen.procedures_emitted {
        emit_procedures(gen, block)?;
    }

    // Process the main statement
    if let Some(stmt) = &block.statement {
        // Emit main label if this is the main block
        if !gen.scope.in_procedure() && !gen.main_emitted {
            ir_emitter::emit_label(gen, "main")?;
            gen.main_emitted = true;
            // Initialize local variables to zero
            stack_manager::initialize_local_variables(gen, block)?;
        }
        stmt.accept(gen)?;
    }
    Ok(())
}

fn emit_single_procedure(gen: &mut IRGenerator, name: &str, proc_block: &Option<Box<dyn Node>>) -> Pl0Result<()> {
    use super::ir_emitter;
    
    ir_emitter::emit_label(gen, name)?;
    
    // Set scope level based on procedure's level in symbol table
    let proc_symbol = gen.symbol_table.get(name)
        .ok_or_else(|| Pl0Error::codegen_error(format!("Procedure {} not found in symbol table", name)))?;
    gen.scope = gen.scope.push_scope(true, Some(proc_symbol.level + 1), false);
    
    // Calculate stack size
    let mut stack_slots = 0;
    if let Some(proc_block) = proc_block {
        if let Some(block) = proc_block.as_any().downcast_ref::<Block>() {
            stack_slots = block.var_decl.var_decl.len();
        }
    }
    let stack_size = calculate_stack_size(stack_slots);
    
    // Emit procedure prologue
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_proc_enter(stack_size)?;
    
    // Process procedure body
    if let Some(proc_block) = proc_block {
        proc_block.accept(gen)?;
    }
    
    // Emit procedure epilogue
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_proc_exit()?;
    
    // Restore parent scope
    gen.scope.pop_scope()?;
    Ok(())
}

fn collect_all_procedures<'a>(block: &'a Block, procedures: &mut Vec<(String, &'a Option<Box<dyn Node>>)>) {
    // Add procedures from this block
    for (name, proc_block) in &block.proc_decl.procedurs {
        procedures.push((name.clone(), proc_block));
        // Recursively collect nested procedures
        if let Some(proc_block) = proc_block {
            if let Some(nested_block) = proc_block.as_any().downcast_ref::<Block>() {
                collect_all_procedures(nested_block, procedures);
            }
        }
    }
}

fn emit_procedures(gen: &mut IRGenerator, block: &Block) -> Pl0Result<()> {
    if gen.procedures_emitted {
        return Ok(());
    }
    let mut all_procedures = Vec::new();
    collect_all_procedures(block, &mut all_procedures);
    for (name, proc_block) in all_procedures {
        emit_single_procedure(gen, &name, proc_block)?;
    }
    gen.procedures_emitted = true;
    Ok(())
}

fn calculate_stack_size(stack_slots: usize) -> usize {
    let base_size = stack_slots * WORD_SIZE + WORD_SIZE; // +8 for return address
    (base_size + STACK_ALIGNMENT - 1) & !(STACK_ALIGNMENT - 1)
}