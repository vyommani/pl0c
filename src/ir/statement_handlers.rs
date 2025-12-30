use super::IRGenerator;
use crate::{
    ast::{ExpressionNode, AssignStmt, BeginStmt, CallStmt, IfStmt,WhileStatement, Read, Write, WriteStr, ConstDecl, VarDecl,},
    utils::errors::{Pl0Result, Pl0Error},
    ir::code_emitter::{CodeEmitter, StringCodeEmitter},
};

pub fn handle_assign(gen: &mut IRGenerator, stmt: &AssignStmt) -> Pl0Result<()> {
    use super::symbol_helpers;
    
    let symbol = symbol_helpers::get_variable_symbol(gen, &stmt.identifier, "variable in assignment")?.clone();
    let expr = stmt.expr.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("Assign statement missing expression"))?;
    let vreg = ExpressionNode::accept(expr.as_ref(), gen)?;
    symbol_helpers::emit_store_to_symbol(gen, &symbol, &vreg, &stmt.identifier)?;
    Ok(())
}

pub fn handle_call(gen: &mut IRGenerator, call: &CallStmt) -> Pl0Result<()> {
    use super::symbol_helpers;
    use crate::semantic::symboltable::SymbolLocation;
    
    let symbol = symbol_helpers::get_procedure_symbol(gen, &call.identifier, "procedure")?;
    let label = match &symbol.location {
        SymbolLocation::GlobalLabel(label) => label.clone(),
        SymbolLocation::None => call.identifier.clone(),
        _ => return Err(Pl0Error::codegen_error(format!("Procedure {} has no valid address", call.identifier))),
    };
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_call(&label)
}

pub fn handle_begin(gen: &mut IRGenerator, expr: &BeginStmt) -> Pl0Result<()> {
    for stmt in &expr.stmts {
        if let Some(stmt) = stmt {
            stmt.accept(gen)?;
        }
    }
    Ok(())
}

pub fn handle_if(gen: &mut IRGenerator, expr: &IfStmt) -> Pl0Result<()> {
    use super::ir_emitter;
    
    let else_label = gen.create_label();
    let end_label = gen.create_label();
    
    let condition = expr.condition.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("If statement missing condition"))?;
    let cond_vreg = ExpressionNode::accept(condition.as_ref(), gen)?;
    ir_emitter::emit_branch_if_zero(gen, &cond_vreg, &else_label)?;
    
    let then_branch = expr.then_branch.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("If statement missing then branch"))?;
    then_branch.accept(gen)?;
    
    if let Some(else_branch) = &expr.else_branch {
        ir_emitter::emit_jump(gen, &end_label)?;
        ir_emitter::emit_label(gen, &else_label)?;
        else_branch.accept(gen)?;
        ir_emitter::emit_label(gen, &end_label)?;
    } else {
        ir_emitter::emit_jump(gen, &end_label)?;
        ir_emitter::emit_label(gen, &else_label)?;
        ir_emitter::emit_label(gen, &end_label)?;
    }
    Ok(())
}

pub fn handle_while(gen: &mut IRGenerator, stmt: &WhileStatement) -> Pl0Result<()> {
    use super::ir_emitter;
    
    let start_label = ir_emitter::create_and_emit_label(gen)?;
    let end_label = gen.create_label();
    
    let condition = stmt.condition.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("While statement missing condition"))?;
    let cond_vreg = ExpressionNode::accept(condition.as_ref(), gen)?;
    ir_emitter::emit_branch_if_zero(gen, &cond_vreg, &end_label)?;
    
    let body = stmt.body.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("While statement missing body"))?;
    body.accept(gen)?;
    
    ir_emitter::emit_jump(gen, &start_label)?;
    ir_emitter::emit_label(gen, &end_label)?;
    Ok(())
}

pub fn handle_read_int(gen: &mut IRGenerator, expr: &Read) -> Pl0Result<()> {
    use super::symbol_helpers;
    
    let symbol = symbol_helpers::get_variable_symbol(gen, &expr.identifier, "variable in read")?.clone();
    let vreg = gen.allocate_virtual_register();
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_read_int(&vreg)?;
    symbol_helpers::emit_store_to_symbol(gen, &symbol, &vreg, &expr.identifier)?;
    Ok(())
}

pub fn handle_write_int(gen: &mut IRGenerator, stmt: &Write) -> Pl0Result<()> {
    let expr = stmt.expr.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("Write statement missing expression"))?;
    let vreg = ExpressionNode::accept(expr.as_ref(), gen)?;
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_write_int(&vreg)
}

pub fn handle_write_str(gen: &mut IRGenerator, stmt: &WriteStr) -> Pl0Result<()> {
    if stmt.expr.is_empty() {
        return Err(Pl0Error::codegen_error("WriteStr statement missing expression"));
    }
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_write_str(&stmt.expr)
}

pub fn handle_exit(gen: &mut IRGenerator) -> Pl0Result<()> {
    gen.exit_emitted = true;
    gen.system_exit(0)
}

pub fn handle_const_decl(gen: &mut IRGenerator, expr: &ConstDecl) -> Pl0Result<()> {
    for (id, num) in &expr.const_decl {
        let mut emitter = StringCodeEmitter::new(&mut gen.constants);
        emitter.emit_const(id, &num.to_string())?;
    }
    Ok(())
}

pub fn handle_var_decl(gen: &mut IRGenerator, expr: &VarDecl) -> Pl0Result<()> {
    for var_name in &expr.var_decl {
        if gen.scope.in_procedure() {
            let _ = gen.scope.allocate_variable();
        } else {
            let mut emitter = StringCodeEmitter::new(&mut gen.variables);
            emitter.emit_var(var_name)?;
        }
    }
    Ok(())
}