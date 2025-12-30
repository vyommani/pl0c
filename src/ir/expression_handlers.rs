use super::IRGenerator;
use crate::{
    ast::{ExpressionNode, BinOp, OddCondition, RelationalCondition, Ident, Number, Variable},
    utils::errors::{Pl0Result, Pl0Error},
    ir::code_emitter::{CodeEmitter, StringCodeEmitter},
    semantic::symboltable::{SymbolType, SymbolLocation},
};

pub fn handle_ident(gen: &mut IRGenerator, ident: &Ident) -> Pl0Result<String> {
    use super::symbol_helpers;
    
    let symbol = gen.symbol_table.get_at_level(&ident.value, gen.scope.level())
        .ok_or_else(|| Pl0Error::codegen_error(format!("Undefined identifier: {}", ident.value)))?.clone();
    
    match symbol.symbol_type {
        SymbolType::Constant(value) => {
            let vreg = gen.allocate_virtual_register();
            let mut emitter = StringCodeEmitter::new(&mut gen.code);
            emitter.emit_li(&vreg, &value.to_string())?;
            Ok(vreg)
        }
        SymbolType::Procedure => {
            let vreg = gen.allocate_virtual_register();
            match &symbol.location {
                SymbolLocation::GlobalLabel(label) => {
                    let mut emitter = StringCodeEmitter::new(&mut gen.code);
                    emitter.emit(&format!("la {}, {}", vreg, label))?;
                }
                _ => return Err(Pl0Error::codegen_error(format!("Procedure {} has no valid address", ident.value))),
            }
            Ok(vreg)
        }
        SymbolType::Variable => {
            let vreg = symbol_helpers::get_or_load_variable(gen, &ident.value)?;
            if !vreg.starts_with(&gen.vreg_prefix) {
                return Err(Pl0Error::codegen_error(format!("Invalid register format: {}", vreg)));
            }
            Ok(vreg)
        }
        _ => {
            let vreg = gen.allocate_virtual_register();
            symbol_helpers::emit_load_from_symbol(gen, &symbol, &vreg, &ident.value)?;
            Ok(vreg)
        }
    }
}

pub fn handle_number(gen: &mut IRGenerator, number: &Number) -> Pl0Result<String> {
    let vreg = gen.allocate_virtual_register();
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_li(&vreg, &number.value.to_string())?;
    Ok(vreg)
}

pub fn handle_variable(gen: &mut IRGenerator, variable: &Variable) -> Pl0Result<String> {
    use super::symbol_helpers;
    symbol_helpers::get_or_load_variable(gen, &variable.name)
}

pub fn handle_binary_operation(gen: &mut IRGenerator, binop: &BinOp) -> Pl0Result<String> {
    let left_expr = binop.left.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("Binary operation missing left operand"))?;
    let right_expr = binop.right.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("Binary operation missing right operand"))?;
    
    // Try constant folding
    if let (Some(left), Some(right)) = (
        fold_constant_expression(gen, left_expr),
        fold_constant_expression(gen, right_expr)
    ) {
        let result = match binop.operator.as_str() {
            "Plus" => left + right,
            "Minus" => left - right,
            "Multiply" => left * right,
            "Divide" => left / right,
            "Modulo" => left % right,
            _ => return Err(Pl0Error::codegen_error(format!("Unknown operator: {}", binop.operator))),
        };
        let vreg = gen.allocate_virtual_register();
        let mut emitter = StringCodeEmitter::new(&mut gen.code);
        emitter.emit_li(&vreg, &result.to_string())?;
        return Ok(vreg);
    }
    
    emit_binary_operation(gen, left_expr.as_ref(), right_expr.as_ref(), &binop.operator)
}

pub fn handle_odd_condition(gen: &mut IRGenerator, cond: &OddCondition) -> Pl0Result<String> {
    let expr = cond.expr.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("OddCondition missing expression"))?;
    let num_reg = ExpressionNode::accept(expr.as_ref(), gen)?;
    let result_reg = gen.allocate_virtual_register();
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_is_odd(&result_reg, &num_reg)?;
    Ok(result_reg)
}

pub fn handle_relational_condition(gen: &mut IRGenerator, cond: &RelationalCondition) -> Pl0Result<String> {
    let left = cond.left.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("Relational condition missing left operand"))?;
    let right = cond.right.as_ref()
        .ok_or_else(|| Pl0Error::codegen_error("Relational condition missing right operand"))?;
    emit_relational_operation(gen, left.as_ref(), right.as_ref(), &cond.operator)
}

// Helper functions
fn fold_constant_expression(gen: &mut IRGenerator, expr: &Box<dyn ExpressionNode>) -> Option<i64> {
    let expr = expr.as_ref();
    if let Some(num) = expr.as_any().downcast_ref::<Number>() {
        return Some(num.value);
    }
    if let Some(ident) = expr.as_any().downcast_ref::<Ident>() {
        if let Some(symbol) = gen.symbol_table.get(&ident.value) {
            if let SymbolType::Constant(value) = symbol.symbol_type {
                return Some(value);
            }
        }
    }
    None
}

fn emit_binary_operation(
    gen: &mut IRGenerator,
    left_expr: &dyn ExpressionNode,
    right_expr: &dyn ExpressionNode,
    operator: &str
) -> Pl0Result<String> {
    use super::ir_emitter;
    
    let left_result = ExpressionNode::accept(left_expr, gen)?;
    let right_result = ExpressionNode::accept(right_expr, gen)?;
    let result_vreg = gen.allocate_virtual_register();
    
    let op = match operator {
        "Plus" => "add",
        "Minus" => "sub",
        "Multiply" => "mul",
        "Divide" => "div",
        "Modulo" => "mod",
        _ => return Err(Pl0Error::codegen_error(format!("Unknown operator: {}", operator))),
    };
    
    ir_emitter::emit_binary_op(gen, op, &result_vreg, &left_result, &right_result)?;
    Ok(result_vreg)
}

fn emit_relational_operation(
    gen: &mut IRGenerator,
    left_expr: &dyn ExpressionNode,
    right_expr: &dyn ExpressionNode,
    operator: &str
) -> Pl0Result<String> {
    use super::ir_emitter;
    
    let left_result = ExpressionNode::accept(left_expr, gen)?;
    let right_result = ExpressionNode::accept(right_expr, gen)?;
    let result_vreg = gen.allocate_virtual_register();
    
    let op = match operator {
        "GreaterThan" => "cmp_gt",
        "LessThan" => "cmp_lt",
        "Equal" => "cmp_eq",
        "GreaterThanEqual" => "cmp_ge",
        "LessThanEqual" => "cmp_le",
        "!=" | "Hash" => "cmp_ne",
        _ => return Err(Pl0Error::codegen_error(format!("Unknown relational operator: {}", operator))),
    };
    
    ir_emitter::emit_relational_op(gen, op, &result_vreg, &left_result, &right_result)?;
    Ok(result_vreg)
}