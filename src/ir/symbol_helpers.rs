use super::IRGenerator;
use super::ir_emitter;
use super::tac::MemLoc;
use crate::{
    semantic::symboltable::{Symbol, SymbolLocation, SymbolType},
    utils::errors::{Pl0Result, Pl0Error},
};

pub fn get_symbol_with_type<'a>(
    gen: &'a mut IRGenerator,
    name: &str,
    expected_type: SymbolType,
    operation: &str
) -> Pl0Result<&'a Symbol> {
    gen.symbol_table.enter_scope(gen.scope.level());
    let symbol = gen.symbol_table.get(name)
        .ok_or_else(|| Pl0Error::codegen_error(format!("Undefined {}: {}", operation, name)))?;
    match (&symbol.symbol_type, &expected_type) {
        (SymbolType::Variable, SymbolType::Variable) |
        (SymbolType::Procedure, SymbolType::Procedure) => Ok(symbol),
        (SymbolType::Constant(_), SymbolType::Constant(_)) => Ok(symbol),
        _ => Err(Pl0Error::codegen_error(
            format!("Expected {:?} but found {:?} for {}", expected_type, symbol.symbol_type, name)
        ))
    }
}

pub fn get_variable_symbol<'a>(
    gen: &'a mut IRGenerator,
    name: &str,
    operation: &str
) -> Pl0Result<&'a Symbol> {
    gen.symbol_table.enter_scope(gen.scope.level());
    let symbol = gen.symbol_table.get(name)
        .ok_or_else(|| Pl0Error::codegen_error(format!("Undefined {}: {}", operation, name)))?;
    if !matches!(symbol.symbol_type, SymbolType::Variable) {
        return Err(Pl0Error::codegen_error(
            format!("Expected variable but found {:?}: {}", symbol.symbol_type, name)
        ));
    }
    Ok(symbol)
}

pub fn get_procedure_symbol<'a>(
    gen: &'a mut IRGenerator,
    name: &str,
    operation: &str
) -> Pl0Result<&'a Symbol> {
    get_symbol_with_type(gen, name, SymbolType::Procedure, operation)
}

pub fn get_or_load_variable(gen: &mut IRGenerator, variable_name: &str) -> Pl0Result<String> {
    // Always load from memory for correctness
    let symbol = get_variable_symbol(gen, variable_name, "variable")?.clone();
    let vreg = gen.allocate_virtual_register();
    emit_load_from_symbol(gen, &symbol, &vreg, variable_name)?;
    Ok(vreg)
}

pub fn emit_load_from_symbol(
    gen: &mut IRGenerator,
    symbol: &Symbol,
    target_vreg: &str,
    fallback_name: &str
) -> Pl0Result<()> {
    gen.symbol_table.enter_scope(gen.scope.level());
    let distance = gen.symbol_table.distance_to(symbol.level);
    match &symbol.location {
        SymbolLocation::StackOffset(offset) => {
            let loc = if distance > 0 {
                MemLoc::Outer { offset: *offset, distance }
            } else {
                MemLoc::Local { offset: *offset }
            };
            ir_emitter::emit_ld(gen, target_vreg, loc)
        }
        SymbolLocation::GlobalLabel(label) => {
            ir_emitter::emit_ld(gen, target_vreg, MemLoc::Global(label.clone()))
        }
        SymbolLocation::Immediate(value) => ir_emitter::emit_li(gen, target_vreg, *value),
        SymbolLocation::None => {
            ir_emitter::emit_ld(gen, target_vreg, MemLoc::Global(fallback_name.to_string()))
        }
    }
}

pub fn emit_store_to_symbol(
    gen: &mut IRGenerator,
    symbol: &Symbol,
    source_vreg: &str,
    fallback_name: &str
) -> Pl0Result<()> {
    gen.symbol_table.enter_scope(gen.scope.level());
    let distance = gen.symbol_table.distance_to(symbol.level);
    match &symbol.location {
        SymbolLocation::StackOffset(offset) => {
            let loc = if distance > 0 {
                MemLoc::Outer { offset: *offset, distance }
            } else {
                MemLoc::Local { offset: *offset }
            };
            ir_emitter::emit_st(gen, loc, source_vreg)
        }
        SymbolLocation::GlobalLabel(label) => {
            ir_emitter::emit_st(gen, MemLoc::Global(label.clone()), source_vreg)
        }
        SymbolLocation::None => {
            ir_emitter::emit_st(gen, MemLoc::Global(fallback_name.to_string()), source_vreg)
        }
        SymbolLocation::Immediate(_) => {
            Err(Pl0Error::codegen_error(format!("Cannot store to immediate value: {}", fallback_name)))
        }
    }
}