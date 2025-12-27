use super::IRGenerator;
use crate::{
    semantic::symboltable::{Symbol, SymbolLocation, SymbolType},
    errors::{Pl0Result, Pl0Error},
    ir::code_emitter::{CodeEmitter, StringCodeEmitter},
};

pub fn get_symbol_with_type<'a>(
    gen: &'a IRGenerator,
    name: &str,
    expected_type: SymbolType,
    operation: &str
) -> Pl0Result<&'a Symbol> {
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
    gen: &'a IRGenerator,
    name: &str,
    operation: &str
) -> Pl0Result<&'a Symbol> {
    let symbol = gen.symbol_table.get_at_level(name, gen.scope.level())
        .ok_or_else(|| Pl0Error::codegen_error(format!("Undefined {}: {}", operation, name)))?;
    if !matches!(symbol.symbol_type, SymbolType::Variable) {
        return Err(Pl0Error::codegen_error(
            format!("Expected variable but found {:?}: {}", symbol.symbol_type, name)
        ));
    }
    Ok(symbol)
}

pub fn get_procedure_symbol<'a>(
    gen: &'a IRGenerator,
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
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    let distance = gen.scope.level().saturating_sub(symbol.level);
    match &symbol.location {
        SymbolLocation::StackOffset(offset) => {
            if distance > 0 {
                emitter.emit_ld(target_vreg, &format!("up-{}-{}", offset, distance))
            } else {
                emitter.emit_ld(target_vreg, &format!("bp-{}", offset))
            }
        }
        SymbolLocation::GlobalLabel(label) => emitter.emit_ld(target_vreg, label),
        SymbolLocation::Immediate(value) => emitter.emit_li(target_vreg, &value.to_string()),
        SymbolLocation::None => emitter.emit_ld(target_vreg, fallback_name),
    }
}

pub fn emit_store_to_symbol(
    gen: &mut IRGenerator,
    symbol: &Symbol,
    source_vreg: &str,
    fallback_name: &str
) -> Pl0Result<()> {
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    let distance = gen.scope.level().saturating_sub(symbol.level);
    match &symbol.location {
        SymbolLocation::StackOffset(offset) => {
            if distance > 0 {
                emitter.emit_st(&format!("up-{}-{}", offset, distance), source_vreg)
            } else {
                emitter.emit_st(&format!("bp-{}", offset), source_vreg)
            }
        }
        SymbolLocation::GlobalLabel(label) => emitter.emit_st(label, source_vreg),
        SymbolLocation::None => emitter.emit_st(fallback_name, source_vreg),
        SymbolLocation::Immediate(_) => {
            Err(Pl0Error::codegen_error(format!("Cannot store to immediate value: {}", fallback_name)))
        }
    }
}