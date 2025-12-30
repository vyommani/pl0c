use std::collections::HashMap;
use crate::utils::Pl0Result;
use crate::utils::Pl0Error;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Identifier,
    Constant(i64),
    Variable,
    Procedure,
    StringLiteral,
    NumericLiteral,
}

#[derive(Debug, Clone)]
pub enum SymbolLocation {
    StackOffset(usize),
    GlobalLabel(String),
    Immediate(i64),
    None,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub symbol_type: SymbolType,
    pub line_number: usize,
    pub location: SymbolLocation,
    pub scope_level: usize,
    pub is_global: bool,
    pub initialized: bool,
    pub level: usize,
}

impl Symbol {
    pub fn new(symbol_type: SymbolType, line_number: usize, location: SymbolLocation, is_global: bool, level: usize) -> Self {
        Self {
            symbol_type,
            line_number,
            location,
            scope_level: 0,
            is_global,
            initialized: false,
            level,
        }
    }
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn drop_scope(&mut self) -> Result<(), Pl0Error> {
        if self.scopes.len() <= 1 {
            return Err(Pl0Error::codegen_error("Cannot drop global scope - scope underflow"));
        }
        self.scopes.pop();
        Ok(())
    }

    pub fn insert(&mut self, name: &str, symbol: Symbol) -> Pl0Result<()>{
        if name.is_empty() {
            return Err(Pl0Error::InvalidIdentifier {identifier: name.to_string(), line: symbol.line_number});
        }
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                return Err(Pl0Error::SymbolAlreadyDefined {name: name.to_string(),line: symbol.line_number});
            }
            scope.insert(name.to_string(), symbol);
        }
        Ok(())
    }

    // Get a reference to a symbol by name, searching from innermost to outermost scope.
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }

    pub fn get_at_level(&self, name: &str, level: usize) -> Option<&Symbol> {
        // Search from innermost to outermost, but only up to the given lexical level
        for (scope_idx, scope) in self.scopes.iter().enumerate().rev() {
            if scope_idx > level {
                continue;
            }
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    // Get a mutable reference to a symbol by name, searching from innermost to outermost scope.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(sym) = scope.get_mut(name) {
                return Some(sym);
            }
        }
        None
    }

    // Returns an iterator over all symbols in all scopes (innermost to outermost).
    pub fn all_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.scopes.iter().rev().flat_map(|scope| scope.values())
    }

    // Type check a symbol by name and expected type. Returns Ok(()) if found, Err otherwise.
    pub fn type_check(&self, name: &str, expected_type: &SymbolType, line_number: usize) -> Pl0Result<()> {
        let symbol = self.get(name).ok_or_else(|| Pl0Error::UndefinedSymbol {
            name: name.to_string(),
            line: line_number,
        })?;
        if symbol.symbol_type != *expected_type {
            return Err(Pl0Error::TypeMismatch {
                expected: format!("{:?}", expected_type),
                found: format!("{:?}", symbol.symbol_type),
                name: name.to_string(),
                line: line_number,
            });
        }
        Ok(())
    }

    pub fn get_scopes_len(&self) -> usize {
        self.scopes.len()
    }

    pub fn print_symbols(&self) {
        println!("{:-<80}", "");
        println!(
            "| {:<15} | {:<15} | {:<20} | {:<10} | {:<12} | {:<10} |",
            "Scope", "Name", "Type", "Location", "IsGlobal", "Initialized"
        );
        println!("{:-<80}", "");
        for (scope_idx, scope) in self.scopes.iter().enumerate() {
            if scope.is_empty() {
                println!("| {:<15} | (empty) {:<60} |", scope_idx, "");
            } else {
                for (name, symbol) in scope {
                    println!(
                        "| {:<15} | {:<15} | {:<20?} | {:<10?} | {:<12} | {:<10} |",
                        scope_idx,
                        name,
                        symbol.symbol_type,
                        symbol.location,
                        symbol.is_global,
                        symbol.initialized
                    );
                }
            }
        }
        println!("{:-<80}", "");
    }

    pub fn get_with_distance(&self, name: &str, current_level: usize) -> Option<(&Symbol, usize)> {
        //let mut distance = 0;
        for (_scope_idx, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(symbol) = scope.get(name) {
                // Distance = current_level - symbol.level
                let d = current_level.saturating_sub(symbol.level);
                return Some((symbol, d));
            }
            //distance += 1;
        }
        None
    }

    // Checks if a symbol with the given name exists in any scope.
    pub fn contains(&self, name: &str) -> bool {
        self.get(name).is_some()
    }
}
