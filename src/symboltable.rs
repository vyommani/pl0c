use std::collections::HashMap;

#[derive(Debug, Clone)]
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
    StackOffset(isize),
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
    pub level: usize, // lexical nesting level
}

impl Symbol {
    pub fn new(
        symbol_type: SymbolType,
        line_number: usize,
        location: SymbolLocation,
        is_global: bool,
        level: usize,
    ) -> Self {
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

    pub fn drop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: &str, symbol: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), symbol);
        }
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
    pub fn type_check(
        &self,
        name: &str,
        symbol_type: SymbolType,
        line_number: usize,
    ) -> Result<(), String> {
        if self.scopes.is_empty() {
            return Err(format!("Invalid keyword: {}", name));
        }
        let found = self.get(name).is_some();
        if !found {
            let msg = match symbol_type {
                SymbolType::Constant(_) => format!(
                    "error: constant '{}' is not declared at line: {}",
                    name, line_number
                ),
                SymbolType::Identifier => format!(
                    "error: Identifier '{}' is not declared at line: {}",
                    name, line_number
                ),
                SymbolType::Variable => format!(
                    "error: variable '{}' is not declared at line: {}",
                    name, line_number
                ),
                SymbolType::Procedure => format!("error: procedure '{}' is not declared.", name),
                SymbolType::StringLiteral => format!(
                    "error: string literal '{}' is not declared at line: {}",
                    name, line_number
                ),
                SymbolType::NumericLiteral => format!(
                    "error: numeric literal '{}' is not declared at line: {}",
                    name, line_number
                ),
            };
            return Err(msg);
        }
        Ok(())
    }

    pub fn get_scopes_len(&self) -> usize {
        self.scopes.len()
    }

    pub fn print_symbols(&self) {
        println!("Symbol Table Contents:");
        for (scope_idx, scope) in self.scopes.iter().enumerate() {
            println!("Scope {}:", scope_idx);
            if scope.is_empty() {
                println!("  (empty)");
            } else {
                for (name, symbol) in scope {
                    println!(
                        "  Name: {}, Type: {:?}, Location: {:?}, IsGlobal: {}, Initialized: {}, ScopeLevel: {}, Level: {}",
                        name,
                        symbol.symbol_type,
                        symbol.location,
                        symbol.is_global,
                        symbol.initialized,
                        symbol.scope_level,
                        symbol.level
                    );
                }
            }
        }
    }

    pub fn get_with_distance(&self, name: &str, current_level: usize) -> Option<(&Symbol, usize)> {
        let mut distance = 0;
        for (scope_idx, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(symbol) = scope.get(name) {
                // Distance = current_level - symbol.level
                let d = current_level.saturating_sub(symbol.level);
                return Some((symbol, d));
            }
            distance += 1;
        }
        None
    }
}
