use std::collections::HashMap;
use std::process::exit;

pub enum SymbolType {
    Identifier,
    Constant(i64),
    Variable,
    Procedure,
    StringLiteral,
    NumericLiteral,
}

pub struct Symbol {
    pub symbol_type: SymbolType,
    pub line_number: usize,
    pub address: Option<usize>,
}

impl Symbol {
    pub fn new(symbol_type: SymbolType, line_number: usize) -> Self {
        Self {
            symbol_type,
            line_number,
            address: None,
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

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }

    fn report_error(msg: &str) -> ! {
        println!("{}", msg);
        exit(1);
    }

    pub fn type_check(&self, name: &str, symbol_type: SymbolType, line_number: usize) {
        if self.scopes.is_empty() {
            Self::report_error(&format!("Invalid keyword: {}", name));
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
            Self::report_error(&msg);
        }
    }
}
