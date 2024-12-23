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
    name: String,
    symbol_type: SymbolType,
    line_number: usize,
}

impl Symbol {
    pub fn new(name: String, symbol_type: SymbolType, line_number: usize) -> Self {
        Self {
            name,
            symbol_type,
            line_number,
        }
    }
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
    current_scope: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            current_scope: 0,
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.current_scope += 1;
    }

    pub fn drop_scope(&mut self) {
        if self.scopes.len() >= self.current_scope {
            self.scopes.pop();
            self.current_scope -= 1;
        }
    }

    pub fn insert(&mut self, name: String, symbol: Symbol) {
        if self.scopes.len() <= self.current_scope {
            self.scopes.push(HashMap::new());
        }
        self.scopes[self.current_scope].insert(name, symbol);
    }

    pub fn get(&mut self, name: String) -> Option<&Symbol> {
        let value = self.scopes[self.current_scope].get(&name);
        match value {
            Some(_) => {
                return value;
            }
            None => {
                // search the outer scopes as well.
                let mut count = 0;
                for scope in &self.scopes {
                    if scope.contains_key(&name.clone()) {
                        return scope.get(&name.clone());
                    }
                    count += 1;
                    if count == self.current_scope {
                        break;
                    }
                }
                None
            }
        }
    }

    pub fn type_check(&mut self, name: String, symbol_type: SymbolType, line_number: usize) {
        // if there is no symbol in symbol table and if control reached here it means we have wrong keyword
        if self.scopes.len() == 0 {
            println!("Invalid keyword:{}", name);
            exit(1);
        }
        match symbol_type {
            SymbolType::Constant(_) => {
                if let Some(_) = self.get(name.to_string()) {
                    // do nothing
                } else {
                    println!(
                        "error: constant '{}' is not declared at line: {} ",
                        name, line_number
                    );
                }
            }
            SymbolType::Identifier => {
                if let Some(_) = self.get(name.to_string()) {
                } else {
                    println!(
                        "error: Identifier '{}' is not declared at line: {} ",
                        name, line_number
                    );
                    exit(1);
                }
            }
            SymbolType::Variable => {
                if let Some(_) = self.get(name.to_string()) {
                } else {
                    println!(
                        "error: variable '{}' is not declared at line: {} ",
                        name, line_number
                    );
                    exit(1);
                }
            }
            SymbolType::Procedure => {
                if let Some(_) = self.get(name.to_string()) {
                } else {
                    println!("error: procedure '{}' is not declared.", name);
                    exit(1);
                }
            }
            _ => {} // do nothing for now
        }
    }
}
