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
    pub is_global: bool,
    pub initialized: bool,
    pub level: usize,
    // For a Procedure symbol: the scope id of its own body, set once via
    // SymbolTable::set_body_scope right after that scope is created.
    pub body_scope: Option<usize>,
}

impl Symbol {
    pub fn new(symbol_type: SymbolType, line_number: usize, location: SymbolLocation, is_global: bool, level: usize) -> Self {
        Self {
            symbol_type,
            line_number,
            location,
            is_global,
            initialized: false,
            level,
            body_scope: None,
        }
    }
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
    parents: Vec<Option<usize>>,
    active: Vec<usize>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            parents: vec![None],
            active: vec![0],
        }
    }

    /// The scope `insert` and `get` currently resolve against.
    pub fn current_scope(&self) -> usize {
        *self.active.last().unwrap_or(&0)
    }

    /// How many scopes are currently active (i.e. how deeply nested we are).
    pub fn depth(&self) -> usize {
        self.active.len()
    }

    /// Creates a child of the current scope, enters it, and returns its id.
    pub fn push_scope(&mut self) -> usize {
        let id = self.scopes.len();
        self.scopes.push(HashMap::new());
        self.parents.push(Some(self.current_scope()));
        self.active.push(id);
        id
    }

    /// Leaves the current scope, returning to its parent.
    pub fn pop_scope(&mut self) -> Pl0Result<()> {
        if self.active.len() <= 1 {
            return Err(Pl0Error::codegen_error("Cannot drop global scope - scope underflow"));
        }
        self.active.pop();
        Ok(())
    }

    pub fn enter_scope(&mut self, id: usize) {
        let mut chain = Vec::new();
        let mut current = Some(id);
        while let Some(idx) = current {
            chain.push(idx);
            current = self.parents[idx];
        }
        chain.reverse();
        self.active = chain;
    }

    /// Records `name` (declared in `owner_scope`) as owning `body_scope` -
    /// used for procedures, whose own body gets a fresh child scope that IR
    /// generation must be able to find again later via `enter_scope`.
    pub fn set_body_scope(&mut self, owner_scope: usize, name: &str, body_scope: usize) {
        if let Some(symbol) = self.scopes[owner_scope].get_mut(name) {
            symbol.body_scope = Some(body_scope);
        }
    }

    /// Inserts `symbol` into the current scope. Fails on an empty name or a
    /// redefinition within that same scope; shadowing an outer scope is fine.
    pub fn insert(&mut self, name: &str, symbol: Symbol) -> Pl0Result<()> {
        if name.is_empty() {
            return Err(Pl0Error::InvalidIdentifier { identifier: name.to_string(), line: symbol.line_number });
        }
        let current = self.current_scope();
        let scope = &mut self.scopes[current];
        if scope.contains_key(name) {
            return Err(Pl0Error::SymbolAlreadyDefined { name: name.to_string(), line: symbol.line_number });
        }
        scope.insert(name.to_string(), symbol);
        Ok(())
    }

    /// Looks `name` up starting at the current scope and walking outward.
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.active.iter().rev().find_map(|&idx| self.scopes[idx].get(name))
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        let idx = self.active.iter().rev().copied().find(|&idx| self.scopes[idx].contains_key(name))?;
        self.scopes[idx].get_mut(name)
    }

    /// How many static-link hops separate the current scope from `ancestor`
    /// (0 if `ancestor` is the current scope itself).
    pub fn distance_to(&self, ancestor: usize) -> usize {
        match self.active.iter().rposition(|&idx| idx == ancestor) {
            Some(pos) => self.active.len() - 1 - pos,
            None => self.active.len(),
        }
    }

    /// Looks `name` up and checks it has `expected_type`.
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
}
