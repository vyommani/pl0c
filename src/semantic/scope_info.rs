use crate::utils::{errors::Pl0Error, errors::Pl0Result};

#[derive(Debug, Clone)]
pub struct ScopeInfo {
    current_level: usize,
    in_procedure: bool,
    is_in_main: bool,
    parent: Option<Box<ScopeInfo>>,
}
impl ScopeInfo {
    pub fn new() -> Self {
        ScopeInfo {
            current_level: 0,
            in_procedure: false,
            is_in_main: true,
            parent: None,
        }
    }

    pub fn push_scope(&self, in_procedure: bool, parent_level: Option<usize>, is_main_block: bool) -> Self {
        let new_level = if is_main_block {
            0
        } else {
            parent_level.unwrap_or(self.current_level + 1)
        };
        ScopeInfo {
            current_level: new_level,
            in_procedure,
            is_in_main: is_main_block,
            parent: Some(Box::new(self.clone())),
        }
    }

    pub fn pop_scope(&mut self) -> Pl0Result<()> {
        if self.is_in_main {
            return Err(Pl0Error::codegen_error("Cannot pop main block scope".to_string()));
        }
        if let Some(parent) = self.parent.take() {
            *self = *parent;
            Ok(())
        } else {
            Err(Pl0Error::codegen_error("No parent scope to restore".to_string()))
        }
    }

    pub fn level(&self) -> usize {
        self.current_level
    }

    pub fn in_procedure(&self) -> bool {
        self.in_procedure
    }

    pub fn is_in_main(&self) -> bool {
        self.is_in_main
    }
}