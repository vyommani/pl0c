use crate::utils::{errors::Pl0Error, errors::Pl0Result, config::parser::INITIAL_STACK_OFFSET};
use crate::utils::config::codegen::WORD_SIZE;

#[derive(Debug, Clone)]
pub struct ScopeInfo {
    current_level: usize,
    local_var_offset: usize,
    in_procedure: bool,
    is_in_main: bool,
    stack_slots: usize,
    parent: Option<Box<ScopeInfo>>,
}
impl ScopeInfo {
    pub fn new() -> Self {
        ScopeInfo {
            current_level: 0,
            local_var_offset: INITIAL_STACK_OFFSET,
            in_procedure: false,
            is_in_main: true,
            stack_slots: 0,
            parent: None,
        }
    }

    pub fn push_scope(&self, in_procedure: bool, parent_level: Option<usize>, is_main_block: bool) -> Self {
        let new_level = if is_main_block {
            0
        } else {
            parent_level.unwrap_or(self.current_level + 1)
        };
        let new_scope = ScopeInfo {
            current_level: new_level,
            local_var_offset: if in_procedure || is_main_block {
                INITIAL_STACK_OFFSET
            } else {
                self.local_var_offset
            },
            in_procedure,
            is_in_main: is_main_block,
            stack_slots: 0,
            parent: Some(Box::new(self.clone())),
        };
        new_scope
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

    pub fn allocate_variable(&mut self) -> usize {
        let offset = self.local_var_offset;
        self.local_var_offset += WORD_SIZE;
        self.stack_slots += 1;
        offset
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