use crate::{
    ast::Node, semantic::symboltable::SymbolTable, utils::errors::{Pl0Result, Pl0Error},
    ir::code_emitter::{CodeEmitter, StringCodeEmitter},
};
use crate::semantic::scope_info::ScopeInfo;

pub struct IRGenerator {
    pub(crate) label_counter: i32,
    pub(crate) vreg_counter: i32,
    pub(crate) constants: String,
    pub(crate) variables: String,
    pub(crate) code: String,
    pub(crate) vreg_prefix: String,
    pub(crate) symbol_table: SymbolTable,
    pub(crate) exit_emitted: bool,
    pub(crate) main_emitted: bool,
    pub(crate) procedures_emitted: bool,
    pub(crate) scope: ScopeInfo,
}

impl IRGenerator {
    pub fn new(table: SymbolTable) -> Self {
        Self {
            symbol_table: table,
            label_counter: 0,
            vreg_counter: 0,
            constants: String::with_capacity(1024),
            variables: String::with_capacity(256),
            code: String::with_capacity(4096),
            vreg_prefix: "v".to_string(),
            exit_emitted: false,
            main_emitted: false,
            procedures_emitted: false,
            scope: ScopeInfo::new(),
        }
    }

    pub fn allocate_virtual_register(&mut self) -> String {
        let vreg = format!("{}{}", self.vreg_prefix, self.vreg_counter);
        self.vreg_counter += 1;
        vreg
    }

    pub fn create_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    pub fn get_output(&self) -> String {
        let mut output = String::with_capacity(
            self.constants.len() + self.variables.len() + self.code.len() + 10
        );
        if !self.constants.is_empty() {
            output.push_str(&self.constants);
            if !self.constants.ends_with('\n') {
                output.push('\n');
            }
        }
        if !self.variables.is_empty() {
            output.push_str(&self.variables);
            if !self.variables.ends_with('\n') {
                output.push('\n');
            }
        }
        output.push_str(&self.code);
        output
    }

    pub fn generate_code(&mut self, ast: Option<Box<dyn Node + 'static>>) -> Pl0Result<()> {
        self.exit_emitted = false;
        self.main_emitted = false;
        self.procedures_emitted = false;
        let ast = ast.ok_or_else(|| Pl0Error::codegen_error("No AST provided for code generation"))?;
        ast.accept(self)?;
        if !self.exit_emitted {
            self.system_exit(0)?;
        }
        Ok(())
    }

    pub fn system_exit(&mut self, code: i32) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_exit(code)
    }
}