pub mod expression_handlers;
pub mod statement_handlers;
pub mod procedure_handlers;
pub mod symbol_helpers;
pub mod ir_emitter;
pub mod stack_manager;

use crate::{
    ast::Node,
    symboltable::SymbolTable,
    errors::{Pl0Result, Pl0Error},
    program::Program,
    visiters::ASTVisitor,
    code_emitter::{CodeEmitter, StringCodeEmitter},
};
use crate::scope_info::ScopeInfo;

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

    fn system_exit(&mut self, code: i32) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_exit(code)
    }
}

impl ASTVisitor for IRGenerator {
    fn visit_program(&mut self, expr: &Program) -> Pl0Result<()> {
        if let Some(block) = &expr.block {
            block.accept(self)?;
        }
        Ok(())
    }

    // Expression handlers
    fn visit_ident(&mut self, ident: &crate::types::Ident) -> Pl0Result<String> {
        expression_handlers::handle_ident(self, ident)
    }

    fn visit_number(&mut self, number: &crate::types::Number) -> Pl0Result<String> {
        expression_handlers::handle_number(self, number)
    }

    fn visit_variable(&mut self, variable: &crate::ast::Variable) -> Pl0Result<String> {
        expression_handlers::handle_variable(self, variable)
    }

    fn visit_binary_operation(&mut self, binop: &crate::expression::BinOp) -> Pl0Result<String> {
        expression_handlers::handle_binary_operation(self, binop)
    }

    fn visit_condition(&mut self, cond: &crate::expression::OddCondition) -> Pl0Result<String> {
        expression_handlers::handle_odd_condition(self, cond)
    }

    fn visit_relational_condition(&mut self, cond: &crate::expression::RelationalCondition) -> Pl0Result<String> {
        expression_handlers::handle_relational_condition(self, cond)
    }

    // Statement handlers
    fn visit_assign(&mut self, stmt: &crate::statement::AssignStmt) -> Pl0Result<()> {
        statement_handlers::handle_assign(self, stmt)
    }

    fn visit_call(&mut self, call: &crate::statement::CallStmt) -> Pl0Result<()> {
        statement_handlers::handle_call(self, call)
    }

    fn visit_begin(&mut self, expr: &crate::statement::BeginStmt) -> Pl0Result<()> {
        statement_handlers::handle_begin(self, expr)
    }

    fn visit_if(&mut self, expr: &crate::statement::IfStmt) -> Pl0Result<()> {
        statement_handlers::handle_if(self, expr)
    }

    fn visit_while_statement(&mut self, stmt: &crate::statement::WhileStatement) -> Pl0Result<()> {
        statement_handlers::handle_while(self, stmt)
    }

    fn visit_read_int(&mut self, expr: &crate::io::Read) -> Pl0Result<()> {
        statement_handlers::handle_read_int(self, expr)
    }

    fn visit_write_int(&mut self, stmt: &crate::io::Write) -> Pl0Result<()> {
        statement_handlers::handle_write_int(self, stmt)
    }

    fn visit_write_str(&mut self, stmt: &crate::io::WriteStr) -> Pl0Result<()> {
        statement_handlers::handle_write_str(self, stmt)
    }

    fn visit_exit(&mut self, _expr: &crate::ast::Exit) -> Pl0Result<()> {
        statement_handlers::handle_exit(self)
    }

    // Declaration handlers
    fn visit_const(&mut self, expr: &crate::decl::ConstDecl) -> Pl0Result<()> {
        statement_handlers::handle_const_decl(self, expr)
    }

    fn visit_var_decl(&mut self, expr: &crate::decl::VarDecl) -> Pl0Result<()> {
        statement_handlers::handle_var_decl(self, expr)
    }

    fn visit_proc_decl(&mut self, expr: &crate::decl::ProcDecl) -> Pl0Result<()> {
        procedure_handlers::handle_proc_decl(self, expr)
    }

    fn visit_block(&mut self, block: &crate::block::Block) -> Pl0Result<()> {
        procedure_handlers::handle_block(self, block)
    }
}