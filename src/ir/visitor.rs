use crate::{utils::errors::Pl0Result, ast::Program, semantic::visiters::ASTVisitor};
use super::{IRGenerator, expression_handlers, statement_handlers, procedure_handlers};

impl ASTVisitor for IRGenerator {
    fn visit_program(&mut self, expr: &Program) -> Pl0Result<()> {
        if let Some(block) = &expr.block {
            block.accept(self)?;
        }
        Ok(())
    }

    // Expression handlers
    fn visit_ident(&mut self, ident: &crate::ast::Ident) -> Pl0Result<String> {
        expression_handlers::handle_ident(self, ident)
    }

    fn visit_number(&mut self, number: &crate::ast::Number) -> Pl0Result<String> {
        expression_handlers::handle_number(self, number)
    }

    fn visit_variable(&mut self, variable: &crate::ast::Variable) -> Pl0Result<String> {
        expression_handlers::handle_variable(self, variable)
    }

    fn visit_binary_operation(&mut self, binop: &crate::ast::BinOp) -> Pl0Result<String> {
        expression_handlers::handle_binary_operation(self, binop)
    }

    fn visit_condition(&mut self, cond: &crate::ast::OddCondition) -> Pl0Result<String> {
        expression_handlers::handle_odd_condition(self, cond)
    }

    fn visit_relational_condition(&mut self, cond: &crate::ast::RelationalCondition) -> Pl0Result<String> {
        expression_handlers::handle_relational_condition(self, cond)
    }

    // Statement handlers
    fn visit_assign(&mut self, stmt: &crate::ast::AssignStmt) -> Pl0Result<()> {
        statement_handlers::handle_assign(self, stmt)
    }

    fn visit_call(&mut self, call: &crate::ast::CallStmt) -> Pl0Result<()> {
        statement_handlers::handle_call(self, call)
    }

    fn visit_begin(&mut self, expr: &crate::ast::BeginStmt) -> Pl0Result<()> {
        statement_handlers::handle_begin(self, expr)
    }

    fn visit_if(&mut self, expr: &crate::ast::IfStmt) -> Pl0Result<()> {
        statement_handlers::handle_if(self, expr)
    }

    fn visit_while_statement(&mut self, stmt: &crate::ast::WhileStatement) -> Pl0Result<()> {
        statement_handlers::handle_while(self, stmt)
    }

    fn visit_read_int(&mut self, expr: &crate::ast::Read) -> Pl0Result<()> {
        statement_handlers::handle_read_int(self, expr)
    }

    fn visit_write_int(&mut self, stmt: &crate::ast::Write) -> Pl0Result<()> {
        statement_handlers::handle_write_int(self, stmt)
    }

    fn visit_write_str(&mut self, stmt: &crate::ast::WriteStr) -> Pl0Result<()> {
        statement_handlers::handle_write_str(self, stmt)
    }

    fn visit_exit(&mut self, _expr: &crate::ast::Exit) -> Pl0Result<()> {
        statement_handlers::handle_exit(self)
    }

    // Declaration handlers
    fn visit_const(&mut self, expr: &crate::ast::ConstDecl) -> Pl0Result<()> {
        statement_handlers::handle_const_decl(self, expr)
    }

    fn visit_var_decl(&mut self, expr: &crate::ast::VarDecl) -> Pl0Result<()> {
        statement_handlers::handle_var_decl(self, expr)
    }

    fn visit_proc_decl(&mut self, expr: &crate::ast::ProcDecl) -> Pl0Result<()> {
        procedure_handlers::handle_proc_decl(self, expr)
    }

    fn visit_block(&mut self, block: &crate::ast::Block) -> Pl0Result<()> {
        procedure_handlers::handle_block(self, block)
    }
}
