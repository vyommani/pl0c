use crate::ast::Exit;
use crate::ast::Variable;
use crate::block::Block;
use crate::decl::ConstDecl;
use crate::decl::ProcDecl;
use crate::decl::VarDecl;
use crate::expression::BinOp;
use crate::expression::OddCondition;
use crate::expression::RelationalCondition;
use crate::io::ReadChar;
use crate::io::ReadInt;
use crate::io::WriteChar;
use crate::io::WriteInt;
use crate::io::WriteStr;
use crate::program::Program;
use crate::statement::AssignStmt;
use crate::statement::BeginStmt;
use crate::statement::CallStmt;
use crate::statement::IfStmt;
use crate::statement::WhileStatement;
use crate::types::Ident;
use crate::types::Number;

pub trait ASTVisitor {
    fn visit_ident(&mut self, ident: &Ident) -> Result<String, String>;
    fn visit_number(&mut self, number: &Number) -> Result<String, String>;
    fn visit_variable(&mut self, variable: &Variable) -> Result<String, String>;
    fn visit_binary_operation(&mut self, binary_operation: &BinOp) -> Result<String, String>;
    fn visit_while_statement(&mut self, while_statement: &WhileStatement) -> Result<(), String>;
    fn visit_condition(&mut self, condition: &OddCondition) -> Result<String, String>;
    fn visit_relational_condition(&mut self, condition: &RelationalCondition)
        -> Result<String, String>;
    fn visit_call(&mut self, condition: &CallStmt) -> Result<(), String>;
    fn visit_assign(&mut self, expr: &AssignStmt) -> Result<(), String>;
    fn visit_begin(&mut self, expr: &BeginStmt) -> Result<(), String>;
    fn visit_if(&mut self, expr: &IfStmt) -> Result<(), String>;
    fn visit_write_int(&mut self, expr: &WriteInt) -> Result<(), String>;
    fn visit_write_char(&mut self, expr: &WriteChar) -> Result<(), String>;
    fn visit_write_str(&mut self, expr: &WriteStr) -> Result<(), String>;
    fn visit_read_int(&mut self, expr: &ReadInt) -> Result<(), String>;
    fn visit_read_char(&mut self, expr: &ReadChar) -> Result<(), String>;
    fn visit_exit(&mut self, expr: &Exit) -> Result<(), String>;
    fn visit_const(&mut self, expr: &ConstDecl) -> Result<(), String>;
    fn visit_var_decl(&mut self, expr: &VarDecl) -> Result<(), String>;
    fn visit_proc_decl(&mut self, expr: &ProcDecl) -> Result<(), String>;
    fn visit_block(&mut self, expr: &Block) -> Result<(), String>;
    fn visit_program(&mut self, expr: &Program) -> Result<(), String>;
}
