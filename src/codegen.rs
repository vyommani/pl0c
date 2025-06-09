use crate::{
    ast::{Exit, Node, Variable},
    block::Block,
    decl::{ConstDecl, ProcDecl, VarDecl},
    expression::{BinOp, OddCondition, RelationalCondition},
    io::{ReadChar, ReadInt, WriteChar, WriteInt, WriteStr},
    program::Program,
    statement::{AssignStmt, BeginStmt, CallStmt, IfStmt, WhileStatement},
    symboltable::SymbolTable,
    types::{Ident, Number},
    visiters::ASTVisitor,
};
use std::collections::HashMap;

pub struct IRGenerator {
    label_counter: i32,
    vreg_counter: i32,
    data_output: String,
    text_output: String,
    symbol_table: SymbolTable,
    op_map: HashMap<String, &'static str>,
}

impl IRGenerator {
    pub fn new(table: SymbolTable) -> Self {
        let mut op_map = HashMap::new();
        // Initialize operator map in constructor
        op_map.insert("Plus".to_string(), "+");
        op_map.insert("Minus".to_string(), "-");
        op_map.insert("Multiply".to_string(), "*");
        op_map.insert("Divide".to_string(), "/");
        op_map.insert("Equal".to_string(), "==");
        op_map.insert("!=".to_string(), "cmp_ne");
        op_map.insert("LessThan".to_string(), "<");
        op_map.insert("LessThanEqual".to_string(), "<=");
        op_map.insert("GreaterThan".to_string(), ">");
        op_map.insert("GreaterThanEqual".to_string(), ">=");
        op_map.insert("Hash".to_string(), "#");

        Self {
            symbol_table: table,
            label_counter: 0,
            vreg_counter: 0,
            data_output: String::with_capacity(1024),  // Pre-allocate capacity
            text_output: String::with_capacity(4096),  // Pre-allocate capacity
            op_map,
        }
    }

    fn alloc_vreg(&mut self) -> String {
        let vreg = format!("v{}", self.vreg_counter);
        self.vreg_counter += 1;
        vreg
    }

    fn create_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    pub fn get_output(&self) -> String {
        let mut output = String::with_capacity(
            self.data_output.len() + self.text_output.len() + 100
        );
        
        if !self.data_output.is_empty() {
            output.push_str("section .data\n");
            output.push_str(&self.data_output);
        }
        output.push_str("global _start\nsection .text\n_start:\n");
        output.push_str(&self.text_output);
        output
    }

    pub fn generate_code(&mut self, ast: Option<Box<dyn Node + 'static>>) -> Result<(), String> {
        ast.ok_or_else(|| "No AST provided for code generation".to_string())?.accept(self)?;
        Ok(())
    }

    fn emit(&mut self, op: &str, dest: &str, src1: &str, src2: &str) {
        match self.op_map.get(op).map(|&s| s) {
            Some("+") => {
                self.text_output.push_str(&format!("    mov {}, {}\n    add {}, {}\n", dest, src1, dest, src2));
            }
            Some("-") => {
                self.text_output.push_str(&format!("    mov {}, {}\n    sub {}, {}\n", dest, src1, dest, src2));
            }
            Some("*") => {
                self.text_output.push_str(&format!("    mov {}, {}\n    imul {}, {}\n", dest, src1, dest, src2));
            }
            Some("/") => {
                self.text_output.push_str(&format!(
                    "    mov rax, {}\n    cqo\n    idiv {}\n    mov {}, rax\n",
                    src1, src2, dest
                ));
            }
            Some("cmp_eq") | Some("cmp_ne") | Some("cmp_lt") | Some("cmp_le") | Some("cmp_gt") | Some("cmp_ge") => {
                self.text_output.push_str(&format!(
                    "    mov {}, 0\n    cmp {}, {}\n    set{} al\n    movzx {}, al\n",
                    dest, src1, src2, op, dest
                ));
            }
            Some(_) => {}
            None => panic!("Operator mapping not found for {}", op),
        }
    }

    // Generic gen_expr for any node implementing Node
    fn gen_expr(&mut self, expr: &dyn crate::ast::Node) -> String {
        expr.accept(self).expect("Failed to generate expression");
        format!("v{}", self.vreg_counter - 1)
    }
}

impl ASTVisitor for IRGenerator {
    fn visit_ident(&mut self, ident: &Ident) -> Result<(), String> {
        let vreg = self.alloc_vreg();
        self.text_output
            .push_str(&format!("    mov {}, [{}]\n", vreg, ident.value));
        Ok(())
    }

    fn visit_number(&mut self, number: &Number) -> Result<(), String> {
        let vreg = self.alloc_vreg();
        self.text_output
            .push_str(&format!("    mov {}, {}\n", vreg, number.value));
        Ok(())
    }

    fn visit_variable(&mut self, variable: &Variable) -> Result<(), String> {
        let vreg = self.alloc_vreg();
        self.text_output
            .push_str(&format!("    mov {}, [{}]\n", vreg, variable.name));
        Ok(())
    }

    fn visit_binary_operation(&mut self, binop: &BinOp) -> Result<(), String> {
        let left_vreg = binop.left.as_ref().ok_or_else(|| "Binary operation missing left operand".to_string())?;
        let right_vreg = binop.right.as_ref().ok_or_else(|| "Binary operation missing right operand".to_string())?;
        
        // Generate expressions first and store results
        let left_result = self.gen_expr(left_vreg.as_ref());
        let right_result = self.gen_expr(right_vreg.as_ref());
        
        let result_vreg = self.alloc_vreg();
        match binop.operator.as_str() {
            "Plus" => {
                self.text_output.push_str(&format!("    mov {}, {}\n", result_vreg, left_result));
                self.text_output.push_str(&format!("    add {}, {}\n", result_vreg, right_result));
            }
            "Minus" => {
                self.text_output.push_str(&format!("    mov {}, {}\n", result_vreg, left_result));
                self.text_output.push_str(&format!("    sub {}, {}\n", result_vreg, right_result));
            }
            "Multiply" => {
                self.text_output.push_str(&format!("    mov {}, {}\n", result_vreg, left_result));
                self.text_output.push_str(&format!("    imul {}, {}\n", result_vreg, right_result));
            }
            "Divide" => {
                self.text_output.push_str(&format!("    mov rax, {}\n", left_result));
                self.text_output.push_str("    cqo\n");
                self.text_output.push_str(&format!("    idiv {}\n", right_result));
                self.text_output.push_str(&format!("    mov {}, rax\n", result_vreg));
            }
            _ => return Err(format!("Unknown operator: {}", binop.operator)),
        }
        Ok(())
    }

    fn visit_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), String> {
        let start_label = self.create_label();
        let end_label = self.create_label();
        self.text_output.push_str(&format!("{}:\n", start_label));
        let condition = stmt.condition.as_ref().ok_or_else(|| "While statement missing condition".to_string())?;
        let cond_vreg = self.gen_expr(condition.as_ref());
        self.text_output.push_str(&format!("    cmp {}, 0\n", cond_vreg));
        self.text_output.push_str(&format!("    je {}\n", end_label));
        stmt.body.as_ref().ok_or_else(|| "While statement missing body".to_string())?.accept(self)?;
        self.text_output.push_str(&format!("    jmp {}\n", start_label));
        self.text_output.push_str(&format!("{}:\n", end_label));
        Ok(())
    }

    fn visit_condition(&mut self, cond: &OddCondition) -> Result<(), String> {
        let expr = cond.expr.as_ref().ok_or_else(|| "OddCondition missing expression".to_string())?;
        let num_reg = self.gen_expr(expr.as_ref());
        let temp_reg = self.alloc_vreg();
        let remainder_reg = self.alloc_vreg();
        
        // Divide by 2 and check remainder
        self.text_output.push_str(&format!("    mov {}, 2\n", temp_reg));
        self.text_output.push_str(&format!("    mov rax, {}\n", num_reg));
        self.text_output.push_str(&format!("    cqo\n"));
        self.text_output.push_str(&format!("    idiv {}\n", temp_reg));
        self.text_output.push_str(&format!("    mov {}, rdx\n", remainder_reg));
        self.text_output.push_str(&format!("    cmp {}, 0\n", remainder_reg));
        self.text_output.push_str(&format!("    setne al\n"));
        self.text_output.push_str(&format!("    movzx {}, al\n", remainder_reg));
        
        Ok(())
    }

    fn visit_relational_condition(&mut self, cond: &RelationalCondition) -> Result<(), String> {
        let left = cond.left.as_ref().ok_or_else(|| "Relational condition missing left operand".to_string())?;
        let right = cond.right.as_ref().ok_or_else(|| "Relational condition missing right operand".to_string())?;
        
        // Generate expressions first and store results
        let left_result = self.gen_expr(left.as_ref());
        let right_result = self.gen_expr(right.as_ref());
        
        let result_vreg = self.alloc_vreg();
        match cond.operator.as_str() {
            "GreaterThan" => {
                self.text_output.push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output.push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setg al\n"));
                self.text_output.push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "LessThan" => {
                self.text_output.push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output.push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setl al\n"));
                self.text_output.push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "Equal" => {
                self.text_output.push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output.push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    sete al\n"));
                self.text_output.push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "GreaterThanEqual" => {
                self.text_output.push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output.push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setge al\n"));
                self.text_output.push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "LessThanEqual" => {
                self.text_output.push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output.push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setle al\n"));
                self.text_output.push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "!=" => {
                self.text_output.push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output.push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setne al\n"));
                self.text_output.push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "Hash" => {
                self.text_output.push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output.push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setne al\n"));
                self.text_output.push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            _ => return Err(format!("Unknown relational operator: {}", cond.operator)),
        }
        Ok(())
    }

    fn visit_call(&mut self, call: &CallStmt) -> Result<(), String> {
        self.text_output
            .push_str(&format!("    call {}\n", call.identifier));
        Ok(())
    }

    fn visit_assign(&mut self, stmt: &AssignStmt) -> Result<(), String> {
        let expr = stmt.expr.as_ref().ok_or_else(|| "Assign statement missing expression".to_string())?;
        let vreg = self.gen_expr(expr.as_ref());
        self.text_output.push_str(&format!("    mov [{}], {}\n", stmt.identifier, vreg));
        Ok(())
    }

    fn visit_begin(&mut self, expr: &BeginStmt) -> Result<(), String> {
        for stmt in &expr.stmts {
            if let Some(ref stmt) = stmt {
                stmt.accept(self)?;
            }
        }
        Ok(())
    }

    fn visit_if(&mut self, expr: &IfStmt) -> Result<(), String> {
        let else_label = self.create_label();
        let end_label = self.create_label();
        let condition = expr.condition.as_ref().ok_or_else(|| "If statement missing condition".to_string())?;
        let cond_vreg = self.gen_expr(condition.as_ref());
        self.text_output.push_str(&format!("    cmp {}, 0\n", cond_vreg));
        self.text_output.push_str(&format!("    je {}\n", else_label));
        
        let then_branch = expr.then_branch.as_ref().ok_or_else(|| "If statement missing then branch".to_string())?;
        then_branch.accept(self)?;
        
        if let Some(ref else_branch) = expr.else_branch {
            self.text_output.push_str(&format!("    jmp {}\n", end_label));
            self.text_output.push_str(&format!("{}:\n", else_label));
            else_branch.accept(self)?;
            self.text_output.push_str(&format!("{}:\n", end_label));
        } else {
            self.text_output.push_str(&format!("{}:\n", else_label));
        }
        Ok(())
    }

    fn visit_write_int(&mut self, stmt: &WriteInt) -> Result<(), String> {
        if let Some(ref expr) = stmt.expr {
            let vreg = self.gen_expr(expr.as_ref());
            self.text_output
                .push_str(&format!("    write_int {}\n", vreg));
            Ok(())
        } else {
            Err("WriteInt statement missing expression".to_string())
        }
    }

    fn visit_write_char(&mut self, stmt: &WriteChar) -> Result<(), String> {
        if let Some(ref expr) = stmt.expr {
            let vreg = self.gen_expr(expr.as_ref());
            self.text_output
                .push_str(&format!("    write_char {}\n", vreg));
            Ok(())
        } else {
            Err("WriteChar statement missing expression".to_string())
        }
    }

    fn visit_write_str(&mut self, stmt: &WriteStr) -> Result<(), String> {
        self.text_output
            .push_str(&format!("    write_str \"{}\"\n", stmt.expr));
        Ok(())
    }

    fn visit_read_int(&mut self, expr: &ReadInt) -> Result<(), String> {
        let vreg = self.alloc_vreg();
        self.text_output
            .push_str(&format!("    read_int {}\n", vreg));
        self.text_output
            .push_str(&format!("    mov [{}], {}\n", expr.identifier, vreg));
        Ok(())
    }

    fn visit_read_char(&mut self, expr: &ReadChar) -> Result<(), String> {
        let vreg = self.alloc_vreg();
        self.text_output
            .push_str(&format!("    read_char {}\n", vreg));
        self.text_output
            .push_str(&format!("    mov [{}], {}\n", expr.identifier, vreg));
        Ok(())
    }

    fn visit_exit(&mut self, _expr: &Exit) -> Result<(), String> {
        self.text_output.push_str("    exit\n");
        Ok(())
    }

    fn visit_const(&mut self, expr: &ConstDecl) -> Result<(), String> {
        for (id, num) in &expr.const_decl {
            self.data_output
                .push_str(&format!("    {}: dq {}\n", id, num));
        }
        Ok(())
    }

    fn visit_var_decl(&mut self, expr: &VarDecl) -> Result<(), String> {
        for var_name in &expr.var_decl {
            let vreg = self.alloc_vreg();
            self.text_output.push_str(&format!("    mov {}, 0\n", vreg));
            self.text_output
                .push_str(&format!("    mov [{}], {}\n", var_name, vreg));
        }
        Ok(())
    }

    fn visit_proc_decl(&mut self, expr: &ProcDecl) -> Result<(), String> {
        for (name, proc_block) in &expr.procedurs {
            self.text_output.push_str(&format!("{}:\n", name));
            self.text_output
                .push_str("    push rbp\n    mov rbp, rsp\n");
            if let Some(block) = proc_block {
                block.accept(self)?;
                self.text_output
                    .push_str("    mov rsp, rbp\n    pop rbp\n    ret\n");
            }
        }
        Ok(())
    }

    fn visit_block(&mut self, block: &Block) -> Result<(), String> {
        if !block.const_decl.const_decl.is_empty() {
            block.const_decl.accept(self)?;
        }
        if !block.var_decl.var_decl.is_empty() {
            block.var_decl.accept(self)?;
        }
        if !block.proc_decl.procedurs.is_empty() {
            block.proc_decl.accept(self)?;
        }
        if let Some(stmt) = &block.statement {
            stmt.accept(self)?;
        }
        Ok(())
    }

    fn visit_program(&mut self, expr: &Program) -> Result<(), String> {
        if let Some(ref block) = &expr.block {
            block.accept(self)?;
        }
        Ok(())
    }
}
