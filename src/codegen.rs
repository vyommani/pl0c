use crate::ast::Exit;
use crate::ast::Node;
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
use crate::symboltable::SymbolTable;
use crate::types::Ident;
use crate::types::Number;
use crate::visiters::ASTVisitor;
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
        Self {
            symbol_table: table,
            label_counter: 0,
            vreg_counter: 0,
            data_output: String::new(),
            text_output: String::new(),
            op_map: HashMap::new(),
        }
    }

    pub fn init(&mut self) {
        self.op_map.insert("Plus".to_string(), "+");
        self.op_map.insert("Minus".to_string(), "-");
        self.op_map.insert("Multiply".to_string(), "*");
        self.op_map.insert("Divide".to_string(), "/");
        self.op_map.insert("Equal".to_string(), "==");
        self.op_map.insert("!=".to_string(), "cmp_ne");
        self.op_map.insert("LessThan".to_string(), "<");
        self.op_map.insert("LessThanEqual".to_string(), "<=");
        self.op_map.insert("GreaterThan".to_string(), ">");
        self.op_map.insert("GreaterThanEqual".to_string(), ">=");

        self.op_map.insert("Hash".to_string(), "#");
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
        let mut output = String::new();
        if !self.data_output.is_empty() {
            output.push_str("section .data\n");
            output.push_str(&self.data_output);
        }
        output.push_str("global _start\nsection .text\n_start:\n");
        output.push_str(&self.text_output);
        output
    }

    pub fn generate_code(&mut self, ast: Option<Box<dyn Node + 'static>>) -> Result<(), String> {
        self.init();
        if let Some(program) = ast {
            program.accept(self);
            Ok(())
        } else {
            Err("No AST provided for code generation".to_string())
        }
    }

    fn emit(&mut self, op: &str, dest: &str, src1: &str, src2: &str) {
        match self.op_map.get(op).map(|&s| s) {
            Some("+") => {
                self.text_output
                    .push_str(&format!("    mov {}, {}\n", dest, src1));
                self.text_output
                    .push_str(&format!("    add {}, {}\n", dest, src2));
            }
            Some("-") => {
                self.text_output
                    .push_str(&format!("    mov {}, {}\n", dest, src1));
                self.text_output
                    .push_str(&format!("    sub {}, {}\n", dest, src2));
            }
            Some("*") => {
                self.text_output
                    .push_str(&format!("    mov {}, {}\n", dest, src1));
                self.text_output
                    .push_str(&format!("    imul {}, {}\n", dest, src2));
            }
            Some("/") => {
                self.text_output
                    .push_str(&format!("    mov rax, {}\n", src1));
                self.text_output.push_str("    cqo\n");
                self.text_output.push_str(&format!("    idiv {}\n", src2));
                self.text_output
                    .push_str(&format!("    mov {}, rax\n", dest));
            }
            Some("cmp_eq") => {
                self.text_output.push_str(&format!("    mov {}, 0\n", dest));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", src1, src2));
                self.text_output.push_str(&format!("    sete al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", dest));
            }
            Some("cmp_ne") => {
                self.text_output.push_str(&format!("    mov {}, 0\n", dest));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", src1, src2));
                self.text_output.push_str(&format!("    setne al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", dest));
            }
            Some("cmp_lt") => {
                self.text_output.push_str(&format!("    mov {}, 0\n", dest));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", src1, src2));
                self.text_output.push_str(&format!("    setl al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", dest));
            }
            Some("cmp_le") => {
                self.text_output.push_str(&format!("    mov {}, 0\n", dest));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", src1, src2));
                self.text_output.push_str(&format!("    setle al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", dest));
            }
            Some("cmp_gt") => {
                self.text_output.push_str(&format!("    mov {}, 0\n", dest));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", src1, src2));
                self.text_output.push_str(&format!("    setg al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", dest));
            }
            Some("cmp_ge") => {
                self.text_output.push_str(&format!("    mov {}, 0\n", dest));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", src1, src2));
                self.text_output.push_str(&format!("    setge al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", dest));
            }
            Some(_) => {}
            None => panic!("Operator mapping not found for {}", op),
        }
    }

    // Generic gen_expr for any node implementing Node
    fn gen_expr(&mut self, expr: &dyn crate::ast::Node) -> String {
        expr.accept(self);
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
        if let Some(ref left) = binop.left {
            let left_vreg = self.gen_expr(&**left);
            if let Some(ref right) = binop.right {
                let right_vreg = self.gen_expr(&**right);
                let result_vreg = self.alloc_vreg();
                self.emit(&binop.operator, &result_vreg, &left_vreg, &right_vreg);
                Ok(())
            } else {
                Err("Binary operation missing right operand".to_string())
            }
        } else {
            Err("Binary operation missing left operand".to_string())
        }
    }

    fn visit_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), String> {
        let start_label = self.create_label();
        let end_label = self.create_label();
        self.text_output.push_str(&format!("{}:\n", start_label));
        if let Some(ref condition) = &stmt.condition {
            let cond_vreg = self.gen_expr(&**condition);
            self.text_output
                .push_str(&format!("    cmp {}, 0\n", cond_vreg));
            self.text_output
                .push_str(&format!("    je {}\n", end_label));
            if let Some(ref body) = &stmt.body {
                body.accept(self);
            } else {
                return Err("While statement missing body".to_string());
            }
            self.text_output
                .push_str(&format!("    jmp {}\n", start_label));
            self.text_output.push_str(&format!("{}:\n", end_label));
            Ok(())
        } else {
            Err("While statement missing condition".to_string())
        }
    }

    fn visit_condition(&mut self, cond: &OddCondition) -> Result<(), String> {
        if let Some(ref expr) = cond.expr {
            let _vreg = self.gen_expr(expr.as_ref());
            Ok(())
        } else {
            Err("OddCondition missing expr".to_string())
        }
    }

    fn visit_relational_condition(&mut self, cond: &RelationalCondition) -> Result<(), String> {
        if let Some(ref left) = cond.left {
            let left_vreg = self.gen_expr(left.as_ref());
            if let Some(ref right) = cond.right {
                let right_vreg = self.gen_expr(right.as_ref());
                let result_vreg = self.alloc_vreg();
                self.emit(&cond.operator, &result_vreg, &left_vreg, &right_vreg);
                Ok(())
            } else {
                return Err("Relational condition missing right operand".to_string());
            }
        } else {
            Err("Relational condition missing left operand".to_string())
        }
    }

    fn visit_call(&mut self, call: &CallStmt) -> Result<(), String> {
        self.text_output
            .push_str(&format!("    call {}\n", call.identifier));
        Ok(())
    }

    fn visit_assign(&mut self, stmt: &AssignStmt) -> Result<(), String> {
        if let Some(ref expr) = stmt.expr {
            let vreg = self.gen_expr(expr.as_ref());
            self.text_output
                .push_str(&format!("    mov [{}], {}\n", stmt.identifier, vreg));
            Ok(())
        } else {
            Err("Assign statement missing expression".to_string())
        }
    }

    fn visit_begin(&mut self, expr: &BeginStmt) -> Result<(), String> {
        for begin_stmt in &expr.stmts {
            if let Some(ref stmt) = begin_stmt {
                stmt.accept(self);
            }
        }
        Ok(())
    }

    fn visit_if(&mut self, expr: &IfStmt) -> Result<(), String> {
        let else_label = self.create_label();
        let end_label = self.create_label();
        if let Some(ref cond) = expr.condition {
            let cond_vreg = self.gen_expr(cond.as_ref());
            self.text_output
                .push_str(&format!("    cmp {}, 0\n", cond_vreg));
            self.text_output
                .push_str(&format!("    je {}\n", else_label));
        }
        if let Some(ref then) = expr.then_branch {
            then.accept(self);
        }
        if let Some(ref else_stmt) = expr.else_branch {
            else_stmt.accept(self);
            self.text_output
                .push_str(&format!("    jmp {}\n", end_label));
            self.text_output.push_str(&format!("{}:\n", else_label));
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
                block.accept(self);
                self.text_output
                    .push_str("    mov rsp, rbp\n    pop rbp\n    ret\n");
            }
        }
        Ok(())
    }

    fn visit_block(&mut self, block: &Block) -> Result<(), String> {
        if !block.const_decl.const_decl.is_empty() {
            block.const_decl.accept(self);
        }
        if !block.var_decl.var_decl.is_empty() {
            block.var_decl.accept(self);
        }
        if !block.proc_decl.procedurs.is_empty() {
            block.proc_decl.accept(self);
        }
        if let Some(stmt) = &block.statement {
            stmt.accept(self);
        }
        Ok(())
    }

    fn visit_program(&mut self, expr: &Program) -> Result<(), String> {
        if let Some(ref block) = &expr.block {
            block.accept(self);
        }
        Ok(())
    }
}
