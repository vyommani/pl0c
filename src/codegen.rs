use crate::{
    ast::{Exit, Node, Variable},
    block::Block,
    decl::{ConstDecl, ProcDecl, VarDecl},
    expression::{BinOp, OddCondition, RelationalCondition},
    io::{ReadChar, ReadInt, WriteChar, WriteInt, WriteStr},
    program::Program,
    statement::{AssignStmt, BeginStmt, CallStmt, IfStmt, WhileStatement},
    symboltable::{Symbol, SymbolLocation, SymbolTable, SymbolType},
    types::{Ident, Number},
    visiters::ASTVisitor,
};

// Intermediate Representation (IR) generator for PL/0 compiler.
//
// This struct is responsible for generating IR code from an AST.
// It uses virtual registers and generates architecture-independent IR
// that can be later processed by a register allocator and backend.
pub struct IRGenerator {
    label_counter: i32,
    vreg_counter: i32,
    data_output: String,
    bss_output: String,
    text_output: String,
    vreg_prefix: String,
    symbol_table: SymbolTable,
    exit_emitted: bool,
}

impl IRGenerator {
    pub fn new(table: SymbolTable) -> Self {
        Self {
            symbol_table: table,
            label_counter: 0,
            vreg_counter: 0,
            data_output: String::with_capacity(1024),
            bss_output: String::with_capacity(256),
            text_output: String::with_capacity(4096),
            vreg_prefix: "v".to_string(),
            exit_emitted: false,
        }
    }

    fn allocate_virtual_register(&mut self) -> String {
        let vreg = format!("{}{}", self.vreg_prefix, self.vreg_counter);
        self.vreg_counter += 1;
        vreg
    }

    fn create_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    pub fn get_output(&self) -> String {
        let mut output = String::with_capacity(
            self.data_output.len() + self.bss_output.len() + self.text_output.len() + 100,
        );

        if !self.data_output.is_empty() {
            output.push_str("section .data\n");
            output.push_str(&self.data_output);
        }
        if !self.bss_output.is_empty() {
            output.push_str("section .bss\n");
            output.push_str(&self.bss_output);
        }
        output.push_str("global _start\nsection .text\n_start:\n");
        output.push_str(&self.text_output);
        output
    }

    pub fn generate_code(&mut self, ast: Option<Box<dyn Node + 'static>>) -> Result<(), String> {
        self.exit_emitted = false;
        ast.ok_or_else(|| "No AST provided for code generation".to_string())?
            .accept(self)?;
        if !self.exit_emitted {
            self.system_exit(0);
        }
        Ok(())
    }

    fn emit_expression(&mut self, expr: &dyn crate::ast::Node) -> Result<String, String> {
        expr.accept(self)?;
        Ok(format!("v{}", self.vreg_counter - 1))
    }

    fn system_exit(&mut self, code: i32) {
        self.text_output.push_str(&format!(
            "    mov rax, 60\n    mov rdi, {}\n    syscall\n",
            code
        ));
    }

    /// Helper function to generate load instruction based on symbol location
    fn emit_load_from_symbol(
        &mut self,
        symbol: &Symbol,
        target_vreg: &str,
        fallback_name: &str,
    ) -> Result<(), String> {
        match &symbol.location {
            SymbolLocation::StackOffset(offset) => {
                // Local variable on stack
                self.text_output
                    .push_str(&format!("    mov {}, [rbp-{}]\n", target_vreg, offset));
            }
            SymbolLocation::GlobalLabel(label) => {
                // Global variable
                self.text_output
                    .push_str(&format!("    mov {}, [{}]\n", target_vreg, label));
            }
            SymbolLocation::Immediate(value) => {
                // Immediate value (for constants)
                self.text_output
                    .push_str(&format!("    mov {}, {}\n", target_vreg, value));
            }
            SymbolLocation::None => {
                // Fallback: use name as label (for backward compatibility)
                self.text_output
                    .push_str(&format!("    mov {}, [{}]\n", target_vreg, fallback_name));
            }
        }
        Ok(())
    }

    /// Helper function to generate store instruction based on symbol location
    fn emit_store_to_symbol(
        &mut self,
        symbol: &Symbol,
        source_vreg: &str,
        fallback_name: &str,
    ) -> Result<(), String> {
        match &symbol.location {
            SymbolLocation::StackOffset(offset) => {
                // Store to local variable on stack
                self.text_output
                    .push_str(&format!("    mov [rbp-{}], {}\n", offset, source_vreg));
            }
            SymbolLocation::GlobalLabel(label) => {
                // Store to global variable
                self.text_output
                    .push_str(&format!("    mov [{}], {}\n", label, source_vreg));
            }
            SymbolLocation::None => {
                // Fallback: use name as label (for backward compatibility)
                self.text_output
                    .push_str(&format!("    mov [{}], {}\n", fallback_name, source_vreg));
            }
            SymbolLocation::Immediate(_) => {
                return Err(format!(
                    "Cannot store to immediate value: {}",
                    fallback_name
                ));
            }
        }
        Ok(())
    }

    /// Helper function to get symbol and validate it's the expected type
    fn get_symbol_with_type(
        &self,
        name: &str,
        expected_type: SymbolType,
        operation: &str,
    ) -> Result<&Symbol, String> {
        let symbol = self
            .symbol_table
            .get(name)
            .ok_or_else(|| format!("Undefined {}: {}", operation, name))?;

        if std::mem::discriminant(&symbol.symbol_type) != std::mem::discriminant(&expected_type) {
            return Err(format!(
                "Expected {:?} but found {:?}: {}",
                expected_type, symbol.symbol_type, name
            ));
        }

        Ok(symbol)
    }

    /// Helper function to update symbol location in symbol table
    fn update_symbol_location(&mut self, name: &str, location: SymbolLocation, is_global: bool) {
        if let Some(symbol) = self.symbol_table.get_mut(name) {
            symbol.location = location;
            symbol.is_global = is_global;
            if is_global {
                symbol.initialized = true;
            }
        }
    }

    /// Helper function to get a variable symbol specifically
    fn get_variable_symbol(&self, name: &str, operation: &str) -> Result<&Symbol, String> {
        self.get_symbol_with_type(name, SymbolType::Variable, operation)
    }

    /// Helper function to get a procedure symbol specifically
    fn get_procedure_symbol(&self, name: &str, operation: &str) -> Result<&Symbol, String> {
        self.get_symbol_with_type(name, SymbolType::Procedure, operation)
    }
}

impl ASTVisitor for IRGenerator {
    fn visit_ident(&mut self, ident: &Ident) -> Result<(), String> {
        let symbol = self
            .symbol_table
            .get(&ident.value)
            .ok_or_else(|| format!("Undefined identifier: {}", ident.value))?
            .clone();

        let vreg = self.allocate_virtual_register();

        match symbol.symbol_type {
            SymbolType::Constant(value) => {
                // For constants, load the immediate value
                self.text_output
                    .push_str(&format!("    mov {}, {}\n", vreg, value));
            }
            SymbolType::Procedure => {
                // For procedures, load the address (useful for function pointers)
                match &symbol.location {
                    SymbolLocation::GlobalLabel(label) => {
                        self.text_output
                            .push_str(&format!("    lea {}, [{}]\n", vreg, label));
                    }
                    _ => {
                        return Err(format!("Procedure {} has no valid address", ident.value));
                    }
                }
            }
            _ => {
                // For variables and other types, use the helper function
                self.emit_load_from_symbol(&symbol, &vreg, &ident.value)?;
            }
        }

        Ok(())
    }

    fn visit_number(&mut self, number: &Number) -> Result<(), String> {
        let vreg = self.allocate_virtual_register();
        self.text_output
            .push_str(&format!("    mov {}, {}\n", vreg, number.value));
        Ok(())
    }

    fn visit_variable(&mut self, variable: &Variable) -> Result<(), String> {
        let symbol = self
            .get_variable_symbol(&variable.name, "variable")?
            .clone();

        let vreg = self.allocate_virtual_register();
        self.emit_load_from_symbol(&symbol, &vreg, &variable.name)?;

        Ok(())
    }

    fn visit_binary_operation(&mut self, binop: &BinOp) -> Result<(), String> {
        let left_vreg = binop
            .left
            .as_ref()
            .ok_or_else(|| "Binary operation missing left operand".to_string())?;
        let right_vreg = binop
            .right
            .as_ref()
            .ok_or_else(|| "Binary operation missing right operand".to_string())?;

        // Generate expressions first and store results
        let left_result = self.emit_expression(left_vreg.as_ref())?;
        let right_result = self.emit_expression(right_vreg.as_ref())?;

        let result_vreg = self.allocate_virtual_register();
        match binop.operator.as_str() {
            "Plus" => {
                self.text_output
                    .push_str(&format!("    mov {}, {}\n", result_vreg, left_result));
                self.text_output
                    .push_str(&format!("    add {}, {}\n", result_vreg, right_result));
            }
            "Minus" => {
                self.text_output
                    .push_str(&format!("    mov {}, {}\n", result_vreg, left_result));
                self.text_output
                    .push_str(&format!("    sub {}, {}\n", result_vreg, right_result));
            }
            "Multiply" => {
                self.text_output
                    .push_str(&format!("    mov {}, {}\n", result_vreg, left_result));
                self.text_output
                    .push_str(&format!("    imul {}, {}\n", result_vreg, right_result));
            }
            "Divide" => {
                self.text_output
                    .push_str(&format!("    mov rax, {}\n", left_result));
                self.text_output.push_str("    cqo\n");
                self.text_output
                    .push_str(&format!("    idiv {}\n", right_result));
                self.text_output
                    .push_str(&format!("    mov {}, rax\n", result_vreg));
            }
            _ => return Err(format!("Unknown operator: {}", binop.operator)),
        }
        Ok(())
    }

    fn visit_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), String> {
        let start_label = self.create_label();
        let end_label = self.create_label();
        self.text_output.push_str(&format!("{}:\n", start_label));
        let condition = stmt
            .condition
            .as_ref()
            .ok_or_else(|| "While statement missing condition".to_string())?;
        let cond_vreg = self.emit_expression(condition.as_ref())?;
        self.text_output
            .push_str(&format!("    cmp {}, 0\n", cond_vreg));
        self.text_output
            .push_str(&format!("    je {}\n", end_label));
        stmt.body
            .as_ref()
            .ok_or_else(|| "While statement missing body".to_string())?
            .accept(self)?;
        self.text_output
            .push_str(&format!("    jmp {}\n", start_label));
        self.text_output.push_str(&format!("{}:\n", end_label));
        Ok(())
    }

    fn visit_condition(&mut self, cond: &OddCondition) -> Result<(), String> {
        let expr = cond
            .expr
            .as_ref()
            .ok_or_else(|| "OddCondition missing expression".to_string())?;
        let num_reg = self.emit_expression(expr.as_ref())?;
        let temp_reg = self.allocate_virtual_register();
        let remainder_reg = self.allocate_virtual_register();

        // Divide by 2 and check remainder
        self.text_output
            .push_str(&format!("    mov {}, 2\n", temp_reg));
        self.text_output
            .push_str(&format!("    mov rax, {}\n", num_reg));
        self.text_output.push_str(&format!("    cqo\n"));
        self.text_output
            .push_str(&format!("    idiv {}\n", temp_reg));
        self.text_output
            .push_str(&format!("    mov {}, rdx\n", remainder_reg));
        self.text_output
            .push_str(&format!("    cmp {}, 0\n", remainder_reg));
        self.text_output.push_str(&format!("    setne al\n"));
        self.text_output
            .push_str(&format!("    movzx {}, al\n", remainder_reg));

        Ok(())
    }

    fn visit_relational_condition(&mut self, cond: &RelationalCondition) -> Result<(), String> {
        let left = cond
            .left
            .as_ref()
            .ok_or_else(|| "Relational condition missing left operand".to_string())?;
        let right = cond
            .right
            .as_ref()
            .ok_or_else(|| "Relational condition missing right operand".to_string())?;

        // Generate expressions first and store results
        let left_result = self.emit_expression(left.as_ref())?;
        let right_result = self.emit_expression(right.as_ref())?;

        let result_vreg = self.allocate_virtual_register();
        match cond.operator.as_str() {
            "GreaterThan" => {
                self.text_output
                    .push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setg al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "LessThan" => {
                self.text_output
                    .push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setl al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "Equal" => {
                self.text_output
                    .push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    sete al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "GreaterThanEqual" => {
                self.text_output
                    .push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setge al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "LessThanEqual" => {
                self.text_output
                    .push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setle al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "!=" => {
                self.text_output
                    .push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setne al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            "Hash" => {
                self.text_output
                    .push_str(&format!("    mov {}, 0\n", result_vreg));
                self.text_output
                    .push_str(&format!("    cmp {}, {}\n", left_result, right_result));
                self.text_output.push_str(&format!("    setne al\n"));
                self.text_output
                    .push_str(&format!("    movzx {}, al\n", result_vreg));
            }
            _ => return Err(format!("Unknown relational operator: {}", cond.operator)),
        }
        Ok(())
    }

    fn visit_call(&mut self, call: &CallStmt) -> Result<(), String> {
        let symbol = self.get_procedure_symbol(&call.identifier, "procedure")?;

        match &symbol.location {
            SymbolLocation::GlobalLabel(label) => {
                self.text_output.push_str(&format!("    call {}\n", label));
            }
            SymbolLocation::None => {
                // Fallback: use identifier name as label (for backward compatibility)
                self.text_output
                    .push_str(&format!("    call {}\n", call.identifier));
            }
            _ => {
                return Err(format!(
                    "Procedure {} has no valid address",
                    call.identifier
                ));
            }
        }

        Ok(())
    }

    fn visit_assign(&mut self, stmt: &AssignStmt) -> Result<(), String> {
        let symbol = self
            .get_variable_symbol(&stmt.identifier, "variable in assignment")?
            .clone();

        let expr = stmt
            .expr
            .as_ref()
            .ok_or_else(|| "Assign statement missing expression".to_string())?;
        let vreg = self.emit_expression(expr.as_ref())?;
        self.emit_store_to_symbol(&symbol, &vreg, &stmt.identifier)?;

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
        let condition = expr
            .condition
            .as_ref()
            .ok_or_else(|| "If statement missing condition".to_string())?;
        let cond_vreg = self.emit_expression(condition.as_ref())?;
        self.text_output
            .push_str(&format!("    cmp {}, 0\n", cond_vreg));
        self.text_output
            .push_str(&format!("    je {}\n", else_label));

        let then_branch = expr
            .then_branch
            .as_ref()
            .ok_or_else(|| "If statement missing then branch".to_string())?;
        then_branch.accept(self)?;

        if let Some(ref else_branch) = expr.else_branch {
            self.text_output
                .push_str(&format!("    jmp {}\n", end_label));
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
            let vreg = self.emit_expression(expr.as_ref())?;
            self.text_output
                .push_str(&format!("    write_int {}\n", vreg));
            Ok(())
        } else {
            Err("WriteInt statement missing expression".to_string())
        }
    }

    fn visit_write_char(&mut self, stmt: &WriteChar) -> Result<(), String> {
        if let Some(ref expr) = stmt.expr {
            let vreg = self.emit_expression(expr.as_ref())?;
            self.text_output
                .push_str(&format!("    write_char {}\n", vreg));
            Ok(())
        } else {
            Err("WriteChar statement missing expression".to_string())
        }
    }

    fn visit_write_str(&mut self, stmt: &WriteStr) -> Result<(), String> {
        if stmt.expr.is_empty() {
            return Err("WriteStr statement missing expression".to_string());
        }
        // Generate pure IR instruction
        self.text_output
            .push_str(&format!("    write_str \"{}\"\n", stmt.expr));
        Ok(())
    }

    fn visit_read_int(&mut self, expr: &ReadInt) -> Result<(), String> {
        let symbol = self
            .get_variable_symbol(&expr.identifier, "variable in read")?
            .clone();

        let vreg = self.allocate_virtual_register();
        self.text_output
            .push_str(&format!("    read_int {}\n", vreg));
        self.emit_store_to_symbol(&symbol, &vreg, &expr.identifier)?;

        Ok(())
    }

    fn visit_read_char(&mut self, expr: &ReadChar) -> Result<(), String> {
        let symbol = self
            .get_variable_symbol(&expr.identifier, "variable in read")?
            .clone();

        let vreg = self.allocate_virtual_register();
        self.text_output
            .push_str(&format!("    read_char {}\n", vreg));
        self.emit_store_to_symbol(&symbol, &vreg, &expr.identifier)?;

        Ok(())
    }

    fn visit_exit(&mut self, _expr: &Exit) -> Result<(), String> {
        self.exit_emitted = true;
        self.text_output.push_str("    mov rax, 60\n");
        self.text_output.push_str("    mov rdi, 0\n");
        self.text_output.push_str("    syscall\n");
        Ok(())
    }

    fn visit_const(&mut self, expr: &ConstDecl) -> Result<(), String> {
        for (id, num) in &expr.const_decl {
            self.data_output
                .push_str(&format!("    {}: dq {}\n", id, num));

            self.update_symbol_location(id, SymbolLocation::GlobalLabel(id.clone()), true);
        }
        Ok(())
    }

    fn visit_var_decl(&mut self, expr: &VarDecl) -> Result<(), String> {
        for var_name in &expr.var_decl {
            // Only update the symbol table; do not emit mov instructions for initialization
            self.update_symbol_location(
                var_name,
                SymbolLocation::GlobalLabel(var_name.clone()),
                true,
            );
        }
        Ok(())
    }

    fn visit_proc_decl(&mut self, expr: &ProcDecl) -> Result<(), String> {
        for (name, proc_block) in &expr.procedurs {
            self.update_symbol_location(name, SymbolLocation::GlobalLabel(name.clone()), true);

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
