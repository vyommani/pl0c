use crate::{
    ast::{Exit, Node, Variable, ExpressionNode},
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
use std::fmt::Write;

pub struct IRGenerator {
    label_counter: i32,
    vreg_counter: i32,
    data_output: String,
    bss_output: String,
    text_output: String,
    vreg_prefix: String,
    symbol_table: SymbolTable,
    exit_emitted: bool,
    main_emitted: bool,
    in_procedure: bool,
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
            main_emitted: false,
            in_procedure: false,
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
        let mut output = String::new();
        // Add constants as IR data declarations
        if !self.data_output.is_empty() {
            output.push_str(&self.data_output);
            output.push('\n');
        }
        // Add global variables as IR data declarations
        if !self.bss_output.is_empty() {
            output.push_str(&self.bss_output);
            output.push('\n');
        }
        // Add the main IR code
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

    fn emit_expression(&mut self, expr: &dyn crate::ast::ExpressionNode) -> Result<String, String> {
        ExpressionNode::accept(expr, self)
    }

    fn system_exit(&mut self, code: i32) {
        write!(self.text_output, "    exit {}\n", code).unwrap();
    }

    // Helper function to generate load instruction based on symbol location
    fn emit_load_from_symbol(
        &mut self,
        symbol: &Symbol,
        target_vreg: &str,
        fallback_name: &str,
    ) -> Result<(), String> {
        match &symbol.location {
            SymbolLocation::StackOffset(offset) => {
                // Local variable on stack
                write!(self.text_output, "    ld {}, [bp-{}]\n", target_vreg, offset).unwrap();
            }
            SymbolLocation::GlobalLabel(label) => {
                write!(self.text_output, "    ld {}, [{}]\n", target_vreg, label).unwrap();
            }
            SymbolLocation::Immediate(value) => {
                write!(self.text_output, "    li {}, {}\n", target_vreg, value).unwrap();
            }
            SymbolLocation::None => {
                write!(self.text_output, "    ld {}, [{}]\n", target_vreg, fallback_name).unwrap();
            }
        }
        Ok(())
    }

    // Helper function to generate store instruction based on symbol location
    fn emit_store_to_symbol(
        &mut self,
        symbol: &Symbol,
        source_vreg: &str,
        fallback_name: &str,
    ) -> Result<(), String> {
        match &symbol.location {
            SymbolLocation::StackOffset(offset) => {
                // Store to local variable on stack
                write!(self.text_output, "    st [bp-{}], {}\n", offset, source_vreg).unwrap();
            }
            SymbolLocation::GlobalLabel(label) => {
                // Store to global variable
                write!(self.text_output, "    st [{}], {}\n", label, source_vreg).unwrap();
            }
            SymbolLocation::None => {
                // Fallback: use name as label (for backward compatibility)
                write!(self.text_output, "    st [{}], {}\n", fallback_name, source_vreg).unwrap();
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

    // Helper function to get symbol and validate it's the expected type
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

    // Helper function to update symbol location in symbol table
    fn update_symbol_location(&mut self, name: &str, location: SymbolLocation, is_global: bool) {
        if let Some(symbol) = self.symbol_table.get_mut(name) {
            symbol.location = location;
            symbol.is_global = is_global;
            if is_global {
                symbol.initialized = true;
            }
        }
    }

    // Helper function to get a variable symbol specifically
    fn get_variable_symbol(&self, name: &str, operation: &str) -> Result<&Symbol, String> {
        self.get_symbol_with_type(name, SymbolType::Variable, operation)
    }

    // Helper function to get a procedure symbol specifically
    fn get_procedure_symbol(&self, name: &str, operation: &str) -> Result<&Symbol, String> {
        self.get_symbol_with_type(name, SymbolType::Procedure, operation)
    }

    // Helper function to get or load a variable efficiently
    fn get_or_load_variable(&mut self, variable_name: &str) -> Result<String, String> {
        // Always load from memory for correctness
        let symbol = self.get_variable_symbol(variable_name, "variable")?.clone();
        let vreg = self.allocate_virtual_register();
        self.emit_load_from_symbol(&symbol, &vreg, variable_name)?;
        Ok(vreg)
    }

     fn emit_binary_op(&mut self, op: &str, dest: &str, left: &str, right: &str) {
        write!(self.text_output, "    {} {}, {}, {}\n", op, dest, left, right).unwrap();
    }

    fn emit_relational_op(&mut self, op: &str, dest: &str, left: &str, right: &str) {
        write!(self.text_output, "    {} {}, {}, {}\n", op, dest, left, right).unwrap();
    }
     fn emit_branch_if_zero(&mut self, vreg: &str, label: &str) {
        write!(self.text_output, "    beqz {}, {}\n", vreg, label).unwrap();
    }
    fn emit_jump(&mut self, label: &str) {
        write!(self.text_output, "    jump {}\n", label).unwrap();
    }

    // Helper: emit a label
    fn emit_label(&mut self, label: &str) {
        write!(self.text_output, "{}:\n", label).unwrap();
    }

    // Helper: create and emit a label, returning its name
    fn create_and_emit_label(&mut self) -> String {
        let label = self.create_label();
        self.emit_label(&label);
        label
    }

    // Helper: evaluate a condition expression
    fn evaluate_condition(&mut self, condition: &dyn crate::ast::ExpressionNode) -> Result<String, String> {
        self.emit_expression(condition)
    }

    // Helper: conditional branch
    fn emit_conditional_branch(&mut self, vreg: &str, label: &str) {
        self.emit_branch_if_zero(vreg, label);
    }

    // Helper: write operation
    fn emit_write_operation<F>(
        &mut self,
        expr_opt: Option<&dyn crate::ast::ExpressionNode>,
        error_msg: &str,
        write_fn: F,
    ) -> Result<(), String>
    where
        F: FnOnce(&mut Self, &str),
    {
        if let Some(expr) = expr_opt {
            let vreg = self.emit_expression(expr)?;
            write_fn(self, &vreg);
            Ok(())
        } else {
            Err(error_msg.to_string())
        }
    }

    // Helper: read operation
    fn emit_read_operation(&mut self, operation: &str, identifier: &str) -> Result<(), String> {
        let symbol = self.get_variable_symbol(identifier, "variable in read")?.clone();
        let vreg = self.allocate_virtual_register();
        write!(self.text_output, "    {} {}\n", operation, vreg).unwrap();
        self.emit_store_to_symbol(&symbol, &vreg, identifier)?;
        Ok(())
    }

    // Helper: procedure call
    fn emit_procedure_call(&mut self, identifier: &str) -> Result<(), String> {
        let symbol = self.get_procedure_symbol(identifier, "procedure")?;
        let label = match &symbol.location {
            SymbolLocation::GlobalLabel(label) => label.clone(),
            SymbolLocation::None => identifier.to_string(),
            _ => return Err(format!("Procedure {} has no valid address", identifier)),
        };
        write!(self.text_output, "    call {}\n", label).unwrap();
        Ok(())
    }

    // Helper: binary operation
    fn emit_binary_operation(
        &mut self,
        left_expr: &dyn crate::ast::ExpressionNode,
        right_expr: &dyn crate::ast::ExpressionNode,
        operator: &str,
    ) -> Result<String, String> {
        let left_result = self.emit_expression(left_expr)?;
        let right_result = self.emit_expression(right_expr)?;
        let result_vreg = self.allocate_virtual_register();
        let op = match operator {
            "Plus" => "add",
            "Minus" => "sub",
            "Multiply" => "mul",
            "Divide" => "div",
            _ => return Err(format!("Unknown operator: {}", operator)),
        };
        self.emit_binary_op(op, &result_vreg, &left_result, &right_result);
        Ok(result_vreg)
    }

    // Helper: relational operation
    fn emit_relational_operation(
        &mut self,
        left_expr: &dyn crate::ast::ExpressionNode,
        right_expr: &dyn crate::ast::ExpressionNode,
        operator: &str,
    ) -> Result<String, String> {
        let left_result = self.emit_expression(left_expr)?;
        let right_result = self.emit_expression(right_expr)?;
        let result_vreg = self.allocate_virtual_register();
        let op = match operator {
            "GreaterThan" => "cmp_gt",
            "LessThan" => "cmp_lt",
            "Equal" => "cmp_eq",
            "GreaterThanEqual" => "cmp_ge",
            "LessThanEqual" => "cmp_le",
            "!=" | "Hash" => "cmp_ne",
            _ => return Err(format!("Unknown relational operator: {}", operator)),
        };
        self.emit_relational_op(op, &result_vreg, &left_result, &right_result);
        Ok(result_vreg)
    }

}

impl ASTVisitor for IRGenerator {
    fn visit_ident(&mut self, ident: &Ident) -> Result<String, String> {
        let symbol = self
            .symbol_table
            .get(&ident.value)
            .ok_or_else(|| format!("Undefined identifier: {}", ident.value))?
            .clone();
        match symbol.symbol_type {
            SymbolType::Constant(value) => {
                let vreg = self.allocate_virtual_register();
                write!(self.text_output, "    li {}, {}\n", vreg, value).unwrap();
                Ok(vreg)
            }
            SymbolType::Procedure => {
                let vreg = self.allocate_virtual_register();
                match &symbol.location {
                    SymbolLocation::GlobalLabel(label) => {
                        write!(self.text_output, "    la {}, {}\n", vreg, label).unwrap();
                    }
                    _ => {
                        return Err(format!("Procedure {} has no valid address", ident.value));
                    }
                }
                Ok(vreg)
            }
            SymbolType::Variable => {
                // Use optimized variable loading for variables
                let vreg = self.get_or_load_variable(&ident.value)?;
                // If the variable was already loaded, we need to ensure the register is properly tracked
                // for the expression evaluation system
                if !vreg.starts_with(&self.vreg_prefix) {
                    // This shouldn't happen, but just in case
                    return Err(format!("Invalid register format: {}", vreg));
                }
                Ok(vreg)
            }
            _ => {
                // For other types, use the helper function
                let vreg = self.allocate_virtual_register();
                self.emit_load_from_symbol(&symbol, &vreg, &ident.value)?;
                Ok(vreg)
            }
        }
    }

    fn visit_number(&mut self, number: &Number) -> Result<String, String> {
        let vreg = self.allocate_virtual_register();
        write!(self.text_output, "    li {}, {}\n", vreg, number.value).unwrap();
        Ok(vreg)
    }

    fn visit_variable(&mut self, variable: &Variable) -> Result<String, String> {
        // Use the optimized variable loading and ensure the register is properly tracked
        let vreg = self.get_or_load_variable(&variable.name)?;
        // The register is already allocated and tracked, so we don't need to do anything else
        Ok(vreg)
    }

    fn visit_binary_operation(&mut self, binop: &BinOp) -> Result<String, String> {
        let left_vreg = binop
            .left
            .as_ref()
            .ok_or_else(|| "Binary operation missing left operand".to_string())?;
        let right_vreg = binop
            .right
            .as_ref()
            .ok_or_else(|| "Binary operation missing right operand".to_string())?;
        self.emit_binary_operation(left_vreg.as_ref(), right_vreg.as_ref(), &binop.operator)
    }

    fn visit_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), String> {
        let start_label = self.create_and_emit_label();
        let end_label = self.create_label();
        let condition = stmt
            .condition
            .as_ref()
            .ok_or_else(|| "While statement missing condition".to_string())?;
        let cond_vreg = self.evaluate_condition(condition.as_ref())?;
        self.emit_conditional_branch(&cond_vreg, &end_label);
        let body = stmt
            .body
            .as_ref()
            .ok_or_else(|| "While statement missing body".to_string())?;
        body.accept(self)?;
        self.emit_jump(&start_label);
        self.emit_label(&end_label);
        Ok(())
    }

    fn visit_condition(&mut self, cond: &OddCondition) -> Result<String, String> {
        let expr = cond
            .expr
            .as_ref()
            .ok_or_else(|| "OddCondition missing expression".to_string())?;
        let num_reg = self.emit_expression(expr.as_ref())?;
        let result_reg = self.allocate_virtual_register();
        write!(self.text_output, "    is_odd {}, {}\n", result_reg, num_reg).unwrap();
        Ok(result_reg)
    }

    fn visit_relational_condition(&mut self, cond: &RelationalCondition) -> Result<String, String> {
        let left = cond
            .left
            .as_ref()
            .ok_or_else(|| "Relational condition missing left operand".to_string())?;
        let right = cond
            .right
            .as_ref()
            .ok_or_else(|| "Relational condition missing right operand".to_string())?;
        self.emit_relational_operation(left.as_ref(), right.as_ref(), &cond.operator)
    }

    fn visit_call(&mut self, call: &CallStmt) -> Result<(), String> {
        self.emit_procedure_call(&call.identifier)
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
        let cond_vreg = self.evaluate_condition(condition.as_ref())?;
        self.emit_conditional_branch(&cond_vreg, &else_label);
        let then_branch = expr
            .then_branch
            .as_ref()
            .ok_or_else(|| "If statement missing then branch".to_string())?;
        then_branch.accept(self)?;
        if let Some(ref else_branch) = expr.else_branch {
            self.emit_jump(&end_label);
            self.emit_label(&else_label);
            else_branch.accept(self)?;
            self.emit_label(&end_label);
        } else {
            self.emit_jump(&end_label);
            self.emit_label(&else_label);
            self.emit_label(&end_label);
        }
        Ok(())
    }

    fn visit_write_int(&mut self, stmt: &WriteInt) -> Result<(), String> {
        self.emit_write_operation(
            stmt.expr.as_ref().map(|e| e.as_ref()),
            "WriteInt statement missing expression",
            |generator, vreg| {
                write!(generator.text_output, "    write_int {}\n", vreg).unwrap();
            },
        )
    }

    fn visit_write_char(&mut self, stmt: &WriteChar) -> Result<(), String> {
        self.emit_write_operation(
            stmt.expr.as_ref().map(|e| e.as_ref()),
            "WriteChar statement missing expression",
            |generator, vreg| {
                write!(generator.text_output, "    write_char {}\n", vreg).unwrap();
            },
        )
    }

    fn visit_write_str(&mut self, stmt: &WriteStr) -> Result<(), String> {
        if stmt.expr.is_empty() {
            return Err("WriteStr statement missing expression".to_string());
        }
        write!(self.text_output, "    write_str \"{}\"\n", stmt.expr).unwrap();
        Ok(())
    }

    fn visit_read_int(&mut self, expr: &ReadInt) -> Result<(), String> {
        self.emit_read_operation("read_int", &expr.identifier)
    }

    fn visit_read_char(&mut self, expr: &ReadChar) -> Result<(), String> {
        self.emit_read_operation("read_char", &expr.identifier)
    }

    fn visit_exit(&mut self, _expr: &Exit) -> Result<(), String> {
        self.exit_emitted = true;
        write!(self.text_output, "    exit 0\n").unwrap();
        Ok(())
    }

    fn visit_const(&mut self, expr: &ConstDecl) -> Result<(), String> {
        for (id, num) in &expr.const_decl {
            write!(self.data_output, "const {} = {}\n", id, num).unwrap();
            self.update_symbol_location(id, SymbolLocation::GlobalLabel(id.clone()), true);
        }
        Ok(())
    }

    fn visit_var_decl(&mut self, expr: &VarDecl) -> Result<(), String> {
        for var_name in &expr.var_decl {
            // Generate IR variable declaration
            write!(self.bss_output, "var {}\n", var_name).unwrap();
            // Update the symbol table
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
            write!(self.text_output, "{}:\n", name).unwrap();
            // Calculate stack size for this procedure
            let mut stack_slots = 0;
            if let Some(block) = proc_block {
                // Downcast to Block to access var_decl
                if let Some(block) = block.as_any().downcast_ref::<crate::block::Block>() {
                    stack_slots = block.var_decl.var_decl.len();
                }
            }
            let stack_size = ((stack_slots * 8 + 15) / 16) * 16;
            write!(self.text_output, "    proc_enter {}\n", stack_size).unwrap();
            if let Some(block) = proc_block {
                self.in_procedure = true;
                block.accept(self)?;
                self.in_procedure = false;
            }
            write!(self.text_output, "    proc_exit\n").unwrap();
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
            if !self.in_procedure && !self.main_emitted {
                write!(self.text_output, "main:\n").unwrap();
                self.main_emitted = true;
            }
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
