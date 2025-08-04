use crate::{
    ast::{Exit, ExpressionNode, Node, Variable},
    block::Block,
    code_emitter::{CodeEmitter, StringCodeEmitter},
    decl::{ConstDecl, ProcDecl, VarDecl},
    expression::{BinOp, OddCondition, RelationalCondition},
    io::{ReadChar, ReadInt, WriteChar, WriteInt, WriteStr},
    program::Program,
    register_allocator_common::RegisterError,
    statement::{AssignStmt, BeginStmt, CallStmt, IfStmt, WhileStatement},
    symboltable::{Symbol, SymbolLocation, SymbolTable, SymbolType},
    types::{Ident, Number},
    visiters::ASTVisitor,
};

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
    current_scope_level: usize,
    local_var_offset: isize,
}

impl IRGenerator {
    // ---------------------------
    // Constructor
    // ---------------------------
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
            current_scope_level: 0,
            local_var_offset: 8,
        }
    }

    // ---------------------------
    // Virtual Register & Label Helpers
    // ---------------------------
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

    // ---------------------------
    // Output & Main Code Helpers
    // ---------------------------
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
            .accept(self)
            .map_err(Self::to_string_error)?;
        if !self.exit_emitted {
            self.system_exit(0).map_err(Self::to_string_error)?;
        }
        Ok(())
    }

    // ---------------------------
    // Expression & System Helpers
    // ---------------------------
    fn emit_expression(&mut self, expr: &dyn crate::ast::ExpressionNode) -> Result<String, String> {
        ExpressionNode::accept(expr, self)
    }

    fn system_exit(&mut self, code: i32) -> Result<(), RegisterError> {
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        emitter.emit_exit(code)
    }

    // ---------------------------
    // Symbol Table Helpers
    // ---------------------------
    fn get_symbol_with_type(&self, name: &str, expected_type: SymbolType, operation: &str) -> Result<&Symbol, String> {
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

    fn update_symbol_location(&mut self, name: &str, location: SymbolLocation, is_global: bool) {
        if let Some(symbol) = self.symbol_table.get_mut(name) {
            symbol.location = location;
            symbol.is_global = is_global;
            if is_global {
                symbol.initialized = true;
            }
        }
    }

    fn get_variable_symbol(&self, name: &str, operation: &str) -> Result<&Symbol, String> {
        let symbol = self
            .symbol_table
            .get_at_level(name, self.current_scope_level)
            .ok_or_else(|| format!("Undefined {}: {}", operation, name))?;
        if !matches!(symbol.symbol_type, SymbolType::Variable) {
            return Err(format!(
                "Expected variable but found {:?}: {}",
                symbol.symbol_type, name
            ));
        }
        Ok(symbol)
    }

    fn get_procedure_symbol(&self, name: &str, operation: &str) -> Result<&Symbol, String> {
        self.get_symbol_with_type(name, SymbolType::Procedure, operation)
    }

    fn get_or_load_variable(&mut self, variable_name: &str) -> Result<String, String> {
        // Always load from memory for correctness
        let symbol = self.get_variable_symbol(variable_name, "variable")?.clone();
        let vreg = self.allocate_virtual_register();
        self.emit_load_from_symbol(&symbol, &vreg, variable_name)
            .map_err(Self::to_string_error)?;
        Ok(vreg)
    }

    // ---------------------------
    // IR Emission Helpers
    // ---------------------------
    // Helper function to generate load instruction based on symbol location
    fn emit_load_from_symbol(&mut self, symbol: &Symbol, target_vreg: &str, fallback_name: &str) -> Result<(), RegisterError> {
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        let distance = if self.current_scope_level > symbol.level {
            self.current_scope_level - symbol.level
        } else {
            0
        };
        match &symbol.location {
            SymbolLocation::StackOffset(offset) => {
                if distance > 0 {
                    emitter.emit_ld(target_vreg, &format!("up-{}-{}", offset, distance))
                } else {
                    emitter.emit_ld(target_vreg, &format!("bp-{}", offset))
                }
            }
            SymbolLocation::GlobalLabel(label) => emitter.emit_ld(target_vreg, label),
            SymbolLocation::Immediate(value) => emitter.emit_li(target_vreg, &value.to_string()),
            SymbolLocation::None => emitter.emit_ld(target_vreg, fallback_name),
        }
    }

    // Helper function to generate store instruction based on symbol location
    fn emit_store_to_symbol(&mut self, symbol: &Symbol, source_vreg: &str, fallback_name: &str) -> Result<(), RegisterError> {
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        let distance = if self.current_scope_level > symbol.level {
            self.current_scope_level - symbol.level
        } else {
            0
        };
        match &symbol.location {
            SymbolLocation::StackOffset(offset) => {
                if distance > 0 {
                    emitter.emit_st(&format!("up-{}-{}", offset, distance), source_vreg)
                } else {
                    emitter.emit_st(&format!("bp-{}", offset), source_vreg)
                }
            }
            SymbolLocation::GlobalLabel(label) => emitter.emit_st(label, source_vreg),
            SymbolLocation::None => emitter.emit_st(fallback_name, source_vreg),
            SymbolLocation::Immediate(_) => {
                return Err(RegisterError::OutputError(format!(
                    "Cannot store to immediate value: {}",
                    fallback_name
                )));
            }
        }
    }

    fn emit_binary_op(&mut self, op: &str, dest: &str, left: &str, right: &str) -> Result<(), RegisterError> {
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        match op {
            "add" => emitter.emit_add(dest, left, right),
            "sub" => emitter.emit_sub(dest, left, right),
            "mul" => emitter.emit_mul(dest, left, right),
            "div" => emitter.emit_div(dest, left, right),
            "mod" => emitter.emit_mod(dest, left, right),
            _ => Err(RegisterError::InvalidInstruction(format!(
                "Unknown binary op: {}",
                op
            ))),
        }
    }

    fn emit_relational_op(&mut self, op: &str, dest: &str, left: &str, right: &str) -> Result<(), RegisterError> {
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        match op {
            "cmp_gt" => emitter.emit_cmp_gt(dest, left, right),
            "cmp_lt" => emitter.emit_cmp_lt(dest, left, right),
            "cmp_eq" => emitter.emit_cmp_eq(dest, left, right),
            "cmp_ne" => emitter.emit_cmp_ne(dest, left, right),
            "cmp_ge" => emitter.emit_cmp_ge(dest, left, right),
            "cmp_le" => emitter.emit_cmp_le(dest, left, right),
            _ => Err(RegisterError::InvalidInstruction(format!(
                "Unknown relational op: {}",
                op
            ))),
        }
    }

    fn emit_branch_if_zero(&mut self, vreg: &str, label: &str) -> Result<(), RegisterError> {
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        emitter.emit_beqz(vreg, label)
    }

    fn emit_jump(&mut self, label: &str) -> Result<(), RegisterError> {
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        emitter.emit_jump(label)
    }

    // Helper: emit a label
    fn emit_label(&mut self, label: &str) -> Result<(), RegisterError> {
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        emitter.emit_label(label)
    }

    // Helper: create and emit a label, returning its name
    fn create_and_emit_label(&mut self) -> Result<String, RegisterError> {
        let label = self.create_label();
        self.emit_label(&label)?;
        Ok(label)
    }

    // Helper: evaluate a condition expression
    fn evaluate_condition(&mut self, condition: &dyn ExpressionNode) -> Result<String, String> {
        self.emit_expression(condition)
    }

    // Helper: conditional branch
    fn emit_conditional_branch(&mut self, vreg: &str, label: &str) -> Result<(), RegisterError> {
        self.emit_branch_if_zero(vreg, label)
    }

    // Helper: write operation
    fn emit_write_operation<F>(&mut self, expr_opt: Option<&dyn ExpressionNode>, error_msg: &str, write_fn: F) -> Result<(), String>
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
    fn emit_read_operation(&mut self, operation: &str, identifier: &str) -> Result<(), RegisterError> {
        let symbol = self
            .get_variable_symbol(identifier, "variable in read")
            .map_err(Self::to_output_error)?
            .clone();
        let vreg = self.allocate_virtual_register();
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        match operation {
            "read_int" => emitter.emit_read_int(&vreg)?,
            "read_char" => emitter.emit_read_char(&vreg)?,
            _ => {
                return Err(RegisterError::InvalidInstruction(format!(
                    "Unknown read operation: {}",
                    operation
                )))
            }
        }
        self.emit_store_to_symbol(&symbol, &vreg, identifier)?;
        Ok(())
    }

    // Helper: procedure call
    fn emit_procedure_call(&mut self, identifier: &str) -> Result<(), RegisterError> {
        let symbol = self
            .get_procedure_symbol(identifier, "procedure")
            .map_err(Self::to_output_error)?;
        let label = match &symbol.location {
            SymbolLocation::GlobalLabel(label) => label.clone(),
            SymbolLocation::None => identifier.to_string(),
            _ => {
                return Err(RegisterError::OutputError(format!(
                    "Procedure {} has no valid address",
                    identifier
                )))
            }
        };
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        emitter.emit_call(&label)
    }

    fn emit_binary_operation(&mut self, left_expr: &dyn ExpressionNode, right_expr: &dyn ExpressionNode, operator: &str) -> Result<String, RegisterError> {
        let left_result = self
            .emit_expression(left_expr)
            .map_err(Self::to_output_error)?;
        let right_result = self
            .emit_expression(right_expr)
            .map_err(Self::to_output_error)?;
        let result_vreg = self.allocate_virtual_register();
        let op = match operator {
            "Plus" => "add",
            "Minus" => "sub",
            "Multiply" => "mul",
            "Divide" => "div",
            "Modulo" => "mod",
            _ => {
                return Err(RegisterError::InvalidInstruction(format!(
                    "Unknown operator: {}",
                    operator
                )))
            }
        };
        self.emit_binary_op(op, &result_vreg, &left_result, &right_result)?;
        Ok(result_vreg)
    }

    fn emit_relational_operation(&mut self, left_expr: &dyn ExpressionNode, right_expr: &dyn ExpressionNode, operator: &str) -> Result<String, RegisterError> {
        let left_result = self
            .emit_expression(left_expr)
            .map_err(Self::to_output_error)?;
        let right_result = self
            .emit_expression(right_expr)
            .map_err(Self::to_output_error)?;
        let result_vreg = self.allocate_virtual_register();
        let op = match operator {
            "GreaterThan" => "cmp_gt",
            "LessThan" => "cmp_lt",
            "Equal" => "cmp_eq",
            "GreaterThanEqual" => "cmp_ge",
            "LessThanEqual" => "cmp_le",
            "!=" | "Hash" => "cmp_ne",
            _ => {
                return Err(RegisterError::InvalidInstruction(format!(
                    "Unknown relational operator: {}",
                    operator
                )))
            }
        };
        self.emit_relational_op(op, &result_vreg, &left_result, &right_result)?;
        Ok(result_vreg)
    }

    // ---------------------------
    // Procedure & Block Helpers
    // ---------------------------
    // Collect all procedures (including nested ones) from a block
    fn collect_all_procedures<'a>(&self, block: &'a Block, procedures: &mut Vec<(String, &'a Option<Box<dyn Node>>)>) {
        // Add procedures from this block
        for (name, proc_block) in &block.proc_decl.procedurs {
            procedures.push((name.clone(), proc_block));
            // Recursively collect nested procedures
            if let Some(proc_block) = proc_block {
                if let Some(nested_block) =
                    proc_block.as_any().downcast_ref::<crate::block::Block>()
                {
                    self.collect_all_procedures(nested_block, procedures);
                }
            }
        }
    }

    // Emit all procedures at the top level (no nesting in IR)
    fn emit_procedures(&mut self, block: &Block) -> Result<(), RegisterError> {
        let mut all_procedures = Vec::new();
        self.collect_all_procedures(block, &mut all_procedures);
        for (name, proc_block) in all_procedures {
            self.update_symbol_location(&name, SymbolLocation::GlobalLabel(name.clone()), true);
            self.emit_label(&name)?;
            // Set scope level based on procedure's Level in symbol table
            let proc_symbol = self.symbol_table.get(&name).ok_or_else(|| {
                RegisterError::OutputError(format!("Procedure {} not found in symbol table", name))
            })?;
            self.current_scope_level = proc_symbol.level + 1;
            self.local_var_offset = 16;
            let mut stack_slots = 0;
            if let Some(proc_block) = proc_block {
                if let Some(block) = proc_block.as_any().downcast_ref::<crate::block::Block>() {
                    stack_slots = block.var_decl.var_decl.len();
                }
            }
            let stack_size = ((stack_slots * 8 + 8 + 15) / 16) * 16;
            let mut emitter = StringCodeEmitter::new(&mut self.text_output);
            emitter.emit_proc_enter(stack_size)?;
            if let Some(proc_block) = proc_block {
                self.in_procedure = true;
                if let Some(block) = proc_block.as_any().downcast_ref::<crate::block::Block>() {
                    if !block.var_decl.var_decl.is_empty() {
                        block.var_decl.accept(self).map_err(Self::to_output_error)?;
                    }
                    if let Some(stmt) = &block.statement {
                        stmt.accept(self).map_err(Self::to_output_error)?;
                    }
                }
                self.in_procedure = false;
            }
            let mut emitter = StringCodeEmitter::new(&mut self.text_output);
            emitter.emit_proc_exit()?;
        }
        Ok(())
    }

    // ---------------------------
    // Error Helpers
    // ---------------------------
    fn to_output_error<E: std::fmt::Display>(e: E) -> RegisterError {
        RegisterError::OutputError(e.to_string())
    }

    fn to_string_error<E: std::fmt::Display>(e: E) -> String {
        e.to_string()
    }
}

// ---------------------------
// ASTVisitor Implementation
// ---------------------------
impl ASTVisitor for IRGenerator {
    fn visit_ident(&mut self, ident: &Ident) -> Result<String, String> {
        let symbol = self
            .symbol_table
            .get_at_level(&ident.value, self.current_scope_level)
            .ok_or_else(|| format!("Undefined identifier: {}", ident.value))?
            .clone();
        match symbol.symbol_type {
            SymbolType::Constant(value) => {
                let vreg = self.allocate_virtual_register();
                let mut emitter = StringCodeEmitter::new(&mut self.text_output);
                emitter
                    .emit_li(&vreg, &value.to_string())
                    .map_err(|e| e.to_string())?;
                Ok(vreg)
            }
            SymbolType::Procedure => {
                let vreg = self.allocate_virtual_register();
                match &symbol.location {
                    SymbolLocation::GlobalLabel(label) => {
                        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
                        emitter
                            .emit(&format!("la {}, {}", vreg, label))
                            .map_err(|e| e.to_string())?;
                    }
                    _ => {
                        return Err(format!("Procedure {} has no valid address", ident.value));
                    }
                }
                Ok(vreg)
            }
            SymbolType::Variable => {
                let vreg = self.get_or_load_variable(&ident.value)?;
                if !vreg.starts_with(&self.vreg_prefix) {
                    return Err(format!("Invalid register format: {}", vreg));
                }
                Ok(vreg)
            }
            _ => {
                // For other types, use the helper function
                let vreg = self.allocate_virtual_register();
                self.emit_load_from_symbol(&symbol, &vreg, &ident.value)
                    .map_err(|e| e.to_string())?;
                Ok(vreg)
            }
        }
    }

    fn visit_number(&mut self, number: &Number) -> Result<String, String> {
        let vreg = self.allocate_virtual_register();
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        emitter
            .emit_li(&vreg, &number.value.to_string())
            .map_err(|e| e.to_string())?;
        Ok(vreg)
    }

    fn visit_variable(&mut self, variable: &Variable) -> Result<String, String> {
        let vreg = self.get_or_load_variable(&variable.name)?;
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
            .map_err(|e| e.to_string())
    }

    fn visit_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), String> {
        let start_label = self.create_and_emit_label().map_err(|e| e.to_string())?;
        let end_label = self.create_label();
        let condition = stmt
            .condition
            .as_ref()
            .ok_or_else(|| "While statement missing condition".to_string())?;
        let cond_vreg = self.evaluate_condition(condition.as_ref())?;
        self.emit_conditional_branch(&cond_vreg, &end_label)
            .map_err(|e| e.to_string())?;
        let body = stmt
            .body
            .as_ref()
            .ok_or_else(|| "While statement missing body".to_string())?;
        body.accept(self)?;
        self.emit_jump(&start_label).map_err(|e| e.to_string())?;
        self.emit_label(&end_label).map_err(|e| e.to_string())?;
        Ok(())
    }

    fn visit_condition(&mut self, cond: &OddCondition) -> Result<String, String> {
        let expr = cond
            .expr
            .as_ref()
            .ok_or_else(|| "OddCondition missing expression".to_string())?;
        let num_reg = self.emit_expression(expr.as_ref())?;
        let result_reg = self.allocate_virtual_register();
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        emitter
            .emit_is_odd(&result_reg, &num_reg)
            .map_err(|e| e.to_string())?;
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
            .map_err(|e| e.to_string())
    }

    fn visit_call(&mut self, call: &CallStmt) -> Result<(), String> {
        self.emit_procedure_call(&call.identifier)
            .map_err(|e| e.to_string())
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
        self.emit_store_to_symbol(&symbol, &vreg, &stmt.identifier)
            .map_err(|e| e.to_string())?;
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
        self.emit_conditional_branch(&cond_vreg, &else_label)
            .map_err(|e| e.to_string())?;
        let then_branch = expr
            .then_branch
            .as_ref()
            .ok_or_else(|| "If statement missing then branch".to_string())?;
        then_branch.accept(self)?;
        if let Some(ref else_branch) = expr.else_branch {
            self.emit_jump(&end_label).map_err(|e| e.to_string())?;
            self.emit_label(&else_label).map_err(|e| e.to_string())?;
            else_branch.accept(self)?;
            self.emit_label(&end_label).map_err(|e| e.to_string())?;
        } else {
            self.emit_jump(&end_label).map_err(|e| e.to_string())?;
            self.emit_label(&else_label).map_err(|e| e.to_string())?;
            self.emit_label(&end_label).map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    fn visit_write_int(&mut self, stmt: &WriteInt) -> Result<(), String> {
        if let Some(expr) = stmt.expr.as_ref() {
            let vreg = self.emit_expression(expr.as_ref())?;
            let mut emitter = StringCodeEmitter::new(&mut self.text_output);
            emitter.emit_write_int(&vreg).map_err(|e| e.to_string())?;
            Ok(())
        } else {
            Err("WriteInt statement missing expression".to_string())
        }
    }

    fn visit_write_char(&mut self, stmt: &WriteChar) -> Result<(), String> {
        self.emit_write_operation(
            stmt.expr.as_ref().map(|e| e.as_ref()),
            "WriteChar statement missing expression",
            |generator, vreg| {
                let mut emitter = StringCodeEmitter::new(&mut generator.text_output);
                emitter.emit_write_char(vreg).unwrap();
            },
        )
    }

    fn visit_write_str(&mut self, stmt: &WriteStr) -> Result<(), String> {
        if stmt.expr.is_empty() {
            return Err("WriteStr statement missing expression".to_string());
        }
        let mut emitter = StringCodeEmitter::new(&mut self.text_output);
        emitter.emit_write_str(&stmt.expr).unwrap();
        Ok(())
    }

    fn visit_read_int(&mut self, expr: &ReadInt) -> Result<(), String> {
        self.emit_read_operation("read_int", &expr.identifier)
            .map_err(|e| e.to_string())
    }

    fn visit_read_char(&mut self, expr: &ReadChar) -> Result<(), String> {
        self.emit_read_operation("read_char", &expr.identifier)
            .map_err(|e| e.to_string())
    }

    fn visit_exit(&mut self, _expr: &Exit) -> Result<(), String> {
        self.exit_emitted = true;
        self.system_exit(0).map_err(|e| e.to_string())?;
        Ok(())
    }

    fn visit_const(&mut self, expr: &ConstDecl) -> Result<(), String> {
        for (id, num) in &expr.const_decl {
            let mut emitter = StringCodeEmitter::new(&mut self.data_output);
            emitter.emit_const(id, &num.to_string()).unwrap();
            self.update_symbol_location(id, SymbolLocation::Immediate(*num), true);
        }
        Ok(())
    }

    fn visit_var_decl(&mut self, expr: &VarDecl) -> Result<(), String> {
        for var_name in &expr.var_decl {
            if self.in_procedure {
                // Local variable in procedure - use stack offset
                let offset = self.local_var_offset;
                self.local_var_offset += 8; // 8 bytes per variable
                self.update_symbol_location(
                    var_name, // Keep original name for symbol lookup
                    SymbolLocation::StackOffset(offset),
                    false, // Not global
                );
            } else {
                // Global variable
                let mut emitter = StringCodeEmitter::new(&mut self.bss_output);
                emitter.emit_var(var_name).unwrap();
                self.update_symbol_location(
                    var_name,
                    SymbolLocation::GlobalLabel(var_name.clone()),
                    true, // Global
                );
            }
        }
        Ok(())
    }

    fn visit_proc_decl(&mut self, expr: &ProcDecl) -> Result<(), String> {
        if !self.in_procedure {
            for (name, proc_block) in &expr.procedurs {
                self.update_symbol_location(name, SymbolLocation::GlobalLabel(name.clone()), true);
                self.emit_label(name).map_err(|e| e.to_string())?;
                // Set scope level based on procedure's Level in symbol table
                let proc_symbol = self
                    .symbol_table
                    .get_at_level(name, 0)
                    .ok_or_else(|| format!("Procedure {} not found in symbol table", name))?;
                self.current_scope_level = proc_symbol.level + 1;
                self.local_var_offset = 16;
                let mut stack_slots = 0;
                if let Some(block) = proc_block {
                    if let Some(block) = block.as_any().downcast_ref::<crate::block::Block>() {
                        stack_slots = block.var_decl.var_decl.len();
                    }
                }
                let stack_size = ((stack_slots * 8 + 8 + 15) / 16) * 16 + 8;
                let mut emitter = StringCodeEmitter::new(&mut self.text_output);
                emitter
                    .emit_proc_enter(stack_size)
                    .map_err(|e| e.to_string())?;
                if let Some(block) = proc_block {
                    self.in_procedure = true;
                    block.accept(self)?;
                    self.in_procedure = false;
                }
                let mut emitter = StringCodeEmitter::new(&mut self.text_output);
                emitter.emit_proc_exit().map_err(|e| e.to_string())?;
            }
        }
        Ok(())
    }

    fn visit_block(&mut self, block: &Block) -> Result<(), String> {
        if !self.in_procedure {
            self.local_var_offset = 16; // Reset stack offset for main
            self.current_scope_level = 0;
        }
        if !block.const_decl.const_decl.is_empty() {
            block.const_decl.accept(self)?;
        }
        if !block.var_decl.var_decl.is_empty() {
            block.var_decl.accept(self)?;
        }

        // Recursively emit all procedures at the top level
        if !self.in_procedure {
            let original_scope_level = self.current_scope_level;
            self.emit_procedures(block)
                .map_err(|e| e.to_string())?;
            self.current_scope_level = original_scope_level;
        }

        if let Some(stmt) = &block.statement {
            if !self.in_procedure && !self.main_emitted {
                self.emit_label("main").map_err(|e| e.to_string())?;
                self.main_emitted = true;
                // Emit stack variables for local variables
                let mut stack_vars = Vec::new();
                for symbol in self.symbol_table.all_symbols() {
                    if let SymbolLocation::StackOffset(offset) = symbol.location {
                        if !symbol.is_global
                            && matches!(symbol.symbol_type, SymbolType::Variable)
                            && self.in_procedure
                        {
                            stack_vars.push(offset);
                        }
                    }
                }
                stack_vars.sort();
                stack_vars.dedup();
                for offset in stack_vars {
                    let vreg = self.allocate_virtual_register();
                    let mut emitter = StringCodeEmitter::new(&mut self.text_output);
                    emitter.emit_li(&vreg, "0").unwrap();
                    emitter.emit_st(&format!("bp-{}", offset), &vreg).unwrap();
                }
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
