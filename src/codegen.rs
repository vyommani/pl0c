use crate::{
    ast::{Exit, ExpressionNode, Node, Variable},
    block::Block,
    code_emitter::{CodeEmitter, StringCodeEmitter},
    decl::{ConstDecl, ProcDecl, VarDecl},
    expression::{BinOp, OddCondition, RelationalCondition},
    io::{Read, Write, WriteStr},
    program::Program,
    statement::{AssignStmt, BeginStmt, CallStmt, IfStmt, WhileStatement},
    symboltable::{Symbol, SymbolLocation, SymbolTable, SymbolType},
    types::{Ident, Number},
    visiters::ASTVisitor,
    errors::Pl0Result,
    errors::Pl0Error,
};
use crate::scope_info::ScopeInfo;
use crate::config::codegen::{WORD_SIZE, STACK_ALIGNMENT};

pub struct IRGenerator {
    label_counter: i32,
    vreg_counter: i32,
    constants: String,
    variables: String,
    code: String,
    vreg_prefix: String,
    symbol_table: SymbolTable,
    exit_emitted: bool,
    main_emitted: bool,
    procedures_emitted: bool,
    scope: ScopeInfo,
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
        let mut output = String::with_capacity(
            self.constants.len() + self.variables.len() + self.code.len() + 10
        );
        // Add constants as IR data declarations
        if !self.constants.is_empty() {
            output.push_str(&self.constants);
            if !self.constants.ends_with('\n') {
                output.push('\n');
            }
        }
        // Add global variables as IR data declarations
        if !self.variables.is_empty() {
            output.push_str(&self.variables);
            if !self.variables.ends_with('\n') {
                output.push('\n');
            }
        }
        // Add the main IR code
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

    // ---------------------------
    // Expression & System Helpers
    // ---------------------------
    fn emit_expression(&mut self, expr: &dyn crate::ast::ExpressionNode) -> Pl0Result<String> {
        ExpressionNode::accept(expr, self)
    }

    fn system_exit(&mut self, code: i32) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_exit(code)
    }

    // ---------------------------
    // Symbol Table Helpers
    // ---------------------------
    fn get_symbol_with_type(&self, name: &str, expected_type: SymbolType, operation: &str) -> Pl0Result<&Symbol> {
        let symbol = self.symbol_table.get(name).ok_or_else(|| Pl0Error::codegen_error(format!("Undefined {}: {}", operation, name)))?;
        match (&symbol.symbol_type, &expected_type) {
            (SymbolType::Variable, SymbolType::Variable) |
            (SymbolType::Procedure, SymbolType::Procedure) => Ok(symbol),
            (SymbolType::Constant(_), SymbolType::Constant(_)) => Ok(symbol),
            _ => Err(Pl0Error::codegen_error(format!("Expected {:?} but found {:?} for {}", expected_type, symbol.symbol_type, name)))
        }
    }

    fn get_variable_symbol(&self, name: &str, operation: &str) -> Pl0Result<&Symbol> {
        let symbol = self.symbol_table.get_at_level(name, self.scope.level())
            .ok_or_else(|| Pl0Error::codegen_error(format!("Undefined {}: {}", operation, name)))?;
        if !matches!(symbol.symbol_type, SymbolType::Variable) {
            return Err(Pl0Error::codegen_error(format!("Expected variable but found {:?}: {}", symbol.symbol_type, name)));
        }
        Ok(symbol)
    }

    fn get_procedure_symbol(&self, name: &str, operation: &str) -> Pl0Result<&Symbol> {
        self.get_symbol_with_type(name, SymbolType::Procedure, operation)
    }

    fn get_or_load_variable(&mut self, variable_name: &str) -> Pl0Result<String> {
        // Always load from memory for correctness
        let symbol = self.get_variable_symbol(variable_name, "variable")?.clone();
        let vreg = self.allocate_virtual_register();
        self.emit_load_from_symbol(&symbol, &vreg, variable_name)?;
        Ok(vreg)
    }

    // ---------------------------
    // IR Emission Helpers
    // ---------------------------
    fn emit_load_from_symbol(&mut self, symbol: &Symbol, target_vreg: &str, fallback_name: &str) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        let distance = self.scope.level().saturating_sub(symbol.level);
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

    fn emit_store_to_symbol(&mut self, symbol: &Symbol, source_vreg: &str, fallback_name: &str) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        let distance = self.scope.level().saturating_sub(symbol.level);
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
                Err(Pl0Error::codegen_error(format!("Cannot store to immediate value: {}", fallback_name)))
            }
        }
    }

    // ---------------------------
    // Stack Management Helpers
    // ---------------------------
    fn calculate_stack_size(stack_slots: usize) -> usize {
        let base_size = stack_slots * WORD_SIZE + WORD_SIZE; // +8 for return address
        (base_size + STACK_ALIGNMENT - 1) & !(STACK_ALIGNMENT - 1)
    }

    fn initialize_local_variables(&mut self, block: &Block) -> Pl0Result<()> {
        if !block.var_decl.var_decl.is_empty() {
            // Collect all the variables and their offsets first
            let mut var_offsets = Vec::new();
            for var_name in &block.var_decl.var_decl {
                if let Some(symbol) = self.symbol_table.get_at_level(var_name, self.scope.level()) {
                    if let SymbolLocation::StackOffset(offset) = symbol.location {
                        var_offsets.push(offset);
                    }
                }
            }
            // Now emit the initialization code
            for offset in var_offsets {
                let zero_reg = self.allocate_virtual_register();
                let mut emitter = StringCodeEmitter::new(&mut self.code);
                emitter.emit_li(&zero_reg, "0")?;
                emitter.emit_st(&format!("bp-{}", offset), &zero_reg)?;
            }
        }
        Ok(())
    }

    // ---------------------------
    // Binary and Relational Operations
    // ---------------------------
    fn emit_binary_op(&mut self, op: &str, dest: &str, left: &str, right: &str) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        match op {
            "add" => emitter.emit_add(dest, left, right),
            "sub" => emitter.emit_sub(dest, left, right),
            "mul" => emitter.emit_mul(dest, left, right),
            "div" => emitter.emit_div(dest, left, right),
            "mod" => emitter.emit_mod(dest, left, right),
            _ => Err(Pl0Error::codegen_error(format!("Unknown binary op: {}", op))),
        }
    }

    fn emit_relational_op(&mut self, op: &str, dest: &str, left: &str, right: &str) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        match op {
            "cmp_gt" => emitter.emit_cmp_gt(dest, left, right),
            "cmp_lt" => emitter.emit_cmp_lt(dest, left, right),
            "cmp_eq" => emitter.emit_cmp_eq(dest, left, right),
            "cmp_ne" => emitter.emit_cmp_ne(dest, left, right),
            "cmp_ge" => emitter.emit_cmp_ge(dest, left, right),
            "cmp_le" => emitter.emit_cmp_le(dest, left, right),
            _ => Err(Pl0Error::codegen_error(format!("Unknown relational op: {}", op))),
        }
    }

    // ---------------------------
    // Control Flow Helpers
    // ---------------------------
    fn emit_branch_if_zero(&mut self, vreg: &str, label: &str) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_beqz(vreg, label)
    }

    fn emit_jump(&mut self, label: &str) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_jump(label)
    }

    fn emit_label(&mut self, label: &str) -> Pl0Result<()> {
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_label(label)
    }

    fn create_and_emit_label(&mut self) -> Pl0Result<String> {
        let label = self.create_label();
        self.emit_label(&label)?;
        Ok(label)
    }

    // ---------------------------
    // Expression Evaluation
    // ---------------------------
    fn evaluate_condition(&mut self, condition: &dyn ExpressionNode) -> Pl0Result<String> {
        self.emit_expression(condition)
    }

    fn emit_conditional_branch(&mut self, vreg: &str, label: &str) -> Pl0Result<()> {
        self.emit_branch_if_zero(vreg, label)
    }

    // ---------------------------
    // I/O Operations
    // ---------------------------
    fn emit_read_operation(&mut self, operation: &str, identifier: &str) -> Pl0Result<()> {
        let symbol = self.get_variable_symbol(identifier, "variable in read")?.clone();
        let vreg = self.allocate_virtual_register();
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        match operation {
            "read_int" => emitter.emit_read_int(&vreg)?,
            "read_char" => emitter.emit_read_char(&vreg)?,
            _ => return Err(Pl0Error::codegen_error(format!("Unknown read operation: {}", operation))),
        }
        self.emit_store_to_symbol(&symbol, &vreg, identifier)?;
        Ok(())
    }

    // ---------------------------
    // Procedure Management
    // ---------------------------
    fn emit_procedure_call(&mut self, identifier: &str) -> Pl0Result<()> {
        let symbol = self.get_procedure_symbol(identifier, "procedure")?;
        let label = match &symbol.location {
            SymbolLocation::GlobalLabel(label) => label.clone(),
            SymbolLocation::None => identifier.to_string(),
            _ => return Err(Pl0Error::codegen_error(format!("Procedure {} has no valid address", identifier))),
        };
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_call(&label)
    }

    // ---------------------------
    // Binary & Relational Operation Helpers
    // ---------------------------
    fn emit_binary_operation(&mut self, left_expr: &dyn ExpressionNode, right_expr: &dyn ExpressionNode, operator: &str) -> Pl0Result<String> {
        let left_result = self.emit_expression(left_expr)?;
        let right_result = self.emit_expression(right_expr)?;
        let result_vreg = self.allocate_virtual_register();
        let op = match operator {
            "Plus" => "add",
            "Minus" => "sub",
            "Multiply" => "mul",
            "Divide" => "div",
            "Modulo" => "mod",
            _ => return Err(Pl0Error::codegen_error(format!("Unknown operator: {}", operator))),
        };
        self.emit_binary_op(op, &result_vreg, &left_result, &right_result)?;
        Ok(result_vreg)
    }

    fn emit_relational_operation(&mut self, left_expr: &dyn ExpressionNode, right_expr: &dyn ExpressionNode, operator: &str) -> Pl0Result<String> {
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
            _ => return Err(Pl0Error::codegen_error(format!("Unknown relational operator: {}", operator))),
        };
        self.emit_relational_op(op, &result_vreg, &left_result, &right_result)?;
        Ok(result_vreg)
    }

    // ---------------------------
    // Procedure Handling
    // ---------------------------
    fn emit_single_procedure(&mut self, name: &str, proc_block: &Option<Box<dyn Node>>) -> Pl0Result<()> {
        self.emit_label(name)?;
        // Set scope level based on procedure's level in symbol table
        let proc_symbol = self.symbol_table.get(name)
            .ok_or_else(|| Pl0Error::codegen_error(format!("Procedure {} not found in symbol table", name)))?;
        self.scope = self.scope.push_scope(true, Some(proc_symbol.level + 1), false);
        // Calculate stack size
        let mut stack_slots = 0;
        if let Some(proc_block) = proc_block {
            if let Some(block) = proc_block.as_any().downcast_ref::<Block>() {
                stack_slots = block.var_decl.var_decl.len();
            }
        }
        let stack_size = Self::calculate_stack_size(stack_slots);
        // Emit procedure prologue
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_proc_enter(stack_size)?;
        // Process procedure body
        if let Some(proc_block) = proc_block {
            proc_block.accept(self)?;
        }
        // Emit procedure epilogue
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_proc_exit()?;
        // Restore parent scope
        self.scope.pop_scope()?;
        Ok(())
    }

    fn collect_all_procedures<'a>(&self, block: &'a Block, procedures: &mut Vec<(String, &'a Option<Box<dyn Node>>)>) {
        // Add procedures from this block
        for (name, proc_block) in &block.proc_decl.procedurs {
            procedures.push((name.clone(), proc_block));
            // Recursively collect nested procedures
            if let Some(proc_block) = proc_block {
                if let Some(nested_block) = proc_block.as_any().downcast_ref::<Block>() {
                    self.collect_all_procedures(nested_block, procedures);
                }
            }
        }
    }

    fn emit_procedures(&mut self, block: &Block) -> Pl0Result<()> {
        if self.procedures_emitted {
            return Ok(());
        }
        let mut all_procedures = Vec::new();
        self.collect_all_procedures(block, &mut all_procedures);
        for (name, proc_block) in all_procedures {
            self.emit_single_procedure(&name, proc_block)?;
        }
        self.procedures_emitted = true;
        Ok(())
    }

    fn fold_constant_expression(&mut self, expr: &Box<dyn ExpressionNode>) -> Option<i64> {
        let expr = expr.as_ref();
        // Handle Number nodes
        if let Some(num) = expr.as_any().downcast_ref::<Number>() {
            return Some(num.value);
        }
        // Handle Ident nodes with Constant type
        if let Some(ident) = expr.as_any().downcast_ref::<Ident>() {
            if let Some(symbol) = self.symbol_table.get(&ident.value) {
                if let SymbolType::Constant(value) = symbol.symbol_type {
                    return Some(value);
                }
            }
        }
        None
    }
}

// ---------------------------
// ASTVisitor Implementation
// ---------------------------
impl ASTVisitor for IRGenerator {
    fn visit_ident(&mut self, ident: &Ident) -> Pl0Result<String> {
        let symbol = self.symbol_table.get_at_level(&ident.value, self.scope.level())
            .ok_or_else(|| Pl0Error::codegen_error(format!("Undefined identifier: {}", ident.value)))?.clone();
        match symbol.symbol_type {
            SymbolType::Constant(value) => {
                let vreg = self.allocate_virtual_register();
                let mut emitter = StringCodeEmitter::new(&mut self.code);
                emitter.emit_li(&vreg, &value.to_string())?;
                Ok(vreg)
            }
            SymbolType::Procedure => {
                let vreg = self.allocate_virtual_register();
                match &symbol.location {
                    SymbolLocation::GlobalLabel(label) => {
                        let mut emitter = StringCodeEmitter::new(&mut self.code);
                        emitter.emit(&format!("la {}, {}", vreg, label))?;
                    }
                    _ => return Err(Pl0Error::codegen_error(format!("Procedure {} has no valid address", ident.value))),
                }
                Ok(vreg)
            }
            SymbolType::Variable => {
                let vreg = self.get_or_load_variable(&ident.value)?;
                if !vreg.starts_with(&self.vreg_prefix) {
                    return Err(Pl0Error::codegen_error(format!("Invalid register format: {}", vreg)));
                }
                Ok(vreg)
            }
            _ => {
                let vreg = self.allocate_virtual_register();
                self.emit_load_from_symbol(&symbol, &vreg, &ident.value)?;
                Ok(vreg)
            }
        }
    }

    fn visit_number(&mut self, number: &Number) -> Pl0Result<String> {
        let vreg = self.allocate_virtual_register();
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_li(&vreg, &number.value.to_string())?;
        Ok(vreg)
    }

    fn visit_variable(&mut self, variable: &Variable) -> Pl0Result<String> {
        self.get_or_load_variable(&variable.name)
    }

    fn visit_binary_operation(&mut self, binop: &BinOp) -> Pl0Result<String> {
        let left_expr = binop.left.as_ref().ok_or_else(|| Pl0Error::codegen_error("Binary operation missing left operand"))?;
        let right_expr = binop.right.as_ref().ok_or_else(|| Pl0Error::codegen_error("Binary operation missing right operand"))?;
        // Try constant folding
        if let (Some(left), Some(right)) = (
            self.fold_constant_expression(left_expr),
            self.fold_constant_expression(right_expr)
        ) {
            let result = match binop.operator.as_str() {
                "Plus" => left + right,
                "Minus" => left - right,
                "Multiply" => left * right,
                "Divide" => left / right,
                "Modulo" => left % right,
                _ => return Err(Pl0Error::codegen_error(format!("Unknown operator: {}", binop.operator))),
            };
            let vreg = self.allocate_virtual_register();
            let mut emitter = StringCodeEmitter::new(&mut self.code);
            emitter.emit_li(&vreg, &result.to_string())?;
            return Ok(vreg);
        }
        self.emit_binary_operation(left_expr.as_ref(), right_expr.as_ref(), &binop.operator)
    }

    fn visit_while_statement(&mut self, stmt: &WhileStatement) -> Pl0Result<()> {
        let start_label = self.create_and_emit_label()?;
        let end_label = self.create_label();
        let condition = stmt.condition.as_ref().ok_or_else(|| Pl0Error::codegen_error("While statement missing condition"))?;
        let cond_vreg = self.evaluate_condition(condition.as_ref())?;
        self.emit_conditional_branch(&cond_vreg, &end_label)?;
        let body = stmt.body.as_ref().ok_or_else(|| Pl0Error::codegen_error("While statement missing body"))?;
        body.accept(self)?;
        self.emit_jump(&start_label)?;
        self.emit_label(&end_label)?;
        Ok(())
    }

    fn visit_condition(&mut self, cond: &OddCondition) -> Pl0Result<String> {
        let expr = cond.expr.as_ref().ok_or_else(|| Pl0Error::codegen_error("OddCondition missing expression"))?;
        let num_reg = self.emit_expression(expr.as_ref())?;
        let result_reg = self.allocate_virtual_register();
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_is_odd(&result_reg, &num_reg)?;
        Ok(result_reg)
    }

    fn visit_relational_condition(&mut self, cond: &RelationalCondition) -> Pl0Result<String> {
        let left = cond.left.as_ref().ok_or_else(|| Pl0Error::codegen_error("Relational condition missing left operand"))?;
        let right = cond.right.as_ref().ok_or_else(|| Pl0Error::codegen_error("Relational condition missing right operand"))?;
        self.emit_relational_operation(left.as_ref(), right.as_ref(), &cond.operator)
    }

    fn visit_call(&mut self, call: &CallStmt) -> Pl0Result<()> {
        self.emit_procedure_call(&call.identifier)
    }

    fn visit_assign(&mut self, stmt: &AssignStmt) -> Pl0Result<()> {
        let symbol = self.get_variable_symbol(&stmt.identifier, "variable in assignment")?.clone();
        let expr = stmt.expr.as_ref().ok_or_else(|| Pl0Error::codegen_error("Assign statement missing expression"))?;
        let vreg = self.emit_expression(expr.as_ref())?;
        self.emit_store_to_symbol(&symbol, &vreg, &stmt.identifier)?;
        Ok(())
    }

    fn visit_begin(&mut self, expr: &BeginStmt) -> Pl0Result<()> {
        for stmt in &expr.stmts {
            if let Some(stmt) = stmt {
                stmt.accept(self)?;
            }
        }
        Ok(())
    }

    fn visit_if(&mut self, expr: &IfStmt) -> Pl0Result<()> {
        let else_label = self.create_label();
        let end_label = self.create_label();
        let condition = expr.condition.as_ref().ok_or_else(|| Pl0Error::codegen_error("If statement missing condition"))?;
        let cond_vreg = self.evaluate_condition(condition.as_ref())?;
        self.emit_conditional_branch(&cond_vreg, &else_label)?;
        let then_branch = expr.then_branch.as_ref().ok_or_else(|| Pl0Error::codegen_error("If statement missing then branch"))?;
        then_branch.accept(self)?;
        if let Some(else_branch) = &expr.else_branch {
            self.emit_jump(&end_label)?;
            self.emit_label(&else_label)?;
            else_branch.accept(self)?;
            self.emit_label(&end_label)?;
        } else {
            self.emit_jump(&end_label)?;
            self.emit_label(&else_label)?;
            self.emit_label(&end_label)?;
        }
        Ok(())
    }

    fn visit_write_int(&mut self, stmt: &Write) -> Pl0Result<()> {
        let expr = stmt.expr.as_ref().ok_or_else(|| Pl0Error::codegen_error("Write statement missing expression"))?;
        let vreg = self.emit_expression(expr.as_ref())?;
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_write_int(&vreg)
    }

    fn visit_write_str(&mut self, stmt: &WriteStr) -> Pl0Result<()> {
        if stmt.expr.is_empty() {
            return Err(Pl0Error::codegen_error("WriteStr statement missing expression"));
        }
        let mut emitter = StringCodeEmitter::new(&mut self.code);
        emitter.emit_write_str(&stmt.expr)
    }

    fn visit_read_int(&mut self, expr: &Read) -> Pl0Result<()> {
        self.emit_read_operation("read_int", &expr.identifier)
    }

    fn visit_exit(&mut self, _expr: &Exit) -> Pl0Result<()> {
        self.exit_emitted = true;
        self.system_exit(0)
    }

    fn visit_const(&mut self, expr: &ConstDecl) -> Pl0Result<()> {
        for (id, num) in &expr.const_decl {
            let mut emitter = StringCodeEmitter::new(&mut self.constants);
            emitter.emit_const(id, &num.to_string())?;
        }
        Ok(())
    }

    fn visit_var_decl(&mut self, expr: &VarDecl) -> Pl0Result<()> {
        for var_name in &expr.var_decl {
            if self.scope.in_procedure() {
                let offset = self.scope.allocate_variable();
            } else {
                // Global variable
                let mut emitter = StringCodeEmitter::new(&mut self.variables);
                emitter.emit_var(var_name)?;
            }
        }
        Ok(())
    }

    fn visit_proc_decl(&mut self, expr: &ProcDecl) -> Pl0Result<()> {
        // Only emit procedures if we're not already in a procedure
        // This prevents duplicate procedure emissions
        if !self.scope.in_procedure() && !self.procedures_emitted {
            for (name, proc_block) in &expr.procedurs {
                self.emit_single_procedure(name, proc_block)?;
            }
        }
        Ok(())
    }

    fn visit_block(&mut self, block: &Block) -> Pl0Result<()> {
        // Process constants first
        if !block.const_decl.const_decl.is_empty() {
            block.const_decl.accept(self)?;
        }

        // Process variable declarations
        if !block.var_decl.var_decl.is_empty() {
            block.var_decl.accept(self)?;
        }

        // Emit all procedures at the top level (only once)
        if !self.scope.in_procedure() && !self.procedures_emitted {
            self.emit_procedures(block)?;
        }

        // Process the main statement
        if let Some(stmt) = &block.statement {
            // Emit main label if this is the main block
            if !self.scope.in_procedure() && !self.main_emitted {
                self.emit_label("main")?;
                self.main_emitted = true;
                // Initialize local variables to zero
                self.initialize_local_variables(block)?;
            }
            stmt.accept(self)?;
        }
        Ok(())
    }

    fn visit_program(&mut self, expr: &Program) -> Pl0Result<()> {
        if let Some(block) = &expr.block {
            block.accept(self)?;
        }
        Ok(())
    }
}