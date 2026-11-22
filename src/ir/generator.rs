use crate::{
    ast::Node, semantic::symboltable::SymbolTable, utils::errors::{Pl0Result, Pl0Error},
};
use crate::semantic::scope_info::ScopeInfo;
use crate::ir::tac::{TacModule, Function, BasicBlock, Instr};

pub struct IRGenerator {
    pub(crate) label_counter: i32,
    pub(crate) vreg_counter: i32,
    pub(crate) vreg_prefix: String,
    pub(crate) symbol_table: SymbolTable,
    pub(crate) exit_emitted: bool,
    pub(crate) main_emitted: bool,
    pub(crate) procedures_emitted: bool,
    pub(crate) scope: ScopeInfo,
    pub(crate) module: TacModule,
    current_function_name: Option<String>,
    current_block_label: Option<String>,
    current_blocks: Vec<BasicBlock>,
    current_instrs: Vec<Instr>,
}

impl IRGenerator {
    pub fn new(table: SymbolTable) -> Self {
        Self {
            symbol_table: table,
            label_counter: 0,
            vreg_counter: 0,
            vreg_prefix: "v".to_string(),
            exit_emitted: false,
            main_emitted: false,
            procedures_emitted: false,
            scope: ScopeInfo::new(),
            module: TacModule::default(),
            current_function_name: None,
            current_block_label: None,
            current_blocks: Vec::new(),
            current_instrs: Vec::new(),
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

    /// Appends one instruction to the block currently being built - or, if
    /// no function/block has been opened yet (e.g. a program with no
    /// statement at all, so `start_function` was never called before the
    /// auto-appended `exit 0`), to the module's preamble instead of
    /// silently dropping it.
    pub(crate) fn push_instr(&mut self, instr: Instr) {
        if self.current_block_label.is_some() {
            self.current_instrs.push(instr);
        } else {
            self.module.preamble.push(instr);
        }
    }

    pub(crate) fn push_const(&mut self, name: String, value: i64) {
        self.module.consts.push((name, value));
    }

    pub(crate) fn push_global(&mut self, name: String) {
        self.module.globals.push(name);
    }

    /// Closes the block currently being built (if any) and opens a new one
    /// under the same function. Used for in-function control-flow labels
    /// (if/while targets).
    pub(crate) fn start_block(&mut self, label: &str) {
        self.close_block();
        self.current_block_label = Some(label.to_string());
    }

    /// Closes out whatever function/block was being built (if any) and
    /// opens a new function whose first block is labeled `name`. Used for
    /// procedure entries and `main` - distinct from `start_block` because
    /// those two also need to end the *previous* function, not just open a
    /// new block within it.
    pub(crate) fn start_function(&mut self, name: &str) {
        self.close_block();
        self.close_function();
        self.current_function_name = Some(name.to_string());
        self.current_block_label = Some(name.to_string());
    }

    fn close_block(&mut self) {
        if let Some(label) = self.current_block_label.take() {
            let instrs = std::mem::take(&mut self.current_instrs);
            self.current_blocks.push(BasicBlock { label, instrs });
        }
    }

    fn close_function(&mut self) {
        if let Some(name) = self.current_function_name.take() {
            let blocks = std::mem::take(&mut self.current_blocks);
            self.module.functions.push(Function { name, blocks });
        }
    }

    pub fn get_output(&self) -> String {
        self.module.render()
    }

    pub fn generate_code(&mut self, ast: Option<Box<dyn Node + 'static>>) -> Pl0Result<()> {
        self.exit_emitted = false;
        self.main_emitted = false;
        self.procedures_emitted = false;
        self.module = TacModule::default();
        self.current_function_name = None;
        self.current_block_label = None;
        self.current_blocks.clear();
        self.current_instrs.clear();

        let ast = ast.ok_or_else(|| Pl0Error::codegen_error("No AST provided for code generation"))?;
        ast.accept(self)?;
        if !self.exit_emitted {
            self.system_exit(0)?;
        }
        self.close_block();
        self.close_function();
        Ok(())
    }

    pub fn system_exit(&mut self, code: i32) -> Pl0Result<()> {
        self.push_instr(Instr::Exit { code });
        Ok(())
    }
}
