use crate::ast::Node;
use crate::decl::ConstDecl;
use crate::decl::ProcDecl;
use crate::decl::VarDecl;
use crate::visiters::ASTVisitor;

pub struct Block {
    pub const_decl: ConstDecl,
    pub var_decl: VarDecl,
    pub proc_decl: ProcDecl,
    pub statement: Option<Box<dyn Node>>,
}

impl Block {
    pub fn new(
        const_decl: ConstDecl,
        var_decl: VarDecl,
        proc_decl: ProcDecl,
        statement: Option<Box<dyn Node>>,
    ) -> Self {
        Self {
            const_decl,
            var_decl,
            proc_decl,
            statement,
        }
    }
}

impl Node for Block {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        visitor.visit_block(self)
    }

    fn print(&self) {
        self.const_decl.print();
        self.var_decl.print();
        self.proc_decl.print();
        if let Some(stmt) = &self.statement {
            stmt.print();
        }
    }
}
