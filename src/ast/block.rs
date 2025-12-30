use crate::ast::Node;
use crate::ast::{ConstDecl, ProcDecl, VarDecl};
use crate::semantic::visiters::ASTVisitor;
use crate::utils::errors::Pl0Result;

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
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
