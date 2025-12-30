use crate::ast::Node;
use crate::semantic::visiters::ASTVisitor;
use crate::utils::errors::Pl0Result;

pub struct Program {
    pub block: Option<Box<dyn Node>>,
    dot: String,
}

impl Program {
    pub fn new(block: Option<Box<dyn Node>>, dot: String) -> Self {
        Self { block, dot }
    }
}

impl Node for Program {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_program(self)
    }

    fn print(&self) {
        if let Some(block) = &self.block {
            block.print();
        }
        println!("{}", self.dot);
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
