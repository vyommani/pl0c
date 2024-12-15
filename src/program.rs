use crate::ast::Node;
use crate::visiters::ASTVisitor;

pub struct Program {
    block: Option<Box<dyn Node>>,
    dot: String,
}

impl Program {
    pub fn new(block: Option<Box<dyn Node>>, dot: String) -> Self {
        Self { block, dot }
    }
}

impl Node for Program {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_program(self);
    }
    fn print(&self) {
        if let Some(block) = &self.block {
            block.print();
        }
        println!("{}", self.dot);
    }
}
