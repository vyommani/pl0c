use crate::ast::Node;
use crate::visiters::ASTVisitor;

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
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        if let Err(e) = visitor.visit_program(self) {
            eprintln!("Error visiting program node: {:?}", e);
        }
    }
    fn print(&self) {
        if let Some(block) = &self.block {
            block.print();
        }
        println!("{}", self.dot);
    }
}
