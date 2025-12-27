use crate::ast::{ExpressionNode, Node};
use crate::semantic::visiters::ASTVisitor;
use crate::errors::Pl0Result;

pub struct Write {
    pub expr: Option<Box<dyn ExpressionNode>>,
}

impl Write {
    pub fn new(expr: Option<Box<dyn ExpressionNode>>) -> Self {
        Self { expr }
    }
}

impl Node for Write {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_write_int(self)
    }
    fn print(&self) {
        print!("write(");
        if let Some(expr) = &self.expr {
            expr.print();
        }
        print!(")");
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct WriteStr {
    pub expr: String,
}

impl WriteStr {
    pub fn new(expr: String) -> Self {
        Self { expr }
    }
}

impl Node for WriteStr {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_write_str(self)
    }
    fn print(&self) {
        print!("writeStr({})", self.expr);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct Read {
    pub identifier: String,
}

impl Read {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}

impl Node for Read {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_read_int(self)
    }
    fn print(&self) {
        print!("read({})", self.identifier);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
