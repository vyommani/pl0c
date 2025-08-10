use crate::ast::{ExpressionNode, Node};
use crate::visiters::ASTVisitor;
use crate::errors::Pl0Result;

pub struct WriteInt {
    pub expr: Option<Box<dyn ExpressionNode>>,
}

impl WriteInt {
    pub fn new(expr: Option<Box<dyn ExpressionNode>>) -> Self {
        Self { expr }
    }
}

impl Node for WriteInt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_write_int(self)
    }
    fn print(&self) {
        print!("writeInt(");
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

pub struct ReadInt {
    pub identifier: String,
}

impl ReadInt {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}

impl Node for ReadInt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_read_int(self)
    }
    fn print(&self) {
        print!("readInt({})", self.identifier);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
