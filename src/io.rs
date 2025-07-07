use crate::ast::{ExpressionNode, Node};
use crate::visiters::ASTVisitor;

pub struct WriteInt {
    pub expr: Option<Box<dyn ExpressionNode>>,
}

impl WriteInt {
    pub fn new(expr: Option<Box<dyn ExpressionNode>>) -> Self {
        Self { expr }
    }
}

impl Node for WriteInt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
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

pub struct WriteChar {
    pub expr: Option<Box<dyn ExpressionNode>>,
}

impl WriteChar {
    pub fn new(expr: Option<Box<dyn ExpressionNode>>) -> Self {
        Self { expr }
    }
}

impl Node for WriteChar {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        visitor.visit_write_char(self)
    }
    fn print(&self) {
        print!("writeChar(");
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        visitor.visit_read_int(self)
    }
    fn print(&self) {
        print!("readInt({})", self.identifier);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct ReadChar {
    pub identifier: String,
}

impl ReadChar {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}

impl Node for ReadChar {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        visitor.visit_read_char(self)
    }
    fn print(&self) {
        print!("readChar({})", self.identifier);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
