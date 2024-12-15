use crate::ast::Node;
use crate::visiters::ASTVisitor;

pub struct WriteInt {
    expr: Option<Box<dyn Node>>,
}

impl WriteInt {
    pub fn new(expr: Option<Box<dyn Node>>) -> Self {
        Self { expr }
    }
}

impl Node for WriteInt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_write_int(self);
    }
    fn print(&self) {
        print!("writeInt(");
        if let Some(expr) = &self.expr {
            expr.print();
        }
        print!(")");
    }
}

pub struct WriteChar {
    expr: Option<Box<dyn Node>>,
}

impl WriteChar {
    pub fn new(expr: Option<Box<dyn Node>>) -> Self {
        Self { expr }
    }
}

impl Node for WriteChar {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_write_char(self);
    }
    fn print(&self) {
        print!("writeInt(");
        if let Some(expr) = &self.expr {
            expr.print();
        }
        print!(")");
    }
}

pub struct WriteStr {
    expr: String,
}

impl WriteStr {
    pub fn new(expr: String) -> Self {
        Self { expr }
    }
}

impl Node for WriteStr {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_write_str(self);
    }
    fn print(&self) {
        print!("writeStr({})", &self.expr);
    }
}

pub struct ReadInt {
    identifier: String,
}

impl ReadInt {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}

impl Node for ReadInt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_read_int(self);
    }
    fn print(&self) {
        print!("readInt({})", &self.identifier);
    }
}

pub struct ReadChar {
    identifier: String,
}

impl ReadChar {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}

impl Node for ReadChar {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_read_char(self);
    }
    fn print(&self) {
        print!("readChar({})", &self.identifier);
    }
}
