use crate::ast::Node;
use crate::visiters::ASTVisitor;

pub struct WriteInt {
    pub expr: Option<Box<dyn Node>>,
}

impl WriteInt {
    pub fn new(expr: Option<Box<dyn Node>>) -> Self {
        Self { expr }
    }
}

impl Node for WriteInt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        if let Err(e) = visitor.visit_write_int(self) {
            eprintln!("Error visiting writeInt node: {:?}", e);
        }
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
    pub expr: Option<Box<dyn Node>>,
}

impl WriteChar {
    pub fn new(expr: Option<Box<dyn Node>>) -> Self {
        Self { expr }
    }
}

impl Node for WriteChar {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        if let Err(e) = visitor.visit_write_char(self) {
            eprintln!("Error visiting writeChar node: {:?}", e);
        }
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
    pub expr: String,
}

impl WriteStr {
    pub fn new(expr: String) -> Self {
        Self { expr }
    }
}

impl Node for WriteStr {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        if let Err(e) = visitor.visit_write_str(self) {
            eprintln!("Error visiting writeStr node: {:?}", e);
        }
    }
    fn print(&self) {
        print!("writeStr({})", &self.expr);
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        if let Err(e) = visitor.visit_read_int(self) {
            eprintln!("Error visiting readInt node: {:?}", e);
        }
    }
    fn print(&self) {
        print!("readInt({})", &self.identifier);
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        if let Err(e) = visitor.visit_read_char(self) {
            eprintln!("Error visiting readChar node: {:?}", e);
        }
    }
    fn print(&self) {
        print!("readChar({})", &self.identifier);
    }
}
