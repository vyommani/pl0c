use crate::ast::Node;
use crate::visiters::ASTVisitor;

pub struct Ident {
    pub value: String,
}

impl Ident {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl Node for Ident {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_ident(self);
    }
    fn print(&self) {
        print!("{}", &self.value);
    }
}

pub struct Number {
    pub value: i64,
}

impl Number {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}
impl Node for Number {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_number(self);
    }
    fn print(&self) {
        print!("{}", &self.value);
    }
}
