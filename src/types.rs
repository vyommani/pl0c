use crate::ast::{Node, ExpressionNode};
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        // For Node trait, we ignore the return value
        let _ = visitor.visit_ident(self);
        Ok(())
    }
    fn print(&self) {
        print!("{}", self.value);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl ExpressionNode for Ident {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<String, String> {
        visitor.visit_ident(self)
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        // For Node trait, we ignore the return value
        let _ = visitor.visit_number(self);
        Ok(())
    }
    fn print(&self) {
        print!("{}", self.value);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl ExpressionNode for Number {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<String, String> {
        visitor.visit_number(self)
    }
}
