use crate::ast::Node;
use crate::visiters::ASTVisitor;

pub struct BinOp {
    pub left: Option<Box<dyn Node>>,
    pub operator: String,
    pub right: Option<Box<dyn Node>>,
}

impl BinOp {
    pub fn new(
        left: Option<Box<dyn Node>>,
        right: Option<Box<dyn Node>>,
        operator: String,
    ) -> Self {
        Self {
            left,
            right,
            operator,
        }
    }
}

impl Node for BinOp {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_binary_operation(self);
    }

    fn print(&self) {
        if let Some(left_operand) = &self.left {
            left_operand.print();
        }
        match self.operator.as_str() {
            "Plus" => {
                print!(" + ")
            }
            "Minus" => {
                print!(" - ")
            }
            "Not" => {
                print!(" ! ")
            }
            "Multiply" => {
                print!(" * ")
            }
            "Divide" => {
                print!(" / ")
            }
            "Modulo" => {
                print!(" % ")
            }
            _ => {}
        }
        if let Some(right) = &self.right {
            right.print();
        }
    }
}

pub struct OddCondition {
    pub expr: Option<Box<dyn Node>>,
}

impl OddCondition {
    pub fn new(expr: Option<Box<dyn Node>>) -> Self {
        Self { expr }
    }
}
impl Node for OddCondition {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_condition(self);
    }
    fn print(&self) {
        print!("odd ");
        if let Some(expr) = &self.expr {
            expr.print();
        }
    }
}

pub struct RelationalCondition {
    pub operator: String,
    pub left: Option<Box<dyn Node>>,
    pub right: Option<Box<dyn Node>>,
}

impl RelationalCondition {
    pub fn new(
        left: Option<Box<dyn Node>>,
        right: Option<Box<dyn Node>>,
        operator: String,
    ) -> Self {
        Self {
            operator,
            left,
            right,
        }
    }
}

impl Node for RelationalCondition {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_relational_condition(self);
    }
    fn print(&self) {
        if let Some(left) = &self.left {
            left.print();
        }

        match self.operator.as_str() {
            "GreaterThanEqual" => {
                print!(" >= ");
            }
            "LessThanEqual" => {
                print!(" <= ");
            }
            "GreaterThan" => {
                print!(" > ");
            }
            "LessThan" => {
                print!(" < ");
            }
            "Equal" => {
                print!(" == ");
            }
            "Hash" => {
                print!(" # ");
            }
            _ => {}
        }
        if let Some(right) = &self.right {
            right.print();
        }
    }
}
