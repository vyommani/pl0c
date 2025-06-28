use crate::ast::{Node, ExpressionNode};
use crate::visiters::ASTVisitor;

pub struct BinOp {
    pub left: Option<Box<dyn ExpressionNode>>,
    pub operator: String,
    pub right: Option<Box<dyn ExpressionNode>>,
}

impl BinOp {
    pub fn new(
        left: Option<Box<dyn ExpressionNode>>,
        right: Option<Box<dyn ExpressionNode>>,
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        // For Node trait, we ignore the return value
        let _ = visitor.visit_binary_operation(self);
        Ok(())
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

impl ExpressionNode for BinOp {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<String, String> {
        visitor.visit_binary_operation(self)
    }
}

pub struct OddCondition {
    pub expr: Option<Box<dyn ExpressionNode>>,
}

impl OddCondition {
    pub fn new(expr: Option<Box<dyn ExpressionNode>>) -> Self {
        Self { expr }
    }
}

impl Node for OddCondition {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        // For Node trait, we ignore the return value
        let _ = visitor.visit_condition(self);
        Ok(())
    }
    fn print(&self) {
        print!("odd ");
        if let Some(expr) = &self.expr {
            expr.print();
        }
    }
}

impl ExpressionNode for OddCondition {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<String, String> {
        visitor.visit_condition(self)
    }
}

pub struct RelationalCondition {
    pub operator: String,
    pub left: Option<Box<dyn ExpressionNode>>,
    pub right: Option<Box<dyn ExpressionNode>>,
}

impl RelationalCondition {
    pub fn new(
        left: Option<Box<dyn ExpressionNode>>,
        right: Option<Box<dyn ExpressionNode>>,
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<(), String> {
        // For Node trait, we ignore the return value
        let _ = visitor.visit_relational_condition(self);
        Ok(())
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

impl ExpressionNode for RelationalCondition {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Result<String, String> {
        visitor.visit_relational_condition(self)
    }
}
