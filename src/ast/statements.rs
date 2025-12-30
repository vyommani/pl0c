use crate::ast::{ExpressionNode, Node};
use crate::semantic::visiters::ASTVisitor;
use crate::utils::errors::Pl0Result;
use std::any::Any;

pub struct AssignStmt {
    pub identifier: String,
    pub expr: Option<Box<dyn ExpressionNode>>,
}

impl AssignStmt {
    pub fn new(identifier: String, expr: Option<Box<dyn ExpressionNode>>) -> Self {
        Self { identifier, expr }
    }
}

impl Node for AssignStmt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_assign(self)
    }

    fn print(&self) {
        print!("{} ", self.identifier);
        if let Some(expr) = &self.expr {
            print!(":= ");
            expr.print();
        }
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct BeginStmt {
    pub stmts: Vec<Option<Box<dyn Node>>>,
}

impl BeginStmt {
    pub fn new(stmts: Vec<Option<Box<dyn Node>>>) -> Self {
        Self { stmts }
    }
}

impl Node for BeginStmt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_begin(self)
    }
    fn print(&self) {
        println!("begin");
        for stmt in &self.stmts {
            if let Some(stmt) = stmt {
                stmt.print();
                println!(";");
            }
        }
        print!("end");
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct IfStmt {
    pub condition: Option<Box<dyn ExpressionNode>>,
    pub then_branch: Option<Box<dyn Node>>,
    pub else_branch: Option<Box<dyn Node>>,
}

impl IfStmt {
    pub fn new(
        condition: Option<Box<dyn ExpressionNode>>,
        then_branch: Option<Box<dyn Node>>,
        else_branch: Option<Box<dyn Node>>,
    ) -> Self {
        Self {
            condition,
            then_branch,
            else_branch,
        }
    }
}

impl Node for IfStmt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_if(self)
    }
    fn print(&self) {
        if let Some(condition) = &self.condition {
            print!("if ");
            condition.print();
        }
        if let Some(then_branch) = &self.then_branch {
            println!(" then");
            then_branch.print();
        }
        if let Some(else_branch) = &self.else_branch {
            println!();
            println!("else");
            else_branch.print();
        }
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct WhileStatement {
    pub condition: Option<Box<dyn ExpressionNode>>,
    pub body: Option<Box<dyn Node>>,
}

impl WhileStatement {
    pub fn new(condition: Option<Box<dyn ExpressionNode>>, body: Option<Box<dyn Node>>) -> Self {
        Self { condition, body }
    }
}

impl Node for WhileStatement {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_while_statement(self)
    }
    fn print(&self) {
        if let Some(condition) = &self.condition {
            print!("while ");
            condition.print();
            println!(" do");
        }
        if let Some(body) = &self.body {
            body.print();
        }
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub struct CallStmt {
    pub identifier: String,
}

impl CallStmt {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}

impl Node for CallStmt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_call(self)
    }
    fn print(&self) {
        print!("call {}", &self.identifier);
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}


pub struct Exit {
    pub expr: Option<Box<dyn ExpressionNode>>,
}

impl Exit {
    pub fn new(expr: Option<Box<dyn ExpressionNode>>) -> Self {
        Self { expr: expr }
    }
}

impl Node for Exit {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        visitor.visit_exit(self)
    }
    fn print(&self) {
        print!("exit ");
        if let Some(expr) = &self.expr {
            expr.print();
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
