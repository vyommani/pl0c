use crate::ast::Node;
use crate::visiters::ASTVisitor;

pub struct AssignStmt {
    identifier: String,
    expr: Option<Box<dyn Node>>,
}

impl AssignStmt {
    pub fn new(identifier: String, expr: Option<Box<dyn Node>>) -> Self {
        Self { identifier, expr }
    }
}

impl Node for AssignStmt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_assign(self);
    }

    fn print(&self) {
        print!("{} ", self.identifier);
        if let Some(expr) = &self.expr {
            print!(":= ");
            expr.print();
        }
    }
}

pub struct BeginStmt {
    stmts: Vec<Option<Box<dyn Node>>>,
}

impl BeginStmt {
    pub fn new(stmts: Vec<Option<Box<dyn Node>>>) -> Self {
        Self { stmts }
    }
}

impl Node for BeginStmt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_begin(self);
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
}
pub struct IfStmt {
    condition: Option<Box<dyn Node>>,
    then_branch: Option<Box<dyn Node>>,
    else_branch: Option<Box<dyn Node>>,
}

impl IfStmt {
    pub fn new(
        condition: Option<Box<dyn Node>>,
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_if(self);
    }
    fn print(&self) {
        if let Some(condition) = &self.condition {
            print!("if ");
            condition.print();
        }
        if let Some(then_branch) = &self.then_branch {
            println!(" than");
            then_branch.print();
        }
        if let Some(else_branch) = &self.else_branch {
            println!();
            println!("else");
            else_branch.print();
        }
    }
}
pub struct WhileStatement {
    pub condition: Option<Box<dyn Node>>,
    pub body: Option<Box<dyn Node>>,
}

impl WhileStatement {
    pub fn new(condition: Option<Box<dyn Node>>, body: Option<Box<dyn Node>>) -> Self {
        Self { condition, body }
    }
}
impl Node for WhileStatement {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_while_statement(self);
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
}

pub struct CallStmt {
    identifier: String,
}

impl CallStmt {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}
impl Node for CallStmt {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_call(self);
    }
    fn print(&self) {
        print!("call {}", &self.identifier);
    }
}
