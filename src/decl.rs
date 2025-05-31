use crate::ast::Node;
use crate::visiters::ASTVisitor;

pub struct ProcDecl {
    pub procedurs: Vec<(String, Option<Box<dyn Node>>)>,
}

impl ProcDecl {
    pub fn new(procedurs: Vec<(String, Option<Box<dyn Node>>)>) -> Self {
        Self { procedurs }
    }
    pub fn default() -> Self {
        Self {
            procedurs: Vec::default(),
        }
    }
}

impl Node for ProcDecl {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_proc_decl(self);
    }
    fn print(&self) {
        let mut first = true;
        for tuple in &self.procedurs {
            if first {
                first = false;
            } else {
                println!(";");
            }
            if let (name, Some(block)) = tuple {
                println!("procedure {};", name);
                block.print();
            }
        }
        if !first {
            println!(";");
        }
    }
}

pub struct VarDecl {
    pub var_decl: Vec<String>,
}

impl VarDecl {
    pub fn new(idents: Vec<String>) -> Self {
        Self { var_decl: idents }
    }
    pub fn default() -> Self {
        Self {
            var_decl: Vec::default(),
        }
    }
}

impl Node for VarDecl {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_var_decl(self);
    }
    fn print(&self) {
        let mut first = true;
        for id in &self.var_decl {
            if first {
                first = false;
                print!("var ");
            } else {
                print!(", ");
            }
            print!("{}", id);
        }
        if !first {
            println!(";");
        }
    }
}

pub struct ConstDecl {
    pub const_decl: Vec<(String, i64)>,
}

impl ConstDecl {
    pub fn new(const_decl: Vec<(String, i64)>) -> Self {
        Self { const_decl }
    }
    pub fn default() -> Self {
        Self {
            const_decl: Vec::default(),
        }
    }
}

impl Node for ConstDecl {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_const(self);
    }
    fn print(&self) {
        let mut first = true;
        for (id, num) in &self.const_decl {
            if first {
                first = false;
                print!("const ");
            } else {
                print!(", ");
            }
            print!("{} = {}", id, num);
        }
        if !first {
            println!(";");
        }
    }
}
