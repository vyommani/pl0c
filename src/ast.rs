/*
*                    pl0c -- PL/0 compiler.
*
* program    = block "." ;
* block	     = [ "const" ident "=" number { "," ident "=" number } ";" ]
*		     [ "var" ident { "," ident} ";" ]
*		     { "procedure" ident ";" block ";" } statement ;
* statement	 = [ ident ":=" expression
*		     | "call" ident
*		     | "begin" statement { ";" statement } "end"
*		     | "if" condition "then" statement [ "else" statement ]
*		     | "while" condition "do" statement
*		     | "readInt" ( ident )
*		     | "readChar" ( ident )
*		     | "writeInt" expression
*		     | "writeChar" expression
*		     | "writeStr" ( ident | string )
*		     | "exit" expression ] ;
* condition	 = "odd" expression | expression ( comparator ) expression ;
* expression = [ "+" | "-" | "not" ] term { ( "+" | "-" | "or" ) term } ;
* term		 = factor { ( "*" | "/" | "mod" | "and" ) factor } ;
* factor     = ident | number | "(" expression ")" ;
* comparator = "=" | "#" | "<" | ">" | "<=" | ">=" | "<>" ;
*/

use crate::visiters::ASTVisitor;

pub trait Node {
    fn accept(&self, visitor: &mut dyn ASTVisitor);
    fn print(&self);
}

pub struct Variable {
    pub name: String,
}

impl Variable {
    pub fn new(name: &String) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl Node for Variable {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_variable(self);
    }

    fn print(&self) {
        print!("{}", &self.name);
    }
}

pub struct Exit {
    expr: Option<Box<dyn Node>>,
}

impl Exit {
    pub fn new(expr: Option<Box<dyn Node>>) -> Self {
        Self { expr: expr }
    }
}

impl Node for Exit {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_exit(self);
    }
    fn print(&self) {
        print!("exit ");
        if let Some(expr) = &self.expr {
            expr.print();
        }
    }
}
