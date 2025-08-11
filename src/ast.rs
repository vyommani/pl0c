/*
*                    pl0c -- PL/0 compiler.
*
* program    = block "." ;
* block	     = [ "const" ident "=" number { "," ident "=" number } ";" ]
*		       [ "var" ident { "," ident} ";" ]
*		       { "procedure" ident ";" block ";" } statement ;
* statement	 = [ ident ":=" expression
*		       | "call" ident
*		       | "begin" statement { ";" statement } "end"
*		       | "if" condition "then" statement [ "else" statement ]
*		       | "while" condition "do" statement
*		       | "read" ident
*		       | "write" expression
*		       | "writeStr" ( ident | string )
*		       | "exit" expression ] ;
* condition	 = "odd" expression | expression ( comparator ) expression ;
* expression = [ "+" | "-" | "not" ] term { ( "+" | "-" | "or" ) term } ;
* term		 = factor { ( "*" | "/" | "mod" | "and" ) factor } ;
* factor     = ident | number | "(" expression ")" ;
* comparator = "=" | "#" | "<" | ">" | "<=" | ">=" | "<>" ;
*/

use crate::visiters::ASTVisitor;
use crate::errors::Pl0Result;
use std::any::Any;

pub trait Node {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()>;
    fn print(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait ExpressionNode: Node {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<String>;
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
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<()> {
        // For Node trait, we ignore the return value
        let _ = visitor.visit_variable(self);
        Ok(())
    }

    fn print(&self) {
        print!("{}", &self.name);
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ExpressionNode for Variable {
    fn accept(&self, visitor: &mut dyn ASTVisitor) -> Pl0Result<String> {
        visitor.visit_variable(self)
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
