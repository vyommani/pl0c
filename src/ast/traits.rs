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

use crate::semantic::visiters::ASTVisitor;
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
