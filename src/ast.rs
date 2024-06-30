/*
 * pl0c -- PL/0 compiler.
 *
 * program	= block "." .
 * block	= [ "const" ident "=" number { "," ident "=" number } ";" ]
 *		  [ "var" ident [ array ] { "," ident [ array ] } ";" ]
		  { "forward" ident ";" }
 *		  { "procedure" ident ";" block ";" } statement .
 * statement	= [ ident ":=" expression
 *		  | "call" ident
 *		  | "begin" statement { ";" statement } "end"
 *		  | "if" condition "then" statement [ "else" statement ]
 *		  | "while" condition "do" statement
 *		  | "readInt" [ "into" ] ident
 *		  | "readChar" [ "into" ] ident
 *		  | "writeInt" expression
 *		  | "writeChar" expression
 *		  | "writeStr" ( ident | string )
 *		  | "exit" expression ] .
 * condition	= "odd" expression
 *		| expression ( comparator ) expression .
 * expression	= [ "+" | "-" | "not" ] term { ( "+" | "-" | "or" ) term } .
 * term		= factor { ( "*" | "/" | "mod" | "and" ) factor } .
 * factor	= ident
 *		| number
 *		| "(" expression ")" .
 * comparator	= "=" | "#" | "<" | ">" | "<=" | ">=" | "<>" .
 * array	= "size" number .
 */

pub trait Node {
    fn accept(&self, visitor: &mut dyn ASTVisitor);
}

pub struct Number {
    pub value: i64,
}

impl Number {
    pub fn new(num: i64) -> Self {
        Self {
            value:num,
        }
    }
}
impl Node for Number {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_number(self);
    }
}

pub struct Variable {
    pub name: String,
}
impl Variable {
    pub fn new(name: &String) -> Self {
        Variable{
            name:name.to_string(),
        }
    }
}
impl Node for Variable {
    
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_variable(self);
    }
}

pub struct BinaryOperation {
    pub left: Box<dyn Node>,
    pub operator: String,
    pub right: Box<dyn Node>,
}

impl BinaryOperation {
    pub fn new(lhs: Box<dyn Node>, rhs:Box<dyn Node>, op: &String) -> Self {
        BinaryOperation {
            left:lhs,
            right:rhs,
            operator:op.to_string(),            
        }
    }
}
impl Node for BinaryOperation {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_binary_operation(self);
    }
}

pub struct Assignment {
    pub variable: Variable,
    pub expression: Box<dyn Node>,
}

impl Node for Assignment {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_assignment(self);
    }
}

pub struct IfStatement {
    pub condition: Box<dyn Node>,
    pub then_branch: Box<dyn Node>,
    pub else_branch: Option<Box<dyn Node>>,
}

impl Node for IfStatement {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_if_statement(self);
    }
}

pub struct WhileStatement {
    pub condition: Box<dyn Node>,
    pub body: Box<dyn Node>,
}

impl Node for WhileStatement {
    fn accept(&self, visitor: &mut dyn ASTVisitor) {
        visitor.visit_while_statement(self);
    }
}

pub trait ASTVisitor {
    fn visit_number(&mut self, number: &Number);
    fn visit_variable(&mut self, variable: &Variable);
    fn visit_binary_operation(&mut self, binary_operation: &BinaryOperation);
    fn visit_assignment(&mut self, assignment: &Assignment);
    fn visit_if_statement(&mut self, if_statement: &IfStatement);
    fn visit_while_statement(&mut self, while_statement: &WhileStatement);
}
