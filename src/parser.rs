use crate::ast::Exit;
use crate::ast::Node;
use crate::block::Block;
use crate::decl::ConstDecl;
use crate::decl::ProcDecl;
use crate::decl::VarDecl;
use crate::expression::BinOp;
use crate::expression::OddCondition;
use crate::expression::RelationalCondition;
use crate::io::ReadChar;
use crate::io::ReadInt;
use crate::io::WriteChar;
use crate::io::WriteInt;
use crate::io::WriteStr;
use crate::program::Program;
use crate::statement::AssignStmt;
use crate::statement::BeginStmt;
use crate::statement::CallStmt;
use crate::statement::IfStmt;
use crate::statement::WhileStatement;
use crate::token::Token;
use crate::types::Ident;
use crate::types::Number;
use std::slice::Iter;

static mut TOKEN: Token = Token::Null;
static DEFAULT_STRING: &str = "";
static DEFAULT_NUMBER: i64 = 0;

fn next(iter: &mut Iter<Token>) {
    unsafe {
        match iter.next() {
            Some(element) => {
                TOKEN = element.clone();
            }
            None => {}
        }
    }
}

fn expect(expected: Token, iter: &mut Iter<Token>) {
    unsafe {
        if expected != TOKEN {
            panic!("syntax error, expected:{} and found:{}", expected, TOKEN);
        }
    }
    next(iter);
}

fn get_identifier(token: Token) -> String {
    // extract  value for identifiers.
    match &token {
        Token::Ident(id) => id.clone(),
        _ => {
            panic!()
        }
    }
}

fn get_string_literal(token: Token) -> String {
    // extract  value for string literal.
    match &token {
        Token::StringLiteral(literal) => literal.clone(),
        _ => {
            panic!()
        }
    }
}

fn get_numeric_literal(token: Token) -> i64 {
    // extract  value for numeric literals.
    match &token {
        Token::Number(n) => *n,
        _ => {
            panic!()
        }
    }
}

/* block	= [ "const" ident "=" number { "," ident "=" number } ";" ]
 *            ["var" ident {"," ident} ";"]
 *	    	  { "procedure" ident ";" block ";" } statement .
*/
fn block(iter: &mut Iter<Token>) -> Option<Box<dyn Node>> {
    unsafe {
        let mut const_decl = ConstDecl::default();
        let mut var_decl = VarDecl::default();
        let proc_decl;
        if TOKEN == Token::Const {
            let mut consts = Vec::<(String, i64)>::new();
            let mut num: i64;
            let mut id: String;
            expect(Token::Const, iter);
            id = get_identifier(TOKEN.clone());
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            expect(Token::Equal, iter);
            num = get_numeric_literal(TOKEN.clone());
            expect(Token::Number(DEFAULT_NUMBER), iter);
            consts.push((id.clone(), num));
            while TOKEN == Token::Comma {
                expect(Token::Comma, iter);
                id = get_identifier(TOKEN.clone());
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                expect(Token::Equal, iter);
                num = get_numeric_literal(TOKEN.clone());
                expect(Token::Number(DEFAULT_NUMBER), iter);
                consts.push((id.clone(), num));
            }
            expect(Token::Semicolon, iter);
            const_decl = ConstDecl::new(consts);
        }
        if TOKEN == Token::Var {
            let mut idents = Vec::<String>::new();
            let mut id: String;
            expect(Token::Var, iter);
            id = get_identifier(TOKEN.clone());
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            idents.push(id);
            while TOKEN == Token::Comma {
                expect(Token::Comma, iter);
                id = get_identifier(TOKEN.clone());
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                idents.push(id);
            }
            expect(Token::Semicolon, iter);
            var_decl = VarDecl::new(idents);
        }
        let mut procedurs = Vec::new();
        while TOKEN == Token::Procedure {
            expect(Token::Procedure, iter);
            let name = get_identifier(TOKEN.clone());
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            expect(Token::Semicolon, iter);
            let block = block(iter);
            expect(Token::Semicolon, iter);
            procedurs.push((name, block));
        }
        proc_decl = ProcDecl::new(procedurs);
        let stmt = statement(iter);
        return Some(Box::new(Block::new(const_decl, var_decl, proc_decl, stmt)));
    }
}

/* statement= [ ident ":=" expression
 *  		  | "call" ident
 *	    	  | "begin" statement { ";" statement } "end"
 *		      | "if" condition "then" statement [ "else" statement ]
 *		      | "while" condition "do" statement
 *		      | "readInt" ident
 *		      | "readChar" ident
 *		      | "writeInt" expression
 *	    	  | "writeChar" expression
 *  		  | "writeStr" ( ident | string )
 *	    	  | "exit" expression ]  .
 */
fn statement(iter: &mut Iter<Token>) -> Option<Box<dyn Node>> {
    unsafe {
        match &TOKEN {
            Token::Ident(_) => {
                let id = get_identifier(TOKEN.clone());
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                expect(Token::Assign, iter);
                let expr = expression(iter);
                return Some(Box::new(AssignStmt::new(id.clone(), expr)));
            }
            Token::Call => {
                expect(Token::Call, iter);
                let id = get_identifier(TOKEN.clone());
                expect(Token::Ident(TOKEN.to_string()), iter);
                return Some(Box::new(CallStmt::new(id)));
            }
            Token::Begin => {
                expect(Token::Begin, iter);
                let mut stmts = Vec::new();
                //statement(iter);
                stmts.push(statement(iter));
                while TOKEN == Token::Semicolon {
                    expect(Token::Semicolon, iter);
                    //statement(iter);
                    stmts.push(statement(iter));
                }
                expect(Token::End, iter);
                return Some(Box::new(BeginStmt::new(stmts)));
            }
            Token::If => {
                expect(Token::If, iter);
                let condition = condition(iter);
                expect(Token::Then, iter);
                let true_stmt = statement(iter);
                let mut false_stmt = None;
                if TOKEN == Token::Else {
                    expect(Token::Else, iter);
                    false_stmt = statement(iter);
                }
                return Some(Box::new(IfStmt::new(condition, true_stmt, false_stmt)));
            }
            Token::While => {
                expect(Token::While, iter);
                let cond = condition(iter);
                expect(Token::Do, iter);
                let body = statement(iter);
                return Some(Box::new(WhileStatement::new(cond, body)));
            }
            Token::WriteInt => {
                expect(Token::WriteInt, iter);
                if TOKEN == Token::LParen {
                    expect(Token::LParen, iter);
                }
                let expr = expression(iter);
                if TOKEN == Token::RParen {
                    expect(Token::RParen, iter);
                }
                return Some(Box::new(WriteInt::new(expr)));
            }
            Token::WriteChar => {
                expect(Token::WriteChar, iter);
                if TOKEN == Token::LParen {
                    expect(Token::LParen, iter);
                }
                let expr = expression(iter);
                if TOKEN == Token::RParen {
                    expect(Token::RParen, iter);
                }
                return Some(Box::new(WriteChar::new(expr)));
            }
            Token::WriteStr => {
                expect(Token::WriteStr, iter);
                expect(Token::LParen, iter);
                match &TOKEN {
                    Token::Ident(_) => {
                        let id = get_identifier(TOKEN.clone());
                        expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                        expect(Token::RParen, iter);
                        return Some(Box::new(WriteStr::new(id.clone())));
                    }
                    Token::StringLiteral(_) => {
                        let str = get_string_literal(TOKEN.clone());
                        expect(Token::StringLiteral(DEFAULT_STRING.to_string()), iter);
                        expect(Token::RParen, iter);
                        return Some(Box::new(WriteStr::new(str)));
                    }
                    _ => {
                        panic!("writeStr takes a string");
                    }
                }
            }
            Token::ReadInt => {
                expect(Token::ReadInt, iter);
                if TOKEN == Token::LParen {
                    expect(Token::LParen, iter);
                }
                let ident = get_identifier(TOKEN.clone());
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                if TOKEN == Token::RParen {
                    expect(Token::RParen, iter);
                }
                return Some(Box::new(ReadInt::new(ident)));
            }
            Token::ReadChar => {
                expect(Token::ReadChar, iter);
                if TOKEN == Token::LParen {
                    expect(Token::LParen, iter);
                }
                let ident = get_identifier(TOKEN.clone());
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                if TOKEN == Token::RParen {
                    expect(Token::RParen, iter);
                }
                return Some(Box::new(ReadChar::new(ident)));
            }
            Token::Exit => {
                expect(Token::Exit, iter);
                let expr = expression(iter);
                return Some(Box::new(Exit::new(expr)));
            }
            _ => {
                return None;
            }
        }
    }
}

// condition	= "odd" expression
//      		| expression ( comparator ) expression .
fn condition(iter: &mut Iter<Token>) -> Option<Box<dyn Node>> {
    unsafe {
        if TOKEN == Token::Odd {
            expect(Token::Odd, iter);
            let expr = expression(iter);
            return Some(Box::new(OddCondition::new(expr)));
        } else {
            let left = expression(iter);
            let mut op = String::from("");
            match &TOKEN {
                Token::Equal
                | Token::Hash
                | Token::LessThan
                | Token::GreaterThan
                | Token::LessThanEqual
                | Token::GreaterThanEqual => {
                    op = TOKEN.to_string();
                    next(iter);
                }
                _ => {
                    println!("invalid conditional");
                }
            }
            let right = expression(iter);
            return Some(Box::new(RelationalCondition::new(left, right, op)));
        }
    }
}

//expression	= [ "+" | "-" | "not" ] term { ( "+" | "-" | "or" ) term }
fn expression(iter: &mut Iter<Token>) -> Option<Box<dyn Node>> {
    unsafe {
        if TOKEN == Token::Plus || TOKEN == Token::Minus || TOKEN == Token::Not {
            next(iter);
        }
        let mut lhs = term(iter);
        while TOKEN == Token::Plus || TOKEN == Token::Minus || TOKEN == Token::Or {
            let operator = TOKEN.to_string();
            next(iter);
            let rhs = term(iter);
            lhs = Some(Box::new(BinOp::new(lhs, rhs, operator)));
        }
        return lhs;
    }
}

//term		= factor { ( "*" | "/" | "mod" | "and" ) factor }
fn term(iter: &mut Iter<Token>) -> Option<Box<dyn Node>> {
    unsafe {
        let mut lhs = factor(iter);
        while TOKEN == Token::Multiply
            || TOKEN == Token::Divide
            || TOKEN == Token::Modulo
            || TOKEN == Token::And
        {
            let operator = TOKEN.to_string();
            next(iter);
            let rhs = factor(iter);
            lhs = Some(Box::new(BinOp::new(lhs, rhs, operator)));
        }
        return lhs;
    }
}

/* factor	= ident | number | "(" expression ")" . */
fn factor(iter: &mut Iter<Token>) -> Option<Box<dyn Node>> {
    unsafe {
        match &TOKEN {
            Token::Ident(_) => {
                let id = get_identifier(TOKEN.clone());
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                return Some(Box::new(Ident::new(id.to_string())));
            }
            Token::Number(_) => {
                let num = get_numeric_literal(TOKEN.clone());
                expect(Token::Number(DEFAULT_NUMBER), iter);
                return Some(Box::new(Number::new(num)));
            }
            Token::LParen => {
                expect(Token::LParen, iter);
                let expr = expression(iter);
                expect(Token::RParen, iter);
                return expr;
            }
            _ => {
                panic!("Wrong token!");
            }
        }
    }
}

// program	= block "."
fn program(tokens: &mut Vec<Token>) -> Option<Box<dyn Node>> {
    let mut iter: Iter<Token> = tokens.iter();
    next(&mut iter);
    let block = block(&mut iter);
    expect(Token::Dot, &mut iter);
    let dot = String::from(".");
    return Some(Box::new(Program::new(block, dot)));
}
pub fn parse(tokens: &mut Vec<Token>) -> Option<Box<dyn Node>> {
    return program(tokens);
}
