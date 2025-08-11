use crate::ast::Exit;
use crate::ast::{ExpressionNode, Node};
use crate::block::Block;
use crate::decl::ConstDecl;
use crate::decl::ProcDecl;
use crate::decl::VarDecl;
use crate::expression::BinOp;
use crate::expression::OddCondition;
use crate::expression::RelationalCondition;
use crate::io::Read;
use crate::io::Write;
use crate::io::WriteStr;
use crate::program::Program;
use crate::statement::AssignStmt;
use crate::statement::BeginStmt;
use crate::statement::CallStmt;
use crate::statement::IfStmt;
use crate::statement::WhileStatement;
use crate::symboltable::Symbol;
use crate::symboltable::SymbolLocation;
use crate::symboltable::SymbolTable;
use crate::symboltable::SymbolType;
use crate::token::Token;
use crate::types::Ident;
use crate::types::Number;
use std::collections::HashMap;
use std::process::exit;
use std::slice::Iter;

use crate::config::parser::rename_identifier;
use crate::config::parser::get_renamed_identifier;

static mut TOKEN: Token = Token::Null;
static mut LINE_NUMBER: usize = 1;
static DEFAULT_STRING: &str = "";
static DEFAULT_NUMBER: i64 = 0;

fn next(iter: &mut Iter<(Token, usize)>) {
    unsafe {
        match iter.next() {
            Some((element, line)) => {
                TOKEN = element.clone();
                LINE_NUMBER = *line;
            }
            None => {}
        }
    }
}

fn expect(expected: Token, iter: &mut Iter<(Token, usize)>) {
    unsafe {
        if expected != TOKEN {
            println!(
                "syntax error, expected: {} and found: {} at line: {}",
                expected, TOKEN, LINE_NUMBER
            );
            exit(1);
        }
    }
    next(iter);
}

fn get_identifier(token: Token) -> String {
    // extract  value for identifiers.
    match &token {
        Token::Ident(id) => id.clone(),
        _ => {
            println!("Not able to extract the identifier");
            exit(1);
        }
    }
}

fn get_string_literal(token: Token) -> String {
    // extract  value for string literal.
    match &token {
        Token::StringLiteral(literal) => literal.clone(),
        _ => {
            println!("Not able to extract the string literal");
            exit(1);
        }
    }
}

fn get_numeric_literal(token: Token) -> i64 {
    // extract  value for numeric literals.
    match &token {
        Token::Number(n) => *n,
        _ => {
            println!("Not able to extract the numeric literal");
            exit(1);
        }
    }
}

/* block	= [ "const" ident "=" number { "," ident "=" number } ";" ]
 *            ["var" ident {"," ident} ";"]
 *	    	  { "procedure" ident ";" block ";" } statement .
*/
fn block(iter: &mut Iter<(Token, usize)>, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Option<Box<dyn Node>> {
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
            id = rename_identifier(&id, true, mapped_identifiers);
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            expect(Token::Equal, iter);
            num = get_numeric_literal(TOKEN.clone());
            table.insert(
                &id,
                Symbol::new(
                    SymbolType::Constant(num),
                    LINE_NUMBER,
                    SymbolLocation::GlobalLabel(id.clone()),
                    true,
                    table.get_scopes_len() - 1,
                ),
            );
            expect(Token::Number(DEFAULT_NUMBER), iter);
            consts.push((id.clone(), num));
            while TOKEN == Token::Comma {
                expect(Token::Comma, iter);
                id = get_identifier(TOKEN.clone());
                id = rename_identifier(&id, true, mapped_identifiers);
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                expect(Token::Equal, iter);
                num = get_numeric_literal(TOKEN.clone());
                table.insert(
                    &id,
                    Symbol::new(
                        SymbolType::Constant(num),
                        LINE_NUMBER,
                        SymbolLocation::GlobalLabel(id.clone()),
                        true,
                        table.get_scopes_len() - 1,
                    ),
                );
                expect(Token::Number(DEFAULT_NUMBER), iter);
                consts.push((id.clone(), num));
            }
            expect(Token::Semicolon, iter);
            const_decl = ConstDecl::new(consts);
        }
        let mut offset = 8; // Local offset for this block (procedure or global)
        if TOKEN == Token::Var {
            let mut idents = Vec::<String>::new();
            let mut id: String;
            expect(Token::Var, iter);
            id = get_identifier(TOKEN.clone());
            let is_global = table.get_scopes_len() == 1;
            id = rename_identifier(&id, is_global, mapped_identifiers);
            let location = if is_global {
                SymbolLocation::GlobalLabel(id.clone())
            } else {
                SymbolLocation::StackOffset(offset)
            };
            table.insert(
                &id,
                Symbol::new(
                    SymbolType::Variable,
                    LINE_NUMBER,
                    location,
                    is_global,
                    table.get_scopes_len() - 1,
                ),
            );
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            idents.push(id);
            while TOKEN == Token::Comma {
                if !is_global {
                    offset += 8;
                }
                expect(Token::Comma, iter);
                id = get_identifier(TOKEN.clone());
                id = rename_identifier(&id, is_global, mapped_identifiers);
                let location = if is_global {
                    SymbolLocation::GlobalLabel(id.clone())
                } else {
                    SymbolLocation::StackOffset(offset)
                };
                table.insert(
                    &id,
                    Symbol::new(
                        SymbolType::Variable,
                        LINE_NUMBER,
                        location,
                        is_global,
                        table.get_scopes_len() - 1,
                    ),
                );
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                idents.push(id);
            }
            expect(Token::Semicolon, iter);
            var_decl = VarDecl::new(idents);
        }
        let mut procedurs = Vec::new();
        while TOKEN == Token::Procedure {
            // We have to always insert the procedure name in global(top) scope.
            expect(Token::Procedure, iter);
            let mut name = get_identifier(TOKEN.clone());
            name = rename_identifier(&name, true, mapped_identifiers);
            table.insert(
                &name,
                Symbol::new(
                    SymbolType::Procedure,
                    LINE_NUMBER,
                    SymbolLocation::GlobalLabel(name.clone()),
                    true,
                    table.get_scopes_len() - 1,
                ),
            );
            table.push_scope();
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            expect(Token::Semicolon, iter);
            let block = block(iter, table, mapped_identifiers);
            expect(Token::Semicolon, iter);
            procedurs.push((name, block));
        }
        proc_decl = ProcDecl::new(procedurs);
        let stmt = statement(iter, table, mapped_identifiers);
        return Some(Box::new(Block::new(const_decl, var_decl, proc_decl, stmt)));
    }
}

/* statement= [ ident ":=" expression
 *  		  | "call" ident
 *	    	  | "begin" statement { ";" statement } "end"
 *		      | "if" condition "then" statement [ "else" statement ]
 *		      | "while" condition "do" statement
 *		      | "read" ident
 *		      | "write" expression
 *  		  | "writeStr" ( ident | string )
 *	    	  | "exit" expression ]  .
 */
fn statement(iter: &mut Iter<(Token, usize)>, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Option<Box<dyn Node>> {
    unsafe {
        match &TOKEN {
            Token::Ident(_) => {
                let mut id = get_identifier(TOKEN.clone());
                id = get_renamed_identifier(&id, mapped_identifiers);
                table.type_check(&id, SymbolType::Identifier, LINE_NUMBER.clone());
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                expect(Token::Assign, iter);
                let expr = expression(iter, table, mapped_identifiers);
                return Some(Box::new(AssignStmt::new(id.clone(), expr)));
            }
            Token::Call => {
                expect(Token::Call, iter);
                let mut id = get_identifier(TOKEN.clone());
                id = get_renamed_identifier(&id, mapped_identifiers);
                table.type_check(&id, SymbolType::Procedure, LINE_NUMBER.clone());
                expect(Token::Ident(TOKEN.to_string()), iter);
                return Some(Box::new(CallStmt::new(id)));
            }
            Token::Begin => {
                expect(Token::Begin, iter);
                let mut stmts = Vec::new();
                stmts.push(statement(iter, table, mapped_identifiers));
                while TOKEN == Token::Semicolon {
                    expect(Token::Semicolon, iter);
                    stmts.push(statement(iter, table, mapped_identifiers));
                }
                expect(Token::End, iter);
                return Some(Box::new(BeginStmt::new(stmts)));
            }
            Token::If => {
                expect(Token::If, iter);
                let condition = condition(iter, table, mapped_identifiers);
                expect(Token::Then, iter);
                let true_stmt = statement(iter, table, mapped_identifiers);
                let mut false_stmt = None;
                if TOKEN == Token::Else {
                    expect(Token::Else, iter);
                    false_stmt = statement(iter, table, mapped_identifiers);
                }
                return Some(Box::new(IfStmt::new(condition, true_stmt, false_stmt)));
            }
            Token::While => {
                expect(Token::While, iter);
                let cond = condition(iter, table, mapped_identifiers);
                expect(Token::Do, iter);
                let body = statement(iter, table, mapped_identifiers);
                return Some(Box::new(WhileStatement::new(cond, body)));
            }
            Token::Write => {
                expect(Token::Write, iter);
                if TOKEN == Token::LParen {
                    expect(Token::LParen, iter);
                }
                let expr = expression(iter, table, mapped_identifiers);
                if TOKEN == Token::RParen {
                    expect(Token::RParen, iter);
                }
                return Some(Box::new(Write::new(expr)));
            }
            Token::WriteStr => {
                expect(Token::WriteStr, iter);
                expect(Token::LParen, iter);
                match &TOKEN {
                    Token::Ident(_) => {
                        let mut id = get_identifier(TOKEN.clone());
                        id = get_renamed_identifier(&id, mapped_identifiers);
                        table.type_check(&id, SymbolType::Identifier, LINE_NUMBER.clone());
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
                        println!("writeStr takes a string");
                        exit(0);
                    }
                }
            }
            Token::Read => {
                expect(Token::Read, iter);
                if TOKEN == Token::LParen {
                    expect(Token::LParen, iter);
                }
                let mut ident = get_identifier(TOKEN.clone());
                ident = get_renamed_identifier(&ident, mapped_identifiers);
                table.type_check(&ident, SymbolType::Identifier, LINE_NUMBER.clone());
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                if TOKEN == Token::RParen {
                    expect(Token::RParen, iter);
                }
                return Some(Box::new(Read::new(ident)));
            }
            Token::Exit => {
                expect(Token::Exit, iter);
                let expr = expression(iter, table, mapped_identifiers);
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
fn condition(
    iter: &mut Iter<(Token, usize)>,
    table: &mut SymbolTable,
    mapped_identifiers: &mut HashMap<String, String>
) -> Option<Box<dyn ExpressionNode>> {
    unsafe {
        if TOKEN == Token::Odd {
            expect(Token::Odd, iter);
            let expr = expression(iter, table, mapped_identifiers);
            return Some(Box::new(OddCondition::new(expr)));
        } else {
            let left = expression(iter, table, mapped_identifiers);
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
                    println!("invalid conditional at line {} ", LINE_NUMBER);
                }
            }
            let right = expression(iter, table, mapped_identifiers);
            return Some(Box::new(RelationalCondition::new(left, right, op)));
        }
    }
}

//expression	= [ "+" | "-" | "not" ] term { ( "+" | "-" | "or" ) term }
fn expression(
    iter: &mut Iter<(Token, usize)>,
    table: &mut SymbolTable,
    mapped_identifiers: &mut HashMap<String, String>
) -> Option<Box<dyn ExpressionNode>> {
    unsafe {
        if TOKEN == Token::Plus || TOKEN == Token::Minus || TOKEN == Token::Not {
            next(iter);
        }
        let mut lhs = term(iter, table, mapped_identifiers);
        while TOKEN == Token::Plus || TOKEN == Token::Minus || TOKEN == Token::Or {
            let operator = TOKEN.to_string();
            next(iter);
            let rhs = term(iter, table, mapped_identifiers);
            lhs = Some(Box::new(BinOp::new(lhs, rhs, operator)));
        }
        return lhs;
    }
}

//term		= factor { ( "*" | "/" | "mod" | "and" ) factor }
fn term(
    iter: &mut Iter<(Token, usize)>,
    table: &mut SymbolTable,
    mapped_identifiers: &mut HashMap<String, String>
) -> Option<Box<dyn ExpressionNode>> {
    unsafe {
        let mut lhs = factor(iter, table, mapped_identifiers);
        while TOKEN == Token::Multiply
            || TOKEN == Token::Divide
            || TOKEN == Token::Modulo
            || TOKEN == Token::And
        {
            let operator = TOKEN.to_string();
            next(iter);
            let rhs = factor(iter, table, mapped_identifiers);
            lhs = Some(Box::new(BinOp::new(lhs, rhs, operator)));
        }
        return lhs;
    }
}

/* factor	= ident | number | "(" expression ")" . */
fn factor(
    iter: &mut Iter<(Token, usize)>,
    table: &mut SymbolTable,
    mapped_identifiers: &mut HashMap<String, String>
) -> Option<Box<dyn ExpressionNode>> {
    unsafe {
        match TOKEN {
            Token::Ident(_) => {
                let mut id = get_identifier(TOKEN.clone());
                id = get_renamed_identifier(&id, mapped_identifiers);
                table.type_check(&id, SymbolType::Identifier, LINE_NUMBER.clone());
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
                let expr = expression(iter, table, mapped_identifiers);
                expect(Token::RParen, iter);
                return expr;
            }
            _ => {
                None // revisit it.
            }
        }
    }
}

// program	= block "."
fn program(tokens: &mut Vec<(Token, usize)>, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Option<Box<dyn Node>> {
    let mut iter: Iter<(Token, usize)> = tokens.iter();
    next(&mut iter);
    let block = block(&mut iter, table, mapped_identifiers);
    expect(Token::Dot, &mut iter);
    let dot = String::from(".");
    return Some(Box::new(Program::new(block, dot)));
}

pub fn parse(tokens: &mut Vec<(Token, usize)>, table: &mut SymbolTable) -> Option<Box<dyn Node>> {
    let mut mapped_identifiers = HashMap::new();
    return program(tokens, table, &mut mapped_identifiers);
}