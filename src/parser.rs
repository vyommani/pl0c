use crate::token::Token;
use crate::ast::{Node, Number};
use std::slice::Iter;

static mut TOKEN: Token = Token::Null;

static DEFAULT_STRING: &str = "";
static DEFAULT_NUMBER: i64 = 0;

fn next(iter: &mut Iter<Token>) {
    unsafe {
        match iter.next() {
            Some(element) => {
                TOKEN = element.clone();
            },
            None => {},
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

/* block	= [ "const" ident "=" number { "," ident "=" number } ";" ]
 *	    	  [ "var" ident [ array ] { "," ident [ array ] } ";" ]
 *	    	  { "forward" ident ";" }
 *	    	  { "procedure" ident ";" block ";" } statement .
*/
fn block(iter: &mut Iter<Token>) {
    unsafe {
        if TOKEN == Token::Const {
            expect(Token::Const, iter);
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            expect(Token::Equal, iter);
            expect(Token::Number(DEFAULT_NUMBER), iter);
            while TOKEN == Token::Comma {
                expect(Token::Comma, iter);
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                expect(Token::Equal, iter);
                expect(Token::Number(DEFAULT_NUMBER), iter);
            }
            expect(Token::Semicolon, iter); // First line in grammer is complete.
        }
        if TOKEN == Token::Var {
            expect(Token::Var, iter);
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            if TOKEN == Token::Size {
                expect(Token::Size, iter);
                expect(Token::Number(DEFAULT_NUMBER), iter);
            }
            while TOKEN == Token::Comma {
                expect(Token::Comma, iter);
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                if TOKEN == Token::Size {
                    expect(Token::Size, iter);
                    if TOKEN == Token::Number(DEFAULT_NUMBER) {
                        expect(Token::Number(DEFAULT_NUMBER), iter);
                    }
                }
            } //Second line in grammer is clmplete
            expect(Token::Semicolon, iter);
        }
        while TOKEN == Token::Forward {
            expect(Token::Forward, iter);
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            expect(Token::Semicolon, iter);
        } //third  line in grammer is clmplete
        while TOKEN == Token::Procedure {
            expect(Token::Procedure, iter);
            expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            expect(Token::Semicolon, iter);
            block(iter);
            expect(Token::Semicolon, iter);
        } //fourth  line in grammer is clmplete
        statement(iter);
    }
}

/* statement= [ ident ":=" expression
 *  		  | "call" ident
 *	    	  | "begin" statement { ";" statement } "end"
 *		      | "if" condition "then" statement [ "else" statement ]
 *		      | "while" condition "do" statement
 *		      | "readInt" [ "into" ] ident
 *		      | "readChar" [ "into" ] ident
 *		      | "writeInt" expression
 *	    	  | "writeChar" expression
 *  		  | "writeStr" ( ident | string )
 *	    	  | "exit" expression ]  .
 */
fn statement(iter: &mut Iter<Token>) {
    unsafe {
        match &TOKEN {
            Token::Ident(_id) => {
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                /*if TOKEN == Token::LBrack {
                    expect(Token::LBrack, iter);
                    expression(iter);
                    expect(Token::RBrack, iter);
                }*/
                expect(Token::Assign, iter);
                expression(iter);
            }//first  line in grammer is clmplete
            Token::Call => {
                expect(Token::Call, iter);
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            }//second  line in grammer is clmplete
            Token::Begin => {
                expect(Token::Begin, iter);
                statement(iter);
                while TOKEN == Token::Semicolon {
                    expect(Token::Semicolon, iter);
                    statement(iter);
                }
                expect(Token::End, iter);
            }//third  line in grammer is clmplete
            Token::If => {
                expect(Token::If, iter);
                condition(iter);
                expect(Token::Then, iter);
                statement(iter);
                if TOKEN == Token::Else {
                    expect(Token::Else, iter);
                    statement(iter);
                }
            }//fourth  line in grammer is clmplete
            Token::While => {
                expect(Token::While, iter);
                condition(iter);
                expect(Token::Do, iter);
                statement(iter);
            }//fifth  line in grammer is clmplete
            Token::WriteInt => {
                expect(Token::WriteInt, iter);
                expression(iter);
            }//sixth  line in grammer is clmplete
            Token::WriteChar => {
                expect(Token::WriteChar, iter);
                expression(iter);
            }//seventh  line in grammer is clmplete
            Token::WriteStr => {
                expect(Token::WriteStr, iter);
                match &TOKEN {
                    Token::Ident(_id) => {
                        expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
                    }
                    Token::String => {
                        expect(Token::String, iter);
                    }
                    _ => {
                        panic!("writeStr takes an array or a string");
                    }
                }
            }//eight  line in grammer is clmplete
            Token::ReadInt => {
                expect(Token::ReadInt, iter);
                if TOKEN == Token::Into {
                    expect(Token::Into, iter);
                }
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            }//nineth  line in grammer is clmplete
            Token::ReadChar => {
                expect(Token::ReadChar, iter);
                if TOKEN == Token::Into {
                    expect(Token::Into, iter);
                }
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
            }//tenth  line in grammer is clmplete
            Token::Exit => {
                expect(Token::Exit, iter);
                expression(iter);
            }//eleventh  line in grammer is clmplete
            _ => { /* If there is another token that will be handled by RDP algo.*/}
        }
    }
}

// condition	= "odd" expression
//      		| expression ( comparator ) expression .
fn condition(iter: &mut Iter<Token>) {
    unsafe {
        if TOKEN == Token::Odd {
            expect(Token::Odd, iter);
            expression(iter);
        } else {    
            expression(iter);
            match &TOKEN {
                Token::Equal | Token::Hash | Token::LessThan | Token::GreaterThan
                | Token::LessThanEqual | Token::GreaterThanEqual => {
                    next(iter);
                },
                _ => {
                    println!("invalid conditional");
                },
            }
            expression(iter);
        }
    }
}

//expression	= [ "+" | "-" | "not" ] term { ( "+" | "-" | "or" ) term }
fn expression(iter: &mut Iter<Token>) {
    unsafe {
        if TOKEN == Token::Plus || TOKEN == Token::Minus || TOKEN == Token::Not {
            next(iter);
        }
        term(iter);
        while TOKEN == Token::Plus || TOKEN == Token::Minus || TOKEN == Token::Or {
            next(iter);
            term(iter);
        }
    }
}

//term		= factor { ( "*" | "/" | "mod" | "and" ) factor }
fn term(iter: &mut Iter<Token>) {
    unsafe {
        factor(iter);
        while TOKEN == Token::Multiply
            || TOKEN == Token::Divide
            || TOKEN == Token::Modulo
            || TOKEN == Token::And
        {
            next(iter);
            factor(iter);
        }
    }
}

/* factor	= ident
 *		    | number
 *		    | "(" expression ")" .
 */
fn factor(iter: &mut Iter<Token>) {
    unsafe {
        match &TOKEN {
            Token::Ident(_id) => {
                expect(Token::Ident(DEFAULT_STRING.to_string()), iter);
               if TOKEN == Token::LBrack {
                    expect(Token::LBrack, iter);
                    expression(iter);
                    expect(Token::RBrack, iter);
                }
            },
            Token::Number(_) => {
                expect(Token::Number(DEFAULT_NUMBER), iter);
            }
            Token::LParen => {
                expect(Token::LParen, iter);
                expression(iter);
                expect(Token::RParen, iter);
            }
            _ => {
                panic!("Wrong token!");
            }
        }
    }
}

// program	= block "." 
fn program(tokens: &mut Vec<Token>){
    let mut iter: Iter<Token> = tokens.iter();
    next(&mut iter);
    block(&mut iter);
    expect(Token::Dot, &mut iter);
}
pub fn parse(tokens: &mut Vec<Token>) -> Result<Box<dyn Node>, &str> {
    program(tokens);
    Ok(Box::new(Number::new(0))) // currently AST is not yet implemented.... 
}
