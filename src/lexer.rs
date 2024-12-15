use crate::token::Token;
use crate::LineNumber;
use core::panic;
use std::{iter::Peekable, str::Chars};

pub fn scan(state: &mut LineNumber, file_content: &str) -> Result<Vec<Token>, String> {
    let mut chars = file_content.chars().peekable();
    let mut lexeme: Vec<Token> = vec![];
    let mut lookahead = false;

    'lexer: loop {
        if let Some(c) = chars.peek() {
            if (*c).eq(&'{') {
                comment(&mut chars, state);
            } else if (*c).is_whitespace() {
                whitespace(&mut chars, state);
            } else if (*c).is_alphabetic() || (*c).eq(&'_') {
                let token = identifier(&mut chars, state)?;
                lexeme.push(token);
            } else if (*c).is_numeric() {
                let token = number(&mut chars, state)?;
                lexeme.push(token);
            } else if (*c).eq(&':') {
                let token = assignment(&mut chars, state)?;
                lexeme.push(token);
            } else {
                let token = match *c {
                    '.' => Token::Dot,
                    '=' => Token::Equal,
                    ',' => Token::Comma,
                    ';' => Token::Semicolon,
                    '#' => Token::Hash,
                    '+' => Token::Plus,
                    '-' => Token::Minus,
                    '*' => Token::Multiply,
                    '/' => Token::Divide,
                    '%' => Token::Modulo,
                    '(' => Token::LParen,
                    ')' => Token::RParen,
                    '[' => Token::LBrack,
                    ']' => Token::RBrack,
                    '<' => {
                        lookahead = true;
                        chars.next(); // look ahead one charcter
                        if let Some(c) = chars.peek() {
                            if *c == '=' {
                                chars.next(); //consume the '=' character
                                Token::LessThanEqual
                            } else if *c == '>' {
                                chars.next(); //consume the '>' character
                                Token::Hash
                            } else {
                                Token::LessThan
                            }
                        } else {
                            Token::LessThan
                        }
                    }
                    '>' => {
                        lookahead = true;
                        chars.next(); // look ahead one charcter
                        if chars.peek() == Some(&'=') {
                            chars.next(); //consume the '=' character
                            Token::GreaterThanEqual
                        } else {
                            Token::GreaterThan
                        }
                    }
                    ':' => {
                        lookahead = true;
                        chars.next(); // look ahead one charcter
                        if chars.peek() != Some(&'=') {
                            panic!("Unknown token ");
                        }
                        chars.next(); // consume the '=' character.
                        Token::Assign
                    }
                    '\'' => match get_string_literal(&mut chars, state) {
                        Ok(literal) => literal,
                        Err(_) => Token::StringLiteral("".to_string()),
                    },
                    '\0' => Token::Null,
                    _ => {
                        return Err(format!("Unknown token on line {}", state.line));
                    }
                };
                lexeme.push(token);
                if lookahead == true {
                    lookahead = false;
                } else {
                    chars.next();
                }
            }
        } else {
            break 'lexer;
        }
    }
    Ok(lexeme)
}

fn comment(chars: &mut Peekable<Chars<'_>>, state: &mut LineNumber) {
    let mut comment = String::new();
    chars.next(); // consume the opening curly brace
    'comment: for c in chars.by_ref() {
        if c == '\n' {
            state.line += 1;
        }
        if c == '\0' {
            panic!("Unterminited comment.");
        }
        if c == '}' {
            break 'comment;
        }
        comment.push(c);
    }
}

pub fn get_string_literal(
    chars: &mut Peekable<Chars<'_>>,
    state: &mut LineNumber,
) -> Result<Token, String> {
    let mut string_literal = String::new();
    chars.next(); // consume the opening '
    loop {
        if let Some(c) = chars.peek() {
            if (*c) == '\'' {
                break;
            } else if (*c) == '\n' {
                state.line += 1;
                return Err(format!(
                    "multiline string literal not supported {}",
                    state.line
                ));
            } else if (*c) == '\0' {
                return Err(format!(
                    "unterminited  string literal on line {}",
                    state.line
                ));
            } else {
                string_literal.push(*c);
                chars.next();
            }
        } else {
            return Err(format!(
                "Unterminated string literal on line {}",
                state.line
            ));
        }
    }
    Ok(Token::StringLiteral(string_literal))
}

fn whitespace(chars: &mut Peekable<Chars<'_>>, state: &mut LineNumber) {
    'whitespace: for c in chars.by_ref() {
        if c == '\n' {
            state.line += 1;
        }
        if c != ' ' || c != '\t' || c != '\n' || c != '\r' {
            break 'whitespace;
        }
    }
}
pub fn assignment(
    chars: &mut Peekable<Chars<'_>>,
    state: &mut LineNumber,
) -> Result<Token, String> {
    chars.next();
    if let Some(c) = chars.peek() {
        if (*c).eq(&'=') {
            chars.next();
            Ok(Token::Assign)
        } else {
            Err(format!("Unknown token ':' on line {}", state.line))
        }
    } else {
        Err(format!("Unterminated assignment on line {}", state.line))
    }
}

pub fn identifier(
    chars: &mut Peekable<Chars<'_>>,
    state: &mut LineNumber,
) -> Result<Token, String> {
    let mut idt = String::new();
    loop {
        if let Some(c) = chars.peek() {
            if (*c).is_alphanumeric() || (*c).eq(&'_') {
                idt.push(*c);
                chars.next();
            } else {
                break;
            }
        } else {
            return Err(format!("Unterminated identifier on line {}", state.line));
        }
    }

    let token = match idt.as_str() {
        "const" => Token::Const,
        "var" => Token::Var,
        "procedure" => Token::Procedure,
        "call" => Token::Call,
        "begin" => Token::Begin,
        "end" => Token::End,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "while" => Token::While,
        "do" => Token::Do,
        "odd" => Token::Odd,
        "writeint" | "writeInt" => Token::WriteInt,
        "writechar" | "writeChar" => Token::WriteChar,
        "writestr" | "writeStr" => Token::WriteStr,
        "readint" | "readInt" => Token::ReadInt,
        "readchar" | "readChar" => Token::ReadChar,
        "into" => Token::Into,
        "size" => Token::Size,
        "exit" => Token::Exit,
        "and" => Token::And,
        "or" => Token::Or,
        "not" => Token::Not,
        "mod" => Token::Modulo,
        "forward" => Token::Forward,
        _ => Token::Ident(idt),
    };
    Ok(token)
}

pub fn number(chars: &mut Peekable<Chars<'_>>, state: &mut LineNumber) -> Result<Token, String> {
    let mut num = String::new();
    loop {
        if let Some(c) = chars.peek() {
            if (*c).is_numeric() || (*c).eq(&'_') {
                num.push(*c);
                chars.next();
            } else {
                break;
            }
        } else {
            return Err(format!("unterminated number on line {}", state.line));
        }
    }
    if let Ok(val) = num.parse::<i64>() {
        Ok(Token::Number(val))
    } else {
        Err(format!("Invalid number at line {}", state.line))
    }
}
