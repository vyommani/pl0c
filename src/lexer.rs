use crate::LineNumber;
use crate::{symboltable::SymbolTable, token::Token};
use std::{iter::Peekable, str::Chars};
use crate::errors::{Pl0Error, Pl0Result};
pub fn scan(
    state: &mut LineNumber,
    file_content: &str,
    _table: &mut SymbolTable,
) -> Pl0Result<Vec<(Token, usize)>> {
    let mut chars = file_content.chars().peekable();
    let mut lexeme: Vec<(Token, usize)> = vec![];
    let mut lookahead = false;

    'lexer: loop {
        if let Some(c) = chars.peek() {
            if (*c).eq(&'{') {
                let _ = comment(&mut chars, state);
            } else if (*c).is_whitespace() {
                whitespace(&mut chars, state);
            } else if (*c).is_alphabetic() || (*c).eq(&'_') {
                let token = identifier(&mut chars, state)?;
                lexeme.push((token, state.line));
            } else if (*c).is_numeric() {
                let token = number(&mut chars, state)?;
                lexeme.push((token, state.line));
            } else if (*c).eq(&':') {
                let token = assignment(&mut chars, state)?;
                lexeme.push((token, state.line));
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
                            return Err(Pl0Error::UnknownToken {token: ':',line: state.line,});
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
                        return Err(Pl0Error::UnknownToken {token: ':',line: state.line,});
                    }
                };
                lexeme.push((token, state.line));
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

fn comment(chars: &mut Peekable<Chars<'_>>, state: &mut LineNumber) -> Pl0Result<()> {
    chars.next(); // Consume '{'
    let line = state.line;
    for c in chars.by_ref() {
        if c == '\n' {
            state.line += 1;
        } else if c == '}' {
            return Ok(());
        }
    }
    Err(Pl0Error::UnterminatedComment { line })
}

pub fn get_string_literal(
    chars: &mut Peekable<Chars<'_>>,
    state: &mut LineNumber,
) -> Pl0Result<Token> {
    let mut string_literal = String::new();
    chars.next(); // consume the opening '
    loop {
        if let Some(c) = chars.peek() {
            if (*c) == '\'' {
                break;
            } else if (*c) == '\n' {
                state.line += 1;
                return Err(Pl0Error::MultilineString { line: state.line });
            } else if (*c) == '\0' {
                return Err(Pl0Error::UnterminatedString { line: state.line });
            } else {
                string_literal.push(*c);
                chars.next();
            }
        } else {
            return Err(Pl0Error::UnterminatedString { line: state.line });
        }
    }
    Ok(Token::StringLiteral(string_literal))
}

fn whitespace(chars: &mut Peekable<Chars<'_>>, state: &mut LineNumber) {
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            if c == '\n' {
                state.line += 1;
            }
            chars.next();
        } else {
            break;
        }
    }
}

pub fn assignment(
    chars: &mut Peekable<Chars<'_>>,
    state: &mut LineNumber,
) -> Pl0Result<Token> {
    chars.next();
    if let Some(c) = chars.peek() {
        if (*c).eq(&'=') {
            chars.next();
            Ok(Token::Assign)
        } else {
            Err(Pl0Error::lexer_error("Expected '=' after ':'", state.line))
        }
    } else {
        Err(Pl0Error::lexer_error("Unexpected end of file after ':'", state.line))
    }
}

pub fn identifier(
    chars: &mut Peekable<Chars<'_>>,
    state: &mut LineNumber,
) -> Pl0Result<Token> {
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
            return Err(Pl0Error::UnterminatedString { line: state.line });
        }
    }
    static KEYWORDS: &[(&str, Token)] = &[("const", Token::Const), ("var", Token::Var), ("procedure", Token::Procedure), ("call", Token::Call),
        ("begin", Token::Begin), ("end", Token::End), ("if", Token::If), ("then", Token::Then), ("else", Token::Else), ("while", Token::While), 
        ("do", Token::Do), ("odd", Token::Odd), ("write", Token::Write), ("writestr", Token::WriteStr), ("writeStr", Token::WriteStr),
        ("read", Token::Read), ("into", Token::Into), ("size", Token::Size), ("exit", Token::Exit), ("and", Token::And),
        ("or", Token::Or), ("not", Token::Not), ("mod", Token::Modulo), ("forward", Token::Forward),
    ];

    let binding = Token::Ident(idt.clone());
    let token = KEYWORDS
        .iter()
        .find(|&&(k, _)| k == idt)
        .map(|(_, t)| t)
        .unwrap_or(&binding);
    Ok(token.clone())
}

pub fn number(chars: &mut Peekable<Chars<'_>>, state: &mut LineNumber) -> Pl0Result<Token> {
    let mut number_str = String::new();
    while let Some(c) = chars.peek() {
        if c.is_numeric() {
            number_str.push(*c);
            chars.next();
        } else {
            break;
        }
    }
    match number_str.parse::<i64>() {
        Ok(num) => Ok(Token::Number(num)),
        Err(_) => Err(Pl0Error::InvalidNumber {number: number_str, line: state.line,}),
    }
}
