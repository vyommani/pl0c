use crate::LineNumber;
use crate::{semantic::symboltable::SymbolTable, frontend::token::Token};
use std::{iter::Peekable, str::Chars};
use crate::errors::{Pl0Error, Pl0Result};

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    state: &'a mut LineNumber,
}

impl<'a> Lexer<'a> {
    pub fn new(file_content: &'a str, state: &'a mut LineNumber) -> Self {
        Self {
            chars: file_content.chars().peekable(),
            state,
        }
    }

    pub fn scan(mut self, _table: &mut SymbolTable) -> Pl0Result<Vec<(Token, usize)>> {
        let mut tokens = Vec::new();
        while self.chars.peek().is_some() {
            self.skip_whitespace_and_comments()?;
            if let Some(&_ch) = self.chars.peek() {
                let line = self.state.line;
                let token = self.scan_token()?;
                if let Some(token) = token {
                    tokens.push((token, line));
                }
            }
        }
        Ok(tokens)
    }

    fn skip_whitespace_and_comments(&mut self) -> Pl0Result<()> {
        loop {
            match self.chars.peek() {
                Some(&'{') => self.scan_comment()?,
                Some(&ch) if ch.is_whitespace() => self.scan_whitespace(),
                _ => break,
            }
        }
        Ok(())
    }

    fn scan_token(&mut self) -> Pl0Result<Option<Token>> {
        match self.chars.peek() {
            None => Ok(None),
            Some(&ch) if ch.is_alphabetic() || ch == '_' => Ok(Some(self.scan_identifier()?)),
            Some(&ch) if ch.is_ascii_digit() => Ok(Some(self.scan_number()?)),
            Some(&'\'') => Ok(Some(self.scan_string_literal('\'')?)),
            Some(&'"') => Ok(Some(self.scan_string_literal('"')?)),
            Some(&':') => Ok(Some(self.scan_assignment()?)),
            Some(&'<') => Ok(Some(self.scan_less_than()?)),
            Some(&'>') => Ok(Some(self.scan_greater_than()?)),
            Some(&ch) => Ok(Some(self.scan_single_char_token(ch)?)),
        }
    }

    fn scan_comment(&mut self) -> Pl0Result<()> {
        self.chars.next(); // Consume '{'
        let start_line = self.state.line;
        while let Some(ch) = self.chars.next() {
            if ch == '\n' {
                self.state.line += 1;
            } else if ch == '}' {
                return Ok(());
            }
        }
        Err(Pl0Error::UnterminatedComment { line: start_line })
    }

    fn scan_whitespace(&mut self) {
        while let Some(&ch) = self.chars.peek() {
            if ch.is_whitespace() {
                if ch == '\n' {
                    self.state.line += 1;
                }
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn scan_identifier(&mut self) -> Pl0Result<Token> {
        let mut identifier = String::new();
        while let Some(&ch) = self.chars.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                identifier.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }
        Ok(self.keyword_or_identifier(identifier))
    }

    fn keyword_or_identifier(&self, identifier: String) -> Token {
        match identifier.as_str() {
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
            "write" => Token::Write,
            "writestr" | "writeStr" => Token::WriteStr,
            "read" => Token::Read,
            "into" => Token::Into,
            "size" => Token::Size,
            "exit" => Token::Exit,
            "and" => Token::And,
            "or" => Token::Or,
            "not" => Token::Not,
            "mod" => Token::Modulo,
            "forward" => Token::Forward,
            _ => Token::Ident(identifier),
        }
    }

    fn scan_number(&mut self) -> Pl0Result<Token> {
        let mut number_str = String::new();
        while let Some(&ch) = self.chars.peek() {
            if ch.is_ascii_digit() {
                number_str.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }
        number_str.parse::<i64>()
            .map(Token::Number)
            .map_err(|_| Pl0Error::InvalidNumber {number: number_str, line: self.state.line})
    }

    fn scan_string_literal(&mut self, quote_char: char) -> Pl0Result<Token> {
        self.chars.next(); // Consume opening quote
        let mut literal = String::new();
        loop {
            match self.chars.peek() {
                Some(&ch) if ch == quote_char => {
                    self.chars.next(); // Consume closing quote
                    break;
                }
                Some(&'\n') => {
                    self.state.line += 1;
                    return Err(Pl0Error::MultilineString { line: self.state.line });
                }
                Some(&ch) => {
                    literal.push(ch);
                    self.chars.next();
                }
                None => {
                    return Err(Pl0Error::UnterminatedString { line: self.state.line });
                }
            }
        }
        Ok(Token::StringLiteral(literal))
    }

    fn scan_assignment(&mut self) -> Pl0Result<Token> {
        self.chars.next(); // Consume ':'
        match self.chars.peek() {
            Some(&'=') => {
                self.chars.next(); // Consume '='
                Ok(Token::Assign)
            }
            _ => Err(Pl0Error::lexer_error("Expected '=' after ':'", self.state.line)),
        }
    }

    fn scan_less_than(&mut self) -> Pl0Result<Token> {
        self.chars.next(); // Consume '<'
        match self.chars.peek() {
            Some(&'=') => {
                self.chars.next(); // Consume '='
                Ok(Token::LessThanEqual)
            }
            Some(&'>') => {
                self.chars.next(); // Consume '>'
                Ok(Token::Hash)
            }
            _ => Ok(Token::LessThan),
        }
    }

    fn scan_greater_than(&mut self) -> Pl0Result<Token> {
        self.chars.next(); // Consume '>'
        if self.chars.peek() == Some(&'=') {
            self.chars.next(); // Consume '='
            Ok(Token::GreaterThanEqual)
        } else {
            Ok(Token::GreaterThan)
        }
    }

    fn scan_single_char_token(&mut self, ch: char) -> Pl0Result<Token> {
        self.chars.next(); // Consume the character
        let token = match ch {
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
            '\0' => Token::Null,
            _ => return Err(Pl0Error::UnknownToken {token: ch, line: self.state.line}),
        };
        Ok(token)
    }
}

// Convenience function to maintain the original API
pub fn scan(state: &mut LineNumber, file_content: &str, table: &mut SymbolTable) -> Pl0Result<Vec<(Token, usize)>> {
    let lexer = Lexer::new(file_content, state);
    lexer.scan(table)
}