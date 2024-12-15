use std::mem;
#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Number(i64),
    Const,
    Var,
    Procedure,
    Call,
    Begin,
    End,
    If,
    Then,
    Else,
    While,
    Do,
    Odd,
    Dot,
    Into,
    ReadInt,
    ReadChar,
    WriteInt,
    WriteChar,
    WriteStr,
    Equal,
    Comma,
    Semicolon,
    Assign,
    Hash,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Not,
    Plus,
    Minus,
    Or,
    Multiply,
    Divide,
    Modulo,
    And,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LAngBrack,
    RAngBrack,
    StringLiteral(String),
    Null,
    Size,
    Forward,
    Exit,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        //This can be used to compare enums that carry data, while disregarding the actual data:
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
