use pl0c::utils::errors::{Pl0Error, Pl0Result};
use pl0c::frontend::lexer::{scan};
use pl0c::semantic::symboltable::SymbolTable;
use pl0c::frontend::token::Token;
use pl0c::LineNumber;

#[test]
fn test_number_literals() -> Pl0Result<()> {
    let source = "123 456 0 999999";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let tokens = scan(&mut state, source, &mut table)?;
    assert_eq!(
        tokens,
        vec![
            (Token::Number(123), 1),
            (Token::Number(456), 1),
            (Token::Number(0), 1),
            (Token::Number(999999), 1),
        ]
    );
    Ok(())
}

#[test]
fn test_string_literal() -> Pl0Result<()> {
    let source = "writestr(\"Hello, World!\") .";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let tokens = scan(&mut state, source, &mut table)?;
    assert_eq!(
        tokens,
        vec![
            (Token::WriteStr, 1),
            (Token::LParen, 1),
            (Token::StringLiteral("Hello, World!".to_string()), 1),
            (Token::RParen, 1),
            (Token::Dot, 1),
        ]
    );
    Ok(())
}

#[test]
fn test_unterminated_string_literal() -> Pl0Result<()> {
    let source = "\"this is unterminated";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let result = scan(&mut state, source, &mut table);
    if let Err(Pl0Error::UnterminatedString { line }) = result {
        assert_eq!(line, 1);
        Ok(())
    } else {
        panic!("Expected a UnterminatedString for unterminated string, but got: {:?}", result);
    }
}

#[test]
fn test_multiline_string_literal_error() -> Pl0Result<()> {
    let source = "\"this is not\na valid string\"";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let result = scan(&mut state, source, &mut table);
    if let Err(Pl0Error::MultilineString { line }) = result {
        assert_eq!(line, 2);
        Ok(())
    } else {
        panic!("Expected a MultilineString error, but got: {:?}", result);
    }
}

#[test]
fn test_single_line_comment() -> Pl0Result<()> {
    let source = "var x; { This is a comment } x := 1;";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let tokens = scan(&mut state, source, &mut table)?;
    assert_eq!(
        tokens,
        vec![
            (Token::Var, 1),
            (Token::Ident("x".to_string()), 1),
            (Token::Semicolon, 1),
            (Token::Ident("x".to_string()), 1),
            (Token::Assign, 1),
            (Token::Number(1), 1),
            (Token::Semicolon, 1),
        ]
    );
    Ok(())
}

#[test]
fn test_multi_line_comment() -> Pl0Result<()> {
    let source = "
var x; { This is a
multi-line
comment } x := 1;
";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let tokens = scan(&mut state, source, &mut table)?;
    assert_eq!(
        tokens,
        vec![
            (Token::Var, 2),
            (Token::Ident("x".to_string()), 2),
            (Token::Semicolon, 2),
            (Token::Ident("x".to_string()), 4),
            (Token::Assign, 4),
            (Token::Number(1), 4),
            (Token::Semicolon, 4),
        ]
    );
    Ok(())
}


#[test]
fn test_unterminated_comment() -> Pl0Result<()> {
    let source = "var x; { This is an unterminated comment\nx := 1;";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let result = scan(&mut state, source, &mut table);
    if let Err(Pl0Error::UnterminatedComment { line }) = result {
        assert_eq!(line, 1);
        Ok(())
    } else {
        panic!("Expected an UnterminatedComment error, but got: {:?}", result);
    }
}

#[test]
fn test_complex_operators() -> Pl0Result<()> {
    let source = "if x <> y then z := x + y else z := x - y; .";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let tokens = scan(&mut state, source, &mut table)?;
    assert_eq!(
        tokens,
        vec![
            (Token::If, 1),
            (Token::Ident("x".to_string()), 1),
            (Token::Hash, 1),
            (Token::Ident("y".to_string()), 1),
            (Token::Then, 1),
            (Token::Ident("z".to_string()), 1),
            (Token::Assign, 1),
            (Token::Ident("x".to_string()), 1),
            (Token::Plus, 1),
            (Token::Ident("y".to_string()), 1),
            (Token::Else, 1),
            (Token::Ident("z".to_string()), 1),
            (Token::Assign, 1),
            (Token::Ident("x".to_string()), 1),
            (Token::Minus, 1),
            (Token::Ident("y".to_string()), 1),
            (Token::Semicolon, 1),
            (Token::Dot, 1),
        ]
    );
    Ok(())
}

#[test]
fn test_relational_operators() -> Pl0Result<()> {
    let source = "if a > b OR a < b OR a >= b OR a <= b then a := 10;.";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let tokens = scan(&mut state, source, &mut table)?;
    assert_eq!(
        tokens,
        vec![
            (Token::If, 1),
            (Token::Ident("a".to_string()), 1),
            (Token::GreaterThan, 1),
            (Token::Ident("b".to_string()), 1),
            (Token::Ident("OR".to_string()), 1),
            (Token::Ident("a".to_string()), 1),
            (Token::LessThan, 1),
            (Token::Ident("b".to_string()), 1),
            (Token::Ident("OR".to_string()), 1),
            (Token::Ident("a".to_string()), 1),
            (Token::GreaterThanEqual, 1),
            (Token::Ident("b".to_string()), 1),
            (Token::Ident("OR".to_string()), 1),
            (Token::Ident("a".to_string()), 1),
            (Token::LessThanEqual, 1),
            (Token::Ident("b".to_string()), 1),
            (Token::Then, 1),
            (Token::Ident("a".to_string()), 1),
            (Token::Assign, 1),
            (Token::Number(10), 1),
            (Token::Semicolon, 1),
            (Token::Dot, 1),
        ]
    );
    Ok(())
}

#[test]
fn test_full_program() -> Pl0Result<()> {
    let source = "
    var x, y;
    begin
        read(x);
        if x > 0 then
            y := 1;
        else
            y := 0;
        write(y);
    end.
    ";
    let mut state = LineNumber::default();
    let mut table = SymbolTable::new();
    let tokens = scan(&mut state, source, &mut table)?;
    assert_eq!(
        tokens,
        vec![
            (Token::Var, 2),
            (Token::Ident("x".to_string()), 2),
            (Token::Comma, 2),
            (Token::Ident("y".to_string()), 2),
            (Token::Semicolon, 2),
            (Token::Begin, 3),
            (Token::Read, 4),
            (Token::LParen, 4),
            (Token::Ident("x".to_string()), 4),
            (Token::RParen, 4),
            (Token::Semicolon, 4),
            (Token::If, 5),
            (Token::Ident("x".to_string()), 5),
            (Token::GreaterThan, 5),
            (Token::Number(0), 5),
            (Token::Then, 5),
            (Token::Ident("y".to_string()), 6),
            (Token::Assign, 6),
            (Token::Number(1), 6),
            (Token::Semicolon, 6),
            (Token::Else, 7),
            (Token::Ident("y".to_string()), 8),
            (Token::Assign, 8),
            (Token::Number(0), 8),
            (Token::Semicolon, 8),
            (Token::Write, 9),
            (Token::LParen, 9),
            (Token::Ident("y".to_string()), 9),
            (Token::RParen, 9),
            (Token::Semicolon, 9),
            (Token::End, 10),
            (Token::Dot, 10),
        ]
    );
    Ok(())
}

#[test]
fn test_multiple_variables() {
    // This test ensures the lexer correctly tokenizes a line with multiple variable
    // declarations separated by commas, without corrupting the identifiers.
    let source = "
    var a, b;
    .
    ";

    let mut state = LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let result = scan(&mut state, source, &mut symbol_table).unwrap();

    let expected = vec![
        (Token::Var, 2),
        (Token::Ident("a".to_string()), 2),
        (Token::Comma, 2),
        (Token::Ident("b".to_string()), 2),
        (Token::Semicolon, 2),
        (Token::Dot, 3),
    ];

    assert_eq!(result, expected);
}

#[test]
fn test_not_operator() {
    
    let source = "while x <> 100 do.";

    let mut state = LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let result = scan(&mut state, source, &mut symbol_table).unwrap();

    let expected = vec![
        (Token::While, 1),
        (Token::Ident("x".to_string()), 1),
        (Token::Hash, 1),
        (Token::Number(100), 1),
        (Token::Do, 1),
        (Token::Dot, 1),
    ];

    assert_eq!(result, expected);
}