use pl0c::errors::{Pl0Error, Pl0Result};
use pl0c::lexer::scan;
use pl0c::symboltable::SymbolTable;
use pl0c::token::Token;
use pl0c::LineNumber;
use pl0c::parser::Parser;
use pl0c::ast::Node;
use pl0c::symboltable::SymbolType;
use pl0c::block::Block;
use pl0c::statement::BeginStmt;
use pl0c::io;
use pl0c::types::Number;
use pl0c::types::Ident;
use pl0c::statement::AssignStmt;
use pl0c::expression::BinOp;
use pl0c::expression::OddCondition;
use pl0c::statement::IfStmt;
use pl0c::program::Program;
use pl0c::statement::CallStmt;
use crate::io::Write;
use pl0c::statement::WhileStatement;
use crate::io::Read;
use pl0c::io::WriteStr;
use pl0c::ast::Exit;

#[test]
fn test_complex_symbols() -> Pl0Result<()> {
    let source = "begin const var = 1; var := 2; var < > = + - * end.";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::Begin, 1),
        (Token::Const, 1),
        (Token::Var, 1),
        (Token::Equal, 1),
        (Token::Number(1), 1),
        (Token::Semicolon, 1),
        (Token::Var, 1),
        (Token::Assign, 1),
        (Token::Number(2), 1),
        (Token::Semicolon, 1),
        (Token::Var, 1),
        (Token::LessThan, 1),
        (Token::GreaterThan, 1),
        (Token::Equal, 1),
        (Token::Plus, 1),
        (Token::Minus, 1),
        (Token::Multiply, 1),
        (Token::End, 1),
        (Token::Dot, 1),
    ];
    assert_eq!(tokens, expected);
    Ok(())
}

#[test]
fn test_comments() -> Pl0Result<()> {
    let source = "{ This is a comment }\nvar x = 1;\n{ Another comment }\nbegin x := 1 end.";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::Var, 2),
        (Token::Ident("x".to_string()), 2),
        (Token::Equal, 2),
        (Token::Number(1), 2),
        (Token::Semicolon, 2),
        (Token::Begin, 4),
        (Token::Ident("x".to_string()), 4),
        (Token::Assign, 4),
        (Token::Number(1), 4),
        (Token::End, 4),
        (Token::Dot, 4),
    ];
    assert_eq!(tokens, expected);
    Ok(())
}

#[test]
fn test_strings_and_io() -> Pl0Result<()> {
    let source = r#"
        writestr("Hello, World!");
        read(x);
        write(y);
        "#;
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::WriteStr, 2),
        (Token::LParen, 2),
        (Token::StringLiteral("Hello, World!".to_string()), 2),
        (Token::RParen, 2),
        (Token::Semicolon, 2),
        (Token::Read, 3),
        (Token::LParen, 3),
        (Token::Ident("x".to_string()), 3),
        (Token::RParen, 3),
        (Token::Semicolon, 3),
        (Token::Write, 4),
        (Token::LParen, 4),
        (Token::Ident("y".to_string()), 4),
        (Token::RParen, 4),
        (Token::Semicolon, 4),
    ];
    assert_eq!(tokens, expected);
    Ok(())
}

#[test]
fn test_mixed_case_keywords() -> Pl0Result<()> {
    let source = "vAr x = 1; BEgIN x := 1 enD.";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::Ident("vAr".to_string()), 1),
        (Token::Ident("x".to_string()), 1),
        (Token::Equal, 1),
        (Token::Number(1), 1),
        (Token::Semicolon, 1),
        (Token::Ident("BEgIN".to_string()), 1),
        (Token::Ident("x".to_string()), 1),
        (Token::Assign, 1),
        (Token::Number(1), 1),
        (Token::Ident("enD".to_string()), 1),
        (Token::Dot, 1),
    ];
    assert_eq!(tokens, expected);
    Ok(())
}

#[test]
fn test_missing_semicolon_after_var_decl() -> Pl0Result<()> {
    let source = "var x begin x:= 1 end.";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::Var, 1),
        (Token::Ident("x".to_string()), 1),
        (Token::Begin, 1),
        (Token::Ident("x".to_string()), 1),
        (Token::Assign, 1),
        (Token::Number(1), 1),
        (Token::End, 1),
        (Token::Dot, 1),
    ];
    assert_eq!(tokens, expected);
    Ok(())
}

#[test]
fn test_invalid_chars() -> Pl0Result<()> {
    let source = "var @;";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let result = scan(&mut state, source, &mut table);
    assert!(result.is_err());
    Ok(())
}

#[test]
fn test_relational_operators() -> Pl0Result<()> {
    let source = "var a, b; a:=1; b:=2; if a >= b then a := 3 end. while a <= b do a:=a-1.";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::Var, 1),
        (Token::Ident("a".to_string()), 1),
        (Token::Comma, 1),
        (Token::Ident("b".to_string()), 1),
        (Token::Semicolon, 1),
        (Token::Ident("a".to_string()), 1),
        (Token::Assign, 1),
        (Token::Number(1), 1),
        (Token::Semicolon, 1),
        (Token::Ident("b".to_string()), 1),
        (Token::Assign, 1),
        (Token::Number(2), 1),
        (Token::Semicolon, 1),
        (Token::If, 1),
        (Token::Ident("a".to_string()), 1),
        (Token::GreaterThanEqual, 1),
        (Token::Ident("b".to_string()), 1),
        (Token::Then, 1),
        (Token::Ident("a".to_string()), 1),
        (Token::Assign, 1),
        (Token::Number(3), 1),
        (Token::End, 1),
        (Token::Dot, 1),
        (Token::While, 1),
        (Token::Ident("a".to_string()), 1),
        (Token::LessThanEqual, 1),
        (Token::Ident("b".to_string()), 1),
        (Token::Do, 1),
        (Token::Ident("a".to_string()), 1),
        (Token::Assign, 1),
        (Token::Ident("a".to_string()), 1),
        (Token::Minus, 1),
        (Token::Number(1), 1),
        (Token::Dot, 1),
    ];
    assert_eq!(tokens, expected);
    Ok(())
}


#[test]
fn test_procedures_and_scoping() -> Pl0Result<()> {
    let source = "
        var x;
        procedure p;
            var y;
            begin
                y := 1;
                x := y;
            end;
        begin
            x := 0;
            call p;
            write(x);
        end.
    ";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::Var, 2),
        (Token::Ident("x".to_string()), 2),
        (Token::Semicolon, 2),
        (Token::Procedure, 3),
        (Token::Ident("p".to_string()), 3),
        (Token::Semicolon, 3),
        (Token::Var, 4),
        (Token::Ident("y".to_string()), 4),
        (Token::Semicolon, 4),
        (Token::Begin, 5),
        (Token::Ident("y".to_string()), 6),
        (Token::Assign, 6),
        (Token::Number(1), 6),
        (Token::Semicolon, 6),
        (Token::Ident("x".to_string()), 7),
        (Token::Assign, 7),
        (Token::Ident("y".to_string()), 7),
        (Token::Semicolon, 7),
        (Token::End, 8),
        (Token::Semicolon, 8),
        (Token::Begin, 9),
        (Token::Ident("x".to_string()), 10),
        (Token::Assign, 10),
        (Token::Number(0), 10),
        (Token::Semicolon, 10),
        (Token::Call, 11),
        (Token::Ident("p".to_string()), 11),
        (Token::Semicolon, 11),
        (Token::Write, 12),
        (Token::LParen, 12),
        (Token::Ident("x".to_string()), 12),
        (Token::RParen, 12),
        (Token::Semicolon, 12),
        (Token::End, 13),
        (Token::Dot, 13),
    ];
    assert_eq!(tokens, expected);
    Ok(())
}

#[test]
fn test_comprehensive_program_flow() -> Pl0Result<()> {
    let source = "
        const limit = 10;
        var i, sum;
        begin
            i := 0;
            sum := 0;
            while i < limit do
            begin
                sum := sum + i;
                if sum = 10 then
                    write(sum)
                else
                    write(i);
                i := i + 1;
            end;
        end.
    ";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::Const, 2),
        (Token::Ident("limit".to_string()), 2),
        (Token::Equal, 2),
        (Token::Number(10), 2),
        (Token::Semicolon, 2),
        (Token::Var, 3),
        (Token::Ident("i".to_string()), 3),
        (Token::Comma, 3),
        (Token::Ident("sum".to_string()), 3),
        (Token::Semicolon, 3),
        (Token::Begin, 4),
        (Token::Ident("i".to_string()), 5),
        (Token::Assign, 5),
        (Token::Number(0), 5),
        (Token::Semicolon, 5),
        (Token::Ident("sum".to_string()), 6),
        (Token::Assign, 6),
        (Token::Number(0), 6),
        (Token::Semicolon, 6),
        (Token::While, 7),
        (Token::Ident("i".to_string()), 7),
        (Token::LessThan, 7),
        (Token::Ident("limit".to_string()), 7),
        (Token::Do, 7),
        (Token::Begin, 8),
        (Token::Ident("sum".to_string()), 9),
        (Token::Assign, 9),
        (Token::Ident("sum".to_string()), 9),
        (Token::Plus, 9),
        (Token::Ident("i".to_string()), 9),
        (Token::Semicolon, 9),
        (Token::If, 10),
        (Token::Ident("sum".to_string()), 10),
        (Token::Equal, 10),
        (Token::Number(10), 10),
        (Token::Then, 10),
        (Token::Write, 11),
        (Token::LParen, 11),
        (Token::Ident("sum".to_string()), 11),
        (Token::RParen, 11),
        (Token::Else, 12),
        (Token::Write, 13),
        (Token::LParen, 13),
        (Token::Ident("i".to_string()), 13),
        (Token::RParen, 13),
        (Token::Semicolon, 13),
        (Token::Ident("i".to_string()), 14),
        (Token::Assign, 14),
        (Token::Ident("i".to_string()), 14),
        (Token::Plus, 14),
        (Token::Number(1), 14),
        (Token::Semicolon, 14),
        (Token::End, 15),
        (Token::Semicolon, 15),
        (Token::End, 16),
        (Token::Dot, 16),
    ];
    assert_eq!(tokens, expected);
    Ok(())
}

#[test]
fn test_all_relational_and_arithmetic_operators() -> Pl0Result<()> {
    let source = "
        var a, b, c;
        begin
            a := 5;
            b := 10;
            if a <> b then a := b else a := a + b * c / 2;
        end.
    ";
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let tokens = scan(&mut state, source, &mut table)?;
    let expected = vec![
        (Token::Var, 2), (Token::Ident("a".to_string()), 2), (Token::Comma, 2), (Token::Ident("b".to_string()), 2),
        (Token::Comma, 2), (Token::Ident("c".to_string()), 2), (Token::Semicolon, 2), (Token::Begin, 3),
        (Token::Ident("a".to_string()), 4), (Token::Assign, 4), (Token::Number(5), 4), (Token::Semicolon, 4),
        (Token::Ident("b".to_string()), 5), (Token::Assign, 5), (Token::Number(10), 5), (Token::Semicolon, 5),
        (Token::If, 6), (Token::Ident("a".to_string()), 6), (Token::Hash, 6), (Token::Ident("b".to_string()), 6),
        (Token::Then, 6), (Token::Ident("a".to_string()), 6), (Token::Assign, 6), (Token::Ident("b".to_string()), 6),
        (Token::Else, 6), (Token::Ident("a".to_string()), 6), (Token::Assign, 6), (Token::Ident("a".to_string()), 6),
        (Token::Plus, 6), (Token::Ident("b".to_string()), 6), (Token::Multiply, 6), (Token::Ident("c".to_string()), 6),
        (Token::Divide, 6), (Token::Number(2), 6), (Token::Semicolon, 6),
        (Token::End, 7), (Token::Dot, 7)
    ];
    assert_eq!(tokens, expected);
    Ok(())
}

// This test verifies that the lexer correctly tokenizes while-do loops and
// procedure calls.
#[test]
fn test_while_loop_and_call() {
    let source = "
    const limit = 10;
    var x;
    procedure inc;
        begin
            x := x + 1
        end;
    begin
        x := 0;
        while x < limit do
            call inc;
    end.
    ";

    let mut state = LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let result = scan(&mut state, source, &mut symbol_table).unwrap();

    let expected = vec![
        (Token::Const, 2), (Token::Ident("limit".to_string()), 2), (Token::Equal, 2), (Token::Number(10), 2), (Token::Semicolon, 2),
        (Token::Var, 3), (Token::Ident("x".to_string()), 3), (Token::Semicolon, 3),
        (Token::Procedure, 4), (Token::Ident("inc".to_string()), 4), (Token::Semicolon, 4),
        (Token::Begin, 5),
        (Token::Ident("x".to_string()), 6), (Token::Assign, 6), (Token::Ident("x".to_string()), 6), (Token::Plus, 6), (Token::Number(1), 6),
        (Token::End, 7), (Token::Semicolon, 7),
        (Token::Begin, 8),
        (Token::Ident("x".to_string()), 9), (Token::Assign, 9), (Token::Number(0), 9), (Token::Semicolon, 9),
        (Token::While, 10), (Token::Ident("x".to_string()), 10), (Token::LessThan, 10), (Token::Ident("limit".to_string()), 10), (Token::Do, 10),
        (Token::Call, 11), (Token::Ident("inc".to_string()), 11), (Token::Semicolon, 11),
        (Token::End, 12), (Token::Dot, 12)
    ];

    assert_eq!(result, expected);
}

// This test checks that the lexer correctly ignores both single-line and
// multi-line comments.
#[test]
fn test_comments1() {
    let source = "
    begin
        { This is a single-line comment }
        var a;
        {
        This is a multi-line comment
        that spans several lines.
   }
        a := 1;
    end.
    ";

    let mut state = LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let result = scan(&mut state, source, &mut symbol_table).unwrap();

    let expected = vec![
        (Token::Begin, 2),
        (Token::Var, 4), (Token::Ident("a".to_string()), 4), (Token::Semicolon, 4),
        (Token::Ident("a".to_string()), 9), (Token::Assign, 9), (Token::Number(1), 9), (Token::Semicolon, 9),
        (Token::End, 10), (Token::Dot, 10)
    ];

    assert_eq!(result, expected);
}
#[test]
fn test_nested_procedures() {
    // This test case includes a nested procedure (innerproc) inside an outer one (outerproc).
    // It verifies that the lexer correctly tokenizes all keywords, identifiers, and symbols
    // in a nested block structure using valid PL/0 syntax.
    let source = "
    var a;
    procedure outerproc;
        var b;
        procedure innerproc;
            var c;
        begin
            c := 5;
            b := b + c;
        end;
    begin
        b := 10;
        call innerproc;
        a := b;
    end;
    
    begin
        a := 1;
        call outerproc;
    end.
    ";

    let mut state = LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let result = scan(&mut state, source, &mut symbol_table).unwrap();

    let expected = vec![
        (Token::Var, 2), (Token::Ident("a".to_string()), 2), (Token::Semicolon, 2),
        (Token::Procedure, 3), (Token::Ident("outerproc".to_string()), 3), (Token::Semicolon, 3),
        (Token::Var, 4), (Token::Ident("b".to_string()), 4), (Token::Semicolon, 4),
        (Token::Procedure, 5), (Token::Ident("innerproc".to_string()), 5), (Token::Semicolon, 5),
        (Token::Var, 6), (Token::Ident("c".to_string()), 6), (Token::Semicolon, 6),
        (Token::Begin, 7),
        (Token::Ident("c".to_string()), 8), (Token::Assign, 8), (Token::Number(5), 8), (Token::Semicolon, 8),
        (Token::Ident("b".to_string()), 9), (Token::Assign, 9), (Token::Ident("b".to_string()), 9), (Token::Plus, 9), (Token::Ident("c".to_string()), 9), (Token::Semicolon, 9),
        (Token::End, 10), (Token::Semicolon, 10),
        (Token::Begin, 11),
        (Token::Ident("b".to_string()), 12), (Token::Assign, 12), (Token::Number(10), 12), (Token::Semicolon, 12),
        (Token::Call, 13), (Token::Ident("innerproc".to_string()), 13), (Token::Semicolon, 13),
        (Token::Ident("a".to_string()), 14), (Token::Assign, 14), (Token::Ident("b".to_string()), 14), (Token::Semicolon, 14),
        (Token::End, 15), (Token::Semicolon, 15),
        (Token::Begin, 17),
        (Token::Ident("a".to_string()), 18), (Token::Assign, 18), (Token::Number(1), 18), (Token::Semicolon, 18),
        (Token::Call, 19), (Token::Ident("outerproc".to_string()), 19), (Token::Semicolon, 19),
        (Token::End, 20), (Token::Dot, 20)
    ];

    assert_eq!(result, expected);
}
#[test]
fn test_multiple_constants() {
    // This test verifies that the lexer correctly tokenizes a line with multiple constant
    // declarations separated by commas.
    let source = "{ 0003: multiple constants }
    const one = 1, two = 2;
    var x;
    x := one
    .
    ";

    let mut state = LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let result = scan(&mut state, source, &mut symbol_table).unwrap();

    let expected = vec![
        (Token::Const, 2),
        (Token::Ident("one".to_string()), 2),
        (Token::Equal, 2),
        (Token::Number(1), 2),
        (Token::Comma, 2),
        (Token::Ident("two".to_string()), 2),
        (Token::Equal, 2),
        (Token::Number(2), 2),
        (Token::Semicolon, 2),
        (Token::Var, 3),
        (Token::Ident("x".to_string()), 3),
        (Token::Semicolon, 3),
        (Token::Ident("x".to_string()), 4),
        (Token::Assign, 4),
        (Token::Ident("one".to_string()), 4),
        (Token::Dot, 5),
    ];

    assert_eq!(result, expected);
}

#[test]
fn test_multiple_variables_ast_print() {
    // Create the token stream in the correct `(Token, usize)` format.
    let mut tokens = vec![
        (Token::Var, 1),
        (Token::Ident("a".to_string()), 1),
        (Token::Comma, 1),
        (Token::Ident("b".to_string()), 1),
        (Token::Semicolon, 1),
        (Token::Dot, 1),
    ];

    // Initialize the parser and symbol table.
    let mut symbol_table = SymbolTable::new();
    let mut parser = Parser::new(&mut tokens);

    // Parse the tokens.    
    let ast = parser.parse(&mut symbol_table).unwrap();
    // The test simply checks that an AST was created, then prints it.
    // The actual debugging happens when you inspect the console output.
    assert!(ast.is_some());

    // Call the `print` method on the AST to visualize the tree structure.
    if let Some(ast_node) = ast {
        ast_node.print();
    }
}

#[test]
fn test_fizzbuzz_program() -> Pl0Result<()>{
    // The complete FizzBuzz program to be parsed.
    const SOURCE: &str = r#"
{
  Fizzbuzz
}

var x,i,o;

procedure divthree;
  var div, tmp;
begin
  div := i / 3;
  tmp := div * 3;
  if i = tmp then
    o := o + 1;
end;

procedure divfive;
  var div, tmp;
begin
  div := i / 5;
  tmp := div * 5;
  if i = tmp then
    o := o + 2;
end;

begin
  x := 0;
  while x <> 100 do
  begin
    x := x + 1;
    i := x;
    o := 0;
    call divthree;
    call divfive;
    if o = 1 then
      write (1);
    if o = 2 then
      write (2);
    if o = 3 then
      write (3);
    if o = 0 then
      write(x);
  end;
end.
"#;

    // 1. Lex the source code into a stream of tokens.
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let mut tokens = scan(&mut state, SOURCE, &mut table)?;
    let mut parser = Parser::new(&mut tokens);
    let ast = parser.parse(&mut table).unwrap();
    // The test simply checks that an AST was created, then prints it.
    // The actual debugging happens when you inspect the console output.
    assert!(ast.is_some());
    Ok(())
}

#[test]
fn test_complex_calculator_program() -> Pl0Result<()> {
    // The complete calculator program to be parsed.
    const SOURCE: &str = r#"
        var a, b, operation, result;

        procedure getInputs;
        begin
            read a;
            read b;
            read operation
        end;

        procedure add;
        begin
            result := a + b
        end;

        procedure subtract;
        begin
            result := a - b
        end;

        procedure multiply;
        begin
            result := a * b
        end;

        procedure divide;
        begin
            if b # 0 then
                result := a / b
            else
                result := 0
        end;

        procedure power;
        var temp, counter;
        begin
            result := 1;
            counter := 0;
            temp := b;
            
            while counter < temp do
            begin
                result := result * a;
                counter := counter + 1
            end
        end;

        procedure calculate;
        begin
            if operation = 1 then
                call add
            else
                if operation = 2 then
                    call subtract
                else
                    if operation = 3 then
                        call multiply
                    else
                        if operation = 4 then
                            call divide
                        else
                            if operation = 5 then
                                call power
                            else
                                result := 0
        end;

        procedure printResult;
        begin
            write result
        end;

        begin
            call getInputs;
            call calculate;
            call printResult
        end.
    "#;

    // 1. Lex the source code into a stream of tokens.
    let mut table = SymbolTable::new();
    let mut state = LineNumber::default();
    let mut tokens = scan(&mut state, SOURCE, &mut table)?;
    let mut parser = Parser::new(&mut tokens);
    let ast = parser.parse(&mut table).unwrap();
    if let Some(ast) = &ast {
        ast.print();
    }
    // The test simply checks that an AST was created, then prints it.
    // The actual debugging happens when you inspect the console output.
    assert!(ast.is_some());
    Ok(())
}

#[test]
    fn test_complex_expression_precedence() -> Pl0Result<()> {
        let source = "
            var a, b, c;
            begin
                a := b + c * 2
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");

        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        assert_eq!(block.var_decl.var_decl.len(), 3, "Expected 3 variables");
        assert_eq!(block.var_decl.var_decl, vec!["a", "b", "c"]);
        let stmt = block.statement.as_ref().unwrap().as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        let assign = stmt.stmts[0].as_ref().unwrap().as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(assign.identifier, "a");

        // Verify expression: a := b + (c * 2)
        let expr = assign.expr.as_ref().unwrap().as_any().downcast_ref::<BinOp>().expect("Expected BinOp");
        assert_eq!(expr.operator, "Plus", "Expected + operator");
        let left = expr.left.as_ref().unwrap().as_any().downcast_ref::<Ident>().expect("Expected Ident");
        assert_eq!(left.value, "b");
        let right = expr.right.as_ref().unwrap().as_any().downcast_ref::<BinOp>().expect("Expected nested BinOp");
        assert_eq!(right.operator, "Multiply", "Expected * operator");
        let right_left = right.left.as_ref().unwrap().as_any().downcast_ref::<Ident>().expect("Expected Ident");
        assert_eq!(right_left.value, "c");
        let right_right = right.right.as_ref().unwrap().as_any().downcast_ref::<Number>().expect("Expected Number");
        assert_eq!(right_right.value, 2);

        // Verify SymbolTable
        assert!(table.get("a").is_some(), "Variable a not found");
        assert!(table.get("b").is_some(), "Variable b not found");
        assert!(table.get("c").is_some(), "Variable c not found");
        Ok(())
    }

    #[test]
    fn test_odd_condition() -> Pl0Result<()> {
        let source = "
            var x;
            begin
                if odd x then x := 1 else x := 2
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");

        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        let stmt = block.statement.as_ref().unwrap().as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        let if_stmt = stmt.stmts[0].as_ref().unwrap().as_any().downcast_ref::<IfStmt>().expect("Expected IfStmt");
        let condition = if_stmt.condition.as_ref().unwrap().as_any().downcast_ref::<OddCondition>().expect("Expected OddCondition");
        let expr = condition.expr.as_ref().unwrap().as_any().downcast_ref::<Ident>().expect("Expected Ident");
        assert_eq!(expr.value, "x");

        let true_stmt = if_stmt.then_branch.as_ref().unwrap().as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(true_stmt.identifier, "x");
        assert_eq!(true_stmt.expr.as_ref().unwrap().as_any().downcast_ref::<Number>().unwrap().value, 1);
        let false_stmt = if_stmt.else_branch.as_ref().unwrap().as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(false_stmt.identifier, "x");
        assert_eq!(false_stmt.expr.as_ref().unwrap().as_any().downcast_ref::<Number>().unwrap().value, 2);

        // Verify SymbolTable
        let x_symbol = table.get("x").expect("Variable x not found");
        assert_eq!(x_symbol.symbol_type, SymbolType::Variable);
        assert_eq!(x_symbol.level, 0);
        Ok(())
    }

    #[test]
    fn test_empty_block() -> Pl0Result<()> {
        let source = ".";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");

        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        assert_eq!(block.var_decl.var_decl.len(), 0, "Expected no variables");
        assert_eq!(block.proc_decl.procedurs.len(), 0, "Expected no procedures");
        assert!(block.statement.is_none(), "Expected no statement");
        Ok(())
    }

    #[test]
    fn test_nested_begin_end() -> Pl0Result<()> {
        let source = "
            var x, y;
            begin
                x := 1;
                begin
                    y := x + 2;
                    x := y
                end
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        assert_eq!(block.var_decl.var_decl.len(), 2, "Expected 2 variables");
        assert_eq!(block.var_decl.var_decl, vec!["x", "y"]);
        let outer_begin = block.statement.as_ref().unwrap().as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        assert_eq!(outer_begin.stmts.len(), 2, "Expected 2 statements in outer begin");

        let first_stmt = outer_begin.stmts[0].as_ref().unwrap().as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(first_stmt.identifier, "x");
        assert_eq!(first_stmt.expr.as_ref().unwrap().as_any().downcast_ref::<Number>().unwrap().value, 1);

        let inner_begin = outer_begin.stmts[1].as_ref().unwrap().as_any().downcast_ref::<BeginStmt>().expect("Expected nested BeginStmt");
        assert_eq!(inner_begin.stmts.len(), 2, "Expected 2 statements in inner begin");
        let inner_assign1 = inner_begin.stmts[0].as_ref().unwrap().as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(inner_assign1.identifier, "y");
        let expr = inner_assign1.expr.as_ref().unwrap().as_any().downcast_ref::<BinOp>().expect("Expected BinOp");
        assert_eq!(expr.operator, "Plus");
        let inner_assign2 = inner_begin.stmts[1].as_ref().unwrap().as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(inner_assign2.identifier, "x");
        assert_eq!(inner_assign2.expr.as_ref().unwrap().as_any().downcast_ref::<Ident>().unwrap().value, "y");

        // Verify SymbolTable
        assert!(table.get("x").is_some(), "Variable x not found");
        assert!(table.get("y").is_some(), "Variable y not found");
        Ok(())
    }

    #[test]
    fn test_undefined_variable() {
        let source = "
        begin
            x := 1
        end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table).expect("Scanning should succeed");
        let mut parser = Parser::new(&mut tokens);
        let result = parser.parse(&mut table);
        assert!(result.is_err(), "Expected error for undefined variable x");
        match result {
            Err(Pl0Error::UndefinedSymbol { name, line }) => {
                assert_eq!(name, "x", "Expected undefined variable x");
                assert_eq!(line, 3, "Expected error on line 3");
            }
            Err(other_error) => {
                panic!("Expected UndefinedSymbol error, got: {:?}", other_error);
            }
            Ok(_) => {
                panic!("Expected UndefinedSymbol error, but parsing succeeded");
            }
        }
    }

    #[test]
    fn test_large_number_constant() -> Pl0Result<()> {
        let source = "
            const max = 9999999999;
            var x;
            begin
                x := max
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");

        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.const_decl.const_decl.len(), 1, "Expected 1 constant");
        assert_eq!(block.const_decl.const_decl[0], ("max".to_string(), 9999999999));
        assert_eq!(block.var_decl.var_decl.len(), 1, "Expected 1 variable");
        assert_eq!(block.var_decl.var_decl[0], "x");

        let stmt = block.statement.as_ref().unwrap().as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        let assign = stmt.stmts[0].as_ref().unwrap().as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(assign.identifier, "x");
        assert_eq!(assign.expr.as_ref().unwrap().as_any().downcast_ref::<Ident>().unwrap().value, "max");

        // Verify SymbolTable
        let max_symbol = table.get("max").expect("Constant max not found");
        assert_eq!(max_symbol.symbol_type, SymbolType::Constant(9999999999));
        assert_eq!(max_symbol.level, 0);
        let x_symbol = table.get("x").expect("Variable x not found");
        assert_eq!(x_symbol.symbol_type, SymbolType::Variable);
        assert_eq!(x_symbol.level, 0);
        Ok(())
    }

    #[test]
    fn test_minimal_program() -> Pl0Result<()> {
        let source = "
            begin
                write(0)
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");

        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        assert_eq!(block.var_decl.var_decl.len(), 0, "Expected no variables");
        assert_eq!(block.proc_decl.procedurs.len(), 0, "Expected no procedures");
        let stmt = block.statement.as_ref().unwrap().as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        let write = stmt.stmts[0].as_ref().unwrap().as_any().downcast_ref::<crate::io::Write>().expect("Expected Write");
        assert_eq!(write.expr.as_ref().unwrap().as_any().downcast_ref::<Number>().unwrap().value, 0);
        Ok(())
    }
    
    #[test]
    fn test_undefined_procedure() {
        let source = "
        begin
            call nonexistent
        end.
        ";
        
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table).expect("Scanning should succeed");
        let mut parser = Parser::new(&mut tokens);
        let result = parser.parse(&mut table);
        
        assert!(result.is_err(), "Expected error for undefined procedure");
        
        match result {
            Err(Pl0Error::UndefinedSymbol { name, .. }) => {
                assert_eq!(name, "nonexistent", "Expected undefined procedure nonexistent");
            }
            Err(other_error) => {
                panic!("Expected UndefinedSymbol error, got: {:?}", other_error);
            }
            Ok(_) => {
                panic!("Expected error but parsing succeeded");
            }
        }
    }

    #[test]
    fn test_constant_assignment() -> Pl0Result<()> {
        let source = "
        const c = 10;
        begin
            c := 5
        end.
        ";
        
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table).expect("Scanning should succeed");
        let mut parser = Parser::new(&mut tokens);
        let result = parser.parse(&mut table);
        
        assert!(result.is_err(), "Expected error for assigning to constant");
        
        match result {
            Err(Pl0Error::AssignmentToConstant {name, line }) => {
                assert_eq!(name, "c", "Expected assignment to constant c");
            }
            Err(other_error) => {
                panic!("Expected AssignmentToConstant error, got: {:?}", other_error);
            }
            Ok(_) => {
                panic!("Expected error but parsing succeeded");
            }
        }
        Ok(())
    }

    #[test]
    fn test_duplicate_declaration() -> Pl0Result<()> {
        let source = "
        var x;
        var x;
        begin
            x := 1
        end.
        ";
        // currently the lexer does not catch duplicate declarations, the parser does
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table).expect("Scanning should succeed");
        let mut parser = Parser::new(&mut tokens);
        let result = parser.parse(&mut table);
        
        assert!(result.is_err(), "Expected error for duplicate declaration");
        
        match result {
            Err(Pl0Error::SymbolAlreadyDefined {name, line }) => {
                assert_eq!(name, "x", "Expected duplicate symbol x");
            }
            Err(other_error) => {
                panic!("Expected DuplicateSymbol error, got: {:?}", other_error);
            }
            Ok(_) => {
                panic!("Expected error but parsing succeeded");
            }
        }
        Ok(())
    }
    
    #[test]
    fn test_duplicate_constant_declaration() -> Pl0Result<()> {
        let source = "
            const c = 10;
            const c = 20;
            begin
                c := 5
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let result = parser.parse(&mut table);
        
        assert!(result.is_err(), "Expected error for duplicate constant declaration");
        
        match result {
            Err(Pl0Error::SymbolAlreadyDefined { name, line }) => {
                assert_eq!(name, "c", "Expected duplicate constant c");
                assert_eq!(line, 3, "Expected error on line 3");
                Ok(())
            }
            Err(_) => {
                panic!("Expected SymbolAlreadyDefined error, got a different error");
            }
            Ok(_) => {
                panic!("Expected error but parsing succeeded");
            }
        }
    }

    #[test]
    fn test_procedure_declaration_and_call() -> Pl0Result<()> {
        let source = "
            procedure p;
            begin
                write 42
            end;
            begin
                call p
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.proc_decl.procedurs.len(), 1, "Expected one procedure in block");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        assert_eq!(block.var_decl.var_decl.len(), 0, "Expected no variables");
        
        let stmt = block.statement.as_ref().expect("Expected statement in block").as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        let call = stmt.stmts[0].as_ref().expect("Expected statement in BeginStmt").as_any().downcast_ref::<CallStmt>().expect("Expected CallStmt");
        assert_eq!(call.identifier, "p", "Expected call to procedure p");
        
        // Verify SymbolTable
        let p_symbol = table.get("p").expect("Procedure p not found");
        assert_eq!(p_symbol.symbol_type, SymbolType::Procedure, "Expected procedure symbol");
        assert_eq!(p_symbol.level, 0, "Expected procedure at level 0");
        
        Ok(())
    }

    #[test]
    fn test_if_statement_with_condition() -> Pl0Result<()> {
        let source = "
            var x;
            begin
                x := 5;
                if x = 5 then
                    write x
                else
                    write 0
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.var_decl.var_decl.len(), 1, "Expected one variable");
        assert_eq!(block.var_decl.var_decl[0], "x");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        
        let stmt = block.statement.as_ref().expect("Expected statement in block").as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        assert_eq!(stmt.stmts.len(), 2, "Expected two statements in BeginStmt");
        
        let assign = stmt.stmts[0].as_ref().expect("Expected first statement").as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(assign.identifier, "x", "Expected assignment to x");
        assert_eq!(assign.expr.as_ref().expect("Expected expression").as_any().downcast_ref::<Number>().expect("Expected Number").value, 5);
        
        let if_stmt = stmt.stmts[1].as_ref().expect("Expected second statement").as_any().downcast_ref::<IfStmt>().expect("Expected IfStmt");
        let write_then = if_stmt.then_branch.as_ref().expect("Expected then statement").as_any().downcast_ref::<Write>().expect("Expected Write in then");
        assert_eq!(write_then.expr.as_ref().expect("Expected expression").as_any().downcast_ref::<Ident>().expect("Expected Ident").value, "x");
        
        let write_else = if_stmt.else_branch.as_ref().expect("Expected else statement").as_any().downcast_ref::<Write>().expect("Expected Write in else");
        assert_eq!(write_else.expr.as_ref().expect("Expected expression").as_any().downcast_ref::<Number>().expect("Expected Number").value, 0);
        
        // Verify SymbolTable
        let x_symbol = table.get("x").expect("Variable x not found");
        assert_eq!(x_symbol.symbol_type, SymbolType::Variable, "Expected variable symbol");
        assert_eq!(x_symbol.level, 0, "Expected variable at level 0");
        
        Ok(())
    }

    #[test]
    fn test_while_statement_with_odd_condition() -> Pl0Result<()> {
        let source = "
            var x;
            begin
                x := 1;
                while odd x do
                    x := x + 2
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.var_decl.var_decl.len(), 1, "Expected one variable");
        assert_eq!(block.var_decl.var_decl[0], "x");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        
        let stmt = block.statement.as_ref().expect("Expected statement in block").as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        assert_eq!(stmt.stmts.len(), 2, "Expected two statements in BeginStmt");
        
        let assign = stmt.stmts[0].as_ref().expect("Expected first statement").as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(assign.identifier, "x", "Expected assignment to x");
        assert_eq!(assign.expr.as_ref().expect("Expected expression").as_any().downcast_ref::<Number>().expect("Expected Number").value, 1);
        
        let while_stmt = stmt.stmts[1].as_ref().expect("Expected second statement").as_any().downcast_ref::<WhileStatement>().expect("Expected WhileStatement");
        let while_body = while_stmt.body.as_ref().expect("Expected while body").as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt in while");
        assert_eq!(while_body.identifier, "x", "Expected assignment to x in while");
        
        // Verify SymbolTable
        let x_symbol = table.get("x").expect("Variable x not found");
        assert_eq!(x_symbol.symbol_type, SymbolType::Variable, "Expected variable symbol");
        assert_eq!(x_symbol.level, 0, "Expected variable at level 0");
        
        Ok(())
    }

    #[test]
    fn test_read_and_write_statements() -> Pl0Result<()> {
        let source = "
            var x;
            begin
                read x;
                write x
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.var_decl.var_decl.len(), 1, "Expected one variable");
        assert_eq!(block.var_decl.var_decl[0], "x");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        
        let stmt = block.statement.as_ref().expect("Expected statement in block").as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        assert_eq!(stmt.stmts.len(), 2, "Expected two statements in BeginStmt");
        
        let read = stmt.stmts[0].as_ref().expect("Expected first statement").as_any().downcast_ref::<Read>().expect("Expected Read");
        assert_eq!(read.identifier, "x", "Expected read to x");
        
        let write = stmt.stmts[1].as_ref().expect("Expected second statement").as_any().downcast_ref::<Write>().expect("Expected Write");
        assert_eq!(write.expr.as_ref().expect("Expected expression").as_any().downcast_ref::<Ident>().expect("Expected Ident").value, "x");
        
        // Verify SymbolTable
        let x_symbol = table.get("x").expect("Variable x not found");
        assert_eq!(x_symbol.symbol_type, SymbolType::Variable, "Expected variable symbol");
        assert_eq!(x_symbol.level, 0, "Expected variable at level 0");
        
        Ok(())
    }

    #[test]
    fn test_write_str_with_string_literal() -> Pl0Result<()> {
        let source = "
            begin
                writeStr(\"Hello, World!\")
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.var_decl.var_decl.len(), 0, "Expected no variables");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        
        let stmt = block.statement.as_ref().expect("Expected statement in block").as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        assert_eq!(stmt.stmts.len(), 1, "Expected one statement in BeginStmt");
        
        Ok(())
    }

    #[test]
    fn test_write_str_with_variable() -> Pl0Result<()> {
        let source = "
            var msg;
            begin
                writeStr(msg)
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.var_decl.var_decl.len(), 1, "Expected one variable");
        assert_eq!(block.var_decl.var_decl[0], "msg");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        
        let stmt = block.statement.as_ref().expect("Expected statement in block").as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        assert_eq!(stmt.stmts.len(), 1, "Expected one statement in BeginStmt");
        // Verify SymbolTable
        let msg_symbol = table.get("msg").expect("Variable msg not found");
        assert_eq!(msg_symbol.symbol_type, SymbolType::Variable, "Expected variable symbol");
        assert_eq!(msg_symbol.level, 0, "Expected variable at level 0");
        
        Ok(())
    }

    #[test]
    fn test_exit_statement() -> Pl0Result<()> {
        let source = "
            begin
                exit 0
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.var_decl.var_decl.len(), 0, "Expected no variables");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        
        let stmt = block.statement.as_ref().expect("Expected statement in block").as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        assert_eq!(stmt.stmts.len(), 1, "Expected one statement in BeginStmt");
        
        let exit = stmt.stmts[0].as_ref().expect("Expected statement").as_any().downcast_ref::<Exit>().expect("Expected Exit");
        assert_eq!(exit.expr.as_ref().expect("Expected expression").as_any().downcast_ref::<Number>().expect("Expected Number").value, 0);
        
        Ok(())
    }

    #[test]
    fn test_complex_expression() -> Pl0Result<()> {
        let source = "
            var x;
            begin
                x := -5 + (3 * 2) mod 4
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        assert!(ast.is_some(), "Expected AST, found None");
        
        let binding = ast.unwrap();
        let program = binding.as_any().downcast_ref::<Program>().expect("Expected Program node");
        let block = program.block.as_ref().expect("Expected Block in Program").as_any().downcast_ref::<Block>().expect("Expected Block node");
        
        assert_eq!(block.var_decl.var_decl.len(), 1, "Expected one variable");
        assert_eq!(block.var_decl.var_decl[0], "x");
        assert_eq!(block.const_decl.const_decl.len(), 0, "Expected no constants");
        
        let stmt = block.statement.as_ref().expect("Expected statement in block").as_any().downcast_ref::<BeginStmt>().expect("Expected BeginStmt");
        assert_eq!(stmt.stmts.len(), 1, "Expected one statement in BeginStmt");
        
        let assign = stmt.stmts[0].as_ref().expect("Expected statement").as_any().downcast_ref::<AssignStmt>().expect("Expected AssignStmt");
        assert_eq!(assign.identifier, "x", "Expected assignment to x");
        
        let bin_op = assign.expr.as_ref().expect("Expected expression").as_any().downcast_ref::<BinOp>().expect("Expected BinOp");
        assert_eq!(bin_op.operator, "Plus", "Expected plus operator");
        
        // Verify SymbolTable
        let x_symbol = table.get("x").expect("Variable x not found");
        assert_eq!(x_symbol.symbol_type, SymbolType::Variable, "Expected variable symbol");
        assert_eq!(x_symbol.level, 0, "Expected variable at level 0");
        
        Ok(())
    }

    #[test]
    fn test_missing_semicolon_error() -> Pl0Result<()> {
        let source = "
            var x
            begin
                x := 1
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let result = parser.parse(&mut table);
        
        assert!(result.is_err(), "Expected error for missing semicolon after var declaration");
        
        match result {
            Err(Pl0Error::SyntaxError { expected, found, line }) => {
                assert_eq!(expected, "Semicolon", "Expected missing semicolon error");
                assert_eq!(found, "Begin", "Expected 'begin' as found token");
                assert_eq!(line, 3, "Expected error on line 3");
                Ok(())
            }
            Err(_) => {
                panic!("Expected SyntaxError for missing semicolon, got a different error");
            }
            Ok(_) => {
                panic!("Expected error but parsing succeeded");
            }
        }
    }

    #[test]
    fn test_invalid_condition_error() -> Pl0Result<()> {
        let source = "
            var x;
            begin
                if x + 5 then
                    x := 1
            end.
        ";
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let result = parser.parse(&mut table);
        
        assert!(result.is_err(), "Expected error for invalid condition operator");
        
        match result {
            Err(Pl0Error::GenericError(msg)) => {
                assert!(msg.contains("Invalid conditional operator"), "Expected invalid condition error");
                assert!(msg.contains("line 4"), "Expected error on line 4");
                Ok(())
            }
            Err(_) => {
                panic!("Expected GenericError for invalid condition, got a different error");
            }
            Ok(_) => {
                panic!("Expected error but parsing succeeded");
            }
        }
    }