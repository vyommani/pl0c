use crate::ast::Exit;
use crate::ast::{ExpressionNode, Node};
use crate::ast::Block;
use crate::ast::ConstDecl;
use crate::ast::ProcDecl;
use crate::ast::VarDecl;
use crate::ast::BinOp;
use crate::ast::OddCondition;
use crate::ast::RelationalCondition;
use crate::ast::Read;
use crate::ast::Write;
use crate::ast::WriteStr;
use crate::ast::Program;
use crate::ast::AssignStmt;
use crate::ast::BeginStmt;
use crate::ast::CallStmt;
use crate::ast::IfStmt;
use crate::ast::WhileStatement;
use crate::semantic::symboltable::Symbol;
use crate::semantic::symboltable::SymbolLocation;
use crate::semantic::symboltable::SymbolTable;
use crate::semantic::symboltable::SymbolType;
use crate::frontend::token::Token;
use crate::ast::Ident;
use crate::ast::Number;
use std::collections::HashMap;
use std::slice::Iter;
use crate::utils::errors::Pl0Error;

use crate::utils::config::parser::rename_identifier;
use crate::utils::config::parser::get_renamed_identifier;
use crate::utils::config::parser::INITIAL_STACK_OFFSET;

const STACK_SLOT_SIZE: usize = 8;

pub struct Parser<'a> {
    current_token: Token,
    line_number: usize,
    iter: Iter<'a, (Token, usize)>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a mut Vec<(Token, usize)>) -> Self {
        let mut parser = Self {
            current_token: Token::Null,
            line_number: 1,
            iter: tokens.iter(),
        };
        parser.next();
        parser
    }

    fn next(&mut self) {
        match self.iter.next() {
            Some((element, line)) => {
                self.current_token = element.clone();
                self.line_number = *line;
            }
            None => {}
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), Pl0Error> {
        if std::mem::discriminant(&expected) != std::mem::discriminant(&self.current_token) {
            return Err(Pl0Error::SyntaxError {
                expected: expected.to_string(),
                found: self.current_token.to_string(),
                line: self.line_number,
            });
        }
        self.next();
        Ok(())
    }

    fn get_identifier(&self, token: &Token) -> Result<String, Pl0Error> {
        match token {
            Token::Ident(id) => Ok(id.clone()),
            _ => Err(Pl0Error::GenericError(format!(
                "Not able to extract the identifier from token: {:?} at line {}",
                token, self.line_number
            ))),
        }
    }

    fn get_string_literal(&self, token: &Token) -> Result<String, Pl0Error> {
        match token {
            Token::StringLiteral(literal) => Ok(literal.clone()),
            _ => Err(Pl0Error::GenericError(format!(
                "Not able to extract the string literal from token: {:?} at line {}",
                token, self.line_number
            ))),
        }
    }

    fn get_numeric_literal(&self, token: &Token) -> Result<i64, Pl0Error> {
        match token {
            Token::Number(n) => Ok(*n),
            _ => Err(Pl0Error::GenericError(format!(
                "Not able to extract the numeric literal from token: {:?} at line {}",
                token, self.line_number
            ))),
        }
    }
    
    fn expect_ident(&mut self) -> Result<(), Pl0Error> {
        if !matches!(self.current_token, Token::Ident(_)) {
            return Err(Pl0Error::SyntaxError {
                expected: "identifier".to_string(),
                found: self.current_token.to_string(),
                line: self.line_number,
            });
        }
        self.next();
        Ok(())
    }

    fn expect_number(&mut self) -> Result<(), Pl0Error> {
        if !matches!(self.current_token, Token::Number(_)) {
            return Err(Pl0Error::SyntaxError {
                expected: "number".to_string(),
                found: self.current_token.to_string(),
                line: self.line_number,
            });
        }
        self.next();
        Ok(())
    }
    
    fn parse_const_declarations(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<ConstDecl, Pl0Error> {
        let mut consts = Vec::<(String, i64)>::new();
        while self.current_token == Token::Const {
            self.expect(Token::Const)?;
            let mut id = self.get_identifier(&self.current_token)?;
            id = rename_identifier(&id, true, mapped_identifiers);
            self.expect_ident()?;
            self.expect(Token::Equal)?;
            let num = self.get_numeric_literal(&self.current_token)?;
            table.insert(&id, Symbol::new(SymbolType::Constant(num), self.line_number, SymbolLocation::Immediate(num), true, table.get_scopes_len() - 1))?;
            self.expect_number()?;
            consts.push((id, num));
            while self.current_token == Token::Comma {
                self.expect(Token::Comma)?;
                let mut id = self.get_identifier(&self.current_token)?;
                id = rename_identifier(&id, true, mapped_identifiers);
                self.expect_ident()?;
                self.expect(Token::Equal)?;
                let num = self.get_numeric_literal(&self.current_token)?;
                table.insert(&id, Symbol::new(SymbolType::Constant(num), self.line_number, SymbolLocation::Immediate(num), true, table.get_scopes_len() - 1))?;
                self.expect_number()?;
                consts.push((id, num));
            }
            self.expect(Token::Semicolon)?;
        }
        Ok(ConstDecl::new(consts))
    }

    fn parse_var_declarations(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<VarDecl, Pl0Error> {
        let mut idents = Vec::<String>::new();
        while self.current_token == Token::Var {
            self.expect(Token::Var)?;
            let is_global = table.get_scopes_len() == 1;
            let mut offset = INITIAL_STACK_OFFSET;
            loop {
                let mut id = self.get_identifier(&self.current_token)?;
                id = rename_identifier(&id, is_global, mapped_identifiers);
                let location = if is_global {
                    SymbolLocation::GlobalLabel(id.clone())
                } else {
                    SymbolLocation::StackOffset(offset.try_into().unwrap())
                };
                table.insert(&id, Symbol::new(SymbolType::Variable, self.line_number, location, is_global, table.get_scopes_len() - 1))?;
                self.expect_ident()?;
                idents.push(id);
                if !is_global {
                    offset += STACK_SLOT_SIZE;
                }
                if self.current_token != Token::Comma {
                    break;
                }
                self.expect(Token::Comma)?;
            }
            self.expect(Token::Semicolon)?;
        }
        Ok(VarDecl::new(idents))
    }

    /// Helper function to parse procedure declarations
    fn parse_procedure_declarations(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<Vec<(String, Option<Box<dyn Node>>)>, Pl0Error> {
        let mut procedures = Vec::new();
        while self.current_token == Token::Procedure {
            // Always insert procedure name in global (top) scope
            self.expect(Token::Procedure)?;
            let mut name = self.get_identifier(&self.current_token)?;
            name = rename_identifier(&name, true, mapped_identifiers);
            table.insert(&name, Symbol::new(SymbolType::Procedure, self.line_number, SymbolLocation::GlobalLabel(name.clone()), true, table.get_scopes_len() - 1))?;
            table.push_scope();
            self.expect_ident()?;
            self.expect(Token::Semicolon)?;
            let block = self.block(table, mapped_identifiers)?;
            self.expect(Token::Semicolon)?;
            procedures.push((name, block));
        }
        Ok(procedures)
    }

    /**
     * Parse a block according to the grammar:
     * block = [ "const" ident "=" number { "," ident "=" number } ";" ]
     *         ["var" ident {"," ident} ";"]
     *         { "procedure" ident ";" block ";" } statement .
     */
    fn block(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<Option<Box<dyn Node>>, Pl0Error> {
        // Parse constant declarations if present
        let const_decl = if self.current_token == Token::Const {
            self.parse_const_declarations(table, mapped_identifiers)?
        } else {
            ConstDecl::default()
        };

        // Parse variable declarations if present
        let var_decl = if self.current_token == Token::Var {
            self.parse_var_declarations(table, mapped_identifiers)?
        } else {
            VarDecl::default()
        };

        // Parse procedure declarations
        let procedures = self.parse_procedure_declarations(table, mapped_identifiers)?;
        let proc_decl = ProcDecl::new(procedures);

        // Parse statement
        let stmt = self.statement(table, mapped_identifiers)?;
        Ok(Some(Box::new(Block::new(const_decl, var_decl, proc_decl, stmt))))
    }

    /**
     * Parse a statement according to the grammar:
     * statement = [ ident ":=" expression
     *             | "call" ident
     *             | "begin" statement { ";" statement } "end"
     *             | "if" condition "then" statement [ "else" statement ]
     *             | "while" condition "do" statement
     *             | "read" ident
     *             | "write" expression
     *             | "writeStr" ( ident | string )
     *             | "exit" expression ]  .
     */
    fn statement(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<Option<Box<dyn Node>>, Pl0Error> {
        match &self.current_token {
            Token::Ident(_) => {
                let mut id = self.get_identifier(&self.current_token)?;
                id = get_renamed_identifier(&id, mapped_identifiers);
                let symbol = table.get(&id).ok_or_else(|| Pl0Error::UndefinedSymbol {
                    name: id.clone(),
                    line: self.line_number,
                })?;
                if matches!(symbol.symbol_type, SymbolType::Constant(_)) {
                    return Err(Pl0Error::AssignmentToConstant {
                        name: id.clone(),
                        line: self.line_number,
                    });
                }
                table.type_check(&id, &SymbolType::Variable, self.line_number)?;
                self.expect_ident()?;
                self.expect(Token::Assign)?;
                let expr = self.expression(table, mapped_identifiers)?;
                Ok(Some(Box::new(AssignStmt::new(id, expr))))
            }
            Token::Call => {
                self.expect(Token::Call)?;
                let mut id = self.get_identifier(&self.current_token)?;
                id = get_renamed_identifier(&id, mapped_identifiers);
                table.type_check(&id, &SymbolType::Procedure, self.line_number)?;
                self.expect_ident()?;
                Ok(Some(Box::new(CallStmt::new(id))))
            }
            Token::Begin => {
                self.expect(Token::Begin)?;
                let mut stmts = Vec::new();
                stmts.push(self.statement(table, mapped_identifiers)?);

                while self.current_token == Token::Semicolon {
                    self.expect(Token::Semicolon)?;
                    stmts.push(self.statement(table, mapped_identifiers)?);
                }

                self.expect(Token::End)?;
                Ok(Some(Box::new(BeginStmt::new(stmts))))
            }
            Token::If => {
                self.expect(Token::If)?;
                let condition = self.condition(table, mapped_identifiers)?;
                self.expect(Token::Then)?;
                let true_stmt = self.statement(table, mapped_identifiers)?;

                let false_stmt = if self.current_token == Token::Else {
                    self.expect(Token::Else)?;
                    self.statement(table, mapped_identifiers)?
                } else {
                    None
                };

                Ok(Some(Box::new(IfStmt::new(condition, true_stmt, false_stmt))))
            }
            Token::While => {
                self.expect(Token::While)?;
                let condition = self.condition(table, mapped_identifiers)?;
                self.expect(Token::Do)?;
                let body = self.statement(table, mapped_identifiers)?;
                Ok(Some(Box::new(WhileStatement::new(condition, body))))
            }
            Token::Write => {
                self.expect(Token::Write)?;

                // Optional parentheses
                if self.current_token == Token::LParen {
                    self.expect(Token::LParen)?;
                }

                let expr = self.expression(table, mapped_identifiers)?;

                if self.current_token == Token::RParen {
                    self.expect(Token::RParen)?;
                }

                Ok(Some(Box::new(Write::new(expr))))
            }
            Token::WriteStr => {
                self.expect(Token::WriteStr)?;
                self.expect(Token::LParen)?;

                let result = match &self.current_token {
                    Token::Ident(_) => {
                        let mut id = self.get_identifier(&self.current_token)?;
                        id = get_renamed_identifier(&id, mapped_identifiers);
                        table.type_check(&id, &SymbolType::Variable, self.line_number)?;
                        self.expect_ident()?;
                        Box::new(WriteStr::new(id))
                    }
                    Token::StringLiteral(_) => {
                        let string_literal = self.get_string_literal(&self.current_token)?;
                        self.expect(Token::StringLiteral("".to_string()))?;
                        Box::new(WriteStr::new(string_literal))
                    }
                    _ => {
                        return Err(Pl0Error::GenericError(format!(
                            "writeStr takes a string or identifier at line {}",
                            self.line_number
                        )));
                    }
                };

                self.expect(Token::RParen)?;
                Ok(Some(result))
            }
            Token::Read => {
                self.expect(Token::Read)?;

                // Optional parentheses
                if self.current_token == Token::LParen {
                    self.expect(Token::LParen)?;
                }

                let mut ident = self.get_identifier(&self.current_token)?;
                ident = get_renamed_identifier(&ident, mapped_identifiers);
                table.type_check(&ident, &SymbolType::Variable, self.line_number)?;
                self.expect_ident()?;
                if self.current_token == Token::RParen {
                    self.expect(Token::RParen)?;
                }

                Ok(Some(Box::new(Read::new(ident))))
            }
            Token::Exit => {
                self.expect(Token::Exit)?;
                let expr = self.expression(table, mapped_identifiers)?;
                Ok(Some(Box::new(Exit::new(expr))))
            }
            _ => Ok(None),
        }
    }

    /**
     * Parse a condition according to the grammar:
     * condition = "odd" expression
     *           | expression ( comparator ) expression .
     */
    fn condition(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<Option<Box<dyn ExpressionNode>>, Pl0Error> {
        if self.current_token == Token::Odd {
            self.expect(Token::Odd)?;
            let expr = self.expression(table, mapped_identifiers)?;
            Ok(Some(Box::new(OddCondition::new(expr))))
        } else {
            let left = self.expression(table, mapped_identifiers)?;

            let operator = match &self.current_token {
                Token::Equal | Token::Hash | Token::LessThan | Token::GreaterThan
                | Token::LessThanEqual | Token::GreaterThanEqual => {
                    let op = self.current_token.to_string();
                    self.next();
                    op
                }
                _ => {
                    return Err(Pl0Error::GenericError(format!(
                        "Invalid conditional operator at line {}",
                        self.line_number
                    )));
                }
            };

            let right = self.expression(table, mapped_identifiers)?;
            Ok(Some(Box::new(RelationalCondition::new(left, right, operator))))
        }
    }

    /**
     * Parse an expression according to the grammar:
     * expression = [ "+" | "-" | "not" ] term { ( "+" | "-" | "or" ) term }
     */
    fn expression(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<Option<Box<dyn ExpressionNode>>, Pl0Error> {
        let mut lhs = self.term(table, mapped_identifiers)?;
        while matches!(self.current_token, Token::Plus | Token::Minus | Token::Or) {
            let operator = self.current_token.to_string();
            self.next();
            let rhs = self.term(table, mapped_identifiers)?;
            if lhs.is_none() || rhs.is_none() {
                return Err(Pl0Error::SyntaxError {
                    expected: "expression".to_string(),
                    found: "none".to_string(),
                    line: self.line_number,
                });
            }
            lhs = Some(Box::new(BinOp::new(lhs, rhs, operator)));
        }
        Ok(lhs)
    }

    /**
     * Parse a term according to the grammar:
     * term = factor { ( "*" | "/" | "mod" | "and" ) factor }
     */
    fn term(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<Option<Box<dyn ExpressionNode>>, Pl0Error> {
        let mut lhs = self.factor(table, mapped_identifiers)?;
        while matches!(
            self.current_token,
            Token::Multiply | Token::Divide | Token::Modulo | Token::And
        ) {
            let operator = self.current_token.to_string();
            self.next();
            let rhs = self.factor(table, mapped_identifiers)?;
            lhs = Some(Box::new(BinOp::new(lhs, rhs, operator)));
        }
        Ok(lhs)
    }

    /**
     * I enhance it to handle complex factors like unary plus/minus and parentheses.
     * Parse a factor according to the grammar:
     * factor = ident | number | "(" expression ")" | [ "+" | "-" ] factor
     */
    fn factor(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<Option<Box<dyn ExpressionNode>>, Pl0Error> {
        match &self.current_token {
            Token::Ident(_) => {
                let mut id = self.get_identifier(&self.current_token)?;
                id = get_renamed_identifier(&id, mapped_identifiers);
                let symbol = table.get(&id).ok_or_else(|| Pl0Error::UndefinedSymbol {
                    name: id.clone(),
                    line: self.line_number,
                })?;
                if !matches!(symbol.symbol_type, SymbolType::Variable | SymbolType::Constant(_)) {
                    return Err(Pl0Error::TypeMismatch {
                        expected: "Variable or Constant".to_string(),
                        found: format!("{:?}", symbol.symbol_type),
                        name: id.clone(),
                        line: self.line_number,
                    });
                }
                self.expect_ident()?;
                Ok(Some(Box::new(Ident::new(id))))
            }
            Token::Number(_) => {
                let num = self.get_numeric_literal(&self.current_token)?;
                self.expect_number()?;
                Ok(Some(Box::new(Number::new(num))))
            }
            Token::LParen => {
                self.expect(Token::LParen)?;
                let expr = self.expression(table, mapped_identifiers)?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::Minus => {
                self.expect(Token::Minus)?;
                let operand = self.factor(table, mapped_identifiers)?;
                if operand.is_none() {
                    return Err(Pl0Error::SyntaxError {
                        expected: "factor".to_string(),
                        found: "none".to_string(),
                        line: self.line_number,
                    });
                }
                let zero = Some(Box::new(Number::new(0)) as Box<dyn ExpressionNode>);
                Ok(Some(Box::new(BinOp::new(zero, operand, "Minus".to_string()))))
            }
            Token::Plus => {
                self.expect(Token::Plus)?;
                self.factor(table, mapped_identifiers)
            }
            _ => Ok(None), // No factor found
        }
    }

    /**
     * Parse a program according to the grammar:
     * program = block "."
     */
    fn program(&mut self, table: &mut SymbolTable, mapped_identifiers: &mut HashMap<String, String>) -> Result<Option<Box<dyn Node>>, Pl0Error> {
        let block = self.block(table, mapped_identifiers)?;
        self.expect(Token::Dot)?;
        let dot = String::from(".");
        Ok(Some(Box::new(Program::new(block, dot))))
    }

    pub fn parse(&mut self, table: &mut SymbolTable) -> Result<Option<Box<dyn Node>>, Pl0Error> {
        let mut mapped_identifiers = HashMap::new();
        self.program(table, &mut mapped_identifiers)
    }
}
