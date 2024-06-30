PL/0 Compiler

This project implements a simple compiler for the PL/0 programming language in Rust. The compiler includes a lexer, parser, abstract syntax tree (AST) generation, and a code generator to output PL/0 assembly code.
Table of Contents
    Introduction
    PL/0 Programming Language
        Syntax
        Sample Program
    Features
    Directory Structure
    Getting Started
    Usage
    Examples
    Contributing
    License

Introduction

PL/0 is a small educational programming language used to teach compiler construction. This project provides a basic implementation of a PL/0 compiler in Rust, demonstrating the main components of a compiler, including lexical analysis, parsing, AST construction, and code generation.
PL/0 Programming Language
Syntax

PL/0 is a simple, structured programming language that includes basic constructs such as variables, arithmetic operations, conditionals, and loops. The syntax is similar to Pascal and is designed to be easy to parse and understand. Below are some of the key constructs in PL/0:

    Variable Assignment: x := 5;
    Arithmetic Operations: x := 5 + 3;
    Conditionals: if x > 5 then y := 10 else y := 20;
    Loops: while x < 10 do x := x + 1;

Sample Program

pl0

var x, y, z;
begin
    x := 1;
    y := 2;
    z := x + y;
    if z > 2 then
        z := z - 1
    else
        z := z + 1;
    while x < 10 do
        x := x + 1;
end.

This sample program demonstrates variable declarations, assignments, conditionals, and loops in PL/0.
Features

    Lexer: Converts source code into tokens.
    Parser: Parses tokens into an abstract syntax tree (AST).
    AST: Represents the program's structure in a tree format.
    Code Generator: Translates the AST into PL/0 assembly code.

Directory Structure

css

src/
├── ast.rs
├── code_generator.rs
├── lexer.rs
├── main.rs
├── parser.rs
└── token.rs
|__ lib.rs

    ast.rs: Defines the abstract syntax tree (AST) nodes and the visitor pattern.
    code_generator.rs: Generates PL/0 assembly code from the AST.
    lexer.rs: Lexical analyzer that tokenizes the input source code.
    main.rs: Entry point of the application.
    parser.rs: Parses tokens into an AST.
    token.rs: Defines the tokens used by the lexer and parser.

Getting Started
Prerequisites

    Rust (version 1.50 or later)

Installation

Clone the repository:

bash

git clone https://github.com/vyommani/pl0c.git
cd pl0c

Usage

To compile and run the PL/0 compiler:

bash

cargo run

Contributions are welcome! Please open an issue or submit a pull request if you have any suggestions or improvements.

Steps to Contribute

    Fork the repository.
    Create a new branch (git checkout -b feature-branch).
    Commit your changes (git commit -am 'Add new feature').
    Push to the branch (git push origin feature-branch).
    Open a pull request.

License

This project is licensed under the MIT License. See the LICENSE file for details.
