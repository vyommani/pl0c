PL/0 Compiler

A Rust-based compiler for the PL/0 programming language, designed for educational purposes to demonstrate compiler construction. This project implements a lexer, parser, abstract syntax tree (AST) generation, intermediate representation (IR) generation, and code generation targeting x86-64 & ARM64 assembly.
Table of Contents
    Introduction (#introduction)
    PL/0 Language Syntax (#pl0-language-syntax)
    Sample Program (#sample-program)
    Features (#features)
    Directory Structure (#directory-structure)
    Getting Started (#getting-started)
    Usage (#usage)
    Roadmap (#roadmap)
    Contributing (#contributing)
    License (#license)


Introduction

PL/0 is a minimalist programming language used to teach compiler design principles. This project provides a Rust implementation of a PL/0 compiler, showcasing key compiler components: lexical analysis, parsing, AST construction, IR generation, and code generation for x86-64 assembly. The compiler is a work-in-progress, with active development on IR optimization and assembly emission.

PL/0 Programming Language Syntax

PL/0 supports structured programming with variables, arithmetic, conditionals, loops, and procedures. Its syntax is inspired by Pascal, making it simple to parse and understand. Below are the key constructs:

Sample Program

var x, y, z;
begin
    x := 1;          { Initialize x to 1 }
    y := 2;          { Initialize y to 2 }
    z := x + y;      { Compute sum }
    if z > 2 then
        z := z - 1   { Decrement if sum > 2 }
    else
        z := z + 1;  { Increment otherwise }
    while x < 10 do  { Loop until x reaches 10 }
        x := x + 1;
    writeInt(z);     { Output z }
end.

This sample program demonstrates variable declarations, assignments, conditionals, and loops in PL/0.

Features
    Lexer: Tokenizes PL/0 source code into a stream of tokens.
    Parser: Constructs an AST from tokens (in progress).
    AST: Represents program structure using a tree (partially implemented).
    IR Generator: Translates AST to intermediate representation (work in progress).
    Code Generator: Emits x86-64 assembly with register allocation and spilling (in development).
    Error Handling: Provides detailed error messages for invalid syntax or semantics.
    Static Link Support: Handles nested procedures via static links.
    

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

cargo build

Contributions are welcome! Please open an issue or submit a pull request if you have any suggestions or improvements.

Steps to Contribute

    Fork the repository.
    Create a new branch (git checkout -b feature-branch).
    Commit your changes (git commit -am 'Add new feature').
    Push to the branch (git push origin feature-branch).
    Open a pull request.

License

This project is licensed under the MIT License. See the LICENSE file for details.
