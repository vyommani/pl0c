use clap::Parser;
use pl0c::{
    self, codegen::IRGenerator, lexer::scan, parser::parse, read, symboltable::SymbolTable, VERSION,
};
use std::process::exit;

#[derive(Parser)]
struct Cli {
    /// The path to the file to compile
    path: std::path::PathBuf,
}

fn main() {
    let args = Cli::parse();
    let mut state = pl0c::LineNumber::default();
    let mut symbol_table = SymbolTable::new();

    println!("pl0rs -- PL/0 compiler version {}", VERSION);
    println!("(c) Vyom Tewari, 2025 GPLv3");

    let bytes = match read(&args.path) {
        Ok(file_content) => file_content,
        Err(_) => {
            println!("Error: No such file({}) found.", args.path.display());
            exit(1);
        }
    };

    let mut tokens = match scan(&mut state, &bytes, &mut symbol_table) {
        Ok(tokens) => tokens,
        Err(e) => {
            println!("{e}");
            exit(1);
        }
    };

    let ast = parse(&mut tokens, &mut symbol_table);
    let mut codegen = IRGenerator::new(symbol_table);
    if let Err(e) = codegen.generate_code(ast) {
        eprintln!("Code generation error: {e}");
        exit(1);
    }
    let output = codegen.get_output();
    println!("\n{}", output);
}
