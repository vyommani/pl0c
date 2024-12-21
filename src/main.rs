use clap::Parser;
use pl0c::{self, lexer::scan, parser::parse, read, symboltable::SymbolTable, VERSION};
use std::process::exit;

#[derive(Parser)]
struct Cli {
    /// The path to the file to compile
    path: std::path::PathBuf,
}

fn main() {
    let args = Cli::parse();
    let bytes: String;
    let mut state = pl0c::LineNumber::default();
    let mut symbol_table = SymbolTable::new();

    println!("pl0rs -- PL/0 compiler version {}", VERSION);
    println!("(c) Vyom Tewari, 2024 GPLv3");

    let file_path = &args.path;
    match read(file_path) {
        Ok(file_content) => {
            bytes = file_content;
        }
        Err(_) => {
            println!("Error: No such file({}) found.", file_path.display());
            exit(1);
        }
    }

    match scan(&mut state, &bytes, &mut symbol_table) {
        Ok(mut tokens) => {
            let ast = parse(&mut tokens, &mut symbol_table);
            if let Some(ast) = ast {
                ast.print();
            }
        }
        Err(e) => {
            println!("{e}");
        }
    }
}
