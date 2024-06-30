use clap::Parser;
use pl0c::{self, lexer::scan, parser::parse, read, VERSION};
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

    match scan(&mut state, &bytes) {
        Ok(mut tokens) => {
            println!("Tokenization successfull!");
            let _ = parse(&mut tokens);
            println!("Parsing successfull, a valid pl/0 program!")
        }
        Err(e) => {
            println!("{e}");
        }
    }
}
