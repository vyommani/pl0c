use clap::Parser;
use pl0c::{
    self, assembly_generator::AssemblyGenerator, codegen::IRGenerator, lexer::scan, parser::parse,
    read, symboltable::SymbolTable, VERSION,
};
use std::{path::PathBuf, process::exit};

#[derive(Parser)]
#[command(
    author,
    version,
    about = "PL/0 compiler that generates x86-64 assembly",
    long_about = "PL/0 compiler that generates x86-64 assembly.\n\
                 This compiler takes PL/0 source code and generates x86-64 assembly code.\n\
                 It performs lexical analysis, parsing, code generation, and register allocation.\n\
                 Example usage:\n\
                 pl0rs input.pl0 -o output.s\n\
                 pl0rs input.pl0 -i -a\n\
                 Copyright (c) Vyom Tewari, 2025 GPLv3\n\
                 For more information, visit: https://github.com/vyommani/pl0c"
)]
struct Cli {
    /// The path to the file to compile
    path: PathBuf,

    /// Output file path (defaults to input file with .s extension)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Print IR code before register allocation
    #[arg(short = 'i', long)]
    print_ir: bool,

    /// Print assembly code after register allocation
    #[arg(short = 'a', long)]
    print_asm: bool,
}

#[derive(Debug)]
enum CompilerError {
    FileReadError(String),
    LexerError(String),
    ParserError(String),
    CodeGenError(String),
    RegisterAllocError(String),
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::FileReadError(msg) => write!(f, "File read error: {}", msg),
            CompilerError::LexerError(msg) => write!(f, "Lexer error: {}", msg),
            CompilerError::ParserError(msg) => write!(f, "Parser error: {}", msg),
            CompilerError::CodeGenError(msg) => write!(f, "Code generation error: {}", msg),
            CompilerError::RegisterAllocError(msg) => write!(f, "Register allocation error: {}", msg),
        }
    }
}

fn compile(input_path: &PathBuf, print_ir: bool, print_asm: bool) -> Result<String, CompilerError> {
    // Read input file
    let bytes = read(input_path).map_err(|e| CompilerError::FileReadError(e.to_string()))?;

    // Lexical analysis
    let mut state = pl0c::LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let tokens = scan(&mut state, &bytes, &mut symbol_table)
        .map_err(|e| CompilerError::LexerError(e.to_string()))?;

    // Parsing
    let mut tokens = tokens;
    let ast = parse(&mut tokens, &mut symbol_table);

    if let Some(ref ast) = ast {
        ast.print();
    }
    // Intermediate Code generation
    let mut codegen = IRGenerator::new(symbol_table);
    codegen.generate_code(ast)
        .map_err(|e| CompilerError::CodeGenError(e.to_string()))?;
    let ir_output = codegen.get_output();
    if print_ir {
        println!("\nIR:");
        println!("{}", ir_output);
    }
    Ok(ir_output)
}

fn main() {
    let args = Cli::parse();

    // Determine output path
    let output_path = args.output.unwrap_or_else(|| {
        let mut path = args.path.clone();
        path.set_extension("s");
        path
    });

    // Compile the code
    match compile(&args.path, args.print_ir, args.print_asm) {
        Ok(asm_output) => {
            // Write output to file
            if let Err(e) = std::fs::write(&output_path, asm_output) {
                eprintln!("Error writing output file: {}", e);
                exit(1);
            }
            println!("Successfully compiled to: {}", output_path.display());
        }
        Err(e) => {
            eprintln!("Compilation failed: {}", e);
            exit(1);
        }
    }
}
