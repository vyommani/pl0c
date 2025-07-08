use clap::Parser;
use pl0c::{
    self, assembly_generator::AssemblyGenerator, assembly_generator::TargetArch,
    codegen::IRGenerator, lexer::scan, parser::parse, read, symboltable::SymbolTable,
};
use std::{path::PathBuf, process::exit, process::Command, time::Instant};

#[derive(Parser)]
#[command(
    author,
    version,
    about = "PL/0 compiler that generates platform-specific assembly",
    long_about = "PL/0 compiler that generates platform-specific assembly.\n\
                 This compiler takes PL/0 source code and generates platform-specific assembly code.\n\
                 It performs lexical analysis, parsing, and intermediate code generation.\n\
                 \n\
                 Example usage:\n\
                 pl0c input.pl0                    # Compile to assembly\n\
                 pl0c input.pl0 -o output.s        # Specify output file\n\
                 pl0c input.pl0 --print-ir         # Print IR code\n\
                 pl0c input.pl0 --verbose          # Verbose compilation output\n\
                 pl0c input.pl0 --timing           # Show compilation timing\n\
                 pl0c input.pl0 --show-ast         # Display abstract syntax tree\n\
                 \n\
                 Copyright (c) Vyom Tewari, 2025 GPLv3\n\
                 For more information, visit: https://github.com/vyommani/pl0c"
)]
struct Cli {
    /// The path to the file to compile
    path: PathBuf,

    /// Target architecture (e.g., x86_64, arm64)
    #[arg(long)]
    target: Option<String>,

    /// Output file path (defaults to input file with .s extension)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Print IR code before register allocation
    #[arg(short = 'i', long)]
    print_ir: bool,

    /// Print assembly code after register allocation
    #[arg(short = 'a', long)]
    print_asm: bool,

    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Show compilation timing
    #[arg(short, long)]
    timing: bool,

    /// Show AST after parsing
    #[arg(long)]
    show_ast: bool,
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
            CompilerError::RegisterAllocError(msg) => {
                write!(f, "Register allocation error: {}", msg)
            }
        }
    }
}

struct CompilationStats {
    lexer_time: f64,
    parser_time: f64,
    codegen_time: f64,
    assembly_time: f64,
    total_time: f64,
    token_count: usize,
    ast_size: usize,
    ir_instructions: usize,
    assembly_instructions: usize,
}

fn fatal(msg: &str) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1);
}

fn detect_arch(target: &Option<String>) -> &'static str {
    match target.as_deref() {
        Some("x86_64") => "x86_64",
        Some("arm64") => "arm64",
        _ => {
            if cfg!(target_arch = "x86_64") {
                "x86_64"
            } else if cfg!(target_arch = "aarch64") {
                "arm64"
            } else {
                fatal("Unsupported architecture for build.sh");
            }
        }
    }
}

fn compile(input_path: &PathBuf, args: &Cli) -> Result<(String, CompilationStats), CompilerError> {
    let start_time = Instant::now();

    if args.verbose {
        println!("Starting compilation of: {}", input_path.display());
    }

    // Determine target architecture
    let target_arch = match args.target.as_deref() {
        Some("x86_64") => TargetArch::X86_64,
        Some("arm64") => TargetArch::ARM64,
        Some(other) => {
            return Err(CompilerError::ParserError(format!(
                "Unsupported target: {}",
                other
            )))
        }
        None => {
            // Auto-detect host architecture
            if cfg!(target_arch = "x86_64") {
                TargetArch::X86_64
            } else if cfg!(target_arch = "aarch64") {
                TargetArch::ARM64
            } else {
                return Err(CompilerError::ParserError(
                    "Unsupported host architecture".to_string(),
                ));
            }
        }
    };

    // Read input file
    let bytes = read(input_path).map_err(|e| CompilerError::FileReadError(e.to_string()))?;
    if args.verbose {
        println!("Read {} bytes from input file", bytes.len());
    }

    // Lexical analysis
    let lexer_start = Instant::now();
    let mut state = pl0c::LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let tokens = scan(&mut state, &bytes, &mut symbol_table)
        .map_err(|e| CompilerError::LexerError(e.to_string()))?;
    let lexer_time = lexer_start.elapsed().as_secs_f64();

    if args.verbose {
        println!("Lexical analysis completed in {:.3}s", lexer_time);
        println!("Generated {} tokens", tokens.len());
    }

    // Parsing
    let parser_start = Instant::now();
    let ast = parse(&mut tokens.clone(), &mut symbol_table);
    let parser_time = parser_start.elapsed().as_secs_f64();

    if args.verbose {
        println!("Parsing completed in {:.5}s", parser_time);
    }

    if let Some(ref ast) = ast {
        if args.show_ast {
            println!("\nAbstract Syntax Tree:");
            ast.print();
        }
    } else {
        return Err(CompilerError::ParserError(
            "Failed to parse input".to_string(),
        ));
    }

    // Intermediate Code generation
    let codegen_start = Instant::now();
    let mut codegen = IRGenerator::new(symbol_table);
    codegen
        .generate_code(ast)
        .map_err(|e| CompilerError::CodeGenError(e.to_string()))?;
    let ir_output = codegen.get_output();
    let codegen_time = codegen_start.elapsed().as_secs_f64();

    if args.verbose {
        println!("Code generation completed in {:.3}s", codegen_time);
    }

    // Print IR if requested
    if args.print_ir {
        println!("\nIntermediate Representation:");
        println!("{}", ir_output);
    }

    // Assembly generation
    let assembly_start = Instant::now();
    let mut assembly_gen = AssemblyGenerator::new(target_arch);
    assembly_gen
        .emit_assembly(&ir_output)
        .map_err(|e| CompilerError::RegisterAllocError(e.to_string()))?;
    let assembly_output = assembly_gen.get_output().to_string();
    let assembly_time = assembly_start.elapsed().as_secs_f64();

    if args.verbose {
        println!("Assembly generation completed in {:.3}s", assembly_time);
    }

    // Print assembly if requested
    if args.print_asm {
        println!("\nGenerated Assembly:");
        println!("{}", assembly_output);
    }

    let total_time = start_time.elapsed().as_secs_f64();

    let stats = CompilationStats {
        lexer_time,
        parser_time,
        codegen_time,
        assembly_time,
        total_time,
        token_count: tokens.len(),
        ast_size: 1, // Placeholder - could implement AST size calculation
        ir_instructions: ir_output.lines().count(),
        assembly_instructions: assembly_output.lines().count(),
    };

    Ok((assembly_output, stats))
}

fn print_stats(stats: &CompilationStats) {
    println!("\nCompilation Statistics:");
    println!("  Lexical Analysis: {:.3}s", stats.lexer_time);
    println!("  Parsing:          {:.3}s", stats.parser_time);
    println!("  Code Generation:  {:.3}s", stats.codegen_time);
    println!("  Assembly Gen:     {:.3}s", stats.assembly_time);
    println!("  Total Time:       {:.3}s", stats.total_time);
    println!("  Tokens:           {}", stats.token_count);
    println!("  IR Instructions:  {}", stats.ir_instructions);
    println!("  Assembly Instr:   {}", stats.assembly_instructions);
}

fn assemble_and_link(arch: &str, asm_file: &str, exe_file: &str) {
    use std::process::Command;
    use std::path::Path;
    use std::fs;
    let os_name = std::env::consts::OS;
    let obj_file = "output.o";
    let as_status;
    let link_status;
    if os_name == "macos" {
        // macOS: support both x86_64 and arm64
        if arch != "x86_64" && arch != "arm64" {
            fatal("On macOS, only x86_64 and arm64 architectures are supported.");
        }
        as_status = Command::new("as")
            .arg("-arch").arg(arch)
            .arg("-o").arg(obj_file)
            .arg(asm_file)
            .status();
        if let Err(e) = &as_status {
            fatal(&format!("Failed to invoke assembler: {}", e));
        }
        if !as_status.as_ref().unwrap().success() {
            fatal("Assembler failed.");
        }
        link_status = Command::new("clang")
            .arg("-arch").arg(arch)
            .arg("-o").arg(exe_file)
            .arg(obj_file)
            .arg("-e").arg("_start")
            .status();
        if let Err(e) = &link_status {
            fatal(&format!("Failed to invoke linker: {}", e));
        }
        if !link_status.as_ref().unwrap().success() {
            fatal("Linker failed.");
        }
    } else if os_name == "linux" {
        // Linux: only support x86_64
        if arch != "x86_64" {
            fatal("On Linux, only x86_64 architecture is supported.");
        }
        as_status = Command::new("as")
            .arg("-o").arg(obj_file)
            .arg(asm_file)
            .status();
        if let Err(e) = &as_status {
            fatal(&format!("Failed to invoke assembler: {}", e));
        }
        if !as_status.as_ref().unwrap().success() {
            fatal("Assembler failed.");
        }
        link_status = Command::new("gcc")
            .arg("-no-pie")
            .arg("-o").arg(exe_file)
            .arg(obj_file)
            .status();
        if let Err(e) = &link_status {
            fatal(&format!("Failed to invoke linker: {}", e));
        }
        if !link_status.as_ref().unwrap().success() {
            fatal("Linker failed.");
        }
    } else {
        fatal(&format!("Unsupported OS: {}", os_name));
    }
    // Clean up object file
    let _ = fs::remove_file(obj_file);
}

fn main() {
    let args = Cli::parse();

    match compile(&args.path, &args) {
        Ok((output, stats)) => {
            let output_path = match &args.output {
                Some(p) => p.clone(),
                None => {
                    let basename = args.path.file_stem().unwrap_or_default();
                    let mut path = std::path::PathBuf::from(basename);
                    path.set_extension("s");
                    path
                }
            };

            // Write output to file
            if let Err(e) = std::fs::write(&output_path, &output) {
                fatal(&format!("Error writing output file: {}", e));
            }

            let arch = detect_arch(&args.target);
            let asm_file = output_path.to_string_lossy();
            let exe_file = output_path.with_extension("");
            let exe_file_str = exe_file.to_string_lossy();
            assemble_and_link(arch, &asm_file, &exe_file_str);

            if args.timing {
                print_stats(&stats);
            }
        }
        Err(e) => fatal(&format!("Compilation failed: {}", e)),
    }
}
