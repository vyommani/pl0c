use clap::Parser;
use pl0c::{
    backend::assembly_generator::{AssemblyGenerator, TargetArch},
    backend::common::target_os::{TargetOS, Linux, FreeBSD, MacOS},
    ir::IRGenerator, frontend::lexer::scan, read, semantic::symboltable::SymbolTable,
};
use std::{path::PathBuf, time::Instant, process::Command, fs};
use pl0c::utils::errors::{Pl0Error, Pl0Result};

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
    // The path to the file to compile
    path: PathBuf,

    // Target architecture (e.g., x86_64, arm64)
    #[arg(long, value_parser = validate_target_arch)]
    target: Option<String>,

    #[arg(long, value_parser = validate_target_os)]
    target_os: Option<String>,

    // Output file path (defaults to input file with .s extension)
    #[arg(short, long)]
    output: Option<PathBuf>,

    // Print IR code before register allocation
    #[arg(short = 'i', long)]
    print_ir: bool,

    // Print assembly code after register allocation
    #[arg(short = 'a', long)]
    print_asm: bool,

    // Enable verbose output
    #[arg(short, long)]
    verbose: bool,

    // Show compilation timing
    #[arg(short, long)]
    timing: bool,

    // Show AST after parsing
    #[arg(long)]
    show_ast: bool,

    // Skip assembly and linking (only generate .s file)
    #[arg(long)]
    no_link: bool,

    // Enable optimization passes
    #[arg(short = 'O', long)]
    optimize: bool,
}

// Compilation statistics for performance analysis
#[derive(Debug)]
struct CompilationStats {
    lexer_time: f64,
    parser_time: f64,
    codegen_time: f64,
    assembly_time: f64,
    linking_time: f64,
    total_time: f64,
    token_count: usize,
    ast_size: usize,
    ir_instructions: usize,
    assembly_instructions: usize,
}

impl CompilationStats {
    fn new() -> Self {
        Self {
            lexer_time: 0.0,
            parser_time: 0.0,
            codegen_time: 0.0,
            assembly_time: 0.0,
            linking_time: 0.0,
            total_time: 0.0,
            token_count: 0,
            ast_size: 0,
            ir_instructions: 0,
            assembly_instructions: 0,
        }
    }
}

// Supported operating systems
#[derive(Debug, PartialEq, Clone)]
enum OperatingSystem { MacOS, Linux, Windows, FreeBSD }

impl OperatingSystem {
    fn detect() -> Pl0Result<Self> {
        match std::env::consts::OS {
            "macos" => Ok(OperatingSystem::MacOS),
            "linux" => Ok(OperatingSystem::Linux),
            "windows" => Ok(OperatingSystem::Windows),
            "freebsd" => Ok(OperatingSystem::FreeBSD),
            other => Err(Pl0Error::compilation_error(
                "os detection",
                format!("Unsupported operating system: {}", other)
            )),
        }
    }

    fn supported_architectures(&self) -> &[&str] {
        match self {
            OperatingSystem::MacOS => &["x86_64", "arm64"],
            OperatingSystem::Linux => &["x86_64"],
            OperatingSystem::Windows => &["x86_64"],
            OperatingSystem::FreeBSD => &["x86_64"],
        }
    }
}

// Validate target architecture argument
fn validate_target_arch(s: &str) -> Result<String, String> {
    match s {
        "x86_64" | "arm64" => Ok(s.to_string()),
        _ => Err(format!("Unsupported target architecture: {}. Supported: x86_64, arm64", s)),
    }
}

// Validate target OS argument
fn validate_target_os(s: &str) -> Result<String, String> {
    match s {
        "linux" | "freebsd" | "macos" => Ok(s.to_string()),
        _ => Err(format!("Unsupported target OS: {}. Supported: linux, freebsd, macos", s)),
    }
}

// Print error message and exit with error code
fn fatal(msg: &str) -> ! {
    eprintln!("Error: {}", msg);
    std::process::exit(1);
}

// Detect target architecture based on user input or host system
fn detect_target_arch(target: &Option<String>) -> Pl0Result<TargetArch> {
    match target.as_deref() {
        Some("x86_64") => Ok(TargetArch::X86_64),
        Some("arm64") => Ok(TargetArch::ARM64),
        Some(other) => Err(Pl0Error::compilation_error(
            "target detection", 
            format!("Unsupported target: {}", other)
        )),
        None => {
            // Auto-detect host architecture
            if cfg!(target_arch = "x86_64") {
                Ok(TargetArch::X86_64)
            } else if cfg!(target_arch = "aarch64") {
                Ok(TargetArch::ARM64)
            } else {
                Err(Pl0Error::compilation_error(
                    "target detection", 
                    "Unsupported host architecture".to_string()
                ))
            }
        }
    }
}

// Detect target OS based on user input or host system
fn detect_target_os(target_os: &Option<String>) -> Pl0Result<Box<dyn TargetOS>> {
    match target_os.as_deref() {
        Some("linux") => Ok(Box::new(Linux)),
        Some("freebsd") => Ok(Box::new(FreeBSD)),
        Some("macos") => Ok(Box::new(MacOS)),
        Some(other) => Err(Pl0Error::compilation_error(
            "OS detection",
            format!("Unsupported target OS: {}", other)
        )),
        None => {
            // Auto-detect host OS
            if cfg!(target_os = "linux") {
                Ok(Box::new(Linux))
            } else if cfg!(target_os = "freebsd") {
                Ok(Box::new(FreeBSD))
            } else if cfg!(target_os = "macos") {
                Ok(Box::new(MacOS))
            } else {
                Err(Pl0Error::compilation_error(
                    "OS detection",
                    "Unsupported host OS. Please specify --target-os explicitly".to_string()
                ))
            }
        }
    }
}

// Generate output file path from input path if not specified
fn determine_output_path(input_path: &PathBuf, output: &Option<PathBuf>) -> PathBuf {
    match output {
        Some(path) => path.clone(),
        None => {
            let basename = input_path.file_stem().unwrap_or_default();
            let mut path = PathBuf::from(basename);
            path.set_extension("s");
            path
        }
    }
}

fn calculate_ast_size(ast: &Option<Box<dyn pl0c::ast::Node>>) -> usize {
    // Placeholder implementation
    match ast {
        Some(_) => 1, // Basic implementation
        None => 0,
    }
}

// Perform lexical analysis phase
fn lexical_analysis(
    bytes: &str, 
    stats: &mut CompilationStats, 
    verbose: bool
) -> Pl0Result<(Vec<(pl0c::frontend::token::Token, usize)>, SymbolTable)> {
    let lexer_start = Instant::now();
    let mut state = pl0c::LineNumber::default();
    let mut symbol_table = SymbolTable::new();
    let tokens = scan(&mut state, bytes, &mut symbol_table)?;
    stats.lexer_time = lexer_start.elapsed().as_secs_f64();
    stats.token_count = tokens.len();

    if verbose {
        println!("✓ Lexical analysis completed in {:.3}s", stats.lexer_time);
        println!("  Generated {} tokens", stats.token_count);
    }

    Ok((tokens, symbol_table))
}

// Perform parsing phase
fn parsing_phase(
    tokens: &mut Vec<(pl0c::frontend::token::Token, usize)>,
    symbol_table: &mut SymbolTable,
    stats: &mut CompilationStats,
    verbose: bool
) -> Pl0Result<Option<Box<dyn pl0c::ast::Node>>> {
    let parser_start = Instant::now();
    let mut parser = pl0c::frontend::parser::Parser::new(tokens);
    let ast = parser.parse(symbol_table)?;
    stats.parser_time = parser_start.elapsed().as_secs_f64();
    stats.ast_size = calculate_ast_size(&ast);

    if verbose {
        println!(" Parsing completed in {:.3}s", stats.parser_time);
    }

    Ok(ast)
}

// Perform code generation phase
fn code_generation_phase(
    ast: Option<Box<dyn pl0c::ast::Node>>,
    symbol_table: SymbolTable,
    stats: &mut CompilationStats,
    verbose: bool
) -> Pl0Result<String> {
    let codegen_start = Instant::now();
    let mut codegen = IRGenerator::new(symbol_table);
    codegen.generate_code(ast)?;
    let ir_output = codegen.get_output();
    stats.codegen_time = codegen_start.elapsed().as_secs_f64();
    stats.ir_instructions = ir_output.lines().count();

    if verbose {
        println!(" Code generation completed in {:.3}s", stats.codegen_time);
        println!(" Generated {} IR instructions", stats.ir_instructions);
    }

    Ok(ir_output)
}

// Perform assembly generation phase
fn assembly_generation_phase(
    ir_output: &str,
    target_arch: TargetArch,
    target_os: Box<dyn TargetOS>,
    stats: &mut CompilationStats,
    verbose: bool
) -> Pl0Result<String> {
    let assembly_start = Instant::now();
    let mut assembly_gen = AssemblyGenerator::new(target_arch, target_os);
    assembly_gen.emit_assembly(ir_output)?;
    let assembly_output = assembly_gen.get_output().to_string();
    stats.assembly_time = assembly_start.elapsed().as_secs_f64();
    stats.assembly_instructions = assembly_output.lines().filter(|line| !line.trim().is_empty()).count();

    if verbose {
        println!("  Assembly generation completed in {:.3}s", stats.assembly_time);
        println!("  Generated {} assembly instructions", stats.assembly_instructions);
    }

    Ok(assembly_output)
}

// Main compilation function
fn compile(input_path: &PathBuf, args: &Cli) -> Pl0Result<(String, CompilationStats)> {
    let start_time = Instant::now();
    let mut stats = CompilationStats::new();

    if args.verbose {
        println!(" Starting compilation of: {}", input_path.display());
    }

    // Validate input file exists
    if !input_path.exists() {
        return Err(Pl0Error::compilation_error(
            "file not found",
            format!("Input file does not exist: {}", input_path.display())
        ));
    }

    // Determine target architecture
    let target_arch = detect_target_arch(&args.target)?;

    // Determine target OS (auto-detect if not specified)
    let target_os = detect_target_os(&args.target_os)?;
    if args.verbose {
        // Prefer explicit CLI value when present, otherwise indicate auto-detection
        let os_name = match args.target_os.as_deref() {
            Some(s) => s.to_string(),
            None => "auto-detected".to_string(),
        };
        let arch_name = match target_arch {
            TargetArch::X86_64 => "x86_64",
            TargetArch::ARM64 => "arm64",
        };
        println!("Target: {} on {}", arch_name, os_name);
    }

    // Read input file
    let source_str = read(input_path)?;
    if args.verbose {
        println!("Read {} bytes from input file", source_str.len());
    }

    // Lexical analysis
    let (mut tokens, mut symbol_table) = lexical_analysis(&source_str, &mut stats, args.verbose)?;

    // Parsing
    let ast = parsing_phase(&mut tokens, &mut symbol_table, &mut stats, args.verbose)?;

    // Show AST if requested
    if args.show_ast {
        println!("\n Abstract Syntax Tree:");
        if let Some(ref ast) = ast {
            ast.print();
        } else {
            println!("  (empty)");
        }
    }

    // Intermediate Code generation
    let ir_output = code_generation_phase(ast, symbol_table, &mut stats, args.verbose)?;

    // Print IR if requested
    if args.print_ir {
        println!("\n Intermediate Representation:");
        println!("{}", ir_output);
    }

    // Assembly generation
    let assembly_output = assembly_generation_phase(&ir_output, target_arch, target_os, &mut stats, args.verbose)?;

    // Print assembly if requested
    if args.print_asm {
        println!("\n  Generated Assembly:");
        println!("{}", assembly_output);
    }

    stats.total_time = start_time.elapsed().as_secs_f64();

    if args.verbose {
        println!(" Compilation completed in {:.3}s", stats.total_time);
    }

    Ok((assembly_output, stats))
}

// Run assembler
fn run_assembler(
    assembler: &str,
    assembler_args: Vec<&str>,
    verbose: bool
) -> Pl0Result<()> {
    let output = Command::new(assembler)
        .args(&assembler_args)
        .output()
        .map_err(|e| Pl0Error::compilation_error("assembly", e.to_string()))?;

    if !output.status.success() {
        return Err(Pl0Error::compilation_error(
            "assembly",
            format!("Assembler '{}' failed: {}", assembler, String::from_utf8_lossy(&output.stderr))
        ));
    }

    if verbose {
        println!("  {} completed successfully", assembler);
    }

    Ok(())
}

// Run linker
fn run_linker(
    linker: &str,
    linker_args: Vec<&str>,
    verbose: bool
) -> Pl0Result<()> {
    let output = Command::new(linker)
        .args(&linker_args)
        .output()
        .map_err(|e| Pl0Error::compilation_error("linking", e.to_string()))?;

    if !output.status.success() {
        return Err(Pl0Error::compilation_error(
            "linking",
            format!("Linker '{}' failed: {}", linker, String::from_utf8_lossy(&output.stderr))
        ));
    }

    if verbose {
        println!("  {} completed successfully", linker);
    }

    Ok(())
}

// Platform-specific assembly and linking
fn assemble_and_link(
    arch: &str, 
    asm_file: &str, 
    exe_file: &str, 
    verbose: bool
) -> Pl0Result<f64> {
    let start_time = Instant::now();
    let os = OperatingSystem::detect()?;

    // Use unique object file name to avoid collisions
    let obj_file = format!("output_{}.o", std::process::id());

    // Validate architecture is supported on this OS
    if !os.supported_architectures().contains(&arch) {
        return Err(Pl0Error::compilation_error(
            "architecture validation",
            format!("Architecture '{}' is not supported on {:?}. Supported: {:?}", 
                    arch, os, os.supported_architectures())
        ));
    }

    if verbose {
        println!(" Assembling and linking for {} on {:?}", arch, os);
    }

    match os {
        OperatingSystem::MacOS => {
            run_assembler("as", vec!["-arch", arch, "-o", &obj_file, asm_file], verbose)?;
            run_linker("clang", vec!["-arch", arch, "-o", exe_file, &obj_file, "-e", "_start"], verbose)?;
        }
        OperatingSystem::Linux => {
            run_assembler("nasm", vec!["-f", "elf64", asm_file, "-o", &obj_file], verbose)?;
            run_linker("ld", vec![&obj_file, "-o", exe_file], verbose)?;
        }
        OperatingSystem::FreeBSD => {
            run_assembler("nasm", vec!["-f", "elf64", "-o", &obj_file, asm_file], verbose)?;
            run_linker("ld", vec!["-m", "elf_amd64_fbsd", "-static", &obj_file, "-o", exe_file], verbose)?;
        }
        OperatingSystem::Windows => {
            return Err(Pl0Error::compilation_error(
                "platform support",
                "Windows support not yet implemented".to_string()
            ));
        }
    }

    // Clean up object file
    if let Err(e) = fs::remove_file(&obj_file) {
       if verbose {
           eprintln!("Warning: Failed to clean up {}: {}", obj_file, e);
       }
    }

    let linking_time = start_time.elapsed().as_secs_f64();
    if verbose {
        println!(" Assembly and linking completed in {:.3}s", linking_time);
    }

    Ok(linking_time)
}

// Print detailed compilation statistics
fn print_stats(stats: &CompilationStats) {
    println!("\n Compilation Statistics:");
    println!("┌─────────────────────┬───────────┐");
    println!("│ Phase               │ Time (s)  │");
    println!("├─────────────────────┼───────────┤");
    println!("│ Lexical Analysis    │ {:>8.3} │", stats.lexer_time);
    println!("│ Parsing             │ {:>8.3} │", stats.parser_time);
    println!("│ Code Generation     │ {:>8.3} │", stats.codegen_time);
    println!("│ Assembly Generation │ {:>8.3} │", stats.assembly_time);
    println!("│ Linking             │ {:>8.3} │", stats.linking_time);
    println!("├─────────────────────┼───────────┤");
    println!("│ Total Time          │ {:>8.3} │", stats.total_time);
    println!("└─────────────────────┴───────────┘");
    
    println!("\n Code Metrics:");
    println!("  • Tokens:             {}", stats.token_count);
    println!("  • AST Nodes:          {}", stats.ast_size);
    println!("  • IR Instructions:    {}", stats.ir_instructions);
    println!("  • Assembly Lines:     {}", stats.assembly_instructions);
}

fn main() {
    let args = Cli::parse();

    // Validate input file extension
    if let Some(ext) = args.path.extension() {
        if ext != "pl0" {
            eprintln!("Warning: Input file does not have .pl0 extension");
        }
    }

    match compile(&args.path, &args) {
        Ok((output, mut stats)) => {
            let output_path = determine_output_path(&args.path, &args.output);

            // Write assembly output to file
            if let Err(e) = fs::write(&output_path, &output) {
                fatal(&format!("Failed to write output file '{}': {}", output_path.display(), e));
            }

            if args.verbose {
                println!("Assembly written to: {}", output_path.display());
            }

            // Assemble and link unless --no-link is specified
            if !args.no_link {
                let target_arch_str = match detect_target_arch(&args.target) {
                    Ok(TargetArch::X86_64) => "x86_64",
                    Ok(TargetArch::ARM64) => "arm64",
                    Err(e) => fatal(&format!("Target detection failed: {}", e)),
                };

                let asm_file = output_path.to_string_lossy();
                let exe_file = output_path.with_extension("");
                let exe_file_str = exe_file.to_string_lossy();

                match assemble_and_link(&target_arch_str, &asm_file, &exe_file_str, args.verbose) {
                    Ok(linking_time) => {
                        stats.linking_time = linking_time;
                        stats.total_time += linking_time;
                        
                        if args.verbose {
                            println!("Executable created: {}", exe_file_str);
                        }
                    }
                    Err(e) => fatal(&format!("Assembly/linking failed: {}", e)),
                }
            }

            // Print timing information if requested
            if args.timing {
                print_stats(&stats);
            }

            if args.verbose {
                println!("Compilation successful!");
            }
        }
        Err(e) => fatal(&format!("Compilation failed: {}", e)),
    }
}