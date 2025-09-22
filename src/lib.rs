use std::{fs::File, io::Read, path::Path};

pub mod assembly_emitter_arm64;
pub mod assembly_emitter_x86_64;
pub mod assembly_generator;
pub mod ast;
pub mod block;
pub mod code_emitter;
pub mod codegen;
pub mod config;
pub mod decl;
pub mod errors;
pub mod expression;
pub mod io;
pub mod ir_dispatch;
pub mod lexer;
pub mod live_range_manager;
pub mod parser;
pub mod program;
pub mod register_allocator_arm64;
pub mod register_allocator_common;
pub mod register_allocator_x86_64;
pub mod register_pool;
pub mod runtime_arm64;
pub mod runtime_x86_64;
pub mod spill_manager;
pub mod statement;
pub mod symboltable;
pub mod token;
pub mod types;
pub mod utils;
pub mod visiters;
pub mod scope_info;

pub const VERSION: &str = "0.1.1";

pub struct LineNumber {
    pub line: usize,
}

impl Default for LineNumber {
    fn default() -> Self {
        Self { line: 1 }
    }
}

use crate::errors::{Pl0Error, Pl0Result};

pub fn read(filename: &Path) -> Pl0Result<String> {
    let path = Path::new(filename);

    match path.extension() {
        Some(ext) => {
            if !ext.eq("pl0") {
                return Err(Pl0Error::FileReadError("File must have a .pl0 extension".to_string()));
            }
        }
        None => {
            return Err(Pl0Error::FileReadError("File must have a .pl0 extension".to_string()));
        }
    }
    // Open the path in read-only mode, returns `io::Result<File>`
    let mut file = File::open(path)?;
    // Read the file contents into a string, returns `io::Result<usize>`
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}
