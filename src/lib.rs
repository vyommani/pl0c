use std::{fs::File, io::Read, path::Path};

pub mod ast;
pub mod ir;
pub mod config;
pub mod errors;
pub mod symboltable;
pub mod utils;
pub mod visiters;
pub mod scope_info;
pub mod frontend;
pub mod backend;

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
