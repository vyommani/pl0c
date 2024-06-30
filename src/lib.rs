use std::{fs::File, io::Read, path::Path, process::exit};

pub mod lexer;
pub mod parser;
pub mod token;
pub mod ast;

pub const VERSION: &str = "0.1.0";

pub struct LineNumber {
    pub line: u32,
}

impl Default for LineNumber {
    fn default() -> Self {
        Self {
            line: 1,
        }
    }
}

pub fn read(filename: &Path) -> Result<String, String> {
    let path = Path::new(filename);

    match path.extension() {
        Some(ext) => {
            if !ext.eq("pl0") {
                println!("Error: File must have a .pl0 extension");
                exit(1);
            }
        }
        None => {
            println!("Error: File must have a .pl0 extension");
            exit(1);
        }
    }
    // Open the path in read-only mode, returns `io::Result<File>`
    let mut file = match File::open(path) {
        Err(why) => return Err(format!("couldn't open file: {why}")),
        Ok(file) => file,
    };
    // Read the file contents into a string, returns `io::Result<usize>`
    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Err(why) => Err(format!("couldn't read: {why}")),
        Ok(_) => Ok(contents),
    }
}