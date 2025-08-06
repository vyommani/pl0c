use std::fmt;
use std::io;

#[derive(Debug)]
pub enum Pl0Error {
    // File and I/O errors
    FileReadError(String),
    FileWriteError(String),
    IoError(io::Error),
    
    // Lexical analysis errors
    LexerError {
        message: String,
        line: usize,
        column: Option<usize>,
    },
    UnknownToken {
        token: char,
        line: usize,
    },
    UnterminatedComment {
        line: usize,
    },
    UnterminatedString {
        line: usize,
    },
    MultilineString {
        line: usize,
    },
    
    // Parsing errors
    SyntaxError {
        expected: String,
        found: String,
        line: usize,
    },
    UnexpectedToken {
        token: String,
        line: usize,
    },
    InvalidIdentifier {
        identifier: String,
        line: usize,
    },
    InvalidNumber {
        number: String,
        line: usize,
    },
    
    // Semantic analysis errors
    UndefinedSymbol {
        name: String,
        line: usize,
    },
    TypeMismatch {
        expected: String,
        found: String,
        name: String,
        line: usize,
    },
    SymbolAlreadyDefined {
        name: String,
        line: usize,
    },
    
    // Code generation errors
    CodeGenError {
        message: String,
        line: Option<usize>,
    },
    RegisterAllocationError {
        message: String,
    },
    NoRegistersAvailable,
    SpillFailed,
    RegisterConstraintViolation(String),
    
    // Assembly generation errors
    AssemblyError {
        message: String,
    },
    StackFrameError(String),
    OutputError(String),
    
    // Compilation pipeline errors
    CompilationError {
        stage: String,
        message: String,
    },
    
    // Generic errors
    GenericError(String),
}

impl Pl0Error {
    /// Create a lexer error with line information
    pub fn lexer_error(message: impl Into<String>, line: usize) -> Self {
        Pl0Error::LexerError {
            message: message.into(),
            line,
            column: None,
        }
    }
    
    /// Create a syntax error
    pub fn syntax_error(expected: impl Into<String>, found: impl Into<String>, line: usize) -> Self {
        Pl0Error::SyntaxError {
            expected: expected.into(),
            found: found.into(),
            line,
        }
    }
    
    /// Create a code generation error
    pub fn codegen_error(message: impl Into<String>) -> Self {
        Pl0Error::CodeGenError {
            message: message.into(),
            line: None,
        }
    }
    
    /// Create a code generation error with line information
    pub fn codegen_error_with_line(message: impl Into<String>, line: usize) -> Self {
        Pl0Error::CodeGenError {
            message: message.into(),
            line: Some(line),
        }
    }
    
    /// Create a compilation error for a specific stage
    pub fn compilation_error(stage: impl Into<String>, message: impl Into<String>) -> Self {
        Pl0Error::CompilationError {
            stage: stage.into(),
            message: message.into(),
        }
    }
}

impl fmt::Display for Pl0Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pl0Error::FileReadError(msg) => write!(f, "File read error: {}", msg),
            Pl0Error::FileWriteError(msg) => write!(f, "File write error: {}", msg),
            Pl0Error::IoError(err) => write!(f, "I/O error: {}", err),
            
            Pl0Error::LexerError { message, line, column } => {
                if let Some(col) = column {
                    write!(f, "Lexer error at line {} column {}: {}", line, col, message)
                } else {
                    write!(f, "Lexer error at line {}: {}", line, message)
                }
            }
            Pl0Error::UnknownToken { token, line } => {
                write!(f, "Unknown token '{}' at line {}", token, line)
            }
            Pl0Error::UnterminatedComment { line } => {
                write!(f, "Unterminated comment at line {}", line)
            }
            Pl0Error::UnterminatedString { line } => {
                write!(f, "Unterminated string literal at line {}", line)
            }
            Pl0Error::MultilineString { line } => {
                write!(f, "Multiline string literals not supported at line {}", line)
            }
            
            Pl0Error::SyntaxError { expected, found, line } => {
                write!(f, "Syntax error at line {}: expected '{}', found '{}'", line, expected, found)
            }
            Pl0Error::UnexpectedToken { token, line } => {
                write!(f, "Unexpected token '{}' at line {}", token, line)
            }
            Pl0Error::InvalidIdentifier { identifier, line } => {
                write!(f, "Invalid identifier '{}' at line {}", identifier, line)
            }
            Pl0Error::InvalidNumber { number, line } => {
                write!(f, "Invalid number '{}' at line {}", number, line)
            }
            
            Pl0Error::UndefinedSymbol { name, line } => {
                write!(f, "Undefined symbol '{}' at line {}", name, line)
            }
            Pl0Error::TypeMismatch { expected, found, name, line } => {
                write!(f, "Type mismatch at line {}: expected {} but found {} for '{}'", line, expected, found, name)
            }
            Pl0Error::SymbolAlreadyDefined { name, line } => {
                write!(f, "Symbol '{}' already defined at line {}", name, line)
            }
            
            Pl0Error::CodeGenError { message, line } => {
                if let Some(l) = line {
                    write!(f, "Code generation error at line {}: {}", l, message)
                } else {
                    write!(f, "Code generation error: {}", message)
                }
            }
            Pl0Error::RegisterAllocationError { message } => {
                write!(f, "Register allocation error: {}", message)
            }
            Pl0Error::NoRegistersAvailable => {
                write!(f, "No registers available for allocation")
            }
            Pl0Error::SpillFailed => {
                write!(f, "Failed to spill register to memory")
            }
            Pl0Error::RegisterConstraintViolation(msg) => {
                write!(f, "Register constraint violation: {}", msg)
            }
            
            Pl0Error::AssemblyError { message } => {
                write!(f, "Assembly generation error: {}", message)
            }
            Pl0Error::StackFrameError(msg) => {
                write!(f, "Stack frame error: {}", msg)
            }
            Pl0Error::OutputError(msg) => {
                write!(f, "Output error: {}", msg)
            }
            
            Pl0Error::CompilationError { stage, message } => {
                write!(f, "Compilation error in {}: {}", stage, message)
            }
            
            Pl0Error::GenericError(msg) => {
                write!(f, "Error: {}", msg)
            }
        }
    }
}

impl std::error::Error for Pl0Error {}

// Conversion implementations for common error types
impl From<io::Error> for Pl0Error {
    fn from(err: io::Error) -> Self {
        Pl0Error::IoError(err)
    }
}

impl From<String> for Pl0Error {
    fn from(err: String) -> Self {
        Pl0Error::GenericError(err)
    }
}

impl From<&str> for Pl0Error {
    fn from(err: &str) -> Self {
        Pl0Error::GenericError(err.to_string())
    }
}

// Type alias for Result with Pl0Error
pub type Pl0Result<T> = Result<T, Pl0Error>;

// Helper trait for converting other error types
pub trait IntoPl0Error<T> {
    fn into_pl0_error(self) -> Pl0Result<T>;
}

impl<T> IntoPl0Error<T> for Result<T, String> {
    fn into_pl0_error(self) -> Pl0Result<T> {
        self.map_err(Pl0Error::from)
    }
}

impl<T> IntoPl0Error<T> for Result<T, &str> {
    fn into_pl0_error(self) -> Pl0Result<T> {
        self.map_err(Pl0Error::from)
    }
} 