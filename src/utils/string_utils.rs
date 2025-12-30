
/// Shared utility functions for the PL/0 compiler
use crate::utils::errors::{Pl0Error, Pl0Result};

/// Helper function for writing formatted strings to a buffer
/// Used across assembly emission and runtime code generation
pub fn write_line(buf: &mut String, args: std::fmt::Arguments) -> Pl0Result<()> {
    use std::fmt::Write;
    buf.write_fmt(args)
        .map_err(|_| Pl0Error::CodeGenError { message: "fmt error".to_string(), line: None, })
}
