use std::io;

/// Shared utility functions for the PL/0 compiler
pub mod string_utils {
    use std::io;
    
    /// Helper function for writing formatted strings to a buffer
    /// Used across assembly emission and runtime code generation
    pub fn write_line(buf: &mut String, args: std::fmt::Arguments) -> io::Result<()> {
        use std::fmt::Write;
        buf.write_fmt(args)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "fmt error"))
    }
}
