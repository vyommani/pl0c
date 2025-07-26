use std::io;
use crate::assembly_generator::RegisterAllocator;
use crate::utils::string_utils::write_line;
pub struct Arm64Runtime;

impl Arm64Runtime {
    /// Generic helper for runtime function calls with consistent register management
    fn emit_runtime_call_generic<F, G>(
        reg_name: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        function_name: &str,
        use_alloc: bool, // true for alloc (read_int), false for ensure (write_int/write_str)
        pre_call_setup: F,
        post_call_cleanup: G,
        is_dead_after_fn: impl Fn(&str, usize, &mut dyn RegisterAllocator) -> bool,
    ) -> Result<(), io::Error>
    where
        F: FnOnce(usize, &mut String) -> Result<(), io::Error>,
        G: FnOnce(usize, &mut String) -> Result<(), io::Error>,
    {
        // Register allocation with consistent error handling
        let preg = if use_alloc {
            allocator
                .alloc(reg_name, output)
                .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?
        } else {
            allocator
                .ensure(reg_name, output)
                .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?
        };

        // Pre-call setup (move to x0 for input functions like write_int/write_str)
        pre_call_setup(preg, output)?;

        // Call the runtime function
        write_line(output, format_args!("    bl {}\n", function_name))?;

        // Post-call cleanup (move from x0 for output functions like read_int)
        post_call_cleanup(preg, output)?;
        
        // Cleanup if dead
        if is_dead_after_fn(reg_name, idx, allocator) {
            allocator.free(preg);
        }

        Ok(())
    }

    /// Emit a call to write_int runtime function
    pub fn emit_write_int_call(
        src: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        is_dead_after_fn: impl Fn(&str, usize, &mut dyn RegisterAllocator) -> bool,
    ) -> Result<(), io::Error> {
        Self::emit_runtime_call_generic(src, idx, allocator, output, "_write_int", false, |preg, output| {
            if preg != 0 {
                write_line(output, format_args!("    mov x0, x{}\n", preg))?;
            }
            Ok(())
        }, |preg, output| {
            Ok(())
        }, is_dead_after_fn)
    }

    /// Emit a call to read_int runtime function
    pub fn emit_read_int_call(
        dst: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        is_dead_after_fn: impl Fn(&str, usize, &mut dyn RegisterAllocator) -> bool,
    ) -> Result<(), io::Error> {
        Self::emit_runtime_call_generic(dst, idx, allocator, output, "_read_int", true, |preg, output| {
            Ok(())
        }, |preg, output| {
            write_line(output, format_args!("    mov x{}, x0\n", preg))
        }, is_dead_after_fn)
    }

    /// Emit a call to write_str runtime function
    pub fn emit_write_str_call(
        src: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        is_dead_after_fn: impl Fn(&str, usize, &mut dyn RegisterAllocator) -> bool,
    ) -> Result<(), io::Error> {
        Self::emit_runtime_call_generic(src, idx, allocator, output, "_write_str", false, |preg, output| {
            if preg != 0 {
                write_line(output, format_args!("    mov x0, x{}\n", preg))?;
            }
            Ok(())
        }, |preg, output| {
            Ok(())
        }, is_dead_after_fn)
    }

    /// Emit the complete write_int runtime function implementation
    pub fn emit_write_int_implementation(output: &mut String) -> Result<(), io::Error> {
        write_line(output, format_args!(".section __DATA,__data\n"))?;
        write_line(output, format_args!("    .align 3\n"))?;
        write_line(output, format_args!("int_buffer:\n"))?;
        write_line(output, format_args!("    .zero 24\n"))?;
        // Ensure the following code is in the text section
        write_line(output, format_args!(".section __TEXT,__text\n"))?;
        write_line(output, format_args!(".global _write_int\n"))?;
        write_line(output, format_args!("_write_int:\n"))?;
        write_line(output, format_args!("    stp x29, x30, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    mov x29, sp\n"))?;
        write_line(output, format_args!("    stp x19, x20, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    stp x21, x22, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    adrp x1, int_buffer@PAGE\n"))?;
        write_line(
            output,
            format_args!("    add x1, x1, int_buffer@PAGEOFF\n"),
        )?;
        write_line(output, format_args!("    mov x2, #0\n"))?;
        write_line(output, format_args!("    mov x19, x0\n"))?;
        write_line(output, format_args!("    mov x20, #0\n"))?;
        write_line(output, format_args!("    cmp x19, #0\n"))?;
        write_line(output, format_args!("    b.ge 1f\n"))?;
        write_line(output, format_args!("    neg x19, x19\n"))?;
        write_line(output, format_args!("    mov w20, #1\n"))?;
        write_line(output, format_args!("    b 2f\n"))?;
        write_line(output, format_args!("1:\n"))?;
        write_line(output, format_args!("    mov w20, #0\n"))?;
        write_line(output, format_args!("2:\n"))?;
        write_line(output, format_args!("    mov x7, #10\n"))?;
        write_line(output, format_args!("3:\n"))?;
        write_line(output, format_args!("    udiv x4, x19, x7\n"))?;
        write_line(output, format_args!("    msub x5, x4, x7, x19\n"))?;
        write_line(output, format_args!("    add x5, x5, #'0'\n"))?;
        write_line(output, format_args!("    strb w5, [x1, x2]\n"))?;
        write_line(output, format_args!("    add x2, x2, #1\n"))?;
        write_line(output, format_args!("    mov x19, x4\n"))?;
        write_line(output, format_args!("    cbnz x19, 3b\n"))?;
        write_line(output, format_args!("    cmp w20, #0\n"))?;
        write_line(output, format_args!("    beq 4f\n"))?;
        write_line(output, format_args!("    mov w5, #'-'\n"))?;
        write_line(output, format_args!("    strb w5, [x1, x2]\n"))?;
        write_line(output, format_args!("    add x2, x2, #1\n"))?;
        write_line(output, format_args!("4:\n"))?;
        // Reverse the buffer (digits and sign) before printing
        write_line(output, format_args!("    mov x3, #0\n"))?; // start index
        write_line(output, format_args!("    sub x4, x2, #1\n"))?; // end index (exclude newline)
        write_line(output, format_args!("5:\n"))?;
        write_line(output, format_args!("    cmp x3, x4\n"))?;
        write_line(output, format_args!("    bge 6f\n"))?;
        write_line(output, format_args!("    ldrb w5, [x1, x3]\n"))?;
        write_line(output, format_args!("    ldrb w6, [x1, x4]\n"))?;
        write_line(output, format_args!("    strb w6, [x1, x3]\n"))?;
        write_line(output, format_args!("    strb w5, [x1, x4]\n"))?;
        write_line(output, format_args!("    add x3, x3, #1\n"))?;
        write_line(output, format_args!("    sub x4, x4, #1\n"))?;
        write_line(output, format_args!("    b 5b\n"))?;
        write_line(output, format_args!("6:\n"))?;
        // Now write the newline as before
        write_line(output, format_args!("    mov w5, #'\\n'\n"))?;
        write_line(output, format_args!("    strb w5, [x1, x2]\n"))?;
        write_line(output, format_args!("    add x2, x2, #1\n"))?;
        write_line(output, format_args!("    mov x0, #1\n"))?;
        write_line(
            output,
            format_args!("    movz x16, #(0x2000004 & 0xFFFF)\n"),
        )?;
        write_line(
            output,
            format_args!("    movk x16, #((0x2000004 >> 16) & 0xFFFF), lsl #16\n"),
        )?;
        write_line(output, format_args!("    svc #0\n"))?;
        write_line(output, format_args!("    ldp x19, x20, [sp], #16\n"))?;
        write_line(output, format_args!("    ldp x21, x22, [sp], #16\n"))?;
        write_line(output, format_args!("    ldp x29, x30, [sp], #16\n"))?;
        write_line(output, format_args!("    ret\n"))?;
        Ok(())
    }

    /// Emit the complete read_int runtime function implementation
    pub fn emit_read_int_implementation(output: &mut String) -> Result<(), io::Error> {
        write_line(output, format_args!(".section __DATA,__data\n"))?;
        write_line(output, format_args!("    .align 3\n"))?;
        write_line(output, format_args!("input_buffer:\n"))?;
        write_line(output, format_args!("    .zero 24\n"))?;
        // Ensure the following code is in the text section
        write_line(output, format_args!(".section __TEXT,__text\n"))?;
        write_line(output, format_args!(".global _read_int\n"))?;
        write_line(output, format_args!("_read_int:\n"))?;
        write_line(output, format_args!("    stp x29, x30, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    mov x29, sp\n"))?;
        write_line(output, format_args!("    stp x19, x20, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    str x21, [sp, #-16]!\n"))?;
        
        // Read input from stdin
        write_line(output, format_args!("    adrp x1, input_buffer@PAGE\n"))?;
        write_line(output, format_args!("    add x1, x1, input_buffer@PAGEOFF\n"))?;
        write_line(output, format_args!("    mov x0, #0\n"))?;  // stdin
        write_line(output, format_args!("    mov x2, #24\n"))?; // buffer size
        write_line(output, format_args!("    movz x16, #(0x2000003 & 0xFFFF)\n"))?;
        write_line(output, format_args!("    movk x16, #((0x2000003 >> 16) & 0xFFFF), lsl #16\n"))?;
        write_line(output, format_args!("    svc #0\n"))?;
        
        // x0 now contains number of bytes read
        write_line(output, format_args!("    mov x19, x0\n"))?; // Save bytes read count
        write_line(output, format_args!("    adrp x1, input_buffer@PAGE\n"))?;
        write_line(output, format_args!("    add x1, x1, input_buffer@PAGEOFF\n"))?;
        
        // Initialize parsing variables
        write_line(output, format_args!("    mov x0, #0\n"))?;  // result accumulator
        write_line(output, format_args!("    mov x20, #0\n"))?; // current index
        write_line(output, format_args!("    mov x21, #0\n"))?; // negative flag
        
        // Check for negative sign
        write_line(output, format_args!("    ldrb w2, [x1]\n"))?;
        write_line(output, format_args!("    cmp w2, #'-'\n"))?;
        write_line(output, format_args!("    b.ne .Lparse_loop\n"))?;
        write_line(output, format_args!("    mov x21, #1\n"))?; // Set negative flag
        write_line(output, format_args!("    add x20, x20, #1\n"))?; // Skip the '-' character
        
        // Main parsing loop
        write_line(output, format_args!(".Lparse_loop:\n"))?;
        write_line(output, format_args!("    cmp x20, x19\n"))?; // Check if we've reached end of input
        write_line(output, format_args!("    b.ge .Lparse_done\n"))?;
        write_line(output, format_args!("    ldrb w2, [x1, x20]\n"))?; // Load current character
        
        // Check for newline or non-digit (end of number)
        write_line(output, format_args!("    cmp w2, #'\\n'\n"))?;
        write_line(output, format_args!("    b.eq .Lparse_done\n"))?;
        write_line(output, format_args!("    cmp w2, #'0'\n"))?;
        write_line(output, format_args!("    b.lt .Lparse_done\n"))?;
        write_line(output, format_args!("    cmp w2, #'9'\n"))?;
        write_line(output, format_args!("    b.gt .Lparse_done\n"))?;
        
        // Convert character to digit and accumulate
        write_line(output, format_args!("    sub w2, w2, #'0'\n"))?; // Convert ASCII to digit
        write_line(output, format_args!("    mov x4, #10\n"))?;
        write_line(output, format_args!("    mul x0, x0, x4\n"))?;   // result *= 10
        write_line(output, format_args!("    add x0, x0, x2\n"))?;   // result += digit
        write_line(output, format_args!("    add x20, x20, #1\n"))?; // Move to next character
        write_line(output, format_args!("    b .Lparse_loop\n"))?;
        
        // Apply negative sign if needed
        write_line(output, format_args!(".Lparse_done:\n"))?;
        write_line(output, format_args!("    cmp x21, #0\n"))?;
        write_line(output, format_args!("    b.eq .Lreturn\n"))?;
        write_line(output, format_args!("    neg x0, x0\n"))?;
        
        // Restore registers and return
        write_line(output, format_args!(".Lreturn:\n"))?;
        write_line(output, format_args!("    ldr x21, [sp], #16\n"))?;
        write_line(output, format_args!("    ldp x19, x20, [sp], #16\n"))?;
        write_line(output, format_args!("    ldp x29, x30, [sp], #16\n"))?;
        write_line(output, format_args!("    ret\n"))?;
        Ok(())
    }

    /// Emit the complete write_str runtime function implementation
    pub fn emit_write_str_implementation(output: &mut String) -> Result<(), io::Error> {
        write_line(output, format_args!(".section __TEXT,__text\n"))?;
        write_line(output, format_args!(".global _write_str\n"))?;
        write_line(output, format_args!("_write_str:\n"))?;
        write_line(output, format_args!("    stp x29, x30, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    mov x29, sp\n"))?;
        write_line(output, format_args!("    stp x19, x20, [sp, #-16]!\n"))?;
        
        // x0 contains pointer to null-terminated string
        write_line(output, format_args!("    mov x19, x0\n"))?; // Save string pointer
        
        // Calculate string length
        write_line(output, format_args!("    mov x1, x0\n"))?;  // Copy string pointer to x1
        write_line(output, format_args!("    mov x2, #0\n"))?;  // Length counter
        write_line(output, format_args!("1:\n"))?;
        write_line(output, format_args!("    ldrb w3, [x1, x2]\n"))?; // Load byte
        write_line(output, format_args!("    cbz w3, 2f\n"))?;  // If null terminator, exit loop
        write_line(output, format_args!("    add x2, x2, #1\n"))?; // Increment length
        write_line(output, format_args!("    b 1b\n"))?;        // Continue loop
        
        write_line(output, format_args!("2:\n"))?;
        // Now x19 = string pointer, x2 = string length
        // System call: write(1, string, length)
        write_line(output, format_args!("    mov x0, #1\n"))?;  // stdout
        write_line(output, format_args!("    mov x1, x19\n"))?; // string pointer
        // x2 already contains length
        write_line(
            output,
            format_args!("    movz x16, #(0x2000004 & 0xFFFF)\n"),
        )?;
        write_line(
            output,
            format_args!("    movk x16, #((0x2000004 >> 16) & 0xFFFF), lsl #16\n"),
        )?;
        write_line(output, format_args!("    svc #0\n"))?;
        
        // Restore registers and return
        write_line(output, format_args!("    ldp x19, x20, [sp], #16\n"))?;
        write_line(output, format_args!("    ldp x29, x30, [sp], #16\n"))?;
        write_line(output, format_args!("    ret\n"))?;
        Ok(())
    }
}