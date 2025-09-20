// ARM64 Architecture Constants
pub mod arm64 {
    // Stack and memory alignment
    pub const STACK_ALIGNMENT: usize = 16;
    pub const WORD_SIZE: usize = 8;
    pub const FRAME_POINTER_OFFSET: i32 = -8;
    pub const STACK_FRAME_SIZE: usize = 16;

    // Register constants
    pub const MAX_IMMEDIATE_VALUE: u64 = 0xFFFF;
    pub const REGISTER_SHIFT_16: u32 = 16;
    pub const REGISTER_SHIFT_32: u32 = 32;
    pub const REGISTER_SHIFT_48: u32 = 48;
    pub const REGISTER_MASK: u64 = 0xFFFF;

    // System call numbers for macOS ARM64
    pub const SYSCALL_EXIT: u64 = 0x2000001;
    pub const SYSCALL_READ: u64 = 0x2000003;
    pub const SYSCALL_WRITE: u64 = 0x2000004;

    // Runtime buffer sizes
    pub const INT_BUFFER_SIZE: usize = 24;
    pub const DECIMAL_BASE: i32 = 10;

    // File descriptors
    pub const STDIN_FD: i32 = 0;
    pub const STDOUT_FD: i32 = 1;

    // Boolean values for assembly
    pub const ASM_TRUE: i32 = 1;
    pub const ASM_FALSE: i32 = 0;
    pub const MAIN_WRAPPER: &'static str = ".section __TEXT,__text\n.global _start\n_start:\n    bl main\n    b .\n";
    pub const EXIT_WRAPPER: &'static str = "    mov x0, #0\n    movz x16, #(0x2000001 & 0xFFFF)\n    movk x16, #((0x2000001 >> 16) & 0xFFFF), lsl #16\n    svc #0\n";
}

///Register allocation constants
pub mod register_allocation {
    // Maximum number of allocatable registers
    pub const MAX_ALLOCATABLE_REGS: usize = 24;

    // Register indices for ARM64
    pub const CALLER_SAVED_REGS: &[usize] = &[
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    ];
    pub const CALLEE_SAVED_REGS: &[usize] = &[19, 20, 21, 22, 23, 24, 25, 26, 27, 28];

    // Special register indices
    pub const FRAME_POINTER_REG: usize = 29; // x29
    pub const LINK_REGISTER: usize = 30; // x30
    pub const STACK_POINTER_REG: usize = 31; // sp

    // Temporary register for address calculations
    pub const RESERVED_REG: u8 = 12; // x12
}

// Parser and symbol table constants
pub mod parser {
    use lazy_static::lazy_static;
    use std::collections::{HashMap, HashSet};

    // Variable offset increment
    pub const VAR_OFFSET_INCREMENT: usize = 8;
    pub const INITIAL_STACK_OFFSET: usize = 16;

    lazy_static! {
        pub static ref RESERVED_KEYWORDS: HashSet<String> = [
            "ret", "global", "mov", "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp",
            "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
            "mov", "ldr", "str", "b", "bl", "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
            "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
            "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
            "x24", "x25", "x26", "x27", "x28", "x29", "x30", "sp", "lr", "fp",
            "@", "%"
        ]
        .iter()
        .map(|s| s.to_string())
        .collect();
    }

    pub fn rename_identifier(name: &str, is_global: bool, mapped_identifiers: &mut HashMap<String, String>) -> String {
        let is_reserved = RESERVED_KEYWORDS.contains(name);
        let renamed = if is_reserved {
            if is_global {
                format!("_{}", name)
            } else {
                format!("%{}", name)
            }
        } else {
            name.to_string()
        };
        mapped_identifiers.insert(name.to_string(), renamed.clone());
        renamed
    }

    pub fn get_renamed_identifier(original: &str, mapped_identifiers: &HashMap<String, String>) -> String {
        mapped_identifiers
            .get(original)
            .cloned()
            .unwrap_or_else(|| original.to_string())
    }
}

// Code generation constants
pub mod codegen {
    // Stack size calculation
    pub const STACK_SIZE_MULTIPLIER: usize = 8;

    // Alignment helpers
    pub fn align_to_16(size: usize) -> usize {
        ((size + 15) / 16) * 16
    }

    pub fn align_stack_size(var_count: usize) -> usize {
        align_to_16(var_count * STACK_SIZE_MULTIPLIER)
    }
}

// Assembly generation constants
pub mod assembly {
    // Instruction prefixes and suffixes
    pub const INSTRUCTION_INDENT: &str = "    ";
    pub const LABEL_SUFFIX: &str = ":\n";

    // Section names
    pub const DATA_SECTION: &str = ".section __DATA,__data\n";
    pub const TEXT_SECTION: &str = ".section __TEXT,__text\n";
    pub const ALIGN_DIRECTIVE: &str = "    .align 3\n";

    // Common instruction patterns
    pub const STACK_PUSH_PATTERN: &str = "[sp, #-16]!";
    pub const STACK_POP_PATTERN: &str = "[sp], #16";

    // Variable storage
    pub const VAR_SKIP_SIZE: usize = 8;
}

// Error messages and debugging
pub mod errors {
    pub const REGISTER_ALLOC_ERROR: &str = "register allocation error";
    pub const FORMAT_ERROR: &str = "fmt error";
    pub const OUTPUT_ERROR: &str = "output error";
}

// Runtime function names
pub mod runtime_functions {
    pub const WRITE_INT: &str = "_write_int";
    pub const READ_INT: &str = "_read_int";
    pub const WRITE_STR: &str = "_write_str";
}

// Bit manipulation helpers
pub mod bit_ops {
    // Extract 16-bit chunks for ARM64 immediate encoding
    pub fn extract_bits_0_15(value: u64) -> u64 {
        value & 0xFFFF
    }

    pub fn extract_bits_16_31(value: u64) -> u64 {
        (value >> 16) & 0xFFFF
    }

    pub fn extract_bits_32_47(value: u64) -> u64 {
        (value >> 32) & 0xFFFF
    }

    pub fn extract_bits_48_63(value: u64) -> u64 {
        (value >> 48) & 0xFFFF
    }

    // Check if a value fits in 16 bits
    pub fn fits_in_16_bits(value: u64) -> bool {
        value <= 0xFFFF
    }
}
