use crate::{
    assembly_generator::{AssemblyEmitter, RegisterAllocator},
    register_allocator_arm64::RegisterName,
    register_allocator_common::Register,
};
use regex::Regex;
use std::{
    collections::HashMap,
    io::{self},
};
use crate::ir_dispatch::IROp;

pub struct Arm64AssemblyEmitter;

fn write_line(buf: &mut String, args: std::fmt::Arguments) -> std::io::Result<()> {
    use std::fmt::Write;
    buf.write_fmt(args)
        .map_err(|_| std::io::Error::new(std::io::ErrorKind::Other, "fmt error"))
}

impl AssemblyEmitter for Arm64AssemblyEmitter {
    fn emit(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        // Compute next uses of vregs
        self.compute_vreg_next_uses(ir, allocator)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;
        // Collect variables, constants, proc_output, and main_output
        let (variables, constants, needs_write_int, needs_read_int) = Self::collect_data_info(ir);
        let mut data_output = String::new();
        Self::emit_data_section(&mut data_output, &variables, &constants)?;
        let mut proc_output = String::new();
        let mut main_output = String::new();
        let main_stack_size = self.process_ir_lines(ir, allocator, &mut proc_output, &mut main_output)?;
        // Prepend .global main and prologue to main_output
        let new_main_output = Self::emit_main_section(&main_output, allocator, main_stack_size)?;
        // Assemble final output
        let mut footer = String::new();
        self.emit_footer(&mut footer, needs_write_int, needs_read_int)?;
        let final_output = Self::assemble_final_output(&data_output, &new_main_output, &proc_output, &footer)?;
        output.push_str(&final_output);
        Ok(())
    }

    fn compute_vreg_next_uses(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
    ) -> Result<(), crate::register_allocator_common::RegisterError> {
        let mut vreg_uses: HashMap<String, Vec<usize>> = HashMap::new();
        let vreg_regex = Regex::new(r"v[0-9]+\b").unwrap();
        let mut call_indices = Vec::new();
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            // Record function call instructions
            let is_call = line.starts_with("write_int") || line.starts_with("read_int") || line.starts_with("call ");
            if is_call {
                call_indices.push(idx as i32);
            }
            for cap in vreg_regex.captures_iter(line) {
                let vreg = cap.get(0).unwrap().as_str();
                vreg_uses.entry(vreg.to_string()).or_default().push(idx);
            }
        }
        for (i, (vreg, uses)) in vreg_uses.into_iter().enumerate() {
            let live_range = if let (Some(&first), Some(&last)) = (uses.iter().min(), uses.iter().max()) {
                (first as i32, last as i32)
            } else {
                (0, 0)
            };
            // Check if vreg is live across a call
            let live_across_call = call_indices.iter().any(|&call_idx| call_idx >= live_range.0 && call_idx <= live_range.1);
            let mut reg = Register::new(
                usize::MAX,
                i,
                RegisterName::SP,
                uses.iter().map(|u| *u as i32).collect(),
                0,
            );
            reg.live_across_call = live_across_call;
            if let Some(alloc) = allocator
                .as_any_mut()
                .downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>()
            {
                alloc.vreg_map.insert(vreg, reg);
                alloc.set_live_range(i, live_range.0, live_range.1);
            }
        }
        Ok(())
    }
}

impl Arm64AssemblyEmitter {

    // Helper to collect variables, constants, and flags
    fn collect_data_info(ir: &[String]) -> (std::collections::HashSet<String>, std::collections::HashMap<String, String>, bool, bool) {
        let mut variables = std::collections::HashSet::new();
        let mut constants = std::collections::HashMap::new();
        let mut needs_write_int = false;
        let mut needs_read_int = false;
        for line in ir {
            let line = line.trim();
            if line.starts_with("var ") {
                let vars = line[4..]
                    .split(',')
                    .map(|v| v.trim())
                    .filter(|v| !v.is_empty());
                for v in vars {
                    variables.insert(v.to_string());
                }
            } else if line.starts_with("const ") {
                if let Some((name, value)) = line[6..].split_once('=') {
                    constants.insert(name.trim().to_string(), value.trim().to_string());
                }
            } else if line.contains("write_int") {
                needs_write_int = true;
            } else if line.contains("read_int") {
                needs_read_int = true;
            }
        }
        (variables, constants, needs_write_int, needs_read_int)
    }

    // Helper to emit data section
    fn emit_data_section(data_output: &mut String, variables: &std::collections::HashSet<String>, constants: &std::collections::HashMap<String, String>) -> std::io::Result<()> {
        for (name, value) in constants {
            write_line(data_output, format_args!(".equ {}, {}\n", name, value))?;
        }
        write_line(data_output, format_args!(".section __DATA,__bss\n"))?;
        write_line(data_output, format_args!(".align 3\n"))?;
        for v in variables {
            write_line(data_output, format_args!("{}:\n", v))?;
            write_line(data_output, format_args!("    .skip 8\n"))?;
        }
        Ok(())
    }

    // Helper to process IR lines
    #[allow(clippy::too_many_arguments)]
    fn process_ir_lines(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, proc_output: &mut String, main_output: &mut String) -> std::io::Result<usize> {
        let mut main_stack_size: usize = 0;
        let mut main_proc_enter_idx: Option<usize> = None;
        let mut proc_stack_sizes: Vec<usize> = Vec::new();
        let mut in_proc = false;
        let mut i = 0;
        while i < ir.len() {
            if let Some(alloc) = allocator
                .as_any_mut()
                .downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>() {
                alloc.set_instruction_index(i as i32);
            }
            let line = ir[i].trim();
            if line.is_empty()
                || line.starts_with('#')
                || line.starts_with("var ")
                || line.starts_with("const ")
            {
                i += 1;
                continue;
            }
            if line.ends_with(':') {
                // Look ahead for proc_enter
                let mut j = i + 1;
                let mut is_proc = false;
                while j < ir.len() {
                    let next_line = ir[j].trim();
                    if next_line.is_empty() || next_line.starts_with('#') {
                        j += 1;
                        continue;
                    }
                    if next_line.starts_with("proc_enter") {
                        is_proc = true;
                    }
                    break;
                }
                if line == "main:" {
                    in_proc = false;
                    i += 1;
                    continue;
                } else if is_proc {
                    in_proc = true;
                }
                if in_proc {
                    write_line(proc_output, format_args!("{}\n", line))?;
                } else {
                    write_line(main_output, format_args!("{}\n", line))?;
                }
                i += 1;
                continue;
            }
            let mut parts = line.split_whitespace();
            let op_str = parts.next().unwrap_or("");
            let rest: Vec<&str> = parts.collect();
            let op = IROp::from_str(op_str);
            if !in_proc && op == IROp::ProcEnter {
                // This is a proc_enter for main
                main_proc_enter_idx = Some(i);
                main_stack_size = rest.get(0).and_then(|s| s.parse::<usize>().ok()).unwrap_or(0);
                i += 1;
                continue;
            }
            if in_proc {
                self.emit_ir_instruction_arm64_with_stack(op, op_str, &rest, i, allocator,
                    proc_output, &mut in_proc, &mut None, &mut Vec::new(), &mut proc_stack_sizes, line,
                )?;
            } else {
                self.emit_ir_instruction_arm64_with_stack(op, op_str, &rest, i, allocator,
                    main_output, &mut in_proc, &mut None, &mut Vec::new(), &mut proc_stack_sizes, line,
                )?;
            }
            i += 1;
        }
        Ok(main_stack_size)
    }

    // Helper to emit main section
    fn emit_main_section(main_output: &str, allocator: &mut dyn RegisterAllocator, main_stack_size: usize) -> std::io::Result<String> {
        let mut new_main_output = String::new();
        write_line(&mut new_main_output, format_args!(".global main\n"))?;
        write_line(&mut new_main_output, format_args!("main:\n"))?;
        Self::emit_prologue(&mut new_main_output, allocator)?;
        if main_stack_size > 0 {
            write_line(&mut new_main_output, format_args!("    sub sp, sp, #{}\n", main_stack_size))?;
        }
        new_main_output.push_str(main_output);
        // Only emit stack deallocation and epilogue if main_output does NOT already end with an exit syscall
        let trimmed = main_output.trim_end();
        let ends_with_exit = trimmed.ends_with("svc #0") || trimmed.ends_with("svc #0\n");
        if !main_output.is_empty() && !ends_with_exit {
            if main_stack_size > 0 {
                write_line(&mut new_main_output, format_args!("    add sp, sp, #{}\n", main_stack_size))?;
            }
            Self::emit_epilogue(&mut new_main_output, allocator)?;
        }
        Ok(new_main_output)
    }

    // Helper to assemble final output
    fn assemble_final_output(data_output: &str, new_main_output: &str, proc_output: &str, footer: &str) -> std::io::Result<String> {
        let mut final_output = String::new();
        if !data_output.is_empty() {
            write_line(&mut final_output, format_args!("{}", data_output))?;
            write_line(&mut final_output, format_args!("\n"))?;
        }
        write_line(&mut final_output, format_args!(".section __TEXT,__text\n"))?;
        write_line(&mut final_output, format_args!("{}", new_main_output))?;
        write_line(&mut final_output, format_args!("{}", proc_output))?;
        write_line(&mut final_output, format_args!("{}", footer))?;
        Ok(final_output)
    }

    fn emit_prologue(output: &mut String, allocator: &mut dyn RegisterAllocator) -> Result<(), io::Error> {
        write_line(output, format_args!("    stp x29, x30, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    mov x29, sp\n"))?;
        // Save used callee-saved registers
        if let Some(alloc) = allocator
            .as_any_mut()
            .downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>() {
            let mut regs: Vec<_> = alloc.get_used_callee_saved().iter().copied().collect();
            regs.sort();
            for reg in regs {
                write_line(output, format_args!("    str x{}, [sp, #-16]!\n", reg))?;
            }
        }
        Ok(())
    }

    fn emit_epilogue(output: &mut String, allocator: &mut dyn RegisterAllocator) -> Result<(), io::Error> {
        // Restore used callee-saved registers in reverse order
        if let Some(alloc) = allocator
            .as_any_mut()
            .downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>() {
            let mut regs: Vec<_> = alloc.get_used_callee_saved().iter().copied().collect();
            regs.sort_by(|a, b| b.cmp(a));
            for reg in regs {
                write_line(output, format_args!("    ldr x{}, [sp], #16\n", reg))?;
            }
        }
        write_line(output, format_args!("    ldp x29, x30, [sp], #16\n"))?;
        write_line(output, format_args!("    ret\n"))?;
        Ok(())
    }

    fn emit_write_int_arm64(
        &self,
        src: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let psrc = allocator
            .ensure(src, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        if psrc != 0 {
            write_line(output, format_args!("    mov x0, x{}\n", psrc))?;
        }
        write_line(output, format_args!("    bl _write_int\n"))?;
        if self.is_dead_after(src, idx, allocator) {
            allocator.free(psrc);
        }
        Ok(())
    }

    fn emit_read_int_arm64(
        &self,
        dst: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        write_line(output, format_args!("    bl _read_int\n"))?;
        write_line(output, format_args!("    mov x{}, x0\n", pdst))?;
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_footer(
        &self,
        output: &mut String,
        needs_write_int: bool,
        needs_read_int: bool,
    ) -> Result<(), io::Error> {
        write_line(output, format_args!(".section __TEXT,__text\n"))?;
        write_line(output, format_args!(".global _start\n"))?;
        write_line(output, format_args!("_start:\n"))?;
        write_line(output, format_args!("    bl main\n"))?;
        write_line(output, format_args!("    b .\n"))?;
        if needs_write_int {
            Self::emit_write_int_arm64_macos_str(output)?;
        }
        if needs_read_int {
            Self::emit_read_int_arm64_macos_str(output)?;
        }
        Ok(())
    }

    fn emit_write_int_arm64_macos_str(output: &mut String) -> Result<(), io::Error> {
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
        write_line(output, format_args!("    adrp x1, int_buffer@PAGE\n"))?;
        write_line(output, format_args!("    add x1, x1, int_buffer@PAGEOFF\n"))?;
        write_line(output, format_args!("    add x1, x1, #23\n"))?;
        write_line(output, format_args!("    mov x2, #0\n"))?;
        write_line(output, format_args!("    mov x19, x0\n"))?;
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
        write_line(output, format_args!("    strb w5, [x1]\n"))?;
        write_line(output, format_args!("    sub x1, x1, #1\n"))?;
        write_line(output, format_args!("    add x2, x2, #1\n"))?;
        write_line(output, format_args!("    mov x19, x4\n"))?;
        write_line(output, format_args!("    cbnz x19, 3b\n"))?;
        write_line(output, format_args!("    cmp w20, #0\n"))?;
        write_line(output, format_args!("    beq 4f\n"))?;
        write_line(output, format_args!("    mov w5, #'-'\n"))?;
        write_line(output, format_args!("    strb w5, [x1]\n"))?;
        write_line(output, format_args!("    sub x1, x1, #1\n"))?;
        write_line(output, format_args!("    add x2, x2, #1\n"))?;
        write_line(output, format_args!("4:\n"))?;
        write_line(output, format_args!("    mov w5, #'\\n'\n"))?;
        write_line(output, format_args!("    strb w5, [x1]\n"))?;
        write_line(output, format_args!("    sub x1, x1, #1\n"))?;
        write_line(output, format_args!("    add x2, x2, #1\n"))?;
        write_line(output, format_args!("    add x1, x1, #1\n"))?;
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
        write_line(output, format_args!("    ldp x29, x30, [sp], #16\n"))?;
        write_line(output, format_args!("    ret\n"))?;
        Ok(())
    }

    fn emit_read_int_arm64_macos_str(output: &mut String) -> Result<(), io::Error> {
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
        write_line(output, format_args!("    adrp x1, input_buffer@PAGE\n"))?;
        write_line(
            output,
            format_args!("    add x1, x1, input_buffer@PAGEOFF\n"),
        )?;
        write_line(output, format_args!("    mov x0, #0\n"))?;
        write_line(output, format_args!("    mov x2, #24\n"))?;
        write_line(
            output,
            format_args!("    movz x16, #(0x2000003 & 0xFFFF)\n"),
        )?;
        write_line(
            output,
            format_args!("    movk x16, #((0x2000003 >> 16) & 0xFFFF), lsl #16\n"),
        )?;
        write_line(output, format_args!("    svc #0\n"))?;
        write_line(output, format_args!("    mov x19, x0\n"))?;
        write_line(output, format_args!("    mov x0, #0\n"))?;
        write_line(output, format_args!("    mov x20, #0\n"))?;
        write_line(output, format_args!("    mov x21, #0\n"))?;
        write_line(output, format_args!("    adrp x1, input_buffer@PAGE\n"))?;
        write_line(
            output,
            format_args!("    add x1, x1, input_buffer@PAGEOFF\n"),
        )?;
        write_line(output, format_args!("    ldrb w2, [x1]\n"))?;
        write_line(output, format_args!("    cmp w2, #'-'\n"))?;
        write_line(output, format_args!("    b.ne 1f\n"))?;
        write_line(output, format_args!("    mov x21, #1\n"))?;
        write_line(output, format_args!("    add x1, x1, #1\n"))?;
        write_line(output, format_args!("    sub x19, x19, #1\n"))?;
        write_line(output, format_args!("    mov x20, #1\n"))?;
        write_line(output, format_args!("1:\n"))?;
        write_line(output, format_args!("2:\n"))?;
        write_line(output, format_args!("    cmp x20, x19\n"))?;
        write_line(output, format_args!("    b.ge 3f\n"))?;
        write_line(output, format_args!("    ldrb w2, [x1, x20]\n"))?;
        write_line(output, format_args!("    sub w2, w2, #'0'\n"))?;
        write_line(output, format_args!("    cmp w2, #0\n"))?;
        write_line(output, format_args!("    b.lt 3f\n"))?;
        write_line(output, format_args!("    cmp w2, #9\n"))?;
        write_line(output, format_args!("    b.gt 3f\n"))?;
        write_line(output, format_args!("    mov x3, #10\n"))?;
        write_line(output, format_args!("    mul x0, x0, x3\n"))?;
        write_line(output, format_args!("    add x0, x0, x2\n"))?;
        write_line(output, format_args!("    add x20, x20, #1\n"))?;
        write_line(output, format_args!("    b 2b\n"))?;
        write_line(output, format_args!("3:\n"))?;
        write_line(output, format_args!("    cmp x21, #1\n"))?;
        write_line(output, format_args!("    b.ne 4f\n"))?;
        write_line(output, format_args!("    neg x0, x0\n"))?;
        write_line(output, format_args!("4:\n"))?;
        write_line(output, format_args!("    ldr x21, [sp], #16\n"))?;
        write_line(output, format_args!("    ldp x19, x20, [sp], #16\n"))?;
        write_line(output, format_args!("    ldp x29, x30, [sp], #16\n"))?;
        write_line(output, format_args!("    ret\n"))?;
        Ok(())
    }

    fn is_dead_after(&self, vreg: &str, idx: usize, allocator: &mut dyn RegisterAllocator) -> bool {
        if let Some(reg_any) = allocator.get_vreg(vreg) {
            if let Some(reg) = reg_any.downcast_ref::<Register<RegisterName>>() {
                reg.next_uses.iter().all(|&use_idx| use_idx <= idx as i32)
            } else {
                true
            }
        } else {
            true
        }
    }

    fn emit_var_addr(&self, output: &mut String, reg: u8, var: &str) -> Result<(), io::Error> {
        write_line(output, format_args!("    adrp x{}, {}@PAGE\n", reg, var))?;
        write_line(
            output,
            format_args!("    add x{}, x{}, {}@PAGEOFF\n", reg, reg, var),
        )?;
        Ok(())
    }

    fn emit_li(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let imm = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        // Try to parse as u64 (for large immediates)
        if let Ok(imm_val) = imm.parse::<u64>() {
            if imm_val <= 0xFFFF {
                write_line(output, format_args!("    mov x{}, #{}\n", pdst, imm_val))?;
            } else {
                write_line(output, format_args!("    movz x{}, #{}\n", pdst, imm_val & 0xFFFF))?;
                if (imm_val >> 16) & 0xFFFF != 0 {
                    write_line(output, format_args!("    movk x{}, #{}, lsl #16\n", pdst, (imm_val >> 16) & 0xFFFF))?;
                }
                if (imm_val >> 32) & 0xFFFF != 0 {
                    write_line(output, format_args!("    movk x{}, #{}, lsl #32\n", pdst, (imm_val >> 32) & 0xFFFF))?;
                }
                if (imm_val >> 48) & 0xFFFF != 0 {
                    write_line(output, format_args!("    movk x{}, #{}, lsl #48\n", pdst, (imm_val >> 48) & 0xFFFF))?;
                }
            }
        } else if let Ok(imm_val) = imm.parse::<i64>() {
            // For negative immediates, use mov/mvn or movz/movk with sign extension
            if imm_val >= 0 && imm_val <= 0xFFFF {
                write_line(output, format_args!("    mov x{}, #{}\n", pdst, imm_val))?;
            } else {
                // Fallback: use movz/movk for the unsigned representation
                let imm_val = imm_val as u64;
                write_line(output, format_args!("    movz x{}, #{}\n", pdst, imm_val & 0xFFFF))?;
                if (imm_val >> 16) & 0xFFFF != 0 {
                    write_line(output, format_args!("    movk x{}, #{}, lsl #16\n", pdst, (imm_val >> 16) & 0xFFFF))?;
                }
                if (imm_val >> 32) & 0xFFFF != 0 {
                    write_line(output, format_args!("    movk x{}, #{}, lsl #32\n", pdst, (imm_val >> 32) & 0xFFFF))?;
                }
                if (imm_val >> 48) & 0xFFFF != 0 {
                    write_line(output, format_args!("    movk x{}, #{}, lsl #48\n", pdst, (imm_val >> 48) & 0xFFFF))?;
                }
            }
        } else {
            write_line(output, format_args!("    mov x{}, #{}\n", pdst, imm))?;
        }
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_ld(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"");
        let src = src.trim().replace(['[', ']', ','], "");
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        // Check if this is a stack offset access [bp-offset]
        if src.starts_with("bp-") {
            let offset = src[3..].parse::<i32>().unwrap_or(0);
            write_line(
                output,
                format_args!("    ldr x{}, [x29, #{}]\n", pdst, -offset),
            )?;
        } else {
            // Global variable access
            self.emit_var_addr(output, 12, &src)?;
            write_line(output, format_args!("    ldr x{}, [x12]\n", pdst))?;
        }
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_st(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let dst = rest.get(0).unwrap_or(&"");
        let dst = dst.trim().replace(['[', ']', ','], "");
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator
            .ensure(src, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        // Check if this is a stack offset access [bp-offset]
        if dst.starts_with("bp-") {
            let offset = dst[3..].parse::<i32>().unwrap_or(0);
            write_line(
                output,
                format_args!("    str x{}, [x29, #{}]\n", psrc, -offset),
            )?;
        } else {
            // Global variable access
            self.emit_var_addr(output, 12, &dst)?;
            write_line(output, format_args!("    str x{}, [x12]\n", psrc))?;
        }
        if self.is_dead_after(src, idx, allocator) {
            allocator.free(psrc);
        }
        Ok(())
    }

    fn emit_binop(
        &self,
        op: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        // Handle immediate-immediate case
        if let (Ok(imm1), Ok(imm2)) = (src1.parse::<i64>(), src2.parse::<i64>()) {
            let result = match op {
                "add" => imm1.wrapping_add(imm2),
                "sub" => imm1.wrapping_sub(imm2),
                "mul" => imm1.wrapping_mul(imm2),
                "sdiv" => if imm2 != 0 { imm1.wrapping_div(imm2) } else { return Err(io::Error::new(io::ErrorKind::Other, "Division by zero")) },
                _ => return Err(io::Error::new(io::ErrorKind::Other, format!("Operation {} not supported", op))),
            };
            let pdst = allocator.alloc(dst, output)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
            if pdst == 12 {
                return Err(io::Error::new(io::ErrorKind::Other, "x12 cannot be used for computation"));
            }
            write_line(output, format_args!("    mov x{}, #{}\n", pdst, result))?;
            if self.is_dead_after(dst, idx, allocator) {
                allocator.free(pdst);
            }
            return Ok(());
        }
    
        let psrc1 = allocator.ensure(src1, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        if psrc1 == 12 {
            return Err(io::Error::new(io::ErrorKind::Other, "x12 cannot be used for computation"));
        }
    
        // Reuse psrc1 for dst if temporary and dead
        let pdst = if dst == src1 || (self.is_dead_after(dst, idx, allocator) && !allocator.get_vreg(dst).map_or(false, |r| r.downcast_ref::<Register<RegisterName>>().map_or(false, |reg| reg.live_across_call))) {
            psrc1
        } else {
            allocator.alloc(dst, output)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?
        };
        if pdst == 12 {
            return Err(io::Error::new(io::ErrorKind::Other, "x12 cannot be used for computation"));
        }
    
        if let Ok(imm) = src2.parse::<i64>() {
            match op {
                "add" | "sub" => {
                    if imm >= -512 && imm <= 511 {
                        write_line(output, format_args!("    {} x{}, x{}, #{}\n", op, pdst, psrc1, imm))?;
                    } else {
                        let temp_reg = if pdst != 1 && psrc1 != 1 { 1 } else { 2 };
                        if temp_reg == 12 {
                            return Err(io::Error::new(io::ErrorKind::Other, "x12 cannot be used for computation"));
                        }
                        write_line(output, format_args!("    mov x{}, #{}\n", temp_reg, imm))?;
                        write_line(output, format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, temp_reg))?;
                    }
                }
                "sdiv" | "mul" => {
                    let temp_reg = if pdst != 1 && psrc1 != 1 { 1 } else { 2 };
                    if temp_reg == 12 {
                        return Err(io::Error::new(io::ErrorKind::Other, "x12 cannot be used for computation"));
                    }
                    write_line(output, format_args!("    mov x{}, #{}\n", temp_reg, imm))?;
                    write_line(output, format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, temp_reg))?;
                }
                _ => {
                    return Err(io::Error::new(io::ErrorKind::Other, format!("Operation {} with immediate not supported", op)));
                }
            }
        } else {
            let psrc2 = allocator.ensure(src2, output)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
            if psrc2 == 12 {
                return Err(io::Error::new(io::ErrorKind::Other, "x12 cannot be used for computation"));
            }
            write_line(output, format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, psrc2))?;
            if self.is_dead_after(src2, idx, allocator) {
                allocator.free(psrc2);
            }
        }
    
        if self.is_dead_after(src1, idx, allocator) && dst != src1 {
            allocator.free(psrc1);
        }
        if self.is_dead_after(dst, idx, allocator) && pdst != psrc1 {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_relational(
        &self,
        op: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        let psrc1 = allocator
            .ensure(src1, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let psrc2 = allocator
            .ensure(src2, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        // Never use x12 for computation
        if psrc1 == 12 || psrc2 == 12 || pdst == 12 {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "x12 cannot be used for computation",
            ));
        }
        write_line(output, format_args!("    cmp x{}, x{}\n", psrc1, psrc2))?;
        match op {
            "cmp_eq" => write_line(output, format_args!("    cset x{}, eq\n", pdst))?,
            "cmp_ne" => write_line(output, format_args!("    cset x{}, ne\n", pdst))?,
            "cmp_gt" => write_line(output, format_args!("    cset x{}, gt\n", pdst))?,
            "cmp_lt" => write_line(output, format_args!("    cset x{}, lt\n", pdst))?,
            "cmp_ge" => write_line(output, format_args!("    cset x{}, ge\n", pdst))?,
            "cmp_le" => write_line(output, format_args!("    cset x{}, le\n", pdst))?,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "invalid relational op",
                ))
            }
        }
        if self.is_dead_after(src1, idx, allocator) {
            allocator.free(psrc1);
        }
        if self.is_dead_after(src2, idx, allocator) {
            allocator.free(psrc2);
        }
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_is_odd(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator
            .ensure(src, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        write_line(output, format_args!("    and x{}, x{}, #1\n", pdst, psrc))?;
        if self.is_dead_after(src, idx, allocator) {
            allocator.free(psrc);
        }
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_jump(
        &self,
        rest: &[&str],
        _idx: usize,
        _allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        write_line(output, format_args!("    b {}\n", label))?;
        Ok(())
    }

    fn emit_beqz(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let label = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator
            .ensure(src, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        write_line(output, format_args!("    cbz x{}, {}\n", psrc, label))?;
        if self.is_dead_after(src, idx, allocator) {
            allocator.free(psrc);
        }
        Ok(())
    }

    fn emit_call(
        &self,
        rest: &[&str],
        _idx: usize,
        _allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        write_line(output, format_args!("    bl {}\n", label))?;
        Ok(())
    }

    fn emit_exit(&self, output: &mut String) -> Result<(), io::Error> {
        write_line(output, format_args!("    mov x0, #0\n"))?;
        write_line(
            output,
            format_args!("    movz x16, #(0x2000001 & 0xFFFF)\n"),
        )?;
        write_line(
            output,
            format_args!("    movk x16, #((0x2000001 >> 16) & 0xFFFF), lsl #16\n"),
        )?;
        write_line(output, format_args!("    svc #0\n"))?;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_ir_instruction_arm64(
        &self,
        op: IROp,
        op_str: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        target_output: &mut String,
        in_proc: &mut bool,
        current_proc: &mut Option<String>,
        proc_stack: &mut Vec<String>,
        line: &str,
    ) -> Result<(), io::Error> {
        match op {
            IROp::ProcEnter => {
                *in_proc = true;
                Arm64AssemblyEmitter::emit_prologue(target_output, allocator)?;
            }
            IROp::ProcExit => {
                Arm64AssemblyEmitter::emit_epilogue(target_output, allocator)?;
                proc_stack.pop();
                if proc_stack.is_empty() {
                    *in_proc = false;
                    *current_proc = None;
                }
            }
            IROp::Exit => {
                self.emit_exit(target_output)?;
            }
            IROp::Li => self.emit_li(rest, idx, allocator, target_output)?,
            IROp::Ld => self.emit_ld(rest, idx, allocator, target_output)?,
            IROp::St => self.emit_st(rest, idx, allocator, target_output)?,
            IROp::Add | IROp::Sub | IROp::Mul => {
                self.emit_binop(op_str, rest, idx, allocator, target_output)?
            }
            IROp::Div => self.emit_binop("sdiv", rest, idx, allocator, target_output)?,
            IROp::Mod => self.emit_mod(rest, idx, allocator, target_output)?,
            IROp::CmpGt | IROp::CmpLt | IROp::CmpLe | IROp::CmpGe | IROp::CmpEq | IROp::CmpNe => {
                self.emit_relational(op_str, rest, idx, allocator, target_output)?
            }
            IROp::IsOdd => self.emit_is_odd(rest, idx, allocator, target_output)?,
            IROp::Beqz => self.emit_beqz(rest, idx, allocator, target_output)?,
            IROp::Jump => self.emit_jump(rest, idx, allocator, target_output)?,
            IROp::Call => self.emit_call(rest, idx, allocator, target_output)?,
            IROp::WriteInt => {
                let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                self.emit_write_int_arm64(src, idx, allocator, target_output)?;
            }
            IROp::ReadInt => {
                let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                self.emit_read_int_arm64(dst, idx, allocator, target_output)?;
            }
            IROp::WriteStr => {},
            IROp::Unknown => write_line(target_output, format_args!("// Unhandled IR: {}\n", line))?,
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_ir_instruction_arm64_with_stack(
        &self,
        op: IROp,
        op_str: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        target_output: &mut String,
        in_proc: &mut bool,
        current_proc: &mut Option<String>,
        proc_stack: &mut Vec<String>,
        proc_stack_sizes: &mut Vec<usize>,
        line: &str,
    ) -> Result<(), io::Error> {
        match op {
            IROp::ProcEnter => {
                *in_proc = true;
                let stack_size = rest.get(0).and_then(|s| s.parse::<usize>().ok()).unwrap_or(0);
                proc_stack_sizes.push(stack_size);
                Arm64AssemblyEmitter::emit_prologue(target_output, allocator)?;
                if stack_size > 0 {
                    write_line(target_output, format_args!("    sub sp, sp, #{}\n", stack_size))?;
                }
            }
            IROp::ProcExit => {
                let stack_size = proc_stack_sizes.pop().unwrap_or(0);
                if stack_size > 0 {
                    write_line(target_output, format_args!("    add sp, sp, #{}\n", stack_size))?;
                }
                Arm64AssemblyEmitter::emit_epilogue(target_output, allocator)?;
                proc_stack.pop();
                if proc_stack.is_empty() {
                    *in_proc = false;
                    *current_proc = None;
                }
            }
            _ => self.emit_ir_instruction_arm64(op, op_str, rest, idx, allocator, target_output, in_proc, current_proc, proc_stack, line)?,
        }
        Ok(())
    }

    fn emit_mod(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');

        let psrc1 = allocator.ensure(src1, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let psrc2 = allocator.ensure(src2, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let pdst = allocator.alloc(dst, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;

        // Use a temp register for the quotient (let's use x11, but ensure it's not used for vregs)
        let temp = 11usize;
        if psrc1 == temp || psrc2 == temp || pdst == temp {
            return Err(io::Error::new(io::ErrorKind::Other, "x11 cannot be used for computation"));
        }

        // sdiv x11, x<src1>, x<src2>
        write_line(output, format_args!("    sdiv x{}, x{}, x{}\n", temp, psrc1, psrc2))?;
        // msub x<dst>, x11, x<src2>, x<src1>  ; x<dst> = x<src1> - (x11 * x<src2>)
        write_line(output, format_args!("    msub x{}, x{}, x{}, x{}\n", pdst, temp, psrc2, psrc1))?;

        if self.is_dead_after(src1, idx, allocator) {
            allocator.free(psrc1);
        }
        if self.is_dead_after(src2, idx, allocator) {
            allocator.free(psrc2);
        }
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }
}
