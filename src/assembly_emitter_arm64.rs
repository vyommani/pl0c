use crate::{assembly_generator::{AssemblyEmitter, RegisterAllocator}, register_allocator_arm64::RegisterName, register_allocator_common::Register};
use std::{collections::HashMap, io::{self, Write}};
use regex::Regex;
use std::{collections::HashSet, fmt::Write as FmtWrite};

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
        output: &mut dyn std::io::Write,
    ) -> Result<(), io::Error> {
        let mut variables = HashSet::new();
        let mut constants = HashMap::new();
        let mut in_proc = false;
        let mut needs_write_int = false;
        let mut needs_read_int = false;
        let mut data_output = String::new();
        let mut proc_output = String::new();
        let mut main_output = String::new();

        // Step 1: Collect variables, constants, proc_output, and main_output
        for line in ir {
            let line = line.trim();
            if line.starts_with("var ") {
                let vars = line[4..].split(',').map(|v| v.trim()).filter(|v| !v.is_empty());
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

        // Emit constants to data_output
        for (name, value) in &constants {
            write_line(&mut data_output, format_args!(".equ {}, {}\n", name, value))?;
        }

        // Emit variables to BSS section
        write_line(&mut data_output, format_args!(".section __DATA,__bss\n"))?;
        write_line(&mut data_output, format_args!(".align 3\n"))?;
        for v in &variables {
            write_line(&mut data_output, format_args!("{}:\n", v))?;
            write_line(&mut data_output, format_args!("    .skip 8\n"))?;
        }

        // Process IR lines for instructions
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with("var ") || line.starts_with("const ") {
                continue;
            }

            if line.ends_with(':') {
                let label = &line[..line.len()-1];
                // Skip emitting 'main:' label to avoid duplication
                if label == "main" {
                    continue;
                }
                let next_line = ir.get(idx + 1).map(|s| s.trim()).unwrap_or("");
                if next_line.starts_with("proc_enter") {
                    in_proc = true;
                    write_line(&mut proc_output, format_args!("{}:\n", label))?;
                } else {
                    let target_output = if in_proc { &mut proc_output } else { &mut main_output };
                    write_line(target_output, format_args!("{}:\n", label))?;
                }
                continue;
            }

            let target_output = if in_proc { &mut proc_output } else { &mut main_output };
            let mut parts = line.split_whitespace();
            let op = parts.next().unwrap_or("");
            let rest: Vec<&str> = parts.collect();

            match op {
                "proc_enter" => {
                    in_proc = true;
                    Arm64AssemblyEmitter::emit_prologue(target_output)?;
                }
                "proc_exit" => {
                    in_proc = false;
                    Arm64AssemblyEmitter::emit_epilogue(target_output)?;
                }
                "exit" => {
                    self.emit_exit(target_output)?;
                }
                "li" => self.emit_li(&rest, idx, allocator, target_output)?,
                "ld" => self.emit_ld(&rest, idx, allocator, target_output)?,
                "st" => self.emit_st(&rest, idx, allocator, target_output)?,
                "add" | "sub" | "mul" => self.emit_binop(op, &rest, idx, allocator, target_output)?,
                "div" => self.emit_binop("sdiv", &rest, idx, allocator, target_output)?,
                "cmp_gt" | "cmp_lt" | "cmp_le" | "cmp_ge" | "cmp_eq" | "cmp_ne" => {
                    self.emit_relational(op, &rest, idx, allocator, target_output)?
                }
                "is_odd" => self.emit_is_odd(&rest, idx, allocator, target_output)?,
                "beqz" => self.emit_beqz(&rest, idx, allocator, target_output)?,
                "jump" => self.emit_jump(&rest, idx, allocator, target_output)?,
                "call" => self.emit_call(&rest, idx, allocator, target_output)?,
                "write_int" => {
                    let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                    self.emit_write_int_arm64(src, idx, allocator, target_output)?;
                }
                "read_int" => {
                    let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                    self.emit_read_int_arm64(dst, idx, allocator, target_output)?;
                }
                _ => write_line(target_output, format_args!("// Unhandled IR: {}\n", line))?,
            }
        }

        // Step 2: Prepend .global main and prologue to main_output
        let mut new_main_output = String::new();
        write_line(&mut new_main_output, format_args!(".global main\n"))?;
        write_line(&mut new_main_output, format_args!("main:\n"))?;
        Arm64AssemblyEmitter::emit_prologue(&mut new_main_output)?;
        new_main_output.push_str(&main_output);
        // Append epilogue if main_output contains instructions
        if !main_output.is_empty() {
            Arm64AssemblyEmitter::emit_epilogue(&mut new_main_output)?;
        }

        // Step 3: Assemble final output
        let mut final_output = String::new();
        if !data_output.is_empty() {
            write_line(&mut final_output, format_args!("{}", data_output))?;
            write_line(&mut final_output, format_args!("\n"))?;
        }

        write_line(&mut final_output, format_args!(".section __TEXT,__text\n"))?;
        write_line(&mut final_output, format_args!("{}", new_main_output))?;
        write_line(&mut final_output, format_args!("{}", proc_output))?;

        let mut footer = String::new();
        self.emit_footer(&mut footer, needs_write_int, needs_read_int)?;
        write_line(&mut final_output, format_args!("{}", footer))?;

        output.write_all(final_output.as_bytes())?;

        Ok(())
    }

    fn analyze_register_usage(&self, ir: &[String], allocator: &mut dyn RegisterAllocator) -> Result<(), crate::register_allocator_common::RegisterError> {
        let mut vreg_uses: HashMap<String, Vec<usize>> = HashMap::new();
        let vreg_regex = Regex::new(r"v[0-9]+\b").unwrap();
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            for cap in vreg_regex.captures_iter(line) {
                let vreg = cap.get(0).unwrap().as_str();
                vreg_uses.entry(vreg.to_string()).or_default().push(idx);
            }
        }
        for (i, (vreg, uses)) in vreg_uses.into_iter().enumerate() {
            let reg = Register::new(usize::MAX, i, RegisterName::SP, uses.into_iter().map(|u| u as i32).collect(), 0);
            if let Some(alloc) = allocator.as_any_mut().downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>() {
                alloc.vreg_map.insert(vreg, reg);
            }
        }
        Ok(())
    }
}

impl Arm64AssemblyEmitter {
    fn free_if_dead(
        &self,
        vreg: &str,
        idx: usize,
        preg: usize,
        allocator: &mut dyn RegisterAllocator,
    ) {
        if self.is_dead_after(vreg, idx, allocator) {
            allocator.free(preg);
        }
    }

    fn emit_prologue(output: &mut String) -> Result<(), io::Error> {
        write_line(output, format_args!("    stp x29, x30, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    mov x29, sp\n"))?;
        Ok(())
    }

    fn emit_epilogue(output: &mut String) -> Result<(), io::Error> {
        write_line(output, format_args!("    ldp x29, x30, [sp], #16\n"))?;
        write_line(output, format_args!("    ret\n"))?;
        Ok(())
    }

    fn emit_write_int_arm64(&self, src: &str, idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Result<(), io::Error> {
        let psrc = allocator.ensure(src).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        if psrc != 0 {
            write_line(output, format_args!("    mov x0, x{}\n", psrc))?;
        }
        write_line(output, format_args!("    bl _write_int\n"))?;
        if self.is_dead_after(src, idx, allocator) {
            allocator.free(psrc);
        }
        Ok(())
    }

    fn emit_read_int_arm64(&self, dst: &str, idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Result<(), io::Error> {
        let pdst = allocator.alloc(dst).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        write_line(output, format_args!("    bl _read_int\n"))?;
        write_line(output, format_args!("    mov x{}, x0\n", pdst))?;
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_footer(&self, output: &mut String, needs_write_int: bool, needs_read_int: bool) -> Result<(), io::Error> {
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
        write_line(output, format_args!("    movz x16, #(0x2000004 & 0xFFFF)\n"))?;
        write_line(output, format_args!("    movk x16, #((0x2000004 >> 16) & 0xFFFF), lsl #16\n"))?;
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
        write_line(output, format_args!("    add x1, x1, input_buffer@PAGEOFF\n"))?;
        write_line(output, format_args!("    mov x0, #0\n"))?;
        write_line(output, format_args!("    mov x2, #24\n"))?;
        write_line(output, format_args!("    movz x16, #(0x2000003 & 0xFFFF)\n"))?;
        write_line(output, format_args!("    movk x16, #((0x2000003 >> 16) & 0xFFFF), lsl #16\n"))?;
        write_line(output, format_args!("    svc #0\n"))?;
        write_line(output, format_args!("    mov x19, x0\n"))?;
        write_line(output, format_args!("    mov x0, #0\n"))?;
        write_line(output, format_args!("    mov x20, #0\n"))?;
        write_line(output, format_args!("    mov x21, #0\n"))?;
        write_line(output, format_args!("    adrp x1, input_buffer@PAGE\n"))?;
        write_line(output, format_args!("    add x1, x1, input_buffer@PAGEOFF\n"))?;
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
        write_line(output, format_args!("    add x{}, x{}, {}@PAGEOFF\n", reg, reg, var))?;
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
        let pdst = allocator.alloc(dst).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        write_line(output, format_args!("    mov x{}, #{}\n", pdst, imm))?;
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
        let pdst = allocator.alloc(dst).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        // Always use x12 for address calculation only
        self.emit_var_addr(output, 12, &src)?;
        write_line(output, format_args!("    ldr x{}, [x12]\n", pdst))?;
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
        let psrc = allocator.ensure(src).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        // Always use x12 for address calculation only
        self.emit_var_addr(output, 12, &dst)?;
        write_line(output, format_args!("    str x{}, [x12]\n", psrc))?;
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
        let psrc1 = allocator.ensure(src1).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let psrc2 = allocator.ensure(src2).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let pdst = allocator.alloc(dst).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        // Never use x12 for computation
        if psrc1 == 12 || psrc2 == 12 || pdst == 12 {
            return Err(io::Error::new(io::ErrorKind::Other, "x12 cannot be used for computation"));
        }
        write_line(output, format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, psrc2))?;
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
        let psrc1 = allocator.ensure(src1).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let psrc2 = allocator.ensure(src2).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let pdst = allocator.alloc(dst).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        // Never use x12 for computation
        if psrc1 == 12 || psrc2 == 12 || pdst == 12 {
            return Err(io::Error::new(io::ErrorKind::Other, "x12 cannot be used for computation"));
        }
        write_line(output, format_args!("    cmp x{}, x{}\n", psrc1, psrc2))?;
        match op {
            "cmp_eq" => write_line(output, format_args!("    cset x{}, eq\n", pdst))?,
            "cmp_ne" => write_line(output, format_args!("    cset x{}, ne\n", pdst))?,
            "cmp_gt" => write_line(output, format_args!("    cset x{}, gt\n", pdst))?,
            "cmp_lt" => write_line(output, format_args!("    cset x{}, lt\n", pdst))?,
            "cmp_ge" => write_line(output, format_args!("    cset x{}, ge\n", pdst))?,
            "cmp_le" => write_line(output, format_args!("    cset x{}, le\n", pdst))?,
            _ => return Err(io::Error::new(io::ErrorKind::Other, "invalid relational op")),
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
        let psrc = allocator.ensure(src).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let pdst = allocator.alloc(dst).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
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
        let psrc = allocator.ensure(src).map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
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
        write_line(output, format_args!("    movz x16, #(0x2000001 & 0xFFFF)\n"))?;
        write_line(output, format_args!("    movk x16, #((0x2000001 >> 16) & 0xFFFF), lsl #16\n"))?;
        write_line(output, format_args!("    svc #0\n"))?;
        Ok(())
    }
}