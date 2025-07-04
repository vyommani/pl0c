use crate::{assembly_generator::{AssemblyEmitter, RegisterAllocator}, register_allocator_x86_64::RegisterName, register_allocator_common::Register};
use std::{collections::HashMap, io::{self, Write}};
use regex::Regex;
use std::collections::HashSet;

pub struct X86_64AssemblyEmitter;

impl AssemblyEmitter for X86_64AssemblyEmitter {
    fn emit(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> Result<(), io::Error> {
        // Collect variable names from IR
        let mut variables = HashSet::new();
        let mut in_proc = false;
        let mut is_main = false;
        for line in ir {
            let line = line.trim();
            if line.starts_with("var ") {
                let vars = line[4..].split(',').map(|v| v.trim()).filter(|v| !v.is_empty());
                for v in vars {
                    variables.insert(v.to_string());
                }
            }
        }
        // Emit bss section for variables
        if !variables.is_empty() {
            writeln!(output, ".section .bss")?;
            writeln!(output, ".align 3")?;
            for v in &variables {
                writeln!(output, "{}:", v)?;
                writeln!(output, "    .zero 8")?;
            }
            writeln!(output, ".section .text")?;
        }
        let mut emitted_main = false;
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with("var ") || line.starts_with("const ") {
                continue;
            }
            // Emit 'main:' label if not present as the first code label
            if !emitted_main {
                if line != "main:" {
                    writeln!(output, ".global main")?;
                    writeln!(output, "main:")?;
                }
                emitted_main = true;
            }
            // Handle labels
            if line.ends_with(':') {
                writeln!(output, "{}", line)?;
                is_main = line == "main:";
                in_proc = !is_main;
                continue;
            }

            // Parse opcode and operands
            let mut parts = line.split_whitespace();
            let op = parts.next().unwrap_or("");
            let rest: Vec<&str> = parts.collect();

            match op {
                "proc_enter" => {
                    if in_proc {
                        X86_64AssemblyEmitter::emit_prologue(output)?;
                    }
                    let _n = rest.get(0).unwrap_or(&"0").trim_end_matches(',');
                }
                "proc_exit" => {
                    if in_proc {
                        X86_64AssemblyEmitter::emit_epilogue(output)?;
                    } else {
                        self.emit_exit(output)?;
                    }
                }
                "li" => { self.emit_li(&rest, idx, allocator, output)?; }
                "ld" => { self.emit_ld(&rest, idx, allocator, output)?; }
                "st" => { self.emit_st(&rest, idx, allocator, output)?; }
                "add" | "sub" | "mul" => { self.emit_binop(op, &rest, idx, allocator, output)?;}
                "div" => { self.emit_binop("idiv", &rest, idx, allocator, output)?;}
                "cmp_gt" | "cmp_lt" | "cmp_le" | "cmp_ge" | "cmp_eq" | "cmp_ne" => { self.emit_relational(op, &rest, idx, allocator, output)?; }
                "is_odd" => { self.emit_is_odd(&rest, idx, allocator, output)?; }
                "beqz" => { self.emit_beqz(&rest, idx, allocator, output)?; }
                "jump" => { self.emit_jump(&rest, idx, allocator, output)?; }
                "call" => { self.emit_call(&rest, idx, allocator, output)?; }
                "exit" => { self.emit_exit(output)?; }
                "write_int" => {
                    let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                    self.emit_write_int_x86_64(src, idx, allocator, output)?;
                }
                _ => {
                    writeln!(output, "// Unhandled IR: {}", line)?;
                }
            }
        }
        self.emit_footer(output)?;
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
        // Insert each vreg into vreg_map with its next-use info
        for (i, (vreg, uses)) in vreg_uses.into_iter().enumerate() {
            let reg = Register::new(usize::MAX, i, RegisterName::RSP, uses.into_iter().map(|u| u as i32).collect(), 0);
            if let Some(alloc) = allocator.as_any_mut().downcast_mut::<crate::register_allocator_x86_64::X86_64RegisterAllocator>() {
                alloc.vreg_map.insert(vreg, reg);
            }
        }
        Ok(())
    }
}

impl X86_64AssemblyEmitter {
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

    fn emit_prologue(output: &mut dyn io::Write) -> io::Result<()> {
        writeln!(output, "    push rbp")?;
        writeln!(output, "    mov rbp, rsp")?;
        Ok(())
    }

    fn emit_epilogue(output: &mut dyn io::Write) -> io::Result<()> {
        writeln!(output, "    mov rsp, rbp")?;
        writeln!(output, "    pop rbp")?;
        writeln!(output, "    ret")?;
        Ok(())
    }

    fn emit_write_int_x86_64(&self, src: &str, idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut dyn io::Write) -> Result<(), io::Error> {
        let psrc = allocator.ensure(src).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        if psrc != 0 {
            writeln!(output, "    mov rdi, r{}", psrc)?;
        }
        writeln!(output, "    call write_int")?;
        self.free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_footer(&self, output: &mut dyn io::Write) -> std::io::Result<()> {
        writeln!(output, ".section .text")?;
        writeln!(output, ".global _start")?;
        writeln!(output, "_start:")?;
        writeln!(output, "    call main")?;
        writeln!(output, "    mov rdi, 0")?;
        writeln!(output, "    mov rax, 60")?;
        writeln!(output, "    syscall")?;
        writeln!(output, "")?;
        writeln!(output, "write_int:")?;
        writeln!(output, "    // TODO: Implement write_int for x86-64")?;
        writeln!(output, "    ret")?;
        Ok(())
    }

    fn emit_li(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let imm = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let pdst = allocator.alloc(dst).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        writeln!(output, "    mov r{}, {}", pdst, imm)?;
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_ld(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"");
        let src = src.trim().replace(['[', ']', ','], "");
        let pdst = allocator.alloc(dst).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        writeln!(output, "    mov r{}, [{}]", pdst, src)?;
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_st(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"");
        let dst = dst.trim().replace(['[', ']', ','], "");
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator.ensure(src).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        writeln!(output, "    mov [{}], r{}", dst, psrc)?;
        self.free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_binop(
        &self,
        op: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        let psrc1 = allocator.ensure(src1).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let psrc2 = allocator.ensure(src2).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator.alloc(dst).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        
        let psrc1_name = RegisterName::from_usize(psrc1).unwrap();
        let psrc2_name = RegisterName::from_usize(psrc2).unwrap();
        let pdst_name = RegisterName::from_usize(pdst).unwrap();

        let op_str = match op {
            "add" => "add",
            "sub" => "sub",
            "mul" => "imul",
            "idiv" => "idiv",
            _ => {
                writeln!(output, "    // Unhandled binop: {}", op)?;
                return Ok(());
            }
        };

        if op == "idiv" {
            // For idiv, dividend in rax, divisor in reg, result in rax
            writeln!(output, "    mov rax, {}", psrc1_name)?;
            writeln!(output, "    cqo")?;
            writeln!(output, "    idiv {}", psrc2_name)?;
            writeln!(output, "    mov {}, rax", pdst_name)?;
        } else {
            // Standard 2-operand instructions
            if pdst != psrc1 {
                writeln!(output, "    mov {}, {}", pdst_name, psrc1_name)?;
            }
            writeln!(output, "    {} {}, {}", op_str, pdst_name, psrc2_name)?;
        }

        self.free_if_dead(src1, idx, psrc1, allocator);
        self.free_if_dead(src2, idx, psrc2, allocator);
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_relational(
        &self,
        op: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        let psrc1 = allocator.ensure(src1).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let psrc2 = allocator.ensure(src2).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator.alloc(dst).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        writeln!(output, "    // {} r{}, r{}, r{}", op, pdst, psrc1, psrc2)?;
        self.free_if_dead(src1, idx, psrc1, allocator);
        self.free_if_dead(src2, idx, psrc2, allocator);
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_is_odd(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator.ensure(src).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator.alloc(dst).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        writeln!(output, "    and r{}, r{}, 1", pdst, psrc)?;
        self.free_if_dead(src, idx, psrc, allocator);
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_jump(
        &self,
        rest: &[&str],
        _idx: usize,
        _allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        writeln!(output, "    jmp {}", label)?;
        Ok(())
    }

    fn emit_beqz(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let label = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator.ensure(src).map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        writeln!(output, "    test r{}, r{}", psrc, psrc)?;
        writeln!(output, "    jz {}", label)?;
        self.free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_call(
        &self,
        rest: &[&str],
        _idx: usize,
        _allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        writeln!(output, "    call {}", label)?;
        Ok(())
    }

    fn emit_exit(
        &self,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        writeln!(output, "    mov rdi, 0")?;
        writeln!(output, "    mov rax, 60")?;
        writeln!(output, "    syscall")?;
        Ok(())
    }
}