use crate::{
    assembly_generator::{AssemblyEmitter, RegisterAllocator},
    register_allocator_common::Register,
    register_allocator_x86_64::RegisterName,
};
use crate::ir_dispatch::IROp;
use regex::Regex;
use std::{
    collections::HashMap,
    io::{self},
};

pub struct X86_64AssemblyEmitter;

impl AssemblyEmitter for X86_64AssemblyEmitter {
    fn emit(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        // Collect variables and flags
        let (variables, needs_write_int, needs_read_int) = Self::collect_data_info(ir);
        // Emit bss section for variables
        Self::emit_data_section(output, &variables)?;
        // Add text section
        output.push_str(".section .text\n");
        // Process IR lines
        self.process_ir_lines(ir, allocator, output)?;
        // Emit footer
        self.emit_footer_conditional(output, needs_write_int, needs_read_int)?;
        Ok(())
    }

    fn compute_vreg_next_uses(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
    ) -> Result<(), crate::register_allocator_common::RegisterError> {
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
            let reg = Register::new(
                usize::MAX,
                i,
                RegisterName::RSP,
                uses.into_iter().map(|u| u as i32).collect(),
                0,
            );
            if let Some(alloc) = allocator
                .as_any_mut()
                .downcast_mut::<crate::register_allocator_x86_64::X86_64RegisterAllocator>(
            ) {
                alloc.vreg_map.insert(vreg, reg);
            }
        }
        Ok(())
    }
}

impl X86_64AssemblyEmitter {
    // Helper to collect variables and flags
    fn collect_data_info(ir: &[String]) -> (std::collections::HashSet<String>, bool, bool) {
        let mut variables = std::collections::HashSet::new();
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
            }
            if line.contains("write_int") {
                needs_write_int = true;
            }
            if line.contains("read_int") {
                needs_read_int = true;
            }
        }
        (variables, needs_write_int, needs_read_int)
    }

    // Helper to emit bss section for variables
    fn emit_data_section(output: &mut String, variables: &std::collections::HashSet<String>) -> std::io::Result<()> {
        if !variables.is_empty() {
            output.push_str(".section .bss\n");
            output.push_str(".align 8\n");
            for v in variables {
                output.push_str(&format!("{}:\n", v));
                output.push_str("    .zero 8\n");
            }
        }
        Ok(())
    }

    // Helper to process IR lines
    #[allow(clippy::too_many_arguments)]
    fn process_ir_lines(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, output: &mut String) -> std::io::Result<()> {
        let mut in_proc = false;
        let mut is_main = false;
        let mut emitted_main = false;
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty()
                || line.starts_with('#')
                || line.starts_with("var ")
                || line.starts_with("const ")
            {
                continue;
            }
            // Handle labels
            if line.ends_with(':') {
                let label = &line[..line.len() - 1];
                if label == "main" {
                    output.push_str(".global main\n");
                    output.push_str("main:\n");
                    X86_64AssemblyEmitter::emit_prologue(output)?;
                    is_main = true;
                    in_proc = false;
                } else {
                    output.push_str(&format!("{}\n", line));
                    is_main = false;
                    in_proc = true;
                }
                continue;
            }
            let mut parts = line.split_whitespace();
            let op_str = parts.next().unwrap_or("");
            let rest: Vec<&str> = parts.collect();
            let op = IROp::from_str(op_str);
            self.emit_ir_instruction_x86_64(
                op,
                op_str,
                &rest,
                idx,
                allocator,
                output,
                &mut in_proc,
                &mut is_main,
                line,
            )?;
        }
        // Add epilogue for main if we were in main
        if is_main {
            X86_64AssemblyEmitter::emit_epilogue(output)?;
        }
        Ok(())
    }

    // Helper to emit main section 
    fn emit_main_section() -> std::io::Result<()> {
        Ok(())
    }

    // Helper to assemble final output
    fn assemble_final_output() -> std::io::Result<()> {
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_ir_instruction_x86_64(
        &self,
        op: IROp,
        op_str: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        in_proc: &mut bool,
        is_main: &mut bool,
        line: &str,
    ) -> Result<(), io::Error> {
        match op {
            IROp::ProcEnter => {
                if *in_proc {
                    X86_64AssemblyEmitter::emit_prologue(output)?;
                }
                let _n = rest.get(0).unwrap_or(&"0").trim_end_matches(',');
            }
            IROp::ProcExit => {
                if *in_proc {
                    X86_64AssemblyEmitter::emit_epilogue(output)?;
                } else {
                    self.emit_exit(output)?;
                }
            }
            IROp::Li => {
                self.emit_li(rest, idx, allocator, output)?;
            }
            IROp::Ld => {
                self.emit_ld(rest, idx, allocator, output)?;
            }
            IROp::St => {
                self.emit_st(rest, idx, allocator, output)?;
            }
            IROp::Add | IROp::Sub | IROp::Mul => {
                self.emit_binop(op_str, rest, idx, allocator, output)?;
            }
            IROp::Div => {
                self.emit_binop("idiv", rest, idx, allocator, output)?;
            }
            IROp::Mod => {
                output.push_str("    // TODO: Modulo operation not implemented for x86_64\n");
            }
            IROp::CmpGt | IROp::CmpLt | IROp::CmpLe | IROp::CmpGe | IROp::CmpEq | IROp::CmpNe => {
                self.emit_relational(op_str, rest, idx, allocator, output)?;
            }
            IROp::IsOdd => {
                self.emit_is_odd(rest, idx, allocator, output)?;
            }
            IROp::Beqz => {
                self.emit_beqz(rest, idx, allocator, output)?;
            }
            IROp::Jump => {
                self.emit_jump(rest, idx, allocator, output)?;
            }
            IROp::Call => {
                self.emit_call(rest, idx, allocator, output)?;
            }
            IROp::Exit => {
                self.emit_exit(output)?;
            }
            IROp::WriteInt => {
                let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                self.emit_write_int_x86_64(src, idx, allocator, output)?;
            }
            IROp::ReadInt => {
                let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                let pdst = allocator
                    .alloc(dst, output)
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
                output.push_str("    call read_int\n");
                output.push_str(&format!("    mov r{}, rax\n", pdst));
                self.free_if_dead(dst, idx, pdst, allocator);
            }
            IROp::Unknown => {
                output.push_str(&format!("// Unhandled IR: {}\n", line));
            }
        }
        Ok(())
    }

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

    fn emit_prologue(output: &mut String) -> io::Result<()> {
        output.push_str("    push rbp\n");
        output.push_str("    mov rbp, rsp\n");
        Ok(())
    }

    fn emit_epilogue(output: &mut String) -> io::Result<()> {
        output.push_str("    mov rsp, rbp\n");
        output.push_str("    pop rbp\n");
        output.push_str("    ret\n");
        Ok(())
    }

    fn emit_write_int_x86_64(
        &self,
        src: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let psrc = allocator
            .ensure(src, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        if psrc != 0 {
            output.push_str(&format!("    mov rdi, r{}\n", psrc));
        }
        output.push_str("    call write_int\n");
        self.free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_footer_conditional(&self, output: &mut String, needs_write_int: bool, needs_read_int: bool) -> std::io::Result<()> {
        if needs_write_int {
            self.emit_write_int_routine(output)?;
        }
        if needs_read_int {
            self.emit_read_int_routine(output)?;
        }
        Ok(())
    }

    fn emit_write_int_routine(&self, output: &mut String) -> std::io::Result<()> {
        output.push_str("write_int:\n");
        output.push_str("    push rbp\n");
        output.push_str("    mov rbp, rsp\n");
        output.push_str("    sub rsp, 40\n"); // Reserve 40 bytes for buffer
        output.push_str("    mov rsi, rsp\n"); // rsi = buffer
        output.push_str("    mov rcx, 0\n"); // digit count
        output.push_str("    mov rax, rdi\n"); // rax = value
        output.push_str("    mov rbx, 0\n"); // rbx = is_negative
        output.push_str("    cmp rax, 0\n");
        output.push_str("    jge .write_int_absval\n");
        output.push_str("    neg rax\n");
        output.push_str("    mov bl, 1\n"); // mark as negative
        output.push_str(".write_int_absval:\n");
        output.push_str("    cmp rax, 0\n");
        output.push_str("    jne .write_int_loop\n");
        output.push_str("    mov byte [rsi], '0'\n");
        output.push_str("    inc rcx\n");
        output.push_str("    jmp .write_int_done\n");
        output.push_str(".write_int_loop:\n");
        output.push_str("    mov rdx, 0\n");
        output.push_str("    mov r8, 10\n");
        output.push_str("    div r8\n"); // rax = rax / 10, rdx = rax % 10
        output.push_str("    add dl, '0'\n");
        output.push_str("    mov [rsi + rcx], dl\n");
        output.push_str("    inc rcx\n");
        output.push_str("    cmp rax, 0\n");
        output.push_str("    jne .write_int_loop\n");
        output.push_str(".write_int_done:\n");
        output.push_str("    cmp bl, 0\n");
        output.push_str("    je .write_int_reverse\n");
        output.push_str("    mov byte [rsi + rcx], '-'\n");
        output.push_str("    inc rcx\n");
        output.push_str(".write_int_reverse:\n");
        output.push_str("    mov rdx, rcx\n"); // rdx = length
        output.push_str("    lea rdi, [rsi]\n"); // rdi = buffer
        output.push_str("    mov r8, rcx\n"); // r8 = count
        output.push_str("    shr rcx, 1\n"); // rcx = count / 2
        output.push_str("    cmp r8, 1\n");
        output.push_str("    jbe .write_int_syscall\n");
        output.push_str("    mov r9, 0\n"); // r9 = i
        output.push_str(".write_int_revloop:\n");
        output.push_str("    mov al, [rsi + r9]\n");
        output.push_str("    mov bl, [rsi + r8 - 1 - r9]\n");
        output.push_str("    mov [rsi + r9], bl\n");
        output.push_str("    mov [rsi + r8 - 1 - r9], al\n");
        output.push_str("    inc r9\n");
        output.push_str("    cmp r9, rcx\n");
        output.push_str("    jb .write_int_revloop\n");
        output.push_str(".write_int_syscall:\n");
        output.push_str("    mov rax, 1\n"); // syscall: write
        output.push_str("    mov rdi, 1\n"); // fd = stdout
        output.push_str("    mov rsi, rsi\n"); // buf
        output.push_str("    mov rdx, r8\n"); // count
        output.push_str("    syscall\n");
        output.push_str("    add rsp, 40\n");
        output.push_str("    pop rbp\n");
        output.push_str("    ret\n");
        Ok(())
    }

    fn emit_read_int_routine(&self, output: &mut String) -> std::io::Result<()> {
        output.push_str("read_int:\n");
        output.push_str("    push rbp\n");
        output.push_str("    mov rbp, rsp\n");
        output.push_str("    sub rsp, 64\n"); // Reserve 64 bytes for buffer
        output.push_str("    mov rsi, rsp\n"); // rsi = buffer
        output.push_str("    mov rdi, 0\n"); // fd = stdin
        output.push_str("    mov rdx, 64\n"); // count = 64
        output.push_str("    mov rax, 0\n"); // syscall: read
        output.push_str("    syscall\n");
        output.push_str("    mov rcx, rsp\n"); // rcx = buffer
        output.push_str("    mov rbx, 0\n"); // rbx = result
        output.push_str("    mov rdx, 0\n"); // rdx = sign (0=+, 1=-)
        output.push_str("    mov r8, 0\n"); // r8 = index
        output.push_str(".read_int_skip_ws:\n");
        output.push_str("    mov al, [rcx + r8]\n");
        output.push_str("    cmp al, ' '\n");
        output.push_str("    je .read_int_inc_idx\n");
        output.push_str("    cmp al, '\t'\n");
        output.push_str("    je .read_int_inc_idx\n");
        output.push_str("    jmp .read_int_check_sign\n");
        output.push_str(".read_int_inc_idx:\n");
        output.push_str("    inc r8\n");
        output.push_str("    jmp .read_int_skip_ws\n");
        output.push_str(".read_int_check_sign:\n");
        output.push_str("    mov al, [rcx + r8]\n");
        output.push_str("    cmp al, '-'\n");
        output.push_str("    jne .read_int_check_plus\n");
        output.push_str("    mov rdx, 1\n");
        output.push_str("    inc r8\n");
        output.push_str("    jmp .read_int_parse\n");
        output.push_str(".read_int_check_plus:\n");
        output.push_str("    cmp al, '+'\n");
        output.push_str("    jne .read_int_parse\n");
        output.push_str("    inc r8\n");
        output.push_str(".read_int_parse:\n");
        output.push_str("    mov al, [rcx + r8]\n");
        output.push_str("    cmp al, '0'\n");
        output.push_str("    jb .read_int_done\n");
        output.push_str("    cmp al, '9'\n");
        output.push_str("    ja .read_int_done\n");
        output.push_str("    imul rbx, rbx, 10\n");
        output.push_str("    sub al, '0'\n");
        output.push_str("    movzx rax, al\n");
        output.push_str("    add rbx, rax\n");
        output.push_str("    inc r8\n");
        output.push_str("    jmp .read_int_parse\n");
        output.push_str(".read_int_done:\n");
        output.push_str("    cmp rdx, 0\n");
        output.push_str("    je .read_int_store\n");
        output.push_str("    neg rbx\n");
        output.push_str(".read_int_store:\n");
        output.push_str("    mov rax, rbx\n"); // result in rax
        output.push_str("    add rsp, 64\n");
        output.push_str("    pop rbp\n");
        output.push_str("    ret\n");
        Ok(())
    }

    fn emit_li(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let imm = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        output.push_str(&format!("    mov r{}, {}\n", pdst, imm));
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_ld(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"");
        let src = src.trim().replace(['[', ']', ','], "");
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        output.push_str(&format!("    mov r{}, qword [{}]\n", pdst, src));
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_st(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"");
        let dst = dst.trim().replace(['[', ']', ','], "");
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator
            .ensure(src, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        output.push_str(&format!("    mov qword [{}], r{}\n", dst, psrc));
        self.free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_binop(
        &self,
        op: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        let psrc1 = allocator
            .ensure(src1, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let psrc2 = allocator
            .ensure(src2, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

        let psrc1_name = RegisterName::from_usize(psrc1).unwrap();
        let psrc2_name = RegisterName::from_usize(psrc2).unwrap();
        let pdst_name = RegisterName::from_usize(pdst).unwrap();

        let op_str = match op {
            "add" => "add",
            "sub" => "sub",
            "mul" => "imul",
            "idiv" => "idiv",
            _ => {
                output.push_str(&format!("    // Unhandled binop: {}\n", op));
                return Ok(());
            }
        };

        if op == "idiv" {
            // For idiv, dividend in rax, divisor in reg, result in rax
            output.push_str(&format!("    mov rax, {}\n", psrc1_name));
            output.push_str("    cqo\n");
            output.push_str(&format!("    idiv {}\n", psrc2_name));
            output.push_str(&format!("    mov {}, rax\n", pdst_name));
        } else {
            // Standard 2-operand instructions
            if pdst != psrc1 {
                output.push_str(&format!("    mov {}, {}\n", pdst_name, psrc1_name));
            }
            output.push_str(&format!("    {} {}, {}\n", op_str, pdst_name, psrc2_name));
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
        output: &mut String,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        let psrc1 = allocator
            .ensure(src1, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let psrc2 = allocator
            .ensure(src2, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

        let psrc1_name = RegisterName::from_usize(psrc1).unwrap();
        let psrc2_name = RegisterName::from_usize(psrc2).unwrap();
        let pdst_name = RegisterName::from_usize(pdst).unwrap();

        let set_instr = match op {
            "cmp_gt" => "setg",
            "cmp_lt" => "setl",
            "cmp_le" => "setle",
            "cmp_ge" => "setge",
            "cmp_eq" => "sete",
            "cmp_ne" => "setne",
            _ => {
                output.push_str(&format!("    // Unhandled relational op: {}\n", op));
                return Ok(());
            }
        };

        // cmp src1, src2
        output.push_str(&format!("    cmp {}, {}\n", psrc1_name, psrc2_name));
        // set* al
        output.push_str(&format!("    {} al\n", set_instr));
        // movzx dst, al
        output.push_str(&format!("    movzx {}, al\n", pdst_name));

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
        output: &mut String,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator
            .ensure(src, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        output.push_str(&format!("    mov r{}, r{}\n", pdst, psrc));
        output.push_str(&format!("    and r{}, 1\n", pdst));
        self.free_if_dead(src, idx, psrc, allocator);
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_jump(
        &self,
        rest: &[&str],
        _idx: usize,
        _allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> io::Result<()> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        output.push_str(&format!("    jmp {}\n", label));
        Ok(())
    }

    fn emit_beqz(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> io::Result<()> {
        let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let label = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator
            .ensure(src, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        output.push_str(&format!("    test r{}, r{}\n", psrc, psrc));
        output.push_str(&format!("    jz {}\n", label));
        self.free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_call(
        &self,
        rest: &[&str],
        _idx: usize,
        _allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> io::Result<()> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        output.push_str(&format!("    call {}\n", label));
        Ok(())
    }

    fn emit_exit(&self, output: &mut String) -> io::Result<()> {
        output.push_str("    mov rax, 0\n");
        output.push_str("    ret\n");
        Ok(())
    }
}
