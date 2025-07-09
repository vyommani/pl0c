use crate::{
    assembly_generator::{AssemblyEmitter, RegisterAllocator},
    register_allocator_common::Register,
    register_allocator_x86_64::RegisterName,
};
use crate::ir_dispatch::IROp;
use regex::Regex;
use std::collections::HashSet;
use std::{
    collections::HashMap,
    io::{self, Write},
};

pub struct X86_64AssemblyEmitter;

impl AssemblyEmitter for X86_64AssemblyEmitter {
    fn emit(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> Result<(), io::Error> {
        // Collect variables and flags
        let (variables, needs_write_int, needs_read_int) = Self::collect_data_info(ir);
        // Emit bss section for variables
        Self::emit_data_section(output, &variables)?;
        // Process IR lines
        self.process_ir_lines(ir, allocator, output)?;
        //  Emit footer
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
    fn emit_data_section(output: &mut dyn std::io::Write, variables: &std::collections::HashSet<String>) -> std::io::Result<()> {
        if !variables.is_empty() {
            writeln!(output, ".section .bss")?;
            writeln!(output, ".align 3")?;
            for v in variables {
                writeln!(output, "{}:", v)?;
                writeln!(output, "    .zero 8")?;
            }
            writeln!(output, ".section .text")?;
        }
        Ok(())
    }

    // Helper to process IR lines
    #[allow(clippy::too_many_arguments)]
    fn process_ir_lines(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, output: &mut dyn std::io::Write) -> std::io::Result<()> {
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
        output: &mut dyn io::Write,
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
                    .alloc(dst)
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
                writeln!(output, "    call read_int")?;
                writeln!(output, "    mov r{}, rax", pdst)?;
                self.free_if_dead(dst, idx, pdst, allocator);
            }
            IROp::Unknown => {
                writeln!(output, "// Unhandled IR: {}", line)?;
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

    fn emit_write_int_x86_64(
        &self,
        src: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut dyn io::Write,
    ) -> Result<(), io::Error> {
        let psrc = allocator
            .ensure(src)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        if psrc != 0 {
            writeln!(output, "    mov rdi, r{}", psrc)?;
        }
        writeln!(output, "    call write_int")?;
        self.free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_footer_conditional(&self, output: &mut dyn io::Write, needs_write_int: bool, needs_read_int: bool) -> std::io::Result<()> {
        writeln!(output, ".section .text")?;
        writeln!(output, ".global _start")?;
        writeln!(output, "_start:")?;
        writeln!(output, "    call main")?;
        writeln!(output, "    mov rdi, 0")?;
        writeln!(output, "    mov rax, 60")?;
        writeln!(output, "    syscall")?;
        if needs_write_int {
            self.emit_write_int_routine(output)?;
        }
        if needs_read_int {
            self.emit_read_int_routine(output)?;
        }
        Ok(())
    }

    fn emit_write_int_routine(&self, output: &mut dyn io::Write) -> std::io::Result<()> {
        writeln!(output, "write_int:")?;
        writeln!(output, "    push rbp")?;
        writeln!(output, "    mov rbp, rsp")?;
        writeln!(output, "    sub rsp, 40")?; // Reserve 40 bytes for buffer
        writeln!(output, "    mov rsi, rsp")?; // rsi = buffer
        writeln!(output, "    mov rcx, 0")?; // digit count
        writeln!(output, "    mov rax, rdi")?; // rax = value
        writeln!(output, "    mov rbx, 0")?; // rbx = is_negative
        writeln!(output, "    cmp rax, 0")?;
        writeln!(output, "    jge .write_int_absval")?;
        writeln!(output, "    neg rax")?;
        writeln!(output, "    mov bl, 1")?; // mark as negative
        writeln!(output, ".write_int_absval:")?;
        writeln!(output, "    cmp rax, 0")?;
        writeln!(output, "    jne .write_int_loop")?;
        writeln!(output, "    mov byte [rsi], '0'")?;
        writeln!(output, "    inc rcx")?;
        writeln!(output, "    jmp .write_int_done")?;
        writeln!(output, ".write_int_loop:")?;
        writeln!(output, "    mov rdx, 0")?;
        writeln!(output, "    mov r8, 10")?;
        writeln!(output, "    div r8")?; // rax = rax / 10, rdx = rax % 10
        writeln!(output, "    add dl, '0'")?;
        writeln!(output, "    mov [rsi + rcx], dl")?;
        writeln!(output, "    inc rcx")?;
        writeln!(output, "    cmp rax, 0")?;
        writeln!(output, "    jne .write_int_loop")?;
        writeln!(output, ".write_int_done:")?;
        writeln!(output, "    cmp bl, 0")?;
        writeln!(output, "    je .write_int_reverse")?;
        writeln!(output, "    mov byte [rsi + rcx], '-'" )?;
        writeln!(output, "    inc rcx")?;
        writeln!(output, ".write_int_reverse:")?;
        writeln!(output, "    mov rdx, rcx")?; // rdx = length
        writeln!(output, "    lea rdi, [rsi]")?; // rdi = buffer
        writeln!(output, "    mov r8, rcx")?; // r8 = count
        writeln!(output, "    shr rcx, 1")?; // rcx = count / 2
        writeln!(output, "    cmp r8, 1")?;
        writeln!(output, "    jbe .write_int_syscall")?;
        writeln!(output, "    mov r9, 0")?; // r9 = i
        writeln!(output, ".write_int_revloop:")?;
        writeln!(output, "    mov al, [rsi + r9]")?;
        writeln!(output, "    mov bl, [rsi + r8 - 1 - r9]")?;
        writeln!(output, "    mov [rsi + r9], bl")?;
        writeln!(output, "    mov [rsi + r8 - 1 - r9], al")?;
        writeln!(output, "    inc r9")?;
        writeln!(output, "    cmp r9, rcx")?;
        writeln!(output, "    jb .write_int_revloop")?;
        writeln!(output, ".write_int_syscall:")?;
        writeln!(output, "    mov rax, 1")?; // syscall: write
        writeln!(output, "    mov rdi, 1")?; // fd = stdout
        writeln!(output, "    mov rsi, rsi")?; // buf
        writeln!(output, "    mov rdx, r8")?; // count
        writeln!(output, "    syscall")?;
        writeln!(output, "    add rsp, 40")?;
        writeln!(output, "    pop rbp")?;
        writeln!(output, "    ret")?;
        Ok(())
    }

    fn emit_read_int_routine(&self, output: &mut dyn io::Write) -> std::io::Result<()> {
        writeln!(output, "read_int:")?;
        writeln!(output, "    push rbp")?;
        writeln!(output, "    mov rbp, rsp")?;
        writeln!(output, "    sub rsp, 64")?; // Reserve 64 bytes for buffer
        writeln!(output, "    mov rsi, rsp")?; // rsi = buffer
        writeln!(output, "    mov rdi, 0")?; // fd = stdin
        writeln!(output, "    mov rdx, 64")?; // count = 64
        writeln!(output, "    mov rax, 0")?; // syscall: read
        writeln!(output, "    syscall")?;
        writeln!(output, "    mov rcx, rsp")?; // rcx = buffer
        writeln!(output, "    mov rbx, 0")?; // rbx = result
        writeln!(output, "    mov rdx, 0")?; // rdx = sign (0=+, 1=-)
        writeln!(output, "    mov r8, 0")?; // r8 = index
        writeln!(output, ".read_int_skip_ws:")?;
        writeln!(output, "    mov al, [rcx + r8]")?;
        writeln!(output, "    cmp al, ' '")?;
        writeln!(output, "    je .read_int_inc_idx")?;
        writeln!(output, "    cmp al, '\t'")?;
        writeln!(output, "    je .read_int_inc_idx")?;
        writeln!(output, "    jmp .read_int_check_sign")?;
        writeln!(output, ".read_int_inc_idx:")?;
        writeln!(output, "    inc r8")?;
        writeln!(output, "    jmp .read_int_skip_ws")?;
        writeln!(output, ".read_int_check_sign:")?;
        writeln!(output, "    mov al, [rcx + r8]")?;
        writeln!(output, "    cmp al, '-'" )?;
        writeln!(output, "    jne .read_int_check_plus")?;
        writeln!(output, "    mov rdx, 1")?;
        writeln!(output, "    inc r8")?;
        writeln!(output, "    jmp .read_int_parse")?;
        writeln!(output, ".read_int_check_plus:")?;
        writeln!(output, "    cmp al, '+'" )?;
        writeln!(output, "    jne .read_int_parse")?;
        writeln!(output, "    inc r8")?;
        writeln!(output, ".read_int_parse:")?;
        writeln!(output, "    mov al, [rcx + r8]")?;
        writeln!(output, "    cmp al, '0'")?;
        writeln!(output, "    jb .read_int_done")?;
        writeln!(output, "    cmp al, '9'")?;
        writeln!(output, "    ja .read_int_done")?;
        writeln!(output, "    imul rbx, rbx, 10")?;
        writeln!(output, "    sub al, '0'")?;
        writeln!(output, "    movzx rax, al")?;
        writeln!(output, "    add rbx, rax")?;
        writeln!(output, "    inc r8")?;
        writeln!(output, "    jmp .read_int_parse")?;
        writeln!(output, ".read_int_done:")?;
        writeln!(output, "    cmp rdx, 0")?;
        writeln!(output, "    je .read_int_store")?;
        writeln!(output, "    neg rbx")?;
        writeln!(output, ".read_int_store:")?;
        writeln!(output, "    mov rax, rbx")?; // result in rax
        writeln!(output, "    add rsp, 64")?;
        writeln!(output, "    pop rbp")?;
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
        let pdst = allocator
            .alloc(dst)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
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
        let pdst = allocator
            .alloc(dst)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
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
        let psrc = allocator
            .ensure(src)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
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
        let psrc1 = allocator
            .ensure(src1)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let psrc2 = allocator
            .ensure(src2)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator
            .alloc(dst)
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
        let psrc1 = allocator
            .ensure(src1)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let psrc2 = allocator
            .ensure(src2)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator
            .alloc(dst)
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
                writeln!(output, "    // Unhandled relational op: {}", op)?;
                return Ok(());
            }
        };

        // cmp src1, src2
        writeln!(output, "    cmp {}, {}", psrc1_name, psrc2_name)?;
        // set* al
        writeln!(output, "    {} al", set_instr)?;
        // movzx dst, al
        writeln!(output, "    movzx {}, al", pdst_name)?;

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
        let psrc = allocator
            .ensure(src)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let pdst = allocator
            .alloc(dst)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
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
        let psrc = allocator
            .ensure(src)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
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

    fn emit_exit(&self, output: &mut dyn io::Write) -> io::Result<()> {
        writeln!(output, "    mov rdi, 0")?;
        writeln!(output, "    mov rax, 60")?;
        writeln!(output, "    syscall")?;
        Ok(())
    }
}
