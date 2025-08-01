use crate::ir_dispatch::IROp;
use crate::{
    assembly_generator::{AssemblyEmitter, RegisterAllocator},
    register_allocator_common::Register,
    register_allocator_x86_64::RegisterName,
};
use regex::Regex;
use std::{
    collections::{HashMap, HashSet},
    io::{self},
};

pub struct X86_64AssemblyEmitter;

struct StackAnalyzer {
    main_stack_vars: HashSet<String>,
    proc_stack_vars: HashMap<String, HashSet<String>>,
}

impl StackAnalyzer {
    fn new(ir: &[String]) -> Self {
        let mut main_stack_vars = HashSet::new();
        let mut proc_stack_vars = HashMap::new();
        let mut current_proc = None;
        let mut in_proc = false;

        for line in ir {
            let line = line.trim();
            if line.is_empty()
                || line.starts_with('#')
                || line.starts_with("var ")
                || line.starts_with("const ")
            {
                continue;
            }
            if line.ends_with(':') {
                let label = &line[..line.len() - 1];
                if label == "main" {
                    in_proc = false;
                    continue;
                }
                let next_line = ir
                    .get(ir.iter().position(|l| l == line).unwrap() + 1)
                    .map(|s| s.trim())
                    .unwrap_or("");
                if next_line.starts_with("proc_enter") {
                    in_proc = true;
                    current_proc = Some(label.to_string());
                }
                continue;
            }
            if line.contains("[bp-") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.is_empty() {
                    continue;
                }
                // Check if the instruction is 'st' or 'ld'
                if parts[0] == "st" || parts[0] == "ld" {
                    // Look for stack variable references in the instruction
                    for part in &parts {
                        if part.contains("[bp-") {
                            let var_clean = X86_64AssemblyEmitter::strip_brackets(part);
                            if in_proc {
                                proc_stack_vars
                                    .entry(current_proc.clone().unwrap_or_default())
                                    .or_insert_with(HashSet::new)
                                    .insert(var_clean.to_string());
                            } else {
                                main_stack_vars.insert(var_clean.to_string());
                            }
                        }
                    }
                }
            }
        }
        StackAnalyzer {
            main_stack_vars,
            proc_stack_vars,
        }
    }
}

impl AssemblyEmitter for X86_64AssemblyEmitter {
    fn emit(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let (variables, constants, needs_write_int, needs_read_int, needs_write_str, strings) =
            Self::collect_data_info(ir);
        let mut data_output = String::new();
        Self::emit_data_section(
            &mut data_output,
            &variables,
            &constants,
            &strings,
            needs_write_int,
            needs_write_str,
        )?;
        let mut proc_output = String::new();
        let mut main_output = String::new();
        let stack_analyzer = StackAnalyzer::new(ir);
        self.process_ir_lines(
            ir,
            allocator,
            &mut proc_output,
            &mut main_output,
            &stack_analyzer,
            &constants,
            &strings,
        )?;
        let new_main_output = Self::emit_main_section(&main_output, &stack_analyzer)?;
        let mut footer = String::new();
        self.emit_footer_conditional(
            &mut footer,
            needs_write_int,
            needs_read_int,
            needs_write_str,
        )?;
        let final_output =
            Self::assemble_final_output(&data_output, &new_main_output, &proc_output, &footer)?;
        output.push_str(&final_output);
        Ok(())
    }

    fn compute_vreg_next_uses(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
    ) -> Result<(), crate::register_allocator_common::RegisterError> {
        // Optimization: Track live ranges for better register reuse
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
            let live_range =
                if let (Some(&first), Some(&last)) = (uses.iter().min(), uses.iter().max()) {
                    (first as i32, last as i32)
                } else {
                    (0, 0)
                };
            let reg = Register::new(
                usize::MAX,
                i,
                RegisterName::RSP,
                uses.into_iter().map(|u| u as i32).collect(),
                live_range.1.into(),
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
    fn collect_data_info(
        ir: &[String],
    ) -> (
        HashSet<String>,
        HashMap<String, String>,
        bool,
        bool,
        bool,
        Vec<String>,
    ) {
        // Optimization: Only include used variables
        let mut variables = HashSet::new();
        let mut constants = HashMap::new();
        let mut used_vars = HashSet::new();
        let mut needs_write_int = false;
        let mut needs_read_int = false;
        let mut needs_write_str = false;
        let mut strings = Vec::new();
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
            } else if line.contains("write_str") {
                needs_write_str = true;
                let parts: Vec<&str> = line.split_whitespace().collect();
                if let Some(str_label) = parts.get(1) {
                    let str_clean = str_label.trim_matches(|c| c == ',' || c == '"' || c == '\'');
                    if !str_clean.is_empty() && !strings.contains(&str_clean.to_string()) {
                        strings.push(str_clean.to_string());
                    }
                }
            } else {
                for word in line.split_whitespace() {
                    let clean = word.trim_matches(|c| c == ',' || c == '[' || c == ']');
                    if variables.contains(clean) {
                        used_vars.insert(clean.to_string());
                    }
                }
            }
        }
        (
            used_vars,
            constants,
            needs_write_int,
            needs_read_int,
            needs_write_str,
            strings,
        )
    }

    fn emit_data_section(
        output: &mut String,
        variables: &HashSet<String>,
        constants: &HashMap<String, String>,
        strings: &[String],
        needs_write_int: bool,
        needs_write_str: bool,
    ) -> io::Result<()> {
        let mut used_constants = HashSet::new();
        for (name, _) in constants {
            if output.contains(name) {
                used_constants.insert(name.clone());
            }
        }
        output.push_str("section .data\n");
        if !strings.is_empty() || needs_write_int || needs_write_str {
            output.push_str("    newline db 0xA\n");
            for (i, s) in strings.iter().enumerate() {
                output.push_str(&format!("    string_{} db \"{}\", 0\n", i, s));
            }
        }
        if needs_write_int {
            output.push_str("    digitSpace times 20 db 0\n");
        }
        for (name, value) in constants {
            if used_constants.contains(name) {
                output.push_str(&format!("    {} dq {}\n", name, value));
            }
        }
        if !variables.is_empty() {
            output.push_str("section .bss\n");
            output.push_str("    align 8\n");
            for v in variables {
                output.push_str(&format!("    {} resq 1\n", v));
            }
        }
        Ok(())
    }

    fn process_ir_lines(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
        proc_output: &mut String,
        main_output: &mut String,
        stack_analyzer: &StackAnalyzer,
        constants: &HashMap<String, String>,
        strings: &[String],
    ) -> io::Result<()> {
        let mut in_proc = false;
        let mut current_proc = None;
        let mut proc_stack = Vec::new();
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty()
                || line.starts_with('#')
                || line.starts_with("var ")
                || line.starts_with("const ")
            {
                continue;
            }
            if line.ends_with(':') {
                let label = &line[..line.len() - 1];
                if label == "main" {
                    in_proc = false; // Ensure we are not in a procedure when entering main
                    continue; // Do NOT emit main: label or prologue here
                }
                let next_line = ir.get(idx + 1).map(|s| s.trim()).unwrap_or("");
                if next_line.starts_with("proc_enter") {
                    in_proc = true;
                    current_proc = Some(label.to_string());
                    proc_stack.push(label.to_string());
                    proc_output.push_str(&format!("{}:\n", label));
                } else {
                    if in_proc {
                        proc_output.push_str(&format!("{}:\n", label));
                    } else {
                        main_output.push_str(&format!("{}:\n", label));
                    }
                }
                continue;
            }
            let mut parts = line.split_whitespace();
            let op_str = parts.next().unwrap_or("");
            let rest: Vec<&str> = parts.collect();
            let op = IROp::from_str(op_str);
            if op == IROp::ProcExit {
                self.emit_ir_instruction_x86_64(
                    op,
                    op_str,
                    &rest,
                    idx,
                    allocator,
                    if in_proc { proc_output } else { main_output },
                    &mut in_proc,
                    &mut false,
                    line,
                    stack_analyzer,
                    constants,
                    strings,
                )?;
                in_proc = false; // Explicitly reset in_proc after proc_exit
                continue;
            }
            if in_proc {
                self.emit_ir_instruction_x86_64(
                    op,
                    op_str,
                    &rest,
                    idx,
                    allocator,
                    proc_output,
                    &mut in_proc,
                    &mut false,
                    line,
                    stack_analyzer,
                    constants,
                    strings,
                )?;
            } else {
                self.emit_ir_instruction_x86_64(
                    op,
                    op_str,
                    &rest,
                    idx,
                    allocator,
                    main_output,
                    &mut in_proc,
                    &mut false,
                    line,
                    stack_analyzer,
                    constants,
                    strings,
                )?;
            }
        }
        Ok(())
    }

    fn emit_main_section(main_output: &str, stack_analyzer: &StackAnalyzer) -> io::Result<String> {
        let mut new_main_output = String::new();
        new_main_output.push_str("section .text\n");
        new_main_output.push_str("global _start\n");
        new_main_output.push_str("global main\n");
        new_main_output.push_str("_start:\n");
        new_main_output.push_str("    call main\n");
        new_main_output.push_str("    mov rdi, rax\n");
        new_main_output.push_str("    mov rax, 60\n");
        new_main_output.push_str("    syscall\n");
        new_main_output.push_str("    ret\n");
        new_main_output.push_str("main:\n");
        Self::emit_prologue(&mut new_main_output, stack_analyzer)?;
        new_main_output.push_str(main_output);
        Self::emit_epilogue(&mut new_main_output)?;
        Ok(new_main_output)
    }

    fn assemble_final_output(
        data_output: &str,
        new_main_output: &str,
        proc_output: &str,
        footer: &str,
    ) -> io::Result<String> {
        let mut final_output = String::new();
        if !data_output.is_empty() {
            final_output.push_str(data_output);
            final_output.push_str("\n");
        }
        final_output.push_str(new_main_output);
        final_output.push_str(proc_output);
        final_output.push_str(footer);
        Ok(final_output)
    }

    fn emit_ir_instruction_x86_64(
        &self,
        op: IROp,
        op_str: &str,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        in_proc: &mut bool,
        _is_main: &mut bool,
        line: &str,
        stack_analyzer: &StackAnalyzer,
        constants: &HashMap<String, String>,
        strings: &[String],
    ) -> Result<(), io::Error> {
        match op {
            IROp::ProcEnter => {
                *in_proc = true;
                Self::emit_prologue(output, stack_analyzer)?;
                let size = rest
                    .get(0)
                    .unwrap_or(&"0")
                    .trim_end_matches(',')
                    .parse::<u32>()
                    .unwrap_or(0);
                if size > 0 {
                    output.push_str(&format!("    sub rsp, {}\n", size));
                }
            }
            IROp::ProcExit => {
                Self::emit_epilogue(output)?;
                *in_proc = false;
            }
            IROp::Exit => {}
            IROp::Li => self.emit_li(rest, idx, allocator, output, constants)?,
            IROp::Ld => self.emit_ld(rest, idx, allocator, output, constants)?,
            IROp::St => self.emit_st(rest, idx, allocator, output, constants)?,
            IROp::Add | IROp::Sub | IROp::Mul => {
                self.emit_binop(op_str, rest, idx, allocator, output)?
            }
            IROp::Div => self.emit_binop("idiv", rest, idx, allocator, output)?,
            IROp::Mod => self.emit_binop("mod", rest, idx, allocator, output)?,
            IROp::CmpGt | IROp::CmpLt | IROp::CmpLe | IROp::CmpGe | IROp::CmpEq | IROp::CmpNe => {
                self.emit_relational(op_str, rest, idx, allocator, output)?
            }
            IROp::IsOdd => self.emit_is_odd(rest, idx, allocator, output)?,
            IROp::Beqz => self.emit_beqz(rest, idx, allocator, output)?,
            IROp::Jump => self.emit_jump(rest, idx, allocator, output)?,
            IROp::Call => self.emit_call(rest, idx, allocator, output)?,
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
                output.push_str(&format!("    mov r{}, rax ; read_int {}\n", pdst, dst));
                self.free_if_dead(dst, idx, pdst, allocator);
            }
            IROp::WriteStr => {
                let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                self.emit_write_str_x86_64(src, idx, allocator, output, strings)?;
            }
            IROp::Unknown => {
                output.push_str(&format!("; Unhandled IR: {}\n", line));
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

    fn emit_prologue(output: &mut String, stack_analyzer: &StackAnalyzer) -> io::Result<()> {
        output.push_str("    push rbp\n");
        output.push_str("    mov rbp, rsp\n");
        let stack_size = stack_analyzer.main_stack_vars.len() as u32 * 8;
        output.push_str(&format!("    sub rsp, {}\n", stack_size.max(16))); // Ensure at least 16 bytes
        output.push_str("    push r13\n");
        output.push_str("    push r14\n");
        output.push_str("    push r15\n");
        Ok(())
    }

    fn emit_epilogue(output: &mut String) -> io::Result<()> {
        output.push_str("    pop r15\n"); // Restore callee-saved registers
        output.push_str("    pop r14\n");
        output.push_str("    pop r13\n");
        output.push_str("    mov rax, 0\n");
        output.push_str("    leave\n");
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

    fn emit_write_str_x86_64(
        &self,
        src: &str,
        _idx: usize,
        _allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        strings: &[String],
    ) -> io::Result<()> {
        let str_clean = src.trim_matches(|c| c == ',' || c == '"' || c == '\'');
        let label = strings
            .iter()
            .position(|s| s == str_clean)
            .map(|i| format!("string_{}", i))
            .unwrap_or_else(|| {
                eprintln!("Warning: String '{}' not found in data section", str_clean);
                str_clean.to_string()
            });
        output.push_str(&format!("    mov rdi, {}\n", label));
        output.push_str("    call write_str\n");
        Ok(())
    }

    fn emit_footer_conditional(
        &self,
        output: &mut String,
        needs_write_int: bool,
        needs_read_int: bool,
        needs_write_str: bool,
    ) -> io::Result<()> {
        if needs_write_int {
            self.emit_write_int_routine(output)?;
        }
        if needs_read_int {
            self.emit_read_int_routine(output)?;
        }
        if needs_write_str {
            self.emit_write_str_routine(output)?;
        }
        Ok(())
    }

    fn emit_write_int_routine(&self, output: &mut String) -> io::Result<()> {
        output.push_str("section .text\n");
        output.push_str("write_int:\n");
        output.push_str("    push rbp\n");
        output.push_str("    mov rbp, rsp\n");
        output.push_str("    push rax\n");
        output.push_str("    push rdi\n");
        output.push_str("    push rsi\n");
        output.push_str("    push rdx\n");
        output.push_str("    push rcx\n");
        output.push_str("    push r8\n");
        output.push_str("    push r9\n"); // Save r9 (used as digit counter)
        output.push_str("    mov rax, rdi\n");
        output.push_str("    mov rsi, digitSpace + 19\n");
        output.push_str("    mov rcx, 10\n");
        output.push_str("    mov r8, 0\n"); // Flag for negative number
        output.push_str("    mov r9, 0\n"); // Initialize digit counter
        output.push_str("    cmp rax, 0\n");
        output.push_str("    jne .check_negative\n");
        output.push_str("    mov byte [rsi], '0'\n");
        output.push_str("    mov rdx, 1\n");
        output.push_str("    mov rax, 1\n");
        output.push_str("    mov rdi, 1\n");
        output.push_str("    syscall\n");
        output.push_str("    mov rax, 1\n");
        output.push_str("    mov rdi, 1\n");
        output.push_str("    mov rsi, newline\n");
        output.push_str("    mov rdx, 1\n");
        output.push_str("    syscall\n");
        output.push_str("    jmp .done\n");
        output.push_str(".check_negative:\n");
        output.push_str("    jge .convert_loop\n");
        output.push_str("    neg rax\n");
        output.push_str("    mov r8, 1\n"); // Set negative flag
        output.push_str(".convert_loop:\n");
        output.push_str("    xor rdx, rdx\n");
        output.push_str("    div rcx\n");
        output.push_str("    add dl, '0'\n");
        output.push_str("    dec rsi\n");
        output.push_str("    mov [rsi], dl\n");
        output.push_str("    inc r9\n"); // Increment digit counter
        output.push_str("    test rax, rax\n");
        output.push_str("    jnz .convert_loop\n");
        output.push_str("    cmp r8, 0\n");
        output.push_str("    je .print\n");
        output.push_str("    dec rsi\n");
        output.push_str("    mov byte [rsi], '-'\n");
        output.push_str("    inc r9\n"); // Include '-' in count
        output.push_str(".print:\n");
        output.push_str("    mov rdx, r9\n"); // Use digit count for write length
        output.push_str("    mov rax, 1\n");
        output.push_str("    mov rdi, 1\n");
        output.push_str("    syscall\n");
        output.push_str("    mov rax, 1\n");
        output.push_str("    mov rdi, 1\n");
        output.push_str("    mov rsi, newline\n");
        output.push_str("    mov rdx, 1\n");
        output.push_str("    syscall\n");
        output.push_str(".done:\n");
        output.push_str("    pop r9\n");
        output.push_str("    pop r8\n");
        output.push_str("    pop rcx\n");
        output.push_str("    pop rdx\n");
        output.push_str("    pop rsi\n");
        output.push_str("    pop rdi\n");
        output.push_str("    pop rax\n");
        output.push_str("    pop rbp\n");
        output.push_str("    ret\n");
        Ok(())
    }

    fn emit_read_int_routine(&self, output: &mut String) -> io::Result<()> {
        output.push_str("read_int:\n");
        output.push_str("    push rbp\n");
        output.push_str("    mov rbp, rsp\n");
        output.push_str("    sub rsp, 64\n");
        output.push_str("    mov rsi, rsp\n");
        output.push_str("    mov rdi, 0\n");
        output.push_str("    mov rdx, 64\n");
        output.push_str("    mov rax, 0\n");
        output.push_str("    syscall\n");
        output.push_str("    mov rcx, rsp\n");
        output.push_str("    mov rbx, 0\n");
        output.push_str("    mov rdx, 0\n");
        output.push_str("    mov r8, 0\n");
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
        output.push_str("    mov rax, rbx\n");
        output.push_str("    add rsp, 64\n");
        output.push_str("    pop rbp\n");
        output.push_str("    ret\n");
        Ok(())
    }
    fn emit_write_str_routine(&self, output: &mut String) -> io::Result<()> {
        output.push_str("section .text\n");
        output.push_str("write_str:\n");
        output.push_str("    push rbp\n");
        output.push_str("    mov rbp, rsp\n");
        output.push_str("    push rax\n");
        output.push_str("    push rdi\n");
        output.push_str("    push rsi\n");
        output.push_str("    push rdx\n");
        output.push_str("    push rcx\n");
        output.push_str("    mov rsi, rdi\n");
        output.push_str("    mov rcx, 0\n");
        output.push_str(".count_loop:\n");
        output.push_str("    cmp byte [rsi + rcx], 0\n");
        output.push_str("    je .print\n");
        output.push_str("    inc rcx\n");
        output.push_str("    jmp .count_loop\n");
        output.push_str(".print:\n");
        output.push_str("    mov rax, 1\n");
        output.push_str("    mov rdi, 1\n");
        output.push_str("    mov rdx, rcx\n");
        output.push_str("    syscall\n");
        output.push_str("    mov rax, 1\n");
        output.push_str("    mov rdi, 1\n");
        output.push_str("    mov rsi, newline\n");
        output.push_str("    mov rdx, 1\n");
        output.push_str("    syscall\n");
        output.push_str(".done:\n");
        output.push_str("    pop rcx\n");
        output.push_str("    pop rdx\n");
        output.push_str("    pop rsi\n");
        output.push_str("    pop rdi\n");
        output.push_str("    pop rax\n");
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
        constants: &HashMap<String, String>,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let imm = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        // Optimization: Inline constant values
        let value = constants.get(imm).map(|v| v.as_str()).unwrap_or(imm);
        output.push_str(&format!("    mov r{}, {}\n", pdst, value));
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn strip_brackets(s: &str) -> &str {
        s.trim().trim_start_matches('[').trim_end_matches(']')
    }

    fn format_global_addr(var: &str) -> String {
        format!("[{}]", var)
    }

    fn format_stack_addr(addr: &str) -> String {
        if addr.starts_with("bp-") || addr.starts_with("rbp-") {
            let offset = addr
                .split('-')
                .nth(1)
                .unwrap_or("0")
                .parse::<u32>()
                .unwrap_or(0);
            format!("[rbp - {}]", offset)
        } else {
            format!("[{}]", addr)
        }
    }

    fn emit_ld(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        constants: &HashMap<String, String>,
    ) -> io::Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src_cleaned = rest.get(1).unwrap_or(&"").trim().replace([','], "");
        let src = Self::strip_brackets(&src_cleaned);
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        let src_addr = if constants.contains_key(src) {
            format!("[{}]", src) // Load constant from .data section
        } else if src.starts_with("bp-") || src.starts_with("rbp-") {
            Self::format_stack_addr(src)
        } else {
            Self::format_global_addr(src)
        };
        output.push_str(&format!("    mov r{}, {}\n", pdst, src_addr));
        self.free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_st(
        &self,
        rest: &[&str],
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
        constants: &HashMap<String, String>,
    ) -> io::Result<()> {
        let dst_cleaned = rest.get(0).unwrap_or(&"").trim().replace([','], "");
        let dst = Self::strip_brackets(&dst_cleaned);
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator
            .ensure(src, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
        if constants.contains_key(dst) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!("Cannot store to constant: {}", dst),
            ));
        }
        let dst_addr = if dst.starts_with("bp-") || dst.starts_with("rbp-") {
            Self::format_stack_addr(dst)
        } else {
            Self::format_global_addr(dst)
        };
        let src_is_mem = src.starts_with("bp-")
            || src.starts_with("rbp-")
            || (src.starts_with('[') && src.ends_with(']'));
        if src_is_mem {
            let src_clean = Self::strip_brackets(src);
            if constants.contains_key(src_clean) {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("Cannot load from constant as memory: {}", src_clean),
                ));
            }
            let src_addr = if src_clean.starts_with("bp-") || src_clean.starts_with("rbp-") {
                Self::format_stack_addr(src_clean)
            } else {
                Self::format_global_addr(src_clean)
            };
            output.push_str(&format!("    mov rax, {}\n", src_addr));
            output.push_str(&format!("    mov {}, rax\n", dst_addr));
        } else {
            output.push_str(&format!("    mov {}, r{}\n", dst_addr, psrc));
        }
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
            "mod" => "mod",
            _ => {
                return Ok(());
            }
        };

        if op == "idiv" || op == "mod" {
            output.push_str(&format!("    mov rax, {}\n", psrc1_name));
            output.push_str("    cqo\n");
            output.push_str(&format!("    idiv {}\n", psrc2_name));
            output.push_str(&format!(
                "    mov {}, {}\n",
                pdst_name,
                if op == "mod" { "rdx" } else { "rax" }
            ));
        } else {
            output.push_str(&format!("    mov {}, {}\n", pdst_name, psrc1_name));
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
                output.push_str(&format!("; Unhandled relational op: {}\n", op));
                return Ok(());
            }
        };

        output.push_str(&format!("    cmp {}, {}\n", psrc1_name, psrc2_name));
        output.push_str(&format!("    {} al\n", set_instr));
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
        output.push_str("    mov rax, 0 ; exit 0\n");
        // Do NOT emit leave/ret here; epilogue is handled by section header
        Ok(())
    }
}
