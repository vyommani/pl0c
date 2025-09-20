use crate::ir_dispatch::IROp;
use crate::{
    assembly_generator::{AssemblyEmitter, RegisterAllocator},
    register_allocator_common::Register,
    register_allocator_x86_64::RegisterName,
};
use crate::register_allocator_x86_64::X86_64RegisterAllocator;

use crate::errors::Pl0Result;
use crate::errors::Pl0Error;

use regex::Regex;
use std::collections::{HashMap, HashSet};

use crate::runtime_x86_64::X86_64Runtime;
use crate::assembly_generator::RuntimeNeeds;

use crate::assembly_generator::DataInfo;
use crate::assembly_generator::TargetArch;
pub struct X86_64AssemblyEmitter;

struct StackAnalyzer {
    main_stack_vars: HashSet<String>,
    proc_stack_vars: HashMap<String, HashSet<String>>,
    used_callee_saved: HashSet<usize>, // Store register indices (12–15 for r12–r15)
}

impl StackAnalyzer {
    fn new(ir: &[String], allocator: &mut dyn RegisterAllocator) -> Self {
        let mut main_stack_vars = HashSet::new();
        let mut proc_stack_vars = HashMap::new();
        let mut used_callee_saved = HashSet::new();
        let mut current_proc = None;
        let mut in_proc = false;

        let vreg_regex = Regex::new(r"v[0-9]+\b").unwrap();

        for line in ir {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with("var ") || line.starts_with("const ")
            {
                continue;
            }
            if line.ends_with(':') {
                let label = &line[..line.len() - 1];
                if label == "main" {
                    in_proc = false;
                    current_proc = None;
                    continue;
                }
                let next_line = ir.iter().position(|l| l == line).and_then(|i| ir.get(i + 1)).map(|s| s.trim()).unwrap_or("");
                if next_line.starts_with("proc_enter") {
                    in_proc = true;
                    current_proc = Some(label.to_string());
                }
                continue;
            }
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.is_empty() {
                continue;
            }
            if parts[0] == "st" || parts[0] == "ld" {
                for part in &parts {
                    if part.contains("[bp-") || part.contains("[up-") {
                        let var_clean = X86_64AssemblyEmitter::strip_brackets(part);
                        if in_proc {
                            proc_stack_vars
                                .entry(current_proc.clone().unwrap_or_default())
                                .or_insert_with(HashSet::new)
                                .insert(var_clean.to_string());
                        } else {
                            main_stack_vars.insert(var_clean.to_string());
                        }
                        if part.contains("[up-") {
                            used_callee_saved.insert(12); // r12 for static link
                        }
                    }
                }
            }
            // Track virtual registers (v0–v8)
            for cap in vreg_regex.captures_iter(line) {
                let vreg = cap.get(0).unwrap().as_str();
                if let Some(reg_any) = allocator.get_vreg(vreg) {
                    if let Some(reg) = reg_any.downcast_ref::<Register<RegisterName>>() {
                        if (12..=15).contains(&reg.p_reg) {
                            used_callee_saved.insert(reg.p_reg);
                        }
                    }
                }
            }
        }
        StackAnalyzer {
            main_stack_vars,
            proc_stack_vars,
            used_callee_saved,
        }
    }

    pub fn get_used_callee_saved(&self) -> &HashSet<usize> {
        &self.used_callee_saved
    }
}

impl AssemblyEmitter for X86_64AssemblyEmitter {
    fn emit(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let mut data_output = String::new();
        let data_info = DataInfo::from_ir(ir)?;
        data_info.emit_data_section(&mut data_output, TargetArch::X86_64)?;
        let mut proc_output = String::new();
        let mut main_output = String::new();
        let stack_analyzer = StackAnalyzer::new(ir, allocator);
        self.process_ir_lines(ir, allocator, &mut proc_output, &mut main_output, &stack_analyzer, &data_info)?;
        let new_main_output = Self::emit_main_section(&main_output, &stack_analyzer)?;
        let mut footer = String::new();
        self.emit_footer_conditional(&mut footer, &data_info.runtime_needs)?;
        let final_output = Self::assemble_final_output(&data_output, &new_main_output, &proc_output, &footer)?;
        output.push_str(&final_output);
        Ok(())
    }

    fn compute_vreg_next_uses(&self, ir: &[String], allocator: &mut dyn RegisterAllocator) -> Pl0Result<()> {
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
            let reg = Register::new(usize::MAX, i, RegisterName::None, uses.into_iter().map(|u| u as i32).collect(), live_range.1.into());
            if let Some(alloc) = allocator.as_any_mut().downcast_mut::<X86_64RegisterAllocator>() {
                alloc.vreg_map.insert(vreg, reg);
            }
        }
        Ok(())
    }
}

impl X86_64AssemblyEmitter {
    fn get_reg_name(&self, reg_idx: usize) -> Pl0Result<String> {
        RegisterName::from_usize(reg_idx)
            .map(|name| name.to_string())
            .ok_or_else(|| Pl0Error::CodeGenError {
                message: format!("Invalid register index: {}", reg_idx),
                line: None,
            })
    }

    fn process_ir_lines(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, proc_output: &mut String,
        main_output: &mut String, stack_analyzer: &StackAnalyzer, data_info: &DataInfo) -> Pl0Result<()> {
        let mut in_proc = false;
        let mut current_proc = None;
        let mut proc_stack = Vec::new();
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#')|| line.starts_with("var ") || line.starts_with("const ")
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
                self.emit_ir_instruction_x86_64( op, op_str, &rest, idx, allocator,if in_proc { proc_output } else { main_output },
                    &mut in_proc, &mut false, line, stack_analyzer, data_info)?;
                in_proc = false; // Explicitly reset in_proc after proc_exit
                continue;
            }
            if in_proc {
                self.emit_ir_instruction_x86_64( op, op_str, &rest, idx, allocator, proc_output, &mut in_proc, &mut false, line,
                    stack_analyzer, data_info)?;
            } else {
                self.emit_ir_instruction_x86_64(op, op_str, &rest, idx, allocator, main_output, &mut in_proc, &mut false, line,
                    stack_analyzer, data_info)?;
            }
        }
        Ok(())
    }

    fn emit_main_section(main_output: &str, stack_analyzer: &StackAnalyzer) -> Pl0Result<String> {
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
        Self::emit_epilogue(&mut new_main_output,stack_analyzer)?;
        Ok(new_main_output)
    }

    fn assemble_final_output(data_output: &str, new_main_output: &str, proc_output: &str, footer: &str) -> Pl0Result<String> {
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

    fn emit_ir_instruction_x86_64(&self, op: IROp, op_str: &str, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator,
        output: &mut String, in_proc: &mut bool, _is_main: &mut bool, line: &str, stack_analyzer: &StackAnalyzer, data_info: &DataInfo) -> Pl0Result<()> {
        match op {
            IROp::ProcEnter => {
                *in_proc = true;
                Self::emit_prologue(output, stack_analyzer)?;
                let size = rest.get(0).unwrap_or(&"0").trim_end_matches(',').parse::<u32>().unwrap_or(0);
                if size > 0 {
                    output.push_str(&format!("    sub rsp, {}\n", size));
                }
            }
            IROp::ProcExit => {
                Self::emit_epilogue(output, stack_analyzer)?;
                *in_proc = false;
            }
            IROp::Exit => {let _ = self.emit_exit(output);}
            IROp::Li => self.emit_li(rest, idx, allocator, output, &data_info.constants)?,
            IROp::Ld => self.emit_ld(rest, idx, allocator, output, &data_info.constants)?,
            IROp::St => self.emit_st(rest, idx, allocator, output, &data_info.constants)?,
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
                X86_64Runtime::emit_write_int_x86_64(src, idx, allocator, output)?;
            }
            IROp::ReadInt => {
                let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
                let pdst = allocator
                    .alloc(dst, output)?;
                output.push_str("    call read_int\n");
                output.push_str(&format!("    mov r{}, rax ; read_int {}\n", pdst, dst));
                X86_64Runtime::free_if_dead(dst, idx, pdst, allocator);
            }
            IROp::WriteStr => {
                let src = line[10..].trim_matches(|c| c == '"' || c == '\'');
                X86_64Runtime::emit_write_str_x86_64(src, idx, allocator, output, data_info.strings.as_slice())?;
            }
            IROp::Unknown => {
                output.push_str(&format!("; Unhandled IR: {}\n", line));
            }
        }
        Ok(())
    }

    fn emit_prologue(output: &mut String, stack_analyzer: &StackAnalyzer) -> Pl0Result<()> {
        output.push_str("    push rbp\n");
        output.push_str("    mov rbp, rsp\n");
        // Calculate stack size: 8 bytes per variable + 8 for static link if needed
        let stack_size = if stack_analyzer.main_stack_vars.is_empty() {
            stack_analyzer.proc_stack_vars.values().map(|vars| vars.len() as u32 * 8).max().unwrap_or(0) + if stack_analyzer.used_callee_saved.contains(&12) { 8 } else { 0 }
        } else {
            stack_analyzer.main_stack_vars.len() as u32 * 8 + if stack_analyzer.used_callee_saved.contains(&12) { 8 } else { 0 }
        };
        let aligned_size = (stack_size + 15) & !15; // Align to 16 bytes
        if aligned_size > 0 {
            output.push_str(&format!("    sub rsp, {}\n", aligned_size));
        }
        if stack_analyzer.used_callee_saved.contains(&12) {
            output.push_str("    mov [rbp - 8], r12 ; Store static link\n");
        }
        let used_callee_saved = stack_analyzer.get_used_callee_saved();
        for reg in used_callee_saved {
            output.push_str(&format!("    push r{}\n", reg));
        }
        Ok(())
    }

    fn emit_epilogue(output: &mut String, stack_analyzer: &StackAnalyzer) -> Pl0Result<()> {
        let used_callee_saved: Vec<_> = stack_analyzer.get_used_callee_saved().into_iter().collect();
        let mut sorted_regs = used_callee_saved;
        sorted_regs.sort_by(|a, b| b.cmp(a)); // Restore in reverse order
        for reg in sorted_regs {
            output.push_str(&format!("    pop r{}\n", reg));
        }
        output.push_str("    mov rsp, rbp\n");
        output.push_str("    pop rbp\n");
        output.push_str("    ret\n");
        Ok(())
    }

    fn emit_footer_conditional(&self, output: &mut String, runtime_needs: &RuntimeNeeds) -> Pl0Result<()> {
        if runtime_needs.write_int {
            X86_64Runtime::emit_write_int_routine(output)?;
        }
        if runtime_needs.read_int {
            X86_64Runtime::emit_read_int_routine(output)?;
        }
        if runtime_needs.write_str {
            X86_64Runtime::emit_write_str_routine(output)?;
        }
        Ok(())
    }

    fn emit_li(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String, constants: &HashMap<String, String>) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let imm = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let pdst = allocator.alloc(dst, output)?;
        // Optimization: Inline constant values
        let value = constants.get(imm).map(|v| v.as_str()).unwrap_or(imm);
        output.push_str(&format!("    mov r{}, {}\n", pdst, value));
        X86_64Runtime::free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn strip_brackets(s: &str) -> &str {
        s.trim().trim_start_matches('[').trim_end_matches(']')
    }

    fn emit_ld(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String, constants: &HashMap<String, String>) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src_cleaned = rest.get(1).unwrap_or(&"").trim().replace([','], "");
        let src = Self::strip_brackets(&src_cleaned);
        let pdst = allocator.alloc(dst, output)?;

        if src.starts_with("bp-") {
            let mut offset = src[3..].parse::<i32>().unwrap_or(0);
            if offset == 8 {
                offset = 16;
            } else if offset <= 8 {
                return Err(Pl0Error::CodeGenError { message: format!("Invalid load from reserved stack slot: {}", src), line: None, });
            }
            output.push_str(&format!("    mov r{}, [rbp - {}]\n", pdst, offset));
        } else if src.starts_with("up-") {
            let parts: Vec<&str> = src[3..].split('-').collect();
            if parts.len() != 2 {
                return Err(Pl0Error::CodeGenError { message: "Invalid up-<offset>-<distance> format".to_string(), line: None, });
            }
            let offset = parts[0].parse::<i32>().unwrap_or(0);
            let distance = parts[1].parse::<usize>().unwrap_or(0);
            let temp_reg = 11; // r11: caller-saved
            output.push_str(&format!("    mov r{}, rbp\n", temp_reg));
            for _ in 0..distance {
                output.push_str(&format!("    mov r{}, [r{} - 8]\n", temp_reg, temp_reg));
            }
            output.push_str(&format!("    mov r{}, [r{} - {}]\n", pdst, temp_reg, offset));
        } else if constants.contains_key(src) {
            output.push_str(&format!("    mov r{}, [{}]\n", pdst, src));
        } else {
            output.push_str(&format!("    mov r{}, [{}]\n", pdst, src));
        }

        X86_64Runtime::free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_st(&self,rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String,constants: &HashMap<String, String>) -> Pl0Result<()> {
        let dst_cleaned = rest.get(0).unwrap_or(&"").trim().replace([','], "");
        let dst = Self::strip_brackets(&dst_cleaned);
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator.ensure(src, output)?;

        if constants.contains_key(dst) {
            return Err(Pl0Error::CodeGenError { message: format!("Cannot store to constant: {}", dst), line: None, });
        }

        if dst.starts_with("bp-") {
            let mut offset = dst[3..].parse::<i32>().unwrap_or(0);
            if offset == 8 {
                // Remap [bp-8] to [bp-16] for local variable in procedure b
                offset = 16;
            } else if offset <= 8 {
                return Err(Pl0Error::CodeGenError { message: format!("Invalid store to reserved stack slot: {}", dst), line: None, });
            }
            output.push_str(&format!("    mov [rbp - {}], r{}\n", offset, psrc));
        } else if dst.starts_with("up-") {
            let parts: Vec<&str> = dst[3..].split('-').collect();
            if parts.len() != 2 {
                return Err(Pl0Error::CodeGenError { message: "Invalid up-<offset>-<distance> format".to_string(), line: None, });
            }
            let offset = parts[0].parse::<i32>().unwrap_or(0);
            let distance = parts[1].parse::<usize>().unwrap_or(0);
            let temp_reg = 11; // r11: caller-saved
            output.push_str(&format!("    mov r{}, rbp\n", temp_reg));
            for _ in 0..distance {
                output.push_str(&format!("    mov r{}, [r{} - 8]\n", temp_reg, temp_reg));
            }
            output.push_str(&format!("    mov [r{} - {}], r{}\n", temp_reg, offset, psrc));
        } else {
            output.push_str(&format!("    mov [{}], r{}\n", dst, psrc));
        }

        X86_64Runtime::free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_binop(&self, op: &str, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        
        let psrc1 = allocator.ensure(src1, output)?;
        let psrc2 = allocator.ensure(src2, output)?;
        let pdst = allocator.alloc(dst, output)?;

        let psrc1_name = self.get_reg_name(psrc1)?;
        let psrc2_name = self.get_reg_name(psrc2)?;
        let pdst_name = self.get_reg_name(pdst)?;

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

        X86_64Runtime::free_if_dead(src1, idx, psrc1, allocator);
        X86_64Runtime::free_if_dead(src2, idx, psrc2, allocator);
        X86_64Runtime::free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_relational(&self, op: &str, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        
        let psrc1 = allocator.ensure(src1, output)?;
        let psrc2 = allocator.ensure(src2, output)?;
        let pdst = allocator.alloc(dst, output)?;

        let psrc1_name = self.get_reg_name(psrc1)?;
        let psrc2_name = self.get_reg_name(psrc2)?;
        let pdst_name = self.get_reg_name(pdst)?;

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

        X86_64Runtime::free_if_dead(src1, idx, psrc1, allocator);
        X86_64Runtime::free_if_dead(src2, idx, psrc2, allocator);
        X86_64Runtime::free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_is_odd(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        
        let psrc = allocator.ensure(src, output)?;
        let pdst = allocator.alloc(dst, output)?;
        
        output.push_str(&format!("    mov r{}, r{}\n", pdst, psrc));
        output.push_str(&format!("    and r{}, 1\n", pdst));
        X86_64Runtime::free_if_dead(src, idx, psrc, allocator);
        X86_64Runtime::free_if_dead(dst, idx, pdst, allocator);
        Ok(())
    }

    fn emit_jump(&self, rest: &[&str], _idx: usize, _allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        output.push_str(&format!("    jmp {}\n", label));
        Ok(())
    }

    fn emit_beqz(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let label = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator.ensure(src, output)?;
        output.push_str(&format!("    test r{}, r{}\n", psrc, psrc));
        output.push_str(&format!("    jz {}\n", label));
        X86_64Runtime::free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    fn emit_call(&self, rest: &[&str], _idx: usize, _allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        output.push_str("    mov r12, rbp ; Pass static link\n");
        output.push_str("    and rsp, -16 ; Align stack to 16 bytes\n");
        output.push_str(&format!("    call {}\n", label));
        Ok(())
    }

    fn emit_exit(&self, output: &mut String) -> Pl0Result<()> {
        output.push_str("    mov rax, 0 ; exit 0\n");
        Ok(())
    }
}
