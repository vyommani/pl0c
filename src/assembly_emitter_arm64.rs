use crate::ir_dispatch::IROp;
use crate::runtime_arm64::Arm64Runtime;
use crate::{
    assembly_generator::{AssemblyEmitter, RegisterAllocator},
    register_allocator_arm64::{RegisterName, Arm64RegisterAllocator},
    register_allocator_common::Register,
    utils::string_utils::write_line,
};
use crate::errors::Pl0Result;
use crate::errors::Pl0Error;

use regex::Regex;
use std::{
    collections::HashMap,
    collections::HashSet,
};
use crate::config::register_allocation::RESERVED_REG;
use crate::config::arm64::MAIN_WRAPPER;
use crate::config::arm64::EXIT_WRAPPER;
use crate::assembly_generator::RuntimeNeeds;
use crate::assembly_generator::ProcContext;
use crate::assembly_generator::DataInfo;
use crate::assembly_generator::TargetArch;
pub struct Arm64AssemblyEmitter;

impl AssemblyEmitter for Arm64AssemblyEmitter {
    fn emit(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let mut data_output = String::new();
        let data_info = DataInfo::from_ir(ir)?;
        data_info.emit_data_section(&mut data_output, TargetArch::ARM64)?;
        let mut proc_output = String::new();
        let mut main_output = String::new();
        let main_stack_size = self.process_ir_lines(ir, allocator, &mut proc_output, &mut main_output)?;
        // Prepend .global main and prologue to main_output
        let new_main_output = Self::emit_main_section(&main_output, allocator, main_stack_size)?;
        // Assemble final output
        let mut footer = String::new();
        self.emit_footer(&mut footer, data_info.runtime_needs())?;
        let final_output = Self::assemble_final_output(&data_output, &new_main_output, &proc_output, &footer)?;
        output.push_str(&final_output);
        Ok(())
    }

    fn compute_vreg_next_uses(&self, ir: &[String], allocator: &mut dyn RegisterAllocator) -> Pl0Result<()> {
        let mut vreg_uses: HashMap<String, Vec<usize>> = HashMap::new();
        let vreg_regex = Regex::new(r"v[0-9]+\b").unwrap();
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            // Collect virtual register uses
            for cap in vreg_regex.captures_iter(line) {
                let vreg = cap.get(0).unwrap().as_str();
                vreg_uses.entry(vreg.to_string()).or_default().push(idx);
            }
        }
        for (i, (vreg, uses)) in vreg_uses.into_iter().enumerate() {
            let reg = Register::new(usize::MAX, i, RegisterName::None, uses.iter().map(|u| *u as i32).collect(), 0);
            if let Some(alloc) = allocator.as_any_mut().downcast_mut::<Arm64RegisterAllocator>()
            {
                alloc.vreg_map.insert(vreg, reg);
            }
        }
        Ok(())
    }
}

impl Arm64AssemblyEmitter {
    fn validate_register(&self, reg: usize, error_msg: &str) -> Pl0Result<()> {
        if reg == RESERVED_REG.into() {
            Err(Pl0Error::RegisterConstraintViolation(error_msg.to_string()))
        } else {
            Ok(())
        }
    }

    fn align_stack_size(stack_size: usize) -> usize {
        if stack_size > 0 {
            ((stack_size + 15) / 16) * 16
        } else {
            0
        }
    }

    fn emit_load_immediate(&self, reg: usize, imm: usize, output: &mut String) -> Pl0Result<()> {
        let imm_val = imm as u64;
        if imm <= 0xFFFF {
            write_line(output, format_args!("    mov x{}, #{}\n", reg, imm_val))?;
        } else {
            write_line(output, format_args!("    movz x{}, #{}\n", reg, imm_val & 0xFFFF))?;
            if (imm_val >> 16) & 0xFFFF != 0 {
                write_line(output, format_args!("    movk x{}, #{}, lsl #16\n", reg, (imm_val >> 16) & 0xFFFF))?;
            }
            if (imm_val >> 32) & 0xFFFF != 0 {
                write_line(output, format_args!("    movk x{}, #{}, lsl #32\n", reg, (imm_val >> 32) & 0xFFFF))?;
            }
            if (imm_val >> 48) & 0xFFFF != 0 {
                write_line(output, format_args!("    movk x{}, #{}, lsl #48\n", reg, (imm_val >> 48) & 0xFFFF))?;
            }
        }
        Ok(())
    }

    fn emit_load_store_address(&self, output: &mut String, dst_reg: usize, src_or_dst: &str, is_load: bool) -> Pl0Result<()> {
        if src_or_dst.starts_with("bp-") {
            let offset = src_or_dst[3..].parse::<i32>().unwrap_or(0);
            write_line(
                output,
                format_args!(
                    "    {} x{}, [x29, #{}]\n",
                    if is_load { "ldr" } else { "str" },
                    dst_reg,
                    -offset
                ),
            )?;
        } else if src_or_dst.starts_with("up-") {
            let parts: Vec<&str> = src_or_dst[3..].split('-').collect();
            if parts.len() != 2 {
                return Err(Pl0Error::CodeGenError { message: "Invalid up-<offset>-<distance> format".to_string(), line: None, });
            }
            let offset = parts[0].parse::<i32>().unwrap_or(0);
            let distance = parts[1].parse::<usize>().unwrap_or(0);
            if distance > 0 {
                let temp_reg = 10; // x10: caller-saved, unused by v0–v8 in IR
                write_line(output, format_args!("    mov x{}, x29\n", temp_reg))?;
                for _ in 0..distance {
                    write_line(
                        output,
                        format_args!("    ldr x{}, [x{}, #-8]\n", temp_reg, temp_reg),
                    )?;
                }
                write_line(
                    output,
                    format_args!(
                        "    {} x{}, [x{}, #{}]\n",
                        if is_load { "ldr" } else { "str" },
                        dst_reg,
                        temp_reg,
                        -offset
                    ),
                )?;
            } else {
                write_line(
                    output,
                    format_args!(
                        "    {} x{}, [x29, #{}]\n",
                        if is_load { "ldr" } else { "str" },
                        dst_reg,
                        -offset
                    ),
                )?;
            }
        } else {
            self.emit_var_addr(output, RESERVED_REG, src_or_dst)?;
            write_line(
                output,
                format_args!(
                    "    {} x{}, [x12]\n",
                    if is_load { "ldr" } else { "str" },
                    dst_reg
                ),
            )?;
        }
        Ok(())
    }

    // Helper to check if a line should be skipped
    fn should_skip_line(line: &str) -> bool {
        line.is_empty() || line.starts_with('#') || line.starts_with("var ") || line.starts_with("const ")
    }

    // Helper to check if a label is followed by proc_enter
    fn is_procedure_label(&self, ir: &[String], label_idx: usize) -> bool {
        let mut j = label_idx + 1;
        while j < ir.len() {
            let next_line = ir[j].trim();
            if Self::should_skip_line(next_line) {
                j += 1;
                continue;
            }
            return next_line.starts_with("proc_enter");
        }
        false
    }

    // Helper to process a label line
    fn process_label(&self, line: &str, ir: &[String], i: usize, context: &mut ProcContext) -> (bool, Option<String>) {
        let label = &line[..line.len() - 1];
        if line == "main:" {
            *context = ProcContext::Main;
            return (false, None);
        } else if self.is_procedure_label(ir, i) {
            context.push_proc(label.to_string());
            return (true, Some(format!("{}\n", line)));
        }
        (context.is_in_proc(), Some(format!("{}\n", line)))
    }

    fn process_ir_lines(&self, ir: &[String], allocator: &mut dyn RegisterAllocator, proc_output: &mut String, main_output: &mut String) -> Pl0Result<usize> {
        let mut main_stack_size: usize = 0;
        let mut context = ProcContext::new();
        let mut i = 0;
        while i < ir.len() {
            // Set instruction index for register allocator
            if let Some(alloc) = allocator.as_any_mut().downcast_mut::<Arm64RegisterAllocator>() {
                alloc.set_instruction_index(i as i32);
            }

            let line = ir[i].trim();

            // Skip empty lines, comments, and declarations
            if Self::should_skip_line(line) {
                i += 1;
                continue;
            }

            // Handle labels
            if line.ends_with(':') {
                let (in_proc_result, label) = self.process_label(line, ir, i, &mut context);
                let output = if in_proc_result {
                    &mut *proc_output
                } else {
                    &mut *main_output
                };
                if let Some(label) = label {
                    write_line(output, format_args!("{}", label))?;
                }
                i += 1;
                continue;
            }

            // Parse instruction
            let mut parts = line.split_whitespace();
            let op_str = parts.next().unwrap_or("");
            let rest: Vec<&str> = parts.collect();
            let op = IROp::from_str(op_str);

            // Handle main proc_enter specially
            if !context.is_in_proc() && op == IROp::ProcEnter {
                main_stack_size = rest.get(0).and_then(|s| s.parse::<usize>().ok()).unwrap_or(0);
                i += 1;
                continue;
            }

            // Emit instruction to appropriate output
            let output = if context.is_in_proc() {
                &mut *proc_output
            } else {
                &mut *main_output
            };
            self.emit_instruction_with_stack(op, op_str, &rest, i, allocator, output, &mut context, line)?;
            i += 1;
        }

        Ok(main_stack_size)
    }

    // Helper to emit main section
    fn emit_main_section(main_output: &str, allocator: &mut dyn RegisterAllocator, main_stack_size: usize) -> Pl0Result<String> {
        let mut new_main_output = String::new();
        write_line(&mut new_main_output, format_args!(".global main\n"))?;
        write_line(&mut new_main_output, format_args!("main:\n"))?;
        Self::emit_prologue(&mut new_main_output, allocator)?;
        // Ensure stack alignment to 16 bytes
        let aligned_stack_size = Self::align_stack_size(main_stack_size);
        if aligned_stack_size > 0 {
            write_line(&mut new_main_output, format_args!("    sub sp, sp, #{}\n", aligned_stack_size))?;
        }
        new_main_output.push_str(main_output);
        // Always emit stack deallocation and epilogue for ABI compliance
        if aligned_stack_size > 0 {
            write_line(&mut new_main_output, format_args!("    add sp, sp, #{}\n", aligned_stack_size))?;
        }
        Self::emit_epilogue(&mut new_main_output, allocator)?;
        Ok(new_main_output)
    }

    // Helper to assemble final output
    fn assemble_final_output(data_output: &str, new_main_output: &str, proc_output: &str, footer: &str) -> Pl0Result<String> {
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

    fn emit_prologue(output: &mut String, allocator: &mut dyn RegisterAllocator) -> Pl0Result<()> {
        write_line(output, format_args!("    stp x29, x30, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    mov x29, sp\n"))?;
        // Store static link (from x19) at [x29, #-8]
        write_line(output, format_args!("    str x19, [x29, #-8]\n"))?;
        // Save used callee-saved registers
        if let Some(alloc) = allocator.as_any_mut().downcast_mut::<Arm64RegisterAllocator>() {
            let mut regs: Vec<_> = alloc.get_used_callee_saved().iter().copied().collect();
            regs.sort();
            for reg in regs {
                write_line(output, format_args!("    str x{}, [sp, #-16]!\n", reg))?;
            }
            // Allocate space for spill slots if needed
            let spill_space = alloc.get_spill_space_needed();
            if spill_space > 0 {
                let aligned_spill_space = ((spill_space + 15) / 16) * 16;
                write_line(output, format_args!("    sub sp, sp, #{}\n", aligned_spill_space))?;
            }
        }
        Ok(())
    }

    fn emit_epilogue(output: &mut String, allocator: &mut dyn RegisterAllocator) -> Pl0Result<()> {
        // Restore used callee-saved registers in reverse order
        if let Some(alloc) = allocator.as_any_mut().downcast_mut::<Arm64RegisterAllocator>() {
            let spill_space = alloc.get_spill_space_needed();
            if spill_space > 0 {
                let aligned_spill_space = ((spill_space + 15) / 16) * 16;
                write_line(output, format_args!("    add sp, sp, #{}\n", aligned_spill_space))?;
            }

            let mut regs: Vec<_> = alloc.get_used_callee_saved().iter().copied().collect();
            regs.sort_by(|a, b| b.cmp(a));
            // Restore each register in reverse order
            for reg in regs {
                write_line(output, format_args!("    ldr x{}, [sp], #16\n", reg))?;
            }
        }
        write_line(output, format_args!("    ldp x29, x30, [sp], #16\n"))?;
        write_line(output, format_args!("    ret\n"))?;
        Ok(())
    }

    fn emit_write_int_arm64(&self, src: &str, idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        Arm64Runtime::emit_write_int_call(src, idx, allocator, output, |s, i, a| {
            self.is_dead_after(s, i, a)
        })
    }

    fn emit_read_int_arm64(&self, dst: &str, idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        Arm64Runtime::emit_read_int_call(dst, idx, allocator, output, |s, i, a| {
            self.is_dead_after(s, i, a)
        })
    }

    fn emit_footer(&self, output: &mut String, runtime_needs: &RuntimeNeeds) -> Pl0Result<()> {
        write_line(output, format_args!("{}", MAIN_WRAPPER))?;
        if runtime_needs.write_int {
            Arm64Runtime::emit_write_int_implementation(output)?;
        }
        if runtime_needs.read_int {
            Arm64Runtime::emit_read_int_implementation(output)?;
        }
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

    fn emit_var_addr(&self, output: &mut String, reg: u8, var: &str) -> Pl0Result<()> {
        if reg != RESERVED_REG {
            return Err(Pl0Error::RegisterConstraintViolation("x12 cannot be used for computation".to_string()));
        }
        write_line(output, format_args!("    adrp x{}, {}@PAGE\n", reg, var))?;
        write_line(output, format_args!("    add x{}, x{}, {}@PAGEOFF\n", reg, reg, var))?;
        Ok(())
    }

    fn emit_li(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let imm = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let pdst = allocator.alloc(dst, output)?;
        if let Ok(imm_val) = imm.parse::<usize>() {
            self.emit_load_immediate(pdst, imm_val, output)?;
        } else {
            write_line(output, format_args!("    mov x{}, #{}\n", pdst, imm))?;
        }
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_ld(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"");
        let src = src.trim().replace(['[', ']', ','], "");
        let pdst = allocator.alloc(dst, output)?;
        self.validate_register(pdst, "x12 cannot be used for vreg allocation")?;
        self.emit_load_store_address(output, pdst, &src, true)?;
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_st(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"");
        let dst = dst.trim().replace(['[', ']', ','], "");
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator.ensure(src, output)?;
        self.validate_register(psrc, "x12 cannot be used for vreg allocation")?;
        self.emit_load_store_address(output, psrc, &dst, false)?;
        if self.is_dead_after(src, idx, allocator) {
            allocator.free(psrc);
        }
        Ok(())
    }

    fn emit_binop(&self, op: &str, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        // Handle immediate-immediate case
        if let (Ok(imm1), Ok(imm2)) = (src1.parse::<i64>(), src2.parse::<i64>()) {
            let result = match op {
                "add" => imm1.wrapping_add(imm2),
                "sub" => imm1.wrapping_sub(imm2),
                "mul" => imm1.wrapping_mul(imm2),
                "sdiv" => {
                    if imm2 != 0 {
                        imm1.wrapping_div(imm2)
                    } else {
                        return Err(Pl0Error::CodeGenError { message: "Division by zero".to_string(), line: None, });
                    }
                }
                _ => {
                    return Err(Pl0Error::CodeGenError { message: format!("Operation {} not supported", op), line: None, });
                }
            };
            let pdst = allocator.alloc(dst, output)?;
            self.validate_register(pdst, "x12 cannot be used for computation")?;
            write_line(output, format_args!("    mov x{}, #{}\n", pdst, result))?;
            if self.is_dead_after(dst, idx, allocator) {
                allocator.free(pdst);
            }
            return Ok(());
        }

        let psrc1 = allocator.ensure(src1, output)?;
        self.validate_register(psrc1, "x12 cannot be used for computation")?;

        // Reuse psrc1 for dst if temporary and dead
        let pdst = if dst == src1
            || (self.is_dead_after(dst, idx, allocator)
                && !allocator.get_vreg(dst).map_or(false, |r| {
                    r.downcast_ref::<Register<RegisterName>>()
                        .map_or(false, |reg| reg.live_across_call)
                })) {
            psrc1
        } else {
            allocator.alloc(dst, output)?
        };
        self.validate_register(pdst, "x12 cannot be used for computation")?;

        if let Ok(imm) = src2.parse::<i64>() {
            match op {
                "add" | "sub" => {
                    if imm >= -512 && imm <= 511 {
                        write_line(output, format_args!("    {} x{}, x{}, #{}\n", op, pdst, psrc1, imm))?;
                    } else {
                        let temp_reg = if pdst != 1 && psrc1 != 1 { 1 } else { 2 };
                        if temp_reg == RESERVED_REG {
                            return Err(Pl0Error::RegisterConstraintViolation("x12 cannot be used for computation".to_string()));
                        }
                        write_line(output, format_args!("    mov x{}, #{}\n", temp_reg, imm))?;
                        write_line(output, format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, temp_reg))?;
                    }
                }
                "sdiv" | "mul" => {
                    let temp_reg = if pdst != 1 && psrc1 != 1 { 1 } else { 2 };
                    if temp_reg == RESERVED_REG {
                        return Err(Pl0Error::RegisterConstraintViolation("x12 cannot be used for computation".to_string()));
                    }
                    write_line(output, format_args!("    mov x{}, #{}\n", temp_reg, imm))?;
                    write_line(output, format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, temp_reg))?;
                }
                _ => {
                    return Err(Pl0Error::CodeGenError { message: format!("Operation {} with immediate not supported", op), line: None, });
                }
            }
        } else {
            let psrc2 = allocator.ensure(src2, output)?;
            if psrc2 == RESERVED_REG.into() {
                return Err(Pl0Error::RegisterConstraintViolation("x12 cannot be used for computation".to_string()));
            }
            write_line(output,format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, psrc2))?;
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

    fn emit_relational(&self, op: &str, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');
        let psrc1 = allocator.ensure(src1, output)?;
        let psrc2 = allocator.ensure(src2, output)?;
        let pdst = allocator.alloc(dst, output)?;
        // Never use x12 for computation
        if psrc1 == RESERVED_REG.into() || psrc2 == RESERVED_REG.into() || pdst == RESERVED_REG.into() {
            return Err(Pl0Error::RegisterConstraintViolation("x12 cannot be used for computation".to_string()));
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
                return Err(Pl0Error::CodeGenError { message: "invalid relational op".to_string(), line: None, });
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

    fn emit_is_odd(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator.ensure(src, output)?;
        let pdst = allocator.alloc(dst, output)?;
        write_line(output, format_args!("    and x{}, x{}, #1\n", pdst, psrc))?;
        if self.is_dead_after(src, idx, allocator) {
            allocator.free(psrc);
        }
        if self.is_dead_after(dst, idx, allocator) {
            allocator.free(pdst);
        }
        Ok(())
    }

    fn emit_jump(&self, rest: &[&str], _idx: usize, _allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        write_line(output, format_args!("    b {}\n", label))?;
        Ok(())
    }

    fn emit_beqz(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let src = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let label = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let psrc = allocator.ensure(src, output)?;
        write_line(output, format_args!("    cbz x{}, {}\n", psrc, label))?;
        if self.is_dead_after(src, idx, allocator) {
            allocator.free(psrc);
        }
        Ok(())
    }

    fn emit_call(&self, rest: &[&str], _idx: usize, _allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let label = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        // Pass static link for direct child: mov x19, x29
        write_line(output, format_args!("    mov x19, x29\n"))?;
        write_line(output, format_args!("    bl {}\n", label))?;
        Ok(())
    }

    fn emit_exit(&self, output: &mut String) -> Pl0Result<()> {
        write_line(output, format_args!("{}", EXIT_WRAPPER))?;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_instructions(&self, op: IROp, op_str: &str, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator,target_output: &mut String,
        context: &mut ProcContext, line: &str) -> Pl0Result<()> {
        match op {
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
            IROp::WriteStr => {}
            IROp::Unknown => {
                write_line(target_output, format_args!("// Unhandled IR: {}\n", line))?
            }
            IROp::ProcEnter | IROp::ProcExit => {
                return Err(Pl0Error::CodeGenError { message: format!("{} should be handled by emit_instruction_with_stack", op_str), line: None});
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_instruction_with_stack(&self, op: IROp, op_str: &str, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator,
        target_output: &mut String, context: &mut ProcContext, line: &str) -> Pl0Result<()> {
        match op {
            IROp::ProcEnter => {
                if !context.is_in_proc() {
                    return Err(Pl0Error::CodeGenError { message: "proc_enter outside a procedure context".to_string(), line: None});
                }
                let stack_size = rest.get(0).and_then(|s| s.parse::<usize>().ok()).unwrap_or(0);
                let aligned_stack_size = Self::align_stack_size(stack_size);
                context.push_stack_size(aligned_stack_size);
                Arm64AssemblyEmitter::emit_prologue(target_output, allocator)?;
                if aligned_stack_size > 0 {
                    write_line(target_output, format_args!("    sub sp, sp, #{}\n", aligned_stack_size))?;
                }
            }
            IROp::ProcExit => {
                let stack_size = context.pop_stack_size().unwrap_or(0);
                if stack_size > 0 {
                    write_line(target_output, format_args!("    add sp, sp, #{}\n", stack_size))?;
                }
                Arm64AssemblyEmitter::emit_epilogue(target_output, allocator)?;
               context.pop_proc();
            }
            _ => self.emit_instructions(op, op_str, rest, idx, allocator, target_output, context, line)?,
        }
        Ok(())
    }

    fn emit_mod(&self, rest: &[&str], idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let dst = rest.get(0).unwrap_or(&"").trim_end_matches(',');
        let src1 = rest.get(1).unwrap_or(&"").trim_end_matches(',');
        let src2 = rest.get(2).unwrap_or(&"").trim_end_matches(',');

        let psrc1 = allocator.ensure(src1, output)?;
        let psrc2 = allocator.ensure(src2, output)?;
        let pdst = allocator.alloc(dst, output)?;

        // Use a temp register for the quotient (let's use x11, but ensure it's not used for vregs)
        let temp = 11usize;

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
