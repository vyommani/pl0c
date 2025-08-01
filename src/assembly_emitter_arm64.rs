use crate::ir_dispatch::IROp;
use crate::runtime_arm64::Arm64Runtime;
use crate::{
    assembly_generator::{AssemblyEmitter, RegisterAllocator},
    register_allocator_arm64::RegisterName,
    register_allocator_common::Register,
    utils::string_utils::write_line,
};
use regex::Regex;
use std::{
    collections::HashMap,
    io::{self},
};
pub struct Arm64AssemblyEmitter;

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
        let main_stack_size =
            self.process_ir_lines(ir, allocator, &mut proc_output, &mut main_output)?;
        // Prepend .global main and prologue to main_output
        let new_main_output = Self::emit_main_section(&main_output, allocator, main_stack_size)?;
        // Assemble final output
        let mut footer = String::new();
        self.emit_footer(&mut footer, needs_write_int, needs_read_int)?;
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
        let mut vreg_uses: HashMap<String, Vec<usize>> = HashMap::new();
        let vreg_regex = Regex::new(r"v[0-9]+\b").unwrap();
        let mut call_indices = Vec::new();
        for (idx, line) in ir.iter().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            // Record function call instructions
            let is_call = line.starts_with("write_int")
                || line.starts_with("read_int")
                || line.starts_with("call ");
            if is_call {
                call_indices.push(idx as i32);
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
            // Check if vreg is live across a call
            let live_across_call = call_indices
                .iter()
                .any(|&call_idx| call_idx >= live_range.0 && call_idx <= live_range.1);
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
                .downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>(
            ) {
                alloc.vreg_map.insert(vreg, reg);
                alloc.set_live_range(i, live_range.0, live_range.1);
            }
        }
        Ok(())
    }
}

impl Arm64AssemblyEmitter {
    fn validate_register(&self, reg: usize, error_msg: &str) -> Result<(), io::Error> {
        if reg == 12 {
            Err(io::Error::new(io::ErrorKind::Other, error_msg))
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

    fn emit_load_immediate(
        &self,
        reg: usize,
        imm: usize,
        output: &mut String,
    ) -> Result<(), io::Error> {
        let imm_val = imm as u64;
        if imm >= 0 && imm <= 0xFFFF {
            write_line(output, format_args!("    mov x{}, #{}\n", reg, imm_val))?;
        } else {
            write_line(
                output,
                format_args!("    movz x{}, #{}\n", reg, imm_val & 0xFFFF),
            )?;
            if (imm_val >> 16) & 0xFFFF != 0 {
                write_line(
                    output,
                    format_args!(
                        "    movk x{}, #{}, lsl #16\n",
                        reg,
                        (imm_val >> 16) & 0xFFFF
                    ),
                )?;
            }
            if (imm_val >> 32) & 0xFFFF != 0 {
                write_line(
                    output,
                    format_args!(
                        "    movk x{}, #{}, lsl #32\n",
                        reg,
                        (imm_val >> 32) & 0xFFFF
                    ),
                )?;
            }
            if (imm_val >> 48) & 0xFFFF != 0 {
                write_line(
                    output,
                    format_args!(
                        "    movk x{}, #{}, lsl #48\n",
                        reg,
                        (imm_val >> 48) & 0xFFFF
                    ),
                )?;
            }
        }
        Ok(())
    }

    fn emit_load_store_address(
        &self,
        output: &mut String,
        dst_reg: usize,
        src_or_dst: &str,
        is_load: bool,
    ) -> Result<(), io::Error> {
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
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Invalid up-<offset>-<distance> format",
                ));
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
            self.emit_var_addr(output, 12, src_or_dst)?;
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

    // Helper to collect variables, constants, and flags
    fn collect_data_info(
        ir: &[String],
    ) -> (
        std::collections::HashSet<String>,
        std::collections::HashMap<String, String>,
        bool,
        bool,
    ) {
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
    fn emit_data_section(
        data_output: &mut String,
        variables: &std::collections::HashSet<String>,
        constants: &std::collections::HashMap<String, String>,
    ) -> std::io::Result<()> {
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

    // Helper to check if a line should be skipped
    fn should_skip_line(line: &str) -> bool {
        line.is_empty()
            || line.starts_with('#')
            || line.starts_with("var ")
            || line.starts_with("const ")
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
    fn process_label(
        &self,
        line: &str,
        ir: &[String],
        i: usize,
        in_proc: &mut bool,
        current_proc: &mut Option<String>,
    ) -> (bool, Option<String>) {
        let label = &line[..line.len() - 1];
        if line == "main:" {
            *in_proc = false;
            // Don't emit main: label here - emit_main_section handles it
            return (*in_proc, None);
        } else if self.is_procedure_label(ir, i) {
            *in_proc = true;
            *current_proc = Some(label.to_string());
        }
        (*in_proc, Some(format!("{}\n", line)))
    }

    #[allow(clippy::too_many_arguments)]
    fn process_ir_lines(
        &self,
        ir: &[String],
        allocator: &mut dyn RegisterAllocator,
        proc_output: &mut String,
        main_output: &mut String,
    ) -> std::io::Result<usize> {
        let mut main_stack_size: usize = 0;
        let mut proc_stack_sizes: Vec<usize> = Vec::new();
        let mut in_proc = false;
        let mut current_proc: Option<String> = None;
        let mut proc_stack: Vec<String> = Vec::new();
        let mut i = 0;
        while i < ir.len() {
            // Set instruction index for register allocator
            if let Some(alloc) = allocator
                .as_any_mut()
                .downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>(
            ) {
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
                let (in_proc_result, label) =
                    self.process_label(line, ir, i, &mut in_proc, &mut current_proc);
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
            if !in_proc && op == IROp::ProcEnter {
                main_stack_size = rest
                    .get(0)
                    .and_then(|s| s.parse::<usize>().ok())
                    .unwrap_or(0);
                i += 1;
                continue;
            }

            // Emit instruction to appropriate output
            let output = if in_proc {
                &mut *proc_output
            } else {
                &mut *main_output
            };
            self.emit_instruction_with_stack(
                op,
                op_str,
                &rest,
                i,
                allocator,
                output,
                &mut in_proc,
                &mut current_proc,
                &mut proc_stack,
                &mut proc_stack_sizes,
                line,
            )?;
            i += 1;
        }

        Ok(main_stack_size)
    }

    // Helper to emit main section
    fn emit_main_section(
        main_output: &str,
        allocator: &mut dyn RegisterAllocator,
        main_stack_size: usize,
    ) -> std::io::Result<String> {
        let mut new_main_output = String::new();
        write_line(&mut new_main_output, format_args!(".global main\n"))?;
        write_line(&mut new_main_output, format_args!("main:\n"))?;
        Self::emit_prologue(&mut new_main_output, allocator)?;
        // Ensure stack alignment to 16 bytes
        let aligned_stack_size = Self::align_stack_size(main_stack_size);
        if aligned_stack_size > 0 {
            write_line(
                &mut new_main_output,
                format_args!("    sub sp, sp, #{}\n", aligned_stack_size),
            )?;
        }
        new_main_output.push_str(main_output);
        // Always emit stack deallocation and epilogue for ABI compliance
        if aligned_stack_size > 0 {
            write_line(
                &mut new_main_output,
                format_args!("    add sp, sp, #{}\n", aligned_stack_size),
            )?;
        }
        Self::emit_epilogue(&mut new_main_output, allocator)?;
        Ok(new_main_output)
    }

    // Helper to assemble final output
    fn assemble_final_output(
        data_output: &str,
        new_main_output: &str,
        proc_output: &str,
        footer: &str,
    ) -> std::io::Result<String> {
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

    fn emit_prologue(
        output: &mut String,
        allocator: &mut dyn RegisterAllocator,
    ) -> Result<(), io::Error> {
        write_line(output, format_args!("    stp x29, x30, [sp, #-16]!\n"))?;
        write_line(output, format_args!("    mov x29, sp\n"))?;
        // Store static link (from x19) at [x29, #-8]
        write_line(output, format_args!("    str x19, [x29, #-8]\n"))?;
        // Save used callee-saved registers
        if let Some(alloc) = allocator
            .as_any_mut()
            .downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>(
        ) {
            let mut regs: Vec<_> = alloc.get_used_callee_saved().iter().copied().collect();
            regs.sort();
            for reg in regs {
                write_line(output, format_args!("    str x{}, [sp, #-16]!\n", reg))?;
            }
            // Allocate space for spill slots if needed
            let spill_space = alloc.get_spill_space_needed();
            if spill_space > 0 {
                let aligned_spill_space = ((spill_space + 15) / 16) * 16;
                write_line(
                    output,
                    format_args!("    sub sp, sp, #{}\n", aligned_spill_space),
                )?;
            }
        }
        Ok(())
    }

    fn emit_epilogue(
        output: &mut String,
        allocator: &mut dyn RegisterAllocator,
    ) -> Result<(), io::Error> {
        // Restore used callee-saved registers in reverse order
        if let Some(alloc) = allocator
            .as_any_mut()
            .downcast_mut::<crate::register_allocator_arm64::Arm64RegisterAllocator>(
        ) {
            let spill_space = alloc.get_spill_space_needed();
            if spill_space > 0 {
                let aligned_spill_space = ((spill_space + 15) / 16) * 16;
                write_line(
                    output,
                    format_args!("    add sp, sp, #{}\n", aligned_spill_space),
                )?;
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

    fn emit_write_int_arm64(
        &self,
        src: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        Arm64Runtime::emit_write_int_call(src, idx, allocator, output, |s, i, a| {
            self.is_dead_after(s, i, a)
        })
    }

    fn emit_read_int_arm64(
        &self,
        dst: &str,
        idx: usize,
        allocator: &mut dyn RegisterAllocator,
        output: &mut String,
    ) -> Result<(), io::Error> {
        Arm64Runtime::emit_read_int_call(dst, idx, allocator, output, |s, i, a| {
            self.is_dead_after(s, i, a)
        })
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
            Arm64Runtime::emit_write_int_implementation(output)?;
        }
        if needs_read_int {
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

    fn emit_var_addr(&self, output: &mut String, reg: u8, var: &str) -> Result<(), io::Error> {
        if reg != 12 {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Only x12 can be used for addressing",
            ));
        }
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
        self.validate_register(pdst, "x12 cannot be used for vreg allocation")?;
        self.emit_load_store_address(output, pdst, &src, true)?;
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
        self.validate_register(psrc, "x12 cannot be used for vreg allocation")?;
        self.emit_load_store_address(output, psrc, &dst, false)?;
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
                "sdiv" => {
                    if imm2 != 0 {
                        imm1.wrapping_div(imm2)
                    } else {
                        return Err(io::Error::new(io::ErrorKind::Other, "Division by zero"));
                    }
                }
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("Operation {} not supported", op),
                    ))
                }
            };
            let pdst = allocator
                .alloc(dst, output)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
            self.validate_register(pdst, "x12 cannot be used for computation")?;
            write_line(output, format_args!("    mov x{}, #{}\n", pdst, result))?;
            if self.is_dead_after(dst, idx, allocator) {
                allocator.free(pdst);
            }
            return Ok(());
        }

        let psrc1 = allocator
            .ensure(src1, output)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
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
            allocator
                .alloc(dst, output)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?
        };
        self.validate_register(pdst, "x12 cannot be used for computation")?;

        if let Ok(imm) = src2.parse::<i64>() {
            match op {
                "add" | "sub" => {
                    if imm >= -512 && imm <= 511 {
                        write_line(
                            output,
                            format_args!("    {} x{}, x{}, #{}\n", op, pdst, psrc1, imm),
                        )?;
                    } else {
                        let temp_reg = if pdst != 1 && psrc1 != 1 { 1 } else { 2 };
                        if temp_reg == 12 {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "x12 cannot be used for computation",
                            ));
                        }
                        write_line(output, format_args!("    mov x{}, #{}\n", temp_reg, imm))?;
                        write_line(
                            output,
                            format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, temp_reg),
                        )?;
                    }
                }
                "sdiv" | "mul" => {
                    let temp_reg = if pdst != 1 && psrc1 != 1 { 1 } else { 2 };
                    if temp_reg == 12 {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "x12 cannot be used for computation",
                        ));
                    }
                    write_line(output, format_args!("    mov x{}, #{}\n", temp_reg, imm))?;
                    write_line(
                        output,
                        format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, temp_reg),
                    )?;
                }
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("Operation {} with immediate not supported", op),
                    ));
                }
            }
        } else {
            let psrc2 = allocator
                .ensure(src2, output)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
            if psrc2 == 12 {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "x12 cannot be used for computation",
                ));
            }
            write_line(
                output,
                format_args!("    {} x{}, x{}, x{}\n", op, pdst, psrc1, psrc2),
            )?;
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
        // Pass static link for direct child: mov x19, x29
        write_line(output, format_args!("    mov x19, x29\n"))?;
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
    fn emit_instructions(
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
            IROp::WriteStr => {}
            IROp::Unknown => {
                write_line(target_output, format_args!("// Unhandled IR: {}\n", line))?
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_instruction_with_stack(
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
                if let Some(proc_name) = current_proc.clone() {
                    proc_stack.push(proc_name);
                }
                let stack_size = rest
                    .get(0)
                    .and_then(|s| s.parse::<usize>().ok())
                    .unwrap_or(0);
                let aligned_stack_size = Self::align_stack_size(stack_size);
                proc_stack_sizes.push(aligned_stack_size);
                Arm64AssemblyEmitter::emit_prologue(target_output, allocator)?;
                if aligned_stack_size > 0 {
                    write_line(
                        target_output,
                        format_args!("    sub sp, sp, #{}\n", aligned_stack_size),
                    )?;
                }
            }
            IROp::ProcExit => {
                let stack_size = proc_stack_sizes.pop().unwrap_or(0);
                if stack_size > 0 {
                    write_line(
                        target_output,
                        format_args!("    add sp, sp, #{}\n", stack_size),
                    )?;
                }
                Arm64AssemblyEmitter::emit_epilogue(target_output, allocator)?;
                proc_stack.pop();
                if proc_stack.is_empty() {
                    *in_proc = false;
                    *current_proc = None;
                } else {
                    *current_proc = Some(proc_stack.last().unwrap().clone());
                }
            }
            _ => self.emit_instructions(
                op,
                op_str,
                rest,
                idx,
                allocator,
                target_output,
                in_proc,
                current_proc,
                proc_stack,
                line,
            )?,
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

        let psrc1 = allocator
            .ensure(src1, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let psrc2 = allocator
            .ensure(src2, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;
        let pdst = allocator
            .alloc(dst, output)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "register allocation error"))?;

        // Use a temp register for the quotient (let's use x11, but ensure it's not used for vregs)
        let temp = 11usize;

        // sdiv x11, x<src1>, x<src2>
        write_line(
            output,
            format_args!("    sdiv x{}, x{}, x{}\n", temp, psrc1, psrc2),
        )?;
        // msub x<dst>, x11, x<src2>, x<src1>  ; x<dst> = x<src1> - (x11 * x<src2>)
        write_line(
            output,
            format_args!("    msub x{}, x{}, x{}, x{}\n", pdst, temp, psrc2, psrc1),
        )?;

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
