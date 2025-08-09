use crate::assembly_generator::RegisterAllocator;
use crate::register_allocator_common::Register;
use crate::register_allocator_x86_64::RegisterName;
use crate::errors::Pl0Result;
use crate::utils::string_utils::write_line;

pub struct X86_64Runtime;

impl X86_64Runtime {

    pub fn free_if_dead(vreg: &str, idx: usize, preg: usize, allocator: &mut dyn RegisterAllocator) {
        if Self::is_dead_after(vreg, idx, allocator) {
            allocator.free(preg);
        }
    }

    pub fn is_dead_after(vreg: &str, idx: usize, allocator: &mut dyn RegisterAllocator) -> bool {
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

    pub fn emit_write_int_x86_64(src: &str, idx: usize, allocator: &mut dyn RegisterAllocator, output: &mut String) -> Pl0Result<()> {
        let psrc = allocator
            .ensure(src, output)?;
        if psrc != 0 {
            output.push_str(&format!("    mov rdi, r{}\n", psrc));
        }
        output.push_str("    call write_int\n");
        Self::free_if_dead(src, idx, psrc, allocator);
        Ok(())
    }

    pub fn emit_write_str_x86_64(src: &str, _idx: usize, _allocator: &mut dyn RegisterAllocator, output: &mut String, strings: &[String]) -> Pl0Result<()> {
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

    pub fn emit_write_int_routine(output: &mut String) -> Pl0Result<()> {
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

    pub fn emit_read_int_routine(output: &mut String) -> Pl0Result<()> {
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
    
    pub fn emit_write_str_routine(output: &mut String) -> Pl0Result<()> {
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
}