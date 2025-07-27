// src/code_emitter.rs
use crate::register_allocator_arm64::RegisterName;
use crate::register_allocator_common::RegisterError;

pub trait CodeEmitter {
    // Generic instruction emission
    fn emit(&mut self, instruction: &str) -> Result<(), RegisterError>;
    fn flush(&mut self) -> Result<(), RegisterError>;

    // IR-specific instructions
    fn emit_li(&mut self, dest: &str, value: &str) -> Result<(), RegisterError>;
    fn emit_ld(&mut self, dest: &str, src: &str) -> Result<(), RegisterError>;
    fn emit_st(&mut self, dest: &str, src: &str) -> Result<(), RegisterError>;
    fn emit_add(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_sub(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_mul(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_div(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_mod(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_cmp_gt(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_cmp_lt(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_cmp_eq(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_cmp_ne(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_cmp_ge(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_cmp_le(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError>;
    fn emit_is_odd(&mut self, dest: &str, src: &str) -> Result<(), RegisterError>;
    fn emit_beqz(&mut self, src: &str, label: &str) -> Result<(), RegisterError>;
    fn emit_jump(&mut self, label: &str) -> Result<(), RegisterError>;
    fn emit_call(&mut self, label: &str) -> Result<(), RegisterError>;
    fn emit_exit(&mut self, code: i32) -> Result<(), RegisterError>;
    fn emit_write_int(&mut self, src: &str) -> Result<(), RegisterError>;
    fn emit_read_int(&mut self, dest: &str) -> Result<(), RegisterError>;
    fn emit_label(&mut self, label: &str) -> Result<(), RegisterError>;
    fn emit_proc_enter(&mut self, stack_size: usize) -> Result<(), RegisterError>;
    fn emit_proc_exit(&mut self) -> Result<(), RegisterError>;
    fn emit_const(&mut self, id: &str, num: &str) -> Result<(), RegisterError>;
    fn emit_var(&mut self, name: &str) -> Result<(), RegisterError>;

    // Assembly-specific instructions (for ARM64)
    fn emit_str(&mut self, reg: &RegisterName, offset: i32) -> Result<(), RegisterError>;
    fn emit_ldr(&mut self, reg: &RegisterName, offset: i32) -> Result<(), RegisterError>;
    fn emit_stp(&mut self, reg: &RegisterName) -> Result<(), RegisterError>;
    fn emit_mov(&mut self, dest: &RegisterName, src: &RegisterName) -> Result<(), RegisterError>;
    fn emit_add_asm(
        &mut self,
        dest: &RegisterName,
        src1: &RegisterName,
        src2: &RegisterName,
    ) -> Result<(), RegisterError>;
    fn emit_sub_asm(
        &mut self,
        dest: &RegisterName,
        src1: &RegisterName,
        src2: &RegisterName,
    ) -> Result<(), RegisterError>;
    fn emit_ret(&mut self) -> Result<(), RegisterError>;
    fn emit_bl(&mut self, label: &str) -> Result<(), RegisterError>;
    fn emit_b(&mut self, label: &str) -> Result<(), RegisterError>;
    fn emit_write_char(&mut self, src: &str) -> Result<(), RegisterError>;
    fn emit_write_str(&mut self, s: &str) -> Result<(), RegisterError>;
    fn emit_read_char(&mut self, dest: &str) -> Result<(), RegisterError>;
}

pub struct StringCodeEmitter<'a> {
    output: &'a mut String,
}

impl<'a> StringCodeEmitter<'a> {
    pub fn new(output: &'a mut String) -> Self {
        StringCodeEmitter { output }
    }
}

impl<'a> CodeEmitter for StringCodeEmitter<'a> {
    fn emit(&mut self, instruction: &str) -> Result<(), RegisterError> {
        self.output.push_str("    ");
        self.output.push_str(instruction);
        self.output.push_str("\n");
        Ok(())
    }

    fn flush(&mut self) -> Result<(), RegisterError> {
        Ok(())
    }

    // IR-specific implementations
    fn emit_li(&mut self, dest: &str, value: &str) -> Result<(), RegisterError> {
        self.emit(&format!("li {}, {}", dest, value))
    }

    fn emit_ld(&mut self, dest: &str, src: &str) -> Result<(), RegisterError> {
        self.emit(&format!("ld {}, [{}]", dest, src))
    }

    fn emit_st(&mut self, dest: &str, src: &str) -> Result<(), RegisterError> {
        self.emit(&format!("st [{}], {}", dest, src))
    }

    fn emit_add(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("add {}, {}, {}", dest, src1, src2))
    }

    fn emit_sub(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("sub {}, {}, {}", dest, src1, src2))
    }

    fn emit_mul(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("mul {}, {}, {}", dest, src1, src2))
    }

    fn emit_div(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("div {}, {}, {}", dest, src1, src2))
    }

    fn emit_mod(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("mod {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_gt(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("cmp_gt {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_lt(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("cmp_lt {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_eq(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("cmp_eq {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_ne(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("cmp_ne {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_ge(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("cmp_ge {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_le(&mut self, dest: &str, src1: &str, src2: &str) -> Result<(), RegisterError> {
        self.emit(&format!("cmp_le {}, {}, {}", dest, src1, src2))
    }

    fn emit_is_odd(&mut self, dest: &str, src: &str) -> Result<(), RegisterError> {
        self.emit(&format!("is_odd {}, {}", dest, src))
    }

    fn emit_beqz(&mut self, src: &str, label: &str) -> Result<(), RegisterError> {
        self.emit(&format!("beqz {}, {}", src, label))
    }

    fn emit_jump(&mut self, label: &str) -> Result<(), RegisterError> {
        self.emit(&format!("jump {}", label))
    }

    fn emit_call(&mut self, label: &str) -> Result<(), RegisterError> {
        self.emit(&format!("call {}", label))
    }

    fn emit_exit(&mut self, code: i32) -> Result<(), RegisterError> {
        self.emit(&format!("exit {}", code))
    }

    fn emit_write_int(&mut self, src: &str) -> Result<(), RegisterError> {
        self.emit(&format!("write_int {}", src))
    }

    fn emit_read_int(&mut self, dest: &str) -> Result<(), RegisterError> {
        self.emit(&format!("read_int {}", dest))
    }

    fn emit_label(&mut self, label: &str) -> Result<(), RegisterError> {
        self.output.push_str(&format!("{}:\n", label));
        Ok(())
    }

    fn emit_proc_enter(&mut self, stack_size: usize) -> Result<(), RegisterError> {
        self.emit(&format!("proc_enter {}", stack_size))
    }

    fn emit_proc_exit(&mut self) -> Result<(), RegisterError> {
        self.emit("proc_exit")
    }

    fn emit_const(&mut self, id: &str, num: &str) -> Result<(), RegisterError> {
        self.emit(&format!("const {} = {}", id, num))
    }

    fn emit_var(&mut self, name: &str) -> Result<(), RegisterError> {
        self.emit(&format!("var {}", name))
    }

    // Assembly-specific implementations
    fn emit_str(&mut self, reg: &RegisterName, offset: i32) -> Result<(), RegisterError> {
        self.emit(&format!("str {}, [sp, -{}]", reg, offset))
    }

    fn emit_ldr(&mut self, reg: &RegisterName, offset: i32) -> Result<(), RegisterError> {
        self.emit(&format!("ldr {}, [sp, -{}]", reg, offset))
    }

    fn emit_stp(&mut self, reg: &RegisterName) -> Result<(), RegisterError> {
        self.emit(&format!("stp {}, x29, [sp, -16]!", reg))
    }

    fn emit_mov(&mut self, dest: &RegisterName, src: &RegisterName) -> Result<(), RegisterError> {
        self.emit(&format!("mov {}, {}", dest, src))
    }

    fn emit_add_asm(
        &mut self,
        dest: &RegisterName,
        src1: &RegisterName,
        src2: &RegisterName,
    ) -> Result<(), RegisterError> {
        self.emit(&format!("add {}, {}, {}", dest, src1, src2))
    }

    fn emit_sub_asm(
        &mut self,
        dest: &RegisterName,
        src1: &RegisterName,
        src2: &RegisterName,
    ) -> Result<(), RegisterError> {
        self.emit(&format!("sub {}, {}, {}", dest, src1, src2))
    }

    fn emit_ret(&mut self) -> Result<(), RegisterError> {
        self.emit("ret")
    }

    fn emit_bl(&mut self, label: &str) -> Result<(), RegisterError> {
        self.emit(&format!("bl {}", label))
    }

    fn emit_b(&mut self, label: &str) -> Result<(), RegisterError> {
        self.emit(&format!("b {}", label))
    }

    fn emit_write_char(&mut self, src: &str) -> Result<(), RegisterError> {
        self.emit(&format!("write_char {}", src))
    }

    fn emit_write_str(&mut self, s: &str) -> Result<(), RegisterError> {
        self.emit(&format!("write_str \"{}\"", s))
    }

    fn emit_read_char(&mut self, dest: &str) -> Result<(), RegisterError> {
        self.emit(&format!("read_char {}", dest))
    }
}
