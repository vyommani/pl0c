use crate::backend::arm64::register_allocator::RegisterName;
use crate::Pl0Result;

pub trait CodeEmitter {
    // Generic instruction emission
    fn emit(&mut self, instruction: &str) -> Pl0Result<()>;
    fn flush(&mut self) -> Pl0Result<()>;

    // IR-specific instructions
    fn emit_li(&mut self, dest: &str, value: &str) -> Pl0Result<()>;
    fn emit_ld(&mut self, dest: &str, src: &str) -> Pl0Result<()>;
    fn emit_st(&mut self, dest: &str, src: &str) -> Pl0Result<()>;
    fn emit_add(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_sub(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_mul(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_div(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_mod(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_cmp_gt(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_cmp_lt(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_cmp_eq(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_cmp_ne(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_cmp_ge(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_cmp_le(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()>;
    fn emit_is_odd(&mut self, dest: &str, src: &str) -> Pl0Result<()>;
    fn emit_beqz(&mut self, src: &str, label: &str) -> Pl0Result<()>;
    fn emit_jump(&mut self, label: &str) -> Pl0Result<()>;
    fn emit_call(&mut self, label: &str) -> Pl0Result<()>;
    fn emit_exit(&mut self, code: i32) -> Pl0Result<()>;
    fn emit_write_int(&mut self, src: &str) -> Pl0Result<()>;
    fn emit_read_int(&mut self, dest: &str) -> Pl0Result<()>;
    fn emit_label(&mut self, label: &str) -> Pl0Result<()>;
    fn emit_proc_enter(&mut self, stack_size: usize) -> Pl0Result<()>;
    fn emit_proc_exit(&mut self) -> Pl0Result<()>;
    fn emit_const(&mut self, id: &str, num: &str) -> Pl0Result<()>;
    fn emit_var(&mut self, name: &str) -> Pl0Result<()>;

    // Assembly-specific instructions (for ARM64)
    fn emit_str(&mut self, reg: &RegisterName, offset: i32) -> Pl0Result<()>;
    fn emit_ldr(&mut self, reg: &RegisterName, offset: i32) -> Pl0Result<()>;
    fn emit_stp(&mut self, reg: &RegisterName) -> Pl0Result<()>;
    fn emit_mov(&mut self, dest: &RegisterName, src: &RegisterName) -> Pl0Result<()>;
    fn emit_add_asm(&mut self, dest: &RegisterName, src1: &RegisterName, src2: &RegisterName) -> Pl0Result<()>;
    fn emit_sub_asm(&mut self, dest: &RegisterName, src1: &RegisterName, src2: &RegisterName) -> Pl0Result<()>;
    fn emit_ret(&mut self) -> Pl0Result<()>;
    fn emit_bl(&mut self, label: &str) -> Pl0Result<()>;
    fn emit_b(&mut self, label: &str) -> Pl0Result<()>;
    fn emit_write_char(&mut self, src: &str) -> Pl0Result<()>;
    fn emit_write_str(&mut self, s: &str) -> Pl0Result<()>;
    fn emit_read_char(&mut self, dest: &str) -> Pl0Result<()>;
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
    fn emit(&mut self, instruction: &str) -> Pl0Result<()> {
        self.output.push_str("    ");
        self.output.push_str(instruction);
        self.output.push_str("\n");
        Ok(())
    }

    fn flush(&mut self) -> Pl0Result<()> {
        Ok(())
    }

    // IR-specific implementations
    fn emit_li(&mut self, dest: &str, value: &str) -> Pl0Result<()> {
        self.emit(&format!("li {}, {}", dest, value))
    }

    fn emit_ld(&mut self, dest: &str, src: &str) -> Pl0Result<()> {
        self.emit(&format!("ld {}, [{}]", dest, src))
    }

    fn emit_st(&mut self, dest: &str, src: &str) -> Pl0Result<()> {
        self.emit(&format!("st [{}], {}", dest, src))
    }

    fn emit_add(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("add {}, {}, {}", dest, src1, src2))
    }

    fn emit_sub(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("sub {}, {}, {}", dest, src1, src2))
    }

    fn emit_mul(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("mul {}, {}, {}", dest, src1, src2))
    }

    fn emit_div(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("div {}, {}, {}", dest, src1, src2))
    }

    fn emit_mod(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("mod {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_gt(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("cmp_gt {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_lt(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("cmp_lt {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_eq(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("cmp_eq {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_ne(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("cmp_ne {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_ge(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("cmp_ge {}, {}, {}", dest, src1, src2))
    }

    fn emit_cmp_le(&mut self, dest: &str, src1: &str, src2: &str) -> Pl0Result<()> {
        self.emit(&format!("cmp_le {}, {}, {}", dest, src1, src2))
    }

    fn emit_is_odd(&mut self, dest: &str, src: &str) -> Pl0Result<()> {
        self.emit(&format!("is_odd {}, {}", dest, src))
    }

    fn emit_beqz(&mut self, src: &str, label: &str) -> Pl0Result<()> {
        self.emit(&format!("beqz {}, {}", src, label))
    }

    fn emit_jump(&mut self, label: &str) -> Pl0Result<()> {
        self.emit(&format!("jump {}", label))
    }

    fn emit_call(&mut self, label: &str) -> Pl0Result<()> {
        self.emit(&format!("call {}", label))
    }

    fn emit_exit(&mut self, code: i32) -> Pl0Result<()> {
        self.emit(&format!("exit {}", code))
    }

    fn emit_write_int(&mut self, src: &str) -> Pl0Result<()> {
        self.emit(&format!("write_int {}", src))
    }

    fn emit_read_int(&mut self, dest: &str) -> Pl0Result<()> {
        self.emit(&format!("read_int {}", dest))
    }

    fn emit_label(&mut self, label: &str) -> Pl0Result<()> {
        self.output.push_str(&format!("{}:\n", label));
        Ok(())
    }

    fn emit_proc_enter(&mut self, stack_size: usize) -> Pl0Result<()> {
        self.emit(&format!("proc_enter {}", stack_size))
    }

    fn emit_proc_exit(&mut self) -> Pl0Result<()> {
        self.emit("proc_exit")
    }
    
    fn emit_const(&mut self, id: &str, num: &str) -> Pl0Result<()> {
        self.emit(&format!("const {} = {}", id, num))
    }

    fn emit_var(&mut self, name: &str) -> Pl0Result<()> {
        self.emit(&format!("var {}", name))
    }

    // Assembly-specific implementations
    fn emit_str(&mut self, reg: &RegisterName, offset: i32) -> Pl0Result<()> {
        self.emit(&format!("str {}, [sp, -{}]", reg, offset))
    }

    fn emit_ldr(&mut self, reg: &RegisterName, offset: i32) -> Pl0Result<()> {
        self.emit(&format!("ldr {}, [sp, -{}]", reg, offset))
    }

    fn emit_stp(&mut self, reg: &RegisterName) -> Pl0Result<()> {
        self.emit(&format!("stp {}, x29, [sp, -16]!", reg))
    }
    
    fn emit_mov(&mut self, dest: &RegisterName, src: &RegisterName) -> Pl0Result<()> {
        self.emit(&format!("mov {}, {}", dest, src))
    }

    fn emit_add_asm(&mut self, dest: &RegisterName, src1: &RegisterName, src2: &RegisterName) -> Pl0Result<()> {
        self.emit(&format!("add {}, {}, {}", dest, src1, src2))
    }

    fn emit_sub_asm(&mut self, dest: &RegisterName, src1: &RegisterName, src2: &RegisterName) -> Pl0Result<()> {
        self.emit(&format!("sub {}, {}, {}", dest, src1, src2))
    }

    fn emit_ret(&mut self) -> Pl0Result<()> {
        self.emit("ret")
    }

    fn emit_bl(&mut self, label: &str) -> Pl0Result<()> {
        self.emit(&format!("bl {}", label))
    }

    fn emit_b(&mut self, label: &str) -> Pl0Result<()> {
        self.emit(&format!("b {}", label))
    }

    fn emit_write_char(&mut self, src: &str) -> Pl0Result<()> {
        self.emit(&format!("write_char {}", src))
    }

    fn emit_write_str(&mut self, s: &str) -> Pl0Result<()> {
        self.emit(&format!("write_str \"{}\"", s))
    }

    fn emit_read_char(&mut self, dest: &str) -> Pl0Result<()> {
        self.emit(&format!("read_char {}", dest))
    }
}
