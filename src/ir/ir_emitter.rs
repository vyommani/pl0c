use super::IRGenerator;
use crate::{
    errors::{Pl0Result, Pl0Error},
    code_emitter::{CodeEmitter, StringCodeEmitter},
};

pub fn emit_binary_op(
    gen: &mut IRGenerator,
    op: &str,
    dest: &str,
    left: &str,
    right: &str
) -> Pl0Result<()> {
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    match op {
        "add" => emitter.emit_add(dest, left, right),
        "sub" => emitter.emit_sub(dest, left, right),
        "mul" => emitter.emit_mul(dest, left, right),
        "div" => emitter.emit_div(dest, left, right),
        "mod" => emitter.emit_mod(dest, left, right),
        _ => Err(Pl0Error::codegen_error(format!("Unknown binary op: {}", op))),
    }
}

pub fn emit_relational_op(
    gen: &mut IRGenerator,
    op: &str,
    dest: &str,
    left: &str,
    right: &str
) -> Pl0Result<()> {
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    match op {
        "cmp_gt" => emitter.emit_cmp_gt(dest, left, right),
        "cmp_lt" => emitter.emit_cmp_lt(dest, left, right),
        "cmp_eq" => emitter.emit_cmp_eq(dest, left, right),
        "cmp_ne" => emitter.emit_cmp_ne(dest, left, right),
        "cmp_ge" => emitter.emit_cmp_ge(dest, left, right),
        "cmp_le" => emitter.emit_cmp_le(dest, left, right),
        _ => Err(Pl0Error::codegen_error(format!("Unknown relational op: {}", op))),
    }
}

pub fn emit_branch_if_zero(gen: &mut IRGenerator, vreg: &str, label: &str) -> Pl0Result<()> {
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_beqz(vreg, label)
}

pub fn emit_jump(gen: &mut IRGenerator, label: &str) -> Pl0Result<()> {
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_jump(label)
}

pub fn emit_label(gen: &mut IRGenerator, label: &str) -> Pl0Result<()> {
    let mut emitter = StringCodeEmitter::new(&mut gen.code);
    emitter.emit_label(label)
}

pub fn create_and_emit_label(gen: &mut IRGenerator) -> Pl0Result<String> {
    let label = gen.create_label();
    emit_label(gen, &label)?;
    Ok(label)
}