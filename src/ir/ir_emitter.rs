use super::IRGenerator;
use super::tac::{Instr, MemLoc, VReg, Value};
use crate::utils::errors::{Pl0Result, Pl0Error};

/// Parses a virtual-register name like `"v5"` (as produced by
/// `IRGenerator::allocate_virtual_register`) back into a `VReg`. Every
/// caller in this module passes a string that was always produced that
/// way, so a parse failure here means a genuine internal-compiler bug, not
/// a malformed user program - hence a real `Err`, never a silent fallback.
fn parse_vreg(s: &str) -> Pl0Result<VReg> {
    s.strip_prefix('v')
        .and_then(|n| n.parse::<u32>().ok())
        .map(VReg)
        .ok_or_else(|| Pl0Error::codegen_error(format!("Internal error: not a virtual register: {}", s)))
}

fn value(s: &str) -> Pl0Result<Value> {
    parse_vreg(s).map(Value::VReg)
}

pub fn emit_li(gen: &mut IRGenerator, dest: &str, value: i64) -> Pl0Result<()> {
    let dst = parse_vreg(dest)?;
    gen.push_instr(Instr::Li { dst, value });
    Ok(())
}

pub fn emit_ld(gen: &mut IRGenerator, dest: &str, src: MemLoc) -> Pl0Result<()> {
    let dst = parse_vreg(dest)?;
    gen.push_instr(Instr::Ld { dst, src });
    Ok(())
}

pub fn emit_st(gen: &mut IRGenerator, dest: MemLoc, src: &str) -> Pl0Result<()> {
    let src = value(src)?;
    gen.push_instr(Instr::St { dst: dest, src });
    Ok(())
}

pub fn emit_binary_op(
    gen: &mut IRGenerator,
    op: &str,
    dest: &str,
    left: &str,
    right: &str
) -> Pl0Result<()> {
    let dst = parse_vreg(dest)?;
    let lhs = value(left)?;
    let rhs = value(right)?;
    let instr = match op {
        "add" => Instr::Add { dst, lhs, rhs },
        "sub" => Instr::Sub { dst, lhs, rhs },
        "mul" => Instr::Mul { dst, lhs, rhs },
        "div" => Instr::Div { dst, lhs, rhs },
        "mod" => Instr::Mod { dst, lhs, rhs },
        _ => return Err(Pl0Error::codegen_error(format!("Unknown binary op: {}", op))),
    };
    gen.push_instr(instr);
    Ok(())
}

pub fn emit_relational_op(
    gen: &mut IRGenerator,
    op: &str,
    dest: &str,
    left: &str,
    right: &str
) -> Pl0Result<()> {
    let dst = parse_vreg(dest)?;
    let lhs = value(left)?;
    let rhs = value(right)?;
    let instr = match op {
        "cmp_gt" => Instr::CmpGt { dst, lhs, rhs },
        "cmp_lt" => Instr::CmpLt { dst, lhs, rhs },
        "cmp_eq" => Instr::CmpEq { dst, lhs, rhs },
        "cmp_ne" => Instr::CmpNe { dst, lhs, rhs },
        "cmp_ge" => Instr::CmpGe { dst, lhs, rhs },
        "cmp_le" => Instr::CmpLe { dst, lhs, rhs },
        _ => return Err(Pl0Error::codegen_error(format!("Unknown relational op: {}", op))),
    };
    gen.push_instr(instr);
    Ok(())
}

pub fn emit_is_odd(gen: &mut IRGenerator, dest: &str, src: &str) -> Pl0Result<()> {
    let dst = parse_vreg(dest)?;
    let src = value(src)?;
    gen.push_instr(Instr::IsOdd { dst, src });
    Ok(())
}

pub fn emit_branch_if_zero(gen: &mut IRGenerator, vreg: &str, label: &str) -> Pl0Result<()> {
    let cond = value(vreg)?;
    gen.push_instr(Instr::Beqz { cond, target: label.to_string() });
    Ok(())
}

pub fn emit_jump(gen: &mut IRGenerator, label: &str) -> Pl0Result<()> {
    gen.push_instr(Instr::Jump { target: label.to_string() });
    Ok(())
}

pub fn emit_call(gen: &mut IRGenerator, label: &str, static_link_hops: usize) -> Pl0Result<()> {
    gen.push_instr(Instr::Call { label: label.to_string(), static_link_hops });
    Ok(())
}

pub fn emit_proc_enter(gen: &mut IRGenerator, stack_size: usize) -> Pl0Result<()> {
    gen.push_instr(Instr::ProcEnter { stack_size });
    Ok(())
}

pub fn emit_proc_exit(gen: &mut IRGenerator) -> Pl0Result<()> {
    gen.push_instr(Instr::ProcExit);
    Ok(())
}

pub fn emit_write_int(gen: &mut IRGenerator, src: &str) -> Pl0Result<()> {
    let src = value(src)?;
    gen.push_instr(Instr::WriteInt { src });
    Ok(())
}

pub fn emit_write_str(gen: &mut IRGenerator, text: &str) -> Pl0Result<()> {
    gen.push_instr(Instr::WriteStr { text: text.to_string() });
    Ok(())
}

pub fn emit_read_int(gen: &mut IRGenerator, dest: &str) -> Pl0Result<()> {
    let dst = parse_vreg(dest)?;
    gen.push_instr(Instr::ReadInt { dst });
    Ok(())
}

/// Escape hatch for an instruction not modeled as a proper `Instr` variant
/// (today: just `la`, loading a procedure's address - see the note on
/// `Instr::Raw`).
pub fn emit_raw(gen: &mut IRGenerator, instruction: &str) -> Pl0Result<()> {
    gen.push_instr(Instr::Raw(instruction.to_string()));
    Ok(())
}

/// Starts a new basic block within the function currently being built -
/// used for in-function control-flow labels (if/while targets). Procedure
/// and `main` entries are a different action, `IRGenerator::start_function`,
/// since those also close out the *previous* function; they no longer go
/// through this generic label path.
pub fn emit_label(gen: &mut IRGenerator, label: &str) -> Pl0Result<()> {
    gen.start_block(label);
    Ok(())
}

pub fn create_and_emit_label(gen: &mut IRGenerator) -> Pl0Result<String> {
    let label = gen.create_label();
    emit_label(gen, &label)?;
    Ok(label)
}
