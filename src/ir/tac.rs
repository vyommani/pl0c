
/// A virtual register: `VReg(5)` is today's `"v5"`. Never reused once
/// assigned (mirrors `IRGenerator::allocate_virtual_register`), so within a
/// single basic block this is already in SSA form.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VReg(pub u32);

/// An operand that's either a virtual register or a literal value - replaces
/// passing `&str` operands around and re-parsing them to tell which they
/// were (today, a literal and a vreg are both just strings; telling them
/// apart requires inspecting the text, e.g. `vreg.starts_with(&gen.vreg_prefix)`
/// in expression_handlers.rs).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    VReg(VReg),
    Imm(i64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemLoc {
    /// `[bp-offset]` in the current frame.
    Local { offset: usize },
    /// `[up-offset-distance]`: `distance` static-link hops up, then `offset`
    /// into that frame.
    Outer { offset: usize, distance: usize },
    /// A global variable or constant, referenced by name.
    Global(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    Li { dst: VReg, value: i64 },
    Ld { dst: VReg, src: MemLoc },
    St { dst: MemLoc, src: Value },

    Add { dst: VReg, lhs: Value, rhs: Value },
    Sub { dst: VReg, lhs: Value, rhs: Value },
    Mul { dst: VReg, lhs: Value, rhs: Value },
    Div { dst: VReg, lhs: Value, rhs: Value },
    Mod { dst: VReg, lhs: Value, rhs: Value },

    CmpGt { dst: VReg, lhs: Value, rhs: Value },
    CmpLt { dst: VReg, lhs: Value, rhs: Value },
    CmpLe { dst: VReg, lhs: Value, rhs: Value },
    CmpGe { dst: VReg, lhs: Value, rhs: Value },
    CmpEq { dst: VReg, lhs: Value, rhs: Value },
    CmpNe { dst: VReg, lhs: Value, rhs: Value },
    IsOdd { dst: VReg, src: Value },

    /// Branch to `target` if `cond` is zero.
    Beqz { cond: Value, target: String },
    Jump { target: String },
    /// Call `label`; `static_link_hops` is how many lexical levels up the
    /// callee's static link must walk from the caller's frame.
    Call { label: String, static_link_hops: usize },

    ProcEnter { stack_size: usize },
    ProcExit,
    Exit { code: i32 },

    WriteInt { src: Value },
    WriteStr { text: String },
    ReadInt { dst: VReg },

    Raw(String),
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instrs: Vec<Instr>,
}

/// One procedure (or the `main` block), as a sequence of labeled blocks.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub blocks: Vec<BasicBlock>,
}

/// A whole compiled program: global declarations plus every function.
#[derive(Debug, Clone, Default)]
pub struct TacModule {
    pub consts: Vec<(String, i64)>,
    pub globals: Vec<String>,
    /// Instructions emitted before any function/block was ever opened -
    /// e.g. the auto-appended `exit 0` for a program with no statement at
    /// all. The old flat-text IR had no notion of "no block open yet", so
    /// anything emitted that early just became plain unlabeled lines;
    /// this preserves that instead of silently dropping them (see
    /// `IRGenerator::push_instr`).
    pub preamble: Vec<Instr>,
    pub functions: Vec<Function>,
}

impl VReg {
    fn render(&self) -> String {
        format!("v{}", self.0)
    }
}

impl Value {
    fn render(&self) -> String {
        match self {
            Value::VReg(r) => r.render(),
            Value::Imm(n) => n.to_string(),
        }
    }
}

impl MemLoc {
    fn render(&self) -> String {
        match self {
            MemLoc::Local { offset } => format!("bp-{}", offset),
            MemLoc::Outer { offset, distance } => format!("up-{}-{}", offset, distance),
            MemLoc::Global(name) => name.clone(),
        }
    }
}

impl Instr {
    /// Renders this instruction exactly as `StringCodeEmitter` does today,
    /// so existing backends (which re-parse that text) keep working
    /// unchanged while `IRGenerator` is migrated to build `Instr`s.
    pub fn render(&self) -> String {
        use Instr::*;
        let body = match self {
            Li { dst, value } => format!("li {}, {}", dst.render(), value),
            Ld { dst, src } => format!("ld {}, [{}]", dst.render(), src.render()),
            St { dst, src } => format!("st [{}], {}", dst.render(), src.render()),

            Add { dst, lhs, rhs } => format!("add {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            Sub { dst, lhs, rhs } => format!("sub {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            Mul { dst, lhs, rhs } => format!("mul {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            Div { dst, lhs, rhs } => format!("div {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            Mod { dst, lhs, rhs } => format!("mod {}, {}, {}", dst.render(), lhs.render(), rhs.render()),

            CmpGt { dst, lhs, rhs } => format!("cmp_gt {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            CmpLt { dst, lhs, rhs } => format!("cmp_lt {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            CmpLe { dst, lhs, rhs } => format!("cmp_le {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            CmpGe { dst, lhs, rhs } => format!("cmp_ge {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            CmpEq { dst, lhs, rhs } => format!("cmp_eq {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            CmpNe { dst, lhs, rhs } => format!("cmp_ne {}, {}, {}", dst.render(), lhs.render(), rhs.render()),
            IsOdd { dst, src } => format!("is_odd {}, {}", dst.render(), src.render()),

            Beqz { cond, target } => format!("beqz {}, {}", cond.render(), target),
            Jump { target } => format!("jump {}", target),
            Call { label, static_link_hops } => format!("call {}, {}", label, static_link_hops),

            ProcEnter { stack_size } => format!("proc_enter {}", stack_size),
            ProcExit => "proc_exit".to_string(),
            Exit { code } => format!("exit {}", code),

            WriteInt { src } => format!("write_int {}", src.render()),
            WriteStr { text } => format!("write_str \"{}\"", text),
            ReadInt { dst } => format!("read_int {}", dst.render()),
            Raw(text) => text.clone(),
        };
        // emit_label() is the one exception that isn't indented; every other
        // line gets the 4-space indent StringCodeEmitter::emit() adds.
        format!("    {}", body)
    }
}

impl BasicBlock {
    fn render(&self, out: &mut String) {
        out.push_str(&self.label);
        out.push_str(":\n");
        for instr in &self.instrs {
            out.push_str(&instr.render());
            out.push('\n');
        }
    }
}

impl Function {
    fn render(&self, out: &mut String) {
        for block in &self.blocks {
            block.render(out);
        }
    }
}

impl TacModule {
    /// Renders the whole module back to today's text IR format - the
    /// bridge that lets `IRGenerator` build `Instr`s while every existing
    /// backend keeps consuming text, unchanged, until it's migrated.
    pub fn render(&self) -> String {
        let mut out = String::new();
        for (name, value) in &self.consts {
            out.push_str(&format!("    const {} = {}\n", name, value));
        }
        for name in &self.globals {
            out.push_str(&format!("    var {}\n", name));
        }
        for instr in &self.preamble {
            out.push_str(&instr.render());
            out.push('\n');
        }
        for function in &self.functions {
            function.render(&mut out);
        }
        out
    }
}
