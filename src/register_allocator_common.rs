use std::fmt;

#[derive(Debug)]
pub struct RegisterConstraints {
    pub can_allocate: bool,
    pub can_spill: bool,
    pub special_purpose: &'static str,
}

#[derive(Debug)]
pub enum RegisterError {
    UnknownRegister(String),
    InvalidInstruction(String),
    NoRegistersAvailable,
    SpillFailed,
    RegisterConstraintViolation(String),
    StackFrameError(String),
    OutputError(String),
}

impl fmt::Display for RegisterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegisterError::UnknownRegister(reg) => write!(f, "Unknown register: {}", reg),
            RegisterError::InvalidInstruction(inst) => write!(f, "Invalid instruction: {}", inst),
            RegisterError::NoRegistersAvailable => {
                write!(f, "No registers available for allocation")
            }
            RegisterError::SpillFailed => write!(f, "Failed to spill register to memory"),
            RegisterError::RegisterConstraintViolation(msg) => {
                write!(f, "Register constraint violation: {}", msg)
            }
            RegisterError::StackFrameError(msg) => write!(f, "Stack frame error: {}", msg),
            RegisterError::OutputError(msg) => write!(f, "Output error: {}", msg),
        }
    }
}

impl std::error::Error for RegisterError {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Register<RN> {
    pub p_reg: usize,
    pub v_reg: usize,
    pub name: RN,
    pub next_uses: Vec<i32>,
    pub address: i64,
    pub spill_offset: Option<i32>, // Spill slot offset (None if not spilled)
    pub live_across_call: bool,    // true if vreg is live across a call
}

impl<RN: Clone + Eq> Register<RN> {
    pub fn new(p_reg: usize, v_reg: usize, name: RN, next_uses: Vec<i32>, address: i64) -> Self {
        Self {
            p_reg,
            v_reg,
            name,
            next_uses,
            address,
            spill_offset: None,
            live_across_call: false,
        }
    }

    pub fn is_free(&self) -> bool {
        self.v_reg == usize::MAX
    }
}
