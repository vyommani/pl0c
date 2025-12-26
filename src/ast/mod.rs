
// Abstract Syntax Tree definitions for PL/0 compiler
// This module contains all AST node types and related traits.

mod traits;
pub use traits::{Node, ExpressionNode};

// AST node modules
mod program;
mod block;
mod declarations;
mod statements;
mod expressions;
mod literals;
mod io;

pub use program::Program;
pub use block::Block;
pub use declarations::{ConstDecl, VarDecl, ProcDecl};
pub use statements::{AssignStmt, BeginStmt, CallStmt, IfStmt, WhileStatement, Exit};
pub use expressions::{BinOp, OddCondition, RelationalCondition};
pub use literals::{Ident, Number, Variable};
pub use io::{Read, Write, WriteStr};