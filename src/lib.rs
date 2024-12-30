pub mod ast;
mod checker;
mod parser;

use crate::ast::CodeRange;
pub use checker::typecheck_expr;
pub use parser::{parse_expr, parse_type};

#[derive(Debug, Clone, Default)]
pub struct Ctx {
    pub fresh_id: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic {
    pub range: CodeRange,
    pub message: String,
}
