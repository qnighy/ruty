pub mod ast;
mod checker;
mod encoding;
mod eraser;
mod parser;

use crate::ast::CodeRange;
pub use checker::typecheck_program;
pub use encoding::{CharPlus, EString, Encoding};
pub use eraser::erase_type;
pub use parser::{parse, parse_expr, parse_type};

#[derive(Debug, Clone, Default)]
pub struct Ctx {
    pub fresh_id: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic {
    pub range: CodeRange,
    pub message: String,
}
