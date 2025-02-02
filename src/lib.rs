pub mod ast;
mod checker;
mod checker2;
pub mod encoding;
mod eraser;
mod iseq;
mod liveness;
mod parser;
mod util;

use crate::ast::CodeRange;
pub use checker::typecheck_program;
pub use checker2::typecheck_program as typecheck_program2;
pub use encoding::{CharPlus, EString, Encoding};
pub use eraser::erase_type;
use liveness::liveness_analysis;
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
