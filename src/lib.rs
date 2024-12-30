pub mod ast;
mod checker;
mod parser;

pub use checker::typecheck_expr;
pub use parser::{parse_expr, parse_type};

#[derive(Debug, Clone, Default)]
pub struct Ctx {
    pub fresh_id: u32,
}
