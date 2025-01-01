use crate::ast::CodeRange;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Paren {
    pub range: CodeRange,
    pub open_range: CodeRange,
    pub close_range: CodeRange,
}
