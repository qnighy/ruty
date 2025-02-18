use crate::ast::{CodeRange, Expr, Paren};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgList {
    pub range: CodeRange,
    pub paren: Option<Paren>,

    pub args: Vec<Arg>,
}

impl ArgList {
    pub fn outer_range(&self) -> &CodeRange {
        if let Some(paren) = &self.paren {
            &paren.range()
        } else {
            &self.range
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Arg {
    Expr(ExprArg),
}

impl_from!((Arg, ExprArg, Arg::Expr),);
impl_delegators!(
    enum Arg {
        Expr(ExprArg),
    }
    range (mut range_mut): CodeRange,
    comma (mut comma_mut): Option<CodeRange>,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprArg {
    pub range: CodeRange,
    pub comma: Option<CodeRange>,

    pub expr: Box<Expr>,
}
