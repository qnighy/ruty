use crate::ast::{CodeRange, Expr};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub range: CodeRange,

    pub expr: Expr,

    pub semi: Vec<CodeRange>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StmtList {
    pub range: CodeRange,

    pub semi_prefix: Vec<CodeRange>,

    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program {
    pub range: CodeRange,

    pub stmt_list: StmtList,
}
