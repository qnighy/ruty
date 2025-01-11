use crate::{
    ast::{CodeRange, Expr},
    EString,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub range: CodeRange,

    pub expr: Expr,

    pub semi: Vec<Semicolon>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StmtList {
    pub range: CodeRange,

    pub semi_prefix: Vec<Semicolon>,

    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Semicolon {
    pub range: CodeRange,
    pub kind: SemicolonKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemicolonKind {
    Semicolon,
    Newline,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program {
    pub range: CodeRange,

    pub locals: Vec<EString>,
    pub stmt_list: StmtList,
}
