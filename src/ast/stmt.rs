use crate::{
    ast::{CodeRange, Expr},
    EString,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub range: CodeRange,

    pub expr: Expr,

    pub semi: Vec<StmtSep>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StmtList {
    pub range: CodeRange,

    pub semi_prefix: Vec<StmtSep>,

    pub stmts: Vec<Stmt>,
}

/// Operators separating statements.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StmtSep {
    pub range: CodeRange,
    pub kind: StmtSepKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StmtSepKind {
    /// Explicit semicolon `;`.
    Semicolon,
    /// Unexpected comma `,` interpreted as a semicolon during error recovery.
    InvalidComma,
    /// Newline token.
    Newline,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program {
    pub range: CodeRange,

    pub locals: Vec<EString>,
    pub stmt_list: StmtList,
}
