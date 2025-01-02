use crate::ast::{CodeRange, Paren, StmtList, TypeAnnotation, WriteTarget};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Seq(SeqExpr),
    Nil(NilExpr),
    False(FalseExpr),
    True(TrueExpr),
    Integer(IntegerExpr),
    LocalVariable(LocalVariableExpr),
    Write(WriteExpr),
    Error(ErrorExpr),
}

impl_from!(
    (Expr, SeqExpr, Expr::Seq),
    (Expr, NilExpr, Expr::Nil),
    (Expr, FalseExpr, Expr::False),
    (Expr, TrueExpr, Expr::True),
    (Expr, IntegerExpr, Expr::Integer),
    (Expr, LocalVariableExpr, Expr::LocalVariable),
    (Expr, WriteExpr, Expr::Write),
    (Expr, ErrorExpr, Expr::Error),
);
impl_delegators!(
    enum Expr {
        Seq(SeqExpr),
        Nil(NilExpr),
        False(FalseExpr),
        True(TrueExpr),
        Integer(IntegerExpr),
        LocalVariable(LocalVariableExpr),
        Write(WriteExpr),
        Error(ErrorExpr),
    }
    range (mut range_mut): CodeRange,
    parens (mut parens_mut): Vec<Paren>,
);

impl Expr {
    pub fn outer_range(&self) -> &CodeRange {
        if let Some(last_paren) = self.parens().last() {
            &last_paren.range
        } else {
            self.range()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeqExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub paren: SeqParen,
    pub stmt_list: StmtList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeqParen {
    pub kind: SeqParenKind,
    pub open_range: CodeRange,
    pub close_range: CodeRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SeqParenKind {
    Paren,
    BeginEnd,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FalseExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TrueExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub value: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalVariableExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WriteExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub lhs: Box<WriteTarget>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}
