use crate::ast::{CodeRange, TypeAnnotation, WriteTarget};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    LocalVariable(LocalVariableExpr),
    Integer(IntegerExpr),
    Write(WriteExpr),
}

impl_from!(
    (Expr, LocalVariableExpr, Expr::LocalVariable),
    (Expr, IntegerExpr, Expr::Integer),
    (Expr, WriteExpr, Expr::Write),
);
impl_delegators!(
    enum Expr {
        LocalVariable(LocalVariableExpr),
        Integer(IntegerExpr),
        Write(WriteExpr),
    }
    range (mut range_mut): CodeRange,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalVariableExpr {
    pub range: CodeRange,

    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerExpr {
    pub range: CodeRange,

    pub value: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WriteExpr {
    pub range: CodeRange,

    pub lhs: Box<WriteTarget>,
    pub rhs: Box<Expr>,
}
