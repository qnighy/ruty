mod parser;

pub use parser::parse;

#[derive(Debug, Clone, Default)]
pub struct Ctx {
    pub fresh_id: u32,
}

macro_rules! impl_from {
    ($(($to:ty, $from:ty, $fn:expr),)*) => {
        $(
            impl From<$from> for $to {
                fn from(val: $from) -> Self {
                    $fn(val)
                }
            }
        )*
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    Integer(IntegerExpr),
    Write(WriteExpr),
}

impl_from!(
    (Expr, IntegerExpr, Expr::Integer),
    (Expr, WriteExpr, Expr::Write),
);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntegerExpr {
    pub value: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WriteExpr {
    pub lhs: Box<WriteTarget>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum WriteTarget {
    LocalVariable(LocalVariableWriteTarget),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalVariableWriteTarget {
    pub name: String,
    pub type_annotation: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Integer,
}
