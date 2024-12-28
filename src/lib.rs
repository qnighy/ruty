mod parser;

pub use parser::{parse_expr, parse_type};

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
    LocalVariable(LocalVariableExpr),
    Integer(IntegerExpr),
    Write(WriteExpr),
}

impl_from!(
    (Expr, LocalVariableExpr, Expr::LocalVariable),
    (Expr, IntegerExpr, Expr::Integer),
    (Expr, WriteExpr, Expr::Write),
);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalVariableExpr {
    pub name: String,
    pub type_annotation: Option<Type>,
}

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

impl_from!((
    WriteTarget,
    LocalVariableWriteTarget,
    WriteTarget::LocalVariable
),);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalVariableWriteTarget {
    pub name: String,
    pub type_annotation: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Integer(IntegerType),
    String(StringType),
}

impl_from!(
    (Type, IntegerType, Type::Integer),
    (Type, StringType, Type::String),
);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntegerType {}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringType {}
