use crate::ast::CodeRange;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    pub range: CodeRange,

    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil(NilType),
    Integer(IntegerType),
    String(StringType),
    Error(ErrorType),
}

impl_from!(
    (Type, NilType, Type::Nil),
    (Type, IntegerType, Type::Integer),
    (Type, StringType, Type::String),
    (Type, ErrorType, Type::Error),
);

impl_delegators!(
    enum Type {
        Nil(NilType),
        Integer(IntegerType),
        String(StringType),
        Error(ErrorType),
    }
    range (mut range_mut): CodeRange,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilType {
    pub range: CodeRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerType {
    pub range: CodeRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringType {
    pub range: CodeRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorType {
    pub range: CodeRange,
}
