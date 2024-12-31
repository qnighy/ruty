use crate::ast::CodeRange;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    pub range: CodeRange,

    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Integer(IntegerType),
    String(StringType),
}

impl_from!(
    (Type, IntegerType, Type::Integer),
    (Type, StringType, Type::String),
);

impl_delegators!(
    enum Type {
        Integer(IntegerType),
        String(StringType),
    }
    range (mut range_mut): CodeRange,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerType {
    pub range: CodeRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringType {
    pub range: CodeRange,
}
