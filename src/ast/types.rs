use crate::ast::CodeRange;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    pub range: CodeRange,

    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Nil(NilType),
    False(FalseType),
    True(TrueType),
    Integer(IntegerType),
    String(StringType),
    Regexp(RegexpType),
    Error(ErrorType),
}

impl_from!(
    (Type, NilType, Type::Nil),
    (Type, FalseType, Type::False),
    (Type, TrueType, Type::True),
    (Type, IntegerType, Type::Integer),
    (Type, StringType, Type::String),
    (Type, RegexpType, Type::Regexp),
    (Type, ErrorType, Type::Error),
);

impl_delegators!(
    enum Type {
        Nil(NilType),
        False(FalseType),
        True(TrueType),
        Integer(IntegerType),
        String(StringType),
        Regexp(RegexpType),
        Error(ErrorType),
    }
    range (mut range_mut): CodeRange,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilType {
    pub range: CodeRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FalseType {
    pub range: CodeRange,
}

// Unrelated to the predecessor of OpenType.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TrueType {
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
pub struct RegexpType {
    pub range: CodeRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorType {
    pub range: CodeRange,
}
