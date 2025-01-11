use crate::{
    ast::{CodeRange, TypeAnnotation},
    EString,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WriteTarget {
    LocalVariable(LocalVariableWriteTarget),
    Error(ErrorWriteTarget),
}

impl_from!(
    (
        WriteTarget,
        LocalVariableWriteTarget,
        WriteTarget::LocalVariable
    ),
    (WriteTarget, ErrorWriteTarget, WriteTarget::Error),
);

impl_delegators!(
    enum WriteTarget {
        LocalVariable(LocalVariableWriteTarget),
        Error(ErrorWriteTarget),
    }
    range (mut range_mut): CodeRange,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalVariableWriteTarget {
    pub range: CodeRange,

    pub name: EString,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorWriteTarget {
    pub range: CodeRange,
}
