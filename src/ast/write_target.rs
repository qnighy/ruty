use crate::ast::{CodeRange, TypeAnnotation};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WriteTarget {
    LocalVariable(LocalVariableWriteTarget),
}

impl_from!((
    WriteTarget,
    LocalVariableWriteTarget,
    WriteTarget::LocalVariable
),);

impl_delegators!(
    enum WriteTarget {
        LocalVariable(LocalVariableWriteTarget),
    }
    range (mut range_mut): CodeRange,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalVariableWriteTarget {
    pub range: CodeRange,

    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
}
