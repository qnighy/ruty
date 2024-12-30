use std::ops::Range;

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

macro_rules! impl_delegators {
    // (
    //     enum $enum_name:ident {
    //         $($enum_decl_body:tt)*
    //     }
    //     $(
    //         fn $meth_name:ident($($args:tt)*) -> $ret:ty;
    //     )*
    //     $(
    //         $field_name:ident (mut $field_name_mut:ident): $field_ty:ty,
    //     )*
    // ) => {
    //     impl $enum_name {
    //         // $(
    //         //     empty!($meth_name);
    //         //     _impl_method_delegator!(
    //         //         enum $enum_name {
    //         //             $($enum_decl_body)*
    //         //         }
    //         //         fn $meth_name($($args)*) -> $ret;
    //         //     );
    //         // )*
    //         $(
    //             _impl_field_delegator!(
    //                 enum $enum_name {
    //                     $($enum_decl_body)*
    //                 }
    //                 $field_name (mut $field_name_mut): $field_ty,
    //             );
    //         )*
    //     }
    // };
    (
        enum $enum_name:ident {
            $(
                $constr_name:ident($subtype:ty),
            )*
        }
        fn $meth_name:ident($($args:tt)*) -> $ret:ty;
        $($rest:tt)*
    ) => {
        impl $enum_name {
            pub fn $meth_name($($args)*) -> $ret {
                match self {
                    $(
                        $enum_name::$constr_name(sub) => sub.$meth_name($($args)*),
                    )*
                }
            }
        }
        impl_delegators!(
            enum $enum_name {
                $($constr_name($subtype),)*
            }
            $($rest)*
        );
    };
    (
        enum $enum_name:ident {
            $(
                $constr_name:ident($subtype:ty),
            )*
        }
        $field_name:ident (mut $field_name_mut:ident): $field_ty:ty,
        $($rest:tt)*
    ) => {
        impl $enum_name {
            pub fn $field_name(&self) -> &$field_ty {
                match self {
                    $(
                        $enum_name::$constr_name(sub) => &sub.$field_name,
                    )*
                }
            }
            pub fn $field_name_mut(&mut self) -> &mut $field_ty {
                match self {
                    $(
                        $enum_name::$constr_name(sub) => &mut sub.$field_name,
                    )*
                }
            }
        }
    };
    (
        enum $enum_name:ident {
            $($enum_decl_body:tt)*
        }
    ) => {};
}

macro_rules! _impl_method_delegator {
    (
        enum $enum_name:ident {
            $(
                $constr_name:ident($subtype:ty),
            )*
        }
        fn $meth_name:ident($($args:tt)*) -> $ret:ty;
    ) => {
        pub fn $meth_name($($args)*) -> $ret {
            match self {
                $(
                    $enum_name::$constr_name(sub) => sub.$meth_name($($args)*),
                )*
            }
        }
    };
}

macro_rules! _impl_field_delegator {
    (
        enum $enum_name:ident {
            $(
                $constr_name:ident($subtype:ty),
            )*
        }
        $field_name:ident (mut $field_name_mut:ident): $field_ty:ty,
    ) => {
        pub fn $field_name(&self) -> &$field_ty {
            match self {
                $(
                    $enum_name::$constr_name(sub) => &sub.$field_name,
                )*
            }
        }
        pub fn $field_name_mut(&mut self) -> &mut $field_ty {
            match self {
                $(
                    $enum_name::$constr_name(sub) => &mut sub.$field_name,
                )*
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CodeRange {
    pub start: usize,
    pub end: usize,
}
impl CodeRange {
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
}
impl From<Range<usize>> for CodeRange {
    fn from(range: Range<usize>) -> Self {
        CodeRange {
            start: range.start,
            end: range.end,
        }
    }
}
impl From<CodeRange> for Range<usize> {
    fn from(range: CodeRange) -> Self {
        range.start..range.end
    }
}
pub const DUMMY_RANGE: CodeRange = CodeRange { start: 0, end: 0 };

// For testing
pub fn pos_in(all_text: &[u8], sub_text: &[u8]) -> CodeRange {
    pos_in_at(all_text, sub_text, 0)
}

// For testing
pub fn pos_in_at(all_text: &[u8], sub_text: &[u8], mut idx: usize) -> CodeRange {
    // find substring
    for (i, s) in all_text.windows(sub_text.len()).enumerate() {
        if s == sub_text {
            if idx == 0 {
                return CodeRange {
                    start: i,
                    end: i + sub_text.len(),
                };
            } else {
                idx -= 1;
            }
        }
    }
    panic!("substring not found: {:?}", sub_text);
}

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
    pub type_annotation: Option<Type>,
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
    pub type_annotation: Option<Type>,
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
