#[macro_export]
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
        impl_delegators!(
            enum $enum_name {
                $($constr_name($subtype),)*
            }
            $($rest)*
        );
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
