use std::ops::Range;
use std::str;

use bit_vec::BitVec;

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
pub struct PositionIndex {
    entries: Vec<PositionEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PositionEntry {
    pos: usize,
    rc: (usize, usize),
    skip: BitVec,
}

impl PositionIndex {
    pub fn new(s: &[u8]) -> Self {
        Self::with_chunk_size(s, 32)
    }

    pub fn with_chunk_size(s: &[u8], chunk_size: usize) -> Self {
        let mut entries = Vec::<PositionEntry>::new();
        let mut start_rc = (0_usize, 0_usize);
        let mut start_pos = 0_usize;
        let mut rc = (0_usize, 0_usize);
        let mut pos = 0_usize;
        let mut skip = BitVec::new();
        while pos < s.len() {
            let len = match s[pos] {
                0x00..=0x7F => 1,
                0xC0..=0xDF => 2,
                0xE0..=0xEF => 3,
                0xF0..=0xF7 => 4,
                _ => 1,
            };
            let len = len.min(s.len() - pos);
            let len = if str::from_utf8(&s[pos..pos + len]).is_ok() {
                len
            } else {
                1
            };

            if len > 1 {
                while skip.len() < pos - start_pos {
                    skip.push(false);
                }
            }
            match len {
                1 => {
                    // Defer until multi-byte character appears
                }
                2 => {
                    skip.push(true);
                    skip.push(false);
                }
                3 => {
                    skip.push(true);
                    skip.push(true);
                    skip.push(false);
                }
                4 => {
                    // Count as two characters for UTF-16 position encoding
                    skip.push(true);
                    skip.push(false);
                    skip.push(true);
                    skip.push(false);
                }
                _ => unreachable!(),
            }

            let need_flush = s[pos] == b'\n' || skip.len() > chunk_size;

            if s[pos] == b'\n' {
                rc.0 += 1;
                rc.1 = 0;
            } else if len == 4 {
                rc.1 += 2;
            } else {
                rc.1 += 1;
            }
            pos += len;

            if need_flush {
                entries.push(PositionEntry {
                    pos: start_pos,
                    rc: start_rc,
                    skip: skip.clone(),
                });
                start_rc = rc;
                start_pos = pos;
                skip.truncate(0);
            }
        }
        {
            // flush
            entries.push(PositionEntry {
                pos: start_pos,
                rc: start_rc,
                skip: skip.clone(),
            });
        }
        Self { entries }
    }

    pub fn rc_of(&self, pos: usize) -> (usize, usize) {
        let entry_index = self.entries.partition_point(|entry| entry.pos <= pos);
        let entry = &self.entries[entry_index.max(1) - 1];
        let relpos = pos - entry.pos;
        if entry.skip.is_empty() {
            // Fast path for ASCII case
            return (entry.rc.0, entry.rc.1 + relpos);
        }

        let relcol = if relpos < entry.skip.len() {
            (0..relpos).filter(|&i| !entry.skip[i]).count()
        } else {
            entry.skip.iter().filter(|&b| !b).count() + (relpos - entry.skip.len())
        };
        (entry.rc.0, entry.rc.1 + relcol)
    }

    pub fn pos_of(&self, rc: (usize, usize)) -> usize {
        let entry_index = self.entries.partition_point(|entry| entry.rc <= rc);
        let entry = &self.entries[entry_index.max(1) - 1];
        // Use saturation just in case (it should not happen though)
        let relcol = rc.1.saturating_sub(entry.rc.1);
        if entry.skip.is_empty() {
            // Fast path for ASCII case
            return entry.pos + relcol;
        }

        let mut relpos = 0;
        let mut relcol_left = relcol;
        while relpos < entry.skip.len() && relcol_left > 0 {
            if !entry.skip[relpos] {
                relcol_left -= 1;
            }
            relpos += 1;
        }
        entry.pos + relpos + relcol_left
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rc_of_ascii() {
        helper_test_rc_of(
            "foo bar\nbaz\nfoo bar foo bar foo bar\n".as_bytes(),
            vec![
                (0, (0, 0)),   // 'f'
                (1, (0, 1)),   // 'o'
                (2, (0, 2)),   // 'o'
                (3, (0, 3)),   // ' '
                (4, (0, 4)),   // 'b'
                (5, (0, 5)),   // 'a'
                (6, (0, 6)),   // 'r'
                (7, (0, 7)),   // '\n'
                (8, (1, 0)),   // 'b'
                (9, (1, 1)),   // 'a'
                (10, (1, 2)),  // 'z'
                (11, (1, 3)),  // '\n'
                (12, (2, 0)),  // 'f'
                (13, (2, 1)),  // 'o'
                (14, (2, 2)),  // 'o'
                (15, (2, 3)),  // ' '
                (16, (2, 4)),  // 'b'
                (17, (2, 5)),  // 'a'
                (18, (2, 6)),  // 'r'
                (19, (2, 7)),  // ' '
                (20, (2, 8)),  // 'f'
                (21, (2, 9)),  // 'o'
                (22, (2, 10)), // 'o'
                (23, (2, 11)), // ' '
                (24, (2, 12)), // 'b'
                (25, (2, 13)), // 'a'
                (26, (2, 14)), // 'r'
                (27, (2, 15)), // ' '
                (28, (2, 16)), // 'f'
                (29, (2, 17)), // 'o'
                (30, (2, 18)), // 'o'
                (31, (2, 19)), // ' '
                (32, (2, 20)), // 'b'
                (33, (2, 21)), // 'a'
                (34, (2, 22)), // 'r'
                (35, (2, 23)), // '\n'
                (36, (3, 0)),  // EOF
            ],
        );
    }

    #[test]
    fn test_rc_of_non_ascii() {
        helper_test_rc_of(
            "ωん🍺\nαβγδζηιあいうえおかきくけこ🍺🍻🍼🍽\n".as_bytes(),
            vec![
                (0, (0, 0)),   // 'ω'
                (2, (0, 1)),   // 'ん'
                (5, (0, 2)),   // '🍺'
                (9, (0, 4)),   // '\n'
                (10, (1, 0)),  // 'α'
                (12, (1, 1)),  // 'β'
                (14, (1, 2)),  // 'γ'
                (16, (1, 3)),  // 'δ'
                (18, (1, 4)),  // 'ζ'
                (20, (1, 5)),  // 'η'
                (22, (1, 6)),  // 'ι'
                (24, (1, 7)),  // 'あ'
                (27, (1, 8)),  // 'い'
                (30, (1, 9)),  // 'う'
                (33, (1, 10)), // 'え'
                (36, (1, 11)), // 'お'
                (39, (1, 12)), // 'か'
                (42, (1, 13)), // 'き'
                (45, (1, 14)), // 'く'
                (48, (1, 15)), // 'け'
                (51, (1, 16)), // 'こ'
                (54, (1, 17)), // '🍺'
                (58, (1, 19)), // '🍻'
                (62, (1, 21)), // '🍼'
                (66, (1, 23)), // '🍽'
                (70, (1, 25)), // '\n'
                (71, (2, 0)),  // EOF
            ],
        );
    }

    fn helper_test_rc_of(s: &[u8], expected: Vec<(usize, (usize, usize))>) {
        let index = PositionIndex::new(s);
        let actual = expected
            .iter()
            .map(|&(pos, _)| {
                let rc = index.rc_of(pos);
                (pos, rc)
            })
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_pos_of_ascii() {
        helper_test_pos_of(
            "foo bar\nbaz\nfoo bar foo bar foo bar\n".as_bytes(),
            vec![
                (0, (0, 0)),   // 'f'
                (1, (0, 1)),   // 'o'
                (2, (0, 2)),   // 'o'
                (3, (0, 3)),   // ' '
                (4, (0, 4)),   // 'b'
                (5, (0, 5)),   // 'a'
                (6, (0, 6)),   // 'r'
                (7, (0, 7)),   // '\n'
                (8, (1, 0)),   // 'b'
                (9, (1, 1)),   // 'a'
                (10, (1, 2)),  // 'z'
                (11, (1, 3)),  // '\n'
                (12, (2, 0)),  // 'f'
                (13, (2, 1)),  // 'o'
                (14, (2, 2)),  // 'o'
                (15, (2, 3)),  // ' '
                (16, (2, 4)),  // 'b'
                (17, (2, 5)),  // 'a'
                (18, (2, 6)),  // 'r'
                (19, (2, 7)),  // ' '
                (20, (2, 8)),  // 'f'
                (21, (2, 9)),  // 'o'
                (22, (2, 10)), // 'o'
                (23, (2, 11)), // ' '
                (24, (2, 12)), // 'b'
                (25, (2, 13)), // 'a'
                (26, (2, 14)), // 'r'
                (27, (2, 15)), // ' '
                (28, (2, 16)), // 'f'
                (29, (2, 17)), // 'o'
                (30, (2, 18)), // 'o'
                (31, (2, 19)), // ' '
                (32, (2, 20)), // 'b'
                (33, (2, 21)), // 'a'
                (34, (2, 22)), // 'r'
                (35, (2, 23)), // '\n'
                (36, (3, 0)),  // EOF
            ],
        );
    }

    #[test]
    fn test_pos_of_non_ascii() {
        helper_test_pos_of(
            "ωん🍺\nαβγδζηιあいうえおかきくけこ🍺🍻🍼🍽\n".as_bytes(),
            vec![
                (0, (0, 0)),   // 'ω'
                (2, (0, 1)),   // 'ん'
                (5, (0, 2)),   // '🍺'
                (9, (0, 4)),   // '\n'
                (10, (1, 0)),  // 'α'
                (12, (1, 1)),  // 'β'
                (14, (1, 2)),  // 'γ'
                (16, (1, 3)),  // 'δ'
                (18, (1, 4)),  // 'ζ'
                (20, (1, 5)),  // 'η'
                (22, (1, 6)),  // 'ι'
                (24, (1, 7)),  // 'あ'
                (27, (1, 8)),  // 'い'
                (30, (1, 9)),  // 'う'
                (33, (1, 10)), // 'え'
                (36, (1, 11)), // 'お'
                (39, (1, 12)), // 'か'
                (42, (1, 13)), // 'き'
                (45, (1, 14)), // 'く'
                (48, (1, 15)), // 'け'
                (51, (1, 16)), // 'こ'
                (54, (1, 17)), // '🍺'
                (58, (1, 19)), // '🍻'
                (62, (1, 21)), // '🍼'
                (66, (1, 23)), // '🍽'
                (70, (1, 25)), // '\n'
                (71, (2, 0)),  // EOF
            ],
        );
    }

    fn helper_test_pos_of(s: &[u8], expected: Vec<(usize, (usize, usize))>) {
        let index = PositionIndex::new(s);
        let actual = expected
            .iter()
            .map(|&(_, rc)| {
                let pos = index.pos_of(rc);
                (pos, rc)
            })
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }
}
