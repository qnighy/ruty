use crate::ast::{CodeRange, StmtSep};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Paren {
    Paren(ParenParen),
    BeginEnd(BeginEndParen),
}

impl_from!(
    (Paren, ParenParen, Paren::Paren),
    (Paren, BeginEndParen, Paren::BeginEnd),
);

impl_delegators!(
    enum Paren {
        Paren(ParenParen),
        BeginEnd(BeginEndParen),
    }
    range (mut range_mut): CodeRange,
);

/// Represents `(` ... `)` in the source code, other than:
///
/// - a pair of parentheses containing two or more expressions
/// - a pair of parentheses containing no expressions, which is equivalent to `nil`
/// - a pair of parentheses which is a part of a parenthesized argument list or parameter list
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParenParen {
    pub range: CodeRange,
    pub open_range: CodeRange,
    pub separator_prefix: Vec<StmtSep>,
    pub separator_suffix: Vec<StmtSep>,
    pub close_range: CodeRange,
}

/// Represents `begin` ... `end` in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BeginEndParen {
    pub range: CodeRange,
    pub begin_range: CodeRange,
    pub separator_prefix: Vec<StmtSep>,
    pub separator_suffix: Vec<StmtSep>,
    pub end_range: CodeRange,
}
