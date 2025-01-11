use crate::{
    ast::{CodeRange, Paren, StmtList, TypeAnnotation, WriteTarget},
    EString,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Seq(SeqExpr),
    Nil(NilExpr),
    False(FalseExpr),
    True(TrueExpr),
    Integer(IntegerExpr),
    String(StringExpr),
    Regexp(RegexpExpr),
    XString(XStringExpr),
    LocalVariable(LocalVariableExpr),
    Self_(SelfExpr),
    SourceEncoding(SourceEncodingExpr),
    SourceFile(SourceFileExpr),
    SourceLine(SourceLineExpr),
    Call(CallExpr),
    Write(WriteExpr),
    Error(ErrorExpr),
}

impl_from!(
    (Expr, SeqExpr, Expr::Seq),
    (Expr, NilExpr, Expr::Nil),
    (Expr, FalseExpr, Expr::False),
    (Expr, TrueExpr, Expr::True),
    (Expr, IntegerExpr, Expr::Integer),
    (Expr, StringExpr, Expr::String),
    (Expr, RegexpExpr, Expr::Regexp),
    (Expr, XStringExpr, Expr::XString),
    (Expr, LocalVariableExpr, Expr::LocalVariable),
    (Expr, SelfExpr, Expr::Self_),
    (Expr, SourceEncodingExpr, Expr::SourceEncoding),
    (Expr, SourceFileExpr, Expr::SourceFile),
    (Expr, SourceLineExpr, Expr::SourceLine),
    (Expr, CallExpr, Expr::Call),
    (Expr, WriteExpr, Expr::Write),
    (Expr, ErrorExpr, Expr::Error),
);
impl_delegators!(
    enum Expr {
        Seq(SeqExpr),
        Nil(NilExpr),
        False(FalseExpr),
        True(TrueExpr),
        Integer(IntegerExpr),
        String(StringExpr),
        Regexp(RegexpExpr),
        XString(XStringExpr),
        LocalVariable(LocalVariableExpr),
        Self_(SelfExpr),
        SourceEncoding(SourceEncodingExpr),
        SourceFile(SourceFileExpr),
        SourceLine(SourceLineExpr),
        Call(CallExpr),
        Write(WriteExpr),
        Error(ErrorExpr),
    }
    range (mut range_mut): CodeRange,
    parens (mut parens_mut): Vec<Paren>,
);

impl Expr {
    pub fn outer_range(&self) -> &CodeRange {
        if let Some(last_paren) = self.parens().last() {
            &last_paren.range
        } else {
            self.range()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeqExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub paren: SeqParen,
    pub stmt_list: StmtList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeqParen {
    pub kind: SeqParenKind,
    pub open_range: CodeRange,
    pub close_range: CodeRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SeqParenKind {
    Paren,
    BeginEnd,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FalseExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TrueExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub value: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub open_range: CodeRange,
    pub close_range: CodeRange,
    pub contents: Vec<StringContent>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegexpExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub open_range: CodeRange,
    pub close_range: CodeRange,
    pub contents: Vec<StringContent>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct XStringExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub open_range: CodeRange,
    pub close_range: CodeRange,
    pub contents: Vec<StringContent>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StringContent {
    Text(TextContent),
    Interpolation(InterpolationContent),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextContent {
    pub range: CodeRange,

    pub value: EString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InterpolationContent {
    pub range: CodeRange,

    pub open_range: CodeRange,
    pub close_range: CodeRange,
    pub stmt_list: StmtList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalVariableExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub name: EString,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SelfExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceEncodingExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFileExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceLineExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub style: CallStyle,
    pub private: bool,
    pub receiver: Box<Expr>,
    pub method: EString,
    pub method_range: CodeRange,
    // TODO: support complex args
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallStyle {
    /// `obj.meth` etc.
    Dot,
    /// `meth` etc.
    ImplicitSelf,
    /// `obj.()` etc.
    CallOp,
    /// `-x` etc.
    UnOp,
    /// `not x`
    SpelloutUnOp,
    /// `x + y` etc.
    BinOp,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WriteExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub lhs: Box<WriteTarget>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}
