use num_bigint::{BigInt, BigUint};
use ordered_float::NotNan;

use crate::{
    ast::{ArgList, CodeRange, Paren, Stmt, TypeAnnotation, WriteTarget},
    EString,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Seq(SeqExpr),
    Nil(NilExpr),
    False(FalseExpr),
    True(TrueExpr),
    Numeric(NumericExpr),
    String(StringExpr),
    Regexp(RegexpExpr),
    XString(XStringExpr),
    LocalVariable(LocalVariableExpr),
    Const(ConstExpr),
    Self_(SelfExpr),
    SourceEncoding(SourceEncodingExpr),
    SourceFile(SourceFileExpr),
    SourceLine(SourceLineExpr),
    Call(CallExpr),
    Write(WriteExpr),
    And(AndExpr),
    Or(OrExpr),
    If(IfExpr),
    While(WhileExpr),
    Until(UntilExpr),
    Error(ErrorExpr),
}

impl_from!(
    (Expr, SeqExpr, Expr::Seq),
    (Expr, NilExpr, Expr::Nil),
    (Expr, FalseExpr, Expr::False),
    (Expr, TrueExpr, Expr::True),
    (Expr, NumericExpr, Expr::Numeric),
    (Expr, StringExpr, Expr::String),
    (Expr, RegexpExpr, Expr::Regexp),
    (Expr, XStringExpr, Expr::XString),
    (Expr, LocalVariableExpr, Expr::LocalVariable),
    (Expr, ConstExpr, Expr::Const),
    (Expr, SelfExpr, Expr::Self_),
    (Expr, SourceEncodingExpr, Expr::SourceEncoding),
    (Expr, SourceFileExpr, Expr::SourceFile),
    (Expr, SourceLineExpr, Expr::SourceLine),
    (Expr, CallExpr, Expr::Call),
    (Expr, WriteExpr, Expr::Write),
    (Expr, AndExpr, Expr::And),
    (Expr, OrExpr, Expr::Or),
    (Expr, IfExpr, Expr::If),
    (Expr, WhileExpr, Expr::While),
    (Expr, UntilExpr, Expr::Until),
    (Expr, ErrorExpr, Expr::Error),
);
impl_delegators!(
    enum Expr {
        Seq(SeqExpr),
        Nil(NilExpr),
        False(FalseExpr),
        True(TrueExpr),
        Numeric(NumericExpr),
        String(StringExpr),
        Regexp(RegexpExpr),
        XString(XStringExpr),
        LocalVariable(LocalVariableExpr),
        Const(ConstExpr),
        Self_(SelfExpr),
        SourceEncoding(SourceEncodingExpr),
        SourceFile(SourceFileExpr),
        SourceLine(SourceLineExpr),
        Call(CallExpr),
        Write(WriteExpr),
        And(AndExpr),
        Or(OrExpr),
        If(IfExpr),
        While(WhileExpr),
        Until(UntilExpr),
        Error(ErrorExpr),
    }
    range (mut range_mut): CodeRange,
    parens (mut parens_mut): Vec<Paren>,
);

impl Expr {
    pub fn outer_range(&self) -> &CodeRange {
        if let Some(last_paren) = self.parens().last() {
            last_paren.range()
        } else {
            self.range()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeqExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub style: NilStyle,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NilStyle {
    /// The `nil` keyword.
    Keyword,
    /// Implied nil, such as a part of `()` or `begin end`
    /// In this case, the parser records a dummy range.
    Implicit,
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
pub struct NumericExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub value: NumericValue,
    /// The value should be a Complex number containing an imaginary part.
    pub imaginary: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NumericValue {
    // TODO: use BigInt
    /// The value should be an Integer number unless `imaginary` is set.
    Integer(BigInt),
    /// The value should be a Float number unless `imaginary` is set.
    Float(NotNan<f64>),
    /// The value should be a Rational number unless `imaginary` is set.
    ///
    /// - The first value is the numerator.
    /// - The second value is the denominator, which is always positive.
    /// - The value is always reduced to the simplest form.
    Rational(BigInt, BigUint),
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
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalVariableExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub name: EString,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub receiver: ConstReceiver,
    pub name: EString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstReceiver {
    None,
    Object,
    Expr(Box<Expr>),
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
    pub optional: bool,
    pub receiver: Box<Expr>,
    pub method: EString,
    pub method_range: CodeRange,
    pub args: ArgList,
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

/// `lhs && rhs` or `lhs and rhs`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AndExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

/// `lhs || rhs` or `lhs or rhs`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OrExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

/// One of:
///
/// - `if cond; then_expr end`
/// - `if cond; then_expr else else_expr end`
/// - `... elsif cond; then_expr end`
/// - `... elsif cond; then_expr else else_expr end`
/// - `unless cond; else_expr end`
/// - `unless cond; else_expr else then_expr end`
/// - `then_expr if cond`
/// - `else_expr unless cond`
/// - `cond ? then_expr : else_expr`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub else_: Box<Expr>,
}

/// One of:
///
/// - `while cond; body end`
/// - `body while cond`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhileExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub cond: Box<Expr>,
    pub body: Box<Expr>,
}

/// One of:
///
/// - `until cond; body end`
/// - `body until cond`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UntilExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,

    pub cond: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorExpr {
    pub range: CodeRange,
    pub parens: Vec<Paren>,
}
