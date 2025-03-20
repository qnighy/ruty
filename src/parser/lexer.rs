use core::str;
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    sync::LazyLock,
};

use num_bigint::BigInt;
use num_traits::Num;
use ordered_float::NotNan;

use crate::{
    ast::{CodeRange, Decimal, NumericValue},
    encoding::{EStrRef, EncodingState},
    Diagnostic, EString,
};

const TAB_WIDTH: usize = 8;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct Token {
    pub(super) kind: TokenKind,
    pub(super) range: CodeRange,
    pub(super) indent: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum TokenKind {
    /// `__ENCODING__`, namely `keyword__ENCODING__`
    KeywordCapitalDoubleUnderscoreEncoding,
    /// `__LINE__`, namely `keyword__LINE__`
    KeywordCapitalDoubleUnderscoreLine,
    /// `__FILE__`, namely `keyword__FILE__`
    KeywordCapitalDoubleUnderscoreFile,
    /// `BEGIN`, namely `keyword_BEGIN`
    KeywordCapitalBegin,
    /// `END`, namely `keyword_END`
    KeywordCapitalEnd,
    /// `alias`, namely `keyword_alias`
    KeywordAlias,
    /// `and`, namely `keyword_and`
    KeywordAnd,
    /// `begin`, namely `keyword_begin`
    KeywordBegin,
    /// `break`, namely `keyword_break`
    KeywordBreak,
    /// `case`, namely `keyword_case`
    KeywordCase,
    /// `class`, namely `keyword_class`
    KeywordClass,
    /// `def`, namely `keyword_def`
    KeywordDef,
    /// `defined?`, namely `keyword_defined`
    KeywordDefinedQ,
    /// `do`, namely `keyword_do`, `keyword_do_cond`, `keyword_do_block`, and `keyword_do_LAMBDA`
    KeywordDo,
    /// `else`, namely `keyword_else`
    KeywordElse,
    /// `elsif`, namely `keyword_elsif`
    KeywordElsif,
    /// `end`, namely `keyword_end`
    KeywordEnd,
    /// `ensure`, namely `keyword_ensure`
    KeywordEnsure,
    /// `false`, namely `keyword_false`
    KeywordFalse,
    /// `for`, namely `keyword_for`
    KeywordFor,
    /// `if`, namely `keyword_if`
    KeywordIf,
    /// `if` postfix, namely `modifier_if`
    KeywordIfInfix,
    /// `in`, namely `keyword_in`
    KeywordIn,
    /// `module`, namely `keyword_module`
    KeywordModule,
    /// `next`, namely `keyword_next`
    KeywordNext,
    /// `nil`, namely `keyword_nil`
    KeywordNil,
    /// `not`, namely `keyword_not`
    KeywordNot,
    /// `or`, namely `keyword_or`
    KeywordOr,
    /// `redo`, namely `keyword_redo`
    KeywordRedo,
    /// `rescue`, namely `keyword_rescue`
    KeywordRescue,
    /// `retry`, namely `keyword_retry`
    KeywordRetry,
    /// `return`, namely `keyword_return`
    KeywordReturn,
    /// `self`, namely `keyword_self`
    KeywordSelf,
    /// `super`, namely `keyword_super`
    KeywordSuper,
    /// `then`, namely `keyword_then`
    KeywordThen,
    /// `true`, namely `keyword_true`
    KeywordTrue,
    /// `undef`, namely `keyword_undef`
    KeywordUndef,
    /// `unless`, namely `keyword_unless`
    KeywordUnless,
    /// `unless` postfix, namely `modifier_unless`
    KeywordUnlessInfix,
    /// `until`, namely `keyword_until`
    KeywordUntil,
    /// `until` postfix, namely `modifier_until`
    KeywordUntilInfix,
    /// `when`, namely `keyword_when`
    KeywordWhen,
    /// `while`, namely `keyword_while`
    KeywordWhile,
    /// `while` postfix, namely `modifier_while`
    KeywordWhileInfix,
    /// `yield`, namely `keyword_yield`
    KeywordYield,

    /// `foo` etc., namely `tIDENTIFIER`
    Identifier,
    /// `Foo` etc., namely `tCONSTANT`
    Const,
    /// `foo!` etc., namely `tFID`
    MethodName,
    /// `foo:` etc., namely `tLABEL`
    Label,
    /// `:foo` etc., namely `tSYMBOL` followed by the symbol content
    Symbol,
    /// `@foo` etc., namely `tIVAR`, `tCVAR`, `tGVAR`, `tNTH_REF`, and `tBACK_REF`
    NonLocal(NonLocalKind),

    /// `123` etc., namely `tINTEGER`, `tFLOAT`, `tRATIONAL`, and `tIMAGINARY`
    Numeric(NumericToken),
    /// `?a` etc., namely `tCHAR`
    CharLiteral(EString),

    /// `"`, `'`, `:"`, `/` etc. in expr context. Namely:
    ///
    /// - `tSTRING_BEG`
    /// - `tXSTRING_BEG`
    /// - `tREGEXP_BEG`
    StringBegin,
    /// Similar to [TokenKind::StringBegin] but allows converting to a label
    /// at the end of the string.
    StringBeginLabelable,
    /// `"` etc. in String-like context. Namely:
    ///
    /// - `tSTRING_END`
    /// - `tREGEXP_END`
    StringEnd,
    /// `":` etc. in String-like context. Namely:
    ///
    /// - `tLABEL_END`
    StringEndColon,
    /// `foo` as in `"foo"`, namely `tSTRING_CONTENT`
    StringContent,
    /// `#{`, namely `tSTRING_DBEG`
    StringInterpolationBegin,
    /// `#@foo` etc., namely `tSTRING_DVAR` followed by the variable name
    StringVarInterpolation,

    /// `<<` etc. One of:
    ///
    /// - `||`, namely `tOROP`
    /// - `&&`, namely `tANDOP`
    /// - `==`, namely `tEQ`
    /// - `!=`, namely `tNEQ`
    /// - `=~`, namely `tMATCH`
    /// - `!~`, namely `tNMATCH`
    /// - `===`, namely `tEQQ`
    /// - `<`, namely `'<'`
    /// - `<=`, namely `tLEQ`
    /// - `>`, namely `'>'`
    /// - `>=`, namely `tGEQ`
    /// - `|`, namely `'|`
    /// - `^`, namely `'^'`
    /// - `&`, namely `'&'`
    /// - `<<`, namely `tLSHFT`
    /// - `>>`, namely `tRSHFT`
    /// - `+`, namely `'+'`
    /// - `-`, namely `'-'`
    /// - `*`, namely `'*'`
    /// - `/`, namely `'/'`
    /// - `%`, namely `'%'`
    /// - `**`, namely `tPOW`
    BinOp(BinOpKind),
    /// `!` etc. One of:
    ///
    /// - `!`, namely `'!'`
    /// - `~`, namely `'~'`
    /// - `+`, namely `tUPLUS`
    /// - `-`, namely `tUMINUS` and `tUMINUS_NUM`
    UnOp(UnOpKind),
    /// `+=` etc., namely `tOP_ASGN`
    OpAssign(BinOpKind),

    /// `&` but block argument only, namely `tAMPER`
    AmpPrefix,
    /// `&.`, namely `tANDDOT`
    AmpDot,
    /// `(`, namely `'('` and `tLPAREN`
    LParen,
    /// `(` with restricted syntactic rule (e.g. cannot be an argument list delimiter),
    /// namely `tLPAREN_ARG`
    LParenRestricted,
    /// `)`, namely `')'`
    RParen,
    /// `*` but argument splat only, namely `tSTAR`
    StarPrefix,
    /// `**` but keyword argument splat only, namely `tDSTAR`
    StarStarPrefix,
    /// `,`, namely `','`
    Comma,
    /// `->`, namely `tLAMBDA`
    Arrow,
    /// `.`, namely `'.'`
    Dot,
    /// `..`, namely `tDOT2` and `tBDOT2`
    DotDot,
    /// `...`, namely `tDOT3` and `tBDOT3`
    DotDotDot,
    /// `:`, namely `':'`
    Colon,
    /// `::`, namely `tCOLON2`
    ColonColon,
    /// `::` but prefix only, namely `tCOLON3`
    ColonColonPrefix,
    /// `;`, but in most cases Newline is used instead.
    /// Namely `';'`
    Semicolon,
    /// EOL in certain contexts, namely `'\n'`
    Newline,
    /// `=`, namely `'='`
    Eq,
    /// `=>`, namely `tASSOC`
    FatArrow,
    /// `?`, namely `'?'`
    Question,
    /// `@` (Ruty-specific)
    At,
    /// `[`, namely `'['`
    LBracket,
    /// `[` but prefix only, namely `tLBRACK`
    LBracketPrefix,
    /// `]`, namely `']'`
    RBracket,
    /// `{`, namely '{', `tLBRACE`, and `tLBRACE_ARG`
    LBrace,
    /// `}`, namely `'}'`
    RBrace,
    /// End of file, which is one of:
    ///
    /// - The real end of file (0 bytes wide)
    /// - The NUL (\\x00) character (1 byte wide)
    /// - The EOT (\\x04) character (1 byte wide)
    /// - The SUB (\\x1A) character (1 byte wide)
    /// - The `__END__` keyword occupying the whole line (7 bytes wide)
    ///
    /// To simplify the lexer, the EOF result is not persisted.
    /// Be careful not to consume the EOF token multiple times.
    EOF,
    // Indicates a character sequence that the lexer cannot recognize.
    // Always 1 byte or longer.
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum NonLocalKind {
    /// `@foo`, namely `tIVAR`
    Ivar,
    /// `@@foo`, namely `tCVAR`
    Cvar,
    /// `$foo` (incl. `$&` and `$1`), namely `tGVAR`, `tNTH_REF`, and `tBACK_REF`
    Gvar,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct NumericToken {
    pub(super) value: NumericValue,
    pub(super) imaginary: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum BinOpKind {
    /// `||`
    LogicalOr,
    /// `&&`
    LogicalAnd,
    /// `==`
    Eq,
    /// `!=`
    NotEq,
    /// `=~`
    Match,
    /// `!~`
    NotMatch,
    /// `===`
    Incl,
    /// `<=>`
    Cmp,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    /// `|`, also used for block parameter lists
    BitwiseOr,
    /// `^`
    BitwiseXor,
    /// `&`
    BitwiseAnd,
    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `**`
    Pow,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum UnOpKind {
    /// `!`
    Not,
    /// `~`
    BitwiseNot,
    /// `+`
    Plus,
    /// `-`
    Minus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub(super) enum LexerState {
    /// Expects an expression.
    /// Equivalent to `EXPR_BEG` or `EXPR_ARG|EXPR_LABELED` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - The beginning of the file
    /// - After `\n` or `;`
    /// - After most prefix or infix operators
    ///
    /// Effects:
    ///
    /// - `\n` is skipped as whitespace
    /// - When there is ambiguity between prefix and infix operators,
    ///   prefix is preferred.
    #[default]
    Begin,
    /// Expects a class name. Variant of [LexerState::Begin].
    /// Equivalent to `EXPR_CLASS` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - After `class`
    ///
    /// Effects are equivalent to [LexerState::Begin] except:
    ///
    /// - Heredocs are suppressed and `<<` is parsed separately instead.
    ClassName,
    /// Expects an optional expression. Variant of [LexerState::Begin].
    /// Equivalent to `EXPR_MID` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - After `return`
    /// - After `break`
    /// - After `next`
    /// - After `rescue` (except the infix `rescue` operator)
    ///
    /// Effects are equivalent to [LexerState::Begin] except:
    ///
    /// - `\n` is a real token like in [LexerState::End].
    /// - `if`, `unless`, `while`, `until`, and `rescue` gets infix
    ///   rather than prefix operators.
    BeginOpt,
    /// Expects an expression or a labelled expression, but not arguments.
    /// Variant of [LexerState::Begin].
    /// Equivalent to `EXPR_BEG|EXPR_LABEL` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - After `(`, `[`, or `{` (except `{` for Hashes)
    /// - After `,`
    /// - After `|` (except method names)
    /// - After `=>` or `in` introducing patterns
    /// - After infix `if`, `unless`, `while`, `until` or `rescue`.
    ///
    /// Effects are equivalent to [LexerState::Begin] except:
    ///
    /// - It accepts labels, which is one of:
    ///   - `foo:` or `Foo:`
    ///   - `foo!:` or `foo?:`
    ///   - `"foo":`
    ///   - `'foo':`
    BeginLabelable,
    /// Expects the first argument of a non-parenthesized method call.
    /// Variant of [LexerState::Begin].
    /// Equivalent to `EXPR_ARG` or `EXPR_ARG|EXPR_LABEL` or `EXPR_CMDARG` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - After `defined?`, `super`, or `yield`
    /// - After `not`
    /// - After `=>` or `in` introducing patterns
    /// - After method call candidates, such as:
    ///   - Identifier or `!`/`?`-suffixed identifier at a "Begin"-like position, except known local variables
    ///   - Method name following `.`, `&.`, or `::`, including suffixed ones, keyword-like ones, const-like ones and, op-like ones.
    ///
    /// Effects are equivalent to [LexerState::End] except:
    ///
    /// - It accepts labels like [LexerState::BeginLabelable] does.
    /// - `?` can be a part of a character literal.
    /// - `:` always becomes a symbol, making `1 ? f:x` a syntax error.
    ///   Compare it with `1 ? 2:x`, which is valid.
    /// - Special prefix conditions:
    ///   - Group A
    ///     - `+` starts a unary plus when preceded by whitespace
    ///       and not followed by whitespace.
    ///     - `-` starts a unary minus when preceded by whitespace
    ///       and not followed by whitespace.
    ///     - `*` starts an argument splat when preceded by whitespace
    ///       and not followed by whitespace.
    ///     - `**` starts an keyword argument splat when preceded by whitespace
    ///       and not followed by whitespace.
    ///     - `&` starts a block argument when preceded by whitespace
    ///       and not followed by whitespace.
    ///   - Group B
    ///     - `/` starts a regexp literal when preceded by whitespace
    ///       and not followed by whitespace or `=`.
    ///     - `%` starts a percent literal when preceded by whitespace
    ///       and not followed by whitespace or `=`.
    ///   - Group C
    ///     - `::` starts an absolute constant reference
    ///       when preceded by whitespace.
    ///     - `(` is a grouping token when preceded by whitespace.
    ///       Note that this is slightly different from the ordinary `(`
    ///       token found during [LexerState::Begin].
    ///     - `[` is a part of array literal when preceded by whitespace.
    ///     - `<<` can be a part of heredoc when preceded by whitespace.
    FirstArgument,
    /// Expects a suffix or an infix, but still there is a room for ambiguity.
    /// Variant of [LexerState::End].
    /// Equivalent to `EXPR_END|EXPR_LABEL` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - After known local variable names
    ///
    /// Effects are equivalent to [LexerState::End] except:
    ///
    /// - It accepts labels like [LexerState::BeginLabelable] does.
    /// - `(` token behaves like [LexerState::FirstArgument].
    WeakFirstArgument,
    /// Expects a suffix or an infix.
    /// Equivalent to `EXPR_END` or or `EXPR_ENDARG` or `EXPR_ENDFN` or `EXPR_ENDFN|EXPR_LABEL` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - After most one-token expressions (e.g. integers)
    /// - After most expression closers (e.g. `]`, `end`)
    ///
    /// Effects:
    ///
    /// - `\n` is a real token unless it is a line continuation `\\\n` or
    ///   it is followed by a `.` or a `&.` (except `..`).
    /// - When there is ambiguity between prefix and infix operators,
    ///   prefix is preferred.
    End,
    /// Expects a method name for definition.
    /// Equivalent to `EXPR_FNAME` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - After `def`
    /// - After `def recv.` (incl. `::`)
    ///
    /// Effects:
    ///
    /// - Overridable operators, such as `+`, are treated as method names.
    /// - Keywords are treated as method names, except `__END__` occupying the whole line.
    MethForDef,
    /// Expects a method name for definition, but Symbols are also allowed.
    /// Equivalent to `EXPR_FNAME|EXPR_FITEM` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - As an argument of `alias`
    /// - As an argument of `undef`
    ///
    /// Effects are equivalent to [LexerState::MethForDef] except:
    ///
    /// - Allows `%s{...}` symbols syntax.
    MethOrSymbolForDef,
    /// Expects a method name for calling.
    /// Equivalent to `EXPR_DOT` in CRuby.
    ///
    /// Occurrences:
    ///
    /// - After `.`, `&.`, or `::` (except `::` as a prefix)
    ///
    /// Effects are equivalent to [LexerState::MethForDef] except:
    ///
    /// - tBACK_REF/tNTH_REF token types are kept as-is (but it can be handled in the parser).
    /// - Unlike [LexerState::MethForDef], it does not parse `foo=`-type method names.
    /// - Heredocs are suppressed.
    MethForCall,
}

impl LexerState {
    /// Should we fold lines if `\n` is found?
    fn fold_newline(&self) -> bool {
        match self {
            LexerState::Begin => true,
            LexerState::ClassName => true,
            LexerState::BeginOpt => false,
            LexerState::BeginLabelable => true,
            LexerState::FirstArgument => false,
            LexerState::WeakFirstArgument => false,
            LexerState::End => false,
            LexerState::MethForDef => true,
            LexerState::MethOrSymbolForDef => true,
            LexerState::MethForCall => true,
        }
    }

    /// Should we convert `if`, `unless`, `while`, `until`
    /// and `rescue` to infix operators?
    fn prefer_modifier_if(&self) -> bool {
        match self {
            LexerState::End
            | LexerState::FirstArgument
            | LexerState::WeakFirstArgument
            | LexerState::BeginOpt => true,
            _ => false,
        }
    }

    /// Should we prefer prefix-only tokens over ordinary ones for:
    ///
    /// - `+`, `-`
    /// - `*`, `**`, `&`
    /// - `/`, `%`
    /// - `::`
    /// - `[`
    fn prefer_prefix_operator(&self, space_before: bool, space_or_obstacle_after: bool) -> bool {
        match self {
            LexerState::Begin
            | LexerState::ClassName
            | LexerState::BeginOpt
            | LexerState::BeginLabelable => true,
            LexerState::FirstArgument => space_before && !space_or_obstacle_after,
            LexerState::WeakFirstArgument
            | LexerState::End
            | LexerState::MethForDef
            | LexerState::MethOrSymbolForDef
            | LexerState::MethForCall => false,
        }
    }

    fn restricted_paren(&self, space_before: bool) -> bool {
        match self {
            LexerState::FirstArgument | LexerState::WeakFirstArgument => space_before,
            _ => false,
        }
    }

    /// Should we extend lexer to include extended method names, such as:
    ///
    /// - `foo=`
    /// - `!@`
    /// - `~@`
    /// - `+@`
    /// - `-@`
    /// - `` ` ``
    /// - `[]`
    /// - `[]=`
    fn extended_method_name(&self) -> bool {
        match self {
            LexerState::MethForDef | LexerState::MethOrSymbolForDef | LexerState::MethForCall => {
                true
            }
            _ => false,
        }
    }

    /// Should we extend lexer to include extended method names, such as:
    ///
    /// - `foo=`
    fn allow_assigner_ident(&self) -> bool {
        match self {
            LexerState::MethForDef | LexerState::MethOrSymbolForDef => true,
            _ => false,
        }
    }

    /// Should we allow labels, such as:
    ///
    /// - `foo:`
    /// - `foo!:` or `foo?:`
    /// - `"foo":`
    fn allow_label(&self) -> bool {
        match self {
            LexerState::BeginLabelable
            | LexerState::FirstArgument
            | LexerState::WeakFirstArgument => true,
            _ => false,
        }
    }

    fn string_begin(&self) -> TokenKind {
        if self.allow_label() {
            TokenKind::StringBeginLabelable
        } else {
            TokenKind::StringBegin
        }
    }

    fn allow_symbol(&self) -> bool {
        match self {
            LexerState::Begin => true,
            LexerState::ClassName => true,
            LexerState::BeginOpt => true,
            LexerState::BeginLabelable => true,
            LexerState::FirstArgument => true,
            LexerState::WeakFirstArgument => true,
            LexerState::End => false,
            LexerState::MethForDef => false,
            LexerState::MethOrSymbolForDef => true,
            LexerState::MethForCall => false,
        }
    }

    /// Should we split `||`?
    fn split_vert_vert(&self) -> bool {
        match self {
            LexerState::Begin
            | LexerState::ClassName
            | LexerState::BeginOpt
            | LexerState::BeginLabelable => true,
            _ => false,
        }
    }

    /// If true, `?` cannot be a part of a character literal.
    fn force_single_question_mark(&self) -> bool {
        match self {
            LexerState::Begin => false,
            LexerState::ClassName => false,
            LexerState::BeginOpt => false,
            LexerState::BeginLabelable => false,
            LexerState::FirstArgument => false,
            LexerState::WeakFirstArgument => true,
            LexerState::End => true,
            LexerState::MethForDef => false,
            LexerState::MethOrSymbolForDef => false,
            LexerState::MethForCall => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct StringState {
    pub(super) delim: StringDelimiter,
    pub(super) allow_label: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum StringDelimiter {
    /// `'`
    Quote,
    /// `"`
    DoubleQuote,
    /// `` ` ``
    Backtick,
    /// `/`
    Slash,
    // /// `%`
    // Percent {},
}

impl StringDelimiter {
    fn allow_interpolation(&self) -> bool {
        match self {
            StringDelimiter::Quote => false,
            StringDelimiter::DoubleQuote => true,
            StringDelimiter::Backtick => true,
            StringDelimiter::Slash => true,
        }
    }
}

macro_rules! ident_start {
    () => {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'..
    };
}
macro_rules! ident_continue {
    () => {
        b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b'\x80'..
    };
}
macro_rules! space_inline {
    () => {
        b'\t' | b'\x0B' | b'\x0C' | b'\r' | b' '
    };
}
macro_rules! space {
    () => {
        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' '
    };
}
macro_rules! eof_char {
    () => {
        b'\0' | b'\x04' | b'\x1A'
    };
}
macro_rules! decimal_digit {
    () => {
        b'0'..=b'9'
    };
}
macro_rules! hex_letter {
    () => {
        b'a'..=b'f' | b'A'..=b'F'
    };
}

#[derive(Debug)]
pub(super) struct Lexer<'a> {
    input: EStrRef<'a>,
    pos: usize,
    indent: usize,
    eof_token: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub(super) fn new(input: EStrRef<'a>) -> Self {
        let mut this = Self {
            input,
            pos: 0,
            indent: 0,
            eof_token: None,
        };
        this.reset_indent();
        this
    }

    pub(super) fn input(&self) -> EStrRef<'a> {
        self.input
    }

    pub(super) fn bytes(&self) -> &'a [u8] {
        self.input.as_bytes()
    }

    pub(super) fn pos(&self) -> usize {
        self.pos
    }

    pub(super) fn lex(&mut self, diag: &mut Vec<Diagnostic>, state: LexerState) -> Token {
        if let Some(eof_token) = &self.eof_token {
            return eof_token.clone();
        }
        let space_before = self.lex_space(diag, state);
        let start = self.pos;
        let kind = match self.peek_byte() {
            eof_char!() => {
                if self.pos < self.bytes().len() {
                    self.pos += 1;
                }
                TokenKind::EOF
            }
            b'\n' => {
                let orig_indent = self.indent;
                if self.lookbehind_byte(1) == b'\r' {
                    // Include the preceding CR to form CRLF
                    self.pos += 1;
                    self.reset_indent();
                    return Token {
                        kind: TokenKind::Newline,
                        range: CodeRange {
                            start: self.pos - 2,
                            end: self.pos,
                        },
                        indent: orig_indent,
                    };
                }
                self.pos += 1;
                self.reset_indent();
                return Token {
                    kind: TokenKind::Newline,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    indent: orig_indent,
                };
            }
            ident_start!() => {
                self.scan_ident();
                let is_method_name = match self.peek_byte() {
                    b'!' | b'?' if self.lookahead_byte(1) != b'=' => {
                        self.pos += 1;
                        true
                    }
                    b'=' if state.allow_assigner_ident()
                        && match self.lookahead_byte(1) {
                            // Ignore foo=> or foo=~
                            b'>' | b'~' => false,
                            // Ignore foo== but consume foo==> specially
                            b'=' => self.lookahead_byte(2) == b'>',
                            _ => true,
                        } =>
                    {
                        self.pos += 1;
                        true
                    }
                    _ => false,
                };
                let is_label = if state.allow_label()
                    && self.peek_byte() == b':'
                    && self.lookahead_byte(1) != b':'
                {
                    self.pos += 1;
                    true
                } else {
                    false
                };
                let s_bytes = &self.bytes()[start..self.pos];
                let s = EStrRef::from_bytes(s_bytes, self.input.encoding());
                if let Some(kwd) = KEYWORDS.get(s_bytes) {
                    match kwd {
                        TokenKind::EOF => {
                            if self.is_beginning_of_line(start) && self.is_end_of_line(self.pos) {
                                TokenKind::EOF
                            } else {
                                // __END__ not occupying the whole line should be treated as an ordinary identifier
                                TokenKind::Identifier
                            }
                        }
                        TokenKind::KeywordIf if state.prefer_modifier_if() => {
                            TokenKind::KeywordIfInfix
                        }
                        TokenKind::KeywordUnless if state.prefer_modifier_if() => {
                            TokenKind::KeywordUnlessInfix
                        }
                        TokenKind::KeywordWhile if state.prefer_modifier_if() => {
                            TokenKind::KeywordWhileInfix
                        }
                        TokenKind::KeywordUntil if state.prefer_modifier_if() => {
                            TokenKind::KeywordUntilInfix
                        }
                        _ => kwd.clone(),
                    }
                } else if is_label {
                    // is_label wins over is_method_name so that `foo?:` is a label
                    TokenKind::Label
                } else if is_method_name {
                    TokenKind::MethodName
                } else {
                    if !s.is_valid() {
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("The identifier contains invalid characters"),
                        });
                    }
                    if s.starts_with_ruby_uppercase() {
                        TokenKind::Const
                    } else {
                        TokenKind::Identifier
                    }
                }
            }
            decimal_digit!() => self.lex_numeric(diag),
            // `!`
            // `!@`
            // `!=`
            // `!~`
            b'!' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        TokenKind::BinOp(BinOpKind::NotEq)
                    }
                    b'@' if state.extended_method_name() => {
                        self.pos += 1;
                        TokenKind::MethodName
                    }
                    b'~' => {
                        self.pos += 1;
                        TokenKind::BinOp(BinOpKind::NotMatch)
                    }
                    _ => TokenKind::UnOp(UnOpKind::Not),
                }
            }
            b'"' => {
                self.pos += 1;
                state.string_begin()
            }
            b'#' => {
                unreachable!("Should have been skipped by lex_space");
            }
            b'$' => self.lex_non_local(diag),
            b'%' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign(BinOpKind::Mod)
                    }
                    _ => TokenKind::BinOp(BinOpKind::Mod),
                }
            }
            // `&`
            // `&&`
            // `&&=`
            // `&.`
            // `&=`
            b'&' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'&' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                TokenKind::OpAssign(BinOpKind::LogicalAnd)
                            }
                            _ => TokenKind::BinOp(BinOpKind::LogicalAnd),
                        }
                    }
                    b'.' => {
                        self.pos += 1;
                        TokenKind::AmpDot
                    }
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign(BinOpKind::BitwiseAnd)
                    }
                    _ => {
                        if state.prefer_prefix_operator(space_before, self.peek_space()) {
                            TokenKind::AmpPrefix
                        } else {
                            TokenKind::BinOp(BinOpKind::BitwiseAnd)
                        }
                    }
                }
            }
            b'\'' => {
                self.pos += 1;
                state.string_begin()
            }
            b'(' => {
                self.pos += 1;
                if state.restricted_paren(space_before) {
                    TokenKind::LParenRestricted
                } else {
                    TokenKind::LParen
                }
            }
            b')' => {
                self.pos += 1;
                TokenKind::RParen
            }
            // `*`
            // `**`
            // `**=`
            // `*=`
            b'*' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'*' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                TokenKind::OpAssign(BinOpKind::Pow)
                            }
                            _ => {
                                if state.prefer_prefix_operator(space_before, self.peek_space()) {
                                    TokenKind::StarStarPrefix
                                } else {
                                    TokenKind::BinOp(BinOpKind::Pow)
                                }
                            }
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign(BinOpKind::Mul)
                    }
                    _ => {
                        if state.prefer_prefix_operator(space_before, self.peek_space()) {
                            TokenKind::StarPrefix
                        } else {
                            TokenKind::BinOp(BinOpKind::Mul)
                        }
                    }
                }
            }
            // `+`
            // `+@`
            // `+=`
            b'+' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'@' if state.extended_method_name() => {
                        self.pos += 1;
                        TokenKind::MethodName
                    }
                    // Interestingly, only `+=` and `-=` are subject to splitting
                    // in method name context.
                    b'=' if !state.extended_method_name() => {
                        self.pos += 1;
                        TokenKind::OpAssign(BinOpKind::Add)
                    }
                    _ => {
                        if state.prefer_prefix_operator(space_before, self.peek_space()) {
                            if self.peek_byte().is_ascii_digit()
                                || (self.peek_byte() == b'.'
                                    && self.lookahead_byte(1).is_ascii_digit())
                            {
                                // Leave '+' eaten so that lex_numeric need not to check it again
                                self.lex_numeric(diag)
                            } else {
                                TokenKind::UnOp(UnOpKind::Plus)
                            }
                        } else {
                            TokenKind::BinOp(BinOpKind::Add)
                        }
                    }
                }
            }
            b',' => {
                self.pos += 1;
                TokenKind::Comma
            }
            // `-`
            // `-@`
            // `-=`
            // `->`
            // `-123`
            b'-' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'@' if state.extended_method_name() => {
                        self.pos += 1;
                        TokenKind::MethodName
                    }
                    // Interestingly, only `+=` and `-=` are subject to splitting
                    // in method name context.
                    b'=' if !state.extended_method_name() => {
                        self.pos += 1;
                        TokenKind::OpAssign(BinOpKind::Sub)
                    }
                    b'>' => {
                        self.pos += 1;
                        TokenKind::Arrow
                    }
                    _ => {
                        if state.prefer_prefix_operator(space_before, self.peek_space()) {
                            TokenKind::UnOp(UnOpKind::Minus)
                        } else {
                            TokenKind::BinOp(BinOpKind::Sub)
                        }
                    }
                }
            }
            // `.`
            // `..`
            // `...`
            // `.123` (which is an invalid float)
            b'.' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'.' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'.' => {
                                self.pos += 1;
                                TokenKind::DotDotDot
                            }
                            _ => TokenKind::DotDot,
                        }
                    }
                    decimal_digit!() => {
                        self.pos -= 1;
                        // Always results in an invalid float, though
                        // it seems the best guess at this point.
                        self.lex_numeric(diag)
                    }
                    _ => TokenKind::Dot,
                }
            }
            // `/`
            // `/=`
            b'/' => {
                self.pos += 1;
                if state.prefer_prefix_operator(space_before, self.peek_byte() == b'=') {
                    TokenKind::StringBegin
                } else {
                    match self.peek_byte() {
                        b'=' => {
                            self.pos += 1;
                            TokenKind::OpAssign(BinOpKind::Div)
                        }
                        _ => TokenKind::BinOp(BinOpKind::Div),
                    }
                }
            }
            b':' => {
                if self.lookahead_byte(1) == b':' {
                    self.pos += 2;
                    // Pass `false` so that `p :: Foo` is equivalent to `p ::Foo`
                    if state.prefer_prefix_operator(space_before, false) {
                        TokenKind::ColonColonPrefix
                    } else {
                        TokenKind::ColonColon
                    }
                } else if state.allow_symbol() {
                    self.lex_maybe_symbol(diag)
                } else {
                    self.pos += 1;
                    TokenKind::Colon
                }
            }
            b';' => {
                self.pos += 1;
                TokenKind::Semicolon
            }
            b'<' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'<' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                TokenKind::OpAssign(BinOpKind::LShift)
                            }
                            _ => TokenKind::BinOp(BinOpKind::LShift),
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'>' => {
                                self.pos += 1;
                                TokenKind::BinOp(BinOpKind::Cmp)
                            }
                            _ => TokenKind::BinOp(BinOpKind::Le),
                        }
                    }
                    _ => TokenKind::BinOp(BinOpKind::Lt),
                }
            }
            b'=' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                TokenKind::BinOp(BinOpKind::Incl)
                            }
                            _ => TokenKind::BinOp(BinOpKind::Eq),
                        }
                    }
                    b'>' => {
                        self.pos += 1;
                        TokenKind::FatArrow
                    }
                    b'~' => {
                        self.pos += 1;
                        TokenKind::BinOp(BinOpKind::Match)
                    }
                    _ => TokenKind::Eq,
                }
            }
            b'>' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        TokenKind::BinOp(BinOpKind::Ge)
                    }
                    b'>' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                TokenKind::OpAssign(BinOpKind::RShift)
                            }
                            _ => TokenKind::BinOp(BinOpKind::RShift),
                        }
                    }
                    _ => TokenKind::BinOp(BinOpKind::Gt),
                }
            }
            b'?' => {
                self.pos += 1;
                let char_start = self.pos;
                if state.force_single_question_mark() {
                    TokenKind::Question
                } else {
                    match self.peek_byte() {
                        space!() => TokenKind::Question,
                        b'\0' if self.pos >= self.bytes().len() => TokenKind::Question,
                        ident_continue!() => {
                            let next_len = self
                                .input
                                .encoding()
                                .next_len(&self.bytes()[self.pos..], EncodingState::default());
                            let char_start = self.pos;
                            self.scan_ident();
                            let s = EStrRef::from_bytes(
                                &self.bytes()[char_start..self.pos],
                                self.input.encoding(),
                            );
                            if !s.is_valid() {
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start: char_start,
                                        end: self.pos,
                                    },
                                    message: format!(
                                        "The character literal contains invalid characters"
                                    ),
                                });
                                TokenKind::CharLiteral(self.select_owned(char_start, self.pos))
                            } else if self.pos - char_start == next_len {
                                TokenKind::CharLiteral(self.select_owned(char_start, self.pos))
                            } else if self.get_byte(char_start).is_ascii() {
                                // Long ident starting with an ASCII letter.
                                // Split before the ident in this case.
                                self.pos = char_start;
                                TokenKind::Question
                            } else {
                                let suffix = EStrRef::from_bytes(
                                    &self.bytes()[char_start + next_len..self.pos],
                                    self.input.encoding(),
                                );
                                if SUFFIX_KEYWORDS.contains(suffix.as_bytes()) {
                                    // Valid combination like `?あand`
                                    // (following the current parse.y behavior)
                                    self.pos = char_start + next_len;
                                    TokenKind::CharLiteral(self.select_owned(char_start, self.pos))
                                } else {
                                    // Something like `?あfoo`, which won't yield a valid syntax.
                                    // Concatenate it and report error here.
                                    diag.push(Diagnostic {
                                        range: CodeRange {
                                            start: char_start - 1,
                                            end: self.pos,
                                        },
                                        message: format!("Character literal too long"),
                                    });
                                    TokenKind::CharLiteral(self.select_owned(char_start, self.pos))
                                }
                            }
                        }
                        _ => {
                            // Non-space, non-ident ASCII character.
                            self.pos += 1;
                            TokenKind::CharLiteral(self.select_owned(char_start, self.pos))
                        }
                    }
                }
            }
            b'@' if !matches!(self.lookahead_byte(1), ident_continue!() | b'@') => {
                self.pos += 1;
                TokenKind::At
            }
            b'@' => self.lex_non_local(diag),
            b'[' => {
                self.pos += 1;
                if state.extended_method_name() && self.peek_byte() == b']' {
                    self.pos += 1;
                    match self.peek_byte() {
                        b'=' => {
                            self.pos += 1;
                            TokenKind::MethodName
                        }
                        _ => TokenKind::MethodName,
                    }
                } else if state.prefer_prefix_operator(space_before, false) {
                    TokenKind::LBracketPrefix
                } else {
                    TokenKind::LBracket
                }
            }
            b']' => {
                self.pos += 1;
                TokenKind::RBracket
            }
            b'^' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign(BinOpKind::BitwiseXor)
                    }
                    _ => TokenKind::BinOp(BinOpKind::BitwiseXor),
                }
            }
            b'`' => {
                self.pos += 1;
                if state.extended_method_name() {
                    TokenKind::MethodName
                } else {
                    TokenKind::StringBegin
                }
            }
            b'{' => {
                self.pos += 1;
                TokenKind::LBrace
            }
            b'|' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'|' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                TokenKind::OpAssign(BinOpKind::LogicalOr)
                            }
                            _ if state.split_vert_vert() => {
                                self.pos -= 1;
                                TokenKind::BinOp(BinOpKind::BitwiseOr)
                            }
                            _ => TokenKind::BinOp(BinOpKind::LogicalOr),
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign(BinOpKind::BitwiseOr)
                    }
                    _ => TokenKind::BinOp(BinOpKind::BitwiseOr),
                }
            }
            b'}' => {
                self.pos += 1;
                TokenKind::RBrace
            }
            b'~' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'@' if state.extended_method_name() => {
                        self.pos += 1;
                        TokenKind::MethodName
                    }
                    _ => TokenKind::UnOp(UnOpKind::BitwiseNot),
                }
            }
            _ => {
                self.pos += 1;
                TokenKind::Unknown
            }
        };
        let end = self.pos;
        if matches!(kind, TokenKind::EOF) {
            self.eof_token = Some(Token {
                kind: kind.clone(),
                range: CodeRange { start, end },
                indent: self.indent,
            });
        }
        Token {
            kind,
            range: CodeRange { start, end },
            indent: self.indent,
        }
    }

    fn lex_maybe_symbol(&mut self, diag: &mut Vec<Diagnostic>) -> TokenKind {
        let start = self.pos;
        // Consume the first ':'
        self.pos += 1;

        match self.peek_byte() {
            space!() | eof_char!() | b'#' => {
                return TokenKind::Colon;
            }
            ident_continue!() => {
                let is_numeric = self.peek_byte().is_ascii_digit();
                self.scan_ident();
                match self.peek_byte() {
                    b'!' | b'?' if self.lookahead_byte(1) != b'=' => {
                        self.pos += 1;
                        true
                    }
                    b'=' if match self.lookahead_byte(1) {
                        // Ignore foo=> or foo=~
                        b'>' | b'~' => false,
                        // Ignore foo== but consume foo==> specially
                        b'=' => self.lookahead_byte(2) == b'>',
                        _ => true,
                    } =>
                    {
                        self.pos += 1;
                        true
                    }
                    _ => false,
                };
                let s_bytes = &self.bytes()[start..self.pos];
                let s = EStrRef::from_bytes(s_bytes, self.input.encoding());
                if !s.is_valid() {
                    diag.push(Diagnostic {
                        range: CodeRange {
                            start,
                            end: self.pos,
                        },
                        message: format!("The symbol contains invalid characters"),
                    });
                } else if is_numeric {
                    diag.push(Diagnostic {
                        range: CodeRange {
                            start,
                            end: self.pos,
                        },
                        message: format!("The symbol cannot start with a digit"),
                    });
                }
            }
            // `!`
            // `!@`
            // `!=`
            // `!~`
            b'!' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' | b'@' | b'~' => {
                        self.pos += 1;
                    }
                    _ => {}
                }
            }
            b'"' => {
                self.pos += 1;
                return TokenKind::StringBegin;
            }
            b'$' => {
                self.lex_non_local(diag);
            }
            b'%' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        // :%= is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                    _ => {}
                }
            }
            // `&`
            // `&&`
            // `&&=`
            // `&.`
            // `&=`
            b'&' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'&' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                // :&&= is invalid
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start,
                                        end: self.pos,
                                    },
                                    message: format!("Invalid symbol"),
                                });
                            }
                            _ => {
                                // :&& is invalid
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start,
                                        end: self.pos,
                                    },
                                    message: format!("Invalid symbol"),
                                });
                            }
                        }
                    }
                    b'.' => {
                        self.pos += 1;
                        // :&. is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                    b'=' => {
                        self.pos += 1;
                        // :&= is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                    _ => {}
                }
            }
            b'\'' => {
                self.pos += 1;
                return TokenKind::StringBegin;
            }
            b'(' => {
                // :( is invalid. Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            b')' => {
                // :) is invalid. Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            // `*`
            // `**`
            // `**=`
            // `*=`
            b'*' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'*' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                // :**= is invalid
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start,
                                        end: self.pos,
                                    },
                                    message: format!("Invalid symbol"),
                                });
                            }
                            _ => {}
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        // :*= is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                    _ => {}
                }
            }
            // `+`
            // `+@`
            // `+=`
            b'+' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'@' => {
                        self.pos += 1;
                    }
                    // // Interestingly enough, Ruby lexer splits `+=` apart
                    // // in method name context, unlike `*=`. Therefore,
                    // //
                    // // - `{:+=>2}` is valid, while
                    // // - `{:*=>2}` is invalid.
                    // b'=' => {
                    //     self.pos += 1;
                    //     // :+= is invalid
                    //     diag.push(Diagnostic {
                    //         range: CodeRange {
                    //             start,
                    //             end: self.pos,
                    //         },
                    //         message: format!("Invalid symbol"),
                    //     });
                    // }
                    _ => {}
                }
            }
            b',' => {
                // :, is invalid. Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            // `-`
            // `-@`
            // `-=`
            // `->`
            // `-123`
            b'-' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'@' => {
                        self.pos += 1;
                    }
                    // // Interestingly enough, Ruby lexer splits `+=` apart
                    // // in method name context, unlike `*=`. Therefore,
                    // //
                    // // - `{:+=>2}` is valid, while
                    // // - `{:*=>2}` is invalid.
                    // b'=' => {
                    //     self.pos += 1;
                    //     // :-= is invalid
                    //     diag.push(Diagnostic {
                    //         range: CodeRange {
                    //             start,
                    //             end: self.pos,
                    //         },
                    //         message: format!("Invalid symbol"),
                    //     });
                    // }
                    // // Same as above, making `:->:-` valid
                    // b'>' => {
                    //     self.pos += 1;
                    //     // :-> is invalid
                    //     diag.push(Diagnostic {
                    //         range: CodeRange {
                    //             start,
                    //             end: self.pos,
                    //         },
                    //         message: format!("Invalid symbol"),
                    //     });
                    // }
                    _ => {}
                }
            }
            // `.`
            // `..`
            // `...`
            // `.123` (which is an invalid float)
            b'.' => {
                // Neither of :., :.., :... is invalid.
                // Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            // `/`
            // `/=`
            b'/' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        // :/= is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                    _ => {}
                }
            }
            // Remember that the last letter is also ':'.
            // "::" should already have been handled in the caller.
            b':' => unreachable!(),
            b';' => {
                // :; is invalid. Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            b'<' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'<' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                // :<<= is invalid
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start,
                                        end: self.pos,
                                    },
                                    message: format!("Invalid symbol"),
                                });
                            }
                            _ => {}
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'>' => {
                                self.pos += 1;
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            b'=' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                            }
                            _ => {}
                        }
                    }
                    b'>' => {
                        self.pos += 1;
                        // :=> is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                    b'~' => {
                        self.pos += 1;
                    }
                    _ => {
                        // := is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                }
            }
            b'>' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                    }
                    b'>' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                // :>>= is invalid
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start,
                                        end: self.pos,
                                    },
                                    message: format!("Invalid symbol"),
                                });
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            b'?' => {
                // :? is invalid. Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            b'@' => {
                self.lex_non_local(diag);
            }
            b'[' => {
                if self.lookahead_byte(1) == b']' {
                    self.pos += 2;
                    match self.peek_byte() {
                        b'=' => {
                            self.pos += 1;
                        }
                        _ => {}
                    }
                } else {
                    // :[ is invalid otherwise. Split it apart to get more intuitive result.
                    diag.push(Diagnostic {
                        range: CodeRange {
                            start,
                            end: self.pos,
                        },
                        message: format!("Invalid symbol"),
                    });
                }
            }
            b']' => {
                // :] is invalid. Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            b'^' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        // :^= is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                    _ => {}
                }
            }
            b'`' => {
                self.pos += 1;
            }
            b'{' => {
                // :{ is invalid. Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            b'|' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'|' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                // :||= is invalid
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start,
                                        end: self.pos,
                                    },
                                    message: format!("Invalid symbol"),
                                });
                            }
                            _ => {
                                // :|| is invalid
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start,
                                        end: self.pos,
                                    },
                                    message: format!("Invalid symbol"),
                                });
                            }
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        // :|= is invalid
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid symbol"),
                        });
                    }
                    _ => {}
                }
            }
            b'}' => {
                // :} is invalid. Split it apart to get more intuitive result.
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
            b'~' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'@' => {
                        self.pos += 1;
                    }
                    _ => {}
                }
            }
            _ => {
                diag.push(Diagnostic {
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    message: format!("Invalid symbol"),
                });
            }
        };
        TokenKind::Symbol
    }

    /// Scans tokens starting with `$` or `@`.
    fn lex_non_local(&mut self, diag: &mut Vec<Diagnostic>) -> TokenKind {
        let start = self.pos;
        match self.peek_byte() {
            b'@' => {
                self.pos += 1;
                match self.peek_byte() {
                    ident_continue!() => {
                        let is_numeric = self.peek_byte().is_ascii_digit();
                        self.scan_ident();
                        let s = EStrRef::from_bytes(
                            &self.bytes()[start..self.pos],
                            self.input.encoding(),
                        );
                        if is_numeric {
                            diag.push(Diagnostic {
                                range: CodeRange {
                                    start,
                                    end: self.pos,
                                },
                                message: format!("Invalid instance variable name"),
                            });
                        } else if !s.is_valid() {
                            diag.push(Diagnostic {
                                range: CodeRange {
                                    start,
                                    end: self.pos,
                                },
                                message: format!(
                                    "The instance variable name contains invalid characters"
                                ),
                            });
                        }
                        TokenKind::NonLocal(NonLocalKind::Ivar)
                    }
                    b'@' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            ident_continue!() => {
                                let is_numeric = self.peek_byte().is_ascii_digit();
                                self.scan_ident();
                                let s = EStrRef::from_bytes(
                                    &self.bytes()[start..self.pos],
                                    self.input.encoding(),
                                );
                                if is_numeric {
                                    diag.push(Diagnostic {
                                        range: CodeRange {
                                            start,
                                            end: self.pos,
                                        },
                                        message: format!("Invalid class variable name"),
                                    });
                                } else if !s.is_valid() {
                                    diag.push(Diagnostic {
                                        range: CodeRange {
                                            start,
                                            end: self.pos,
                                        },
                                        message: format!(
                                            "The class variable name contains invalid characters"
                                        ),
                                    });
                                }
                                TokenKind::NonLocal(NonLocalKind::Cvar)
                            }
                            _ => {
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start,
                                        end: self.pos,
                                    },
                                    message: format!("Invalid class variable name"),
                                });
                                TokenKind::NonLocal(NonLocalKind::Cvar)
                            }
                        }
                    }
                    _ => {
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid instance variable name"),
                        });
                        TokenKind::NonLocal(NonLocalKind::Ivar)
                    }
                }
            }
            b'$' => {
                self.pos += 1;
                match self.peek_byte() {
                    ident_start!() => {
                        self.scan_ident();
                        let s = EStrRef::from_bytes(
                            &self.bytes()[start..self.pos],
                            self.input.encoding(),
                        );
                        if !s.is_valid() {
                            diag.push(Diagnostic {
                                range: CodeRange {
                                    start,
                                    end: self.pos,
                                },
                                message: format!(
                                    "The global variable name contains invalid characters"
                                ),
                            });
                        }
                        TokenKind::NonLocal(NonLocalKind::Gvar)
                    }
                    b'!' | b'"' | b'$' | b'&' | b'\'' | b'*' | b'+' | b',' | b'.' | b'/' | b':'
                    | b';' | b'<' | b'=' | b'>' | b'?' | b'@' | b'\\' | b'`' | b'~' => {
                        self.pos += 1;
                        TokenKind::NonLocal(NonLocalKind::Gvar)
                    }
                    decimal_digit!() => {
                        // Lexically speaking `$0` behaves similarly to `$1`
                        // except that `$00` is not allowed.
                        if self.peek_byte() == b'0' {
                            self.pos += 1;
                        } else {
                            while self.peek_byte().is_ascii_digit() {
                                self.pos += 1;
                            }
                        }
                        let num_end = self.pos;
                        self.scan_ident();
                        let cont = &self.bytes()[num_end..self.pos];
                        if cont.is_empty() || SUFFIX_KEYWORDS.contains(cont) {
                            // Split legit pair like `$1and 0`
                            self.pos = num_end;
                            TokenKind::NonLocal(NonLocalKind::Gvar)
                        } else {
                            // Otherwise treat the whole token as an invalid global variable
                            // like `$123foo`
                            diag.push(Diagnostic {
                                range: CodeRange {
                                    start,
                                    end: self.pos,
                                },
                                message: format!("Global variable name cannot start with a digit"),
                            });
                            TokenKind::NonLocal(NonLocalKind::Gvar)
                        }
                    }
                    b'-' => {
                        self.pos += 1;
                        let ident_start = self.pos;
                        self.scan_ident();
                        let s = EStrRef::from_bytes(
                            &self.bytes()[ident_start..self.pos],
                            self.input.encoding(),
                        );
                        if s.is_valid() {
                            let next_len = self
                                .input
                                .encoding()
                                .next_len(&self.bytes()[ident_start..], EncodingState::default());
                            if s.is_empty() {
                                // empty (i.e. `$-` alone)
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start: ident_start - 2,
                                        end: self.pos,
                                    },
                                    message: format!("A letter must follow `$-`"),
                                });
                            } else if next_len < s.len() {
                                // More than one character
                                let cont = &s.as_bytes()[next_len..];
                                if SUFFIX_KEYWORDS.contains(cont) {
                                    // Split legit pair like `$-aand 0`
                                    self.pos = ident_start + next_len;
                                } else {
                                    // Otherwise treat the whole token as an invalid global variable
                                    // like `$-foo`
                                    diag.push(Diagnostic {
                                        range: CodeRange {
                                            start: ident_start,
                                            end: self.pos,
                                        },
                                        message: format!("Only a single letter can follow `$-`"),
                                    });
                                }
                            } else {
                                // One character. Okay!
                            }
                        } else {
                            diag.push(Diagnostic {
                                range: CodeRange {
                                    start: ident_start,
                                    end: self.pos,
                                },
                                message: format!(
                                    "The global variable name contains invalid characters"
                                ),
                            });
                        }
                        TokenKind::NonLocal(NonLocalKind::Gvar)
                    }
                    _ => {
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid global variable name"),
                        });
                        TokenKind::NonLocal(NonLocalKind::Gvar)
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn lex_numeric(&mut self, diag: &mut Vec<Diagnostic>) -> TokenKind {
        // For sign:
        // - '+' was already consumed in the caller
        // - For '-', it is tokenized separately so that `-2 ** 2` parses as `-(2 ** 2)`
        // So, we don't need to check for sign here

        let start_mod = if matches!(self.lookbehind_byte(1), b'+' | b'-') {
            self.pos - 1
        } else {
            self.pos
        };

        let base = self.lex_numeric_base();
        let body_start = self.pos;
        self.lex_numeric_body(base);
        let body_end = self.pos;
        self.lex_numeric_tail();
        let tail_end = self.pos;

        let value = self.parse_numeric_value(
            diag,
            CodeRange {
                start: start_mod,
                end: tail_end,
            },
            base,
            &self.bytes()[body_start..body_end],
            &self.bytes()[body_end..tail_end],
        );
        TokenKind::Numeric(value)
    }

    fn lex_numeric_base(&mut self) -> NumericBase {
        let start = self.pos;
        if self.peek_byte() == b'0' {
            self.pos += 1;
            match self.peek_byte() {
                b'b' | b'B' => {
                    self.pos += 1;
                    NumericBase::Bin
                }
                b'o' | b'O' => {
                    self.pos += 1;
                    NumericBase::OctExplicit
                }
                b'd' | b'D' => {
                    self.pos += 1;
                    NumericBase::DecExplicit
                }
                b'x' | b'X' => {
                    self.pos += 1;
                    NumericBase::Hex
                }
                _ => {
                    // Check if there is another digit after the leading zero.
                    while self.peek_byte() == b'_' {
                        self.pos += 1;
                    }
                    let is_oct = self.peek_byte().is_ascii_digit();
                    // In any case, the leading zero is part of the body,
                    // not the base prefix.
                    self.pos = start;

                    if is_oct {
                        // e.g. `00` or `07`
                        NumericBase::OctImplicit
                    } else {
                        // `0` is decimal
                        NumericBase::DecImplicit
                    }
                }
            }
        } else {
            NumericBase::DecImplicit
        }
    }

    fn lex_numeric_body(&mut self, base: NumericBase) {
        loop {
            match self.peek_byte() {
                decimal_digit!() | b'_' => {
                    self.pos += 1;
                }
                hex_letter!() if base == NumericBase::Hex => {
                    self.pos += 1;
                }
                b'e' | b'E'
                    if matches!(self.lookahead_byte(1), b'+' | b'-')
                        && self.lookahead_byte(2).is_ascii_digit() =>
                {
                    self.pos += 3;
                }
                b'e' | b'E' if self.lookahead_byte(1).is_ascii_digit() => {
                    self.pos += 2;
                }
                b'.' if self.lookahead_byte(1).is_ascii_digit() => {
                    self.pos += 1;
                }
                _ => break,
            }
        }
    }

    fn lex_numeric_tail(&mut self) {
        let start = self.pos;
        self.scan_ident();
        let tail = &self.bytes()[start..self.pos];
        if SUFFIX_KEYWORDS.contains(&tail) {
            // Something like `1and` is found and there is a chance
            // that it is a part of a valid expression like `1and 0`.
            self.pos = start;
            return;
        }
    }

    fn parse_numeric_value(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        range: CodeRange,
        base: NumericBase,
        body: &'a [u8],
        tail: &'a [u8],
    ) -> NumericToken {
        let mut is_rational = false;
        let mut is_imaginary = false;
        let mut has_unknown_suffix = false;
        let mut has_invalid_capital_suffix = false;
        let mut has_duplicate_suffixes = false;
        let mut invalid_suffix_order = false;
        for &b in tail {
            has_invalid_capital_suffix = has_invalid_capital_suffix || matches!(b, b'R' | b'I');
            match b {
                b'r' | b'R' => {
                    has_duplicate_suffixes = has_duplicate_suffixes || is_rational;
                    invalid_suffix_order = invalid_suffix_order || is_imaginary;
                    is_rational = true;
                }
                b'i' | b'I' => {
                    has_duplicate_suffixes = has_duplicate_suffixes || is_imaginary;
                    is_imaginary = true;
                }
                _ => {
                    has_unknown_suffix = true;
                }
            }
        }

        let mut body = Cow::Borrowed(body);
        let is_digit = |b: u8| {
            if base == NumericBase::Hex {
                b.is_ascii_hexdigit()
            } else {
                b.is_ascii_digit()
            }
        };
        let invalid_underscore = (0..body.len()).any(|i| {
            body[i] == b'_'
                && (i == 0
                    || i + 1 == body.len()
                    || !is_digit(body[i - 1])
                    || !is_digit(body[i + 1]))
        });
        let has_underscore = body.contains(&b'_');
        if has_underscore {
            // Remove underscore
            body.to_mut().retain(|&b| b != b'_');
        }

        let has_dot = body.contains(&b'.');
        let has_exp = base != NumericBase::Hex && body.iter().any(|&b| b == b'e' || b == b'E');
        let invalid_decimal_octal = base == NumericBase::OctImplicit
            && (has_dot || has_exp || body.iter().any(|&b| b == b'8' || b == b'9'));
        let invalid_base_in_fractional =
            !invalid_decimal_octal && (has_dot || has_exp) && base != NumericBase::DecImplicit;
        let base_mod = if invalid_decimal_octal || invalid_base_in_fractional {
            NumericBase::DecImplicit
        } else {
            base
        };
        let base_value = match base_mod {
            NumericBase::Bin => 2,
            NumericBase::OctExplicit | NumericBase::OctImplicit => 8,
            NumericBase::DecExplicit | NumericBase::DecImplicit => 10,
            NumericBase::Hex => 16,
        };

        let mut pos_dot: Option<usize> = None;
        let mut pos_exp: Option<usize> = None;
        let mut invalid_fractional_exp = false;
        let mut invalid_multiple_dots = false;
        let mut invalid_multiple_exps = false;
        let mut invalid_rational_exps = false;
        let mut invalid_empty_body = false;
        let mut invalid_empty_integral = false;
        let mut invalid_digit = false;

        if body.starts_with(b".") {
            invalid_empty_integral = true;
            body.to_mut().insert(0, b'0');
        } else if body.is_empty() {
            invalid_empty_body = true;
            body.to_mut().push(b'0');
        }

        let mut pos = 0;
        while pos < body.len() {
            match body[pos] {
                b'e' | b'E' if base_mod != NumericBase::Hex => {
                    invalid_rational_exps = invalid_rational_exps || is_rational;
                    if pos_exp.is_none() {
                        pos_exp = Some(pos);
                        pos += 1;
                        if matches!(body[pos], b'+' | b'-') {
                            pos += 1;
                        }
                    } else {
                        // Something like `1e2e3`
                        invalid_multiple_exps = true;
                        // Ignore the second exponent and the rest
                        break;
                    }
                }
                decimal_digit!() | hex_letter!() => {
                    let val = match body[pos] {
                        b'0'..=b'9' => body[pos] - b'0',
                        b'a'..=b'f' => body[pos] - b'a' + 10,
                        b'A'..=b'F' => body[pos] - b'A' + 10,
                        _ => unreachable!(),
                    };
                    if val as i32 >= base_value {
                        invalid_digit = true;
                        body.to_mut()[pos] = (base_value - 1) as u8 + b'0';
                    } else {
                        pos += 1;
                    }
                }
                b'.' => {
                    if pos_exp.is_some() {
                        // Something like `1e3.4`
                        invalid_fractional_exp = true;
                        // Ignore the fractional part
                        break;
                    } else if pos_dot.is_none() {
                        pos_dot = Some(pos);
                        pos += 1;
                    } else {
                        // Something like `1.2.3`
                        invalid_multiple_dots = true;
                        // Ignore the dot to get e.g. `1.23`
                        body.to_mut().remove(pos);
                    }
                }
                _ => unreachable!(),
            }
        }
        if pos < body.len() {
            body.to_mut().truncate(pos);
        }

        // Error reporting. For readability, report only one error per token.
        // Prefer more important errors over mere stylistic issues.
        let msg = if has_unknown_suffix {
            // `1a`, `123.456f`, etc.
            // Error recovery result: ignore the suffix
            Some("Invalid Numeric literal: unknown suffix")
        } else if invalid_base_in_fractional {
            // `0o0.12`, `0x12.3a`, etc.
            // Error recovery result: ignore the base prefix
            Some("Invalid Numeric literal: Float or Rational literal should not have a base prefix")
        } else if invalid_multiple_exps {
            // `7e8e10`, etc.
            // Error recovery result: ignore the second exponent and the rest
            Some("Invalid Numeric literal: exponent part cannot be itself exponential")
        } else if invalid_fractional_exp {
            // `1e2.3`, etc.
            // Error recovery result: ignore the fractional part and the rest, resulting in a floored value
            Some("Invalid Numeric literal: exponent part cannot have a decimal point")
        } else if invalid_digit {
            // `0o9`, `0b5`, etc.
            // Error recovery result: replace the invalid digit with the maximum valid digit
            Some("Invalid Numeric literal: invalid digit in this base")
        } else if invalid_decimal_octal {
            // `09`, `00.12`, etc.
            // Error recovery result: treat as if all the extra leading zeros are not there
            Some("Invalid Numeric literal: decimal literal cannot have leading zeros")
        } else if invalid_multiple_dots {
            // `1.2.3`, etc.
            // Error recovery result: ignore all dots but the first (e.g. `1.2.3` to `1.23`)
            Some("Invalid Numeric literal: it cannot have multiple decimal points")
        } else if invalid_rational_exps {
            // `1e3r`, etc.
            // Error recovery result: interpret the exponent as it is, unless it is too large
            Some("Invalid Numeric literal: Rational literal cannot have an exponent")
        } else if invalid_empty_integral {
            // `.12`, etc.
            // Error recovery result: insert a leading zero (e.g. `.12` to `0.12`)
            // NOTE: the inverse (e.g. `1.`) does not happen because `.` is only included
            //       when it is followed by a digit.
            Some("Invalid Numeric literal: the integral part cannot be empty")
        } else if invalid_empty_body {
            // `0x`, etc.
            // Error recovery result: insert a zero (e.g. `0x` to `0x0`)
            Some("Invalid Numeric literal: the body cannot be empty")
        } else if invalid_underscore {
            // `1__2`, `1_.3`, etc.
            Some("Invalid Numeric literal: invalid underscore placement")
        } else if has_duplicate_suffixes {
            // `1ii`, `1rr`, etc.
            // Error recovery result: ignore the duplicate suffixes
            Some("Invalid Numeric literal: it cannot have duplicate suffixes")
        } else if invalid_suffix_order {
            // `1ir`, etc.
            // Error recovery result: treat as if the suffixes are in the correct order
            Some("Invalid Numeric literal: the suffixes are in the wrong order")
        } else if has_invalid_capital_suffix {
            // `1R`, `1I`, etc.
            // Error recovery result: treat as if the suffixes are in lowercase
            Some("Invalid Numeric literal: the suffixes should be lowercase")
        } else {
            None
        };
        if let Some(msg) = msg {
            diag.push(Diagnostic {
                range,
                message: msg.to_string(),
            });
        }

        let value = if is_rational && (has_dot || has_exp) {
            let exp = if let Some(pos_exp) = pos_exp {
                let exp: i32 = str::from_utf8(&body[pos_exp + 1..])
                    .unwrap()
                    .parse()
                    .unwrap();
                body.to_mut().truncate(pos_exp);
                // Clamp the exponent so that the resulting AST does not
                // consume too much memory compared to the original source.
                exp.clamp(-255, 255)
            } else {
                0
            };
            let point_pos = if let Some(pos_dot) = pos_dot {
                let n = body.len() - (pos_dot + 1);
                body.to_mut().remove(pos_dot);
                n as i32
            } else {
                0
            };
            let fraction =
                BigInt::from_str_radix(str::from_utf8(&body).unwrap(), base_value as u32).unwrap();
            let value = Decimal::from_fraction_and_exponent(fraction, exp - point_pos);
            NumericValue::Rational(value)
        } else if has_dot || has_exp {
            let value: f64 = str::from_utf8(&body).unwrap().parse().unwrap();
            NumericValue::Float(NotNan::new(value).unwrap())
        } else {
            let value: BigInt =
                BigInt::from_str_radix(str::from_utf8(&body).unwrap(), base_value as u32).unwrap();
            if is_rational {
                NumericValue::Rational(Decimal::from(value))
            } else {
                NumericValue::Integer(value)
            }
        };
        NumericToken {
            value,
            imaginary: is_imaginary,
        }
    }

    pub(super) fn lex_string_like(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        state: StringState,
    ) -> Token {
        if let Some(eof_token) = &self.eof_token {
            return eof_token.clone();
        }
        if self.pos >= self.bytes().len() {
            self.eof_token = Some(Token {
                kind: TokenKind::EOF,
                range: CodeRange {
                    start: self.pos,
                    end: self.pos,
                },
                indent: self.indent,
            });
            return Token {
                kind: TokenKind::EOF,
                range: CodeRange {
                    start: self.pos,
                    end: self.pos,
                },
                indent: self.indent,
            };
        }

        let start = self.pos;
        match self.peek_byte() {
            b'\'' if state.delim == StringDelimiter::Quote => {
                self.pos += 1;
                let kind = self.extend_string_end(state);
                Token {
                    kind,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    indent: self.indent,
                }
            }
            b'"' if state.delim == StringDelimiter::DoubleQuote => {
                self.pos += 1;
                let kind = self.extend_string_end(state);
                Token {
                    kind,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    indent: self.indent,
                }
            }
            b'`' if state.delim == StringDelimiter::Backtick => {
                self.pos += 1;
                Token {
                    kind: TokenKind::StringEnd,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    indent: self.indent,
                }
            }
            b'/' if state.delim == StringDelimiter::Slash => {
                self.pos += 1;
                Token {
                    kind: TokenKind::StringEnd,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    indent: self.indent,
                }
            }
            b'#' if state.delim.allow_interpolation() && self.lookahead_interpolation() => {
                self.pos += 1;
                match self.peek_byte() {
                    b'{' => {
                        self.pos += 1;
                        Token {
                            kind: TokenKind::StringInterpolationBegin,
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            indent: self.indent,
                        }
                    }
                    b'@' | b'$' => {
                        self.lex_non_local(diag);
                        Token {
                            kind: TokenKind::StringVarInterpolation,
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            indent: self.indent,
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                while self.pos < self.bytes().len() {
                    match self.peek_byte() {
                        b'\'' if state.delim == StringDelimiter::Quote => break,
                        b'"' if state.delim == StringDelimiter::DoubleQuote => break,
                        b'`' if state.delim == StringDelimiter::Backtick => break,
                        b'/' if state.delim == StringDelimiter::Slash => break,
                        b'#' if state.delim.allow_interpolation()
                            && self.lookahead_interpolation() =>
                        {
                            break
                        }
                        b'\\' if self.pos + 2 <= self.bytes().len() => {
                            self.pos += 2;
                        }
                        0x00..0x80 => {
                            self.pos += 1;
                        }
                        _ => {
                            let len = self
                                .input
                                .encoding()
                                .next_len(&self.bytes()[self.pos..], EncodingState::default());
                            self.pos += len;
                        }
                    }
                }
                Token {
                    kind: TokenKind::StringContent,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                    indent: self.indent,
                }
            }
        }
    }

    fn extend_string_end(&mut self, state: StringState) -> TokenKind {
        if state.allow_label && self.peek_byte() == b':' {
            self.pos += 1;
            TokenKind::StringEndColon
        } else {
            TokenKind::StringEnd
        }
    }

    fn lookahead_interpolation(&self) -> bool {
        match self.lookahead_byte(1) {
            b'{' => true,
            b'@' => match self.lookahead_byte(2) {
                b'@' => match self.lookahead_byte(3) {
                    ident_start!() => true,
                    _ => false,
                },
                ident_start!() => true,
                _ => false,
            },
            b'$' => match self.lookahead_byte(2) {
                ident_start!() => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn scan_ident(&mut self) {
        // Fast path for ASCII identifiers
        loop {
            if self.peek_byte() >= 0x80 {
                break;
            } else if matches!(self.peek_byte(), ident_continue!()) {
                self.pos += 1;
            } else {
                return;
            }
        }

        let enc = self.input.encoding();
        if enc.is_ascii_substring_compatible() {
            // Fast path for ASCII-substring-compatible encodings
            while matches!(self.peek_byte(), ident_continue!()) {
                self.pos += 1;
            }
            return;
        }

        while matches!(self.peek_byte(), ident_continue!()) {
            if self.peek_byte() < 0x80 {
                self.pos += 1;
            } else {
                let len = enc.next_len(&self.bytes()[self.pos..], EncodingState::default());
                self.pos += len;
            }
        }
    }

    fn lex_space(&mut self, _diag: &mut Vec<Diagnostic>, state: LexerState) -> bool {
        let fold = state.fold_newline();
        let start = self.pos;
        loop {
            match self.peek_byte() {
                b'\n' if !fold => {
                    if self.does_force_fold() {
                        // No self.pos += 1 here beause does_force_fold already consumed it
                    } else {
                        // Should be consumed as a token
                        break;
                    }
                }
                b'\n' => {
                    self.pos += 1;
                    self.reset_indent();
                }
                space_inline!() => {
                    self.pos += 1;
                }
                b'\\' => {
                    if self.lookahead_byte(1) == b'\n' {
                        self.pos += 2;
                        self.reset_indent();
                    } else if self.lookahead_byte(1) == b'\r' && self.lookahead_byte(2) == b'\n' {
                        self.pos += 3;
                        self.reset_indent();
                    } else {
                        break;
                    }
                }
                b'#' => {
                    self.skip_line();
                    // Do not eat the next LF (even if it exists)
                    // The LF, if any, should be processed in the next iteration
                }
                _ => {
                    break;
                }
            }
        }
        self.pos > start
    }

    fn does_force_fold(&mut self) -> bool {
        let rollback = (self.pos, self.indent);
        // Skip the current LF byte
        self.pos += 1;
        self.reset_indent();

        loop {
            match self.peek_byte() {
                space_inline!() => {
                    self.pos += 1;
                }
                b'#' => {
                    self.reset_indent();
                    self.skip_line();
                    if self.peek_byte() == b'\n' {
                        self.pos += 1;
                        self.reset_indent();
                    }
                }
                _ => {
                    break;
                }
            }
        }
        // Check if there is '.' (except '..') or '&.', which is force-fold indicator
        let force_fold = match self.peek_byte() {
            b'.' => match self.lookahead_byte(1) {
                b'.' => false,
                _ => true,
            },
            b'&' => match self.lookahead_byte(1) {
                b'.' => true,
                _ => false,
            },
            _ => false,
        };
        if !force_fold {
            // Rollback when not force-folding as we want to re-scan the current LF byte
            // as a Newline token.
            (self.pos, self.indent) = rollback;
        }
        force_fold
    }

    fn reset_indent(&mut self) {
        let mut pos = self.pos;
        while pos > 0 && self.get_byte(pos - 1) != b'\n' {
            pos -= 1;
        }
        let mut indent = 0;
        while pos < self.bytes().len() {
            match self.get_byte(pos) {
                b'\t' => indent += TAB_WIDTH - indent % TAB_WIDTH,
                b' ' => indent += 1,
                _ => break,
            }
            pos += 1;
        }

        self.indent = indent;
    }

    // Stops before the next LF or EOF.
    fn skip_line(&mut self) {
        loop {
            match self.peek_byte() {
                eof_char!() | b'\n' => {
                    break;
                }
                _ => {
                    self.pos += 1;
                }
            }
        }
    }

    fn peek_space(&self) -> bool {
        match self.peek_byte() {
            space!() => true,
            _ => false,
        }
    }

    fn peek_byte(&self) -> u8 {
        self.get_byte(self.pos)
    }

    fn lookahead_byte(&self, offset: usize) -> u8 {
        self.get_byte(self.pos + offset)
    }

    fn lookbehind_byte(&self, offset: usize) -> u8 {
        if self.pos < offset {
            b'\0'
        } else {
            self.get_byte(self.pos - offset)
        }
    }

    fn get_byte(&self, pos: usize) -> u8 {
        self.bytes().get(pos).copied().unwrap_or(0)
    }

    fn select(&self, start: usize, end: usize) -> EStrRef<'a> {
        EStrRef::from_bytes(&self.bytes()[start..end], self.input.encoding())
    }

    fn select_owned(&self, start: usize, end: usize) -> EString {
        self.select(start, end).to_estring()
    }

    fn is_beginning_of_line(&self, pos: usize) -> bool {
        pos == 0 || self.get_byte(pos - 1) == b'\n'
    }
    fn is_end_of_line(&self, pos: usize) -> bool {
        pos == self.bytes().len()
            || self.get_byte(pos) == b'\n'
            || (self.get_byte(pos) == b'\r' && self.get_byte(pos + 1) == b'\n')
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum NumericBase {
    /// `0b` or `0B`
    Bin,
    /// Octal number without "o" prefix, i.e. `/^(?=0_*[0-9])/`
    OctImplicit,
    /// `0o` or `0O`
    OctExplicit,
    /// Decimal number without base prefix, i.e. `/^(?=0(?!_*[0-9])|[1-9])/`
    DecImplicit,
    /// `0d` or `0D`
    DecExplicit,
    /// `0x` or `0X`
    Hex,
}

static KEYWORDS: LazyLock<HashMap<&'static [u8], TokenKind>> = LazyLock::new(|| {
    HashMap::from_iter(vec![
        (
            &b"__ENCODING__"[..],
            TokenKind::KeywordCapitalDoubleUnderscoreEncoding,
        ),
        (b"__LINE__", TokenKind::KeywordCapitalDoubleUnderscoreLine),
        // It requires an additional position check
        (b"__END__", TokenKind::EOF),
        (b"__FILE__", TokenKind::KeywordCapitalDoubleUnderscoreFile),
        (b"BEGIN", TokenKind::KeywordCapitalBegin),
        (b"END", TokenKind::KeywordCapitalEnd),
        (b"alias", TokenKind::KeywordAlias),
        (b"and", TokenKind::KeywordAnd),
        (b"begin", TokenKind::KeywordBegin),
        (b"break", TokenKind::KeywordBreak),
        (b"case", TokenKind::KeywordCase),
        (b"class", TokenKind::KeywordClass),
        (b"def", TokenKind::KeywordDef),
        (b"defined?", TokenKind::KeywordDefinedQ),
        (b"do", TokenKind::KeywordDo),
        (b"else", TokenKind::KeywordElse),
        (b"elsif", TokenKind::KeywordElsif),
        (b"end", TokenKind::KeywordEnd),
        (b"ensure", TokenKind::KeywordEnsure),
        (b"false", TokenKind::KeywordFalse),
        (b"for", TokenKind::KeywordFor),
        (b"if", TokenKind::KeywordIf),
        (b"in", TokenKind::KeywordIn),
        (b"module", TokenKind::KeywordModule),
        (b"next", TokenKind::KeywordNext),
        (b"nil", TokenKind::KeywordNil),
        (b"not", TokenKind::KeywordNot),
        (b"or", TokenKind::KeywordOr),
        (b"redo", TokenKind::KeywordRedo),
        (b"rescue", TokenKind::KeywordRescue),
        (b"retry", TokenKind::KeywordRetry),
        (b"return", TokenKind::KeywordReturn),
        (b"self", TokenKind::KeywordSelf),
        (b"super", TokenKind::KeywordSuper),
        (b"then", TokenKind::KeywordThen),
        (b"true", TokenKind::KeywordTrue),
        (b"undef", TokenKind::KeywordUndef),
        (b"unless", TokenKind::KeywordUnless),
        (b"until", TokenKind::KeywordUntil),
        (b"when", TokenKind::KeywordWhen),
        (b"while", TokenKind::KeywordWhile),
        (b"yield", TokenKind::KeywordYield),
    ])
});

/// List of keywords that can follow an expression.
///
/// Used to determine when to split a seemingly consecutive tokens like
/// `0and` and `$-Ior`.
static SUFFIX_KEYWORDS: LazyLock<HashSet<&'static [u8]>> = LazyLock::new(|| {
    HashSet::from_iter(vec![
        // `0and 1`
        &b"and"[..],
        // `f 1do end`
        b"do",
        // `if 0; 1else end`
        b"else",
        // `if 0; 1elsif 2; end`
        b"elsif",
        // `begin 1end`
        b"end",
        // `begin 1ensure end`
        b"ensure",
        // `1if 0`
        b"if",
        // `1in 1`
        b"in",
        // `1or 2`
        b"or",
        // `1rescue 2`
        b"rescue",
        // `if 1then end`
        b"then",
        // `1unless 0`
        b"unless",
        // `1until 0`
        b"until",
        // `case 0when 1; end`
        b"when",
        // `1while 0`
        b"while",
    ])
});

#[cfg(test)]
mod tests;
