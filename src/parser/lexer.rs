use core::str;
use std::{
    collections::{HashMap, HashSet},
    sync::LazyLock,
};

use ordered_float::NotNan;

use crate::{
    ast::{CodeRange, NumericValue},
    encoding::{EStrRef, EncodingState},
    Diagnostic,
};

const TAB_WIDTH: usize = 8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Token {
    pub(super) kind: TokenKind,
    pub(super) range: CodeRange,
    pub(super) indent: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// `@foo` etc., namely `tIVAR`
    IvarName,
    /// `@@foo` etc., namely `tCVAR`
    CvarName,
    /// `$foo` etc., namely `tGVAR`, `tNTH_REF`, and `tBACK_REF`
    GvarName,

    /// `123` etc., namely `tINTEGER`, `tFLOAT`, `tRATIONAL`, and `tIMAGINARY`
    Numeric,
    /// `?a` etc., namely `tCHAR`
    CharLiteral,

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

    /// `+=` etc., namely `tOP_ASGN`
    OpAssign,

    /// `!`, namely `'!'`
    Excl,
    /// `!=`, namely `tNEQ`
    ExclEq,
    /// `!~`, namely `tNMATCH`
    ExclTilde,
    /// `%`, namely `'%'`
    Percent,
    /// `&`, namely `'&'`
    Amp,
    /// `&` but block argument only, namely `tAMPER`
    AmpPrefix,
    /// `&&`, namely `tANDOP`
    AmpAmp,
    /// `&.`, namely `tANDDOT`
    AmpDot,
    /// `(`, namely `'('` and `tLPAREN`
    LParen,
    /// `(` with restricted syntactic rule (e.g. cannot be an argument list delimiter),
    /// namely `tLPAREN_ARG`
    LParenRestricted,
    /// `)`, namely `')'`
    RParen,
    /// `*`, namely `'*'`
    Star,
    /// `*` but argument splat only, namely `tSTAR`
    StarPrefix,
    /// `**`, namely `tPOW`
    StarStar,
    /// `**` but keyword argument splat only, namely `tDSTAR`
    StarStarPrefix,
    /// `+`, namely `'+'`
    Plus,
    /// `+` but unary operator only, namely `tUPLUS`
    PlusPrefix,
    /// `,`, namely `','`
    Comma,
    /// `-`, namely `'-'`
    Minus,
    /// `-` but unary operator only, namely `tUMINUS` and `tUMINUS_NUM`
    MinusPrefix,
    /// `->`, namely `tLAMBDA`
    Arrow,
    /// `.`, namely `'.'`
    Dot,
    /// `..`, namely `tDOT2` and `tBDOT2`
    DotDot,
    /// `...`, namely `tDOT3` and `tBDOT3`
    DotDotDot,
    /// `/`, namely `'/'`
    Slash,
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
    /// `<`, namely `'<'`
    Lt,
    /// `<<`, namely `tLSHFT`
    LtLt,
    /// `<=`, namely `tLEQ`
    LtEq,
    /// `<=>`, namely `tCMP`
    LtEqGt,
    /// `=`, namely `'='`
    Eq,
    /// `==`, namely `tEQ`
    EqEq,
    /// `===`, namely `tEQQ`
    EqEqEq,
    /// `=>`, namely `tASSOC`
    FatArrow,
    /// `=~`, namely `tMATCH`
    EqMatch,
    /// `>`, namely `'>'`
    Gt,
    /// `>=`, namely `tGEQ`
    GtEq,
    /// `>>`, namely `tRSHFT`
    GtGt,
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
    /// `^`, namely `'^'`
    Caret,
    /// `{`, namely '{', `tLBRACE`, and `tLBRACE_ARG`
    LBrace,
    /// `|`, namely `'|'`
    Vert,
    /// `||`, namely `tOROP`
    VertVert,
    /// `}`, namely `'}'`
    RBrace,
    /// `~`, namely `'~'`
    Tilde,
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
            b'\0' | b'\x04' | b'\x1A' => {
                if self.pos < self.bytes().len() {
                    self.pos += 1;
                }
                TokenKind::EOF
            }
            b'\n' => {
                let orig_indent = self.indent;
                if self.pos >= 1 && self.bytes()[self.pos - 1] == b'\r' {
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
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'.. => {
                while is_ident_continue(self.peek_byte()) {
                    self.pos += 1;
                }
                let is_method_name = match self.peek_byte() {
                    b'!' | b'?' if self.peek_byte_at(1) != b'=' => {
                        self.pos += 1;
                        true
                    }
                    b'=' if state.allow_assigner_ident()
                        && match self.peek_byte_at(1) {
                            // Ignore foo=> or foo=~
                            b'>' | b'~' => false,
                            // Ignore foo== but consume foo==> specially
                            b'=' => self.peek_byte_at(2) == b'>',
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
                    && self.peek_byte_at(1) != b':'
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
            b'0'..=b'9' => self.lex_numeric(diag),
            // `!`
            // `!@`
            // `!=`
            // `!~`
            b'!' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        TokenKind::ExclEq
                    }
                    b'@' if state.extended_method_name() => {
                        self.pos += 1;
                        TokenKind::MethodName
                    }
                    b'~' => {
                        self.pos += 1;
                        TokenKind::ExclTilde
                    }
                    _ => TokenKind::Excl,
                }
            }
            b'"' => {
                self.pos += 1;
                state.string_begin()
            }
            b'#' => {
                unreachable!("Should have been skipped by lex_space");
            }
            b'$' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'.. => {
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
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
                        TokenKind::GvarName
                    }
                    b'!' | b'"' | b'$' | b'&' | b'\'' | b'*' | b'+' | b',' | b'.' | b'/' | b':'
                    | b';' | b'<' | b'=' | b'>' | b'?' | b'@' | b'\\' | b'`' | b'~' => {
                        self.pos += 1;
                        TokenKind::GvarName
                    }
                    b'0'..=b'9' => {
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
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
                        let cont = &self.bytes()[num_end..self.pos];
                        if cont.is_empty() || SUFFIX_KEYWORDS.contains(cont) {
                            // Split legit pair like `$1and 0`
                            self.pos = num_end;
                            TokenKind::GvarName
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
                            TokenKind::GvarName
                        }
                    }
                    b'-' => {
                        self.pos += 1;
                        let ident_start = self.pos;
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
                        let s = EStrRef::from_bytes(
                            &self.bytes()[ident_start..self.pos],
                            self.input.encoding(),
                        );
                        if s.is_valid() {
                            let mut iter = s.char_indices();
                            if iter.next().is_none() {
                                // empty (i.e. `$-` alone)
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start: ident_start,
                                        end: self.pos,
                                    },
                                    message: format!("A letter must follow `$-`"),
                                });
                            } else if let Some((pos, _)) = iter.next() {
                                // More than one character
                                let cont = &s.as_bytes()[pos..];
                                if SUFFIX_KEYWORDS.contains(cont) {
                                    // Split legit pair like `$-aand 0`
                                    self.pos = ident_start + pos;
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
                        TokenKind::GvarName
                    }
                    _ => {
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid global variable name"),
                        });
                        TokenKind::GvarName
                    }
                }
            }
            b'%' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign
                    }
                    _ => TokenKind::Percent,
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
                                TokenKind::OpAssign
                            }
                            _ => TokenKind::AmpAmp,
                        }
                    }
                    b'.' => {
                        self.pos += 1;
                        TokenKind::AmpDot
                    }
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign
                    }
                    _ => {
                        if state.prefer_prefix_operator(space_before, self.peek_space()) {
                            TokenKind::AmpPrefix
                        } else {
                            TokenKind::Amp
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
                                TokenKind::OpAssign
                            }
                            _ => {
                                if state.prefer_prefix_operator(space_before, self.peek_space()) {
                                    TokenKind::StarStarPrefix
                                } else {
                                    TokenKind::StarStar
                                }
                            }
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign
                    }
                    _ => {
                        if state.prefer_prefix_operator(space_before, self.peek_space()) {
                            TokenKind::StarPrefix
                        } else {
                            TokenKind::Star
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
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign
                    }
                    _ => {
                        if state.prefer_prefix_operator(space_before, self.peek_space()) {
                            if self.peek_byte().is_ascii_digit() {
                                // Leave '+' eaten so that lex_numeric need not to check it again
                                self.lex_numeric(diag)
                            } else {
                                TokenKind::PlusPrefix
                            }
                        } else {
                            TokenKind::Plus
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
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign
                    }
                    b'>' => {
                        self.pos += 1;
                        TokenKind::Arrow
                    }
                    _ => {
                        if state.prefer_prefix_operator(space_before, self.peek_space()) {
                            TokenKind::MinusPrefix
                        } else {
                            TokenKind::Minus
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
                    b'0'..=b'9' => {
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
                            TokenKind::OpAssign
                        }
                        _ => TokenKind::Slash,
                    }
                }
            }
            b':' => {
                if self.peek_byte_at(1) == b':' {
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
                                TokenKind::OpAssign
                            }
                            _ => TokenKind::LtLt,
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'>' => {
                                self.pos += 1;
                                TokenKind::LtEqGt
                            }
                            _ => TokenKind::LtEq,
                        }
                    }
                    _ => TokenKind::Lt,
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
                                TokenKind::EqEqEq
                            }
                            _ => TokenKind::EqEq,
                        }
                    }
                    b'>' => {
                        self.pos += 1;
                        TokenKind::FatArrow
                    }
                    b'~' => {
                        self.pos += 1;
                        TokenKind::EqMatch
                    }
                    _ => TokenKind::Eq,
                }
            }
            b'>' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'=' => {
                        self.pos += 1;
                        TokenKind::GtEq
                    }
                    b'>' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'=' => {
                                self.pos += 1;
                                TokenKind::OpAssign
                            }
                            _ => TokenKind::GtGt,
                        }
                    }
                    _ => TokenKind::Gt,
                }
            }
            b'?' => {
                self.pos += 1;
                if state.force_single_question_mark() {
                    TokenKind::Question
                } else {
                    match self.peek_byte() {
                        b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => TokenKind::Question,
                        b if is_ident_continue(b) => {
                            let char_start = self.pos;
                            while is_ident_continue(self.peek_byte()) {
                                self.pos += 1;
                            }
                            if self.pos - char_start == 1 {
                                TokenKind::CharLiteral
                            } else {
                                self.pos = char_start;
                                TokenKind::Question
                            }
                        }
                        _ => TokenKind::Question,
                    }
                }
            }
            b'@' => {
                self.pos += 1;
                match self.peek_byte() {
                    b if is_ident_continue(b) => {
                        let is_numeric = b.is_ascii_digit();
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
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
                        TokenKind::IvarName
                    }
                    b'@' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b if is_ident_continue(b) => {
                                let is_numeric = b.is_ascii_digit();
                                while is_ident_continue(self.peek_byte()) {
                                    self.pos += 1;
                                }
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
                                TokenKind::CvarName
                            }
                            _ => {
                                self.pos -= 1;
                                TokenKind::At
                            }
                        }
                    }
                    _ => TokenKind::At,
                }
            }
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
                        TokenKind::OpAssign
                    }
                    _ => TokenKind::Caret,
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
                                TokenKind::OpAssign
                            }
                            _ if state.split_vert_vert() => {
                                self.pos -= 1;
                                TokenKind::Vert
                            }
                            _ => TokenKind::VertVert,
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign
                    }
                    _ => TokenKind::Vert,
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
                    _ => TokenKind::Tilde,
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
            b'\0' | b'\x04' | b'\x1A' | b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' | b'#' => {
                return TokenKind::Colon;
            }
            b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'.. => {
                let is_numeric = self.peek_byte().is_ascii_digit();
                while is_ident_continue(self.peek_byte()) {
                    self.pos += 1;
                }
                match self.peek_byte() {
                    b'!' | b'?' if self.peek_byte_at(1) != b'=' => {
                        self.pos += 1;
                        true
                    }
                    b'=' if match self.peek_byte_at(1) {
                        // Ignore foo=> or foo=~
                        b'>' | b'~' => false,
                        // Ignore foo== but consume foo==> specially
                        b'=' => self.peek_byte_at(2) == b'>',
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
                self.pos += 1;
                match self.peek_byte() {
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'.. => {
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
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
                                message: format!("The symbol contains invalid characters"),
                            });
                        }
                    }
                    b'!' | b'"' | b'$' | b'&' | b'\'' | b'*' | b'+' | b',' | b'.' | b'/' | b':'
                    | b';' | b'<' | b'=' | b'>' | b'?' | b'@' | b'\\' | b'`' | b'~' => {
                        self.pos += 1;
                    }
                    b'0'..=b'9' => {
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
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
                        let cont = &self.bytes()[num_end..self.pos];
                        if cont.is_empty() || SUFFIX_KEYWORDS.contains(cont) {
                            // Split legit pair like `:$1and 0`
                            self.pos = num_end;
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
                        }
                    }
                    b'-' => {
                        self.pos += 1;
                        let ident_start = self.pos;
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
                        let s = EStrRef::from_bytes(
                            &self.bytes()[ident_start..self.pos],
                            self.input.encoding(),
                        );
                        if s.is_valid() {
                            let mut iter = s.char_indices();
                            if iter.next().is_none() {
                                // empty (i.e. `$-` alone)
                                diag.push(Diagnostic {
                                    range: CodeRange {
                                        start: ident_start,
                                        end: self.pos,
                                    },
                                    message: format!("A letter must follow `$-`"),
                                });
                            } else if let Some((pos, _)) = iter.next() {
                                // More than one character
                                let cont = &s.as_bytes()[pos..];
                                if SUFFIX_KEYWORDS.contains(cont) {
                                    // Split legit pair like `:$-aand 0`
                                    self.pos = ident_start + pos;
                                } else {
                                    // Otherwise treat the whole token as an invalid global variable
                                    // like `:$-foo`
                                    diag.push(Diagnostic {
                                        range: CodeRange {
                                            start: ident_start,
                                            end: self.pos,
                                        },
                                        message: format!("Only a single letter can follow `:$-`"),
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
                                message: format!("The symbol contains invalid characters"),
                            });
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
                }
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
                self.pos += 1;
                match self.peek_byte() {
                    b if is_ident_continue(b) => {
                        let is_numeric = b.is_ascii_digit();
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
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
                                message: format!("Invalid symbol"),
                            });
                        } else if !s.is_valid() {
                            diag.push(Diagnostic {
                                range: CodeRange {
                                    start,
                                    end: self.pos,
                                },
                                message: format!("The symbol contains invalid characters"),
                            });
                        }
                    }
                    b'@' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b if is_ident_continue(b) => {
                                let is_numeric = b.is_ascii_digit();
                                while is_ident_continue(self.peek_byte()) {
                                    self.pos += 1;
                                }
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
                                        message: format!("Invalid symbol"),
                                    });
                                } else if !s.is_valid() {
                                    diag.push(Diagnostic {
                                        range: CodeRange {
                                            start,
                                            end: self.pos,
                                        },
                                        message: format!("The symbol contains invalid characters"),
                                    });
                                }
                            }
                            _ => {
                                // :@@ is invalid
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
                    _ => {
                        // :@ is invalid
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
            b'[' => {
                if self.peek_byte_at(1) == b']' {
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

    fn lex_numeric(&mut self, diag: &mut Vec<Diagnostic>) -> TokenKind {
        // For sign:
        // - '+' was already consumed in the caller
        // - For '-', it is tokenized separately so that `-2 ** 2` parses as `-(2 ** 2)`
        // So, we don't need to check for sign here

        let start = self.pos;

        let body_result = if self.peek_byte() == b'0' {
            self.pos += 1;
            match self.peek_byte() {
                b'b' | b'B' => {
                    self.pos += 1;
                    self.lex_integer_body(|b| b == b'0' || b == b'1')
                }
                b'd' | b'D' => {
                    self.pos += 1;
                    self.lex_integer_body(|b| b.is_ascii_digit())
                }
                b'o' | b'O' => {
                    self.pos += 1;
                    self.lex_integer_body(|b| matches!(b, b'0'..=b'7'))
                }
                b'x' | b'X' => {
                    self.pos += 1;
                    self.lex_integer_body(|b| b.is_ascii_hexdigit())
                }
                b'0'..b'9' | b'_' => {
                    self.pos -= 1;
                    self.lex_integer_body(|b| matches!(b, b'0'..=b'7'))
                }
                _ => {
                    self.pos -= 1;
                    self.lex_decimal_body()
                }
            }
        } else {
            self.lex_decimal_body()
        };
        if let Ok(found_e) = body_result {
            let body_end = self.pos;
            while is_ident_continue(self.peek_byte()) {
                self.pos += 1;
            }
            let suffix = &self.bytes()[body_end..self.pos];
            if SUFFIX_KEYWORDS.contains(&suffix) {
                // Something like `1and` is found and there is a chance
                // that it is a part of a valid expression like `1and 0`.
                self.pos = body_end;
                return TokenKind::Numeric;
            } else if suffix == b""
                || suffix == b"i"
                || (!found_e && suffix == b"r")
                || (!found_e && suffix == b"ri")
            {
                // Valid numeric suffixes.
                // Also check for numeric-like invalid continuations.
                let has_invalid_cont =
                    self.peek_byte() == b'.' && self.peek_byte_at(1).is_ascii_digit();
                if !has_invalid_cont {
                    return TokenKind::Numeric;
                }
            }
        }
        self.pos = start;
        self.lex_erroneous_numeric(diag)
    }

    fn lex_decimal_body(&mut self) -> Result<bool, ()> {
        self.lex_integer_body(|b| b.is_ascii_digit())?;
        if self.peek_byte() == b'.' && self.peek_byte_at(1).is_ascii_digit() {
            self.pos += 1;
            self.lex_integer_body(|b| b.is_ascii_digit())?;
        }
        if matches!(self.peek_byte(), b'e' | b'E')
            && matches!(self.peek_byte_at(1), b'+' | b'-' | b'0'..=b'9')
        {
            self.pos += 1;
            if matches!(self.peek_byte(), b'+' | b'-') {
                self.pos += 1;
            }
            self.lex_integer_body(|b| b.is_ascii_digit())?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn lex_integer_body(&mut self, mut is_digit: impl FnMut(u8) -> bool) -> Result<bool, ()> {
        let start = self.pos;
        let mut underscore = true;
        loop {
            if self.peek_byte() == b'_' {
                if underscore {
                    return Err(());
                }
                underscore = true;
                self.pos += 1;
            } else if is_digit(self.peek_byte()) {
                underscore = false;
                self.pos += 1;
            } else {
                break;
            }
        }
        if !underscore && start < self.pos {
            Ok(false)
        } else {
            Err(())
        }
    }

    fn lex_erroneous_numeric(&mut self, diag: &mut Vec<Diagnostic>) -> TokenKind {
        // Error recovery. It reconsumes the token to get more intuitive boundary.

        // Modified starting point for error reporting
        let start_mod = if self.pos > 0 && matches!(self.bytes()[self.pos - 1], b'+' | b'-') {
            self.pos - 1
        } else {
            self.pos
        };

        let (base, allow_float) = if self.peek_byte() == b'0' {
            self.pos += 1;
            match self.peek_byte() {
                b'b' | b'B' => {
                    self.pos += 1;
                    (2, false)
                }
                b'd' | b'D' => {
                    self.pos += 1;
                    (10, false)
                }
                b'o' | b'O' => {
                    self.pos += 1;
                    (8, false)
                }
                b'x' | b'X' => {
                    self.pos += 1;
                    (16, false)
                }
                b'0'..b'9' | b'_' => {
                    self.pos -= 1;
                    (8, false)
                }
                _ => {
                    self.pos -= 1;
                    (10, true)
                }
            }
        } else {
            (10, true)
        };
        let mut first_invalid_letter: Option<usize> = None;
        let mut num_points = 0;
        let mut num_exponents = 0;
        let mut has_point_after_exponent = false;
        let mut has_invalid_digit = false;
        let mut invalid_leading_underscore = false;
        let mut invalid_trailing_underscore = false;
        let mut invalid_leading_point = false;
        let mut invalid_trailing_point = false;
        loop {
            match self.peek_byte() {
                // exponents (except hexadecimals)
                b'e' | b'E' if base != 16 && self.peek_byte_at(1).is_ascii_digit() => {
                    num_exponents += 1;
                    self.pos += 2;
                }
                b'e' | b'E'
                    if base != 16
                        && matches!(self.peek_byte_at(1), b'+' | b'-')
                        && self.peek_byte_at(2).is_ascii_digit() =>
                {
                    self.pos += 3;
                }
                b'a'..b'f' | b'A'..b'F' if base == 16 => {
                    self.pos += 1;
                }
                b'a'..b'z' | b'A'..b'Z' => {
                    if first_invalid_letter.is_none() {
                        first_invalid_letter = Some(self.pos);
                    }
                    self.pos += 1;
                }
                b'_' => {
                    let a = self.peek_byte_at(1);
                    if !a.is_ascii_digit() && !(a.is_ascii_hexdigit() && base == 16) {
                        invalid_trailing_underscore = true;
                    }
                    let b = self.bytes()[self.pos.saturating_sub(1)];
                    if !b.is_ascii_digit() && !(b.is_ascii_hexdigit() && base == 16) {
                        invalid_leading_underscore = true;
                    }
                    self.pos += 1;
                }
                b'0'..b'9' => {
                    if (self.peek_byte() - b'0') >= base {
                        has_invalid_digit = true;
                    }
                    self.pos += 1;
                }
                // decimal point, only when followed by a decimal digit
                b'.' if self.peek_byte_at(1).is_ascii_digit() => {
                    num_points += 1;
                    if num_exponents > 0 {
                        has_point_after_exponent = true;
                    }
                    let a = self.peek_byte_at(1);
                    if !a.is_ascii_digit() && a != b'_' {
                        invalid_trailing_point = true;
                    }
                    let b = self.bytes()[self.pos.saturating_sub(1)];
                    if !b.is_ascii_digit() && b != b'_' {
                        invalid_leading_point = true;
                    }
                    self.pos += 2;
                }
                _ => {
                    break;
                }
            }
        }
        let mut multiple_r_suffixes = false;
        let mut multiple_i_suffixes = false;
        let mut i_before_r = false;
        let mut invalid_r = false;
        if let Some(pos) = first_invalid_letter {
            let maybe_suffix = &self.bytes()[pos..self.pos];
            if maybe_suffix.iter().all(|&b| b == b'r' || b == b'i') {
                multiple_r_suffixes = maybe_suffix.iter().filter(|&&b| b == b'r').count() > 1;
                multiple_i_suffixes = maybe_suffix.iter().filter(|&&b| b == b'i').count() > 1;
                i_before_r = maybe_suffix.contains(&b'i')
                    && maybe_suffix.contains(&b'r')
                    && maybe_suffix != b"ri";
                invalid_r = num_exponents > 0 && maybe_suffix.contains(&b'r');
                first_invalid_letter = None;
            }
        }

        let range = CodeRange {
            start: start_mod,
            end: self.pos,
        };
        let msg = if let Some(pos) = first_invalid_letter {
            let b = self.bytes()[pos];
            if b < 0x80 {
                format!("Invalid letter in a number: {}", b as char)
            } else {
                format!("Invalid letter in a number")
            }
        } else if !allow_float && (num_exponents > 0 || num_points > 0) {
            format!("Float literal cannot have a base prefix")
        } else if num_points > 1 {
            format!("Multiple decimal points in a number")
        } else if num_exponents > 1 {
            format!("Multiple exponents in a number")
        } else if has_point_after_exponent {
            format!("Decimal point should appear before the exponent")
        } else if invalid_leading_point {
            format!("Numbers cannot start with a decimal point")
        } else if invalid_trailing_point {
            format!("Numbers cannot end with a decimal point")
        } else if has_invalid_digit {
            format!("Invalid digit for base {}", base)
        } else if invalid_leading_underscore {
            format!("Underscore should follow a digit")
        } else if invalid_trailing_underscore {
            format!("Underscore should precede a digit")
        } else if multiple_r_suffixes {
            format!("Multiple 'r' suffixes in a number")
        } else if multiple_i_suffixes {
            format!("Multiple 'i' suffixes in a number")
        } else if i_before_r {
            format!("'i' should precede 'r' in a number")
        } else if invalid_r {
            format!("Rational number cannot have an exponent")
        } else {
            format!("Invalid number")
        };
        diag.push(Diagnostic {
            range,
            message: msg,
        });
        TokenKind::Numeric
    }

    pub(super) fn lex_string_like(
        &mut self,
        _diag: &mut Vec<Diagnostic>,
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
                        self.pos += 1;
                        if self.peek_byte() == b'@' {
                            self.pos += 1;
                        }
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
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
        match self.peek_byte_at(1) {
            b'{' => true,
            b'@' => match self.peek_byte_at(2) {
                b'@' => match self.peek_byte_at(3) {
                    b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'\x80'.. => true,
                    _ => false,
                },
                b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'\x80'.. => true,
                _ => false,
            },
            b'$' => match self.peek_byte_at(2) {
                b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'\x80'.. => true,
                _ => false,
            },
            _ => false,
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
                b'\t' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
                    self.pos += 1;
                }
                b'\\' => {
                    if self.peek_byte_at(1) == b'\n' {
                        self.pos += 2;
                        self.reset_indent();
                    } else if self.peek_byte_at(1) == b'\r' && self.peek_byte_at(2) == b'\n' {
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
                b'\t' | b'\x0B' | b'\x0C' | b'\r' | b' ' => {
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
            b'.' => match self.peek_byte_at(1) {
                b'.' => false,
                _ => true,
            },
            b'&' => match self.peek_byte_at(1) {
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
        while pos > 0 && self.bytes()[pos - 1] != b'\n' {
            pos -= 1;
        }
        let mut indent = 0;
        while pos < self.bytes().len() {
            match self.bytes()[pos] {
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
                b'\0' | b'\x04' | b'\x1A' | b'\n' => {
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
            b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => true,
            _ => false,
        }
    }

    fn peek_byte(&self) -> u8 {
        self.bytes().get(self.pos).copied().unwrap_or(0)
    }

    fn peek_byte_at(&self, offset: usize) -> u8 {
        self.bytes().get(self.pos + offset).copied().unwrap_or(0)
    }

    fn is_beginning_of_line(&self, pos: usize) -> bool {
        pos == 0 || self.bytes()[pos - 1] == b'\n'
    }
    fn is_end_of_line(&self, pos: usize) -> bool {
        pos == self.bytes().len()
            || self.bytes()[pos] == b'\n'
            || (self.bytes()[pos] == b'\r' && self.bytes().get(pos + 1).copied() == Some(b'\n'))
    }
}

// fn is_ident_start(b: u8) -> bool {
//     b.is_ascii_alphabetic() || b == b'_' || b >= 0x80
// }
fn is_ident_continue(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b >= 0x80
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

pub(crate) fn interpret_numeric(mut s: &[u8]) -> (NumericValue, bool) {
    // Validation is already done by the lexer,
    // so it is just sufficient to loosely parse it.

    // Strip the suffixes and prefixes
    let mut neg = false;
    let mut base = 10;
    let mut imaginary = false;
    let mut rational = false;
    loop {
        if s.starts_with(b"+") {
            s = &s[1..];
            continue;
        } else if s.starts_with(b"-") {
            s = &s[1..];
            neg = true;
            continue;
        }
        break;
    }
    if s.len() >= 2 && s[0] == b'0' {
        match s[1] {
            b'b' | b'B' => {
                base = 2;
                s = &s[2..];
            }
            b'd' | b'D' => {
                base = 10;
                s = &s[2..];
            }
            b'o' | b'O' => {
                base = 8;
                s = &s[2..];
            }
            b'x' | b'X' => {
                base = 16;
                s = &s[2..];
            }
            b'0'..=b'9' | b'_' => {
                base = 8;
            }
            _ => {}
        }
    }
    loop {
        if s.ends_with(b"i") {
            s = &s[..s.len() - 1];
            imaginary = true;
            continue;
        } else if s.ends_with(b"r") {
            s = &s[..s.len() - 1];
            rational = true;
            continue;
        }
        break;
    }
    let value = if !rational
        && (s.contains(&b'.') || (base != 16 && s.iter().any(|&b| b == b'e' || b == b'E')))
    {
        // Float
        if let Some(result) = str::from_utf8(s).ok().and_then(|s| s.parse::<f64>().ok()) {
            NumericValue::Float(NotNan::new(result).unwrap_or_default())
        } else {
            // Get the source reformatted
            let mut src = String::new();
            loop {
                match s.get(0).copied().unwrap_or(b'\0') {
                    b'0'..=b'9' => {
                        src.push(s[0] as char);
                        s = &s[1..];
                    }
                    b'_' => {
                        s = &s[1..];
                    }
                    _ => break,
                }
            }
            if src == "" {
                src.push('0');
            }
            if s.starts_with(b".") {
                src.push('.');
                s = &s[1..];
                let len = s.len();
                loop {
                    match s.get(0).copied().unwrap_or(b'\0') {
                        b'0'..=b'9' => {
                            src.push(s[0] as char);
                            s = &s[1..];
                        }
                        b'_' => {
                            s = &s[1..];
                        }
                        _ => break,
                    }
                }
                if len == s.len() {
                    src.push('0');
                }
            }
            if s.starts_with(b"e") || s.starts_with(b"E") {
                src.push('e');
                s = &s[1..];
                if s.starts_with(b"-") {
                    src.push('-');
                    s = &s[1..];
                } else if s.starts_with(b"+") {
                    s = &s[1..];
                }
                let len = s.len();
                loop {
                    match s.get(0).copied().unwrap_or(b'\0') {
                        b'0'..=b'9' => {
                            src.push(s[0] as char);
                            s = &s[1..];
                        }
                        b'_' => {
                            s = &s[1..];
                        }
                        _ => break,
                    }
                }
                if len == s.len() {
                    src.push('0');
                }
            }
            NumericValue::Float(NotNan::new(src.parse().unwrap_or_default()).unwrap_or_default())
        }
    } else if rational && base == 10 {
        // Decimal Rational
        let mut numerator = 0;
        let mut denominator = 1;
        loop {
            match s.get(0).copied().unwrap_or(b'\0') {
                b'0'..=b'9' => {
                    numerator = numerator * 10 + (s[0] - b'0') as i32;
                    s = &s[1..];
                }
                b'_' => {
                    s = &s[1..];
                }
                _ => break,
            }
        }
        if s.starts_with(b".") {
            s = &s[1..];
            loop {
                match s.get(0).copied().unwrap_or(b'\0') {
                    b'0'..=b'9' => {
                        numerator = numerator * 10 + (s[0] - b'0') as i32;
                        denominator *= 10;
                        s = &s[1..];
                    }
                    b'_' => {
                        s = &s[1..];
                    }
                    _ => break,
                }
            }
        }
        NumericValue::Rational(numerator, denominator)
    } else {
        // Integer
        let mut value = 0;
        loop {
            match s.get(0).copied().unwrap_or(b'\0') {
                b'0'..=b'9' => {
                    value = value * base + (s[0] - b'0') as i32;
                    s = &s[1..];
                }
                b'a'..=b'f' => {
                    value = value * base + (s[0] - b'a' + 10) as i32;
                    s = &s[1..];
                }
                b'A'..=b'F' => {
                    value = value * base + (s[0] - b'A' + 10) as i32;
                    s = &s[1..];
                }
                b'_' => {
                    s = &s[1..];
                }
                _ => break,
            }
        }
        if rational {
            NumericValue::Rational(value, 1)
        } else {
            NumericValue::Integer(value)
        }
    };
    let value = if neg {
        match value {
            NumericValue::Integer(v) => NumericValue::Integer(-v),
            NumericValue::Float(v) => NumericValue::Float(-v),
            NumericValue::Rational(n, d) => NumericValue::Rational(-n, d),
        }
    } else {
        value
    };
    (value, imaginary)
}

#[cfg(test)]
mod tests {
    use crate::{ast::pos_in, Encoding};

    use super::*;

    fn lex_all_from(input: EStrRef<'_>, state: LexerState) -> Vec<Token> {
        let (tokens, diag) = lex_all_with_diag_from(input, state);
        assert_eq!(diag, Vec::new());
        tokens
    }

    fn lex_all_with_diag_from(
        input: EStrRef<'_>,
        mut state: LexerState,
    ) -> (Vec<Token>, Vec<Diagnostic>) {
        let mut diag = Vec::<Diagnostic>::new();
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        let mut string_stack = Vec::<StringState>::new();
        let mut current_string: Option<StringState> = None;
        loop {
            if let Some(current_string_) = &current_string {
                let token = lexer.lex_string_like(&mut diag, current_string_.clone());
                if token.kind == TokenKind::EOF {
                    if token.range
                        != (CodeRange {
                            start: input.len(),
                            end: input.len(),
                        })
                    {
                        tokens.push(token);
                    }
                    break;
                } else if matches!(token.kind, TokenKind::StringEnd | TokenKind::StringEndColon) {
                    current_string = None;
                } else if matches!(token.kind, TokenKind::StringInterpolationBegin) {
                    string_stack.push(current_string.take().unwrap());
                }
                tokens.push(token);
            } else {
                let token = lexer.lex(&mut diag, state);
                if token.kind == TokenKind::EOF {
                    if token.range
                        != (CodeRange {
                            start: input.len(),
                            end: input.len(),
                        })
                    {
                        tokens.push(token);
                    }
                    break;
                }
                if matches!(
                    token.kind,
                    TokenKind::StringBegin | TokenKind::StringBeginLabelable
                ) {
                    current_string = Some(next_string_state_for_testing(&token, input.as_bytes()));
                } else if matches!(token.kind, TokenKind::RBrace) && !string_stack.is_empty() {
                    current_string = Some(string_stack.pop().unwrap());
                } else {
                    state = next_state_for_testing(&token, state);
                }
                tokens.push(token);
            }
        }
        (tokens, diag)
    }

    fn next_state_for_testing(tok: &Token, prev: LexerState) -> LexerState {
        match tok.kind {
            TokenKind::KeywordCapitalDoubleUnderscoreEncoding => LexerState::End,
            TokenKind::KeywordCapitalDoubleUnderscoreLine => LexerState::End,
            TokenKind::KeywordCapitalDoubleUnderscoreFile => LexerState::End,
            TokenKind::KeywordCapitalBegin => LexerState::End,
            TokenKind::KeywordCapitalEnd => LexerState::End,
            TokenKind::KeywordAlias => LexerState::MethOrSymbolForDef,
            TokenKind::KeywordAnd => LexerState::Begin,
            TokenKind::KeywordBegin => LexerState::Begin,
            TokenKind::KeywordBreak => LexerState::Begin,
            TokenKind::KeywordCase => LexerState::Begin,
            TokenKind::KeywordClass => LexerState::ClassName,
            TokenKind::KeywordDef => LexerState::MethForDef,
            TokenKind::KeywordDefinedQ => LexerState::FirstArgument,
            TokenKind::KeywordDo => LexerState::Begin,
            TokenKind::KeywordElse => LexerState::Begin,
            TokenKind::KeywordElsif => LexerState::Begin,
            TokenKind::KeywordEnd => LexerState::End,
            TokenKind::KeywordEnsure => LexerState::Begin,
            TokenKind::KeywordFalse => LexerState::End,
            TokenKind::KeywordFor => LexerState::Begin,
            TokenKind::KeywordIf => LexerState::Begin,
            TokenKind::KeywordIfInfix => LexerState::BeginLabelable,
            TokenKind::KeywordIn => LexerState::Begin,
            TokenKind::KeywordModule => LexerState::Begin,
            TokenKind::KeywordNext => LexerState::Begin,
            TokenKind::KeywordNil => LexerState::End,
            TokenKind::KeywordNot => LexerState::FirstArgument,
            TokenKind::KeywordOr => LexerState::Begin,
            TokenKind::KeywordRedo => LexerState::End,
            TokenKind::KeywordRescue => LexerState::BeginOpt,
            TokenKind::KeywordRetry => LexerState::End,
            TokenKind::KeywordReturn => LexerState::Begin,
            TokenKind::KeywordSelf => LexerState::End,
            TokenKind::KeywordSuper => LexerState::FirstArgument,
            TokenKind::KeywordThen => LexerState::Begin,
            TokenKind::KeywordTrue => LexerState::End,
            TokenKind::KeywordUndef => LexerState::MethOrSymbolForDef,
            TokenKind::KeywordUnless => LexerState::Begin,
            TokenKind::KeywordUnlessInfix => LexerState::BeginLabelable,
            TokenKind::KeywordUntil => LexerState::Begin,
            TokenKind::KeywordUntilInfix => LexerState::BeginLabelable,
            TokenKind::KeywordWhen => LexerState::Begin,
            TokenKind::KeywordWhile => LexerState::Begin,
            TokenKind::KeywordWhileInfix => LexerState::BeginLabelable,
            TokenKind::KeywordYield => LexerState::BeginOpt,
            TokenKind::Identifier => LexerState::WeakFirstArgument,
            TokenKind::Const => LexerState::End,
            TokenKind::MethodName => LexerState::FirstArgument,
            TokenKind::Label => LexerState::Begin,
            TokenKind::Symbol => LexerState::End,
            TokenKind::IvarName => LexerState::End,
            TokenKind::CvarName => LexerState::End,
            TokenKind::GvarName => LexerState::End,
            TokenKind::Numeric => LexerState::End,
            TokenKind::CharLiteral => LexerState::End,
            TokenKind::StringBegin | TokenKind::StringBeginLabelable => {
                unreachable!()
            }
            TokenKind::StringEnd => LexerState::End,
            TokenKind::StringEndColon => LexerState::Begin,
            TokenKind::StringContent => prev,
            TokenKind::StringInterpolationBegin => LexerState::Begin,
            TokenKind::StringVarInterpolation => LexerState::End,
            TokenKind::OpAssign => LexerState::Begin,
            TokenKind::Excl => LexerState::Begin,
            TokenKind::ExclEq => LexerState::Begin,
            TokenKind::ExclTilde => LexerState::Begin,
            TokenKind::Percent => LexerState::Begin,
            TokenKind::Amp => LexerState::Begin,
            TokenKind::AmpPrefix => LexerState::Begin,
            TokenKind::AmpAmp => LexerState::Begin,
            TokenKind::AmpDot => LexerState::MethForCall,
            TokenKind::LParen => LexerState::BeginLabelable,
            TokenKind::LParenRestricted => LexerState::BeginLabelable,
            TokenKind::RParen => LexerState::End,
            TokenKind::Star => LexerState::Begin,
            TokenKind::StarPrefix => LexerState::Begin,
            TokenKind::StarStar => LexerState::Begin,
            TokenKind::StarStarPrefix => LexerState::Begin,
            TokenKind::Plus => LexerState::Begin,
            TokenKind::PlusPrefix => LexerState::Begin,
            TokenKind::Comma => LexerState::BeginLabelable,
            TokenKind::Minus => LexerState::Begin,
            TokenKind::MinusPrefix => LexerState::Begin,
            TokenKind::Arrow => LexerState::End,
            TokenKind::Dot => LexerState::MethForCall,
            TokenKind::DotDot => LexerState::Begin,
            TokenKind::DotDotDot => LexerState::Begin,
            TokenKind::Slash => LexerState::Begin,
            TokenKind::Colon => LexerState::Begin,
            TokenKind::ColonColon => LexerState::MethForCall,
            TokenKind::ColonColonPrefix => LexerState::Begin,
            TokenKind::Semicolon => LexerState::Begin,
            TokenKind::Newline => LexerState::Begin,
            TokenKind::Lt => LexerState::Begin,
            TokenKind::LtLt => LexerState::Begin,
            TokenKind::LtEq => LexerState::Begin,
            TokenKind::LtEqGt => LexerState::Begin,
            TokenKind::Eq => LexerState::Begin,
            TokenKind::EqEq => LexerState::Begin,
            TokenKind::EqEqEq => LexerState::Begin,
            TokenKind::FatArrow => LexerState::Begin,
            TokenKind::EqMatch => LexerState::Begin,
            TokenKind::Gt => LexerState::Begin,
            TokenKind::GtEq => LexerState::Begin,
            TokenKind::GtGt => LexerState::Begin,
            TokenKind::Question => LexerState::Begin,
            TokenKind::At => LexerState::Begin,
            TokenKind::LBracket => LexerState::BeginLabelable,
            TokenKind::LBracketPrefix => LexerState::BeginLabelable,
            TokenKind::RBracket => LexerState::End,
            TokenKind::Caret => LexerState::Begin,
            TokenKind::LBrace => LexerState::Begin,
            TokenKind::Vert => LexerState::BeginLabelable,
            TokenKind::VertVert => LexerState::Begin,
            TokenKind::RBrace => LexerState::End,
            TokenKind::Tilde => LexerState::Begin,
            TokenKind::EOF => LexerState::Begin,
            TokenKind::Unknown => LexerState::Begin,
        }
    }

    fn next_string_state_for_testing(tok: &Token, bytes: &[u8]) -> StringState {
        StringState {
            delim: match bytes[tok.range.start] {
                b'\'' => StringDelimiter::Quote,
                b'"' => StringDelimiter::DoubleQuote,
                b'`' => StringDelimiter::Backtick,
                b'/' => StringDelimiter::Slash,
                _ => unreachable!(),
            },
            allow_label: tok.kind == TokenKind::StringBeginLabelable,
        }
    }

    fn token(kind: TokenKind, range: CodeRange, indent: usize) -> Token {
        Token {
            kind,
            range,
            indent,
        }
    }

    const ALL_STATES: [LexerState; 10] = [
        LexerState::Begin,
        LexerState::ClassName,
        LexerState::BeginOpt,
        LexerState::BeginLabelable,
        LexerState::FirstArgument,
        LexerState::WeakFirstArgument,
        LexerState::End,
        LexerState::MethForDef,
        LexerState::MethOrSymbolForDef,
        LexerState::MethForCall,
    ];

    #[track_caller]
    fn assert_lex<'a, S, F>(src: S, expected: F)
    where
        S: Into<EStrRef<'a>>,
        F: FnOnce(EStrRef<'_>) -> Vec<Token>,
    {
        assert_lex_impl(src, expected, |_| true);
    }

    #[track_caller]
    fn assert_lex_for<'a, S, F>(src: S, states: &[LexerState], expected: F)
    where
        S: Into<EStrRef<'a>>,
        F: FnOnce(EStrRef<'_>) -> Vec<Token>,
    {
        assert_lex_impl(src, expected, |state| states.contains(&state));
    }

    #[track_caller]
    fn assert_lex_except<'a, S, F>(src: S, states: &[LexerState], expected: F)
    where
        S: Into<EStrRef<'a>>,
        F: FnOnce(EStrRef<'_>) -> Vec<Token>,
    {
        assert_lex_impl(src, expected, |state| !states.contains(&state));
    }

    #[track_caller]
    fn assert_lex_impl<'a, S, F, FS>(src: S, expected: F, mut filter_state: FS)
    where
        S: Into<EStrRef<'a>>,
        F: FnOnce(EStrRef<'_>) -> Vec<Token>,
        FS: FnMut(LexerState) -> bool,
    {
        let src = <S as Into<EStrRef<'a>>>::into(src);
        let expected = expected(src);
        let mut difflist1 = Vec::<(LexerState, Vec<Token>)>::new();
        let mut difflist2 = Vec::<(LexerState, Vec<Token>)>::new();
        for &state in &ALL_STATES {
            if !filter_state(state) {
                continue;
            }
            let actual = lex_all_from(src, state);
            if actual != expected {
                difflist1.push((state, actual));
                difflist2.push((state, expected.clone()));
            }
        }
        assert_eq!(difflist1, difflist2);
    }

    #[test]
    fn test_lex_eof_nul() {
        assert_lex("\0", |src| {
            vec![token(TokenKind::EOF, pos_in(src, b"\0", 0), 0)]
        });
    }

    #[test]
    fn test_lex_eof_eot() {
        assert_lex("\x04", |src| {
            vec![token(TokenKind::EOF, pos_in(src, b"\x04", 0), 0)]
        });
    }

    #[test]
    fn test_lex_eof_sub() {
        assert_lex("\x1A", |src| {
            vec![token(TokenKind::EOF, pos_in(src, b"\x1A", 0), 0)]
        });
    }

    #[test]
    fn test_lex_eof_end_token_first_line() {
        assert_lex("__END__", |src| {
            vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
        });
    }

    #[test]
    fn test_lex_eof_end_token_second_line() {
        assert_lex("foo\n__END__", |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
                token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
                token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0),
            ]
        });
    }

    #[test]
    fn test_lex_eof_end_token_eof() {
        assert_lex("__END__", |src| {
            vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
        });
    }

    #[test]
    fn test_lex_eof_end_token_lf() {
        assert_lex("__END__\n", |src| {
            vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
        });
    }

    #[test]
    fn test_lex_eof_end_token_crlf() {
        assert_lex("__END__\r\n", |src| {
            vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
        });
    }

    #[test]
    fn test_lex_non_eof_space_after_end_token() {
        assert_lex("__END__ \n", |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 0),
                token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
            ]
        });
    }

    #[test]
    fn test_lex_non_eof_space_before_end_token() {
        assert_lex(" __END__\n", |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 1),
                token(TokenKind::Newline, pos_in(src, b"\n", 0), 1),
            ]
        });
    }

    #[test]
    fn test_lex_non_eof_cr_after_end_token() {
        assert_lex("__END__\r", |src| {
            vec![token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 0)]
        });
    }

    #[test]
    fn test_lex_spaces_space() {
        assert_lex(" 1", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 1)]
        });
    }

    #[test]
    fn test_lex_spaces_tab() {
        assert_lex("\t1", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 8)]
        });
    }

    #[test]
    fn test_lex_spaces_tab_and_space() {
        assert_lex("\t 1", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 9)]
        });
    }

    #[test]
    fn test_lex_spaces_space_and_tab() {
        assert_lex(" \t1", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 8)]
        });
    }

    #[test]
    fn test_lex_spaces_vtab() {
        assert_lex("\x0B1", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 0)]
        });
    }

    #[test]
    fn test_lex_spaces_ff() {
        assert_lex("\x0C1", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 0)]
        });
    }

    #[test]
    fn test_lex_spaces_cr() {
        assert_lex("\r1", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 0)]
        });
    }

    #[test]
    fn test_lex_spaces_multiple() {
        assert_lex("  1", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 2)]
        });
    }

    #[test]
    fn test_lex_spaces_lf() {
        assert_lex_except(
            "\n1",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 0)],
        );
    }

    #[test]
    fn test_lex_semicolon_lf() {
        assert_lex_for(
            "\n",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| vec![token(TokenKind::Newline, pos_in(src, b"\n", 0), 0)],
        );
    }

    #[test]
    fn test_lex_semicolon_crlf() {
        assert_lex_for(
            "\r\n",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| vec![token(TokenKind::Newline, pos_in(src, b"\r\n", 0), 0)],
        );
    }

    #[test]
    fn test_lex_skip_lf_before_dot() {
        assert_lex_for(
            "\n  .bar",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::Dot, pos_in(src, b".", 0), 2),
                    token(TokenKind::Identifier, pos_in(src, b"bar", 0), 2),
                ]
            },
        );
    }

    #[test]
    fn test_lex_lf_before_dot_dot() {
        assert_lex_for(
            "\n  ..bar",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![
                    token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
                    token(TokenKind::DotDot, pos_in(src, b"..", 0), 2),
                    token(TokenKind::Identifier, pos_in(src, b"bar", 0), 2),
                ]
            },
        );
    }

    #[test]
    fn test_lex_comments() {
        assert_lex_except(
            "# comment2\nfoo bar # comment1\n# comment3\nbaz\n",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
                    token(TokenKind::Identifier, pos_in(src, b"bar", 0), 0),
                    token(TokenKind::Newline, pos_in(src, b"\n", 1), 0),
                    token(TokenKind::Identifier, pos_in(src, b"baz", 0), 0),
                    token(TokenKind::Newline, pos_in(src, b"\n", 3), 0),
                ]
            },
        );
    }

    #[test]
    fn test_lex_keyword_encoding() {
        assert_lex("__ENCODING__", |src| {
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreEncoding,
                pos_in(src, b"__ENCODING__", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_line() {
        assert_lex("__LINE__", |src| {
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreLine,
                pos_in(src, b"__LINE__", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_file() {
        assert_lex("__FILE__", |src| {
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreFile,
                pos_in(src, b"__FILE__", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_cap_begin() {
        assert_lex("BEGIN", |src| {
            vec![token(
                TokenKind::KeywordCapitalBegin,
                pos_in(src, b"BEGIN", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_cap_end() {
        assert_lex("END", |src| {
            vec![token(
                TokenKind::KeywordCapitalEnd,
                pos_in(src, b"END", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_alias() {
        assert_lex("alias", |src| {
            vec![token(TokenKind::KeywordAlias, pos_in(src, b"alias", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_and() {
        assert_lex("and", |src| {
            vec![token(TokenKind::KeywordAnd, pos_in(src, b"and", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_begin() {
        assert_lex("begin", |src| {
            vec![token(TokenKind::KeywordBegin, pos_in(src, b"begin", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_break() {
        assert_lex("break", |src| {
            vec![token(TokenKind::KeywordBreak, pos_in(src, b"break", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_case() {
        assert_lex("case", |src| {
            vec![token(TokenKind::KeywordCase, pos_in(src, b"case", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_class() {
        assert_lex("class", |src| {
            vec![token(TokenKind::KeywordClass, pos_in(src, b"class", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_def() {
        assert_lex("def", |src| {
            vec![token(TokenKind::KeywordDef, pos_in(src, b"def", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_defined_q() {
        assert_lex("defined?", |src| {
            vec![token(
                TokenKind::KeywordDefinedQ,
                pos_in(src, b"defined?", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_do() {
        assert_lex("do", |src| {
            vec![token(TokenKind::KeywordDo, pos_in(src, b"do", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_else() {
        assert_lex("else", |src| {
            vec![token(TokenKind::KeywordElse, pos_in(src, b"else", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_elsif() {
        assert_lex("elsif", |src| {
            vec![token(TokenKind::KeywordElsif, pos_in(src, b"elsif", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_end() {
        assert_lex("end", |src| {
            vec![token(TokenKind::KeywordEnd, pos_in(src, b"end", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_ensure() {
        assert_lex("ensure", |src| {
            vec![token(
                TokenKind::KeywordEnsure,
                pos_in(src, b"ensure", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_false() {
        assert_lex("false", |src| {
            vec![token(TokenKind::KeywordFalse, pos_in(src, b"false", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_for() {
        assert_lex("for", |src| {
            vec![token(TokenKind::KeywordFor, pos_in(src, b"for", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_if_prefix() {
        assert_lex_except(
            "if",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| vec![token(TokenKind::KeywordIf, pos_in(src, b"if", 0), 0)],
        );
    }

    #[test]
    fn test_lex_keyword_if_infix() {
        assert_lex_for(
            "if",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| vec![token(TokenKind::KeywordIfInfix, pos_in(src, b"if", 0), 0)],
        );
    }

    #[test]
    fn test_lex_keyword_in() {
        assert_lex("in", |src| {
            vec![token(TokenKind::KeywordIn, pos_in(src, b"in", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_module() {
        assert_lex("module", |src| {
            vec![token(
                TokenKind::KeywordModule,
                pos_in(src, b"module", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_next() {
        assert_lex("next", |src| {
            vec![token(TokenKind::KeywordNext, pos_in(src, b"next", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_nil() {
        assert_lex("nil", |src| {
            vec![token(TokenKind::KeywordNil, pos_in(src, b"nil", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_not() {
        assert_lex("not", |src| {
            vec![token(TokenKind::KeywordNot, pos_in(src, b"not", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_or() {
        assert_lex("or", |src| {
            vec![token(TokenKind::KeywordOr, pos_in(src, b"or", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_redo() {
        assert_lex("redo", |src| {
            vec![token(TokenKind::KeywordRedo, pos_in(src, b"redo", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_rescue() {
        assert_lex("rescue", |src| {
            vec![token(
                TokenKind::KeywordRescue,
                pos_in(src, b"rescue", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_retry() {
        assert_lex("retry", |src| {
            vec![token(TokenKind::KeywordRetry, pos_in(src, b"retry", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_return() {
        assert_lex("return", |src| {
            vec![token(
                TokenKind::KeywordReturn,
                pos_in(src, b"return", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_keyword_self() {
        assert_lex("self", |src| {
            vec![token(TokenKind::KeywordSelf, pos_in(src, b"self", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_super() {
        assert_lex("super", |src| {
            vec![token(TokenKind::KeywordSuper, pos_in(src, b"super", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_then() {
        assert_lex("then", |src| {
            vec![token(TokenKind::KeywordThen, pos_in(src, b"then", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_true() {
        assert_lex("true", |src| {
            vec![token(TokenKind::KeywordTrue, pos_in(src, b"true", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_undef() {
        assert_lex("undef", |src| {
            vec![token(TokenKind::KeywordUndef, pos_in(src, b"undef", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_unless_prefix() {
        assert_lex_except(
            "unless",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![token(
                    TokenKind::KeywordUnless,
                    pos_in(src, b"unless", 0),
                    0,
                )]
            },
        );
    }

    #[test]
    fn test_lex_keyword_unless_infix() {
        assert_lex_for(
            "unless",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![token(
                    TokenKind::KeywordUnlessInfix,
                    pos_in(src, b"unless", 0),
                    0,
                )]
            },
        );
    }

    #[test]
    fn test_lex_keyword_until_prefix() {
        assert_lex_except(
            "until",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| vec![token(TokenKind::KeywordUntil, pos_in(src, b"until", 0), 0)],
        );
    }

    #[test]
    fn test_lex_keyword_until_infix() {
        assert_lex_for(
            "until",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![token(
                    TokenKind::KeywordUntilInfix,
                    pos_in(src, b"until", 0),
                    0,
                )]
            },
        );
    }

    #[test]
    fn test_lex_keyword_when() {
        assert_lex("when", |src| {
            vec![token(TokenKind::KeywordWhen, pos_in(src, b"when", 0), 0)]
        });
    }

    #[test]
    fn test_lex_keyword_while_prefix() {
        assert_lex_except(
            "while",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| vec![token(TokenKind::KeywordWhile, pos_in(src, b"while", 0), 0)],
        );
    }

    #[test]
    fn test_lex_keyword_while_infix() {
        assert_lex_for(
            "while",
            &[
                LexerState::BeginOpt,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![token(
                    TokenKind::KeywordWhileInfix,
                    pos_in(src, b"while", 0),
                    0,
                )]
            },
        );
    }

    #[test]
    fn test_lex_keyword_yield() {
        assert_lex("yield", |src| {
            vec![token(TokenKind::KeywordYield, pos_in(src, b"yield", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ident_simple() {
        assert_lex("foo123", |src| {
            vec![token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ident_non_ascii() {
        assert_lex("あ", |src| {
            vec![token(
                TokenKind::Identifier,
                pos_in(src, b"\xE3\x81\x82", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_ident_invalid_non_ascii() {
        let src = EStrRef::from_bytes(b"\xE3\x81", Encoding::UTF_8);
        let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
        assert_eq!(
            diag,
            vec![Diagnostic {
                range: pos_in(src, b"\xE3\x81", 0),
                message: "The identifier contains invalid characters".to_owned(),
            }]
        );
    }

    #[test]
    fn test_lex_const_simple() {
        assert_lex("Foo123", |src| {
            vec![token(TokenKind::Const, pos_in(src, b"Foo123", 0), 0)]
        });
    }

    #[test]
    fn test_lex_const_non_ascii() {
        assert_lex("Ω", |src| {
            vec![token(TokenKind::Const, pos_in(src, b"\xCE\xA9", 0), 0)]
        });
    }

    #[test]
    fn test_lex_const_invalid_non_ascii() {
        let src = EStrRef::from_bytes(b"\xCE\xA9\xE3\x81", Encoding::UTF_8);
        let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
        assert_eq!(
            diag,
            vec![Diagnostic {
                range: pos_in(src, b"\xCE\xA9\xE3\x81", 0),
                message: "The identifier contains invalid characters".to_owned(),
            }]
        );
    }

    #[test]
    fn test_lex_ident_bang_simple() {
        assert_lex("foo123!", |src| {
            vec![token(TokenKind::MethodName, pos_in(src, b"foo123!", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ident_bang_capital() {
        assert_lex("Foo123!", |src| {
            vec![token(TokenKind::MethodName, pos_in(src, b"Foo123!", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ident_bang_eq() {
        assert_lex("foo123!=", |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                token(TokenKind::ExclEq, pos_in(src, b"!=", 0), 0),
            ]
        });
    }

    #[test]
    fn test_lex_ident_q_simple() {
        assert_lex("foo123?", |src| {
            vec![token(TokenKind::MethodName, pos_in(src, b"foo123?", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ident_q_capital() {
        assert_lex("Foo123?", |src| {
            vec![token(TokenKind::MethodName, pos_in(src, b"Foo123?", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ident_q_eq() {
        assert_lex("foo123?=", |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                token(TokenKind::Question, pos_in(src, b"?", 0), 0),
                token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
            ]
        });
    }

    #[test]
    fn test_lex_ident_eq_join() {
        assert_lex_for(
            "foo123=",
            &[LexerState::MethForDef, LexerState::MethOrSymbolForDef],
            |src| vec![token(TokenKind::MethodName, pos_in(src, b"foo123=", 0), 0)],
        );
    }

    #[test]
    fn test_lex_ident_eq_separate() {
        assert_lex_except(
            "foo123=",
            &[LexerState::MethForDef, LexerState::MethOrSymbolForDef],
            |src| {
                vec![
                    token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                    token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_lex_ident_eq_tilde() {
        assert_lex("foo123=~", |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                token(TokenKind::EqMatch, pos_in(src, b"=~", 0), 0),
            ]
        });
    }

    #[test]
    fn test_lex_ident_eq_gt() {
        assert_lex("foo123=>", |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
            ]
        });
    }

    #[test]
    fn test_lex_ident_eq_eq() {
        assert_lex("foo123==", |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                token(TokenKind::EqEq, pos_in(src, b"==", 0), 0),
            ]
        });
    }

    #[test]
    fn test_lex_ident_eq_eq_gt_join() {
        assert_lex_for(
            "foo123==>",
            &[LexerState::MethForDef, LexerState::MethOrSymbolForDef],
            |src| {
                vec![
                    token(TokenKind::MethodName, pos_in(src, b"foo123=", 0), 0),
                    token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_lex_ident_eq_eq_gt_separate() {
        assert_lex_except(
            "foo123==>",
            &[LexerState::MethForDef, LexerState::MethOrSymbolForDef],
            |src| {
                vec![
                    token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                    token(TokenKind::EqEq, pos_in(src, b"==", 0), 0),
                    token(TokenKind::Gt, pos_in(src, b">", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_lex_label_simple() {
        assert_lex_for(
            "foo123:",
            &[
                LexerState::BeginLabelable,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
            ],
            |src| vec![token(TokenKind::Label, pos_in(src, b"foo123:", 0), 0)],
        );
    }

    #[test]
    fn test_lex_label_cap() {
        assert_lex_for(
            "Bar:",
            &[
                LexerState::BeginLabelable,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
            ],
            |src| vec![token(TokenKind::Label, pos_in(src, b"Bar:", 0), 0)],
        );
    }

    #[test]
    fn test_lex_label_keyword_like() {
        assert_lex_for(
            "case:",
            &[
                LexerState::BeginLabelable,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
            ],
            |src| vec![token(TokenKind::Label, pos_in(src, b"case:", 0), 0)],
        );
    }

    #[test]
    fn test_lex_label_bang() {
        assert_lex_for(
            "foo!:",
            &[
                LexerState::BeginLabelable,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
            ],
            |src| vec![token(TokenKind::Label, pos_in(src, b"foo!:", 0), 0)],
        );
    }

    #[test]
    fn test_lex_label_question() {
        assert_lex_for(
            "foo?:",
            &[
                LexerState::BeginLabelable,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
            ],
            |src| vec![token(TokenKind::Label, pos_in(src, b"foo?:", 0), 0)],
        );
    }

    #[test]
    fn test_lex_static_symbol_ident_like() {
        assert_lex_except(
            ":foo123",
            &[
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0)],
        );
    }

    #[test]
    fn test_lex_static_symbol_const_like() {
        assert_lex_except(
            ":Baz",
            &[
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Symbol, pos_in(src, b":Baz", 0), 0)],
        );
    }

    #[test]
    fn test_lex_static_symbol_ident_bang() {
        assert_lex_for(":foo123!", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":foo123!", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_ident_question() {
        assert_lex_for(":foo123?", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":foo123?", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_ident_eq() {
        assert_lex_for(":foo123=", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":foo123=", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_unary_plus() {
        assert_lex_for(":+@", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":+@", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_unary_minus() {
        assert_lex_for(":-@", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":-@", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_neg_implicit() {
        assert_lex_for(":!", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":!", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_neg_explicit() {
        assert_lex_for(":!@", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":!@", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_bitwise_not_implicit() {
        assert_lex_for(":~", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":~", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_bitwise_not_explicit() {
        assert_lex_for(":~@", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":~@", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_power() {
        assert_lex_for(":**", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":**", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_mult() {
        assert_lex_for(":*", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":*", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_div() {
        assert_lex_for(":/", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":/", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_mod() {
        assert_lex_for(":%", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":%", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_plus() {
        assert_lex_for(":+", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":+", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_minus() {
        assert_lex_for(":-", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":-", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_lshift() {
        assert_lex_for(":<<", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":<<", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_rshift() {
        assert_lex_for(":>>", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":>>", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_bitwise_and() {
        assert_lex_for(":&", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":&", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_bitwise_or() {
        assert_lex_for(":|", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":|", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_bitwise_xor() {
        assert_lex_for(":^", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":^", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_lt() {
        assert_lex_for(":<", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":<", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_le() {
        assert_lex_for(":<=", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":<=", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_gt() {
        assert_lex_for(":>", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":>", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_ge() {
        assert_lex_for(":>=", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":>=", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_eq_cmp() {
        assert_lex_for(":==", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":==", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_neq() {
        assert_lex_for(":!=", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":!=", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_eqeqeq() {
        assert_lex_for(":===", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":===", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_match() {
        assert_lex_for(":=~", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":=~", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_nmatch() {
        assert_lex_for(":!~", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":!~", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_aref() {
        assert_lex_for(":[]", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":[]", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_aset() {
        assert_lex_for(":[]=", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":[]=", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_ivar() {
        assert_lex_for(":@foo", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":@foo", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_cvar() {
        assert_lex_for(":@@foo", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":@@foo", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_gvar() {
        assert_lex_for(":$foo", &[LexerState::Begin], |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":$foo", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ivar_name_simple() {
        assert_lex("@foo123", |src| {
            vec![token(TokenKind::IvarName, pos_in(src, b"@foo123", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ivar_name_cap() {
        assert_lex("@Baz", |src| {
            vec![token(TokenKind::IvarName, pos_in(src, b"@Baz", 0), 0)]
        });
    }

    #[test]
    fn test_lex_ivar_name_non_ascii() {
        assert_lex("@あ", |src| {
            vec![token(
                TokenKind::IvarName,
                pos_in(src, b"@\xE3\x81\x82", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_ivar_name_invalid_non_ascii() {
        let src = EStrRef::from_bytes(b"@\xE3\x81", Encoding::UTF_8);
        let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
        assert_eq!(
            diag,
            vec![Diagnostic {
                range: pos_in(src, b"@\xE3\x81", 0),
                message: "The instance variable name contains invalid characters".to_owned(),
            }]
        );
    }

    #[test]
    fn test_lex_ivar_name_invalid_digit() {
        let src = EStrRef::from("@123");
        let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
        assert_eq!(
            diag,
            vec![Diagnostic {
                range: pos_in(src, b"@123", 0),
                message: "Invalid instance variable name".to_owned(),
            }]
        );
    }

    #[test]
    fn test_lex_cvar_name_simple() {
        assert_lex("@@foo123", |src| {
            vec![token(TokenKind::CvarName, pos_in(src, b"@@foo123", 0), 0)]
        });
    }

    #[test]
    fn test_lex_cvar_name_cap() {
        assert_lex("@@Baz", |src| {
            vec![token(TokenKind::CvarName, pos_in(src, b"@@Baz", 0), 0)]
        });
    }

    #[test]
    fn test_lex_cvar_name_non_ascii() {
        assert_lex("@@あ", |src| {
            vec![token(
                TokenKind::CvarName,
                pos_in(src, b"@@\xE3\x81\x82", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_cvar_name_invalid_non_ascii() {
        let src = EStrRef::from_bytes(b"@@\xE3\x81", Encoding::UTF_8);
        let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
        assert_eq!(
            diag,
            vec![Diagnostic {
                range: pos_in(src, b"@@\xE3\x81", 0),
                message: "The class variable name contains invalid characters".to_owned(),
            }]
        );
    }

    #[test]
    fn test_lex_cvar_name_invalid_digit() {
        let src = EStrRef::from("@@123");
        let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
        assert_eq!(
            diag,
            vec![Diagnostic {
                range: pos_in(src, b"@@123", 0),
                message: "Invalid class variable name".to_owned(),
            }]
        );
    }

    #[test]
    fn test_lex_gvar_name_simple() {
        assert_lex("$foo123", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$foo123", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_name_cap() {
        assert_lex("$Baz", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$Baz", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_non_ascii() {
        assert_lex("$あ", |src| {
            vec![token(
                TokenKind::GvarName,
                pos_in(src, b"$\xE3\x81\x82", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_gvar_invalid_non_ascii() {
        let src = EStrRef::from_bytes(b"$\xE3\x81", Encoding::UTF_8);
        let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
        assert_eq!(
            diag,
            vec![Diagnostic {
                range: pos_in(src, b"$\xE3\x81", 0),
                message: "The global variable name contains invalid characters".to_owned(),
            }]
        );
    }

    #[test]
    fn test_lex_gvar_underscore() {
        assert_lex("$_", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$_", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_bang() {
        assert_lex("$!", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$!", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_dquote() {
        assert_lex("$\"", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$\"", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_dollar() {
        assert_lex("$$", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$$", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_amp() {
        assert_lex("$&", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$&", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_quote() {
        assert_lex("$'", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$'", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_star() {
        assert_lex("$*", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$*", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_plus() {
        assert_lex("$+", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$+", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_comma() {
        assert_lex("$,", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$,", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_dot() {
        assert_lex("$.", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$.", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_slash() {
        assert_lex("$/", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$/", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_colon() {
        assert_lex("$:", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$:", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_semicolon() {
        assert_lex("$;", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$;", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_lt() {
        assert_lex("$<", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$<", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_eq() {
        assert_lex("$=", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$=", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_gt() {
        assert_lex("$>", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$>", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_question() {
        assert_lex("$?", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$?", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_at() {
        assert_lex("$@", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$@", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_backslash() {
        assert_lex("$\\", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$\\", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_backtick() {
        assert_lex("$`", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$`", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_tilde() {
        assert_lex("$~", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$~", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_zero() {
        assert_lex("$0", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$0", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_num_simple() {
        assert_lex("$5", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$5", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_num_large() {
        assert_lex("$123", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$123", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_dashed_simple() {
        assert_lex("$-I", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$-I", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_dashed_digit() {
        assert_lex("$-9", |src| {
            vec![token(TokenKind::GvarName, pos_in(src, b"$-9", 0), 0)]
        });
    }

    #[test]
    fn test_lex_gvar_dashed_non_ascii() {
        assert_lex("$-あ", |src| {
            vec![token(
                TokenKind::GvarName,
                pos_in(src, b"$-\xE3\x81\x82", 0),
                0,
            )]
        });
    }

    #[test]
    fn test_lex_integer_simple() {
        assert_lex("123", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"123", 0), 0)]
        });
    }

    #[test]
    fn test_lex_integer_positive() {
        assert_lex_except(
            "+123",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Numeric, pos_in(src, b"+123", 0), 0)],
        );
    }

    #[test]
    fn test_lex_integer_underscore() {
        assert_lex("1_2_3", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1_2_3", 0), 0)]
        });
    }

    #[test]
    fn test_lex_integer_hex_small() {
        assert_lex("0xff", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0xff", 0), 0)]
        });
    }

    #[test]
    fn test_lex_integer_hex_capital() {
        assert_lex("0XFF", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0XFF", 0), 0)]
        });
    }

    #[test]
    fn test_lex_integer_explicit_dec_small() {
        assert_lex("0d0129", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0d0129", 0), 0)]
        });
    }

    #[test]
    fn test_lex_integer_explicit_dec_capital() {
        assert_lex("0D0129", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0D0129", 0), 0)]
        });
    }

    #[test]
    fn test_lex_integer_explicit_oct_small() {
        assert_lex("0o0127", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0o0127", 0), 0)]
        });
    }

    #[test]
    fn test_lex_integer_explicit_oct_capital() {
        assert_lex("0O0127", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0O0127", 0), 0)]
        });
    }

    #[test]
    fn test_lex_integer_oct() {
        assert_lex("0127", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0127", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_simple() {
        assert_lex("1.0", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1.0", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_positive() {
        assert_lex_except(
            "+1.0",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Numeric, pos_in(src, b"+1.0", 0), 0)],
        );
    }

    #[test]
    fn test_lex_float_zero() {
        assert_lex("0.5", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0.5", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_with_exponent_simple() {
        assert_lex("1e-3", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1e-3", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_with_exponent_capital() {
        assert_lex("1E-3", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1E-3", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_with_exponent_nosign() {
        assert_lex("1e3", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1e3", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_with_exponent_negative() {
        assert_lex("1e-3", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1e-3", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_with_exponent_positive() {
        assert_lex("1e+3", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1e+3", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_with_exponent_leading_zero() {
        assert_lex("1e+09", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1e+09", 0), 0)]
        });
    }

    #[test]
    fn test_lex_float_with_point_and_exponent() {
        assert_lex("1.53e-4", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1.53e-4", 0), 0)]
        });
    }

    #[test]
    fn test_lex_rational_simple() {
        assert_lex("3r", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"3r", 0), 0)]
        });
    }

    #[test]
    fn test_lex_rational_positive() {
        assert_lex_except(
            "+3r",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Numeric, pos_in(src, b"+3r", 0), 0)],
        );
    }

    #[test]
    fn test_lex_rational_float() {
        assert_lex("3.52r", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"3.52r", 0), 0)]
        });
    }

    #[test]
    fn test_lex_rational_with_base() {
        assert_lex("0xFFr", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"0xFFr", 0), 0)]
        });
    }

    #[test]
    fn test_lex_imaginary_integer_simple() {
        assert_lex("123i", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"123i", 0), 0)]
        });
    }

    #[test]
    fn test_lex_imaginary_integer_positive() {
        assert_lex_except(
            "+123i",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Numeric, pos_in(src, b"+123i", 0), 0)],
        );
    }

    #[test]
    fn test_lex_imaginary_float_simple() {
        assert_lex("1.5i", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1.5i", 0), 0)]
        });
    }

    #[test]
    fn test_lex_imaginary_rational_simple() {
        assert_lex("1.5ri", |src| {
            vec![token(TokenKind::Numeric, pos_in(src, b"1.5ri", 0), 0)]
        });
    }

    #[test]
    fn test_quote_string_tokens_simple_nolabelable() {
        assert_lex_except(
            "' foo '",
            &[
                LexerState::BeginLabelable,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
            ],
            |src| {
                vec![
                    token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
                    token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                    token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
                ]
            },
        );
    }

    #[test]
    fn test_quote_string_tokens_simple_labelable() {
        assert_lex_for(
            "' foo '",
            &[
                LexerState::BeginLabelable,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
            ],
            |src| {
                vec![
                    token(TokenKind::StringBeginLabelable, pos_in(src, b"'", 0), 0),
                    token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                    token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
                ]
            },
        );
    }

    #[test]
    fn test_quote_string_tokens_labelled() {
        assert_lex_for(
            "' foo ':",
            &[
                LexerState::BeginLabelable,
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
            ],
            |src| {
                vec![
                    token(TokenKind::StringBeginLabelable, pos_in(src, b"'", 0), 0),
                    token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                    token(TokenKind::StringEndColon, pos_in(src, b"':", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_quote_string_tokens_escaped() {
        assert_lex_for("'\\''", &[LexerState::Begin], |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
                token(TokenKind::StringContent, pos_in(src, "\\'", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"'", 2), 0),
            ]
        });
    }

    #[test]
    fn test_quote_string_tokens_backslashes() {
        assert_lex_for("'\\\\'", &[LexerState::Begin], |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
                token(TokenKind::StringContent, pos_in(src, "\\\\", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
            ]
        });
    }

    #[test]
    fn test_double_quote_string_tokens_simple() {
        assert_lex_for("\" foo \"", &[LexerState::Begin], |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"\"", 1), 0),
            ]
        });
    }

    #[test]
    fn test_double_quote_string_tokens_dynamic() {
        assert_lex_for("\" foo #{bar}", &[LexerState::Begin], |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringInterpolationBegin, pos_in(src, "#{", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"bar", 0), 0),
                token(TokenKind::RBrace, pos_in(src, b"}", 0), 0),
            ]
        });
    }

    #[test]
    fn test_backtick_string_tokens() {
        assert_lex_except(
            "` foo `",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::StringBegin, pos_in(src, b"`", 0), 0),
                    token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                    token(TokenKind::StringEnd, pos_in(src, b"`", 1), 0),
                ]
            },
        );
    }

    #[test]
    fn test_backtick_op_name() {
        assert_lex_for(
            "`",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::MethodName, pos_in(src, b"`", 0), 0)],
        );
    }

    #[test]
    fn test_regexp_string_tokens() {
        assert_lex_except(
            "/ foo /",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0),
                    token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                    token(TokenKind::StringEnd, pos_in(src, b"/", 1), 0),
                ]
            },
        );
    }

    #[test]
    fn test_op_assign_pow() {
        assert_lex("**=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"**=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_mult() {
        assert_lex("*=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"*=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_div_left_spaced() {
        assert_lex_for(
            " /=",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::OpAssign, pos_in(src, b"/=", 0), 1)],
        );
    }

    #[test]
    fn test_regexp_begin_op_assign_like_left_spaced() {
        assert_lex_except(
            " /=",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::StringBegin, pos_in(src, b"/", 0), 1),
                    token(TokenKind::StringContent, pos_in(src, b"=", 0), 1),
                ]
            },
        );
    }

    #[test]
    fn test_op_assign_div_nospaced() {
        assert_lex_for(
            "/=",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::OpAssign, pos_in(src, b"/=", 0), 0)],
        );
    }

    #[test]
    fn test_regexp_begin_op_assign_like_nospaced() {
        assert_lex_except(
            "/=",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0),
                    token(TokenKind::StringContent, pos_in(src, b"=", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_op_assign_mod() {
        assert_lex("%=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"%=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_add() {
        assert_lex("+=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"+=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_sub() {
        assert_lex("-=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"-=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_lshift() {
        assert_lex("<<=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"<<=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_rshift() {
        assert_lex(">>=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b">>=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_bitwise_and() {
        assert_lex("&=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"&=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_bitwise_or() {
        assert_lex("|=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"|=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_bitwise_xor() {
        assert_lex("^=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"^=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_logical_and() {
        assert_lex("&&=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"&&=", 0), 0)]
        });
    }

    #[test]
    fn test_op_assign_logical_or() {
        assert_lex("||=", |src| {
            vec![token(TokenKind::OpAssign, pos_in(src, b"||=", 0), 0)]
        });
    }

    #[test]
    fn test_excl() {
        assert_lex("!", |src| {
            vec![token(TokenKind::Excl, pos_in(src, b"!", 0), 0)]
        });
    }

    #[test]
    fn test_excl_at_join() {
        assert_lex_for(
            "!@foo",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::MethodName, pos_in(src, b"!@", 0), 0),
                    token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_excl_at_separate() {
        assert_lex_except(
            "!@foo",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::Excl, pos_in(src, b"!", 0), 0),
                    token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_excl_eq() {
        assert_lex("!=", |src| {
            vec![token(TokenKind::ExclEq, pos_in(src, b"!=", 0), 0)]
        });
    }

    #[test]
    fn test_excl_tilde() {
        assert_lex("!~", |src| {
            vec![token(TokenKind::ExclTilde, pos_in(src, b"!~", 0), 0)]
        });
    }

    #[test]
    fn test_percent() {
        assert_lex("%", |src| {
            vec![token(TokenKind::Percent, pos_in(src, b"%", 0), 0)]
        });
    }

    #[test]
    fn test_amp_infix_spaced() {
        assert_lex_for(
            " & ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Amp, pos_in(src, b"&", 0), 1)],
        );
    }

    #[test]
    fn test_amp_prefix_spaced() {
        assert_lex_except(
            " & ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 1)],
        );
    }

    #[test]
    fn test_amp_infix_left_spaced() {
        assert_lex_for(
            " &",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Amp, pos_in(src, b"&", 0), 1)],
        );
    }

    #[test]
    fn test_amp_prefix_left_spaced() {
        assert_lex_except(
            " &",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 1)],
        );
    }

    #[test]
    fn test_amp_infix_nospaced() {
        assert_lex_for(
            "&",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Amp, pos_in(src, b"&", 0), 0)],
        );
    }

    #[test]
    fn test_amp_prefix_nospaced() {
        assert_lex_except(
            "&",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 0)],
        );
    }

    #[test]
    fn test_amp_amp() {
        assert_lex("&&", |src| {
            vec![token(TokenKind::AmpAmp, pos_in(src, b"&&", 0), 0)]
        });
    }

    #[test]
    fn test_amp_dot() {
        assert_lex("&.", |src| {
            vec![token(TokenKind::AmpDot, pos_in(src, b"&.", 0), 0)]
        });
    }

    #[test]
    fn test_lparen_spaced() {
        assert_lex_except(
            " ( ",
            &[LexerState::FirstArgument, LexerState::WeakFirstArgument],
            |src| vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 1)],
        );
    }

    #[test]
    fn test_lparen_noarg_spaced() {
        assert_lex_for(
            " ( ",
            &[LexerState::FirstArgument, LexerState::WeakFirstArgument],
            |src| vec![token(TokenKind::LParenRestricted, pos_in(src, b"(", 0), 1)],
        );
    }

    #[test]
    fn test_lparen_left_spaced() {
        assert_lex_except(
            " (",
            &[LexerState::FirstArgument, LexerState::WeakFirstArgument],
            |src| vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 1)],
        );
    }

    #[test]
    fn test_lparen_noarg_left_spaced() {
        assert_lex_for(
            " (",
            &[LexerState::FirstArgument, LexerState::WeakFirstArgument],
            |src| vec![token(TokenKind::LParenRestricted, pos_in(src, b"(", 0), 1)],
        );
    }

    #[test]
    fn test_lparen_nospaced() {
        assert_lex("(", |src| {
            vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 0)]
        });
    }

    #[test]
    fn test_rparen() {
        assert_lex(")", |src| {
            vec![token(TokenKind::RParen, pos_in(src, b")", 0), 0)]
        });
    }

    #[test]
    fn test_star_infix_spaced() {
        assert_lex_for(
            " * ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Star, pos_in(src, b"*", 0), 1)],
        );
    }

    #[test]
    fn test_star_prefix_spaced() {
        assert_lex_except(
            " * ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 1)],
        );
    }

    #[test]
    fn test_star_infix_left_spaced() {
        assert_lex_for(
            " *",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Star, pos_in(src, b"*", 0), 1)],
        );
    }

    #[test]
    fn test_star_prefix_left_spaced() {
        assert_lex_except(
            " *",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 1)],
        );
    }

    #[test]
    fn test_star_infix_nospaced() {
        assert_lex_for(
            "*",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Star, pos_in(src, b"*", 0), 0)],
        );
    }

    #[test]
    fn test_star_prefix_nospaced() {
        assert_lex_except(
            "*",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 0)],
        );
    }

    #[test]
    fn test_star_star_infix_spaced() {
        assert_lex_for(
            " ** ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarStar, pos_in(src, b"**", 0), 1)],
        );
    }

    #[test]
    fn test_star_star_prefix_spaced() {
        assert_lex_except(
            " ** ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 1)],
        );
    }

    #[test]
    fn test_star_star_infix_left_spaced() {
        assert_lex_for(
            " **",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarStar, pos_in(src, b"**", 0), 1)],
        );
    }

    #[test]
    fn test_star_star_prefix_left_spaced() {
        assert_lex_except(
            " **",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 1)],
        );
    }

    #[test]
    fn test_star_star_infix_nospaced() {
        assert_lex_for(
            "**",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarStar, pos_in(src, b"**", 0), 0)],
        );
    }

    #[test]
    fn test_star_star_prefix_nospaced() {
        assert_lex_except(
            "**",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 0)],
        );
    }

    #[test]
    fn test_plus_infix_spaced() {
        assert_lex_for(
            " + ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Plus, pos_in(src, b"+", 0), 1)],
        );
    }

    #[test]
    fn test_plus_prefix_spaced() {
        assert_lex_except(
            " + ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::PlusPrefix, pos_in(src, b"+", 0), 1)],
        );
    }

    #[test]
    fn test_plus_infix_left_spaced() {
        assert_lex_for(
            " +",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Plus, pos_in(src, b"+", 0), 1)],
        );
    }

    #[test]
    fn test_plus_prefix_left_spaced() {
        assert_lex_except(
            " +",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::PlusPrefix, pos_in(src, b"+", 0), 1)],
        );
    }

    #[test]
    fn test_plus_infix_nospaced() {
        assert_lex_for(
            "+",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Plus, pos_in(src, b"+", 0), 0)],
        );
    }

    #[test]
    fn test_plus_prefix_nospaced() {
        assert_lex_except(
            "+",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::PlusPrefix, pos_in(src, b"+", 0), 0)],
        );
    }

    #[test]
    fn test_plus_at_join() {
        assert_lex_for(
            "+@foo",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::MethodName, pos_in(src, b"+@", 0), 0),
                    token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_plus_at_separate_infix() {
        assert_lex_for(
            "+@foo",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![
                    token(TokenKind::Plus, pos_in(src, b"+", 0), 0),
                    token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_plus_at_separate_prefix() {
        assert_lex_except(
            "+@foo",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::PlusPrefix, pos_in(src, b"+", 0), 0),
                    token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_comma() {
        assert_lex(",", |src| {
            vec![token(TokenKind::Comma, pos_in(src, b",", 0), 0)]
        });
    }

    #[test]
    fn test_minus_infix_spaced() {
        assert_lex_for(
            " - ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Minus, pos_in(src, b"-", 0), 1)],
        );
    }

    #[test]
    fn test_minus_prefix_spaced() {
        assert_lex_except(
            " - ",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::MinusPrefix, pos_in(src, b"-", 0), 1)],
        );
    }

    #[test]
    fn test_minus_infix_left_spaced() {
        assert_lex_for(
            " -",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Minus, pos_in(src, b"-", 0), 1)],
        );
    }

    #[test]
    fn test_minus_prefix_left_spaced() {
        assert_lex_except(
            " -",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::MinusPrefix, pos_in(src, b"-", 0), 1)],
        );
    }

    #[test]
    fn test_minus_infix_nospaced() {
        assert_lex_for(
            "-",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Minus, pos_in(src, b"-", 0), 0)],
        );
    }

    #[test]
    fn test_minus_prefix_nospaced() {
        assert_lex_except(
            "-",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::MinusPrefix, pos_in(src, b"-", 0), 0)],
        );
    }

    #[test]
    fn test_minus_at_join() {
        assert_lex_for(
            "-@foo",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::MethodName, pos_in(src, b"-@", 0), 0),
                    token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_minus_at_separate_infix() {
        assert_lex_for(
            "-@foo",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![
                    token(TokenKind::Minus, pos_in(src, b"-", 0), 0),
                    token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_minus_at_separate_prefix() {
        assert_lex_except(
            "-@foo",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::MinusPrefix, pos_in(src, b"-", 0), 0),
                    token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_arrow() {
        assert_lex("->", |src| {
            vec![token(TokenKind::Arrow, pos_in(src, b"->", 0), 0)]
        });
    }

    #[test]
    fn test_dot() {
        assert_lex(".", |src| {
            vec![token(TokenKind::Dot, pos_in(src, b".", 0), 0)]
        });
    }

    #[test]
    fn test_dot_dot() {
        assert_lex("..", |src| {
            vec![token(TokenKind::DotDot, pos_in(src, b"..", 0), 0)]
        });
    }

    #[test]
    fn test_dot_dot_dot() {
        assert_lex("...", |src| {
            vec![token(TokenKind::DotDotDot, pos_in(src, b"...", 0), 0)]
        });
    }

    #[test]
    fn test_slash_spaced() {
        assert_lex_for(
            " / ",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Slash, pos_in(src, b"/", 0), 1)],
        );
    }

    #[test]
    fn test_regexp_begin_spaced() {
        assert_lex_except(
            " / ",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::StringBegin, pos_in(src, b"/", 0), 1),
                    token(TokenKind::StringContent, pos_in(src, " ", 1), 1),
                ]
            },
        );
    }

    #[test]
    fn test_slash_left_spaced() {
        assert_lex_for(
            " /",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Slash, pos_in(src, b"/", 0), 1)],
        );
    }

    #[test]
    fn test_regexp_begin_left_spaced() {
        assert_lex_except(
            " /",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StringBegin, pos_in(src, b"/", 0), 1)],
        );
    }

    #[test]
    fn test_slash_nospaced() {
        assert_lex_for(
            "/",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::Slash, pos_in(src, b"/", 0), 0)],
        );
    }

    #[test]
    fn test_regexp_begin_nospaced() {
        assert_lex_except(
            "/",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0)],
        );
    }

    #[test]
    fn test_colon() {
        assert_lex(":", |src| {
            vec![token(TokenKind::Colon, pos_in(src, b":", 0), 0)]
        });
    }

    #[test]
    fn test_colon_colon_infix_spaced() {
        assert_lex_for(
            " :: ",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 1)],
        );
    }

    #[test]
    fn test_colon_colon_prefix_spaced() {
        assert_lex_except(
            " :: ",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 1)],
        );
    }

    #[test]
    fn test_colon_colon_infix_left_spaced() {
        assert_lex_for(
            " ::",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 1)],
        );
    }

    #[test]
    fn test_colon_colon_prefix_left_spaced() {
        assert_lex_except(
            " ::",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 1)],
        );
    }

    #[test]
    fn test_colon_colon_infix_nospaced() {
        assert_lex_for(
            "::",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 0)],
        );
    }

    #[test]
    fn test_colon_colon_prefix_nospaced() {
        assert_lex_except(
            "::",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 0)],
        );
    }

    #[test]
    fn test_lex_semicolon() {
        assert_lex(";", |src| {
            vec![token(TokenKind::Semicolon, pos_in(src, b";", 0), 0)]
        });
    }

    #[test]
    fn test_lt() {
        assert_lex("<", |src| {
            vec![token(TokenKind::Lt, pos_in(src, b"<", 0), 0)]
        });
    }

    #[test]
    fn test_lt_lt() {
        assert_lex("<<", |src| {
            vec![token(TokenKind::LtLt, pos_in(src, b"<<", 0), 0)]
        });
    }

    #[test]
    fn test_lt_eq() {
        assert_lex("<=", |src| {
            vec![token(TokenKind::LtEq, pos_in(src, b"<=", 0), 0)]
        });
    }

    #[test]
    fn test_lt_eq_gt() {
        assert_lex("<=>", |src| {
            vec![token(TokenKind::LtEqGt, pos_in(src, b"<=>", 0), 0)]
        });
    }

    #[test]
    fn test_eq() {
        assert_lex("=", |src| {
            vec![token(TokenKind::Eq, pos_in(src, b"=", 0), 0)]
        });
    }

    #[test]
    fn test_eq_eq() {
        assert_lex("==", |src| {
            vec![token(TokenKind::EqEq, pos_in(src, b"==", 0), 0)]
        });
    }

    #[test]
    fn test_eq_eq_eq() {
        assert_lex("===", |src| {
            vec![token(TokenKind::EqEqEq, pos_in(src, b"===", 0), 0)]
        });
    }

    #[test]
    fn test_fat_arrow() {
        assert_lex("=>", |src| {
            vec![token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0)]
        });
    }

    #[test]
    fn test_eq_match() {
        assert_lex("=~", |src| {
            vec![token(TokenKind::EqMatch, pos_in(src, b"=~", 0), 0)]
        });
    }

    #[test]
    fn test_gt() {
        assert_lex(">", |src| {
            vec![token(TokenKind::Gt, pos_in(src, b">", 0), 0)]
        });
    }

    #[test]
    fn test_gt_eq() {
        assert_lex(">=", |src| {
            vec![token(TokenKind::GtEq, pos_in(src, b">=", 0), 0)]
        });
    }

    #[test]
    fn test_gt_gt() {
        assert_lex(">>", |src| {
            vec![token(TokenKind::GtGt, pos_in(src, b">>", 0), 0)]
        });
    }

    #[test]
    fn test_question_simple() {
        assert_lex("?", |src| {
            vec![token(TokenKind::Question, pos_in(src, b"?", 0), 0)]
        });
    }

    #[test]
    fn test_question_separate() {
        assert_lex_for(
            "?a",
            &[LexerState::WeakFirstArgument, LexerState::End],
            |src| {
                vec![
                    token(TokenKind::Question, pos_in(src, b"?", 0), 0),
                    token(TokenKind::Identifier, pos_in(src, b"a", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_char_literal() {
        assert_lex_except(
            "?a",
            &[LexerState::WeakFirstArgument, LexerState::End],
            |src| vec![token(TokenKind::CharLiteral, pos_in(src, b"?a", 0), 0)],
        );
    }

    #[test]
    fn test_at() {
        assert_lex("@", |src| {
            vec![token(TokenKind::At, pos_in(src, b"@", 0), 0)]
        });
    }

    #[test]
    fn test_lbracket_infix_spaced() {
        assert_lex_for(
            " [ ",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 1)],
        );
    }

    #[test]
    fn test_lbracket_prefix_spaced() {
        assert_lex_except(
            " [ ",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 1)],
        );
    }

    #[test]
    fn test_lbracket_infix_left_spaced() {
        assert_lex_for(
            " [",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 1)],
        );
    }

    #[test]
    fn test_lbracket_prefix_left_spaced() {
        assert_lex_except(
            " [",
            &[
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 1)],
        );
    }

    #[test]
    fn test_lbracket_infix_nospaced() {
        assert_lex_for(
            "[",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 0)],
        );
    }

    #[test]
    fn test_lbracket_prefix_nospaced() {
        assert_lex_except(
            "[",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0)],
        );
    }

    #[test]
    fn test_aref_join() {
        assert_lex_for(
            "[]",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::MethodName, pos_in(src, b"[]", 0), 0)],
        );
    }

    #[test]
    fn test_aref_separate_infix() {
        assert_lex_for(
            "[]",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![
                    token(TokenKind::LBracket, pos_in(src, b"[", 0), 0),
                    token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_aref_separate_prefix() {
        assert_lex_except(
            "[]",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0),
                    token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_aset_join() {
        assert_lex_for(
            "[]=",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::MethodName, pos_in(src, b"[]=", 0), 0)],
        );
    }

    #[test]
    fn test_aset_separate_infix() {
        assert_lex_for(
            "[]=",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
            ],
            |src| {
                vec![
                    token(TokenKind::LBracket, pos_in(src, b"[", 0), 0),
                    token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
                    token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_aset_separate_prefix() {
        assert_lex_except(
            "[]=",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0),
                    token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
                    token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_rbracket() {
        assert_lex("]", |src| {
            vec![token(TokenKind::RBracket, pos_in(src, b"]", 0), 0)]
        });
    }

    #[test]
    fn test_caret() {
        assert_lex("^", |src| {
            vec![token(TokenKind::Caret, pos_in(src, b"^", 0), 0)]
        });
    }

    #[test]
    fn test_lbrace() {
        assert_lex("{", |src| {
            vec![token(TokenKind::LBrace, pos_in(src, b"{", 0), 0)]
        });
    }

    #[test]
    fn test_vert() {
        assert_lex("|", |src| {
            vec![token(TokenKind::Vert, pos_in(src, b"|", 0), 0)]
        });
    }

    #[test]
    fn test_vert_vert() {
        assert_lex_for(
            "||",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| vec![token(TokenKind::VertVert, pos_in(src, b"||", 0), 0)],
        );
    }

    #[test]
    fn test_split_vert_vert() {
        assert_lex_except(
            "||",
            &[
                LexerState::FirstArgument,
                LexerState::WeakFirstArgument,
                LexerState::End,
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::Vert, pos_in(src, b"|", 0), 0),
                    token(TokenKind::Vert, pos_in(src, b"|", 1), 0),
                ]
            },
        );
    }

    #[test]
    fn test_rbrace() {
        assert_lex("}", |src| {
            vec![token(TokenKind::RBrace, pos_in(src, b"}", 0), 0)]
        });
    }

    #[test]
    fn test_tilde() {
        assert_lex("~", |src| {
            vec![token(TokenKind::Tilde, pos_in(src, b"~", 0), 0)]
        });
    }

    #[test]
    fn test_tilde_at_join() {
        assert_lex_for(
            "~@foo",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::MethodName, pos_in(src, b"~@", 0), 0),
                    token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_tilde_at_separate() {
        assert_lex_except(
            "~@foo",
            &[
                LexerState::MethForDef,
                LexerState::MethOrSymbolForDef,
                LexerState::MethForCall,
            ],
            |src| {
                vec![
                    token(TokenKind::Tilde, pos_in(src, b"~", 0), 0),
                    token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
                ]
            },
        );
    }

    #[test]
    fn test_interpret_numeric_integer_simple() {
        assert_eq!(
            interpret_numeric(b"123"),
            (NumericValue::Integer(123), false)
        );
    }

    #[test]
    fn test_interpret_numeric_integer_positive() {
        assert_eq!(
            interpret_numeric(b"+123"),
            (NumericValue::Integer(123), false)
        );
    }

    #[test]
    fn test_interpret_numeric_integer_negative() {
        assert_eq!(
            interpret_numeric(b"-123"),
            (NumericValue::Integer(-123), false)
        );
    }

    #[test]
    fn test_interpret_numeric_integer_underscore() {
        assert_eq!(
            interpret_numeric(b"1_2_3"),
            (NumericValue::Integer(123), false)
        );
    }

    #[test]
    fn test_interpret_numeric_float_with_point() {
        assert_eq!(
            interpret_numeric(b"123.75"),
            (NumericValue::Float(NotNan::new(123.75).unwrap()), false)
        );
    }

    #[test]
    fn test_interpret_numeric_float_with_exponent() {
        assert_eq!(
            interpret_numeric(b"12375e-2"),
            (NumericValue::Float(NotNan::new(123.75).unwrap()), false)
        );
    }

    #[test]
    fn test_interpret_numeric_float_negative() {
        assert_eq!(
            interpret_numeric(b"-123.75"),
            (NumericValue::Float(NotNan::new(-123.75).unwrap()), false)
        );
    }

    #[test]
    fn test_interpret_numeric_rational_with_point() {
        assert_eq!(
            interpret_numeric(b"123.75r"),
            (NumericValue::Rational(12375, 100), false)
        );
    }

    #[test]
    fn test_interpret_numeric_rational_negative() {
        assert_eq!(
            interpret_numeric(b"-123.75r"),
            (NumericValue::Rational(-12375, 100), false)
        );
    }
}
