use std::{collections::HashMap, sync::LazyLock};

use crate::{
    ast::CodeRange,
    encoding::{EStrRef, EncodingState},
    Diagnostic,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Token {
    pub(super) kind: TokenKind,
    pub(super) range: CodeRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TokenKind {
    /// `__ENCODING__`
    KeywordCapitalDoubleUnderscoreEncoding,
    /// `__LINE__`
    KeywordCapitalDoubleUnderscoreLine,
    /// `__FILE__`
    KeywordCapitalDoubleUnderscoreFile,
    /// `BEGIN`
    KeywordCapitalBegin,
    /// `END`
    KeywordCapitalEnd,
    /// `alias`
    KeywordAlias,
    /// `and`
    KeywordAnd,
    /// `begin`
    KeywordBegin,
    /// `break`
    KeywordBreak,
    /// `case`
    KeywordCase,
    /// `class`
    KeywordClass,
    /// `def`
    KeywordDef,
    /// `defined?`
    KeywordDefinedQ,
    /// `do`
    KeywordDo,
    /// `else`
    KeywordElse,
    /// `elsif`
    KeywordElsif,
    /// `end`
    KeywordEnd,
    /// `ensure`
    KeywordEnsure,
    /// `false`
    KeywordFalse,
    /// `for`
    KeywordFor,
    /// `if`
    KeywordIf,
    /// `if` postfix
    KeywordIfInfix,
    /// `in`
    KeywordIn,
    /// `module`
    KeywordModule,
    /// `next`
    KeywordNext,
    /// `nil`
    KeywordNil,
    /// `not`
    KeywordNot,
    /// `or`
    KeywordOr,
    /// `redo`
    KeywordRedo,
    /// `rescue`
    KeywordRescue,
    /// `retry`
    KeywordRetry,
    /// `return`
    KeywordReturn,
    /// `self`
    KeywordSelf,
    /// `super`
    KeywordSuper,
    /// `then`
    KeywordThen,
    /// `true`
    KeywordTrue,
    /// `undef`
    KeywordUndef,
    /// `unless`
    KeywordUnless,
    /// `unless` postfix
    KeywordUnlessInfix,
    /// `until`
    KeywordUntil,
    /// `until` postfix
    KeywordUntilInfix,
    /// `when`
    KeywordWhen,
    /// `while`
    KeywordWhile,
    /// `while` postfix
    KeywordWhileInfix,
    /// `yield`
    KeywordYield,

    /// `foo` etc.
    Identifier,
    /// `Foo` etc.
    Const,
    /// `foo!` etc.
    MethodName,
    /// `foo:` etc.
    Label,
    /// `:foo` etc.
    Symbol,
    /// `@foo` etc.
    IvarName,
    /// `@@foo` etc.
    CvarName,
    /// `$foo` etc.
    GvarName,

    // TODO: merge Integer/Float/Rational/Imaginary into Numeric
    /// `123` etc.
    Integer,
    /// `123.0` etc.
    Float,
    /// `123r` etc.
    Rational,
    /// `123i` etc.
    Imaginary,
    /// `?a` etc.
    CharLiteral,

    /// `"`, `'`, `:"`, `/` etc. in expr context.
    StringBegin,
    /// `"` etc. in String-like context.
    StringEnd,
    /// `":` etc. in String-like context.
    StringEndColon,
    /// `foo` as in `"foo"`
    StringContent,
    /// `#{`
    StringInterpolationBegin,
    /// `#@foo` etc.
    StringVarInterpolation,

    /// `+=` etc.
    OpAssign,

    /// `!`
    Excl,
    /// `!=`
    ExclEq,
    /// `!~`
    ExclTilde,
    /// `%`
    Percent,
    /// `&`
    Amp,
    /// `&`, block argument only
    AmpPrefix,
    /// `&&`
    AmpAmp,
    /// `&.`
    AmpDot,
    /// `(`
    LParen,
    /// `(` with restricted syntactic rule (e.g. cannot be an argument list delimiter)
    LParenRestricted,
    /// `)`
    RParen,
    /// `*`
    Star,
    /// `*`, argument splat only
    StarPrefix,
    /// `**`
    StarStar,
    /// `**`, keyword argument splat only
    StarStarPrefix,
    /// `+`
    Plus,
    /// `+`, unary operator only
    PlusPrefix,
    /// `,`
    Comma,
    /// `-`
    Minus,
    /// `-`, unary operator only
    MinusPrefix,
    /// `->`
    Arrow,
    /// `.`
    Dot,
    /// `..`
    DotDot,
    /// `...`
    DotDotDot,
    /// `/`
    Slash,
    /// `:`
    Colon,
    /// `::`
    ColonColon,
    /// `::`, prefix only
    ColonColonPrefix,
    /// `;`, but in most cases Newline is used instead.
    Semicolon,
    /// EOL in certain contexts.
    Newline,
    /// `<`
    Lt,
    /// `<<`
    LtLt,
    /// `<=`
    LtEq,
    /// `<=>`
    LtEqGt,
    /// `=`
    Eq,
    /// `==`
    EqEq,
    /// `===`
    EqEqEq,
    /// `=>`
    FatArrow,
    /// `=~`
    EqMatch,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
    /// `>>`
    GtGt,
    /// `?`
    Question,
    /// `@`
    At,
    /// `[`
    LBracket,
    /// `[`, prefix only
    LBracketPrefix,
    /// `]`
    RBracket,
    /// `^`
    Caret,
    /// `{`
    LBrace,
    /// `|`
    Vert,
    /// `||`
    VertVert,
    /// `}`
    RBrace,
    /// `~`
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
    /// Effects are equivalent to [LexerState::BeginLabelable] except:
    ///
    /// - `\n` is a real token like in [LexerState::End].
    /// - For `..` and `...` token types, it is treated like [LexerState::End].
    /// - if the next token is `{`, it is treated as a block start.
    /// - For prefix `(` token, it becomes a special token that restricts
    ///   what can be inside.
    /// - For ambiguous tokens to be treated as prefix operators, if imposes
    ///   additional requirements which did not exist in [LexerState::BeginLabelable]:
    ///   - Should be preceded by whitespace and not followed by whitespace:
    ///     - `+`, `-`
    ///     - `*`, `**`, `&`
    ///   - Should be preceded by whitespace and not followed by `=`:
    ///     - `/`, `%`
    ///   - Should be preceded by whitespace:
    ///     - `::`
    ///     - `(`, `[`
    ///     - `<<` as part of heredoc
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
    /// Special state for string contents.
    StringLike(StringDelimiter),
}

impl LexerState {
    /// Generic IS_BEG condition
    // TODO: refactor it into several semantic methods
    fn begin_strict(&self) -> bool {
        match self {
            LexerState::Begin
            | LexerState::ClassName
            | LexerState::BeginOpt
            | LexerState::BeginLabelable
            | LexerState::FirstArgument => true,
            _ => false,
        }
    }

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
            LexerState::MethForDef => false,
            LexerState::MethOrSymbolForDef => false,
            LexerState::MethForCall => true,
            LexerState::StringLike(_) => unreachable!(),
        }
    }

    /// Should we convert `if`, `unless`, `while`, `until`
    /// and `rescue` to infix operators?
    fn prefer_modifier_if(&self) -> bool {
        match self {
            LexerState::End | LexerState::WeakFirstArgument | LexerState::BeginOpt => true,
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
            LexerState::StringLike(_) => unreachable!(),
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
}

impl<'a> Lexer<'a> {
    pub(super) fn new(input: EStrRef<'a>) -> Self {
        Self { input, pos: 0 }
    }

    // pub(super) fn input(&self) -> EStrRef<'a> {
    //     self.input
    // }

    pub(super) fn bytes(&self) -> &'a [u8] {
        self.input.bytes()
    }

    pub(super) fn pos(&self) -> usize {
        self.pos
    }

    pub(super) fn lex(&mut self, diag: &mut Vec<Diagnostic>, state: LexerState) -> Token {
        if let LexerState::StringLike(delim) = state {
            return self.lex_string_like(diag, delim);
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
                if self.pos >= 1 && self.bytes()[self.pos - 1] == b'\r' {
                    // Include the preceding CR to form CRLF
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Newline,
                        range: CodeRange {
                            start: self.pos - 1,
                            end: self.pos,
                        },
                    };
                }
                self.pos += 1;
                TokenKind::Newline
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'.. => {
                while is_ident_continue(self.peek_byte()) {
                    self.pos += 1;
                }
                let suffixed = match self.peek_byte() {
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
                let s_bytes = &self.bytes()[start..self.pos];
                let s = EStrRef::from_bytes(s_bytes, self.input.encoding());
                if let Some(kwd) = KEYWORDS.get(s_bytes).copied() {
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
                        _ => kwd,
                    }
                } else if suffixed {
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
            b'0'..=b'9' => {
                while self.peek_byte().is_ascii_digit() {
                    self.pos += 1;
                }
                TokenKind::Integer
            }
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
                TokenKind::StringBegin
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
                    _ => TokenKind::Unknown,
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
                TokenKind::StringBegin
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
                            TokenKind::PlusPrefix
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
                    b'0'..=b'9'
                        if state.begin_strict() && self.peek_byte_at(1).is_ascii_digit() =>
                    {
                        while self.peek_byte().is_ascii_digit() {
                            self.pos += 1;
                        }
                        TokenKind::Integer
                    }
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
                        while self.peek_byte().is_ascii_digit() {
                            self.pos += 1;
                        }
                        diag.push(Diagnostic {
                            range: CodeRange {
                                start,
                                end: self.pos,
                            },
                            message: format!("Invalid float literal"),
                        });
                        TokenKind::Float
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
                self.pos += 1;
                match self.peek_byte() {
                    b if is_ident_continue(b) => {
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
                        TokenKind::Symbol
                    }
                    b'"' => {
                        self.pos += 1;
                        TokenKind::StringBegin
                    }
                    b'\'' => {
                        self.pos += 1;
                        TokenKind::StringBegin
                    }
                    b':' => {
                        self.pos += 1;
                        // Pass `false` so that `p :: Foo` is equivalent to `p ::Foo`
                        if state.prefer_prefix_operator(space_before, false) {
                            TokenKind::ColonColonPrefix
                        } else {
                            TokenKind::ColonColon
                        }
                    }
                    _ => TokenKind::Colon,
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
                if !state.begin_strict() {
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
                            _ if state.begin_strict() => {
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
        Token {
            kind,
            range: CodeRange { start, end },
        }
    }

    fn lex_string_like(&mut self, _diag: &mut Vec<Diagnostic>, delim: StringDelimiter) -> Token {
        if self.pos >= self.bytes().len() {
            return Token {
                kind: TokenKind::EOF,
                range: CodeRange {
                    start: self.pos,
                    end: self.pos,
                },
            };
        }

        let start = self.pos;
        match self.peek_byte() {
            b'\'' if delim == StringDelimiter::Quote => {
                self.pos += 1;
                Token {
                    kind: TokenKind::StringEnd,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                }
            }
            b'"' if delim == StringDelimiter::DoubleQuote => {
                self.pos += 1;
                Token {
                    kind: TokenKind::StringEnd,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                }
            }
            b'`' if delim == StringDelimiter::Backtick => {
                self.pos += 1;
                Token {
                    kind: TokenKind::StringEnd,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                }
            }
            b'/' if delim == StringDelimiter::Slash => {
                self.pos += 1;
                Token {
                    kind: TokenKind::StringEnd,
                    range: CodeRange {
                        start,
                        end: self.pos,
                    },
                }
            }
            b'#' if delim.allow_interpolation() && self.lookahead_interpolation() => {
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
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                while self.pos < self.bytes().len() {
                    match self.peek_byte() {
                        b'\'' if delim == StringDelimiter::Quote => break,
                        b'"' if delim == StringDelimiter::DoubleQuote => break,
                        b'`' if delim == StringDelimiter::Backtick => break,
                        b'/' if delim == StringDelimiter::Slash => break,
                        b'#' if delim.allow_interpolation() && self.lookahead_interpolation() => {
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
                }
            }
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
                b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b'\x13' | b' ' => {
                    self.pos += 1;
                }
                b'\\' => {
                    if self.peek_byte_at(1) == b'\n' {
                        self.pos += 2;
                    } else if self.peek_byte_at(1) == b'\r' && self.peek_byte_at(2) == b'\n' {
                        self.pos += 3;
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
        let rollback = self.pos;
        // Skip the current LF byte
        self.pos += 1;

        loop {
            match self.peek_byte() {
                b'\t' | b'\x0B' | b'\x0C' | b'\r' | b'\x13' | b' ' => {
                    self.pos += 1;
                }
                b'#' => {
                    self.skip_line();
                    if self.peek_byte() == b'\n' {
                        self.pos += 1;
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
            self.pos = rollback;
        }
        force_fold
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

#[cfg(test)]
mod tests {
    use crate::ast::{pos_in, pos_in_at};

    use super::*;

    fn lex_all(input: EStrRef<'_>) -> Vec<Token> {
        let mut diag = Vec::<Diagnostic>::new();
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        let mut state = LexerState::default();
        loop {
            let token = lexer.lex(&mut diag, state);
            if token.kind == TokenKind::EOF {
                break;
            }
            state = next_state_for_testing(&token, input.bytes(), state);
            tokens.push(token);
        }
        tokens
    }

    fn next_state_for_testing(tok: &Token, bytes: &[u8], prev: LexerState) -> LexerState {
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
            TokenKind::Integer => LexerState::End,
            TokenKind::Float => LexerState::End,
            TokenKind::Rational => LexerState::End,
            TokenKind::Imaginary => LexerState::End,
            TokenKind::CharLiteral => LexerState::End,
            TokenKind::StringBegin => LexerState::StringLike(match bytes[tok.range.start] {
                b'\'' => StringDelimiter::Quote,
                b'"' => StringDelimiter::DoubleQuote,
                b'`' => StringDelimiter::Backtick,
                b'/' => StringDelimiter::Slash,
                _ => unreachable!(),
            }),
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

    fn token(kind: TokenKind, range: CodeRange) -> Token {
        Token { kind, range }
    }

    #[test]
    fn test_lex_eof() {
        let src = EStrRef::from("foo \0 bar");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
        let src = EStrRef::from("foo \x04 bar");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
        let src = EStrRef::from("foo \x1A bar");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
        let src = EStrRef::from("foo \n__END__\n bar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
            ]
        );
        let src = EStrRef::from("foo \n__END__\r\n bar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
            ]
        );
        let src = EStrRef::from("foo \n__END__");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
            ]
        );
    }

    #[test]
    fn test_lex_non_eof() {
        let src = EStrRef::from("foo \n__END__ \n bar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"__END__")),
                token(TokenKind::Newline, pos_in_at(src, b"\n", 1)),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
        let src = EStrRef::from("foo \n __END__\n bar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"__END__")),
                token(TokenKind::Newline, pos_in_at(src, b"\n", 1)),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
        let src = EStrRef::from("foo \n__END__\r bar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"__END__")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
    }

    #[test]
    fn test_lex_spaces() {
        let src = EStrRef::from("foo bar\nbaz");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"baz")),
            ]
        );
    }

    #[test]
    fn test_lex_semicolon() {
        let src = EStrRef::from("foo;bar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b";")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = EStrRef::from("foo\nbar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = EStrRef::from("do\nbar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordDo, pos_in(src, b"do")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = EStrRef::from("foo\n  .bar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Dot, pos_in(src, b".")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = EStrRef::from("foo\n  ..bar");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Newline, pos_in(src, b"\n")),
                token(TokenKind::DotDot, pos_in(src, b"..")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
    }

    #[test]
    fn test_lex_comments() {
        let src = EStrRef::from("# comment2\nfoo bar # comment1\n# comment3\nbaz\n");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
                token(TokenKind::Newline, pos_in_at(src, b"\n", 1)),
                token(TokenKind::Identifier, pos_in(src, b"baz")),
                token(TokenKind::Newline, pos_in_at(src, b"\n", 3)),
            ]
        );
    }

    #[test]
    fn test_lex_keywords() {
        let src = EStrRef::from("__ENCODING__");
        assert_eq!(
            lex_all(src),
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreEncoding,
                pos_in(src, b"__ENCODING__")
            ),]
        );
        let src = EStrRef::from("__LINE__");
        assert_eq!(
            lex_all(src),
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreLine,
                pos_in(src, b"__LINE__")
            ),]
        );
        let src = EStrRef::from("__FILE__");
        assert_eq!(
            lex_all(src),
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreFile,
                pos_in(src, b"__FILE__")
            ),]
        );
        let src = EStrRef::from("BEGIN");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordCapitalBegin, pos_in(src, b"BEGIN")),]
        );
        let src = EStrRef::from("END");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordCapitalEnd, pos_in(src, b"END")),]
        );
        let src = EStrRef::from("alias");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordAlias, pos_in(src, b"alias")),]
        );
        let src = EStrRef::from("and");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordAnd, pos_in(src, b"and")),]
        );
        let src = EStrRef::from("begin");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordBegin, pos_in(src, b"begin")),]
        );
        let src = EStrRef::from("break");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordBreak, pos_in(src, b"break")),]
        );
        let src = EStrRef::from("case");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordCase, pos_in(src, b"case")),]
        );
        let src = EStrRef::from("class");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordClass, pos_in(src, b"class")),]
        );
        let src = EStrRef::from("def");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordDef, pos_in(src, b"def")),]
        );
        let src = EStrRef::from("defined?");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordDefinedQ, pos_in(src, b"defined?")),]
        );
        let src = EStrRef::from("do");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordDo, pos_in(src, b"do")),]
        );
        let src = EStrRef::from("else");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordElse, pos_in(src, b"else")),]
        );
        let src = EStrRef::from("elsif");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordElsif, pos_in(src, b"elsif")),]
        );
        let src = EStrRef::from("end");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordEnd, pos_in(src, b"end")),]
        );
        let src = EStrRef::from("ensure");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordEnsure, pos_in(src, b"ensure")),]
        );
        let src = EStrRef::from("false");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordFalse, pos_in(src, b"false")),]
        );
        let src = EStrRef::from("for");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordFor, pos_in(src, b"for")),]
        );
        let src = EStrRef::from("if");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordIf, pos_in(src, b"if")),]
        );
        let src = EStrRef::from("nil if");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordNil, pos_in(src, b"nil")),
                token(TokenKind::KeywordIfInfix, pos_in(src, b"if")),
            ]
        );
        let src = EStrRef::from("in");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordIn, pos_in(src, b"in")),]
        );
        let src = EStrRef::from("module");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordModule, pos_in(src, b"module")),]
        );
        let src = EStrRef::from("next");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordNext, pos_in(src, b"next")),]
        );
        let src = EStrRef::from("nil");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordNil, pos_in(src, b"nil")),]
        );
        let src = EStrRef::from("not");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordNot, pos_in(src, b"not")),]
        );
        let src = EStrRef::from("or");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordOr, pos_in(src, b"or")),]
        );
        let src = EStrRef::from("redo");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordRedo, pos_in(src, b"redo")),]
        );
        let src = EStrRef::from("rescue");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordRescue, pos_in(src, b"rescue")),]
        );
        let src = EStrRef::from("retry");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordRetry, pos_in(src, b"retry")),]
        );
        let src = EStrRef::from("return");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordReturn, pos_in(src, b"return")),]
        );
        let src = EStrRef::from("self");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordSelf, pos_in(src, b"self")),]
        );
        let src = EStrRef::from("super");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordSuper, pos_in(src, b"super")),]
        );
        let src = EStrRef::from("then");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordThen, pos_in(src, b"then")),]
        );
        let src = EStrRef::from("true");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordTrue, pos_in(src, b"true")),]
        );
        let src = EStrRef::from("undef");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordUndef, pos_in(src, b"undef")),]
        );
        let src = EStrRef::from("unless");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordUnless, pos_in(src, b"unless")),]
        );
        let src = EStrRef::from("nil unless");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordNil, pos_in(src, b"nil")),
                token(TokenKind::KeywordUnlessInfix, pos_in(src, b"unless")),
            ]
        );
        let src = EStrRef::from("until");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordUntil, pos_in(src, b"until")),]
        );
        let src = EStrRef::from("nil until");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordNil, pos_in(src, b"nil")),
                token(TokenKind::KeywordUntilInfix, pos_in(src, b"until")),
            ]
        );
        let src = EStrRef::from("when");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordWhen, pos_in(src, b"when")),]
        );
        let src = EStrRef::from("while");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordWhile, pos_in(src, b"while")),]
        );
        let src = EStrRef::from("nil while");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordNil, pos_in(src, b"nil")),
                token(TokenKind::KeywordWhileInfix, pos_in(src, b"while")),
            ]
        );
        let src = EStrRef::from("yield");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordYield, pos_in(src, b"yield")),]
        );
    }

    #[test]
    fn test_lex_ident() {
        let src = EStrRef::from("foo bar123 あ");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Identifier, pos_in(src, b"bar123")),
                token(TokenKind::Identifier, pos_in(src, b"\xE3\x81\x82")),
            ]
        );
    }

    #[test]
    fn test_lex_const() {
        let src = EStrRef::from("Foo Bar123 Ω");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Const, pos_in(src, b"Foo")),
                token(TokenKind::Const, pos_in(src, b"Bar123")),
                token(TokenKind::Const, pos_in(src, b"\xCE\xA9")),
            ]
        );
    }

    #[test]
    fn test_lex_method_name_normal_ctx() {
        let src = EStrRef::from("foo! bar123? Baz!");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"foo!")),
                token(TokenKind::MethodName, pos_in(src, b"bar123?")),
                token(TokenKind::MethodName, pos_in(src, b"Baz!")),
            ]
        );
    }

    #[test]
    fn test_lex_static_symbol() {
        let src = EStrRef::from(":foo :bar123 :Baz");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Symbol, pos_in(src, b":foo")),
                token(TokenKind::Symbol, pos_in(src, b":bar123")),
                token(TokenKind::Symbol, pos_in(src, b":Baz")),
            ]
        );
    }

    #[test]
    fn test_lex_ivar_name() {
        let src = EStrRef::from("@foo @bar123 @Baz");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::IvarName, pos_in(src, b"@foo")),
                token(TokenKind::IvarName, pos_in(src, b"@bar123")),
                token(TokenKind::IvarName, pos_in(src, b"@Baz")),
            ]
        );
    }

    #[test]
    fn test_lex_cvar_name() {
        let src = EStrRef::from("@@foo @@bar123 @@Baz");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::CvarName, pos_in(src, b"@@foo")),
                token(TokenKind::CvarName, pos_in(src, b"@@bar123")),
                token(TokenKind::CvarName, pos_in(src, b"@@Baz")),
            ]
        );
    }

    #[test]
    fn test_lex_gvar_name() {
        let src = EStrRef::from("$foo $bar123 $Baz");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::GvarName, pos_in(src, b"$foo")),
                token(TokenKind::GvarName, pos_in(src, b"$bar123")),
                token(TokenKind::GvarName, pos_in(src, b"$Baz")),
            ]
        );
    }

    #[test]
    fn test_lex_integer() {
        let src = EStrRef::from("123 456");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Integer, pos_in(src, b"123")),
                token(TokenKind::Integer, pos_in(src, b"456")),
            ]
        );
    }

    #[test]
    fn test_char_literal() {
        let src = EStrRef::from("?a");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::CharLiteral, pos_in(src, b"?a")),]
        );
    }

    #[test]
    fn test_quote_string_tokens() {
        let src = EStrRef::from("' foo '");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'")),
                token(TokenKind::StringContent, pos_in(src, " foo ")),
                token(TokenKind::StringEnd, pos_in_at(src, b"'", 1)),
            ]
        );
        let src = EStrRef::from("'\\''");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'")),
                token(TokenKind::StringContent, pos_in(src, "\\'")),
                token(TokenKind::StringEnd, pos_in_at(src, b"'", 2)),
            ]
        );
        let src = EStrRef::from("'\\\\'");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'")),
                token(TokenKind::StringContent, pos_in(src, "\\\\")),
                token(TokenKind::StringEnd, pos_in_at(src, b"'", 1)),
            ]
        );
    }

    #[test]
    fn test_double_quote_string_tokens() {
        let src = EStrRef::from("\" foo \"");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"\"")),
                token(TokenKind::StringContent, pos_in(src, " foo ")),
                token(TokenKind::StringEnd, pos_in_at(src, b"\"", 1)),
            ]
        );
        let src = EStrRef::from("\" foo #{bar}");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"\"")),
                token(TokenKind::StringContent, pos_in(src, " foo ")),
                token(TokenKind::StringInterpolationBegin, pos_in(src, "#{")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
                token(TokenKind::RBrace, pos_in(src, b"}")),
            ]
        );
    }

    #[test]
    fn test_backtick_string_tokens() {
        let src = EStrRef::from("` foo `");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"`")),
                token(TokenKind::StringContent, pos_in(src, " foo ")),
                token(TokenKind::StringEnd, pos_in_at(src, b"`", 1)),
            ]
        );
    }

    #[test]
    fn test_regexp_string_tokens() {
        let src = EStrRef::from("/ foo /");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"/")),
                token(TokenKind::StringContent, pos_in(src, " foo ")),
                token(TokenKind::StringEnd, pos_in_at(src, b"/", 1)),
            ]
        );
    }

    #[test]
    fn test_op_assign() {
        let src = EStrRef::from("**=");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::OpAssign, pos_in(src, b"**=")),]
        );

        let src = EStrRef::from("*= x /= y %=");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"*=")),
                token(TokenKind::Identifier, pos_in(src, b"x")),
                token(TokenKind::OpAssign, pos_in(src, b"/=")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
                token(TokenKind::OpAssign, pos_in(src, b"%=")),
            ]
        );

        let src = EStrRef::from("+= -=");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"+=")),
                token(TokenKind::OpAssign, pos_in(src, b"-=")),
            ]
        );

        let src = EStrRef::from("<<= >>=");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"<<=")),
                token(TokenKind::OpAssign, pos_in(src, b">>=")),
            ]
        );

        let src = EStrRef::from("&=");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::OpAssign, pos_in(src, b"&=")),]
        );

        let src = EStrRef::from("|= ^=");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"|=")),
                token(TokenKind::OpAssign, pos_in(src, b"^=")),
            ]
        );

        let src = EStrRef::from("&&= ||=");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"&&=")),
                token(TokenKind::OpAssign, pos_in(src, b"||=")),
            ]
        );
    }

    #[test]
    fn test_excl() {
        let src = EStrRef::from("!");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Excl, pos_in(src, b"!")),]
        );
    }

    #[test]
    fn test_excl_eq() {
        let src = EStrRef::from("!=");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::ExclEq, pos_in(src, b"!=")),]
        );
    }

    #[test]
    fn test_excl_tilde() {
        let src = EStrRef::from("!~");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::ExclTilde, pos_in(src, b"!~")),]
        );
    }

    #[test]
    fn test_percent() {
        let src = EStrRef::from("%");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Percent, pos_in(src, b"%")),]
        );
    }

    #[test]
    fn test_amp() {
        let src = EStrRef::from("&");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::AmpPrefix, pos_in(src, b"&")),]
        );
        let src = EStrRef::from("x & y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"x")),
                token(TokenKind::Amp, pos_in(src, b"&")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! & y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::Amp, pos_in(src, b"&")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! &y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::AmpPrefix, pos_in(src, b"&")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth!&y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::Amp, pos_in(src, b"&")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
    }

    #[test]
    fn test_amp_amp() {
        let src = EStrRef::from("&&");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::AmpAmp, pos_in(src, b"&&")),]
        );
    }

    #[test]
    fn test_amp_dot() {
        let src = EStrRef::from("&.");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::AmpDot, pos_in(src, b"&.")),]
        );
    }

    #[test]
    fn test_lparen() {
        let src = EStrRef::from("(");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::LParen, pos_in(src, b"(")),]
        );
        let src = EStrRef::from("x ( y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"x")),
                token(TokenKind::LParenRestricted, pos_in(src, b"(")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("x (y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"x")),
                token(TokenKind::LParenRestricted, pos_in(src, b"(")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("x(y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"x")),
                token(TokenKind::LParen, pos_in(src, b"(")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! ( y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::LParenRestricted, pos_in(src, b"(")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! (y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::LParenRestricted, pos_in(src, b"(")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth!(y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::LParen, pos_in(src, b"(")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
    }

    #[test]
    fn test_rparen() {
        let src = EStrRef::from(")");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::RParen, pos_in(src, b")")),]
        );
    }

    #[test]
    fn test_star() {
        let src = EStrRef::from("*");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::StarPrefix, pos_in(src, b"*")),]
        );
        let src = EStrRef::from("meth! * y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::Star, pos_in(src, b"*")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! *y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::StarPrefix, pos_in(src, b"*")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth!*y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::Star, pos_in(src, b"*")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
    }

    #[test]
    fn test_star_star() {
        let src = EStrRef::from("**");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**")),]
        );
        let src = EStrRef::from("meth! ** y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::StarStar, pos_in(src, b"**")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! **y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::StarStarPrefix, pos_in(src, b"**")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth!**y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::StarStar, pos_in(src, b"**")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
    }

    #[test]
    fn test_plus() {
        let src = EStrRef::from("+");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::PlusPrefix, pos_in(src, b"+")),]
        );
        let src = EStrRef::from("meth! + y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::Plus, pos_in(src, b"+")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! +y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::PlusPrefix, pos_in(src, b"+")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth!+y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::Plus, pos_in(src, b"+")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
    }

    #[test]
    fn test_comma() {
        let src = EStrRef::from(",");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Comma, pos_in(src, b",")),]
        );
    }

    #[test]
    fn test_minus() {
        let src = EStrRef::from("-");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::MinusPrefix, pos_in(src, b"-")),]
        );
        let src = EStrRef::from("meth! - y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::Minus, pos_in(src, b"-")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! -y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::MinusPrefix, pos_in(src, b"-")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth!-y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::Minus, pos_in(src, b"-")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
    }

    #[test]
    fn test_arrow() {
        let src = EStrRef::from("->");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Arrow, pos_in(src, b"->")),]
        );
    }

    #[test]
    fn test_dot() {
        let src = EStrRef::from(".");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Dot, pos_in(src, b".")),]
        );
    }

    #[test]
    fn test_dot_dot() {
        let src = EStrRef::from("..");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::DotDot, pos_in(src, b"..")),]
        );
    }

    #[test]
    fn test_dot_dot_dot() {
        let src = EStrRef::from("...");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::DotDotDot, pos_in(src, b"...")),]
        );
    }

    #[test]
    fn test_slash() {
        let src = EStrRef::from("x /");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"x")),
                token(TokenKind::Slash, pos_in(src, b"/")),
            ]
        );
    }

    #[test]
    fn test_colon() {
        let src = EStrRef::from(":");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Colon, pos_in(src, b":")),]
        );
    }

    #[test]
    fn test_colon_colon() {
        let src = EStrRef::from("::");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::")),]
        );
        let src = EStrRef::from("meth! :: Foo");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::ColonColonPrefix, pos_in(src, b"::")),
                token(TokenKind::Const, pos_in(src, b"Foo")),
            ]
        );
        let src = EStrRef::from("meth! ::Foo");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::ColonColonPrefix, pos_in(src, b"::")),
                token(TokenKind::Const, pos_in(src, b"Foo")),
            ]
        );
        let src = EStrRef::from("meth!::Foo");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::ColonColon, pos_in(src, b"::")),
                token(TokenKind::Const, pos_in(src, b"Foo")),
            ]
        );
    }

    #[test]
    fn test_lt() {
        let src = EStrRef::from("<");
        assert_eq!(lex_all(src), vec![token(TokenKind::Lt, pos_in(src, b"<")),]);
    }

    #[test]
    fn test_lt_lt() {
        let src = EStrRef::from("<<");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::LtLt, pos_in(src, b"<<")),]
        );
    }

    #[test]
    fn test_lt_eq() {
        let src = EStrRef::from("<=");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::LtEq, pos_in(src, b"<=")),]
        );
    }

    #[test]
    fn test_lt_eq_gt() {
        let src = EStrRef::from("<=>");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::LtEqGt, pos_in(src, b"<=>")),]
        );
    }

    #[test]
    fn test_eq() {
        let src = EStrRef::from("=");
        assert_eq!(lex_all(src), vec![token(TokenKind::Eq, pos_in(src, b"=")),]);
    }

    #[test]
    fn test_eq_eq() {
        let src = EStrRef::from("==");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::EqEq, pos_in(src, b"==")),]
        );
    }

    #[test]
    fn test_eq_eq_eq() {
        let src = EStrRef::from("===");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::EqEqEq, pos_in(src, b"===")),]
        );
    }

    #[test]
    fn test_fat_arrow() {
        let src = EStrRef::from("=>");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::FatArrow, pos_in(src, b"=>")),]
        );
    }

    #[test]
    fn test_eq_match() {
        let src = EStrRef::from("=~");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::EqMatch, pos_in(src, b"=~")),]
        );
    }

    #[test]
    fn test_gt() {
        let src = EStrRef::from(">");
        assert_eq!(lex_all(src), vec![token(TokenKind::Gt, pos_in(src, b">")),]);
    }

    #[test]
    fn test_gt_eq() {
        let src = EStrRef::from(">=");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::GtEq, pos_in(src, b">=")),]
        );
    }

    #[test]
    fn test_gt_gt() {
        let src = EStrRef::from(">>");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::GtGt, pos_in(src, b">>")),]
        );
    }

    #[test]
    fn test_question() {
        let src = EStrRef::from("?");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Question, pos_in(src, b"?")),]
        );
    }

    #[test]
    fn test_at() {
        let src = EStrRef::from("@");
        assert_eq!(lex_all(src), vec![token(TokenKind::At, pos_in(src, b"@")),]);
    }

    #[test]
    fn test_lbracket() {
        let src = EStrRef::from("[");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[")),]
        );
        let src = EStrRef::from("meth! [ y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::LBracketPrefix, pos_in(src, b"[")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth! [y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::LBracketPrefix, pos_in(src, b"[")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
        let src = EStrRef::from("meth![y");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::MethodName, pos_in(src, b"meth!")),
                token(TokenKind::LBracket, pos_in(src, b"[")),
                token(TokenKind::Identifier, pos_in(src, b"y")),
            ]
        );
    }

    #[test]
    fn test_rbracket() {
        let src = EStrRef::from("]");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::RBracket, pos_in(src, b"]")),]
        );
    }

    #[test]
    fn test_caret() {
        let src = EStrRef::from("^");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Caret, pos_in(src, b"^")),]
        );
    }

    #[test]
    fn test_lbrace() {
        let src = EStrRef::from("{");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::LBrace, pos_in(src, b"{")),]
        );
    }

    #[test]
    fn test_vert() {
        let src = EStrRef::from("|");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Vert, pos_in(src, b"|")),]
        );
    }

    #[test]
    fn test_vert_vert() {
        let src = EStrRef::from("x ||");
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"x")),
                token(TokenKind::VertVert, pos_in(src, b"||")),
            ]
        );
    }

    #[test]
    fn test_rbrace() {
        let src = EStrRef::from("}");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::RBrace, pos_in(src, b"}")),]
        );
    }

    #[test]
    fn test_tilde() {
        let src = EStrRef::from("~");
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Tilde, pos_in(src, b"~")),]
        );
    }
}
