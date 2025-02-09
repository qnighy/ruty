use std::{collections::HashMap, sync::LazyLock};

use crate::{
    ast::CodeRange,
    encoding::{EStrRef, EncodingState},
    Diagnostic,
};

const TAB_WIDTH: usize = 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Token {
    pub(super) kind: TokenKind,
    pub(super) range: CodeRange,
    pub(super) indent: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

    // TODO: merge Integer/Float/Rational/Imaginary into Numeric
    /// `123` etc., namely `tINTEGER`
    Integer,
    /// `123.0` etc., namely `tFLOAT`
    Float,
    /// `123r` etc., namely `tRATIONAL`
    Rational,
    /// `123i` etc., namely `tIMAGINARY`
    Imaginary,
    /// `?a` etc., namely `tCHAR`
    CharLiteral,

    /// `"`, `'`, `:"`, `/` etc. in expr context. Namely:
    ///
    /// - `tSTRING_BEG`
    /// - `tXSTRING_BEG`
    /// - `tREGEXP_BEG`
    StringBegin,
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
    /// Special state for string contents.
    StringLike(StringDelimiter),
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
            LexerState::StringLike(_) => unreachable!(),
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
            LexerState::StringLike(_) => unreachable!(),
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
    indent: usize,
}

impl<'a> Lexer<'a> {
    pub(super) fn new(input: EStrRef<'a>) -> Self {
        let mut this = Self {
            input,
            pos: 0,
            indent: 0,
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
        Token {
            kind,
            range: CodeRange { start, end },
            indent: self.indent,
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
                indent: self.indent,
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
                    indent: self.indent,
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
                    indent: self.indent,
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
                    indent: self.indent,
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
                    indent: self.indent,
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
                    indent: self.indent,
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
        loop {
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
            state = next_state_for_testing(&token, input.as_bytes(), state);
            tokens.push(token);
        }
        (tokens, diag)
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
            vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 1)]
        });
    }

    #[test]
    fn test_lex_spaces_tab() {
        assert_lex("\t1", |src| {
            vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 8)]
        });
    }

    #[test]
    fn test_lex_spaces_tab_and_space() {
        assert_lex("\t 1", |src| {
            vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 9)]
        });
    }

    #[test]
    fn test_lex_spaces_space_and_tab() {
        assert_lex(" \t1", |src| {
            vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 8)]
        });
    }

    #[test]
    fn test_lex_spaces_vtab() {
        assert_lex("\x0B1", |src| {
            vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 0)]
        });
    }

    #[test]
    fn test_lex_spaces_ff() {
        assert_lex("\x0C1", |src| {
            vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 0)]
        });
    }

    #[test]
    fn test_lex_spaces_cr() {
        assert_lex("\r1", |src| {
            vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 0)]
        });
    }

    #[test]
    fn test_lex_spaces_multiple() {
        assert_lex("  1", |src| {
            vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 2)]
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
            |src| vec![token(TokenKind::Integer, pos_in(src, b"1", 0), 0)],
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
    fn test_lex_static_symbol_ident_like() {
        assert_lex(":foo123", |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0)]
        });
    }

    #[test]
    fn test_lex_static_symbol_const_like() {
        assert_lex(":Baz", |src| {
            vec![token(TokenKind::Symbol, pos_in(src, b":Baz", 0), 0)]
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
    fn test_lex_integer() {
        assert_lex("123", |src| {
            vec![token(TokenKind::Integer, pos_in(src, b"123", 0), 0)]
        });
    }

    #[test]
    fn test_quote_string_tokens_simple() {
        assert_lex("' foo '", |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
            ]
        });
    }

    #[test]
    fn test_quote_string_tokens_escaped() {
        assert_lex("'\\''", |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
                token(TokenKind::StringContent, pos_in(src, "\\'", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"'", 2), 0),
            ]
        });
    }

    #[test]
    fn test_quote_string_tokens_backslashes() {
        assert_lex("'\\\\'", |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
                token(TokenKind::StringContent, pos_in(src, "\\\\", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
            ]
        });
    }

    #[test]
    fn test_double_quote_string_tokens_simple() {
        assert_lex("\" foo \"", |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"\"", 1), 0),
            ]
        });
    }

    #[test]
    fn test_double_quote_string_tokens_dynamic() {
        assert_lex("\" foo #{bar}", |src| {
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
}
