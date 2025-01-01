use std::{collections::HashMap, sync::LazyLock};

use crate::{ast::CodeRange, Diagnostic};

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
    /// `&&`
    AmpAmp,
    /// `&.`
    AmpDot,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `*`
    Star,
    /// `**`
    StarStar,
    /// `+`
    Plus,
    /// `,`
    Comma,
    /// `-`
    Minus,
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
    /// `;` or EOL in a certain condition, but most likely the latter.
    Semicolon,
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
    ///
    /// ## Occurrences
    ///
    /// - The beginning of the file
    /// - ...
    ///
    /// ## Effects
    #[default]
    Begin,
    /// Expects a punctuator connecting expressions or following an expression.
    ///
    /// ## Occurrences
    ///
    /// - ...
    ///
    /// ## Effects
    End,
    /// Expects a method name.
    ///
    /// ## Occurrences
    ///
    /// - After `def`
    /// - After `.`
    /// - After `::`
    ///
    /// ## Effects
    Meth,
}

#[derive(Debug)]
pub(super) struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub(super) fn new(input: &'a [u8]) -> Self {
        Self { input, pos: 0 }
    }

    pub(super) fn input(&self) -> &'a [u8] {
        self.input
    }

    pub(super) fn lex(&mut self, diag: &mut Vec<Diagnostic>, state: LexerState) -> Token {
        self.lex_space(diag, state);
        let start = self.pos;
        let kind = match self.peek_byte() {
            b'\0' | b'\x04' | b'\x1A' => {
                if self.pos < self.input.len() {
                    self.pos += 1;
                }
                TokenKind::EOF
            }
            b'\n' => {
                if self.pos >= 1 && self.input()[self.pos - 1] == b'\r' {
                    // Include the preceding CR to form CRLF
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Semicolon,
                        range: CodeRange {
                            start: self.pos - 1,
                            end: self.pos,
                        },
                    };
                }
                self.pos += 1;
                TokenKind::Semicolon
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
                    b'=' if state == LexerState::Meth
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
                if let Ok(s) = std::str::from_utf8(&self.input[start..self.pos]) {
                    if let Some(kwd) = KEYWORDS.get(s).copied() {
                        match kwd {
                            TokenKind::EOF => {
                                if self.is_beginning_of_line(start) && self.is_end_of_line(self.pos)
                                {
                                    TokenKind::EOF
                                } else {
                                    // __END__ not occupying the whole line should be treated as an ordinary identifier
                                    TokenKind::Identifier
                                }
                            }
                            TokenKind::KeywordIf if state == LexerState::End => {
                                TokenKind::KeywordIfInfix
                            }
                            TokenKind::KeywordUnless if state == LexerState::End => {
                                TokenKind::KeywordUnlessInfix
                            }
                            TokenKind::KeywordWhile if state == LexerState::End => {
                                TokenKind::KeywordWhileInfix
                            }
                            TokenKind::KeywordUntil if state == LexerState::End => {
                                TokenKind::KeywordUntilInfix
                            }
                            _ => kwd,
                        }
                    } else if suffixed {
                        TokenKind::MethodName
                    } else {
                        let ch = s.chars().next().unwrap();
                        if ch.is_uppercase() {
                            TokenKind::Const
                        } else {
                            TokenKind::Identifier
                        }
                    }
                } else {
                    TokenKind::Unknown
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
                    b'@' if state == LexerState::Meth => {
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
                    _ => TokenKind::Amp,
                }
            }
            b'\'' => {
                self.pos += 1;
                TokenKind::StringBegin
            }
            b'(' => {
                self.pos += 1;
                TokenKind::LParen
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
                            _ => TokenKind::StarStar,
                        }
                    }
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign
                    }
                    _ => TokenKind::Star,
                }
            }
            // `+`
            // `+@`
            // `+=`
            b'+' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'@' if state == LexerState::Meth => {
                        self.pos += 1;
                        TokenKind::MethodName
                    }
                    b'=' => {
                        self.pos += 1;
                        TokenKind::OpAssign
                    }
                    _ => TokenKind::Plus,
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
                        if state == LexerState::Begin && self.peek_byte_at(1).is_ascii_digit() =>
                    {
                        while self.peek_byte().is_ascii_digit() {
                            self.pos += 1;
                        }
                        TokenKind::Integer
                    }
                    b'@' if state == LexerState::Meth => {
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
                    _ => TokenKind::Minus,
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
                if state == LexerState::Begin {
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
                        TokenKind::ColonColon
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
                if state == LexerState::End {
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
                        if is_numeric {
                            diag.push(Diagnostic {
                                range: CodeRange {
                                    start,
                                    end: self.pos,
                                },
                                message: format!("Invalid instance variable name"),
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
                                if is_numeric {
                                    diag.push(Diagnostic {
                                        range: CodeRange {
                                            start,
                                            end: self.pos,
                                        },
                                        message: format!("Invalid class variable name"),
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
                if state == LexerState::Meth && self.peek_byte() == b']' {
                    self.pos += 1;
                    match self.peek_byte() {
                        b'=' => {
                            self.pos += 1;
                            TokenKind::MethodName
                        }
                        _ => TokenKind::MethodName,
                    }
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
                if state == LexerState::Meth {
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
                            _ if state == LexerState::Begin => {
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
                    b'@' if state == LexerState::Meth => {
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

    fn lex_space(&mut self, diag: &mut Vec<Diagnostic>, state: LexerState) -> bool {
        let fold = match state {
            LexerState::Begin => true,
            LexerState::End => false,
            LexerState::Meth => true,
        };
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
                b'\t' | b'\n' | b'\x0C' | b'\r' | b'\x13' | b' ' => {
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
                b'\t' | b'\x0C' | b'\r' | b'\x13' | b' ' => {
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
            // as a semicolon token.
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

    fn peek_byte(&mut self) -> u8 {
        self.input.get(self.pos).copied().unwrap_or(0)
    }

    fn peek_byte_at(&mut self, offset: usize) -> u8 {
        self.input.get(self.pos + offset).copied().unwrap_or(0)
    }

    fn is_beginning_of_line(&self, pos: usize) -> bool {
        pos == 0 || self.input[pos - 1] == b'\n'
    }
    fn is_end_of_line(&self, pos: usize) -> bool {
        pos == self.input.len()
            || self.input[pos] == b'\n'
            || (self.input[pos] == b'\r' && self.input.get(pos + 1).copied() == Some(b'\n'))
    }
}

// fn is_ident_start(b: u8) -> bool {
//     b.is_ascii_alphabetic() || b == b'_' || b >= 0x80
// }
fn is_ident_continue(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b >= 0x80
}

static KEYWORDS: LazyLock<HashMap<&'static str, TokenKind>> = LazyLock::new(|| {
    HashMap::from_iter(vec![
        (
            "__ENCODING__",
            TokenKind::KeywordCapitalDoubleUnderscoreEncoding,
        ),
        ("__LINE__", TokenKind::KeywordCapitalDoubleUnderscoreLine),
        // It requires an additional position check
        ("__END__", TokenKind::EOF),
        ("__FILE__", TokenKind::KeywordCapitalDoubleUnderscoreFile),
        ("BEGIN", TokenKind::KeywordCapitalBegin),
        ("END", TokenKind::KeywordCapitalEnd),
        ("alias", TokenKind::KeywordAlias),
        ("and", TokenKind::KeywordAnd),
        ("begin", TokenKind::KeywordBegin),
        ("break", TokenKind::KeywordBreak),
        ("case", TokenKind::KeywordCase),
        ("class", TokenKind::KeywordClass),
        ("def", TokenKind::KeywordDef),
        ("defined?", TokenKind::KeywordDefinedQ),
        ("do", TokenKind::KeywordDo),
        ("else", TokenKind::KeywordElse),
        ("elsif", TokenKind::KeywordElsif),
        ("end", TokenKind::KeywordEnd),
        ("ensure", TokenKind::KeywordEnsure),
        ("false", TokenKind::KeywordFalse),
        ("for", TokenKind::KeywordFor),
        ("if", TokenKind::KeywordIf),
        ("in", TokenKind::KeywordIn),
        ("module", TokenKind::KeywordModule),
        ("next", TokenKind::KeywordNext),
        ("nil", TokenKind::KeywordNil),
        ("not", TokenKind::KeywordNot),
        ("or", TokenKind::KeywordOr),
        ("redo", TokenKind::KeywordRedo),
        ("rescue", TokenKind::KeywordRescue),
        ("retry", TokenKind::KeywordRetry),
        ("return", TokenKind::KeywordReturn),
        ("self", TokenKind::KeywordSelf),
        ("super", TokenKind::KeywordSuper),
        ("then", TokenKind::KeywordThen),
        ("true", TokenKind::KeywordTrue),
        ("undef", TokenKind::KeywordUndef),
        ("unless", TokenKind::KeywordUnless),
        ("until", TokenKind::KeywordUntil),
        ("when", TokenKind::KeywordWhen),
        ("while", TokenKind::KeywordWhile),
        ("yield", TokenKind::KeywordYield),
    ])
});

#[cfg(test)]
mod tests {
    use crate::ast::{pos_in, pos_in_at};

    use super::*;

    fn lex_all(input: &[u8]) -> Vec<Token> {
        let mut diag = Vec::<Diagnostic>::new();
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        let mut state = LexerState::default();
        loop {
            let token = lexer.lex(&mut diag, state);
            if token.kind == TokenKind::EOF {
                break;
            }
            state = next_state_for_testing(&token);
            tokens.push(token);
        }
        tokens
    }

    fn next_state_for_testing(tok: &Token) -> LexerState {
        match tok.kind {
            TokenKind::KeywordCapitalDoubleUnderscoreEncoding => LexerState::End,
            TokenKind::KeywordCapitalDoubleUnderscoreLine => LexerState::End,
            TokenKind::KeywordCapitalDoubleUnderscoreFile => LexerState::End,
            TokenKind::KeywordCapitalBegin => LexerState::Begin,
            TokenKind::KeywordCapitalEnd => LexerState::Begin,
            TokenKind::KeywordAlias => LexerState::Meth,
            TokenKind::KeywordAnd => LexerState::Begin,
            TokenKind::KeywordBegin => LexerState::Begin,
            TokenKind::KeywordBreak => LexerState::Begin,
            TokenKind::KeywordCase => LexerState::Begin,
            TokenKind::KeywordClass => LexerState::Begin,
            TokenKind::KeywordDef => LexerState::Meth,
            TokenKind::KeywordDefinedQ => LexerState::Begin,
            TokenKind::KeywordDo => LexerState::Begin,
            TokenKind::KeywordElse => LexerState::Begin,
            TokenKind::KeywordElsif => LexerState::Begin,
            TokenKind::KeywordEnd => LexerState::End,
            TokenKind::KeywordEnsure => LexerState::Begin,
            TokenKind::KeywordFalse => LexerState::End,
            TokenKind::KeywordFor => LexerState::Begin,
            TokenKind::KeywordIf => LexerState::Begin,
            TokenKind::KeywordIfInfix => LexerState::Begin,
            TokenKind::KeywordIn => LexerState::Begin,
            TokenKind::KeywordModule => LexerState::Begin,
            TokenKind::KeywordNext => LexerState::Begin,
            TokenKind::KeywordNil => LexerState::End,
            TokenKind::KeywordNot => LexerState::Begin,
            TokenKind::KeywordOr => LexerState::Begin,
            TokenKind::KeywordRedo => LexerState::End,
            TokenKind::KeywordRescue => LexerState::Begin,
            TokenKind::KeywordRetry => LexerState::End,
            TokenKind::KeywordReturn => LexerState::Begin,
            TokenKind::KeywordSelf => LexerState::End,
            TokenKind::KeywordSuper => LexerState::End,
            TokenKind::KeywordThen => LexerState::Begin,
            TokenKind::KeywordTrue => LexerState::End,
            TokenKind::KeywordUndef => LexerState::Meth,
            TokenKind::KeywordUnless => LexerState::Begin,
            TokenKind::KeywordUnlessInfix => LexerState::Begin,
            TokenKind::KeywordUntil => LexerState::Begin,
            TokenKind::KeywordUntilInfix => LexerState::Begin,
            TokenKind::KeywordWhen => LexerState::Begin,
            TokenKind::KeywordWhile => LexerState::Begin,
            TokenKind::KeywordWhileInfix => LexerState::Begin,
            TokenKind::KeywordYield => LexerState::Begin,
            TokenKind::Identifier => LexerState::End,
            TokenKind::Const => LexerState::End,
            TokenKind::MethodName => LexerState::End,
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
            TokenKind::StringBegin => LexerState::Begin,
            TokenKind::StringEnd => LexerState::End,
            TokenKind::StringEndColon => LexerState::Begin,
            TokenKind::StringContent => LexerState::Begin,
            TokenKind::StringInterpolationBegin => LexerState::Begin,
            TokenKind::OpAssign => LexerState::Begin,
            TokenKind::Excl => LexerState::Begin,
            TokenKind::ExclEq => LexerState::Begin,
            TokenKind::ExclTilde => LexerState::Begin,
            TokenKind::Percent => LexerState::Begin,
            TokenKind::Amp => LexerState::Begin,
            TokenKind::AmpAmp => LexerState::Begin,
            TokenKind::AmpDot => LexerState::Meth,
            TokenKind::LParen => LexerState::Begin,
            TokenKind::RParen => LexerState::End,
            TokenKind::Star => LexerState::Begin,
            TokenKind::StarStar => LexerState::Begin,
            TokenKind::Plus => LexerState::Begin,
            TokenKind::Comma => LexerState::Begin,
            TokenKind::Minus => LexerState::Begin,
            TokenKind::Arrow => LexerState::Begin,
            TokenKind::Dot => LexerState::Meth,
            TokenKind::DotDot => LexerState::Begin,
            TokenKind::DotDotDot => LexerState::Begin,
            TokenKind::Slash => LexerState::Begin,
            TokenKind::Colon => LexerState::Begin,
            TokenKind::ColonColon => LexerState::Begin,
            TokenKind::Semicolon => LexerState::Begin,
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
            TokenKind::LBracket => LexerState::Begin,
            TokenKind::RBracket => LexerState::End,
            TokenKind::Caret => LexerState::Begin,
            TokenKind::LBrace => LexerState::Begin,
            TokenKind::Vert => LexerState::Begin,
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
        let src = b"foo \0 bar";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
        let src = b"foo \x04 bar";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
        let src = b"foo \x1A bar";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
        let src = b"foo \n__END__\n bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
            ]
        );
        let src = b"foo \n__END__\r\n bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
            ]
        );
        let src = b"foo \n__END__";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
            ]
        );
    }

    #[test]
    fn test_lex_non_eof() {
        let src = b"foo \n__END__ \n bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"__END__")),
                token(TokenKind::Semicolon, pos_in_at(src, b"\n", 1)),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
        let src = b"foo \n __END__\n bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"__END__")),
                token(TokenKind::Semicolon, pos_in_at(src, b"\n", 1)),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
        let src = b"foo \n__END__\r bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"__END__")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
    }

    #[test]
    fn test_lex_spaces() {
        let src = b"foo bar\nbaz";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"baz")),
            ]
        );
    }

    #[test]
    fn test_lex_semicolon() {
        let src = b"foo;bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b";")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = b"foo\nbar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = b"do\nbar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordDo, pos_in(src, b"do")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = b"foo\n  .bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Dot, pos_in(src, b".")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = b"foo\n  ..bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
                token(TokenKind::DotDot, pos_in(src, b"..")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
    }

    #[test]
    fn test_lex_comments() {
        let src = b"# comment2\nfoo bar # comment1\n# comment3\nbaz\n";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
                token(TokenKind::Semicolon, pos_in_at(src, b"\n", 1)),
                token(TokenKind::Identifier, pos_in(src, b"baz")),
                token(TokenKind::Semicolon, pos_in_at(src, b"\n", 3)),
            ]
        );
    }

    #[test]
    fn test_lex_keywords() {
        let src = b"__ENCODING__";
        assert_eq!(
            lex_all(src),
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreEncoding,
                pos_in(src, b"__ENCODING__")
            ),]
        );
        let src = b"__LINE__";
        assert_eq!(
            lex_all(src),
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreLine,
                pos_in(src, b"__LINE__")
            ),]
        );
        let src = b"__FILE__";
        assert_eq!(
            lex_all(src),
            vec![token(
                TokenKind::KeywordCapitalDoubleUnderscoreFile,
                pos_in(src, b"__FILE__")
            ),]
        );
        let src = b"BEGIN";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordCapitalBegin, pos_in(src, b"BEGIN")),]
        );
        let src = b"END";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordCapitalEnd, pos_in(src, b"END")),]
        );
        let src = b"alias";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordAlias, pos_in(src, b"alias")),]
        );
        let src = b"and";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordAnd, pos_in(src, b"and")),]
        );
        let src = b"begin";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordBegin, pos_in(src, b"begin")),]
        );
        let src = b"break";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordBreak, pos_in(src, b"break")),]
        );
        let src = b"case";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordCase, pos_in(src, b"case")),]
        );
        let src = b"class";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordClass, pos_in(src, b"class")),]
        );
        let src = b"def";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordDef, pos_in(src, b"def")),]
        );
        let src = b"defined?";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordDefinedQ, pos_in(src, b"defined?")),]
        );
        let src = b"do";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordDo, pos_in(src, b"do")),]
        );
        let src = b"else";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordElse, pos_in(src, b"else")),]
        );
        let src = b"elsif";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordElsif, pos_in(src, b"elsif")),]
        );
        let src = b"end";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordEnd, pos_in(src, b"end")),]
        );
        let src = b"ensure";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordEnsure, pos_in(src, b"ensure")),]
        );
        let src = b"false";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordFalse, pos_in(src, b"false")),]
        );
        let src = b"for";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordFor, pos_in(src, b"for")),]
        );
        let src = b"if";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordIf, pos_in(src, b"if")),]
        );
        let src = b"nil if";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordNil, pos_in(src, b"nil")),
                token(TokenKind::KeywordIfInfix, pos_in(src, b"if")),
            ]
        );
        let src = b"in";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordIn, pos_in(src, b"in")),]
        );
        let src = b"module";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordModule, pos_in(src, b"module")),]
        );
        let src = b"next";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordNext, pos_in(src, b"next")),]
        );
        let src = b"nil";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordNil, pos_in(src, b"nil")),]
        );
        let src = b"not";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordNot, pos_in(src, b"not")),]
        );
        let src = b"or";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordOr, pos_in(src, b"or")),]
        );
        let src = b"redo";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordRedo, pos_in(src, b"redo")),]
        );
        let src = b"rescue";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordRescue, pos_in(src, b"rescue")),]
        );
        let src = b"retry";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordRetry, pos_in(src, b"retry")),]
        );
        let src = b"return";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordReturn, pos_in(src, b"return")),]
        );
        let src = b"self";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordSelf, pos_in(src, b"self")),]
        );
        let src = b"super";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordSuper, pos_in(src, b"super")),]
        );
        let src = b"then";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordThen, pos_in(src, b"then")),]
        );
        let src = b"true";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordTrue, pos_in(src, b"true")),]
        );
        let src = b"undef";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordUndef, pos_in(src, b"undef")),]
        );
        let src = b"unless";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordUnless, pos_in(src, b"unless")),]
        );
        let src = b"nil unless";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordNil, pos_in(src, b"nil")),
                token(TokenKind::KeywordUnlessInfix, pos_in(src, b"unless")),
            ]
        );
        let src = b"until";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordUntil, pos_in(src, b"until")),]
        );
        let src = b"nil until";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordNil, pos_in(src, b"nil")),
                token(TokenKind::KeywordUntilInfix, pos_in(src, b"until")),
            ]
        );
        let src = b"when";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordWhen, pos_in(src, b"when")),]
        );
        let src = b"while";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordWhile, pos_in(src, b"while")),]
        );
        let src = b"nil while";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::KeywordNil, pos_in(src, b"nil")),
                token(TokenKind::KeywordWhileInfix, pos_in(src, b"while")),
            ]
        );
        let src = b"yield";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::KeywordYield, pos_in(src, b"yield")),]
        );
    }

    #[test]
    fn test_lex_ident() {
        let src = b"foo bar123 \xE3\x81\x82";
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
        let src = b"Foo Bar123 \xCE\xA9";
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
        let src = b"foo! bar123? Baz!";
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
        let src = b":foo :bar123 :Baz";
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
        let src = b"@foo @bar123 @Baz";
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
        let src = b"@@foo @@bar123 @@Baz";
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
        let src = b"$foo $bar123 $Baz";
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
        let src = b"123 456";
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
        let src = b"?a";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::CharLiteral, pos_in(src, b"?a")),]
        );
    }

    #[test]
    fn test_op_assign() {
        let src = b"**=";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::OpAssign, pos_in(src, b"**=")),]
        );

        let src = b"*= x /= y %=";
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

        let src = b"+= -=";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"+=")),
                token(TokenKind::OpAssign, pos_in(src, b"-=")),
            ]
        );

        let src = b"<<= >>=";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"<<=")),
                token(TokenKind::OpAssign, pos_in(src, b">>=")),
            ]
        );

        let src = b"&=";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::OpAssign, pos_in(src, b"&=")),]
        );

        let src = b"|= ^=";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"|=")),
                token(TokenKind::OpAssign, pos_in(src, b"^=")),
            ]
        );

        let src = b"&&= ||=";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::OpAssign, pos_in(src, b"&&=")),
                token(TokenKind::OpAssign, pos_in(src, b"||=")),
            ]
        );
    }
}
