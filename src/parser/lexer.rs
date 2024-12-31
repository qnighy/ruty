use std::{collections::HashMap, sync::LazyLock};

use crate::ast::CodeRange;

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
    /// `until`
    KeywordUntil,
    /// `when`
    KeywordWhen,
    /// `while`
    KeywordWhile,
    /// `yield`
    KeywordYield,
    /// `foo` etc.
    Identifier,
    /// `Foo` etc.
    Const,
    /// `@foo` etc.
    IvarName,
    /// `@@foo` etc.
    CvarName,
    /// `$foo` etc.
    GvarName,
    /// `123` etc.
    Integer,
    /// `:`
    Colon,
    /// `::`
    ColonColon,
    /// `;` or EOL in a certain condition, but most likely the latter.
    Semicolon,
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
    /// `@`
    At,
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

    pub(super) fn lex(&mut self, state: LexerState) -> Token {
        self.lex_space(state);
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
                if let Ok(s) = std::str::from_utf8(&self.input[start..self.pos]) {
                    if let Some(kwd) = KEYWORDS.get(s).copied() {
                        if kwd == TokenKind::EOF
                            && (!self.is_beginning_of_line(start) || !self.is_end_of_line(self.pos))
                        {
                            // __END__ not in the beginning of a line should be treated as an identifier
                            TokenKind::Identifier
                        } else {
                            kwd
                        }
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
            b':' => {
                self.pos += 1;
                match self.peek_byte() {
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
            b'@' => {
                self.pos += 1;
                match self.peek_byte() {
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'.. => {
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
                        TokenKind::IvarName
                    }
                    b'0'..=b'9' => {
                        while is_ident_continue(self.peek_byte()) {
                            self.pos += 1;
                        }
                        TokenKind::Unknown
                    }
                    b'@' => {
                        self.pos += 1;
                        match self.peek_byte() {
                            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'.. => {
                                while is_ident_continue(self.peek_byte()) {
                                    self.pos += 1;
                                }
                                TokenKind::CvarName
                            }
                            b'0'..=b'9' => {
                                while is_ident_continue(self.peek_byte()) {
                                    self.pos += 1;
                                }
                                TokenKind::Unknown
                            }
                            _ => TokenKind::Unknown,
                        }
                    }
                    _ => TokenKind::At,
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

    fn lex_space(&mut self, state: LexerState) -> bool {
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
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        let mut state = LexerState::default();
        loop {
            let token = lexer.lex(state);
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
            TokenKind::KeywordUntil => LexerState::Begin,
            TokenKind::KeywordWhen => LexerState::Begin,
            TokenKind::KeywordWhile => LexerState::Begin,
            TokenKind::KeywordYield => LexerState::Begin,
            TokenKind::Identifier => LexerState::End,
            TokenKind::Const => LexerState::End,
            TokenKind::IvarName => LexerState::End,
            TokenKind::CvarName => LexerState::End,
            TokenKind::GvarName => LexerState::End,
            TokenKind::Integer => LexerState::End,
            TokenKind::Colon => LexerState::Begin,
            TokenKind::ColonColon => LexerState::Begin,
            TokenKind::Semicolon => LexerState::Begin,
            TokenKind::Eq => LexerState::Begin,
            TokenKind::EqEq => LexerState::Begin,
            TokenKind::EqEqEq => LexerState::Begin,
            TokenKind::FatArrow => LexerState::Begin,
            TokenKind::EqMatch => LexerState::Begin,
            TokenKind::At => LexerState::Begin,
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
                // TODO: promote it to TokenKind::Dot
                token(TokenKind::Unknown, pos_in(src, b".")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );

        let src = b"foo\n  ..bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Semicolon, pos_in(src, b"\n")),
                // TODO: promote them to TokenKind::DotDot
                token(TokenKind::Unknown, pos_in(src, b".")),
                token(TokenKind::Unknown, pos_in_at(src, b".", 1)),
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
}
