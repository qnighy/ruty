use std::{collections::HashMap, sync::LazyLock};

use crate::ast::CodeRange;

#[derive(Debug, Clone, PartialEq, Eq)]
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
    EOF,
    Error,
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

    pub(super) fn lex(&mut self) -> Token {
        self.lex_space();
        let start = self.pos;
        let kind = match self.peek_byte() {
            b'\0' | b'\x04' | b'\x1A' => {
                if self.pos < self.input.len() {
                    self.pos += 1;
                }
                TokenKind::EOF
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
                    TokenKind::Error
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
                    _ => TokenKind::Error,
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
                        TokenKind::Error
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
                                TokenKind::Error
                            }
                            _ => TokenKind::Error,
                        }
                    }
                    _ => TokenKind::At,
                }
            }
            _ => {
                self.pos += 1;
                TokenKind::Error
            }
        };
        let end = self.pos;
        Token {
            kind,
            range: CodeRange { start, end },
        }
    }

    fn lex_space(&mut self) -> bool {
        let start = self.pos;
        loop {
            match self.peek_byte() {
                b'\t' | b'\n' | b'\x0C' | b'\r' | b'\x13' | b' ' => {
                    self.pos += 1;
                }
                b'#' => {
                    self.pos += 1;
                    loop {
                        match self.peek_byte() {
                            b'\n' => {
                                self.pos += 1;
                                break;
                            }
                            _ => {
                                self.pos += 1;
                            }
                        }
                    }
                }
                _ => {
                    break;
                }
            }
        }
        self.pos > start
    }

    fn peek_byte(&mut self) -> u8 {
        self.input.get(self.pos).copied().unwrap_or(0)
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
    use crate::ast::pos_in;

    use super::*;

    fn lex_all(input: &[u8]) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.lex();
            if token.kind == TokenKind::EOF {
                break;
            }
            tokens.push(token);
        }
        tokens
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
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
        let src = b"foo \n__END__\r\n bar";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
        let src = b"foo \n__END__";
        assert_eq!(
            lex_all(src),
            vec![token(TokenKind::Identifier, pos_in(src, b"foo")),]
        );
    }

    #[test]
    fn test_lex_non_eof() {
        let src = b"foo \n__END__ \n bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Identifier, pos_in(src, b"__END__")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
        let src = b"foo \n __END__\n bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
                token(TokenKind::Identifier, pos_in(src, b"__END__")),
                token(TokenKind::Identifier, pos_in(src, b"bar")),
            ]
        );
        let src = b"foo \n__END__\r bar";
        assert_eq!(
            lex_all(src),
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo")),
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
                token(TokenKind::Identifier, pos_in(src, b"baz")),
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
                token(TokenKind::Identifier, pos_in(src, b"baz")),
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
