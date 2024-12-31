use crate::ast::CodeRange;
use anyhow::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Token {
    pub(super) kind: TokenKind,
    pub(super) range: CodeRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum TokenKind {
    Identifier,
    Const,
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

    pub(super) fn lex(&mut self) -> Result<Token, Error> {
        self.lex_space()?;
        let start = self.pos;
        let kind = match self.peek_byte() {
            b'\0' | b'\x04' | b'\x1A' => {
                if self.pos < self.input.len() {
                    self.pos += 1;
                }
                TokenKind::EOF
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\x80'.. => {
                while self.peek_byte().is_ascii_alphanumeric()
                    || self.peek_byte() == b'_'
                    || self.peek_byte() >= 0x80
                {
                    self.pos += 1;
                }
                let s = std::str::from_utf8(&self.input[start..self.pos])?;
                let ch = s.chars().next().unwrap();
                if ch.is_uppercase() {
                    TokenKind::Const
                } else {
                    TokenKind::Identifier
                }
            }
            b'0'..=b'9' => {
                while self.peek_byte().is_ascii_digit() {
                    self.pos += 1;
                }
                TokenKind::Integer
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
                        todo!("parse identifier after @");
                    }
                    b'0'..=b'9' => {
                        return Err(Error::msg(format!(
                            "unexpected character: {:?}",
                            self.peek_byte()
                        )));
                    }
                    _ => TokenKind::At,
                }
            }
            _ => {
                return Err(Error::msg(format!(
                    "unexpected character: {:?}",
                    self.peek_byte()
                )));
            }
        };
        let end = self.pos;
        Ok(Token {
            kind,
            range: CodeRange { start, end },
        })
    }

    fn lex_space(&mut self) -> Result<bool, Error> {
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
        Ok(self.pos > start)
    }

    fn peek_byte(&mut self) -> u8 {
        self.input.get(self.pos).copied().unwrap_or(0)
    }
}
