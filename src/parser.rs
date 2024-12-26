use anyhow::Error;

use crate::{Expr, IntegerExpr, IntegerType, StringType, Type};

pub fn parse_expr(input: &[u8]) -> Result<Expr, Error> {
    let mut parser = Parser::new(input);
    let expr = parser.parse_expr()?;
    Ok(expr)
}

pub fn parse_type(input: &[u8]) -> Result<Type, Error> {
    let mut parser = Parser::new(input);
    let ty = parser.parse_type()?;
    Ok(ty)
}

#[derive(Debug)]
struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a [u8]) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        let token = self.lex()?;
        match token.kind {
            TokenKind::Integer => Ok(IntegerExpr {
                value: std::str::from_utf8(&self.input[token.start..token.end])?
                    .parse()
                    .unwrap(),
            }
            .into()),
            _ => {
                return Err(Error::msg("unexpected token"));
            }
        }
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let token = self.lex()?;
        match token.kind {
            TokenKind::Const => {
                let s = std::str::from_utf8(&self.input[token.start..token.end])?;
                match s {
                    "Integer" => Ok(IntegerType {}.into()),
                    "String" => Ok(StringType {}.into()),
                    _ => Err(Error::msg("unexpected token")),
                }
            }
            _ => Err(Error::msg("unexpected token")),
        }
    }

    fn lex(&mut self) -> Result<Token, Error> {
        self.lex_space()?;
        let start = self.pos;
        let kind = match self.peek_byte() {
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
            b'=' => {
                self.pos += 1;
                TokenKind::Eq
            }
            b':' => {
                self.pos += 1;
                if self.peek_byte() == b':' {
                    self.pos += 1;
                    TokenKind::ColonColon
                } else {
                    return Err(Error::msg("expected `::`"));
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
        Ok(Token { kind, start, end })
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct Token {
    kind: TokenKind,
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenKind {
    Identifier,
    Const,
    Integer,
    Eq,
    ColonColon,
}

#[cfg(test)]
mod tests {
    use crate::IntegerExpr;

    use super::*;

    #[test]
    fn test_parse_integer_expr() {
        assert_eq!(parse_expr(b"42").unwrap(), IntegerExpr { value: 42 }.into())
    }

    #[test]
    fn test_parse_integer_type() {
        assert_eq!(parse_type(b"Integer").unwrap(), IntegerType {}.into())
    }
}
