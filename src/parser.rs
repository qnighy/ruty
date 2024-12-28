use anyhow::Error;

use crate::{
    Expr, IntegerExpr, IntegerType, LocalVariableExpr, LocalVariableWriteTarget, StringType, Type,
    WriteExpr, WriteTarget,
};

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
        let (expr, token) = self.parse_expr_lv_assignment()?;
        match token.kind {
            TokenKind::EOF => {}
            _ => return Err(Error::msg("unexpected token (expected EOF)")),
        }
        Ok(expr)
    }

    fn parse_expr_lv_assignment(&mut self) -> Result<(Expr, Token), Error> {
        let (expr, token) = self.parse_expr_lv_type_annotated()?;
        match token.kind {
            TokenKind::Eq => {
                let (rhs, token) = self.parse_expr_lv_assignment()?;
                let lhs: WriteTarget = match expr {
                    Expr::LocalVariable(expr) => LocalVariableWriteTarget {
                        name: expr.name,
                        type_annotation: expr.type_annotation,
                    }
                    .into(),
                    _ => {
                        return Err(Error::msg("non-assignable expression"));
                    }
                };
                let expr: Expr = WriteExpr {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .into();
                return Ok((expr, token));
            }
            _ => {}
        }
        Ok((expr, token))
    }

    fn parse_expr_lv_type_annotated(&mut self) -> Result<(Expr, Token), Error> {
        let mut expr = self.parse_expr_lv_primary()?;
        let mut token = self.lex()?;
        loop {
            match token.kind {
                TokenKind::At => {
                    let ty = self.parse_type()?;
                    expr = match expr {
                        Expr::LocalVariable(mut e) => {
                            e.type_annotation = Some(ty);
                            e.into()
                        }
                        _ => {
                            return Err(Error::msg("non-annotatable expression"));
                        }
                    };
                    token = self.lex()?;
                }
                _ => break,
            }
        }

        Ok((expr, token))
    }

    fn parse_expr_lv_primary(&mut self) -> Result<Expr, Error> {
        let token = self.lex()?;
        match token.kind {
            TokenKind::Identifier => {
                let s = std::str::from_utf8(&self.input[token.start..token.end])?;
                Ok(LocalVariableExpr {
                    name: s.to_owned(),
                    type_annotation: None,
                }
                .into())
            }
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

#[cfg(test)]
mod tests {
    use crate::IntegerExpr;

    use super::*;

    #[test]
    fn test_parse_variable_expr() {
        assert_eq!(
            parse_expr(b"x").unwrap(),
            LocalVariableExpr {
                name: "x".to_owned(),
                type_annotation: None,
            }
            .into()
        );
        assert_eq!(
            parse_expr(b"x @ Integer").unwrap(),
            LocalVariableExpr {
                name: "x".to_owned(),
                type_annotation: Some(IntegerType {}.into()),
            }
            .into()
        )
    }

    #[test]
    fn test_parse_integer_expr() {
        assert_eq!(parse_expr(b"42").unwrap(), IntegerExpr { value: 42 }.into())
    }

    #[test]
    fn test_parse_assignment_expr() {
        assert_eq!(
            parse_expr(b"x = 42").unwrap(),
            WriteExpr {
                lhs: Box::new(
                    LocalVariableWriteTarget {
                        name: "x".to_owned(),
                        type_annotation: None,
                    }
                    .into()
                ),
                rhs: Box::new(IntegerExpr { value: 42 }.into()),
            }
            .into()
        );
        assert_eq!(
            parse_expr(b"x @ Integer = 42").unwrap(),
            WriteExpr {
                lhs: Box::new(
                    LocalVariableWriteTarget {
                        name: "x".to_owned(),
                        type_annotation: Some(IntegerType {}.into()),
                    }
                    .into()
                ),
                rhs: Box::new(IntegerExpr { value: 42 }.into()),
            }
            .into()
        )
    }

    #[test]
    fn test_parse_integer_type() {
        assert_eq!(parse_type(b"Integer").unwrap(), IntegerType {}.into())
    }
}
