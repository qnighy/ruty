use anyhow::Error;

use crate::ast::{
    CodeRange, Expr, IntegerExpr, IntegerType, LocalVariableExpr, LocalVariableWriteTarget,
    StringType, Type, TypeAnnotation, WriteExpr, WriteTarget,
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
                        range: expr.range,
                        name: expr.name,
                        type_annotation: expr.type_annotation,
                    }
                    .into(),
                    _ => {
                        return Err(Error::msg("non-assignable expression"));
                    }
                };
                let expr: Expr = WriteExpr {
                    range: spanned(*lhs.range(), *rhs.range()),
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
                            let ty_range = *ty.range();
                            e.type_annotation = Some(TypeAnnotation {
                                range: spanned(token.range, ty_range),
                                type_: ty,
                            });
                            e.range = spanned(e.range, ty_range);
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
                let s = std::str::from_utf8(&self.input[token.range.range()])?;
                Ok(LocalVariableExpr {
                    range: token.range,
                    name: s.to_owned(),
                    type_annotation: None,
                }
                .into())
            }
            TokenKind::Integer => Ok(IntegerExpr {
                range: token.range,
                value: std::str::from_utf8(&self.input[token.range.range()])?
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
                let s = std::str::from_utf8(&self.input[token.range.range()])?;
                match s {
                    "Integer" => Ok(IntegerType { range: token.range }.into()),
                    "String" => Ok(StringType { range: token.range }.into()),
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct Token {
    kind: TokenKind,
    range: CodeRange,
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

fn spanned(range1: CodeRange, range2: CodeRange) -> CodeRange {
    CodeRange {
        start: range1.start,
        end: range2.end,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{pos_in, IntegerExpr};

    use super::*;

    #[test]
    fn test_parse_variable_expr() {
        let src = b"x";
        assert_eq!(
            parse_expr(src).unwrap(),
            LocalVariableExpr {
                range: pos_in(src, b"x"),
                name: "x".to_owned(),
                type_annotation: None,
            }
            .into()
        );
        let src = b"x @ Integer";
        assert_eq!(
            parse_expr(src).unwrap(),
            LocalVariableExpr {
                range: pos_in(src, b"x @ Integer"),
                name: "x".to_owned(),
                type_annotation: Some(TypeAnnotation {
                    range: pos_in(src, b"@ Integer"),
                    type_: IntegerType {
                        range: pos_in(src, b"Integer")
                    }
                    .into()
                }),
            }
            .into()
        )
    }

    #[test]
    fn test_parse_integer_expr() {
        let src = b"42";
        assert_eq!(
            parse_expr(src).unwrap(),
            IntegerExpr {
                range: pos_in(src, b"42"),
                value: 42,
            }
            .into()
        )
    }

    #[test]
    fn test_parse_assignment_expr() {
        let src = b"x = 42";
        assert_eq!(
            parse_expr(src).unwrap(),
            WriteExpr {
                range: pos_in(src, b"x = 42"),
                lhs: Box::new(
                    LocalVariableWriteTarget {
                        range: pos_in(src, b"x"),
                        name: "x".to_owned(),
                        type_annotation: None,
                    }
                    .into()
                ),
                rhs: Box::new(
                    IntegerExpr {
                        range: pos_in(src, b"42"),
                        value: 42
                    }
                    .into()
                ),
            }
            .into()
        );
        let src = b"x @ Integer = 42";
        assert_eq!(
            parse_expr(src).unwrap(),
            WriteExpr {
                range: pos_in(src, b"x @ Integer = 42"),
                lhs: Box::new(
                    LocalVariableWriteTarget {
                        range: pos_in(src, b"x @ Integer"),
                        name: "x".to_owned(),
                        type_annotation: Some(TypeAnnotation {
                            range: pos_in(src, b"@ Integer"),
                            type_: IntegerType {
                                range: pos_in(src, b"Integer"),
                            }
                            .into()
                        }),
                    }
                    .into()
                ),
                rhs: Box::new(
                    IntegerExpr {
                        range: pos_in(src, b"42"),
                        value: 42,
                    }
                    .into()
                ),
            }
            .into()
        )
    }

    #[test]
    fn test_parse_integer_type() {
        let src = b"Integer";
        assert_eq!(
            parse_type(src).unwrap(),
            IntegerType {
                range: pos_in(src, b"Integer"),
            }
            .into()
        )
    }
}
