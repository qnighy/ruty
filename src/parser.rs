mod lexer;

use std::borrow::Cow;

use crate::{
    ast::{
        CodeRange, ErrorExpr, ErrorType, ErrorWriteTarget, Expr, IntegerExpr, IntegerType,
        LocalVariableExpr, LocalVariableWriteTarget, Program, Stmt, StmtList, StringType, Type,
        TypeAnnotation, WriteExpr, WriteTarget,
    },
    Diagnostic,
};
use lexer::{Lexer, LexerState, Token, TokenKind};

pub fn parse(diag: &mut Vec<Diagnostic>, input: &[u8]) -> Program {
    let mut parser = Parser::new(input);
    let program = parser.parse_whole_program(diag);
    program
}

pub fn parse_expr(diag: &mut Vec<Diagnostic>, input: &[u8]) -> Expr {
    let mut parser = Parser::new(input);
    let expr = parser.parse_whole_expr(diag);
    expr
}

pub fn parse_type(diag: &mut Vec<Diagnostic>, input: &[u8]) -> Type {
    let mut parser = Parser::new(input);
    let ty = parser.parse_whole_type(diag);
    ty
}

#[derive(Debug)]
struct Parser<'a> {
    lexer: Lexer<'a>,
    next_token: Option<Token>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a [u8]) -> Self {
        Self {
            lexer: Lexer::new(input),
            next_token: None,
        }
    }

    fn input(&self) -> &'a [u8] {
        self.lexer.input()
    }

    fn parse_whole_program(&mut self, diag: &mut Vec<Diagnostic>) -> Program {
        let stmt_list = self.parse_stmt_list(diag);
        let token = self.fill_token(diag, LexerState::End);
        match token.kind {
            TokenKind::EOF => {}
            _ => {
                diag.push(Diagnostic {
                    range: token.range,
                    message: format!("unexpected token"),
                });
            }
        }
        Program {
            range: CodeRange {
                start: 0,
                end: self.input().len(),
            },
            stmt_list,
        }
    }

    fn parse_stmt_list(&mut self, diag: &mut Vec<Diagnostic>) -> StmtList {
        let mut semi_prefix = Vec::<CodeRange>::new();
        let mut stmts = Vec::<Stmt>::new();
        loop {
            let token = self.fill_token(diag, LexerState::Begin);
            match token.kind {
                TokenKind::EOF => break,
                TokenKind::Semicolon => {
                    self.bump();
                    if let Some(last_stmt) = stmts.last_mut() {
                        last_stmt.range = spanned(last_stmt.range, token.range);
                        last_stmt.semi.push(token.range);
                    } else {
                        semi_prefix.push(token.range);
                    }
                }
                _ => {
                    let expr = self.parse_expr_lv_assignment(diag);
                    stmts.push(Stmt {
                        range: *expr.range(),
                        expr,
                        semi: Vec::new(),
                    });
                }
            }
        }
        StmtList {
            range: CodeRange {
                start: semi_prefix.first().map_or(0, |r| r.start),
                end: stmts.last().map_or(0, |stmt| stmt.range.end),
            },
            semi_prefix,
            stmts,
        }
    }

    fn parse_whole_expr(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let expr = self.parse_expr_lv_assignment(diag);
        let token = self.fill_token(diag, LexerState::End);
        match token.kind {
            TokenKind::EOF => {}
            _ => {
                diag.push(Diagnostic {
                    range: token.range,
                    message: format!("unexpected token"),
                });
            }
        }
        expr
    }

    fn parse_expr_lv_assignment(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let expr = self.parse_expr_lv_type_annotated(diag);
        let token = self.fill_token(diag, LexerState::End);
        match token.kind {
            TokenKind::Eq => {
                self.bump();
                let rhs = self.parse_expr_lv_assignment(diag);
                let lhs: WriteTarget = match expr {
                    Expr::LocalVariable(expr) => LocalVariableWriteTarget {
                        range: expr.range,
                        name: expr.name,
                        type_annotation: expr.type_annotation,
                    }
                    .into(),
                    _ => {
                        diag.push(Diagnostic {
                            range: *expr.range(),
                            message: format!("non-assignable expression"),
                        });
                        ErrorWriteTarget {
                            range: *expr.range(),
                        }
                        .into()
                    }
                };
                let expr: Expr = WriteExpr {
                    range: spanned(*lhs.range(), *rhs.range()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .into();
                return expr;
            }
            _ => {}
        }
        expr
    }

    fn parse_expr_lv_type_annotated(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_primary(diag);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::At => {
                    self.bump();
                    let ty = self.parse_type(diag);
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
                            diag.push(Diagnostic {
                                range: spanned(token.range, *ty.range()),
                                message: format!("non-annotatable expression"),
                            });
                            expr
                        }
                    };
                }
                _ => break,
            }
        }

        expr
    }

    fn parse_expr_lv_primary(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let token = self.fill_token(diag, LexerState::Begin);
        match token.kind {
            TokenKind::Identifier => {
                self.bump();
                let s = self.select(token.range);
                LocalVariableExpr {
                    range: token.range,
                    name: s.into_owned(),
                    type_annotation: None,
                }
                .into()
            }
            TokenKind::Integer => {
                self.bump();
                IntegerExpr {
                    range: token.range,
                    value: self.select(token.range).parse().unwrap(),
                }
                .into()
            }
            _ => {
                self.bump();
                diag.push(Diagnostic {
                    range: token.range,
                    message: format!("unexpected token"),
                });
                ErrorExpr { range: token.range }.into()
            }
        }
    }

    fn parse_whole_type(&mut self, diag: &mut Vec<Diagnostic>) -> Type {
        let ty = self.parse_type(diag);
        let token = self.fill_token(diag, LexerState::End);
        match token.kind {
            TokenKind::EOF => {}
            _ => {
                diag.push(Diagnostic {
                    range: token.range,
                    message: format!("unexpected token"),
                });
            }
        }
        ty
    }

    fn parse_type(&mut self, diag: &mut Vec<Diagnostic>) -> Type {
        let token = self.fill_token(diag, LexerState::Begin);
        self.bump();
        match token.kind {
            TokenKind::Const => {
                let s = self.select(token.range);
                match &*s {
                    "Integer" => IntegerType { range: token.range }.into(),
                    "String" => StringType { range: token.range }.into(),
                    _ => {
                        diag.push(Diagnostic {
                            range: token.range,
                            message: format!("unexpected token"),
                        });
                        ErrorType { range: token.range }.into()
                    }
                }
            }
            _ => {
                diag.push(Diagnostic {
                    range: token.range,
                    message: format!("unexpected token"),
                });
                ErrorType { range: token.range }.into()
            }
        }
    }

    fn fill_token(&mut self, diag: &mut Vec<Diagnostic>, state: LexerState) -> Token {
        if let Some(token) = self.next_token {
            token
        } else {
            let token = self.lexer.lex(diag, state);
            self.next_token = Some(token);
            token
        }
    }

    fn bump(&mut self) {
        if self.next_token.is_some() {
            self.next_token = None;
        } else {
            panic!("bump: no token to bump");
        }
    }

    // fn peek(&mut self) -> Token {
    //     if let Some(token) = self.next_token {
    //         token
    //     } else {
    //         panic!("peek: token not filled yet");
    //     }
    // }

    fn select(&self, range: CodeRange) -> Cow<'a, str> {
        String::from_utf8_lossy(&self.input()[range.range()])
    }
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

    fn p_expr(input: &[u8]) -> (Expr, Vec<Diagnostic>) {
        let mut diag = Vec::new();
        let expr = parse_expr(&mut diag, input);
        (expr, diag)
    }

    fn p_type(input: &[u8]) -> (Type, Vec<Diagnostic>) {
        let mut diag = Vec::new();
        let ty = parse_type(&mut diag, input);
        (ty, diag)
    }

    #[test]
    fn test_parse_variable_expr() {
        let src = b"x";
        assert_eq!(
            p_expr(src),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x"),
                    name: "x".to_owned(),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
        let src = b"x @ Integer";
        assert_eq!(
            p_expr(src),
            (
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
                .into(),
                vec![],
            )
        )
    }

    #[test]
    fn test_parse_integer_expr() {
        let src = b"42";
        assert_eq!(
            p_expr(src),
            (
                IntegerExpr {
                    range: pos_in(src, b"42"),
                    value: 42,
                }
                .into(),
                vec![],
            )
        )
    }

    #[test]
    fn test_parse_assignment_expr() {
        let src = b"x = 42";
        assert_eq!(
            p_expr(src),
            (
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
                .into(),
                vec![],
            )
        );
        let src = b"x @ Integer = 42";
        assert_eq!(
            p_expr(src),
            (
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
                .into(),
                vec![],
            )
        )
    }

    #[test]
    fn test_parse_integer_type() {
        let src = b"Integer";
        assert_eq!(
            p_type(src),
            (
                IntegerType {
                    range: pos_in(src, b"Integer"),
                }
                .into(),
                vec![],
            )
        )
    }
}
