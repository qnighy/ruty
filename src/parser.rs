mod lexer;

use std::borrow::Cow;

use crate::{
    ast::{
        CodeRange, ErrorExpr, ErrorType, ErrorWriteTarget, Expr, FalseExpr, IntegerExpr,
        IntegerType, LocalVariableExpr, LocalVariableWriteTarget, NilExpr, Paren, Program,
        Semicolon, SemicolonKind, SeqExpr, SeqParen, SeqParenKind, Stmt, StmtList, StringType,
        TrueExpr, Type, TypeAnnotation, WriteExpr, WriteTarget,
    },
    encoding::EStrRef,
    Diagnostic,
};
use lexer::{Lexer, LexerState, Token, TokenKind};

pub fn parse(diag: &mut Vec<Diagnostic>, input: EStrRef<'_>) -> Program {
    let mut parser = Parser::new(input);
    let program = parser.parse_whole_program(diag);
    program
}

pub fn parse_expr(diag: &mut Vec<Diagnostic>, input: EStrRef<'_>) -> Expr {
    let mut parser = Parser::new(input);
    let expr = parser.parse_whole_expr(diag);
    expr
}

pub fn parse_type(diag: &mut Vec<Diagnostic>, input: EStrRef<'_>) -> Type {
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
    fn new(input: EStrRef<'a>) -> Self {
        Self {
            lexer: Lexer::new(input),
            next_token: None,
        }
    }

    // fn input(&self) -> EStrRef<'a> {
    //     self.lexer.input()
    // }

    fn bytes(&self) -> &'a [u8] {
        self.lexer.bytes()
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
                end: self.bytes().len(),
            },
            stmt_list,
        }
    }

    fn parse_stmt_list(&mut self, diag: &mut Vec<Diagnostic>) -> StmtList {
        let start_pos = self.lexer.pos();
        let mut semi_prefix = Vec::<Semicolon>::new();
        let mut stmts = Vec::<Stmt>::new();
        loop {
            let token = self.fill_token(diag, LexerState::Begin);
            match token.kind {
                TokenKind::EOF
                | TokenKind::RParen
                | TokenKind::RBrace
                | TokenKind::RBracket
                | TokenKind::KeywordEnd => break,
                TokenKind::Semicolon | TokenKind::Newline => {
                    self.bump();
                    let kind = match token.kind {
                        TokenKind::Semicolon => SemicolonKind::Semicolon,
                        TokenKind::Newline => SemicolonKind::Newline,
                        _ => unreachable!(),
                    };
                    if let Some(last_stmt) = stmts.last_mut() {
                        last_stmt.range = spanned(last_stmt.range, token.range);
                        last_stmt.semi.push(Semicolon {
                            range: token.range,
                            kind,
                        });
                    } else {
                        semi_prefix.push(Semicolon {
                            range: token.range,
                            kind,
                        });
                    }
                }
                _ => {
                    let expr = self.parse_expr_lv_assignment(diag);
                    stmts.push(Stmt {
                        range: *expr.outer_range(),
                        expr,
                        semi: Vec::new(),
                    });
                }
            }
        }
        let start = if let Some(first_semi) = semi_prefix.first() {
            first_semi.range.start
        } else if let Some(first_stmt) = stmts.first() {
            first_stmt.range.start
        } else {
            start_pos
        };
        let end = if let Some(last_stmt) = stmts.last() {
            last_stmt.range.end
        } else if let Some(last_semi) = semi_prefix.last() {
            last_semi.range.end
        } else {
            start_pos
        };
        StmtList {
            range: CodeRange { start, end },
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
                            range: *expr.outer_range(),
                            message: format!("non-assignable expression"),
                        });
                        ErrorWriteTarget {
                            range: *expr.outer_range(),
                        }
                        .into()
                    }
                };
                let expr: Expr = WriteExpr {
                    range: spanned(*lhs.range(), *rhs.outer_range()),
                    parens: Vec::new(),
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
            TokenKind::KeywordNil => {
                self.bump();
                NilExpr {
                    range: token.range,
                    parens: Vec::new(),
                }
                .into()
            }
            TokenKind::KeywordFalse => {
                self.bump();
                FalseExpr {
                    range: token.range,
                    parens: Vec::new(),
                }
                .into()
            }
            TokenKind::KeywordTrue => {
                self.bump();
                TrueExpr {
                    range: token.range,
                    parens: Vec::new(),
                }
                .into()
            }
            TokenKind::Identifier => {
                self.bump();
                let s = self.select(token.range);
                LocalVariableExpr {
                    range: token.range,
                    parens: Vec::new(),
                    name: s.into_owned(),
                    type_annotation: None,
                }
                .into()
            }
            TokenKind::Integer => {
                self.bump();
                IntegerExpr {
                    range: token.range,
                    parens: Vec::new(),
                    value: self.select(token.range).parse().unwrap(),
                }
                .into()
            }
            TokenKind::LParen => {
                let open_range = token.range;
                self.bump();
                let stmt_list = self.parse_stmt_list(diag);
                let close_range = loop {
                    let token = self.fill_token(diag, LexerState::End);
                    match token.kind {
                        TokenKind::RParen => {
                            self.bump();
                            break token.range;
                        }
                        TokenKind::EOF => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("unexpected end of file"),
                            });
                            break token.range;
                        }
                        _ => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("unexpected token"),
                            });
                        }
                    }
                };
                let is_simple = stmt_list.semi_prefix.is_empty()
                    && stmt_list.stmts.len() == 1
                    && stmt_list.stmts[0]
                        .semi
                        .iter()
                        .all(|s| s.kind == SemicolonKind::Newline);
                if is_simple {
                    let mut expr = { stmt_list }.stmts.pop().unwrap().expr;
                    expr.parens_mut().push(Paren {
                        range: spanned(open_range, close_range),
                        open_range,
                        close_range,
                    });
                    expr
                } else {
                    Expr::Seq(SeqExpr {
                        range: spanned(open_range, close_range),
                        parens: Vec::new(),
                        paren: SeqParen {
                            kind: SeqParenKind::Paren,
                            open_range,
                            close_range,
                        },
                        stmt_list,
                    })
                }
            }
            _ => {
                self.bump();
                diag.push(Diagnostic {
                    range: token.range,
                    message: format!("unexpected token"),
                });
                ErrorExpr {
                    range: token.range,
                    parens: Vec::new(),
                }
                .into()
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
        String::from_utf8_lossy(&self.bytes()[range.range()])
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
    #[allow(unused)]
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::ast::{pos_in, IntegerExpr};

    use super::*;

    fn p_program(input: EStrRef<'_>) -> (Program, Vec<Diagnostic>) {
        let mut diag = Vec::new();
        let program = parse(&mut diag, input);
        (program, diag)
    }

    fn p_expr(input: EStrRef<'_>) -> (Expr, Vec<Diagnostic>) {
        let mut diag = Vec::new();
        let expr = parse_expr(&mut diag, input);
        (expr, diag)
    }

    fn p_type(input: EStrRef<'_>) -> (Type, Vec<Diagnostic>) {
        let mut diag = Vec::new();
        let ty = parse_type(&mut diag, input);
        (ty, diag)
    }

    #[test]
    fn test_parse_toplevel_stmts() {
        let src = EStrRef::from("x; y\nz");
        assert_eq!(
            p_program(src),
            (
                Program {
                    range: pos_in(src, b"x; y\nz"),
                    stmt_list: StmtList {
                        range: pos_in(src, b"x; y\nz"),
                        semi_prefix: vec![],
                        stmts: vec![
                            Stmt {
                                range: pos_in(src, b"x;"),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"x"),
                                    parens: vec![],
                                    name: "x".to_owned(),
                                    type_annotation: None,
                                }
                                .into(),
                                semi: vec![Semicolon {
                                    range: pos_in(src, b";"),
                                    kind: SemicolonKind::Semicolon
                                }],
                            },
                            Stmt {
                                range: pos_in(src, b"y\n"),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"y"),
                                    parens: vec![],
                                    name: "y".to_owned(),
                                    type_annotation: None,
                                }
                                .into(),
                                semi: vec![Semicolon {
                                    range: pos_in(src, b"\n"),
                                    kind: SemicolonKind::Newline
                                }],
                            },
                            Stmt {
                                range: pos_in(src, b"z"),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"z"),
                                    parens: vec![],
                                    name: "z".to_owned(),
                                    type_annotation: None,
                                }
                                .into(),
                                semi: vec![],
                            },
                        ],
                    },
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_parenthesized_expr() {
        let src = EStrRef::from("(x)");
        assert_eq!(
            p_expr(src),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x"),
                    parens: vec![Paren {
                        range: pos_in(src, b"(x)"),
                        open_range: pos_in(src, b"("),
                        close_range: pos_in(src, b")"),
                    }],
                    name: "x".to_owned(),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("(\nx\n)");
        assert_eq!(
            p_expr(src),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x"),
                    parens: vec![Paren {
                        range: pos_in(src, b"(\nx\n)"),
                        open_range: pos_in(src, b"("),
                        close_range: pos_in(src, b")"),
                    }],
                    name: "x".to_owned(),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_parenthesized_seq_expr() {
        let src = EStrRef::from("(x;)");
        assert_eq!(
            p_expr(src),
            (
                SeqExpr {
                    range: pos_in(src, b"(x;)"),
                    parens: vec![],
                    paren: SeqParen {
                        kind: SeqParenKind::Paren,
                        open_range: pos_in(src, b"("),
                        close_range: pos_in(src, b")"),
                    },
                    stmt_list: StmtList {
                        range: pos_in(src, b"x;"),
                        semi_prefix: vec![],
                        stmts: vec![Stmt {
                            range: pos_in(src, b"x;"),
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"x"),
                                parens: vec![],
                                name: "x".to_owned(),
                                type_annotation: None,
                            }
                            .into(),
                            semi: vec![Semicolon {
                                range: pos_in(src, b";"),
                                kind: SemicolonKind::Semicolon
                            }],
                        }]
                    },
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("(;x)");
        assert_eq!(
            p_expr(src),
            (
                SeqExpr {
                    range: pos_in(src, b"(;x)"),
                    parens: vec![],
                    paren: SeqParen {
                        kind: SeqParenKind::Paren,
                        open_range: pos_in(src, b"("),
                        close_range: pos_in(src, b")"),
                    },
                    stmt_list: StmtList {
                        range: pos_in(src, b";x"),
                        semi_prefix: vec![Semicolon {
                            range: pos_in(src, b";"),
                            kind: SemicolonKind::Semicolon
                        }],
                        stmts: vec![Stmt {
                            range: pos_in(src, b"x"),
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"x"),
                                parens: vec![],
                                name: "x".to_owned(),
                                type_annotation: None,
                            }
                            .into(),
                            semi: vec![],
                        }]
                    },
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("(x\ny)");
        assert_eq!(
            p_expr(src),
            (
                SeqExpr {
                    range: pos_in(src, b"(x\ny)"),
                    parens: vec![],
                    paren: SeqParen {
                        kind: SeqParenKind::Paren,
                        open_range: pos_in(src, b"("),
                        close_range: pos_in(src, b")"),
                    },
                    stmt_list: StmtList {
                        range: pos_in(src, b"x\ny"),
                        semi_prefix: vec![],
                        stmts: vec![
                            Stmt {
                                range: pos_in(src, b"x\n"),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"x"),
                                    parens: vec![],
                                    name: "x".to_owned(),
                                    type_annotation: None,
                                }
                                .into(),
                                semi: vec![Semicolon {
                                    range: pos_in(src, b"\n"),
                                    kind: SemicolonKind::Newline
                                }],
                            },
                            Stmt {
                                range: pos_in(src, b"y"),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"y"),
                                    parens: vec![],
                                    name: "y".to_owned(),
                                    type_annotation: None,
                                }
                                .into(),
                                semi: vec![],
                            }
                        ]
                    },
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_nil_expr() {
        let src = EStrRef::from("nil");
        assert_eq!(
            p_expr(src),
            (
                NilExpr {
                    range: pos_in(src, b"nil"),
                    parens: vec![],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_false_true_expr() {
        let src = EStrRef::from("false");
        assert_eq!(
            p_expr(src),
            (
                FalseExpr {
                    range: pos_in(src, b"false"),
                    parens: vec![],
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("true");
        assert_eq!(
            p_expr(src),
            (
                TrueExpr {
                    range: pos_in(src, b"true"),
                    parens: vec![],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_variable_expr() {
        let src = EStrRef::from("x");
        assert_eq!(
            p_expr(src),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x"),
                    parens: vec![],
                    name: "x".to_owned(),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("x @ Integer");
        assert_eq!(
            p_expr(src),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x @ Integer"),
                    parens: vec![],
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
        let src = EStrRef::from("42");
        assert_eq!(
            p_expr(src),
            (
                IntegerExpr {
                    range: pos_in(src, b"42"),
                    parens: vec![],
                    value: 42,
                }
                .into(),
                vec![],
            )
        )
    }

    #[test]
    fn test_parse_assignment_expr() {
        let src = EStrRef::from("x = 42");
        assert_eq!(
            p_expr(src),
            (
                WriteExpr {
                    range: pos_in(src, b"x = 42"),
                    parens: vec![],
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
                            parens: vec![],
                            value: 42
                        }
                        .into()
                    ),
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("x @ Integer = 42");
        assert_eq!(
            p_expr(src),
            (
                WriteExpr {
                    range: pos_in(src, b"x @ Integer = 42"),
                    parens: vec![],
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
                            parens: vec![],
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
        let src = EStrRef::from("Integer");
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
