mod lexer;

use std::{collections::HashMap, mem::take};

use crate::{
    ast::{
        AndExpr, Arg, ArgList, CallExpr, CallStyle, CodeRange, ConstExpr, ConstReceiver, ErrorExpr,
        ErrorType, ErrorWriteTarget, Expr, ExprArg, FalseExpr, FalseType, IfExpr, ImplicitParen,
        IntegerType, InterpolationContent, LocalVariableExpr, LocalVariableWriteTarget, NilExpr,
        NilStyle, NilType, NumericExpr, OrExpr, Paren, ParenParen, Program, RegexpExpr, RegexpType,
        SelfExpr, SeqExpr, SourceEncodingExpr, SourceFileExpr, SourceLineExpr, Stmt, StmtSep,
        StmtSepKind, StringContent, StringExpr, StringType, TextContent, TrueExpr, TrueType, Type,
        TypeAnnotation, UntilExpr, WhileExpr, WriteExpr, WriteTarget, XStringExpr, DUMMY_RANGE,
    },
    encoding::EStrRef,
    Diagnostic, EString, Encoding,
};
use lexer::{
    BinOpKind, Lexer, LexerState, NumericToken, StringVariant, Token, TokenKind, UnOpKind,
};

pub fn parse(diag: &mut Vec<Diagnostic>, input: EStrRef<'_>, locals: &[EString]) -> Program {
    let mut parser = Parser::new(input);
    let program = parser.parse_whole_program(diag, locals);
    program
}

pub fn parse_expr(diag: &mut Vec<Diagnostic>, input: EStrRef<'_>, locals: &[EString]) -> Expr {
    let mut parser = Parser::new(input);
    let expr = parser.parse_whole_expr(diag, locals);
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
}

impl<'a> Parser<'a> {
    fn new(input: EStrRef<'a>) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }

    fn input(&self) -> EStrRef<'a> {
        self.lexer.input()
    }

    fn bytes(&self) -> &'a [u8] {
        self.lexer.bytes()
    }

    fn fix_input_encoding(&mut self, diag: &mut Vec<Diagnostic>) {
        let enc = self.lexer.input().encoding();
        if !enc.is_ascii_compatible() || !enc.is_stateless() {
            diag.push(Diagnostic {
                range: CodeRange { start: 0, end: 0 },
                message: format!("invalid source encoding: {}", enc.name()),
            });
            self.lexer = Lexer::new(EStrRef::from_bytes(
                self.lexer.bytes(),
                Encoding::ASCII_8BIT,
            ));
        }
    }

    fn parse_whole_program(&mut self, diag: &mut Vec<Diagnostic>, locals: &[EString]) -> Program {
        self.fix_input_encoding(diag);
        let mut lv = LVCtx::new();
        for local in locals {
            lv.push(local.clone());
        }
        let lv_checkpoint = lv.checkpoint();
        let first_token = self.lexer.lex(diag, LexerState::Begin);
        let (body, last_token) = self.parse_stmt_list(
            diag,
            first_token,
            &mut lv,
            BailCtx {
                end_style: EndStyle::EOF,
                indent: 0,
                ..Default::default()
            },
        );
        match last_token.kind {
            TokenKind::EOF => {}
            _ => {
                diag.push(Diagnostic {
                    range: last_token.range,
                    message: format!("unexpected token"),
                });
            }
        }
        let locals = lv.commit(lv_checkpoint);
        Program {
            range: CodeRange {
                start: 0,
                end: self.bytes().len(),
            },
            locals,
            body,
        }
    }

    fn parse_stmt_list(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
        bail: BailCtx,
    ) -> (Expr, Token) {
        let mut separator_prefix = Vec::<StmtSep>::new();
        let mut stmts = Vec::<Stmt>::new();
        let mut range = DUMMY_RANGE;
        let mut last_token = first_token;
        loop {
            let token = last_token;
            match token.kind {
                _ if bail.should_bail(&token) => {
                    last_token = token;
                    break;
                }
                TokenKind::Semicolon | TokenKind::Newline => {
                    range |= token.range;
                    last_token = self.lexer.lex(diag, LexerState::Begin);
                    let kind = match token.kind {
                        TokenKind::Semicolon => StmtSepKind::Semicolon,
                        TokenKind::Newline => StmtSepKind::Newline,
                        _ => unreachable!(),
                    };
                    if let Some(last_stmt) = stmts.last_mut() {
                        last_stmt.range = last_stmt.range | token.range;
                        last_stmt.separator_suffix.push(StmtSep {
                            range: token.range,
                            kind,
                        });
                    } else {
                        separator_prefix.push(StmtSep {
                            range: token.range,
                            kind,
                        });
                    }
                }
                _ => {
                    let (expr, token_after_expr) =
                        self.parse_expr_lv_spelled_cond_infix(diag, token, lv);
                    range |= *expr.outer_range();
                    let separator_prefix = take(&mut separator_prefix);
                    stmts.push(Stmt {
                        range: *expr.outer_range(),
                        expr,
                        separator_prefix,
                        separator_suffix: Vec::new(),
                    });
                    last_token = token_after_expr;
                }
            }
        }
        if stmts.is_empty() {
            // Empty case such as `()` or `(;)`.
            // In these cases, interpret them like `(nil)` or `(nil;)`.
            let separator_suffix = take(&mut separator_prefix);
            stmts.push(Stmt {
                range,
                expr: NilExpr {
                    range: DUMMY_RANGE,
                    parens: Vec::new(),

                    style: NilStyle::Implicit,
                }
                .into(),
                separator_prefix: Vec::new(),
                separator_suffix,
            });
        }
        let expr = if stmts.len() == 1 {
            let stmt = stmts.swap_remove(0);
            let mut expr = stmt.expr;
            if !stmt.separator_prefix.is_empty() || !stmt.separator_suffix.is_empty() {
                expr.parens_mut().push(
                    ImplicitParen {
                        range: stmt.range,
                        separator_prefix: stmt.separator_prefix,
                        separator_suffix: stmt.separator_suffix,
                    }
                    .into(),
                );
            }
            expr
        } else {
            SeqExpr {
                range,
                parens: Vec::new(),

                statements: stmts,
            }
            .into()
        };
        (expr, last_token)
    }

    fn parse_whole_expr(&mut self, diag: &mut Vec<Diagnostic>, locals: &[EString]) -> Expr {
        self.fix_input_encoding(diag);
        let mut lv = LVCtx::new();
        for local in locals {
            lv.push(local.clone());
        }
        let first_token = self.lexer.lex(diag, LexerState::Begin);
        let (expr, last_token) = self.parse_expr_lv_spelled_cond_infix(diag, first_token, &mut lv);
        match last_token.kind {
            TokenKind::EOF => {}
            _ => {
                diag.push(Diagnostic {
                    range: last_token.range,
                    message: format!("unexpected token"),
                });
            }
        }
        expr
    }

    fn parse_expr_lv_spelled_cond_infix(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (Expr, Token) {
        let (mut lhs, mut last_token) = self.parse_expr_lv_spelled_and_or(diag, first_token, lv);
        let mut count = 0;
        loop {
            let token = last_token;
            match token.kind {
                TokenKind::KeywordIfInfix
                | TokenKind::KeywordUnlessInfix
                | TokenKind::KeywordWhileInfix
                | TokenKind::KeywordUntilInfix => {
                    count += 1;
                    if count > 1 {
                        diag.push(Diagnostic {
                            range: token.range,
                            message: format!("these operators cannot be chained"),
                        });
                    }
                    let rhs_first_token = self.lexer.lex(diag, LexerState::Begin);
                    let (rhs, token_after_rhs) =
                        self.parse_expr_lv_spelled_and_or(diag, rhs_first_token, lv);
                    lhs = match token.kind {
                        TokenKind::KeywordIfInfix => IfExpr {
                            range: *lhs.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            cond: Box::new(rhs),
                            then: Box::new(lhs),
                            else_: Box::new(
                                NilExpr {
                                    range: DUMMY_RANGE,
                                    parens: Vec::new(),

                                    style: NilStyle::Implicit,
                                }
                                .into(),
                            ),
                        }
                        .into(),
                        TokenKind::KeywordUnlessInfix => IfExpr {
                            range: *lhs.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            cond: Box::new(rhs),
                            then: Box::new(
                                NilExpr {
                                    range: DUMMY_RANGE,
                                    parens: Vec::new(),

                                    style: NilStyle::Implicit,
                                }
                                .into(),
                            ),
                            else_: Box::new(lhs),
                        }
                        .into(),
                        TokenKind::KeywordWhileInfix => WhileExpr {
                            range: *lhs.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            cond: Box::new(rhs),
                            body: Box::new(lhs),
                        }
                        .into(),
                        TokenKind::KeywordUntilInfix => UntilExpr {
                            range: *lhs.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            cond: Box::new(rhs),
                            body: Box::new(lhs),
                        }
                        .into(),
                        _ => unreachable!(),
                    };
                    last_token = token_after_rhs;
                }
                _ => {
                    last_token = token;
                    break;
                }
            }
        }
        (lhs, last_token)
    }

    fn parse_expr_lv_spelled_and_or(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (Expr, Token) {
        let (mut lhs, mut last_token) = self.parse_expr_lv_cmd(diag, first_token, lv);
        loop {
            let token = last_token;
            match token.kind {
                TokenKind::KeywordAnd | TokenKind::KeywordOr => {
                    let rhs_first_token = self.lexer.lex(diag, LexerState::Begin);
                    let (rhs, token_after_rhs) = self.parse_expr_lv_cmd(diag, rhs_first_token, lv);
                    lhs = match token.kind {
                        TokenKind::KeywordAnd => AndExpr {
                            range: *lhs.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }
                        .into(),
                        TokenKind::KeywordOr => OrExpr {
                            range: *lhs.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }
                        .into(),
                        _ => unreachable!(),
                    };
                    last_token = token_after_rhs;
                }
                _ => {
                    last_token = token;
                    break;
                }
            }
        }
        (lhs, last_token)
    }

    fn parse_expr_lv_cmd(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (Expr, Token) {
        let (lhs, token_after_lhs) =
            self.parse_expr_lv_assignment(diag, first_token, lv, PrecCtx::default());
        if is_cmdarg_begin(&token_after_lhs) {
            let method_range = token_after_lhs.range;
            let (args, token_after_args) = self.parse_cmd_args(diag, token_after_lhs, lv);
            let lhs = lhs.callify();
            let expr = match lhs {
                ExprLike::ArglessCall {
                    range,
                    style,
                    private,
                    optional,
                    receiver,
                    method,
                    method_range,
                } => CallExpr {
                    range: range | *args.outer_range(),
                    parens: Vec::new(),

                    style,
                    private,
                    optional,
                    receiver,
                    method,
                    method_range,
                    args,
                }
                .into(),
                ExprLike::NotOp { range } => {
                    if args.args.len() > 1 {
                        diag.push(Diagnostic {
                            range: args.range,
                            message: format!("too many arguments"),
                        });
                    }
                    let args_range = *args.outer_range();
                    let Arg::Expr(arg) = { args.args }.swap_remove(0);
                    let arg = *arg.expr;
                    CallExpr {
                        range: range | args_range,
                        parens: Vec::new(),

                        style: CallStyle::SpelloutUnOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(arg),
                        method: symbol("!"),
                        method_range,
                        args: ArgList {
                            range: DUMMY_RANGE,
                            paren: None,
                            args: vec![],
                        },
                    }
                    .into()
                }
                _ => {
                    diag.push(Diagnostic {
                        range: method_range,
                        message: format!("this expression cannot be called"),
                    });
                    let lhs = lhs.into_expr(diag);
                    CallExpr {
                        range: *lhs.outer_range() | *args.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::CallOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(lhs),
                        method: symbol("call"),
                        method_range,
                        args,
                    }
                    .into()
                }
            };
            (expr, token_after_args)
        } else {
            (lhs.into_expr(diag), token_after_lhs)
        }
    }

    fn parse_expr_lv_assignment(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> (ExprLike, Token) {
        let (lhs, token_after_lhs) = self.parse_expr_lv_tertiary_cond(diag, first_token, lv, prec);
        match token_after_lhs.kind {
            TokenKind::Eq => {
                let lhs: WriteTarget = match lhs {
                    ExprLike::Identifier {
                        range,
                        has_local: _,
                        name,
                    } => LocalVariableWriteTarget {
                        range,
                        name,
                        type_annotation: None,
                    }
                    .into(),
                    ExprLike::Expr(Expr::LocalVariable(expr)) => LocalVariableWriteTarget {
                        range: expr.range,
                        name: expr.name,
                        type_annotation: expr.type_annotation,
                    }
                    .into(),
                    _ => {
                        let lhs = lhs.into_expr(diag);
                        diag.push(Diagnostic {
                            range: *lhs.outer_range(),
                            message: format!("non-assignable expression"),
                        });
                        ErrorWriteTarget {
                            range: *lhs.outer_range(),
                        }
                        .into()
                    }
                };
                match &lhs {
                    WriteTarget::LocalVariable(lhs) => {
                        if !lv.has(&lhs.name) {
                            lv.push(lhs.name.clone());
                        }
                    }
                    _ => {}
                }
                let rhs_first_token = self.lexer.lex(diag, LexerState::Begin);
                let (rhs, token_after_rhs) = self.parse_expr_lv_assignment(
                    diag,
                    rhs_first_token,
                    lv,
                    prec.with_invalid_command(),
                );
                let rhs = rhs.into_expr(diag);
                let expr: Expr = WriteExpr {
                    range: *lhs.range() | *rhs.outer_range(),
                    parens: Vec::new(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .into();
                return (ExprLike::Expr(expr), token_after_rhs);
            }
            _ => {}
        }
        (lhs, token_after_lhs)
    }

    fn parse_expr_lv_tertiary_cond(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> (ExprLike, Token) {
        let (lhs, token_after_lhs) = self.parse_expr_lv_logical_or(diag, first_token, lv, prec);
        match token_after_lhs.kind {
            TokenKind::Question => {
                let mid_first_token = self.lexer.lex(diag, LexerState::Begin);
                // TODO: middle expression precedence needs review
                let (mid, token_after_mid) = self.parse_expr_lv_assignment(
                    diag,
                    mid_first_token,
                    lv,
                    prec.with_invalid_command(),
                );
                let mid = mid.into_expr(diag);
                if !matches!(token_after_mid.kind, TokenKind::Colon) {
                    diag.push(Diagnostic {
                        range: token_after_mid.range,
                        message: format!("expected `:`"),
                    });
                    let lhs_expr = lhs.into_expr(diag);
                    let expr = ExprLike::Expr(
                        IfExpr {
                            range: *lhs_expr.outer_range() | token_after_mid.range,
                            parens: Vec::new(),

                            cond: Box::new(lhs_expr),
                            then: Box::new(mid),
                            else_: Box::new(
                                NilExpr {
                                    range: DUMMY_RANGE,
                                    parens: Vec::new(),

                                    style: NilStyle::Implicit,
                                }
                                .into(),
                            ),
                        }
                        .into(),
                    );
                    return (expr, token_after_mid);
                }
                let rhs_first_token = self.lexer.lex(diag, LexerState::Begin);
                let (rhs, token_after_rhs) = self.parse_expr_lv_tertiary_cond(
                    diag,
                    rhs_first_token,
                    lv,
                    prec.with_invalid_command(),
                );
                let rhs = rhs.into_expr(diag);
                let lhs_expr = lhs.into_expr(diag);
                let expr = ExprLike::Expr(
                    IfExpr {
                        range: *lhs_expr.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        cond: Box::new(lhs_expr),
                        then: Box::new(mid),
                        else_: Box::new(rhs),
                    }
                    .into(),
                );
                (expr, token_after_rhs)
            }
            _ => (lhs, token_after_lhs),
        }
    }

    fn parse_expr_lv_logical_or(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
        _prec: PrecCtx,
    ) -> (ExprLike, Token) {
        StackParser {
            parser: self,
            stack: Vec::new(),
        }
        .parse(diag, first_token, lv)
    }

    fn parse_expr_lv_primary(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (ExprLike, Token) {
        let mut next_token: Option<Token> = None;
        let expr = match first_token.kind {
            TokenKind::KeywordNil => ExprLike::Expr(
                NilExpr {
                    range: first_token.range,
                    parens: Vec::new(),

                    style: NilStyle::Keyword,
                }
                .into(),
            ),
            TokenKind::KeywordFalse => ExprLike::Expr(
                FalseExpr {
                    range: first_token.range,
                    parens: Vec::new(),
                }
                .into(),
            ),
            TokenKind::KeywordTrue => ExprLike::Expr(
                TrueExpr {
                    range: first_token.range,
                    parens: Vec::new(),
                }
                .into(),
            ),
            TokenKind::Identifier => {
                let s = self.select(first_token.range);
                let name = s.to_estring().asciified();
                let has_local = lv.has(&name);
                if has_local {
                    next_token = Some(self.lexer.lex(diag, LexerState::WeakFirstArgument))
                } else {
                    next_token = Some(self.lexer.lex(diag, LexerState::FirstArgument));
                }
                ExprLike::Identifier {
                    range: first_token.range,
                    has_local,
                    name,
                }
            }
            TokenKind::Const => {
                let s = self.select(first_token.range);
                next_token = Some(self.lexer.lex(diag, LexerState::FirstArgument));
                ExprLike::Const {
                    receiver: ConstReceiver::None,
                    private: true,
                    range: first_token.range,
                    name: s.to_estring().asciified(),
                }
            }
            TokenKind::MethodName => {
                let s = self.select(first_token.range);
                next_token = Some(self.lexer.lex(diag, LexerState::FirstArgument));
                ExprLike::ArglessCall {
                    range: first_token.range,
                    style: CallStyle::ImplicitSelf,
                    private: true,
                    optional: false,
                    receiver: Box::new(
                        SelfExpr {
                            range: DUMMY_RANGE,
                            parens: Vec::new(),
                        }
                        .into(),
                    ),
                    method: s.to_estring().asciified(),
                    method_range: first_token.range,
                }
            }
            TokenKind::KeywordNot => {
                let token2 = self.lexer.lex(diag, LexerState::FirstArgument);
                next_token = Some(if matches!(token2.kind, TokenKind::Newline) {
                    self.lexer.lex(diag, LexerState::Begin)
                } else {
                    token2
                });
                ExprLike::NotOp {
                    range: first_token.range,
                }
            }
            TokenKind::KeywordSelf => ExprLike::Expr(
                SelfExpr {
                    range: first_token.range,
                    parens: Vec::new(),
                }
                .into(),
            ),
            TokenKind::KeywordCapitalDoubleUnderscoreEncoding => ExprLike::Expr(
                SourceEncodingExpr {
                    range: first_token.range,
                    parens: Vec::new(),
                }
                .into(),
            ),
            TokenKind::KeywordCapitalDoubleUnderscoreFile => ExprLike::Expr(
                SourceFileExpr {
                    range: first_token.range,
                    parens: Vec::new(),
                }
                .into(),
            ),
            TokenKind::KeywordCapitalDoubleUnderscoreLine => ExprLike::Expr(
                SourceLineExpr {
                    range: first_token.range,
                    parens: Vec::new(),
                }
                .into(),
            ),
            TokenKind::Numeric(value) => {
                let NumericToken { value, imaginary } = value;
                ExprLike::Expr(
                    NumericExpr {
                        range: first_token.range,
                        parens: Vec::new(),
                        value,
                        imaginary,
                    }
                    .into(),
                )
            }
            TokenKind::StringBegin | TokenKind::StringBeginLabelable => {
                let mut state = self.lexer.prepare_string(&first_token);
                let mut contents = Vec::<StringContent>::new();
                let close_range;
                loop {
                    let token = self.lexer.lex_string_like(diag, &mut state);
                    match token.kind {
                        TokenKind::StringEnd => {
                            close_range = token.range;
                            break;
                        }
                        TokenKind::EOF => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("unexpected end of file"),
                            });
                            close_range = token.range;
                            break;
                        }
                        TokenKind::StringContent => {
                            let s = self.select(token.range);
                            contents.push(StringContent::Text(TextContent {
                                range: token.range,
                                // TODO: unescape
                                value: s.to_estring().asciified(),
                            }));
                        }
                        TokenKind::StringInterpolationBegin => {
                            let open_range = token.range;
                            let inner_first_token = self.lexer.lex(diag, LexerState::Begin);
                            let (inner, token_after_inner) = self.parse_stmt_list(
                                diag,
                                inner_first_token,
                                lv,
                                BailCtx {
                                    end_style: EndStyle::RBrace,
                                    indent: token.indent,
                                    ..Default::default()
                                },
                            );
                            if !matches!(token_after_inner.kind, TokenKind::RBrace) {
                                diag.push(Diagnostic {
                                    range: token_after_inner.range,
                                    message: format!("expected '}}'"),
                                });
                            }
                            contents.push(StringContent::Interpolation(InterpolationContent {
                                range: open_range | token_after_inner.range,
                                open_range,
                                close_range: token_after_inner.range,
                                expr: inner,
                            }));
                        }
                        // TokenKind::StringVarInterpolation => {}
                        _ => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("unexpected token"),
                            });
                        }
                    }
                }
                ExprLike::Expr(match state.variant {
                    StringVariant::String => StringExpr {
                        range: first_token.range | close_range,
                        parens: Vec::new(),
                        open_range: first_token.range,
                        close_range,
                        contents,
                    }
                    .into(),
                    StringVariant::Command => XStringExpr {
                        range: first_token.range | close_range,
                        parens: Vec::new(),
                        open_range: first_token.range,
                        close_range,
                        contents,
                    }
                    .into(),
                    StringVariant::Regexp => RegexpExpr {
                        range: first_token.range | close_range,
                        parens: Vec::new(),
                        open_range: first_token.range,
                        close_range,
                        contents,
                    }
                    .into(),
                    StringVariant::Symbol => todo!(),
                })
            }
            TokenKind::KeywordIf => {
                let (expr, token_after_expr) = self.parse_if_chain(diag, first_token, lv);
                next_token = Some(token_after_expr);
                ExprLike::Expr(expr)
            }
            TokenKind::KeywordUnless => {
                let (expr, token_after_expr) = self.parse_unless(diag, first_token, lv);
                next_token = Some(token_after_expr);
                ExprLike::Expr(expr)
            }
            TokenKind::KeywordWhile | TokenKind::KeywordUntil => {
                let flip = matches!(first_token.kind, TokenKind::KeywordUntil);
                let indent = first_token.indent;
                let cond_first_token = self.lexer.lex(diag, LexerState::Begin);
                let (cond, token_after_cond) =
                    self.parse_expr_lv_spelled_and_or(diag, cond_first_token, lv);
                let token_after_do = match token_after_cond.kind {
                    TokenKind::Semicolon | TokenKind::Newline | TokenKind::KeywordDo => {
                        self.lexer.lex(diag, LexerState::Begin)
                    }
                    _ => {
                        diag.push(Diagnostic {
                            range: token_after_cond.range,
                            message: format!("expected 'do', ';', or newline"),
                        });
                        token_after_cond
                    }
                };
                let (body, token_after_body) = self.parse_stmt_list(
                    diag,
                    token_after_do,
                    lv,
                    BailCtx {
                        end_style: EndStyle::End,
                        indent,
                        ..Default::default()
                    },
                );
                if !matches!(token_after_body.kind, TokenKind::KeywordEnd) {
                    diag.push(Diagnostic {
                        range: token_after_body.range,
                        message: format!("expected 'end'"),
                    });
                }
                if flip {
                    ExprLike::Expr(
                        UntilExpr {
                            range: first_token.range | *body.outer_range() | token_after_body.range,
                            parens: Vec::new(),

                            cond: Box::new(cond),
                            body: Box::new(body),
                        }
                        .into(),
                    )
                } else {
                    ExprLike::Expr(
                        WhileExpr {
                            range: first_token.range | *body.outer_range() | token_after_body.range,
                            parens: Vec::new(),

                            cond: Box::new(cond),
                            body: Box::new(body),
                        }
                        .into(),
                    )
                }
            }
            TokenKind::LParen => {
                let open_range = first_token.range;
                let expr_first_token = self.lexer.lex(diag, LexerState::BeginLabelable);
                let (expr, token_after_expr) = self.parse_stmt_list(
                    diag,
                    expr_first_token,
                    lv,
                    BailCtx {
                        end_style: EndStyle::RParen,
                        indent: first_token.indent,
                        ..Default::default()
                    },
                );
                let mut last_token = token_after_expr;
                let close_range = loop {
                    match last_token.kind {
                        TokenKind::RParen => {
                            let range = last_token.range;
                            last_token = self.lexer.lex(diag, LexerState::End);
                            break range;
                        }
                        TokenKind::EOF => {
                            diag.push(Diagnostic {
                                range: last_token.range,
                                message: format!("unexpected end of file"),
                            });
                            break last_token.range;
                        }
                        _ => {
                            diag.push(Diagnostic {
                                range: last_token.range,
                                message: format!("unexpected token"),
                            });
                        }
                    }
                };
                // Add paren
                let mut expr = expr;
                if matches!(expr.parens().last(), Some(Paren::Implicit(_))) {
                    let Paren::Implicit(paren) = expr.parens_mut().pop().unwrap() else {
                        unreachable!();
                    };
                    expr.parens_mut().push(
                        ParenParen {
                            range: open_range | close_range,
                            open_range,
                            separator_prefix: paren.separator_prefix,
                            separator_suffix: paren.separator_suffix,
                            close_range,
                        }
                        .into(),
                    );
                } else {
                    expr.parens_mut().push(
                        ParenParen {
                            range: open_range | close_range,
                            open_range,
                            separator_prefix: Vec::new(),
                            separator_suffix: Vec::new(),
                            close_range,
                        }
                        .into(),
                    );
                }
                next_token = Some(last_token);
                ExprLike::Expr(expr)
            }
            _ => {
                diag.push(Diagnostic {
                    range: first_token.range,
                    message: format!("unexpected token"),
                });
                ExprLike::Expr(
                    ErrorExpr {
                        range: first_token.range,
                        parens: Vec::new(),
                    }
                    .into(),
                )
            }
        };
        let next_token = if let Some(next_token) = next_token {
            next_token
        } else {
            self.lexer.lex(diag, LexerState::End)
        };
        (expr, next_token)
    }

    fn parse_if_chain(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (Expr, Token) {
        let if_token = first_token;
        let cond_first_token = self.lexer.lex(diag, LexerState::Begin);
        let (cond, token_after_cond) =
            self.parse_expr_lv_spelled_and_or(diag, cond_first_token, lv);
        let body_first_token = match token_after_cond.kind {
            TokenKind::Semicolon | TokenKind::Newline => {
                let token_after_cond2 = self.lexer.lex(diag, LexerState::Begin);
                if matches!(token_after_cond2.kind, TokenKind::KeywordThen) {
                    self.lexer.lex(diag, LexerState::Begin)
                } else {
                    token_after_cond2
                }
            }
            TokenKind::KeywordThen => self.lexer.lex(diag, LexerState::Begin),
            _ => {
                diag.push(Diagnostic {
                    range: token_after_cond.range,
                    message: format!("expected 'then'"),
                });
                token_after_cond
            }
        };
        let (then, token_after_then_body) = self.parse_stmt_list(
            diag,
            body_first_token,
            lv,
            BailCtx {
                end_style: EndStyle::EndOrElse,
                indent: if_token.indent,
                ..Default::default()
            },
        );
        match token_after_then_body.kind {
            TokenKind::KeywordElse => {
                let else_first_token = self.lexer.lex(diag, LexerState::Begin);
                let (else_, token_after_else) = self.parse_stmt_list(
                    diag,
                    else_first_token,
                    lv,
                    BailCtx {
                        end_style: EndStyle::End,
                        indent: token_after_then_body.indent,
                        ..Default::default()
                    },
                );
                if !matches!(token_after_else.kind, TokenKind::KeywordEnd) {
                    diag.push(Diagnostic {
                        range: token_after_else.range,
                        message: format!("expected 'end'"),
                    });
                    let expr = IfExpr {
                        range: if_token.range | *else_.outer_range(),
                        parens: Vec::new(),

                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Box::new(else_),
                    }
                    .into();
                    let last_token = self.lexer.lex(diag, LexerState::End);
                    return (expr, last_token);
                }
                let expr = IfExpr {
                    range: if_token.range | token_after_else.range,
                    parens: Vec::new(),

                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: Box::new(else_),
                }
                .into();
                let last_token = self.lexer.lex(diag, LexerState::End);
                (expr, last_token)
            }
            TokenKind::KeywordElsif => {
                let (else_, token_after_else) =
                    self.parse_if_chain(diag, token_after_then_body, lv);
                let expr = IfExpr {
                    range: if_token.range | *else_.outer_range(),
                    parens: Vec::new(),

                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: Box::new(else_),
                }
                .into();
                (expr, token_after_else)
            }
            _ => {
                let (end_range, last_token) =
                    if matches!(token_after_then_body.kind, TokenKind::KeywordEnd) {
                        let token = self.lexer.lex(diag, LexerState::End);
                        (token_after_then_body.range, token)
                    } else {
                        diag.push(Diagnostic {
                            range: token_after_then_body.range,
                            message: format!("expected 'end', 'else', or 'elsif'"),
                        });
                        (DUMMY_RANGE, token_after_then_body)
                    };
                let expr = IfExpr {
                    range: if_token.range | *then.outer_range() | end_range,
                    parens: Vec::new(),

                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: Box::new(
                        NilExpr {
                            range: DUMMY_RANGE,
                            parens: Vec::new(),
                            style: NilStyle::Implicit,
                        }
                        .into(),
                    ),
                }
                .into();
                (expr, last_token)
            }
        }
    }

    fn parse_unless(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (Expr, Token) {
        let unless_token = first_token;
        let cond_first_token = self.lexer.lex(diag, LexerState::Begin);
        let (cond, token_after_cond) =
            self.parse_expr_lv_spelled_and_or(diag, cond_first_token, lv);
        let body_first_token = match token_after_cond.kind {
            TokenKind::Semicolon | TokenKind::Newline => {
                let token_after_cond2 = self.lexer.lex(diag, LexerState::Begin);
                if matches!(token_after_cond2.kind, TokenKind::KeywordThen) {
                    self.lexer.lex(diag, LexerState::Begin)
                } else {
                    token_after_cond2
                }
            }
            TokenKind::KeywordThen => self.lexer.lex(diag, LexerState::Begin),
            _ => {
                diag.push(Diagnostic {
                    range: token_after_cond.range,
                    message: format!("expected 'then'"),
                });
                token_after_cond
            }
        };
        let (then, token_after_then_body) = self.parse_stmt_list(
            diag,
            body_first_token,
            lv,
            BailCtx {
                end_style: EndStyle::EndOrElse,
                indent: unless_token.indent,
                ..Default::default()
            },
        );
        match token_after_then_body.kind {
            TokenKind::KeywordElse => {
                let else_first_token = self.lexer.lex(diag, LexerState::Begin);
                let (else_, token_after_else) = self.parse_stmt_list(
                    diag,
                    else_first_token,
                    lv,
                    BailCtx {
                        end_style: EndStyle::End,
                        indent: token_after_then_body.indent,
                        ..Default::default()
                    },
                );
                if !matches!(token_after_else.kind, TokenKind::KeywordEnd) {
                    diag.push(Diagnostic {
                        range: token_after_else.range,
                        message: format!("expected 'end'"),
                    });
                    let expr = IfExpr {
                        range: unless_token.range | *else_.outer_range(),
                        parens: Vec::new(),

                        cond: Box::new(cond),
                        then: Box::new(else_),
                        else_: Box::new(then),
                    }
                    .into();
                    let last_token = self.lexer.lex(diag, LexerState::End);
                    return (expr, last_token);
                }
                let expr = IfExpr {
                    range: unless_token.range | token_after_else.range,
                    parens: Vec::new(),

                    cond: Box::new(cond),
                    then: Box::new(else_),
                    else_: Box::new(then),
                }
                .into();
                let last_token = self.lexer.lex(diag, LexerState::End);
                (expr, last_token)
            }
            TokenKind::KeywordElsif => {
                diag.push(Diagnostic {
                    range: token_after_then_body.range,
                    message: format!("'unless' cannot have 'elsif'"),
                });
                let (else_, token_after_else) =
                    self.parse_if_chain(diag, token_after_then_body, lv);
                let expr = IfExpr {
                    range: unless_token.range | *else_.outer_range(),
                    parens: Vec::new(),

                    cond: Box::new(cond),
                    then: Box::new(else_),
                    else_: Box::new(then),
                }
                .into();
                (expr, token_after_else)
            }
            _ => {
                let (end_range, last_token) =
                    if matches!(token_after_then_body.kind, TokenKind::KeywordEnd) {
                        let token = self.lexer.lex(diag, LexerState::End);
                        (token_after_then_body.range, token)
                    } else {
                        diag.push(Diagnostic {
                            range: token_after_then_body.range,
                            message: format!("expected 'end', 'else', or 'elsif'"),
                        });
                        (DUMMY_RANGE, token_after_then_body)
                    };
                let expr = IfExpr {
                    range: unless_token.range | *then.outer_range() | end_range,
                    parens: Vec::new(),

                    cond: Box::new(cond),
                    then: Box::new(
                        NilExpr {
                            range: DUMMY_RANGE,
                            parens: Vec::new(),
                            style: NilStyle::Implicit,
                        }
                        .into(),
                    ),
                    else_: Box::new(then),
                }
                .into();
                (expr, last_token)
            }
        }
    }

    fn parse_cmd_args(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (ArgList, Token) {
        let (first_arg, mut last_token) = self.parse_expr_lv_cmd(diag, first_token, lv);
        let mut args = ArgList {
            range: *first_arg.outer_range(),
            paren: None,
            args: vec![ExprArg {
                range: *first_arg.outer_range(),
                comma: None,
                expr: Box::new(first_arg),
            }
            .into()],
        };
        loop {
            let token = last_token;
            match token.kind {
                TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                    diag.push(Diagnostic {
                        range: token.range,
                        message: format!("unexpected token"),
                    });
                    last_token = token;
                    break;
                }
                TokenKind::Newline | TokenKind::Semicolon | TokenKind::EOF => {
                    last_token = token;
                    break;
                }
                TokenKind::Comma => {
                    *args.args.last_mut().unwrap().comma_mut() = Some(token.range);
                    let arg_first_token = self.lexer.lex(diag, LexerState::BeginLabelable);
                    let (arg, arg_next_token) = self.parse_expr_lv_cmd(diag, arg_first_token, lv);
                    last_token = arg_next_token;
                    args.range |= *arg.outer_range();
                    args.args.push(
                        ExprArg {
                            range: *arg.outer_range(),
                            comma: None,
                            expr: Box::new(arg),
                        }
                        .into(),
                    );
                }
                _ => {
                    diag.push(Diagnostic {
                        range: token.range,
                        message: format!("unexpected token"),
                    });
                    last_token = token;
                    break;
                }
            }
        }
        (args, last_token)
    }

    fn parse_paren_args(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (ArgList, Token) {
        let mut args = ArgList {
            range: DUMMY_RANGE,
            paren: None,
            args: Vec::new(),
        };
        let paren_open_range = first_token.range;
        let mut paren_close_range = DUMMY_RANGE;
        let mut last_token = self.lexer.lex(diag, LexerState::BeginLabelable);
        loop {
            let token = last_token;
            match token.kind {
                TokenKind::RParen => {
                    paren_close_range = token.range;
                    last_token = self.lexer.lex(diag, LexerState::End);
                    break;
                }
                TokenKind::EOF => {
                    diag.push(Diagnostic {
                        range: token.range,
                        message: format!("unexpected end of file"),
                    });
                    last_token = token;
                    break;
                }
                _ => {
                    let (arg, token_after_arg) = self.parse_expr_lv_cmd(diag, token, lv);
                    args.args.push(
                        ExprArg {
                            range: *arg.outer_range(),
                            comma: None,
                            expr: Box::new(arg),
                        }
                        .into(),
                    );
                    match token_after_arg.kind {
                        TokenKind::Comma => {
                            *args.args.last_mut().unwrap().comma_mut() =
                                Some(token_after_arg.range);
                            last_token = self.lexer.lex(diag, LexerState::BeginLabelable);
                        }
                        TokenKind::EOF | TokenKind::RParen => {
                            last_token = token_after_arg;
                        }
                        _ => {
                            diag.push(Diagnostic {
                                range: token_after_arg.range,
                                message: format!("unexpected token"),
                            });
                            last_token = token_after_arg;
                        }
                    }
                }
            }
        }
        args.paren = Some(
            ParenParen {
                range: paren_open_range | args.range | paren_close_range,
                open_range: paren_open_range,
                separator_prefix: Vec::new(),
                separator_suffix: Vec::new(),
                close_range: paren_close_range,
            }
            .into(),
        );
        (args, last_token)
    }

    fn parse_whole_type(&mut self, diag: &mut Vec<Diagnostic>) -> Type {
        self.fix_input_encoding(diag);
        let first_token = self.lexer.lex(diag, LexerState::Begin);
        let (ty, next_token) = self.parse_type(diag, first_token);
        match next_token.kind {
            TokenKind::EOF => {}
            _ => {
                diag.push(Diagnostic {
                    range: next_token.range,
                    message: format!("unexpected token"),
                });
            }
        }
        ty
    }

    fn parse_type(&mut self, diag: &mut Vec<Diagnostic>, first_token: Token) -> (Type, Token) {
        let typ = match first_token.kind {
            TokenKind::KeywordNil => NilType {
                range: first_token.range,
            }
            .into(),
            TokenKind::KeywordFalse => FalseType {
                range: first_token.range,
            }
            .into(),
            TokenKind::KeywordTrue => TrueType {
                range: first_token.range,
            }
            .into(),
            TokenKind::Const => {
                let s = self.select(first_token.range);
                match s.as_bytes() {
                    b"NilClass" => NilType {
                        range: first_token.range,
                    }
                    .into(),
                    b"FalseClass" => FalseType {
                        range: first_token.range,
                    }
                    .into(),
                    b"TrueClass" => TrueType {
                        range: first_token.range,
                    }
                    .into(),
                    b"Integer" => IntegerType {
                        range: first_token.range,
                    }
                    .into(),
                    b"String" => StringType {
                        range: first_token.range,
                    }
                    .into(),
                    b"Regexp" => RegexpType {
                        range: first_token.range,
                    }
                    .into(),
                    _ => {
                        diag.push(Diagnostic {
                            range: first_token.range,
                            message: format!("unexpected token"),
                        });
                        ErrorType {
                            range: first_token.range,
                        }
                        .into()
                    }
                }
            }
            _ => {
                diag.push(Diagnostic {
                    range: first_token.range,
                    message: format!("unexpected token"),
                });
                ErrorType {
                    range: first_token.range,
                }
                .into()
            }
        };
        let next_token = self.lexer.lex(diag, LexerState::End);
        (typ, next_token)
    }

    // fn fill_token(&mut self, diag: &mut Vec<Diagnostic>, state: LexerState) -> &Token {
    //     if self.next_token.is_none() {
    //         let token = self.lexer.lex(diag, state);
    //         self.next_token = Some(token);
    //     }
    //     self.next_token.as_ref().unwrap()
    // }

    // #[track_caller]
    // fn bump(&mut self) -> Token {
    //     if matches!(
    //         self.next_token,
    //         Some(Token {
    //             kind: TokenKind::EOF,
    //             ..
    //         })
    //     ) {
    //         self.next_token.clone().unwrap()
    //     } else if let Some(next_token) = self.next_token.take() {
    //         next_token
    //     } else {
    //         panic!("bump: no token to bump");
    //     }
    // }

    // fn peek(&mut self) -> Token {
    //     if let Some(token) = self.next_token {
    //         token
    //     } else {
    //         panic!("peek: token not filled yet");
    //     }
    // }

    fn select(&self, range: CodeRange) -> EStrRef<'a> {
        EStrRef::from_bytes(&self.bytes()[range.range()], self.input().encoding())
    }
}

/// LR(1)-like handwritten parser.
// Note: I plan to replace the whole Parser with this stack-based one,
// but currently it is used only for parsing operators.
#[derive(Debug)]
struct StackParser<'a, 'b> {
    parser: &'b mut Parser<'a>,
    stack: Vec<StackElem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum StackElem {
    BinOpStep1(BinOpStep1),
    UnOpStep1(UnOpStep1),
}

/// Suspension where `lhs op` has been parsed and
/// the parser is waiting for `rhs`.
#[derive(Debug, Clone, PartialEq, Eq)]
struct BinOpStep1 {
    lhs: Expr,
    op_token: Token,
    op_prec: usize,
}

/// Suspension where `op` has been parsed and
/// the parser is waiting for `expr`.
#[derive(Debug, Clone, PartialEq, Eq)]
struct UnOpStep1 {
    op_token: Token,
    op_prec: usize,
}

// Operator precedence values.
// Small value represents higher precedence.
// When determining reduction before left-associative operator,
// add 1 to the incoming precedence value.
/// Call-like expressions (suffix). E.g.
///
/// - `()` as in `f()`
/// - `{}` as in `f {}`
/// - `.f` as in `x.f`
/// - `.` as in `x.()`
/// - `::Foo` as in `x::Foo`
/// - `[` as in `x[i]`
///
/// Only appears as a shift precedence.
const PREC_CALL_LIKE: usize = 0;
/// `+x`, `!x`, `~x` (prefix)
///
/// Only appears as a reduction precedence.
const PREC_UNARY: usize = 1;
/// `x ** y`, right associative
const PREC_EXPONENTIAL: usize = 2;
/// `-x`
///
/// Only appears as a reduction precedence.
const PREC_UNARY_MINUS: usize = 3;
/// `x * y`, `x / y`, `x % y`, left
const PREC_MULTIPLICATIVE: usize = 4;
/// `x + y`, `x - y`, left
const PREC_ADDITIVE: usize = 5;
/// `x << y`, `x >> y`, left
const PREC_SHIFT: usize = 6;
/// `x & y`, left
const PREC_BITWISE_AND: usize = 7;
/// `x | y`, `x ^ y`, left
const PREC_BITWISE_OR: usize = 8;
/// `x < y`, `x <= y`, `x > y`, `x >= y`, left
const PREC_INEQUALITY: usize = 9;
/// `x == y`, `x != y`, `x =~ y`, `x !~ y`, `x === y`, non-associative
const PREC_EQUALITY: usize = 10;
/// `x && y`, left
const PREC_LOGICAL_AND: usize = 11;
/// `x || y`, left
const PREC_LOGICAL_OR: usize = 12;
/// Used as a special shift precedence for tokens that close something.
/// (which means that we can basically reduce all the infix and prefixes
/// until we find the corresponding opening stack element)
///
/// Typically `)`, `]`, `}`, and `end`.
const PREC_LOWEST: usize = 100;

fn prec_left_assoc(prec: usize) -> usize {
    prec + 1
}
fn prec_right_assoc(prec: usize) -> usize {
    prec
}
fn suffix_token_shift_precedence(token: &Token) -> usize {
    match token.kind {
        TokenKind::BinOp(ref op) => match op {
            BinOpKind::LogicalOr => prec_left_assoc(PREC_LOGICAL_OR),
            BinOpKind::LogicalAnd => prec_left_assoc(PREC_LOGICAL_AND),
            // TODO: handle non-associativity
            BinOpKind::Eq
            | BinOpKind::NotEq
            | BinOpKind::Match
            | BinOpKind::NotMatch
            | BinOpKind::Incl
            | BinOpKind::Cmp => prec_left_assoc(PREC_EQUALITY),
            BinOpKind::Lt | BinOpKind::Le | BinOpKind::Gt | BinOpKind::Ge => {
                prec_left_assoc(PREC_INEQUALITY)
            }
            BinOpKind::BitwiseOr | BinOpKind::BitwiseXor => prec_left_assoc(PREC_BITWISE_OR),
            BinOpKind::BitwiseAnd => prec_left_assoc(PREC_BITWISE_AND),
            BinOpKind::LShift | BinOpKind::RShift => prec_left_assoc(PREC_SHIFT),
            BinOpKind::Add | BinOpKind::Sub => prec_left_assoc(PREC_ADDITIVE),
            BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
                prec_left_assoc(PREC_MULTIPLICATIVE)
            }
            BinOpKind::Pow => prec_right_assoc(PREC_EXPONENTIAL),
        },

        TokenKind::LParen
        | TokenKind::LBracket
        | TokenKind::LBrace
        | TokenKind::Dot
        | TokenKind::AmpDot
        | TokenKind::ColonColon => PREC_CALL_LIKE,

        TokenKind::KeywordCapitalDoubleUnderscoreEncoding
        | TokenKind::KeywordCapitalDoubleUnderscoreLine
        | TokenKind::KeywordCapitalDoubleUnderscoreFile
        | TokenKind::KeywordCapitalBegin
        | TokenKind::KeywordCapitalEnd
        | TokenKind::KeywordAlias
        | TokenKind::KeywordAnd
        | TokenKind::KeywordBegin
        | TokenKind::KeywordBreak
        | TokenKind::KeywordCase
        | TokenKind::KeywordClass
        | TokenKind::KeywordDef
        | TokenKind::KeywordDefinedQ
        | TokenKind::KeywordDo
        | TokenKind::KeywordElse
        | TokenKind::KeywordElsif
        | TokenKind::KeywordEnd
        | TokenKind::KeywordEnsure
        | TokenKind::KeywordFalse
        | TokenKind::KeywordFor
        | TokenKind::KeywordIf
        | TokenKind::KeywordIfInfix
        | TokenKind::KeywordIn
        | TokenKind::KeywordModule
        | TokenKind::KeywordNext
        | TokenKind::KeywordNil
        | TokenKind::KeywordNot
        | TokenKind::KeywordOr
        | TokenKind::KeywordRedo
        | TokenKind::KeywordRescue
        | TokenKind::KeywordRetry
        | TokenKind::KeywordReturn
        | TokenKind::KeywordSelf
        | TokenKind::KeywordSuper
        | TokenKind::KeywordThen
        | TokenKind::KeywordTrue
        | TokenKind::KeywordUndef
        | TokenKind::KeywordUnless
        | TokenKind::KeywordUnlessInfix
        | TokenKind::KeywordUntil
        | TokenKind::KeywordUntilInfix
        | TokenKind::KeywordWhen
        | TokenKind::KeywordWhile
        | TokenKind::KeywordWhileInfix
        | TokenKind::KeywordYield
        | TokenKind::Identifier
        | TokenKind::Const
        | TokenKind::MethodName
        | TokenKind::Label
        | TokenKind::Symbol
        | TokenKind::NonLocal(_)
        | TokenKind::Numeric(_)
        | TokenKind::CharLiteral(_)
        | TokenKind::StringBegin
        | TokenKind::StringBeginLabelable
        | TokenKind::StringEnd
        | TokenKind::StringEndColon
        | TokenKind::StringContent
        | TokenKind::StringInterpolationBegin
        | TokenKind::StringVarInterpolation
        | TokenKind::StringComma
        | TokenKind::UnOp(_)
        | TokenKind::OpAssign(_)
        | TokenKind::AmpPrefix
        | TokenKind::LParenRestricted
        | TokenKind::RParen
        | TokenKind::StarPrefix
        | TokenKind::StarStarPrefix
        | TokenKind::Comma
        | TokenKind::Arrow
        | TokenKind::DotDot
        | TokenKind::DotDotDot
        | TokenKind::Colon
        | TokenKind::ColonColonPrefix
        | TokenKind::Semicolon
        | TokenKind::Newline
        | TokenKind::Eq
        | TokenKind::FatArrow
        | TokenKind::Question
        | TokenKind::At
        | TokenKind::LBracketPrefix
        | TokenKind::RBracket
        | TokenKind::RBrace
        | TokenKind::EOF
        | TokenKind::Unknown => PREC_LOWEST,
    }
}

impl<'a, 'b> StackParser<'a, 'b> {
    fn parse(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> (ExprLike, Token) {
        let mut last_token = first_token;
        let mut last_expr: Option<ExprLike> = None;
        loop {
            let result = if let Some(last_expr) = last_expr {
                self.continue_parse(diag, last_expr, last_token, lv)
            } else {
                self.parse_step(diag, last_token, lv)
            };
            match result {
                Ok(v) => return v,
                Err(SuspendParse { next_token, expr }) => {
                    last_token = next_token;
                    last_expr = expr;
                }
            }
        }
    }

    fn parse_step(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> Result<(ExprLike, Token), SuspendParse> {
        match first_token.kind {
            TokenKind::UnOp(_) => self.parse_un_op_step1(diag, first_token),
            _ => {
                let (expr, next_token) = self.parser.parse_expr_lv_primary(diag, first_token, lv);
                Err(SuspendParse {
                    next_token,
                    expr: Some(expr),
                })
            }
        }
    }

    fn continue_parse(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        last_expr: ExprLike,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> Result<(ExprLike, Token), SuspendParse> {
        let last_expr = self.auto_reduce(diag, last_expr, &first_token);
        match first_token.kind {
            TokenKind::BinOp(_) => self.parse_bin_op_step1(diag, last_expr, first_token),
            TokenKind::LParen => self.parse_paren_call_step1(diag, last_expr, first_token, lv),
            TokenKind::Dot | TokenKind::AmpDot | TokenKind::ColonColon => {
                self.parse_dot_step1(diag, last_expr, first_token, lv)
            }
            TokenKind::At => self.parse_type_annotation_step1(diag, last_expr, first_token),
            _ => {
                assert_eq!(self.stack.len(), 0);
                Ok((last_expr, first_token))
            }
        }
    }

    fn auto_reduce(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        mut expr: ExprLike,
        next_token: &Token,
    ) -> ExprLike {
        let shift_prec = suffix_token_shift_precedence(next_token);
        loop {
            let Some(last) = self.stack.last() else {
                break;
            };
            let last_op_prec = match last {
                StackElem::BinOpStep1(BinOpStep1 { op_prec, .. }) => *op_prec,
                StackElem::UnOpStep1(UnOpStep1 { op_prec, .. }) => *op_prec,
                _ => break,
            };
            if shift_prec <= last_op_prec {
                break;
            }
            let cont = self.stack.pop().unwrap();
            expr = match cont {
                StackElem::BinOpStep1(cont) => self.parse_bin_op_reduce(diag, cont, expr),
                StackElem::UnOpStep1(cont) => self.parse_un_op_reduce(diag, cont, expr),
                _ => unreachable!(),
            };
        }
        expr
    }

    // BinOp step 1: found `lhs op`, push it
    fn parse_bin_op_step1<T>(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_expr: ExprLike,
        next_token: Token,
    ) -> Result<T, SuspendParse> {
        let op_prec = match next_token.kind {
            TokenKind::BinOp(ref op) => match op {
                BinOpKind::LogicalOr => PREC_LOGICAL_OR,
                BinOpKind::LogicalAnd => PREC_LOGICAL_AND,
                BinOpKind::Eq
                | BinOpKind::NotEq
                | BinOpKind::Match
                | BinOpKind::NotMatch
                | BinOpKind::Incl
                | BinOpKind::Cmp => PREC_EQUALITY,
                BinOpKind::Lt | BinOpKind::Le | BinOpKind::Gt | BinOpKind::Ge => PREC_INEQUALITY,
                BinOpKind::BitwiseOr | BinOpKind::BitwiseXor => PREC_BITWISE_OR,
                BinOpKind::BitwiseAnd => PREC_BITWISE_AND,
                BinOpKind::LShift | BinOpKind::RShift => PREC_SHIFT,
                BinOpKind::Add | BinOpKind::Sub => PREC_ADDITIVE,
                BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => PREC_MULTIPLICATIVE,
                BinOpKind::Pow => PREC_EXPONENTIAL,
            },
            _ => unreachable!(),
        };
        let next_state = if matches!(next_token.kind, TokenKind::BinOp(BinOpKind::BitwiseOr)) {
            LexerState::BeginLabelable
        } else {
            LexerState::Begin
        };
        self.stack.push(StackElem::BinOpStep1(BinOpStep1 {
            lhs: first_expr.into_expr(diag),
            op_token: next_token,
            op_prec,
        }));
        let next_token = self.parser.lexer.lex(diag, next_state);
        Err(SuspendParse {
            next_token,
            expr: None,
        })
    }

    // BinOp step 2: found `lhs op rhs`, reduce it
    fn parse_bin_op_reduce(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        cont: BinOpStep1,
        first_expr: ExprLike,
    ) -> ExprLike {
        let BinOpStep1 {
            lhs,
            op_token,
            op_prec: _,
        } = cont;
        let rhs = first_expr.into_expr(diag);
        let op_kind = if let TokenKind::BinOp(ref op) = op_token.kind {
            op
        } else {
            unreachable!()
        };
        let expr: Expr = match op_kind {
            BinOpKind::LogicalOr => OrExpr {
                range: *lhs.outer_range() | *rhs.outer_range(),
                parens: Vec::new(),

                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
            .into(),
            BinOpKind::LogicalAnd => AndExpr {
                range: *lhs.outer_range() | *rhs.outer_range(),
                parens: Vec::new(),

                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
            .into(),
            _ => {
                let method_name = match op_kind {
                    BinOpKind::LogicalOr => unreachable!(),
                    BinOpKind::LogicalAnd => unreachable!(),
                    BinOpKind::Eq => "==",
                    BinOpKind::NotEq => "!=",
                    BinOpKind::Match => "=~",
                    BinOpKind::NotMatch => "!~",
                    BinOpKind::Incl => "===",
                    BinOpKind::Cmp => "<=>",
                    BinOpKind::Lt => "<",
                    BinOpKind::Le => "<=",
                    BinOpKind::Gt => ">",
                    BinOpKind::Ge => ">=",
                    BinOpKind::BitwiseOr => "|",
                    BinOpKind::BitwiseXor => "^",
                    BinOpKind::BitwiseAnd => "&",
                    BinOpKind::LShift => "<<",
                    BinOpKind::RShift => ">>",
                    BinOpKind::Add => "+",
                    BinOpKind::Sub => "-",
                    BinOpKind::Mul => "*",
                    BinOpKind::Div => "/",
                    BinOpKind::Mod => "%",
                    BinOpKind::Pow => "**",
                };
                let expr = CallExpr {
                    range: *lhs.outer_range() | *rhs.outer_range(),
                    parens: Vec::new(),

                    receiver: Box::new(lhs),
                    args: ArgList {
                        range: *rhs.outer_range(),
                        paren: None,
                        args: vec![ExprArg {
                            range: *rhs.outer_range(),
                            comma: None,
                            expr: Box::new(rhs),
                        }
                        .into()],
                    },
                    style: CallStyle::BinOp,
                    private: false,
                    optional: false,
                    method: EString::from(method_name).asciified(),
                    method_range: op_token.range,
                };
                expr.into()
            }
        };
        ExprLike::Expr(expr)
    }

    // UnOp step 1: found `op`, push it
    fn parse_un_op_step1<T>(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_token: Token,
    ) -> Result<T, SuspendParse> {
        let op_kind = if let TokenKind::UnOp(ref op) = first_token.kind {
            op
        } else {
            unreachable!()
        };
        let op_prec = match op_kind {
            UnOpKind::Minus => PREC_UNARY_MINUS,
            _ => PREC_UNARY,
        };
        self.stack.push(StackElem::UnOpStep1(UnOpStep1 {
            op_token: first_token,
            op_prec,
        }));
        let next_token = self.parser.lexer.lex(diag, LexerState::Begin);
        Err(SuspendParse {
            next_token,
            expr: None,
        })
    }

    // UnOp step 2: found `op expr`, reduce it
    fn parse_un_op_reduce(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        cont: UnOpStep1,
        first_expr: ExprLike,
    ) -> ExprLike {
        let UnOpStep1 {
            op_token,
            op_prec: _,
        } = cont;
        let op_kind = if let TokenKind::UnOp(ref op) = op_token.kind {
            op
        } else {
            unreachable!()
        };
        let rhs = first_expr.into_expr(diag);
        let method_name = match op_kind {
            UnOpKind::Not => "!",
            UnOpKind::BitwiseNot => "~",
            UnOpKind::Plus => "+@",
            UnOpKind::Minus => "-@",
        };
        let expr = CallExpr {
            range: op_token.range | *rhs.outer_range(),
            parens: Vec::new(),

            receiver: Box::new(rhs),
            args: ArgList {
                range: DUMMY_RANGE,
                paren: None,
                args: Vec::new(),
            },
            style: CallStyle::UnOp,
            private: false,
            optional: false,
            method: EString::from(method_name).asciified(),
            method_range: op_token.range,
        };
        ExprLike::Expr(expr.into())
    }

    /// Paren-call step 1: found `f(`, parse all
    // TODO: stackerize arg list parsing
    fn parse_paren_call_step1<T>(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_expr: ExprLike,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> Result<T, SuspendParse> {
        let method_range = first_token.range;
        let (args, token_after_args) = self.parser.parse_paren_args(diag, first_token, lv);
        let expr = match first_expr.callify() {
            ExprLike::ArglessCall {
                range,
                style,
                private,
                optional,
                receiver,
                method,
                method_range,
            } => ExprLike::BlocklessCall {
                range: range | *args.outer_range(),
                style,
                private,
                optional,
                receiver,
                method,
                method_range,
                args,
            },
            ExprLike::NotOp { range } => {
                if args.args.len() == 0 {
                    // `not()` is `nil.!()` (but why!?!?)
                    ExprLike::Expr(
                        CallExpr {
                            range: range | *args.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::SpelloutUnOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(
                                NilExpr {
                                    range: DUMMY_RANGE,
                                    parens: vec![ParenParen {
                                        range,
                                        // TODO: put a correct range
                                        open_range: DUMMY_RANGE,
                                        separator_prefix: vec![],
                                        separator_suffix: vec![],
                                        // TODO: put a correct range
                                        close_range: DUMMY_RANGE,
                                    }
                                    .into()],
                                    style: NilStyle::Implicit,
                                }
                                .into(),
                            ),
                            method: symbol("!"),
                            method_range,
                            args: ArgList {
                                range: DUMMY_RANGE,
                                paren: None,
                                args: vec![],
                            },
                        }
                        .into(),
                    )
                } else {
                    // TODO: also check against `not(expr,)` etc.
                    if args.args.len() > 1 {
                        diag.push(Diagnostic {
                            range: args.range,
                            message: format!("too many arguments"),
                        });
                    }
                    let args_range = *args.outer_range();
                    let Arg::Expr(arg) = { args.args }.swap_remove(0);
                    let arg = *arg.expr;
                    ExprLike::Expr(
                        CallExpr {
                            range: range | args_range,
                            parens: Vec::new(),

                            style: CallStyle::SpelloutUnOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(arg),
                            method: symbol("!"),
                            method_range,
                            args: ArgList {
                                range: DUMMY_RANGE,
                                paren: None,
                                args: vec![],
                            },
                        }
                        .into(),
                    )
                }
            }
            last_expr => {
                diag.push(Diagnostic {
                    range: method_range,
                    message: format!("Need a dot to call an expression"),
                });
                let last_expr = last_expr.into_expr(diag);
                ExprLike::Expr(
                    CallExpr {
                        range: *last_expr.outer_range() | *args.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::CallOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(last_expr),
                        method: symbol("call"),
                        method_range,
                        args,
                    }
                    .into(),
                )
            }
        };
        Err(SuspendParse {
            next_token: token_after_args,
            expr: Some(expr),
        })
    }

    /// Dot step 1: found `x.`, parse all
    fn parse_dot_step1<T>(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_expr: ExprLike,
        first_token: Token,
        lv: &mut LVCtx,
    ) -> Result<T, SuspendParse> {
        let private = if let ExprLike::Expr(Expr::Self_(expr)) = &first_expr {
            expr.parens.is_empty()
        } else {
            false
        };
        let optional = matches!(first_token.kind, TokenKind::AmpDot);
        let const_like = matches!(first_token.kind, TokenKind::ColonColon);
        let token_after_dot = self.parser.lexer.lex(diag, LexerState::MethForCall);
        let (expr, last_token) = match token_after_dot.kind {
            TokenKind::LParen => {
                let method_range = token_after_dot.range;
                // expr.(args)
                let (args, token_after_args) =
                    self.parser.parse_paren_args(diag, token_after_dot, lv);
                let last_expr = first_expr.into_expr(diag);
                (
                    ExprLike::BlocklessCall {
                        range: *last_expr.outer_range() | *args.outer_range(),
                        style: CallStyle::Dot,
                        private,
                        optional,
                        receiver: Box::new(last_expr),
                        method: symbol("call"),
                        method_range,
                        args,
                    },
                    token_after_args,
                )
            }
            TokenKind::Const if const_like => {
                // expr::Const
                let s = self.parser.select(token_after_dot.range);
                let last_expr = first_expr.into_expr(diag);
                let last_token = self.parser.lexer.lex(diag, LexerState::FirstArgument);
                (
                    ExprLike::Const {
                        range: *last_expr.outer_range() | token_after_dot.range,
                        private,
                        receiver: ConstReceiver::Expr(Box::new(last_expr)),
                        name: s.to_estring().asciified(),
                    },
                    last_token,
                )
            }
            TokenKind::Identifier | TokenKind::Const | TokenKind::MethodName => {
                // expr.meth
                let s = self.parser.select(token_after_dot.range);
                let last_expr = first_expr.into_expr(diag);
                let last_token = self.parser.lexer.lex(diag, LexerState::FirstArgument);
                (
                    ExprLike::ArglessCall {
                        range: *last_expr.outer_range() | token_after_dot.range,
                        style: CallStyle::Dot,
                        private,
                        optional,
                        receiver: Box::new(last_expr),
                        method: s.to_estring().asciified(),
                        method_range: token_after_dot.range,
                    },
                    last_token,
                )
            }
            _ => {
                diag.push(Diagnostic {
                    range: token_after_dot.range,
                    message: format!("unexpected token"),
                });
                let last_expr = first_expr.into_expr(diag);
                (
                    ExprLike::ArglessCall {
                        range: *last_expr.outer_range() | token_after_dot.range,
                        style: CallStyle::Dot,
                        private,
                        optional,
                        receiver: Box::new(last_expr),
                        method: symbol(""),
                        method_range: token_after_dot.range,
                    },
                    token_after_dot,
                )
            }
        };
        Err(SuspendParse {
            next_token: last_token,
            expr: Some(expr),
        })
    }

    /// Type annotation step 1: found `x @`, parse all
    fn parse_type_annotation_step1<T>(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        first_expr: ExprLike,
        first_token: Token,
    ) -> Result<T, SuspendParse> {
        let ty_first_token = self.parser.lexer.lex(diag, LexerState::Begin);
        let (ty, token_after_ty) = self.parser.parse_type(diag, ty_first_token);
        let mut last_expr = first_expr;
        if let ExprLike::Identifier { has_local, .. } = &mut last_expr {
            *has_local = true;
        }
        let lhs_expr = last_expr.into_expr(diag);
        let expr = ExprLike::Expr(match lhs_expr {
            Expr::LocalVariable(mut e) => {
                let ty_range = *ty.range();
                e.type_annotation = Some(TypeAnnotation {
                    range: first_token.range | ty_range,
                    type_: ty,
                });
                e.range = e.range | ty_range;
                e.into()
            }
            _ => {
                diag.push(Diagnostic {
                    range: first_token.range | *ty.range(),
                    message: format!("non-annotatable expression"),
                });
                lhs_expr
            }
        });
        Err(SuspendParse {
            next_token: token_after_ty,
            expr: Some(expr),
        })
    }
}

#[derive(Debug)]
struct SuspendParse {
    next_token: Token,
    expr: Option<ExprLike>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct PrecCtx {
    /// Command calls like `f x` cannot begin here due to the precedence of the previous token,
    /// therefore the parser should deliberately parse it and issue a diagnostic.
    invalid_command: bool,
}

impl PrecCtx {
    fn with_invalid_command(self) -> Self {
        Self {
            invalid_command: true,
            ..self
        }
    }
}

/// Describes how a closed-world parsing function (i.e. consumes unknown tokens by default)
/// should behave when it encounters certain tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct BailCtx {
    /// End parsing when `,` is encountered.
    bail_on_comma: bool,
    /// End parsing when `;` or newline is encountered.
    bail_on_semicolon: bool,
    /// Expected token to end the expression or such.
    end_style: EndStyle,
    /// Expected indentation level.
    /// Used after paren mismatch is detected so that we can recover from the error
    /// in a more graceful way.
    indent: usize,
}

impl BailCtx {
    fn should_bail(self, token: &Token) -> bool {
        match token.kind {
            TokenKind::RParen
            | TokenKind::RBracket
            | TokenKind::RBrace
            | TokenKind::KeywordEnd
            | TokenKind::KeywordElse
            | TokenKind::KeywordElsif
            | TokenKind::EOF => {
                let exact_match = match (self.end_style, &token.kind) {
                    (EndStyle::RParen, TokenKind::RParen)
                    | (EndStyle::RBracket, TokenKind::RBracket)
                    | (EndStyle::RBrace, TokenKind::RBrace)
                    | (EndStyle::EndOrElse, TokenKind::KeywordEnd)
                    | (EndStyle::EndOrElse, TokenKind::KeywordElse)
                    | (EndStyle::EndOrElse, TokenKind::KeywordElsif)
                    | (EndStyle::End, TokenKind::KeywordEnd)
                    | (EndStyle::EOF, TokenKind::EOF) => true,
                    _ => false,
                };
                if exact_match {
                    return true;
                }
                if token.indent > self.indent {
                    // Token too deep. Assume it's an extra closing delimiter.
                    return false;
                } else if token.indent < self.indent {
                    // Token too shallow. Assume there's a missing closing delimiter.
                    return true;
                }
                let token_tier = match token.kind {
                    TokenKind::RParen => 2,
                    TokenKind::RBracket => 2,
                    TokenKind::RBrace => 2,
                    TokenKind::KeywordEnd => 1,
                    TokenKind::KeywordElse => 1,
                    TokenKind::KeywordElsif => 1,
                    TokenKind::EOF => 0,
                    _ => unreachable!(),
                };
                let self_tier = match self.end_style {
                    EndStyle::RParen => 2,
                    EndStyle::RBracket => 2,
                    EndStyle::RBrace => 2,
                    EndStyle::EndOrElse => 1,
                    EndStyle::End => 1,
                    EndStyle::EOF => 0,
                };
                token_tier <= self_tier
            }
            TokenKind::Comma => self.bail_on_comma,
            TokenKind::Semicolon | TokenKind::Newline => self.bail_on_semicolon,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
enum EndStyle {
    /// Expects `)` as the final delimiter.
    RParen,
    /// Expects `]` as the final delimiter.
    RBracket,
    /// Expects `}` as the final delimiter.
    RBrace,
    /// Expects `end` as the final delimiter.
    End,
    /// Expects `end`, `else`, or `elsif` as the final delimiter.
    EndOrElse,
    /// Expects EOF as the final delimiter.
    #[default]
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ExprLike {
    Expr(Expr),
    /// `foo`, to be extended as:
    ///
    /// - `foo` itself
    /// - `foo(args)` parenthesized call
    /// - `foo args` non-parenthesized call
    /// - `foo { |args| ... }` block call
    Identifier {
        range: CodeRange,
        /// Is there already a local variable with the same name in the scope?
        /// Used to determine whether to extend as a local variable or a method call
        /// when interpreting it as an right-hand side expression.
        ///
        /// If the expression is going to be used as a left-hand side expression,
        /// it is always interpreted as a local variable regardless of this flag.
        has_local: bool,
        name: EString,
    },
    /// `Foo` or `obj::Foo`, to be extended as:
    ///
    /// - `Foo` or `obj::Foo` itself
    /// - `Foo(args)` or `obj::Foo(args)` parenthesized call
    /// - `Foo args` or `obj::Foo args` non-parenthesized call
    /// - `Foo { |args| ... }` or `obj::Foo { |args| ... }` block call
    Const {
        range: CodeRange,
        /// Used when `expr::Const` is extended as `expr::Const(args)`
        private: bool,
        receiver: ConstReceiver,
        name: EString,
    },
    /// The keyword `not`, to be extended as:
    ///
    /// - `not(expr)` parenthesized call
    /// - `not expr` non-parenthesized call
    NotOp {
        range: CodeRange,
    },
    /// call without arguments or block, to be extended as:
    ///
    /// - call with parenthesized arguments
    /// - call with non-parenthesized arguments
    /// - call with block
    ///
    /// Expressions identified as ArglessCall include:
    ///
    /// - `foo!` or `foo?`
    /// - `foo.bar` or `foo&.bar`
    /// - `foo::bar` (other than const)
    ArglessCall {
        range: CodeRange,
        style: CallStyle,
        private: bool,
        optional: bool,
        receiver: Box<Expr>,
        method: EString,
        method_range: CodeRange,
    },
    /// call with arguments but without block, to be extended as:
    ///
    /// - call with block
    BlocklessCall {
        range: CodeRange,
        style: CallStyle,
        private: bool,
        optional: bool,
        receiver: Box<Expr>,
        method: EString,
        method_range: CodeRange,
        args: ArgList,
    },
}

impl ExprLike {
    fn into_expr(self, diag: &mut Vec<Diagnostic>) -> Expr {
        match self {
            ExprLike::Expr(expr) => expr,
            ExprLike::Identifier {
                range,
                has_local: true,
                name,
            } => LocalVariableExpr {
                range,
                parens: Vec::new(),
                name,
                type_annotation: None,
            }
            .into(),
            ExprLike::Identifier {
                range,
                has_local: false,
                name,
            } => CallExpr {
                range,
                parens: Vec::new(),
                style: CallStyle::ImplicitSelf,
                private: true,
                optional: false,
                receiver: Box::new(
                    SelfExpr {
                        range: DUMMY_RANGE,
                        parens: Vec::new(),
                    }
                    .into(),
                ),
                method: name,
                method_range: range,
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![],
                },
            }
            .into(),
            ExprLike::Const {
                range,
                private: _,
                receiver,
                name,
            } => ConstExpr {
                range,
                parens: Vec::new(),
                receiver,
                name,
            }
            .into(),
            ExprLike::NotOp { range } => {
                diag.push(Diagnostic {
                    range,
                    message: format!("the keyword `not` needs an argument"),
                });
                // Return not(error)
                CallExpr {
                    range,
                    parens: Vec::new(),
                    style: CallStyle::SpelloutUnOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        ErrorExpr {
                            range: DUMMY_RANGE,
                            parens: Vec::new(),
                        }
                        .into(),
                    ),
                    method: symbol("!"),
                    method_range: range,
                    args: ArgList {
                        range: DUMMY_RANGE,
                        paren: None,
                        args: vec![],
                    },
                }
                .into()
            }
            ExprLike::ArglessCall {
                range,
                style,
                private,
                optional,
                receiver,
                method,
                method_range,
            } => CallExpr {
                range,
                parens: Vec::new(),
                style,
                private,
                optional,
                receiver,
                method,
                method_range,
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![],
                },
            }
            .into(),
            ExprLike::BlocklessCall {
                range,
                style,
                private,
                optional,
                receiver,
                method,
                method_range,
                args,
            } => CallExpr {
                range,
                parens: Vec::new(),
                style,
                private,
                optional,
                receiver,
                method,
                method_range,
                args,
            }
            .into(),
        }
    }

    fn callify(self) -> ExprLike {
        match self {
            ExprLike::Identifier {
                range,
                has_local: _,
                name,
            } => ExprLike::ArglessCall {
                range,
                style: CallStyle::ImplicitSelf,
                private: true,
                optional: false,
                receiver: Box::new(
                    SelfExpr {
                        range: DUMMY_RANGE,
                        parens: Vec::new(),
                    }
                    .into(),
                ),
                method: name,
                method_range: range,
            },
            ExprLike::Const {
                range,
                private,
                receiver: ConstReceiver::None,
                name,
            } => ExprLike::ArglessCall {
                range,
                style: CallStyle::Dot,
                private,
                optional: false,
                receiver: Box::new(
                    SelfExpr {
                        range: DUMMY_RANGE,
                        parens: Vec::new(),
                    }
                    .into(),
                ),
                method: name,
                method_range: range,
            },
            ExprLike::Const {
                range,
                private,
                receiver: ConstReceiver::Expr(receiver),
                name,
            } => ExprLike::ArglessCall {
                range,
                style: CallStyle::Dot,
                private,
                optional: false,
                receiver,
                method: name,
                method_range: range,
            },
            _ => self,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LVCtx {
    lvars: Vec<(EString, Option<usize>)>,
    reverse: HashMap<EString, usize>,
    fence: usize,
}

impl LVCtx {
    fn new() -> Self {
        Self {
            lvars: Vec::new(),
            reverse: HashMap::new(),
            fence: 0,
        }
    }

    fn push(&mut self, name: EString) {
        let prev = self.reverse.get(&name).copied();
        self.reverse.insert(name.clone(), self.lvars.len());
        self.lvars.push((name, prev));
    }

    fn checkpoint(&self) -> usize {
        self.lvars.len()
    }

    fn commit(&mut self, checkpoint: usize) -> Vec<EString> {
        let mut names = Vec::with_capacity(self.lvars.len().saturating_sub(checkpoint));
        while self.lvars.len() > checkpoint {
            let (name, prev) = self.lvars.pop().unwrap();
            if let Some(prev) = prev {
                *self.reverse.get_mut(&name).unwrap() = prev;
            } else {
                self.reverse.remove(&name);
            }
            names.push(name);
        }
        names.reverse();
        names
    }

    fn has(&self, name: &EString) -> bool {
        if let Some(&idx) = self.reverse.get(name) {
            idx >= self.fence
        } else {
            false
        }
    }

    fn fence(&mut self) -> usize {
        let prev_fence = self.fence;
        self.fence = prev_fence;
        prev_fence
    }

    fn rollback_fence(&mut self, prev_fence: usize) {
        self.fence = prev_fence;
    }
}

fn is_cmdarg_begin(token: &Token) -> bool {
    match token.kind {
        TokenKind::KeywordCapitalDoubleUnderscoreEncoding
        | TokenKind::KeywordCapitalDoubleUnderscoreLine
        | TokenKind::KeywordCapitalDoubleUnderscoreFile
        | TokenKind::KeywordCapitalBegin
        | TokenKind::KeywordCapitalEnd
        | TokenKind::KeywordAlias
        | TokenKind::KeywordBegin
        | TokenKind::KeywordBreak
        | TokenKind::KeywordCase
        | TokenKind::KeywordClass
        | TokenKind::KeywordDef
        | TokenKind::KeywordDefinedQ
        | TokenKind::KeywordFalse
        | TokenKind::KeywordFor
        | TokenKind::KeywordIf
        | TokenKind::KeywordModule
        | TokenKind::KeywordNext
        | TokenKind::KeywordNil
        | TokenKind::KeywordNot
        | TokenKind::KeywordRedo
        | TokenKind::KeywordRetry
        | TokenKind::KeywordReturn
        | TokenKind::KeywordSelf
        | TokenKind::KeywordSuper
        | TokenKind::KeywordTrue
        | TokenKind::KeywordUndef
        | TokenKind::KeywordUnless
        | TokenKind::KeywordUntil
        | TokenKind::KeywordWhile
        | TokenKind::KeywordYield
        | TokenKind::Identifier
        | TokenKind::Const
        | TokenKind::MethodName
        | TokenKind::Label
        | TokenKind::Symbol
        | TokenKind::NonLocal(_)
        | TokenKind::Numeric(_)
        | TokenKind::CharLiteral(_)
        | TokenKind::StringBegin
        | TokenKind::StringBeginLabelable
        | TokenKind::UnOp(_)
        | TokenKind::AmpPrefix
        | TokenKind::LParenRestricted
        | TokenKind::StarPrefix
        | TokenKind::StarStarPrefix
        | TokenKind::Arrow
        | TokenKind::ColonColonPrefix
        | TokenKind::LBracketPrefix => true,

        TokenKind::KeywordAnd
        | TokenKind::KeywordElse
        | TokenKind::KeywordElsif
        | TokenKind::KeywordEnd
        | TokenKind::KeywordEnsure
        | TokenKind::KeywordIfInfix
        | TokenKind::KeywordIn
        | TokenKind::KeywordOr
        | TokenKind::KeywordRescue
        | TokenKind::KeywordThen
        | TokenKind::KeywordUnlessInfix
        | TokenKind::KeywordUntilInfix
        | TokenKind::KeywordWhen
        | TokenKind::KeywordWhileInfix
        | TokenKind::StringEnd
        | TokenKind::StringEndColon
        | TokenKind::StringContent
        | TokenKind::StringInterpolationBegin
        | TokenKind::StringVarInterpolation
        | TokenKind::StringComma
        | TokenKind::BinOp(_)
        | TokenKind::OpAssign(_)
        | TokenKind::AmpDot
        | TokenKind::RParen
        | TokenKind::Comma
        | TokenKind::Dot
        | TokenKind::DotDot
        | TokenKind::DotDotDot
        | TokenKind::Colon
        | TokenKind::ColonColon
        | TokenKind::Semicolon
        | TokenKind::Newline
        | TokenKind::Eq
        | TokenKind::FatArrow
        | TokenKind::Question
        | TokenKind::At
        | TokenKind::LBracket
        | TokenKind::RBracket
        | TokenKind::RBrace
        | TokenKind::EOF => false,

        TokenKind::KeywordDo | TokenKind::LParen | TokenKind::LBrace | TokenKind::Unknown => false,
    }
}

fn symbol<T>(s: T) -> EString
where
    T: Into<String>,
{
    EString::from(s.into()).asciified()
}

fn symbol_ref<T>(s: &T) -> EStrRef<'_>
where
    T: AsRef<str> + ?Sized,
{
    EStrRef::from(s.as_ref()).asciified()
}

#[cfg(test)]
mod tests;
