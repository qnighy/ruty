mod lexer;

use std::{collections::HashMap, mem::take};

use crate::{
    ast::{
        CallExpr, CallStyle, CodeRange, ConstExpr, ConstReceiver, ErrorExpr, ErrorType,
        ErrorWriteTarget, Expr, FalseExpr, FalseType, ImplicitParen, IntegerExpr, IntegerType,
        InterpolationContent, LocalVariableExpr, LocalVariableWriteTarget, NilExpr, NilStyle,
        NilType, Paren, ParenParen, Program, RegexpExpr, RegexpType, SelfExpr, SeqExpr, Stmt,
        StmtSep, StmtSepKind, StringContent, StringExpr, StringType, TextContent, TrueExpr,
        TrueType, Type, TypeAnnotation, WriteExpr, WriteTarget, XStringExpr, DUMMY_RANGE,
    },
    encoding::EStrRef,
    Diagnostic, EString,
};
use lexer::{Lexer, LexerState, StringDelimiter, Token, TokenKind};

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
    next_token: Option<Token>,
}

impl<'a> Parser<'a> {
    fn new(input: EStrRef<'a>) -> Self {
        Self {
            lexer: Lexer::new(input),
            next_token: None,
        }
    }

    fn input(&self) -> EStrRef<'a> {
        self.lexer.input()
    }

    fn bytes(&self) -> &'a [u8] {
        self.lexer.bytes()
    }

    fn parse_whole_program(&mut self, diag: &mut Vec<Diagnostic>, locals: &[EString]) -> Program {
        let mut lv = LVCtx::new();
        for local in locals {
            lv.push(local.clone());
        }
        let lv_checkpoint = lv.checkpoint();
        let body = self.parse_stmt_list(diag, &mut lv);
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

    fn parse_stmt_list(&mut self, diag: &mut Vec<Diagnostic>, lv: &mut LVCtx) -> Expr {
        let mut separator_prefix = Vec::<StmtSep>::new();
        let mut stmts = Vec::<Stmt>::new();
        let mut range = DUMMY_RANGE;
        loop {
            let token = self.fill_token(diag, LexerState::Begin);
            match token.kind {
                TokenKind::EOF
                | TokenKind::RParen
                | TokenKind::RBrace
                | TokenKind::RBracket
                | TokenKind::KeywordEnd => break,
                TokenKind::Semicolon | TokenKind::Newline => {
                    range |= token.range;
                    self.bump();
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
                    let expr = self.parse_expr_lv_cmd(diag, lv);
                    range |= *expr.outer_range();
                    let separator_prefix = take(&mut separator_prefix);
                    stmts.push(Stmt {
                        range: *expr.outer_range(),
                        expr,
                        separator_prefix,
                        separator_suffix: Vec::new(),
                    });
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
        if stmts.len() == 1 {
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
        }
    }

    fn parse_whole_expr(&mut self, diag: &mut Vec<Diagnostic>, locals: &[EString]) -> Expr {
        let mut lv = LVCtx::new();
        for local in locals {
            lv.push(local.clone());
        }
        let expr = self.parse_expr_lv_cmd(diag, &mut lv);
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

    fn parse_expr_lv_cmd(&mut self, diag: &mut Vec<Diagnostic>, lv: &mut LVCtx) -> Expr {
        let lhs = self.parse_expr_lv_assignment(diag, lv, PrecCtx::default());
        let token = self.fill_token(diag, LexerState::End);
        if is_cmdarg_begin(&token) {
            let (args, args_range) = self.parse_cmd_args(diag, lv);
            let lhs = lhs.callify();
            match lhs {
                ExprLike::ArglessCall {
                    range,
                    style,
                    private,
                    optional,
                    receiver,
                    method,
                    method_range,
                } => CallExpr {
                    range: range | args_range,
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
                    if args.len() > 1 {
                        diag.push(Diagnostic {
                            range: args_range,
                            message: format!("too many arguments"),
                        });
                    }
                    let arg = { args }.swap_remove(0);
                    CallExpr {
                        range: range | args_range,
                        parens: Vec::new(),

                        style: CallStyle::SpelloutUnOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(arg),
                        method: symbol("!"),
                        method_range: token.range,
                        args: vec![],
                    }
                    .into()
                }
                _ => {
                    diag.push(Diagnostic {
                        range: token.range,
                        message: format!("this expression cannot be called"),
                    });
                    let lhs = lhs.into_expr(diag);
                    CallExpr {
                        range: *lhs.outer_range() | args_range,
                        parens: Vec::new(),

                        style: CallStyle::CallOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(lhs),
                        method: symbol("call"),
                        method_range: token.range,
                        args,
                    }
                    .into()
                }
            }
        } else {
            lhs.into_expr(diag)
        }
    }

    fn parse_expr_lv_assignment(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let lhs = self.parse_expr_lv_eq(diag, lv, prec);
        let token = self.fill_token(diag, LexerState::End);
        match token.kind {
            TokenKind::Eq => {
                self.bump();
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
                let rhs = self.parse_expr_lv_assignment(diag, lv, prec.with_invalid_command());
                let rhs = rhs.into_expr(diag);
                let expr: Expr = WriteExpr {
                    range: *lhs.range() | *rhs.outer_range(),
                    parens: Vec::new(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
                .into();
                return ExprLike::Expr(expr);
            }
            _ => {}
        }
        lhs
    }

    fn parse_expr_lv_eq(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let mut lhs = self.parse_expr_lv_ineq(diag, lv, prec);
        let mut count = 0;
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::EqEq
                | TokenKind::ExclEq
                | TokenKind::EqEqEq
                | TokenKind::EqMatch
                | TokenKind::ExclTilde => {
                    count += 1;
                    if count > 1 {
                        diag.push(Diagnostic {
                            range: token.range,
                            message: format!("these operators cannot be chained"),
                        });
                    }
                    let meth = match token.kind {
                        TokenKind::EqEq => "==",
                        TokenKind::ExclEq => "!=",
                        TokenKind::EqEqEq => "===",
                        TokenKind::EqMatch => "=~",
                        TokenKind::ExclTilde => "!~",
                        _ => unimplemented!(),
                    };
                    self.bump();
                    let rhs = self
                        .parse_expr_lv_ineq(diag, lv, prec.with_invalid_command())
                        .into_expr(diag);
                    let lhs_expr = lhs.into_expr(diag);
                    lhs = ExprLike::Expr(
                        CallExpr {
                            range: *lhs_expr.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(lhs_expr),
                            method: symbol(meth),
                            method_range: token.range,
                            args: vec![rhs],
                        }
                        .into(),
                    );
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_expr_lv_ineq(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let mut lhs = self.parse_expr_lv_bitwise_or(diag, lv, prec);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => {
                    let meth = match token.kind {
                        TokenKind::Lt => "<",
                        TokenKind::LtEq => "<=",
                        TokenKind::Gt => ">",
                        TokenKind::GtEq => ">=",
                        _ => unimplemented!(),
                    };
                    self.bump();
                    let rhs = self
                        .parse_expr_lv_bitwise_or(diag, lv, prec.with_invalid_command())
                        .into_expr(diag);
                    let lhs_expr = lhs.into_expr(diag);
                    lhs = ExprLike::Expr(
                        CallExpr {
                            range: *lhs_expr.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(lhs_expr),
                            method: symbol(meth),
                            method_range: token.range,
                            args: vec![rhs],
                        }
                        .into(),
                    );
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_expr_lv_bitwise_or(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let mut lhs = self.parse_expr_lv_bitwise_and(diag, lv, prec);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::Vert | TokenKind::Caret => {
                    let meth = match token.kind {
                        TokenKind::Vert => "|",
                        TokenKind::Caret => "^",
                        _ => unimplemented!(),
                    };
                    self.bump();
                    if token.kind == TokenKind::Vert {
                        self.fill_token(diag, LexerState::BeginLabelable);
                    }
                    let rhs = self
                        .parse_expr_lv_bitwise_and(diag, lv, prec.with_invalid_command())
                        .into_expr(diag);
                    let lhs_expr = lhs.into_expr(diag);
                    lhs = ExprLike::Expr(
                        CallExpr {
                            range: *lhs_expr.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(lhs_expr),
                            method: symbol(meth),
                            method_range: token.range,
                            args: vec![rhs],
                        }
                        .into(),
                    );
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_expr_lv_bitwise_and(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let mut lhs = self.parse_expr_lv_shift(diag, lv, prec);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::Amp => {
                    let meth = match token.kind {
                        TokenKind::Amp => "&",
                        _ => unimplemented!(),
                    };
                    self.bump();
                    let rhs = self
                        .parse_expr_lv_shift(diag, lv, prec.with_invalid_command())
                        .into_expr(diag);
                    let lhs_expr = lhs.into_expr(diag);
                    lhs = ExprLike::Expr(
                        CallExpr {
                            range: *lhs_expr.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(lhs_expr),
                            method: symbol(meth),
                            method_range: token.range,
                            args: vec![rhs],
                        }
                        .into(),
                    );
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_expr_lv_shift(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let mut lhs = self.parse_expr_lv_additive(diag, lv, prec);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::LtLt | TokenKind::GtGt => {
                    let meth = match token.kind {
                        TokenKind::LtLt => "<<",
                        TokenKind::GtGt => ">>",
                        _ => unimplemented!(),
                    };
                    self.bump();
                    let rhs = self
                        .parse_expr_lv_additive(diag, lv, prec.with_invalid_command())
                        .into_expr(diag);
                    let lhs_expr = lhs.into_expr(diag);
                    lhs = ExprLike::Expr(
                        CallExpr {
                            range: *lhs_expr.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(lhs_expr),
                            method: symbol(meth),
                            method_range: token.range,
                            args: vec![rhs],
                        }
                        .into(),
                    );
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_expr_lv_additive(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let mut lhs = self.parse_expr_lv_multiplicative(diag, lv, prec);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    let meth = match token.kind {
                        TokenKind::Plus => "+",
                        TokenKind::Minus => "-",
                        _ => unimplemented!(),
                    };
                    self.bump();
                    let rhs = self
                        .parse_expr_lv_multiplicative(diag, lv, prec.with_invalid_command())
                        .into_expr(diag);
                    let lhs_expr = lhs.into_expr(diag);
                    lhs = ExprLike::Expr(
                        CallExpr {
                            range: *lhs_expr.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(lhs_expr),
                            method: symbol(meth),
                            method_range: token.range,
                            args: vec![rhs],
                        }
                        .into(),
                    );
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_expr_lv_multiplicative(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let mut lhs = self.parse_expr_lv_exponential(diag, lv, prec);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
                    let meth = match token.kind {
                        TokenKind::Star => "*",
                        TokenKind::Slash => "/",
                        TokenKind::Percent => "%",
                        _ => unimplemented!(),
                    };
                    self.bump();
                    let rhs = self
                        .parse_expr_lv_exponential(diag, lv, prec.with_invalid_command())
                        .into_expr(diag);
                    let lhs_expr = lhs.into_expr(diag);
                    lhs = ExprLike::Expr(
                        CallExpr {
                            range: *lhs_expr.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(lhs_expr),
                            method: symbol(meth),
                            method_range: token.range,
                            args: vec![rhs],
                        }
                        .into(),
                    );
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_expr_lv_exponential(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let lhs = self.parse_expr_lv_unary(diag, lv, prec);
        let token = self.fill_token(diag, LexerState::End);
        match token.kind {
            TokenKind::StarStar => {
                let meth = match token.kind {
                    TokenKind::StarStar => "**",
                    _ => unimplemented!(),
                };
                self.bump();
                let rhs = self
                    .parse_expr_lv_exponential(diag, lv, prec.with_invalid_command())
                    .into_expr(diag);
                match self.reparse_minus(lhs.into_expr(diag)) {
                    Ok((minus_range, lhs)) => ExprLike::Expr(
                        CallExpr {
                            range: minus_range | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::UnOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(
                                CallExpr {
                                    range: *lhs.outer_range() | *rhs.outer_range(),
                                    parens: Vec::new(),

                                    style: CallStyle::BinOp,
                                    private: false,
                                    optional: false,
                                    receiver: Box::new(lhs),
                                    method: symbol(meth),
                                    method_range: token.range,
                                    args: vec![rhs],
                                }
                                .into(),
                            ),
                            method: symbol("-@"),
                            method_range: minus_range,
                            args: vec![],
                        }
                        .into(),
                    ),
                    Err(lhs) => ExprLike::Expr(
                        CallExpr {
                            range: *lhs.outer_range() | *rhs.outer_range(),
                            parens: Vec::new(),

                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(lhs),
                            method: symbol(meth),
                            method_range: token.range,
                            args: vec![rhs],
                        }
                        .into(),
                    ),
                }
            }
            _ => lhs,
        }
    }

    fn reparse_minus(&self, expr: Expr) -> Result<(CodeRange, Expr), Expr> {
        // TODO: also split numeric literals
        match expr {
            Expr::Call(expr)
                if expr.method.as_estr() == symbol_ref("-@")
                    && matches!(expr.style, CallStyle::UnOp)
                    && expr.parens.is_empty() =>
            {
                Ok((expr.method_range, *expr.receiver))
            }
            _ => Err(expr),
        }
    }

    fn parse_expr_lv_unary(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let token = self.fill_token(diag, LexerState::Begin);
        match token.kind {
            TokenKind::Plus
            | TokenKind::PlusPrefix
            | TokenKind::Minus
            | TokenKind::MinusPrefix
            | TokenKind::Excl
            | TokenKind::Tilde => {
                let meth = match token.kind {
                    TokenKind::Plus | TokenKind::PlusPrefix => "+@",
                    TokenKind::Minus | TokenKind::MinusPrefix => "-@",
                    TokenKind::Excl => "!",
                    TokenKind::Tilde => "~",
                    _ => unreachable!(),
                };
                self.bump();
                let inner = self
                    .parse_expr_lv_unary(diag, lv, prec.with_invalid_command())
                    .into_expr(diag);
                ExprLike::Expr(
                    CallExpr {
                        range: token.range | *inner.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::UnOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(inner),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![],
                    }
                    .into(),
                )
            }
            _ => self.parse_expr_lv_call_like_absorb_cmds(diag, lv, prec),
        }
    }

    fn parse_expr_lv_call_like_absorb_cmds(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
        prec: PrecCtx,
    ) -> ExprLike {
        let lhs = self.parse_expr_lv_call_like(diag, lv);
        if prec.invalid_command {
            let token = self.fill_token(diag, LexerState::End);
            if is_cmdarg_begin(&token) {
                diag.push(Diagnostic {
                    range: token.range,
                    message: format!("non-parenthesized calls are not allowed in this context"),
                });
                let (args, args_range) = self.parse_cmd_args(diag, lv);
                let lhs_expr = lhs.into_expr(diag);
                return ExprLike::Expr(
                    CallExpr {
                        range: *lhs_expr.outer_range() | args_range,
                        parens: Vec::new(),

                        style: CallStyle::CallOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(lhs_expr),
                        method: symbol("call"),
                        method_range: token.range,
                        args,
                    }
                    .into(),
                );
            }
        }
        lhs
    }

    fn parse_expr_lv_call_like(&mut self, diag: &mut Vec<Diagnostic>, lv: &mut LVCtx) -> ExprLike {
        let mut lhs = self.parse_expr_lv_primary(diag, lv);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::LParen => {
                    let (args, args_range) = self.parse_paren_args(diag, lv);
                    lhs = lhs.callify();
                    match lhs {
                        ExprLike::ArglessCall {
                            range,
                            style,
                            private,
                            optional,
                            receiver,
                            method,
                            method_range,
                        } => {
                            lhs = ExprLike::BlocklessCall {
                                range: range | args_range,
                                style,
                                private,
                                optional,
                                receiver,
                                method,
                                method_range,
                                args,
                            };
                        }
                        ExprLike::NotOp { range } => {
                            if args.len() == 0 {
                                // `not()` is `nil.!()` (but why!?!?)
                                lhs = ExprLike::Expr(
                                    CallExpr {
                                        range: range | args_range,
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
                                        method_range: token.range,
                                        args: vec![],
                                    }
                                    .into(),
                                );
                            } else {
                                // TODO: also check against `not(expr,)` etc.
                                if args.len() > 1 {
                                    diag.push(Diagnostic {
                                        range: args_range,
                                        message: format!("too many arguments"),
                                    });
                                }
                                let arg = { args }.swap_remove(0);
                                lhs = ExprLike::Expr(
                                    CallExpr {
                                        range: range | args_range,
                                        parens: Vec::new(),

                                        style: CallStyle::SpelloutUnOp,
                                        private: false,
                                        optional: false,
                                        receiver: Box::new(arg),
                                        method: symbol("!"),
                                        method_range: token.range,
                                        args: vec![],
                                    }
                                    .into(),
                                );
                            }
                        }
                        _ => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("Need a dot to call an expression"),
                            });
                            let lhs_expr = lhs.into_expr(diag);
                            lhs = ExprLike::Expr(
                                CallExpr {
                                    range: *lhs_expr.outer_range() | args_range,
                                    parens: Vec::new(),

                                    style: CallStyle::CallOp,
                                    private: false,
                                    optional: false,
                                    receiver: Box::new(lhs_expr),
                                    method: symbol("call"),
                                    method_range: token.range,
                                    args,
                                }
                                .into(),
                            );
                        }
                    }
                }
                TokenKind::Dot | TokenKind::AmpDot | TokenKind::ColonColon => {
                    let private = if let ExprLike::Expr(Expr::Self_(expr)) = &lhs {
                        expr.parens.is_empty()
                    } else {
                        false
                    };
                    let optional = matches!(token.kind, TokenKind::AmpDot);
                    let const_like = matches!(token.kind, TokenKind::ColonColon);
                    self.bump();
                    let token = self.fill_token(diag, LexerState::MethForCall);
                    match token.kind {
                        TokenKind::LParen => {
                            // expr.(args)
                            let (args, args_range) = self.parse_paren_args(diag, lv);
                            let lhs_expr = lhs.into_expr(diag);
                            lhs = ExprLike::BlocklessCall {
                                range: *lhs_expr.outer_range() | args_range,
                                style: CallStyle::Dot,
                                private,
                                optional,
                                receiver: Box::new(lhs_expr),
                                method: symbol("call"),
                                method_range: token.range,
                                args,
                            };
                        }
                        TokenKind::Const if const_like => {
                            self.bump();
                            // expr::Const
                            let s = self.select(token.range);
                            let lhs_expr = lhs.into_expr(diag);
                            lhs = ExprLike::Const {
                                range: *lhs_expr.outer_range() | token.range,
                                private,
                                receiver: ConstReceiver::Expr(Box::new(lhs_expr)),
                                name: s.to_estring().asciified(),
                            };
                        }
                        TokenKind::Identifier | TokenKind::Const | TokenKind::MethodName => {
                            self.bump();
                            // expr.meth
                            let s = self.select(token.range);
                            let lhs_expr = lhs.into_expr(diag);
                            lhs = ExprLike::ArglessCall {
                                range: *lhs_expr.outer_range() | token.range,
                                style: CallStyle::Dot,
                                private,
                                optional,
                                receiver: Box::new(lhs_expr),
                                method: s.to_estring().asciified(),
                                method_range: token.range,
                            };
                        }
                        _ => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("unexpected token"),
                            });
                            let lhs_expr = lhs.into_expr(diag);
                            lhs = ExprLike::ArglessCall {
                                range: *lhs_expr.outer_range() | token.range,
                                style: CallStyle::Dot,
                                private,
                                optional,
                                receiver: Box::new(lhs_expr),
                                method: symbol(""),
                                method_range: token.range,
                            };
                        }
                    }
                }
                TokenKind::At => {
                    self.bump();
                    let ty = self.parse_type(diag);
                    if let ExprLike::Identifier { has_local, .. } = &mut lhs {
                        *has_local = true;
                    }
                    let lhs_expr = lhs.into_expr(diag);
                    lhs = ExprLike::Expr(match lhs_expr {
                        Expr::LocalVariable(mut e) => {
                            let ty_range = *ty.range();
                            e.type_annotation = Some(TypeAnnotation {
                                range: token.range | ty_range,
                                type_: ty,
                            });
                            e.range = e.range | ty_range;
                            e.into()
                        }
                        _ => {
                            diag.push(Diagnostic {
                                range: token.range | *ty.range(),
                                message: format!("non-annotatable expression"),
                            });
                            lhs_expr
                        }
                    });
                }
                _ => break,
            }
        }

        lhs
    }

    fn parse_expr_lv_primary(&mut self, diag: &mut Vec<Diagnostic>, lv: &mut LVCtx) -> ExprLike {
        let token = self.fill_token(diag, LexerState::Begin);
        match token.kind {
            TokenKind::KeywordNil => {
                self.bump();
                ExprLike::Expr(
                    NilExpr {
                        range: token.range,
                        parens: Vec::new(),

                        style: NilStyle::Keyword,
                    }
                    .into(),
                )
            }
            TokenKind::KeywordFalse => {
                self.bump();
                ExprLike::Expr(
                    FalseExpr {
                        range: token.range,
                        parens: Vec::new(),
                    }
                    .into(),
                )
            }
            TokenKind::KeywordTrue => {
                self.bump();
                ExprLike::Expr(
                    TrueExpr {
                        range: token.range,
                        parens: Vec::new(),
                    }
                    .into(),
                )
            }
            TokenKind::Identifier => {
                self.bump();
                let s = self.select(token.range);
                let name = s.to_estring().asciified();
                let has_local = lv.has(&name);
                ExprLike::Identifier {
                    range: token.range,
                    has_local,
                    name,
                }
            }
            TokenKind::Const => {
                self.bump();
                let s = self.select(token.range);
                ExprLike::Const {
                    receiver: ConstReceiver::None,
                    private: true,
                    range: token.range,
                    name: s.to_estring().asciified(),
                }
            }
            TokenKind::MethodName => {
                self.bump();
                let s = self.select(token.range);
                ExprLike::ArglessCall {
                    range: token.range,
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
                    method_range: token.range,
                }
            }
            TokenKind::KeywordNot => {
                self.bump();
                let token2 = self.fill_token(diag, LexerState::FirstArgument);
                if matches!(token2.kind, TokenKind::Newline) {
                    self.bump();
                    self.fill_token(diag, LexerState::Begin);
                }
                ExprLike::NotOp { range: token.range }
            }
            TokenKind::Integer => {
                self.bump();
                ExprLike::Expr(
                    IntegerExpr {
                        range: token.range,
                        parens: Vec::new(),
                        value: String::from_utf8_lossy(self.select(token.range).as_bytes())
                            .parse()
                            .unwrap(),
                    }
                    .into(),
                )
            }
            TokenKind::StringBegin => {
                self.bump();
                let delim = match self.bytes()[token.range.start] {
                    b'\'' => StringDelimiter::Quote,
                    b'"' => StringDelimiter::DoubleQuote,
                    b'`' => StringDelimiter::Backtick,
                    b'/' => StringDelimiter::Slash,
                    _ => unreachable!(),
                };
                let mut contents = Vec::<StringContent>::new();
                let close_range;
                loop {
                    let token = self.fill_token(diag, LexerState::StringLike(delim));
                    match token.kind {
                        TokenKind::StringEnd => {
                            self.bump();
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
                            self.bump();
                            let s = self.select(token.range);
                            contents.push(StringContent::Text(TextContent {
                                range: token.range,
                                // TODO: unescape
                                value: s.to_estring().asciified(),
                            }));
                        }
                        TokenKind::StringInterpolationBegin => {
                            self.bump();
                            let open_range = token.range;
                            let inner = self.parse_stmt_list(diag, lv);
                            let token = self.fill_token(diag, LexerState::End);
                            if let TokenKind::RBrace = token.kind {
                                self.bump();
                            } else {
                                diag.push(Diagnostic {
                                    range: token.range,
                                    message: format!("expected '}}'"),
                                });
                            }
                            contents.push(StringContent::Interpolation(InterpolationContent {
                                range: open_range | token.range,
                                open_range,
                                close_range: token.range,
                                expr: inner,
                            }));
                        }
                        // TokenKind::StringVarInterpolation => {}
                        _ => {
                            self.bump();
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("unexpected token"),
                            });
                        }
                    }
                }
                ExprLike::Expr(match delim {
                    StringDelimiter::Quote | StringDelimiter::DoubleQuote => StringExpr {
                        range: token.range | close_range,
                        parens: Vec::new(),
                        open_range: token.range,
                        close_range,
                        contents,
                    }
                    .into(),
                    StringDelimiter::Backtick => XStringExpr {
                        range: token.range | close_range,
                        parens: Vec::new(),
                        open_range: token.range,
                        close_range,
                        contents,
                    }
                    .into(),
                    StringDelimiter::Slash => RegexpExpr {
                        range: token.range | close_range,
                        parens: Vec::new(),
                        open_range: token.range,
                        close_range,
                        contents,
                    }
                    .into(),
                })
            }
            TokenKind::LParen => {
                let open_range = token.range;
                self.bump();
                self.fill_token(diag, LexerState::BeginLabelable);
                let expr = self.parse_stmt_list(diag, lv);
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
                ExprLike::Expr(expr)
            }
            _ => {
                self.bump();
                diag.push(Diagnostic {
                    range: token.range,
                    message: format!("unexpected token"),
                });
                ExprLike::Expr(
                    ErrorExpr {
                        range: token.range,
                        parens: Vec::new(),
                    }
                    .into(),
                )
            }
        }
    }

    fn parse_cmd_args(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
    ) -> (Vec<Expr>, CodeRange) {
        let first_arg = self.parse_expr_lv_cmd(diag, lv);
        let mut args_range = *first_arg.outer_range();
        let mut args = vec![first_arg];
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                    diag.push(Diagnostic {
                        range: token.range,
                        message: format!("unexpected token"),
                    });
                    break;
                }
                TokenKind::Newline | TokenKind::Semicolon | TokenKind::EOF => {
                    break;
                }
                TokenKind::Comma => {
                    self.bump();
                    self.fill_token(diag, LexerState::BeginLabelable);
                    let arg = self.parse_expr_lv_cmd(diag, lv);
                    args_range |= *arg.outer_range();
                    args.push(arg);
                }
                _ => {
                    diag.push(Diagnostic {
                        range: token.range,
                        message: format!("unexpected token"),
                    });
                    break;
                }
            }
        }
        (args, args_range)
    }

    fn parse_paren_args(
        &mut self,
        diag: &mut Vec<Diagnostic>,
        lv: &mut LVCtx,
    ) -> (Vec<Expr>, CodeRange) {
        // Bump the first `(`
        let token = self.next_token.unwrap();
        self.bump();
        let mut args = Vec::<Expr>::new();
        let mut args_range = token.range;
        loop {
            let token = self.fill_token(diag, LexerState::BeginLabelable);
            match token.kind {
                TokenKind::RParen => {
                    self.bump();
                    args_range |= token.range;
                    break;
                }
                TokenKind::EOF => {
                    diag.push(Diagnostic {
                        range: token.range,
                        message: format!("unexpected end of file"),
                    });
                    args_range |= token.range;
                    break;
                }
                _ => {
                    let arg = self.parse_expr_lv_cmd(diag, lv);
                    args.push(arg);
                    let token = self.fill_token(diag, LexerState::End);
                    match token.kind {
                        TokenKind::Comma => {
                            self.bump();
                        }
                        TokenKind::EOF | TokenKind::RParen => {}
                        _ => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("unexpected token"),
                            });
                        }
                    }
                }
            }
        }
        (args, args_range)
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
            TokenKind::KeywordNil => NilType { range: token.range }.into(),
            TokenKind::KeywordFalse => FalseType { range: token.range }.into(),
            TokenKind::KeywordTrue => TrueType { range: token.range }.into(),
            TokenKind::Const => {
                let s = self.select(token.range);
                match s.as_bytes() {
                    b"NilClass" => NilType { range: token.range }.into(),
                    b"FalseClass" => FalseType { range: token.range }.into(),
                    b"TrueClass" => TrueType { range: token.range }.into(),
                    b"Integer" => IntegerType { range: token.range }.into(),
                    b"String" => StringType { range: token.range }.into(),
                    b"Regexp" => RegexpType { range: token.range }.into(),
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

    fn select(&self, range: CodeRange) -> EStrRef<'a> {
        EStrRef::from_bytes(&self.bytes()[range.range()], self.input().encoding())
    }
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
        args: Vec<Expr>,
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
                args: vec![],
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
                    args: vec![],
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
                args: vec![],
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
        | TokenKind::IvarName
        | TokenKind::CvarName
        | TokenKind::GvarName
        | TokenKind::Integer
        | TokenKind::Float
        | TokenKind::Rational
        | TokenKind::Imaginary
        | TokenKind::CharLiteral
        | TokenKind::StringBegin
        | TokenKind::Excl
        | TokenKind::AmpPrefix
        | TokenKind::LParenRestricted
        | TokenKind::StarPrefix
        | TokenKind::StarStarPrefix
        | TokenKind::PlusPrefix
        | TokenKind::MinusPrefix
        | TokenKind::Arrow
        | TokenKind::ColonColonPrefix
        | TokenKind::LBracketPrefix
        | TokenKind::Tilde => true,

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
        | TokenKind::OpAssign
        | TokenKind::ExclEq
        | TokenKind::ExclTilde
        | TokenKind::Percent
        | TokenKind::Amp
        | TokenKind::AmpAmp
        | TokenKind::AmpDot
        | TokenKind::RParen
        | TokenKind::Star
        | TokenKind::StarStar
        | TokenKind::Plus
        | TokenKind::Comma
        | TokenKind::Minus
        | TokenKind::Dot
        | TokenKind::DotDot
        | TokenKind::DotDotDot
        | TokenKind::Slash
        | TokenKind::Colon
        | TokenKind::ColonColon
        | TokenKind::Semicolon
        | TokenKind::Newline
        | TokenKind::Lt
        | TokenKind::LtLt
        | TokenKind::LtEq
        | TokenKind::LtEqGt
        | TokenKind::Eq
        | TokenKind::EqEq
        | TokenKind::EqEqEq
        | TokenKind::FatArrow
        | TokenKind::EqMatch
        | TokenKind::Gt
        | TokenKind::GtEq
        | TokenKind::GtGt
        | TokenKind::Question
        | TokenKind::At
        | TokenKind::LBracket
        | TokenKind::RBracket
        | TokenKind::Caret
        | TokenKind::Vert
        | TokenKind::VertVert
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
mod tests {
    #[allow(unused)]
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::ast::{pos_in, IntegerExpr, ParenParen};

    use super::*;

    fn p_program(input: EStrRef<'_>, locals: &[&str]) -> (Program, Vec<Diagnostic>) {
        let locals = locals.iter().map(|&s| symbol(s)).collect::<Vec<_>>();
        let mut diag = Vec::new();
        let program = parse(&mut diag, input, &locals);
        (program, diag)
    }

    fn p_expr(input: EStrRef<'_>, locals: &[&str]) -> (Expr, Vec<Diagnostic>) {
        let locals = locals.iter().map(|&s| symbol(s)).collect::<Vec<_>>();
        let mut diag = Vec::new();
        let expr = parse_expr(&mut diag, input, &locals);
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
            p_program(src, &["x", "y", "z"]),
            (
                Program {
                    range: pos_in(src, b"x; y\nz", 0),
                    locals: vec![],
                    body: SeqExpr {
                        range: pos_in(src, b"x; y\nz", 0),
                        parens: vec![],
                        statements: vec![
                            Stmt {
                                range: pos_in(src, b"x;", 0),
                                separator_prefix: vec![],
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"x", 0),
                                    parens: vec![],
                                    name: symbol("x"),
                                    type_annotation: None,
                                }
                                .into(),
                                separator_suffix: vec![StmtSep {
                                    range: pos_in(src, b";", 0),
                                    kind: StmtSepKind::Semicolon,
                                }],
                            },
                            Stmt {
                                range: pos_in(src, b"y\n", 0),
                                separator_prefix: vec![],
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"y", 0),
                                    parens: vec![],
                                    name: symbol("y"),
                                    type_annotation: None,
                                }
                                .into(),
                                separator_suffix: vec![StmtSep {
                                    range: pos_in(src, b"\n", 0),
                                    kind: StmtSepKind::Newline,
                                }],
                            },
                            Stmt {
                                range: pos_in(src, b"z", 0),
                                separator_prefix: vec![],
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"z", 0),
                                    parens: vec![],
                                    name: symbol("z"),
                                    type_annotation: None,
                                }
                                .into(),
                                separator_suffix: vec![],
                            },
                        ],
                    }
                    .into(),
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
            p_expr(src, &["x"]),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x", 0),
                    parens: vec![ParenParen {
                        range: pos_in(src, b"(x)", 0),
                        open_range: pos_in(src, b"(", 0),
                        separator_prefix: vec![],
                        separator_suffix: vec![],
                        close_range: pos_in(src, b")", 0),
                    }
                    .into()],
                    name: symbol("x"),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("(\nx\n)");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x", 0),
                    parens: vec![ParenParen {
                        range: pos_in(src, b"(\nx\n)", 0),
                        open_range: pos_in(src, b"(", 0),
                        separator_prefix: vec![],
                        separator_suffix: vec![StmtSep {
                            range: pos_in(src, b"\n", 1),
                            kind: StmtSepKind::Newline,
                        }],
                        close_range: pos_in(src, b")", 0),
                    }
                    .into()],
                    name: symbol("x"),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("(x;)");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x", 0),
                    parens: vec![ParenParen {
                        range: pos_in(src, b"(x;)", 0),
                        open_range: pos_in(src, b"(", 0),
                        separator_prefix: vec![],
                        separator_suffix: vec![StmtSep {
                            range: pos_in(src, b";", 0),
                            kind: StmtSepKind::Semicolon,
                        }],
                        close_range: pos_in(src, b")", 0),
                    }
                    .into()],
                    name: symbol("x"),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("(;x)");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x", 0),
                    parens: vec![ParenParen {
                        range: pos_in(src, b"(;x)", 0),
                        open_range: pos_in(src, b"(", 0),
                        separator_prefix: vec![StmtSep {
                            range: pos_in(src, b";", 0),
                            kind: StmtSepKind::Semicolon,
                        }],
                        separator_suffix: vec![],
                        close_range: pos_in(src, b")", 0),
                    }
                    .into()],
                    name: symbol("x"),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_parenthesized_seq_expr() {
        let src = EStrRef::from("(x\ny)");
        assert_eq!(
            p_expr(src, &["x", "y"]),
            (
                SeqExpr {
                    range: pos_in(src, b"x\ny", 0),
                    parens: vec![ParenParen {
                        range: pos_in(src, b"(x\ny)", 0),
                        open_range: pos_in(src, b"(", 0),
                        separator_prefix: vec![],
                        separator_suffix: vec![],
                        close_range: pos_in(src, b")", 0),
                    }
                    .into()],
                    statements: vec![
                        Stmt {
                            range: pos_in(src, b"x\n", 0),
                            separator_prefix: vec![],
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"x", 0),
                                parens: vec![],
                                name: symbol("x"),
                                type_annotation: None,
                            }
                            .into(),
                            separator_suffix: vec![StmtSep {
                                range: pos_in(src, b"\n", 0),
                                kind: StmtSepKind::Newline
                            }],
                        },
                        Stmt {
                            range: pos_in(src, b"y", 0),
                            separator_prefix: vec![],
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"y", 0),
                                parens: vec![],
                                name: symbol("y"),
                                type_annotation: None,
                            }
                            .into(),
                            separator_suffix: vec![],
                        }
                    ],
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
            p_expr(src, &[]),
            (
                NilExpr {
                    range: pos_in(src, b"nil", 0),
                    parens: vec![],

                    style: NilStyle::Keyword,
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
            p_expr(src, &[]),
            (
                FalseExpr {
                    range: pos_in(src, b"false", 0),
                    parens: vec![],
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("true");
        assert_eq!(
            p_expr(src, &[]),
            (
                TrueExpr {
                    range: pos_in(src, b"true", 0),
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
            p_expr(src, &["x"]),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x", 0),
                    parens: vec![],
                    name: symbol("x"),
                    type_annotation: None,
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("x @ Integer");
        assert_eq!(
            p_expr(src, &[]),
            (
                LocalVariableExpr {
                    range: pos_in(src, b"x @ Integer", 0),
                    parens: vec![],
                    name: symbol("x"),
                    type_annotation: Some(TypeAnnotation {
                        range: pos_in(src, b"@ Integer", 0),
                        type_: IntegerType {
                            range: pos_in(src, b"Integer", 0)
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
            p_expr(src, &[]),
            (
                IntegerExpr {
                    range: pos_in(src, b"42", 0),
                    parens: vec![],
                    value: 42,
                }
                .into(),
                vec![],
            )
        )
    }

    #[test]
    fn test_parse_string_expr() {
        let src = EStrRef::from("'foo'");
        assert_eq!(
            p_expr(src, &[]),
            (
                StringExpr {
                    range: pos_in(src, b"'foo'", 0),
                    parens: vec![],
                    open_range: pos_in(src, b"'", 0),
                    close_range: pos_in(src, b"'", 1),
                    contents: vec![StringContent::Text(TextContent {
                        range: pos_in(src, b"foo", 0),
                        value: symbol("foo"),
                    })],
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("\"foo\"");
        assert_eq!(
            p_expr(src, &[]),
            (
                StringExpr {
                    range: pos_in(src, b"\"foo\"", 0),
                    parens: vec![],
                    open_range: pos_in(src, b"\"", 0),
                    close_range: pos_in(src, b"\"", 1),
                    contents: vec![StringContent::Text(TextContent {
                        range: pos_in(src, b"foo", 0),
                        value: symbol("foo"),
                    })],
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("\"foo #{bar} baz\"");
        assert_eq!(
            p_expr(src, &["bar"]),
            (
                StringExpr {
                    range: pos_in(src, b"\"foo #{bar} baz\"", 0),
                    parens: vec![],
                    open_range: pos_in(src, b"\"", 0),
                    close_range: pos_in(src, b"\"", 1),
                    contents: vec![
                        StringContent::Text(TextContent {
                            range: pos_in(src, b"foo ", 0),
                            value: symbol("foo "),
                        }),
                        StringContent::Interpolation(InterpolationContent {
                            range: pos_in(src, b"#{bar}", 0),
                            open_range: pos_in(src, b"#{", 0),
                            close_range: pos_in(src, b"}", 0),
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"bar", 0),
                                parens: vec![],
                                name: symbol("bar"),
                                type_annotation: None,
                            }
                            .into(),
                        }),
                        StringContent::Text(TextContent {
                            range: pos_in(src, b" baz", 0),
                            value: symbol(" baz"),
                        }),
                    ],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_regexp_expr() {
        let src = EStrRef::from("/foo/");
        assert_eq!(
            p_expr(src, &[]),
            (
                RegexpExpr {
                    range: pos_in(src, b"/foo/", 0),
                    parens: vec![],
                    open_range: pos_in(src, b"/", 0),
                    close_range: pos_in(src, b"/", 1),
                    contents: vec![StringContent::Text(TextContent {
                        range: pos_in(src, b"foo", 0),
                        value: symbol("foo"),
                    })],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_xstring_expr() {
        let src = EStrRef::from("`foo`");
        assert_eq!(
            p_expr(src, &[]),
            (
                XStringExpr {
                    range: pos_in(src, b"`foo`", 0),
                    parens: vec![],
                    open_range: pos_in(src, b"`", 0),
                    close_range: pos_in(src, b"`", 1),
                    contents: vec![StringContent::Text(TextContent {
                        range: pos_in(src, b"foo", 0),
                        value: symbol("foo"),
                    })],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_func_call() {
        let src = EStrRef::from("foo()");
        assert_eq!(
            p_expr(src, &[]),
            (
                CallExpr {
                    range: pos_in(src, b"foo()", 0),
                    parens: vec![],
                    style: CallStyle::ImplicitSelf,
                    private: true,
                    optional: false,
                    receiver: Box::new(
                        SelfExpr {
                            range: DUMMY_RANGE,
                            parens: vec![],
                        }
                        .into()
                    ),
                    method: symbol("foo"),
                    method_range: pos_in(src, b"foo", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_cmd_func_call() {
        let src = EStrRef::from("foo 1, 2");
        assert_eq!(
            p_expr(src, &[]),
            (
                CallExpr {
                    range: pos_in(src, b"foo 1, 2", 0),
                    parens: vec![],
                    style: CallStyle::ImplicitSelf,
                    private: true,
                    optional: false,
                    receiver: Box::new(
                        SelfExpr {
                            range: DUMMY_RANGE,
                            parens: vec![],
                        }
                        .into()
                    ),
                    method: symbol("foo"),
                    method_range: pos_in(src, b"foo", 0),
                    args: vec![
                        IntegerExpr {
                            range: pos_in(src, b"1", 0),
                            parens: vec![],
                            value: 1,
                        }
                        .into(),
                        IntegerExpr {
                            range: pos_in(src, b"2", 0),
                            parens: vec![],
                            value: 2,
                        }
                        .into(),
                    ],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_method_call() {
        let src = EStrRef::from("x.foo()");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                CallExpr {
                    range: pos_in(src, b"x.foo()", 0),
                    parens: vec![],
                    style: CallStyle::Dot,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("foo"),
                    method_range: pos_in(src, b"foo", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("x.foo");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                CallExpr {
                    range: pos_in(src, b"x.foo", 0),
                    parens: vec![],
                    style: CallStyle::Dot,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("foo"),
                    method_range: pos_in(src, b"foo", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_cmd_method_call() {
        let src = EStrRef::from("x.foo 1, 2");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                CallExpr {
                    range: pos_in(src, b"x.foo 1, 2", 0),
                    parens: vec![],
                    style: CallStyle::Dot,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("foo"),
                    method_range: pos_in(src, b"foo", 0),
                    args: vec![
                        IntegerExpr {
                            range: pos_in(src, b"1", 0),
                            parens: vec![],
                            value: 1,
                        }
                        .into(),
                        IntegerExpr {
                            range: pos_in(src, b"2", 0),
                            parens: vec![],
                            value: 2,
                        }
                        .into(),
                    ],
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("x.foo");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                CallExpr {
                    range: pos_in(src, b"x.foo", 0),
                    parens: vec![],
                    style: CallStyle::Dot,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("foo"),
                    method_range: pos_in(src, b"foo", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_assignment_expr() {
        let src = EStrRef::from("x = 42");
        assert_eq!(
            p_expr(src, &[]),
            (
                WriteExpr {
                    range: pos_in(src, b"x = 42", 0),
                    parens: vec![],
                    lhs: Box::new(
                        LocalVariableWriteTarget {
                            range: pos_in(src, b"x", 0),
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    rhs: Box::new(
                        IntegerExpr {
                            range: pos_in(src, b"42", 0),
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
            p_expr(src, &[]),
            (
                WriteExpr {
                    range: pos_in(src, b"x @ Integer = 42", 0),
                    parens: vec![],
                    lhs: Box::new(
                        LocalVariableWriteTarget {
                            range: pos_in(src, b"x @ Integer", 0),
                            name: symbol("x"),
                            type_annotation: Some(TypeAnnotation {
                                range: pos_in(src, b"@ Integer", 0),
                                type_: IntegerType {
                                    range: pos_in(src, b"Integer", 0),
                                }
                                .into()
                            }),
                        }
                        .into()
                    ),
                    rhs: Box::new(
                        IntegerExpr {
                            range: pos_in(src, b"42", 0),
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
    fn test_parse_multiplicative_operators() {
        let src = EStrRef::from("x * y / z % w");
        assert_eq!(
            p_expr(src, &["x", "y", "z", "w"]),
            (
                CallExpr {
                    range: pos_in(src, b"x * y / z % w", 0),
                    parens: vec![],
                    style: CallStyle::BinOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"x * y / z", 0),
                            parens: vec![],
                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(
                                CallExpr {
                                    range: pos_in(src, b"x * y", 0),
                                    parens: vec![],
                                    style: CallStyle::BinOp,
                                    private: false,
                                    optional: false,
                                    receiver: Box::new(
                                        LocalVariableExpr {
                                            range: pos_in(src, b"x", 0),
                                            parens: vec![],
                                            name: symbol("x"),
                                            type_annotation: None,
                                        }
                                        .into()
                                    ),
                                    method: symbol("*"),
                                    method_range: pos_in(src, b"*", 0),
                                    args: vec![LocalVariableExpr {
                                        range: pos_in(src, b"y", 0),
                                        parens: vec![],
                                        name: symbol("y"),
                                        type_annotation: None,
                                    }
                                    .into()],
                                }
                                .into()
                            ),
                            method: symbol("/"),
                            method_range: pos_in(src, b"/", 0),
                            args: vec![LocalVariableExpr {
                                range: pos_in(src, b"z", 0),
                                parens: vec![],
                                name: symbol("z"),
                                type_annotation: None,
                            }
                            .into()],
                        }
                        .into()
                    ),
                    method: symbol("%"),
                    method_range: pos_in(src, b"%", 0),
                    args: vec![LocalVariableExpr {
                        range: pos_in(src, b"w", 0),
                        parens: vec![],
                        name: symbol("w"),
                        type_annotation: None,
                    }
                    .into()],
                }
                .into(),
                vec![],
            )
        );

        let src = EStrRef::from("x ** y * z ** w");
        assert_eq!(
            p_expr(src, &["x", "y", "z", "w"]),
            (
                CallExpr {
                    range: pos_in(src, b"x ** y * z ** w", 0),
                    parens: vec![],
                    style: CallStyle::BinOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"x ** y", 0),
                            parens: vec![],
                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(
                                LocalVariableExpr {
                                    range: pos_in(src, b"x", 0),
                                    parens: vec![],
                                    name: symbol("x"),
                                    type_annotation: None,
                                }
                                .into()
                            ),
                            method: symbol("**"),
                            method_range: pos_in(src, b"**", 0),
                            args: vec![LocalVariableExpr {
                                range: pos_in(src, b"y", 0),
                                parens: vec![],
                                name: symbol("y"),
                                type_annotation: None,
                            }
                            .into()],
                        }
                        .into()
                    ),
                    method: symbol("*"),
                    method_range: pos_in(src, b"*", 2),
                    args: vec![CallExpr {
                        range: pos_in(src, b"z ** w", 0),
                        parens: vec![],
                        style: CallStyle::BinOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(
                            LocalVariableExpr {
                                range: pos_in(src, b"z", 0),
                                parens: vec![],
                                name: symbol("z"),
                                type_annotation: None,
                            }
                            .into()
                        ),
                        method: symbol("**"),
                        method_range: pos_in(src, b"**", 1),
                        args: vec![LocalVariableExpr {
                            range: pos_in(src, b"w", 0),
                            parens: vec![],
                            name: symbol("w"),
                            type_annotation: None,
                        }
                        .into()],
                    }
                    .into()],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_exponential_operator() {
        let src = EStrRef::from("x ** y ** z");
        assert_eq!(
            p_expr(src, &["x", "y", "z"]),
            (
                CallExpr {
                    range: pos_in(src, b"x ** y ** z", 0),
                    parens: vec![],
                    style: CallStyle::BinOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("**"),
                    method_range: pos_in(src, b"**", 0),
                    args: vec![CallExpr {
                        range: pos_in(src, b"y ** z", 0),
                        parens: vec![],
                        style: CallStyle::BinOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(
                            LocalVariableExpr {
                                range: pos_in(src, b"y", 0),
                                parens: vec![],
                                name: symbol("y"),
                                type_annotation: None,
                            }
                            .into()
                        ),
                        method: symbol("**"),
                        method_range: pos_in(src, b"**", 1),
                        args: vec![LocalVariableExpr {
                            range: pos_in(src, b"z", 0),
                            parens: vec![],
                            name: symbol("z"),
                            type_annotation: None,
                        }
                        .into()],
                    }
                    .into()],
                }
                .into(),
                vec![],
            )
        );
        let src = EStrRef::from("+x ** +y");
        assert_eq!(
            p_expr(src, &["x", "y"]),
            (
                CallExpr {
                    range: pos_in(src, b"+x ** +y", 0),
                    parens: vec![],
                    style: CallStyle::BinOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"+x", 0),
                            parens: vec![],
                            style: CallStyle::UnOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(
                                LocalVariableExpr {
                                    range: pos_in(src, b"x", 0),
                                    parens: vec![],
                                    name: symbol("x"),
                                    type_annotation: None,
                                }
                                .into()
                            ),
                            method: symbol("+@"),
                            method_range: pos_in(src, b"+", 0),
                            args: vec![],
                        }
                        .into()
                    ),
                    method: symbol("**"),
                    method_range: pos_in(src, b"**", 0),
                    args: vec![CallExpr {
                        range: pos_in(src, b"+y", 0),
                        parens: vec![],
                        style: CallStyle::UnOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(
                            LocalVariableExpr {
                                range: pos_in(src, b"y", 0),
                                parens: vec![],
                                name: symbol("y"),
                                type_annotation: None,
                            }
                            .into()
                        ),
                        method: symbol("+@"),
                        method_range: pos_in(src, b"+", 1),
                        args: vec![],
                    }
                    .into()],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_reparse_minus_exp() {
        let src = EStrRef::from("-x ** -y ** z");
        assert_eq!(
            p_expr(src, &["x", "y", "z"]),
            (
                CallExpr {
                    range: pos_in(src, b"-x ** -y ** z", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"x ** -y ** z", 0),
                            parens: vec![],
                            style: CallStyle::BinOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(
                                LocalVariableExpr {
                                    range: pos_in(src, b"x", 0),
                                    parens: vec![],
                                    name: symbol("x"),
                                    type_annotation: None,
                                }
                                .into()
                            ),
                            method: symbol("**"),
                            method_range: pos_in(src, b"**", 0),
                            args: vec![CallExpr {
                                range: pos_in(src, b"-y ** z", 0),
                                parens: vec![],
                                style: CallStyle::UnOp,
                                private: false,
                                optional: false,
                                receiver: Box::new(
                                    CallExpr {
                                        range: pos_in(src, b"y ** z", 0),
                                        parens: vec![],
                                        style: CallStyle::BinOp,
                                        private: false,
                                        optional: false,
                                        receiver: Box::new(
                                            LocalVariableExpr {
                                                range: pos_in(src, b"y", 0),
                                                parens: vec![],
                                                name: symbol("y"),
                                                type_annotation: None,
                                            }
                                            .into()
                                        ),
                                        method: symbol("**"),
                                        method_range: pos_in(src, b"**", 1),
                                        args: vec![LocalVariableExpr {
                                            range: pos_in(src, b"z", 0),
                                            parens: vec![],
                                            name: symbol("z"),
                                            type_annotation: None,
                                        }
                                        .into()],
                                    }
                                    .into()
                                ),
                                method: symbol("-@"),
                                method_range: pos_in(src, b"-", 1),
                                args: vec![],
                            }
                            .into()],
                        }
                        .into()
                    ),
                    method: symbol("-@"),
                    method_range: pos_in(src, b"-", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_unary_operators() {
        let src = EStrRef::from("+x");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                CallExpr {
                    range: pos_in(src, b"+x", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("+@"),
                    method_range: pos_in(src, b"+", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );

        let src = EStrRef::from("-x");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                CallExpr {
                    range: pos_in(src, b"-x", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("-@"),
                    method_range: pos_in(src, b"-", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );

        let src = EStrRef::from("!x");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                CallExpr {
                    range: pos_in(src, b"!x", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("!"),
                    method_range: pos_in(src, b"!", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );

        let src = EStrRef::from("~x");
        assert_eq!(
            p_expr(src, &["x"]),
            (
                CallExpr {
                    range: pos_in(src, b"~x", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into()
                    ),
                    method: symbol("~"),
                    method_range: pos_in(src, b"~", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );

        let src = EStrRef::from("!-+~0");
        assert_eq!(
            p_expr(src, &[]),
            (
                CallExpr {
                    range: pos_in(src, b"!-+~0", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
                    optional: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"-+~0", 0),
                            parens: vec![],
                            style: CallStyle::UnOp,
                            private: false,
                            optional: false,
                            receiver: Box::new(
                                CallExpr {
                                    range: pos_in(src, b"+~0", 0),
                                    parens: vec![],
                                    style: CallStyle::UnOp,
                                    private: false,
                                    optional: false,
                                    receiver: Box::new(
                                        CallExpr {
                                            range: pos_in(src, b"~0", 0),
                                            parens: vec![],
                                            style: CallStyle::UnOp,
                                            private: false,
                                            optional: false,
                                            receiver: Box::new(
                                                IntegerExpr {
                                                    range: pos_in(src, b"0", 0),
                                                    parens: vec![],
                                                    value: 0,
                                                }
                                                .into()
                                            ),
                                            method: symbol("~"),
                                            method_range: pos_in(src, b"~", 0),
                                            args: vec![],
                                        }
                                        .into()
                                    ),
                                    method: symbol("+@"),
                                    method_range: pos_in(src, b"+", 0),
                                    args: vec![],
                                }
                                .into()
                            ),
                            method: symbol("-@"),
                            method_range: pos_in(src, b"-", 0),
                            args: vec![],
                        }
                        .into()
                    ),
                    method: symbol("!"),
                    method_range: pos_in(src, b"!", 0),
                    args: vec![],
                }
                .into(),
                vec![],
            )
        );
    }

    #[test]
    fn test_parse_integer_type() {
        let src = EStrRef::from("Integer");
        assert_eq!(
            p_type(src),
            (
                IntegerType {
                    range: pos_in(src, b"Integer", 0),
                }
                .into(),
                vec![],
            )
        )
    }
}
