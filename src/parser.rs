mod lexer;

use crate::{
    ast::{
        CallExpr, CallStyle, CodeRange, ErrorExpr, ErrorType, ErrorWriteTarget, Expr, FalseExpr,
        FalseType, IntegerExpr, IntegerType, InterpolationContent, LocalVariableExpr,
        LocalVariableWriteTarget, NilExpr, NilType, Paren, Program, RegexpExpr, RegexpType,
        SelfExpr, Semicolon, SemicolonKind, SeqExpr, SeqParen, SeqParenKind, Stmt, StmtList,
        StringContent, StringExpr, StringType, TextContent, TrueExpr, TrueType, Type,
        TypeAnnotation, WriteExpr, WriteTarget, XStringExpr, DUMMY_RANGE,
    },
    encoding::EStrRef,
    Diagnostic, EString,
};
use lexer::{Lexer, LexerState, StringDelimiter, Token, TokenKind};

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

    fn input(&self) -> EStrRef<'a> {
        self.lexer.input()
    }

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
                        last_stmt.range = last_stmt.range | token.range;
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
                    let expr = self.parse_expr_lv_spelled_not(diag);
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
        let expr = self.parse_expr_lv_spelled_not(diag);
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

    fn parse_expr_lv_spelled_not(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        // TODO: expressions like `false | not(true)` should also be handled
        let token = self.fill_token(diag, LexerState::Begin);
        match token.kind {
            TokenKind::KeywordNot => {
                let meth = match token.kind {
                    TokenKind::KeywordNot => "!",
                    _ => unreachable!(),
                };
                self.bump();
                let expr = self.parse_expr_lv_assignment(diag);
                CallExpr {
                    range: token.range | *expr.outer_range(),
                    parens: Vec::new(),

                    style: CallStyle::SpelloutUnOp,
                    private: false,
                    receiver: Box::new(expr),
                    method: symbol(meth),
                    method_range: token.range,
                    args: vec![],
                }
                .into()
            }
            _ => self.parse_expr_lv_assignment(diag),
        }
    }

    fn parse_expr_lv_assignment(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let expr = self.parse_expr_lv_eq(diag);
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
                    range: *lhs.range() | *rhs.outer_range(),
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

    fn parse_expr_lv_eq(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_ineq(diag);
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
                    let rhs = self.parse_expr_lv_ineq(diag);
                    expr = CallExpr {
                        range: *expr.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::BinOp,
                        private: false,
                        receiver: Box::new(expr),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![rhs],
                    }
                    .into();
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_expr_lv_ineq(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_bitwise_or(diag);
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
                    let rhs = self.parse_expr_lv_bitwise_or(diag);
                    expr = CallExpr {
                        range: *expr.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::BinOp,
                        private: false,
                        receiver: Box::new(expr),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![rhs],
                    }
                    .into();
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_expr_lv_bitwise_or(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_bitwise_and(diag);
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
                    let rhs = self.parse_expr_lv_bitwise_and(diag);
                    expr = CallExpr {
                        range: *expr.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::BinOp,
                        private: false,
                        receiver: Box::new(expr),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![rhs],
                    }
                    .into();
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_expr_lv_bitwise_and(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_shift(diag);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::Amp => {
                    let meth = match token.kind {
                        TokenKind::Amp => "&",
                        _ => unimplemented!(),
                    };
                    self.bump();
                    let rhs = self.parse_expr_lv_shift(diag);
                    expr = CallExpr {
                        range: *expr.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::BinOp,
                        private: false,
                        receiver: Box::new(expr),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![rhs],
                    }
                    .into();
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_expr_lv_shift(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_additive(diag);
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
                    let rhs = self.parse_expr_lv_additive(diag);
                    expr = CallExpr {
                        range: *expr.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::BinOp,
                        private: false,
                        receiver: Box::new(expr),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![rhs],
                    }
                    .into();
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_expr_lv_additive(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_multiplicative(diag);
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
                    let rhs = self.parse_expr_lv_multiplicative(diag);
                    expr = CallExpr {
                        range: *expr.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::BinOp,
                        private: false,
                        receiver: Box::new(expr),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![rhs],
                    }
                    .into();
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_expr_lv_multiplicative(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_exponential(diag);
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
                    let rhs = self.parse_expr_lv_exponential(diag);
                    expr = CallExpr {
                        range: *expr.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::BinOp,
                        private: false,
                        receiver: Box::new(expr),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![rhs],
                    }
                    .into();
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_expr_lv_exponential(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let expr = self.parse_expr_lv_unary(diag);
        let token = self.fill_token(diag, LexerState::End);
        match token.kind {
            TokenKind::StarStar => {
                let meth = match token.kind {
                    TokenKind::StarStar => "**",
                    _ => unimplemented!(),
                };
                self.bump();
                let rhs = self.parse_expr_lv_exponential(diag);
                match self.reparse_minus(expr) {
                    Ok((minus_range, lhs)) => CallExpr {
                        range: minus_range | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::UnOp,
                        private: false,
                        receiver: Box::new(
                            CallExpr {
                                range: *lhs.outer_range() | *rhs.outer_range(),
                                parens: Vec::new(),

                                style: CallStyle::BinOp,
                                private: false,
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
                    Err(lhs) => CallExpr {
                        range: *lhs.outer_range() | *rhs.outer_range(),
                        parens: Vec::new(),

                        style: CallStyle::BinOp,
                        private: false,
                        receiver: Box::new(lhs),
                        method: symbol(meth),
                        method_range: token.range,
                        args: vec![rhs],
                    }
                    .into(),
                }
            }
            _ => expr,
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

    fn parse_expr_lv_unary(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
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
                let expr = self.parse_expr_lv_unary(diag);
                CallExpr {
                    range: token.range | *expr.outer_range(),
                    parens: Vec::new(),

                    style: CallStyle::UnOp,
                    private: false,
                    receiver: Box::new(expr),
                    method: symbol(meth),
                    method_range: token.range,
                    args: vec![],
                }
                .into()
            }
            _ => self.parse_expr_lv_call_like(diag),
        }
    }

    fn parse_expr_lv_call_like(&mut self, diag: &mut Vec<Diagnostic>) -> Expr {
        let mut expr = self.parse_expr_lv_primary(diag);
        loop {
            let token = self.fill_token(diag, LexerState::End);
            match token.kind {
                TokenKind::LParen => {
                    let (args, args_range) = self.parse_paren_args(diag);
                    match expr {
                        Expr::LocalVariable(callee) if callee.parens.is_empty() => {
                            expr = CallExpr {
                                range: callee.range | args_range,
                                parens: Vec::new(),

                                style: CallStyle::ImplicitSelf,
                                private: true,
                                receiver: Box::new(
                                    SelfExpr {
                                        range: DUMMY_RANGE,
                                        parens: Vec::new(),
                                    }
                                    .into(),
                                ),
                                method: callee.name,
                                method_range: callee.range,
                                args,
                            }
                            .into();
                        }
                        Expr::Call(callee)
                            if callee.parens.is_empty()
                                && matches!(
                                    callee.style,
                                    CallStyle::ImplicitSelf | CallStyle::Dot
                                )
                                && callee.args.is_empty() =>
                        {
                            expr = CallExpr {
                                range: callee.range | args_range,
                                parens: Vec::new(),

                                style: callee.style,
                                private: callee.private,
                                receiver: callee.receiver,
                                method: callee.method,
                                method_range: callee.method_range,
                                args,
                            }
                            .into();
                        }
                        _ => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("Need a dot to call an expression"),
                            });
                            expr = CallExpr {
                                range: *expr.outer_range() | args_range,
                                parens: Vec::new(),

                                style: CallStyle::CallOp,
                                private: false,
                                receiver: Box::new(expr),
                                method: symbol("call"),
                                method_range: token.range,
                                args,
                            }
                            .into();
                        }
                    }
                }
                TokenKind::Dot | TokenKind::AmpDot | TokenKind::ColonColon => {
                    self.bump();
                    let token = self.fill_token(diag, LexerState::MethForCall);
                    match token.kind {
                        TokenKind::LParen => {
                            // expr.(args)
                            let (args, args_range) = self.parse_paren_args(diag);
                            expr = CallExpr {
                                range: *expr.outer_range() | args_range,
                                parens: Vec::new(),

                                style: CallStyle::CallOp,
                                private: false,
                                receiver: Box::new(expr),
                                method: symbol("call"),
                                method_range: token.range,
                                args,
                            }
                            .into();
                        }
                        TokenKind::Identifier | TokenKind::Const | TokenKind::MethodName => {
                            self.bump();
                            // expr.meth
                            let s = self.select(token.range);
                            let private =
                                matches!(expr, Expr::Self_(_)) && expr.parens().is_empty();
                            expr = CallExpr {
                                range: *expr.outer_range() | token.range,
                                parens: Vec::new(),

                                style: CallStyle::Dot,
                                private,
                                receiver: Box::new(expr),
                                method: s.to_estring().asciified(),
                                method_range: token.range,
                                args: vec![],
                            }
                            .into();
                        }
                        _ => {
                            diag.push(Diagnostic {
                                range: token.range,
                                message: format!("unexpected token"),
                            });
                            expr = CallExpr {
                                range: *expr.outer_range() | token.range,
                                parens: Vec::new(),

                                style: CallStyle::Dot,
                                private: false,
                                receiver: Box::new(expr),
                                method: symbol(""),
                                method_range: token.range,
                                args: vec![],
                            }
                            .into();
                        }
                    }
                }
                TokenKind::At => {
                    self.bump();
                    let ty = self.parse_type(diag);
                    expr = match expr {
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
                    name: s.to_estring().asciified(),
                    type_annotation: None,
                }
                .into()
            }
            TokenKind::MethodName => {
                self.bump();
                let s = self.select(token.range);
                CallExpr {
                    range: token.range,
                    parens: Vec::new(),

                    style: CallStyle::ImplicitSelf,
                    private: true,
                    receiver: Box::new(
                        SelfExpr {
                            range: DUMMY_RANGE,
                            parens: Vec::new(),
                        }
                        .into(),
                    ),
                    method: s.to_estring().asciified(),
                    method_range: token.range,
                    args: vec![],
                }
                .into()
            }
            TokenKind::Integer => {
                self.bump();
                IntegerExpr {
                    range: token.range,
                    parens: Vec::new(),
                    value: String::from_utf8_lossy(self.select(token.range).as_bytes())
                        .parse()
                        .unwrap(),
                }
                .into()
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
                            let stmt_list = self.parse_stmt_list(diag);
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
                                stmt_list,
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
                match delim {
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
                }
            }
            TokenKind::LParen => {
                let open_range = token.range;
                self.bump();
                self.fill_token(diag, LexerState::BeginLabelable);
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
                        range: open_range | close_range,
                        open_range,
                        close_range,
                    });
                    expr
                } else {
                    Expr::Seq(SeqExpr {
                        range: open_range | close_range,
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

    fn parse_paren_args(&mut self, diag: &mut Vec<Diagnostic>) -> (Vec<Expr>, CodeRange) {
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
                    let arg = self.parse_expr_lv_assignment(diag);
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
                    range: pos_in(src, b"x; y\nz", 0),
                    stmt_list: StmtList {
                        range: pos_in(src, b"x; y\nz", 0),
                        semi_prefix: vec![],
                        stmts: vec![
                            Stmt {
                                range: pos_in(src, b"x;", 0),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"x", 0),
                                    parens: vec![],
                                    name: symbol("x"),
                                    type_annotation: None,
                                }
                                .into(),
                                semi: vec![Semicolon {
                                    range: pos_in(src, b";", 0),
                                    kind: SemicolonKind::Semicolon
                                }],
                            },
                            Stmt {
                                range: pos_in(src, b"y\n", 0),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"y", 0),
                                    parens: vec![],
                                    name: symbol("y"),
                                    type_annotation: None,
                                }
                                .into(),
                                semi: vec![Semicolon {
                                    range: pos_in(src, b"\n", 0),
                                    kind: SemicolonKind::Newline
                                }],
                            },
                            Stmt {
                                range: pos_in(src, b"z", 0),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"z", 0),
                                    parens: vec![],
                                    name: symbol("z"),
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
                    range: pos_in(src, b"x", 0),
                    parens: vec![Paren {
                        range: pos_in(src, b"(x)", 0),
                        open_range: pos_in(src, b"(", 0),
                        close_range: pos_in(src, b")", 0),
                    }],
                    name: symbol("x"),
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
                    range: pos_in(src, b"x", 0),
                    parens: vec![Paren {
                        range: pos_in(src, b"(\nx\n)", 0),
                        open_range: pos_in(src, b"(", 0),
                        close_range: pos_in(src, b")", 0),
                    }],
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
        let src = EStrRef::from("(x;)");
        assert_eq!(
            p_expr(src),
            (
                SeqExpr {
                    range: pos_in(src, b"(x;)", 0),
                    parens: vec![],
                    paren: SeqParen {
                        kind: SeqParenKind::Paren,
                        open_range: pos_in(src, b"(", 0),
                        close_range: pos_in(src, b")", 0),
                    },
                    stmt_list: StmtList {
                        range: pos_in(src, b"x;", 0),
                        semi_prefix: vec![],
                        stmts: vec![Stmt {
                            range: pos_in(src, b"x;", 0),
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"x", 0),
                                parens: vec![],
                                name: symbol("x"),
                                type_annotation: None,
                            }
                            .into(),
                            semi: vec![Semicolon {
                                range: pos_in(src, b";", 0),
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
                    range: pos_in(src, b"(;x)", 0),
                    parens: vec![],
                    paren: SeqParen {
                        kind: SeqParenKind::Paren,
                        open_range: pos_in(src, b"(", 0),
                        close_range: pos_in(src, b")", 0),
                    },
                    stmt_list: StmtList {
                        range: pos_in(src, b";x", 0),
                        semi_prefix: vec![Semicolon {
                            range: pos_in(src, b";", 0),
                            kind: SemicolonKind::Semicolon
                        }],
                        stmts: vec![Stmt {
                            range: pos_in(src, b"x", 0),
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"x", 0),
                                parens: vec![],
                                name: symbol("x"),
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
                    range: pos_in(src, b"(x\ny)", 0),
                    parens: vec![],
                    paren: SeqParen {
                        kind: SeqParenKind::Paren,
                        open_range: pos_in(src, b"(", 0),
                        close_range: pos_in(src, b")", 0),
                    },
                    stmt_list: StmtList {
                        range: pos_in(src, b"x\ny", 0),
                        semi_prefix: vec![],
                        stmts: vec![
                            Stmt {
                                range: pos_in(src, b"x\n", 0),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"x", 0),
                                    parens: vec![],
                                    name: symbol("x"),
                                    type_annotation: None,
                                }
                                .into(),
                                semi: vec![Semicolon {
                                    range: pos_in(src, b"\n", 0),
                                    kind: SemicolonKind::Newline
                                }],
                            },
                            Stmt {
                                range: pos_in(src, b"y", 0),
                                expr: LocalVariableExpr {
                                    range: pos_in(src, b"y", 0),
                                    parens: vec![],
                                    name: symbol("y"),
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
                    range: pos_in(src, b"nil", 0),
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
                    range: pos_in(src, b"false", 0),
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
            p_expr(src),
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
            p_expr(src),
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
            p_expr(src),
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
            p_expr(src),
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
            p_expr(src),
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
            p_expr(src),
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
                            stmt_list: StmtList {
                                range: pos_in(src, b"bar", 0),
                                semi_prefix: vec![],
                                stmts: vec![Stmt {
                                    range: pos_in(src, b"bar", 0),
                                    expr: LocalVariableExpr {
                                        range: pos_in(src, b"bar", 0),
                                        parens: vec![],
                                        name: symbol("bar"),
                                        type_annotation: None,
                                    }
                                    .into(),
                                    semi: vec![],
                                }],
                            },
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
            p_expr(src),
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
            p_expr(src),
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"foo()", 0),
                    parens: vec![],
                    style: CallStyle::ImplicitSelf,
                    private: true,
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
    fn test_method_call() {
        let src = EStrRef::from("x.foo()");
        assert_eq!(
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"x.foo()", 0),
                    parens: vec![],
                    style: CallStyle::Dot,
                    private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"x.foo", 0),
                    parens: vec![],
                    style: CallStyle::Dot,
                    private: false,
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
            p_expr(src),
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
            p_expr(src),
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"x * y / z % w", 0),
                    parens: vec![],
                    style: CallStyle::BinOp,
                    private: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"x * y / z", 0),
                            parens: vec![],
                            style: CallStyle::BinOp,
                            private: false,
                            receiver: Box::new(
                                CallExpr {
                                    range: pos_in(src, b"x * y", 0),
                                    parens: vec![],
                                    style: CallStyle::BinOp,
                                    private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"x ** y * z ** w", 0),
                    parens: vec![],
                    style: CallStyle::BinOp,
                    private: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"x ** y", 0),
                            parens: vec![],
                            style: CallStyle::BinOp,
                            private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"x ** y ** z", 0),
                    parens: vec![],
                    style: CallStyle::BinOp,
                    private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"+x ** +y", 0),
                    parens: vec![],
                    style: CallStyle::BinOp,
                    private: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"+x", 0),
                            parens: vec![],
                            style: CallStyle::UnOp,
                            private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"-x ** -y ** z", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"x ** -y ** z", 0),
                            parens: vec![],
                            style: CallStyle::BinOp,
                            private: false,
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
                                receiver: Box::new(
                                    CallExpr {
                                        range: pos_in(src, b"y ** z", 0),
                                        parens: vec![],
                                        style: CallStyle::BinOp,
                                        private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"+x", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"-x", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"!x", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"~x", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
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
            p_expr(src),
            (
                CallExpr {
                    range: pos_in(src, b"!-+~0", 0),
                    parens: vec![],
                    style: CallStyle::UnOp,
                    private: false,
                    receiver: Box::new(
                        CallExpr {
                            range: pos_in(src, b"-+~0", 0),
                            parens: vec![],
                            style: CallStyle::UnOp,
                            private: false,
                            receiver: Box::new(
                                CallExpr {
                                    range: pos_in(src, b"+~0", 0),
                                    parens: vec![],
                                    style: CallStyle::UnOp,
                                    private: false,
                                    receiver: Box::new(
                                        CallExpr {
                                            range: pos_in(src, b"~0", 0),
                                            parens: vec![],
                                            style: CallStyle::UnOp,
                                            private: false,
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
