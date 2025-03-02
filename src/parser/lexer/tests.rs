use crate::{ast::CodeRange, encoding::EStrRef, Diagnostic};

use super::{BinOpKind, Lexer, LexerState, StringDelimiter, StringState, Token, TokenKind};

fn lex_all_from(input: EStrRef<'_>, state: LexerState) -> Vec<Token> {
    let (tokens, diag) = lex_all_with_diag_from(input, state);
    assert_eq!(diag, Vec::new());
    tokens
}

fn lex_all_with_diag_from(
    input: EStrRef<'_>,
    mut state: LexerState,
) -> (Vec<Token>, Vec<Diagnostic>) {
    let mut diag = Vec::<Diagnostic>::new();
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    let mut string_stack = Vec::<StringState>::new();
    let mut current_string: Option<StringState> = None;
    loop {
        if let Some(current_string_) = &current_string {
            let token = lexer.lex_string_like(&mut diag, current_string_.clone());
            if token.kind == TokenKind::EOF {
                if token.range
                    != (CodeRange {
                        start: input.len(),
                        end: input.len(),
                    })
                {
                    tokens.push(token);
                }
                break;
            } else if matches!(token.kind, TokenKind::StringEnd | TokenKind::StringEndColon) {
                current_string = None;
            } else if matches!(token.kind, TokenKind::StringInterpolationBegin) {
                string_stack.push(current_string.take().unwrap());
            }
            tokens.push(token);
        } else {
            let token = lexer.lex(&mut diag, state);
            if token.kind == TokenKind::EOF {
                if token.range
                    != (CodeRange {
                        start: input.len(),
                        end: input.len(),
                    })
                {
                    tokens.push(token);
                }
                break;
            }
            if matches!(
                token.kind,
                TokenKind::StringBegin | TokenKind::StringBeginLabelable
            ) {
                current_string = Some(next_string_state_for_testing(&token, input.as_bytes()));
            } else if matches!(token.kind, TokenKind::RBrace) && !string_stack.is_empty() {
                current_string = Some(string_stack.pop().unwrap());
            } else {
                state = next_state_for_testing(&token, state);
            }
            tokens.push(token);
        }
    }
    (tokens, diag)
}

fn next_state_for_testing(tok: &Token, prev: LexerState) -> LexerState {
    match tok.kind {
        TokenKind::KeywordCapitalDoubleUnderscoreEncoding => LexerState::End,
        TokenKind::KeywordCapitalDoubleUnderscoreLine => LexerState::End,
        TokenKind::KeywordCapitalDoubleUnderscoreFile => LexerState::End,
        TokenKind::KeywordCapitalBegin => LexerState::End,
        TokenKind::KeywordCapitalEnd => LexerState::End,
        TokenKind::KeywordAlias => LexerState::MethOrSymbolForDef,
        TokenKind::KeywordAnd => LexerState::Begin,
        TokenKind::KeywordBegin => LexerState::Begin,
        TokenKind::KeywordBreak => LexerState::Begin,
        TokenKind::KeywordCase => LexerState::Begin,
        TokenKind::KeywordClass => LexerState::ClassName,
        TokenKind::KeywordDef => LexerState::MethForDef,
        TokenKind::KeywordDefinedQ => LexerState::FirstArgument,
        TokenKind::KeywordDo => LexerState::Begin,
        TokenKind::KeywordElse => LexerState::Begin,
        TokenKind::KeywordElsif => LexerState::Begin,
        TokenKind::KeywordEnd => LexerState::End,
        TokenKind::KeywordEnsure => LexerState::Begin,
        TokenKind::KeywordFalse => LexerState::End,
        TokenKind::KeywordFor => LexerState::Begin,
        TokenKind::KeywordIf => LexerState::Begin,
        TokenKind::KeywordIfInfix => LexerState::BeginLabelable,
        TokenKind::KeywordIn => LexerState::Begin,
        TokenKind::KeywordModule => LexerState::Begin,
        TokenKind::KeywordNext => LexerState::Begin,
        TokenKind::KeywordNil => LexerState::End,
        TokenKind::KeywordNot => LexerState::FirstArgument,
        TokenKind::KeywordOr => LexerState::Begin,
        TokenKind::KeywordRedo => LexerState::End,
        TokenKind::KeywordRescue => LexerState::BeginOpt,
        TokenKind::KeywordRetry => LexerState::End,
        TokenKind::KeywordReturn => LexerState::Begin,
        TokenKind::KeywordSelf => LexerState::End,
        TokenKind::KeywordSuper => LexerState::FirstArgument,
        TokenKind::KeywordThen => LexerState::Begin,
        TokenKind::KeywordTrue => LexerState::End,
        TokenKind::KeywordUndef => LexerState::MethOrSymbolForDef,
        TokenKind::KeywordUnless => LexerState::Begin,
        TokenKind::KeywordUnlessInfix => LexerState::BeginLabelable,
        TokenKind::KeywordUntil => LexerState::Begin,
        TokenKind::KeywordUntilInfix => LexerState::BeginLabelable,
        TokenKind::KeywordWhen => LexerState::Begin,
        TokenKind::KeywordWhile => LexerState::Begin,
        TokenKind::KeywordWhileInfix => LexerState::BeginLabelable,
        TokenKind::KeywordYield => LexerState::BeginOpt,
        TokenKind::Identifier => LexerState::WeakFirstArgument,
        TokenKind::Const => LexerState::End,
        TokenKind::MethodName => LexerState::FirstArgument,
        TokenKind::Label => LexerState::Begin,
        TokenKind::Symbol => LexerState::End,
        TokenKind::NonLocal(_) => LexerState::End,
        TokenKind::Numeric(_) => LexerState::End,
        TokenKind::CharLiteral => LexerState::End,
        TokenKind::StringBegin | TokenKind::StringBeginLabelable => {
            unreachable!()
        }
        TokenKind::StringEnd => LexerState::End,
        TokenKind::StringEndColon => LexerState::Begin,
        TokenKind::StringContent => prev,
        TokenKind::StringInterpolationBegin => LexerState::Begin,
        TokenKind::StringVarInterpolation => LexerState::End,
        TokenKind::BinOp(BinOpKind::BitwiseOr) => LexerState::BeginLabelable,
        TokenKind::BinOp(_) => LexerState::Begin,
        TokenKind::UnOp(_) => LexerState::Begin,
        TokenKind::OpAssign(_) => LexerState::Begin,
        TokenKind::AmpPrefix => LexerState::Begin,
        TokenKind::AmpDot => LexerState::MethForCall,
        TokenKind::LParen => LexerState::BeginLabelable,
        TokenKind::LParenRestricted => LexerState::BeginLabelable,
        TokenKind::RParen => LexerState::End,
        TokenKind::StarPrefix => LexerState::Begin,
        TokenKind::StarStarPrefix => LexerState::Begin,
        TokenKind::Comma => LexerState::BeginLabelable,
        TokenKind::Arrow => LexerState::End,
        TokenKind::Dot => LexerState::MethForCall,
        TokenKind::DotDot => LexerState::Begin,
        TokenKind::DotDotDot => LexerState::Begin,
        TokenKind::Colon => LexerState::Begin,
        TokenKind::ColonColon => LexerState::MethForCall,
        TokenKind::ColonColonPrefix => LexerState::Begin,
        TokenKind::Semicolon => LexerState::Begin,
        TokenKind::Newline => LexerState::Begin,
        TokenKind::Eq => LexerState::Begin,
        TokenKind::FatArrow => LexerState::Begin,
        TokenKind::Question => LexerState::Begin,
        TokenKind::At => LexerState::Begin,
        TokenKind::LBracket => LexerState::BeginLabelable,
        TokenKind::LBracketPrefix => LexerState::BeginLabelable,
        TokenKind::RBracket => LexerState::End,
        TokenKind::LBrace => LexerState::Begin,
        TokenKind::RBrace => LexerState::End,
        TokenKind::EOF => LexerState::Begin,
        TokenKind::Unknown => LexerState::Begin,
    }
}

fn next_string_state_for_testing(tok: &Token, bytes: &[u8]) -> StringState {
    StringState {
        delim: match bytes[tok.range.start] {
            b'\'' => StringDelimiter::Quote,
            b'"' => StringDelimiter::DoubleQuote,
            b'`' => StringDelimiter::Backtick,
            b'/' => StringDelimiter::Slash,
            _ => unreachable!(),
        },
        allow_label: tok.kind == TokenKind::StringBeginLabelable,
    }
}

fn token(kind: TokenKind, range: CodeRange, indent: usize) -> Token {
    Token {
        kind,
        range,
        indent,
    }
}

const ALL_STATES: [LexerState; 10] = [
    LexerState::Begin,
    LexerState::ClassName,
    LexerState::BeginOpt,
    LexerState::BeginLabelable,
    LexerState::FirstArgument,
    LexerState::WeakFirstArgument,
    LexerState::End,
    LexerState::MethForDef,
    LexerState::MethOrSymbolForDef,
    LexerState::MethForCall,
];

#[track_caller]
fn assert_lex<'a, S, F>(src: S, expected: F)
where
    S: Into<EStrRef<'a>>,
    F: FnOnce(EStrRef<'_>) -> Vec<Token>,
{
    assert_lex_impl(src, expected, |_| true);
}

#[track_caller]
fn assert_lex_for<'a, S, F>(src: S, states: &[LexerState], expected: F)
where
    S: Into<EStrRef<'a>>,
    F: FnOnce(EStrRef<'_>) -> Vec<Token>,
{
    assert_lex_impl(src, expected, |state| states.contains(&state));
}

#[track_caller]
fn assert_lex_except<'a, S, F>(src: S, states: &[LexerState], expected: F)
where
    S: Into<EStrRef<'a>>,
    F: FnOnce(EStrRef<'_>) -> Vec<Token>,
{
    assert_lex_impl(src, expected, |state| !states.contains(&state));
}

#[track_caller]
fn assert_lex_impl<'a, S, F, FS>(src: S, expected: F, mut filter_state: FS)
where
    S: Into<EStrRef<'a>>,
    F: FnOnce(EStrRef<'_>) -> Vec<Token>,
    FS: FnMut(LexerState) -> bool,
{
    let src = <S as Into<EStrRef<'a>>>::into(src);
    let expected = expected(src);
    let mut difflist1 = Vec::<(LexerState, Vec<Token>)>::new();
    let mut difflist2 = Vec::<(LexerState, Vec<Token>)>::new();
    for &state in &ALL_STATES {
        if !filter_state(state) {
            continue;
        }
        let actual = lex_all_from(src, state);
        if actual != expected {
            difflist1.push((state, actual));
            difflist2.push((state, expected.clone()));
        }
    }
    assert_eq!(difflist1, difflist2);
}

mod ambig1_tests;
mod ambig2_tests;
mod ambig3_tests;
mod identifier_tests;
mod keywords_tests;
mod nonlocals_tests;
mod numeric_tests;
mod op_tests;
mod parens_tests;
mod spacing_tests;
mod string_tests;
mod symbol_tests;
