use crate::{
    ast::{pos_in, CodeRange},
    encoding::EStrRef,
    Diagnostic, Encoding,
};

use super::{
    BinOpKind, Lexer, LexerState, StringDelimiter, StringState, Token, TokenKind, UnOpKind,
};

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
        TokenKind::IvarName => LexerState::End,
        TokenKind::CvarName => LexerState::End,
        TokenKind::GvarName => LexerState::End,
        TokenKind::Numeric => LexerState::End,
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

#[test]
fn test_lex_eof_nul() {
    assert_lex("\0", |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"\0", 0), 0)]
    });
}

#[test]
fn test_lex_eof_eot() {
    assert_lex("\x04", |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"\x04", 0), 0)]
    });
}

#[test]
fn test_lex_eof_sub() {
    assert_lex("\x1A", |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"\x1A", 0), 0)]
    });
}

#[test]
fn test_lex_eof_end_token_first_line() {
    assert_lex("__END__", |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_eof_end_token_second_line() {
    assert_lex("foo\n__END__", |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
            token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
            token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0),
        ]
    });
}

#[test]
fn test_lex_eof_end_token_eof() {
    assert_lex("__END__", |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_eof_end_token_lf() {
    assert_lex("__END__\n", |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_eof_end_token_crlf() {
    assert_lex("__END__\r\n", |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_non_eof_space_after_end_token() {
    assert_lex("__END__ \n", |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 0),
            token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
        ]
    });
}

#[test]
fn test_lex_non_eof_space_before_end_token() {
    assert_lex(" __END__\n", |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 1),
            token(TokenKind::Newline, pos_in(src, b"\n", 0), 1),
        ]
    });
}

#[test]
fn test_lex_non_eof_cr_after_end_token() {
    assert_lex("__END__\r", |src| {
        vec![token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_spaces_space() {
    assert_lex(" 1", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 1)]
    });
}

#[test]
fn test_lex_spaces_tab() {
    assert_lex("\t1", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 8)]
    });
}

#[test]
fn test_lex_spaces_tab_and_space() {
    assert_lex("\t 1", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 9)]
    });
}

#[test]
fn test_lex_spaces_space_and_tab() {
    assert_lex(" \t1", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 8)]
    });
}

#[test]
fn test_lex_spaces_vtab() {
    assert_lex("\x0B1", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 0)]
    });
}

#[test]
fn test_lex_spaces_ff() {
    assert_lex("\x0C1", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 0)]
    });
}

#[test]
fn test_lex_spaces_cr() {
    assert_lex("\r1", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 0)]
    });
}

#[test]
fn test_lex_spaces_multiple() {
    assert_lex("  1", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 2)]
    });
}

#[test]
fn test_lex_spaces_lf() {
    assert_lex_except(
        "\n1",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| vec![token(TokenKind::Numeric, pos_in(src, b"1", 0), 0)],
    );
}

#[test]
fn test_lex_semicolon_lf() {
    assert_lex_for(
        "\n",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| vec![token(TokenKind::Newline, pos_in(src, b"\n", 0), 0)],
    );
}

#[test]
fn test_lex_semicolon_crlf() {
    assert_lex_for(
        "\r\n",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| vec![token(TokenKind::Newline, pos_in(src, b"\r\n", 0), 0)],
    );
}

#[test]
fn test_lex_skip_lf_before_dot() {
    assert_lex_for(
        "\n  .bar",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::Dot, pos_in(src, b".", 0), 2),
                token(TokenKind::Identifier, pos_in(src, b"bar", 0), 2),
            ]
        },
    );
}

#[test]
fn test_lex_lf_before_dot_dot() {
    assert_lex_for(
        "\n  ..bar",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![
                token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
                token(TokenKind::DotDot, pos_in(src, b"..", 0), 2),
                token(TokenKind::Identifier, pos_in(src, b"bar", 0), 2),
            ]
        },
    );
}

#[test]
fn test_lex_comments() {
    assert_lex_except(
        "# comment2\nfoo bar # comment1\n# comment3\nbaz\n",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"bar", 0), 0),
                token(TokenKind::Newline, pos_in(src, b"\n", 1), 0),
                token(TokenKind::Identifier, pos_in(src, b"baz", 0), 0),
                token(TokenKind::Newline, pos_in(src, b"\n", 3), 0),
            ]
        },
    );
}

#[test]
fn test_lex_keyword_encoding() {
    assert_lex("__ENCODING__", |src| {
        vec![token(
            TokenKind::KeywordCapitalDoubleUnderscoreEncoding,
            pos_in(src, b"__ENCODING__", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_line() {
    assert_lex("__LINE__", |src| {
        vec![token(
            TokenKind::KeywordCapitalDoubleUnderscoreLine,
            pos_in(src, b"__LINE__", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_file() {
    assert_lex("__FILE__", |src| {
        vec![token(
            TokenKind::KeywordCapitalDoubleUnderscoreFile,
            pos_in(src, b"__FILE__", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_cap_begin() {
    assert_lex("BEGIN", |src| {
        vec![token(
            TokenKind::KeywordCapitalBegin,
            pos_in(src, b"BEGIN", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_cap_end() {
    assert_lex("END", |src| {
        vec![token(
            TokenKind::KeywordCapitalEnd,
            pos_in(src, b"END", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_alias() {
    assert_lex("alias", |src| {
        vec![token(TokenKind::KeywordAlias, pos_in(src, b"alias", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_and() {
    assert_lex("and", |src| {
        vec![token(TokenKind::KeywordAnd, pos_in(src, b"and", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_begin() {
    assert_lex("begin", |src| {
        vec![token(TokenKind::KeywordBegin, pos_in(src, b"begin", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_break() {
    assert_lex("break", |src| {
        vec![token(TokenKind::KeywordBreak, pos_in(src, b"break", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_case() {
    assert_lex("case", |src| {
        vec![token(TokenKind::KeywordCase, pos_in(src, b"case", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_class() {
    assert_lex("class", |src| {
        vec![token(TokenKind::KeywordClass, pos_in(src, b"class", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_def() {
    assert_lex("def", |src| {
        vec![token(TokenKind::KeywordDef, pos_in(src, b"def", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_defined_q() {
    assert_lex("defined?", |src| {
        vec![token(
            TokenKind::KeywordDefinedQ,
            pos_in(src, b"defined?", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_do() {
    assert_lex("do", |src| {
        vec![token(TokenKind::KeywordDo, pos_in(src, b"do", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_else() {
    assert_lex("else", |src| {
        vec![token(TokenKind::KeywordElse, pos_in(src, b"else", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_elsif() {
    assert_lex("elsif", |src| {
        vec![token(TokenKind::KeywordElsif, pos_in(src, b"elsif", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_end() {
    assert_lex("end", |src| {
        vec![token(TokenKind::KeywordEnd, pos_in(src, b"end", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_ensure() {
    assert_lex("ensure", |src| {
        vec![token(
            TokenKind::KeywordEnsure,
            pos_in(src, b"ensure", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_false() {
    assert_lex("false", |src| {
        vec![token(TokenKind::KeywordFalse, pos_in(src, b"false", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_for() {
    assert_lex("for", |src| {
        vec![token(TokenKind::KeywordFor, pos_in(src, b"for", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_if_prefix() {
    assert_lex_except(
        "if",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| vec![token(TokenKind::KeywordIf, pos_in(src, b"if", 0), 0)],
    );
}

#[test]
fn test_lex_keyword_if_infix() {
    assert_lex_for(
        "if",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| vec![token(TokenKind::KeywordIfInfix, pos_in(src, b"if", 0), 0)],
    );
}

#[test]
fn test_lex_keyword_in() {
    assert_lex("in", |src| {
        vec![token(TokenKind::KeywordIn, pos_in(src, b"in", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_module() {
    assert_lex("module", |src| {
        vec![token(
            TokenKind::KeywordModule,
            pos_in(src, b"module", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_next() {
    assert_lex("next", |src| {
        vec![token(TokenKind::KeywordNext, pos_in(src, b"next", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_nil() {
    assert_lex("nil", |src| {
        vec![token(TokenKind::KeywordNil, pos_in(src, b"nil", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_not() {
    assert_lex("not", |src| {
        vec![token(TokenKind::KeywordNot, pos_in(src, b"not", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_or() {
    assert_lex("or", |src| {
        vec![token(TokenKind::KeywordOr, pos_in(src, b"or", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_redo() {
    assert_lex("redo", |src| {
        vec![token(TokenKind::KeywordRedo, pos_in(src, b"redo", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_rescue() {
    assert_lex("rescue", |src| {
        vec![token(
            TokenKind::KeywordRescue,
            pos_in(src, b"rescue", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_retry() {
    assert_lex("retry", |src| {
        vec![token(TokenKind::KeywordRetry, pos_in(src, b"retry", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_return() {
    assert_lex("return", |src| {
        vec![token(
            TokenKind::KeywordReturn,
            pos_in(src, b"return", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_self() {
    assert_lex("self", |src| {
        vec![token(TokenKind::KeywordSelf, pos_in(src, b"self", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_super() {
    assert_lex("super", |src| {
        vec![token(TokenKind::KeywordSuper, pos_in(src, b"super", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_then() {
    assert_lex("then", |src| {
        vec![token(TokenKind::KeywordThen, pos_in(src, b"then", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_true() {
    assert_lex("true", |src| {
        vec![token(TokenKind::KeywordTrue, pos_in(src, b"true", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_undef() {
    assert_lex("undef", |src| {
        vec![token(TokenKind::KeywordUndef, pos_in(src, b"undef", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_unless_prefix() {
    assert_lex_except(
        "unless",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![token(
                TokenKind::KeywordUnless,
                pos_in(src, b"unless", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_keyword_unless_infix() {
    assert_lex_for(
        "unless",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![token(
                TokenKind::KeywordUnlessInfix,
                pos_in(src, b"unless", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_keyword_until_prefix() {
    assert_lex_except(
        "until",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| vec![token(TokenKind::KeywordUntil, pos_in(src, b"until", 0), 0)],
    );
}

#[test]
fn test_lex_keyword_until_infix() {
    assert_lex_for(
        "until",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![token(
                TokenKind::KeywordUntilInfix,
                pos_in(src, b"until", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_keyword_when() {
    assert_lex("when", |src| {
        vec![token(TokenKind::KeywordWhen, pos_in(src, b"when", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_while_prefix() {
    assert_lex_except(
        "while",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| vec![token(TokenKind::KeywordWhile, pos_in(src, b"while", 0), 0)],
    );
}

#[test]
fn test_lex_keyword_while_infix() {
    assert_lex_for(
        "while",
        &[
            LexerState::BeginOpt,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![token(
                TokenKind::KeywordWhileInfix,
                pos_in(src, b"while", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_keyword_yield() {
    assert_lex("yield", |src| {
        vec![token(TokenKind::KeywordYield, pos_in(src, b"yield", 0), 0)]
    });
}

#[test]
fn test_lex_ident_simple() {
    assert_lex("foo123", |src| {
        vec![token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0)]
    });
}

#[test]
fn test_lex_ident_non_ascii() {
    assert_lex("あ", |src| {
        vec![token(
            TokenKind::Identifier,
            pos_in(src, b"\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ident_invalid_non_ascii() {
    let src = EStrRef::from_bytes(b"\xE3\x81", Encoding::UTF_8);
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"\xE3\x81", 0),
            message: "The identifier contains invalid characters".to_owned(),
        }]
    );
}

#[test]
fn test_lex_const_simple() {
    assert_lex("Foo123", |src| {
        vec![token(TokenKind::Const, pos_in(src, b"Foo123", 0), 0)]
    });
}

#[test]
fn test_lex_const_non_ascii() {
    assert_lex("Ω", |src| {
        vec![token(TokenKind::Const, pos_in(src, b"\xCE\xA9", 0), 0)]
    });
}

#[test]
fn test_lex_const_invalid_non_ascii() {
    let src = EStrRef::from_bytes(b"\xCE\xA9\xE3\x81", Encoding::UTF_8);
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"\xCE\xA9\xE3\x81", 0),
            message: "The identifier contains invalid characters".to_owned(),
        }]
    );
}

#[test]
fn test_lex_ident_bang_simple() {
    assert_lex("foo123!", |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"foo123!", 0), 0)]
    });
}

#[test]
fn test_lex_ident_bang_capital() {
    assert_lex("Foo123!", |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"Foo123!", 0), 0)]
    });
}

#[test]
fn test_lex_ident_bang_eq() {
    assert_lex("foo123!=", |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::NotEq), pos_in(src, b"!=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_q_simple() {
    assert_lex("foo123?", |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"foo123?", 0), 0)]
    });
}

#[test]
fn test_lex_ident_q_capital() {
    assert_lex("Foo123?", |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"Foo123?", 0), 0)]
    });
}

#[test]
fn test_lex_ident_q_eq() {
    assert_lex("foo123?=", |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::Question, pos_in(src, b"?", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_join() {
    assert_lex_for(
        "foo123=",
        &[LexerState::MethForDef, LexerState::MethOrSymbolForDef],
        |src| vec![token(TokenKind::MethodName, pos_in(src, b"foo123=", 0), 0)],
    );
}

#[test]
fn test_lex_ident_eq_separate() {
    assert_lex_except(
        "foo123=",
        &[LexerState::MethForDef, LexerState::MethOrSymbolForDef],
        |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
            ]
        },
    );
}

#[test]
fn test_lex_ident_eq_tilde() {
    assert_lex("foo123=~", |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Match), pos_in(src, b"=~", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_gt() {
    assert_lex("foo123=>", |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_eq() {
    assert_lex("foo123==", |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Eq), pos_in(src, b"==", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_eq_gt_join() {
    assert_lex_for(
        "foo123==>",
        &[LexerState::MethForDef, LexerState::MethOrSymbolForDef],
        |src| {
            vec![
                token(TokenKind::MethodName, pos_in(src, b"foo123=", 0), 0),
                token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
            ]
        },
    );
}

#[test]
fn test_lex_ident_eq_eq_gt_separate() {
    assert_lex_except(
        "foo123==>",
        &[LexerState::MethForDef, LexerState::MethOrSymbolForDef],
        |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
                token(TokenKind::BinOp(BinOpKind::Eq), pos_in(src, b"==", 0), 0),
                token(TokenKind::BinOp(BinOpKind::Gt), pos_in(src, b">", 0), 0),
            ]
        },
    );
}

#[test]
fn test_lex_label_simple() {
    assert_lex_for(
        "foo123:",
        &[
            LexerState::BeginLabelable,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
        ],
        |src| vec![token(TokenKind::Label, pos_in(src, b"foo123:", 0), 0)],
    );
}

#[test]
fn test_lex_label_cap() {
    assert_lex_for(
        "Bar:",
        &[
            LexerState::BeginLabelable,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
        ],
        |src| vec![token(TokenKind::Label, pos_in(src, b"Bar:", 0), 0)],
    );
}

#[test]
fn test_lex_label_keyword_like() {
    assert_lex_for(
        "case:",
        &[
            LexerState::BeginLabelable,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
        ],
        |src| vec![token(TokenKind::Label, pos_in(src, b"case:", 0), 0)],
    );
}

#[test]
fn test_lex_label_bang() {
    assert_lex_for(
        "foo!:",
        &[
            LexerState::BeginLabelable,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
        ],
        |src| vec![token(TokenKind::Label, pos_in(src, b"foo!:", 0), 0)],
    );
}

#[test]
fn test_lex_label_question() {
    assert_lex_for(
        "foo?:",
        &[
            LexerState::BeginLabelable,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
        ],
        |src| vec![token(TokenKind::Label, pos_in(src, b"foo?:", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_like() {
    assert_lex_except(
        ":foo123",
        &[
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_const_like() {
    assert_lex_except(
        ":Baz",
        &[
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":Baz", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_bang() {
    assert_lex_for(":foo123!", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123!", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ident_question() {
    assert_lex_for(":foo123?", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123?", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ident_eq() {
    assert_lex_for(":foo123=", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_unary_plus() {
    assert_lex_for(":+@", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":+@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_unary_minus() {
    assert_lex_for(":-@", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":-@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neg_implicit() {
    assert_lex_for(":!", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neg_explicit() {
    assert_lex_for(":!@", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_not_implicit() {
    assert_lex_for(":~", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_not_explicit() {
    assert_lex_for(":~@", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":~@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_power() {
    assert_lex_for(":**", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":**", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_mult() {
    assert_lex_for(":*", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":*", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_div() {
    assert_lex_for(":/", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":/", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_mod() {
    assert_lex_for(":%", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":%", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_plus() {
    assert_lex_for(":+", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":+", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_minus() {
    assert_lex_for(":-", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":-", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_lshift() {
    assert_lex_for(":<<", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<<", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_rshift() {
    assert_lex_for(":>>", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>>", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_and() {
    assert_lex_for(":&", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":&", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_or() {
    assert_lex_for(":|", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":|", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_xor() {
    assert_lex_for(":^", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":^", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_lt() {
    assert_lex_for(":<", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_le() {
    assert_lex_for(":<=", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_gt() {
    assert_lex_for(":>", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ge() {
    assert_lex_for(":>=", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_eq_cmp() {
    assert_lex_for(":==", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":==", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neq() {
    assert_lex_for(":!=", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_eqeqeq() {
    assert_lex_for(":===", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":===", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_match() {
    assert_lex_for(":=~", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":=~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_nmatch() {
    assert_lex_for(":!~", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_aref() {
    assert_lex_for(":[]", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":[]", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_aset() {
    assert_lex_for(":[]=", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":[]=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ivar() {
    assert_lex_for(":@foo", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":@foo", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_cvar() {
    assert_lex_for(":@@foo", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":@@foo", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_gvar() {
    assert_lex_for(":$foo", &[LexerState::Begin], |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":$foo", 0), 0)]
    });
}

#[test]
fn test_lex_ivar_name_simple() {
    assert_lex("@foo123", |src| {
        vec![token(TokenKind::IvarName, pos_in(src, b"@foo123", 0), 0)]
    });
}

#[test]
fn test_lex_ivar_name_cap() {
    assert_lex("@Baz", |src| {
        vec![token(TokenKind::IvarName, pos_in(src, b"@Baz", 0), 0)]
    });
}

#[test]
fn test_lex_ivar_name_non_ascii() {
    assert_lex("@あ", |src| {
        vec![token(
            TokenKind::IvarName,
            pos_in(src, b"@\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ivar_name_invalid_non_ascii() {
    let src = EStrRef::from_bytes(b"@\xE3\x81", Encoding::UTF_8);
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"@\xE3\x81", 0),
            message: "The instance variable name contains invalid characters".to_owned(),
        }]
    );
}

#[test]
fn test_lex_ivar_name_invalid_digit() {
    let src = EStrRef::from("@123");
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"@123", 0),
            message: "Invalid instance variable name".to_owned(),
        }]
    );
}

#[test]
fn test_lex_cvar_name_simple() {
    assert_lex("@@foo123", |src| {
        vec![token(TokenKind::CvarName, pos_in(src, b"@@foo123", 0), 0)]
    });
}

#[test]
fn test_lex_cvar_name_cap() {
    assert_lex("@@Baz", |src| {
        vec![token(TokenKind::CvarName, pos_in(src, b"@@Baz", 0), 0)]
    });
}

#[test]
fn test_lex_cvar_name_non_ascii() {
    assert_lex("@@あ", |src| {
        vec![token(
            TokenKind::CvarName,
            pos_in(src, b"@@\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_cvar_name_invalid_non_ascii() {
    let src = EStrRef::from_bytes(b"@@\xE3\x81", Encoding::UTF_8);
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"@@\xE3\x81", 0),
            message: "The class variable name contains invalid characters".to_owned(),
        }]
    );
}

#[test]
fn test_lex_cvar_name_invalid_digit() {
    let src = EStrRef::from("@@123");
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"@@123", 0),
            message: "Invalid class variable name".to_owned(),
        }]
    );
}

#[test]
fn test_lex_gvar_name_simple() {
    assert_lex("$foo123", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$foo123", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_name_cap() {
    assert_lex("$Baz", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$Baz", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_non_ascii() {
    assert_lex("$あ", |src| {
        vec![token(
            TokenKind::GvarName,
            pos_in(src, b"$\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_invalid_non_ascii() {
    let src = EStrRef::from_bytes(b"$\xE3\x81", Encoding::UTF_8);
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"$\xE3\x81", 0),
            message: "The global variable name contains invalid characters".to_owned(),
        }]
    );
}

#[test]
fn test_lex_gvar_underscore() {
    assert_lex("$_", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$_", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_bang() {
    assert_lex("$!", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$!", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dquote() {
    assert_lex("$\"", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$\"", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dollar() {
    assert_lex("$$", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$$", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_amp() {
    assert_lex("$&", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$&", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_quote() {
    assert_lex("$'", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$'", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_star() {
    assert_lex("$*", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$*", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_plus() {
    assert_lex("$+", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$+", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_comma() {
    assert_lex("$,", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$,", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dot() {
    assert_lex("$.", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$.", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_slash() {
    assert_lex("$/", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$/", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_colon() {
    assert_lex("$:", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$:", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_semicolon() {
    assert_lex("$;", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$;", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_lt() {
    assert_lex("$<", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$<", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_eq() {
    assert_lex("$=", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$=", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_gt() {
    assert_lex("$>", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$>", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_question() {
    assert_lex("$?", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$?", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_at() {
    assert_lex("$@", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$@", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_backslash() {
    assert_lex("$\\", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$\\", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_backtick() {
    assert_lex("$`", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$`", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_tilde() {
    assert_lex("$~", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$~", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_zero() {
    assert_lex("$0", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$0", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_num_simple() {
    assert_lex("$5", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$5", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_num_large() {
    assert_lex("$123", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$123", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dashed_simple() {
    assert_lex("$-I", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$-I", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dashed_digit() {
    assert_lex("$-9", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$-9", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dashed_non_ascii() {
    assert_lex("$-あ", |src| {
        vec![token(
            TokenKind::GvarName,
            pos_in(src, b"$-\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_simple() {
    assert_lex("123", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"123", 0), 0)]
    });
}

#[test]
fn test_lex_integer_positive() {
    assert_lex_except(
        "+123",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::Numeric, pos_in(src, b"+123", 0), 0)],
    );
}

#[test]
fn test_lex_integer_underscore() {
    assert_lex("1_2_3", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1_2_3", 0), 0)]
    });
}

#[test]
fn test_lex_integer_hex_small() {
    assert_lex("0xff", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0xff", 0), 0)]
    });
}

#[test]
fn test_lex_integer_hex_capital() {
    assert_lex("0XFF", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0XFF", 0), 0)]
    });
}

#[test]
fn test_lex_integer_explicit_dec_small() {
    assert_lex("0d0129", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0d0129", 0), 0)]
    });
}

#[test]
fn test_lex_integer_explicit_dec_capital() {
    assert_lex("0D0129", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0D0129", 0), 0)]
    });
}

#[test]
fn test_lex_integer_explicit_oct_small() {
    assert_lex("0o0127", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0o0127", 0), 0)]
    });
}

#[test]
fn test_lex_integer_explicit_oct_capital() {
    assert_lex("0O0127", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0O0127", 0), 0)]
    });
}

#[test]
fn test_lex_integer_oct() {
    assert_lex("0127", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0127", 0), 0)]
    });
}

#[test]
fn test_lex_float_simple() {
    assert_lex("1.0", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1.0", 0), 0)]
    });
}

#[test]
fn test_lex_float_positive() {
    assert_lex_except(
        "+1.0",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::Numeric, pos_in(src, b"+1.0", 0), 0)],
    );
}

#[test]
fn test_lex_float_zero() {
    assert_lex("0.5", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0.5", 0), 0)]
    });
}

#[test]
fn test_lex_float_with_exponent_simple() {
    assert_lex("1e-3", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1e-3", 0), 0)]
    });
}

#[test]
fn test_lex_float_with_exponent_capital() {
    assert_lex("1E-3", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1E-3", 0), 0)]
    });
}

#[test]
fn test_lex_float_with_exponent_nosign() {
    assert_lex("1e3", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1e3", 0), 0)]
    });
}

#[test]
fn test_lex_float_with_exponent_negative() {
    assert_lex("1e-3", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1e-3", 0), 0)]
    });
}

#[test]
fn test_lex_float_with_exponent_positive() {
    assert_lex("1e+3", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1e+3", 0), 0)]
    });
}

#[test]
fn test_lex_float_with_exponent_leading_zero() {
    assert_lex("1e+09", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1e+09", 0), 0)]
    });
}

#[test]
fn test_lex_float_with_point_and_exponent() {
    assert_lex("1.53e-4", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1.53e-4", 0), 0)]
    });
}

#[test]
fn test_lex_rational_simple() {
    assert_lex("3r", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"3r", 0), 0)]
    });
}

#[test]
fn test_lex_rational_positive() {
    assert_lex_except(
        "+3r",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::Numeric, pos_in(src, b"+3r", 0), 0)],
    );
}

#[test]
fn test_lex_rational_float() {
    assert_lex("3.52r", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"3.52r", 0), 0)]
    });
}

#[test]
fn test_lex_rational_with_base() {
    assert_lex("0xFFr", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"0xFFr", 0), 0)]
    });
}

#[test]
fn test_lex_imaginary_integer_simple() {
    assert_lex("123i", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"123i", 0), 0)]
    });
}

#[test]
fn test_lex_imaginary_integer_positive() {
    assert_lex_except(
        "+123i",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::Numeric, pos_in(src, b"+123i", 0), 0)],
    );
}

#[test]
fn test_lex_imaginary_float_simple() {
    assert_lex("1.5i", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1.5i", 0), 0)]
    });
}

#[test]
fn test_lex_imaginary_rational_simple() {
    assert_lex("1.5ri", |src| {
        vec![token(TokenKind::Numeric, pos_in(src, b"1.5ri", 0), 0)]
    });
}

#[test]
fn test_quote_string_tokens_simple_nolabelable() {
    assert_lex_except(
        "' foo '",
        &[
            LexerState::BeginLabelable,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
        ],
        |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
            ]
        },
    );
}

#[test]
fn test_quote_string_tokens_simple_labelable() {
    assert_lex_for(
        "' foo '",
        &[
            LexerState::BeginLabelable,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
        ],
        |src| {
            vec![
                token(TokenKind::StringBeginLabelable, pos_in(src, b"'", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
            ]
        },
    );
}

#[test]
fn test_quote_string_tokens_labelled() {
    assert_lex_for(
        "' foo ':",
        &[
            LexerState::BeginLabelable,
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
        ],
        |src| {
            vec![
                token(TokenKind::StringBeginLabelable, pos_in(src, b"'", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEndColon, pos_in(src, b"':", 0), 0),
            ]
        },
    );
}

#[test]
fn test_quote_string_tokens_escaped() {
    assert_lex_for("'\\''", &[LexerState::Begin], |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, "\\'", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 2), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_backslashes() {
    assert_lex_for("'\\\\'", &[LexerState::Begin], |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, "\\\\", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
        ]
    });
}

#[test]
fn test_double_quote_string_tokens_simple() {
    assert_lex_for("\" foo \"", &[LexerState::Begin], |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"\"", 1), 0),
        ]
    });
}

#[test]
fn test_double_quote_string_tokens_dynamic() {
    assert_lex_for("\" foo #{bar}", &[LexerState::Begin], |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringInterpolationBegin, pos_in(src, "#{", 0), 0),
            token(TokenKind::Identifier, pos_in(src, b"bar", 0), 0),
            token(TokenKind::RBrace, pos_in(src, b"}", 0), 0),
        ]
    });
}

#[test]
fn test_backtick_string_tokens() {
    assert_lex_except(
        "` foo `",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"`", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"`", 1), 0),
            ]
        },
    );
}

#[test]
fn test_backtick_op_name() {
    assert_lex_for(
        "`",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::MethodName, pos_in(src, b"`", 0), 0)],
    );
}

#[test]
fn test_regexp_string_tokens() {
    assert_lex_except(
        "/ foo /",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"/", 1), 0),
            ]
        },
    );
}

#[test]
fn test_op_assign_pow() {
    assert_lex("**=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Pow),
            pos_in(src, b"**=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_mult() {
    assert_lex("*=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Mul),
            pos_in(src, b"*=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_div_left_spaced() {
    assert_lex_for(
        " /=",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::OpAssign(BinOpKind::Div),
                pos_in(src, b"/=", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_regexp_begin_op_assign_like_left_spaced() {
    assert_lex_except(
        " /=",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"/", 0), 1),
                token(TokenKind::StringContent, pos_in(src, b"=", 0), 1),
            ]
        },
    );
}

#[test]
fn test_op_assign_div_nospaced() {
    assert_lex_for(
        "/=",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::OpAssign(BinOpKind::Div),
                pos_in(src, b"/=", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_regexp_begin_op_assign_like_nospaced() {
    assert_lex_except(
        "/=",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0),
                token(TokenKind::StringContent, pos_in(src, b"=", 0), 0),
            ]
        },
    );
}

#[test]
fn test_op_assign_mod() {
    assert_lex("%=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Mod),
            pos_in(src, b"%=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_add() {
    assert_lex("+=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Add),
            pos_in(src, b"+=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_sub() {
    assert_lex("-=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Sub),
            pos_in(src, b"-=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_lshift() {
    assert_lex("<<=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::LShift),
            pos_in(src, b"<<=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_rshift() {
    assert_lex(">>=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::RShift),
            pos_in(src, b">>=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_bitwise_and() {
    assert_lex("&=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::BitwiseAnd),
            pos_in(src, b"&=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_bitwise_or() {
    assert_lex("|=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::BitwiseOr),
            pos_in(src, b"|=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_bitwise_xor() {
    assert_lex("^=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::BitwiseXor),
            pos_in(src, b"^=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_logical_and() {
    assert_lex("&&=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::LogicalAnd),
            pos_in(src, b"&&=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_logical_or() {
    assert_lex("||=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::LogicalOr),
            pos_in(src, b"||=", 0),
            0,
        )]
    });
}

#[test]
fn test_excl() {
    assert_lex("!", |src| {
        vec![token(
            TokenKind::UnOp(UnOpKind::Not),
            pos_in(src, b"!", 0),
            0,
        )]
    });
}

#[test]
fn test_excl_at_join() {
    assert_lex_for(
        "!@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::MethodName, pos_in(src, b"!@", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_excl_at_separate() {
    assert_lex_except(
        "!@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::UnOp(UnOpKind::Not), pos_in(src, b"!", 0), 0),
                token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_excl_eq() {
    assert_lex("!=", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::NotEq),
            pos_in(src, b"!=", 0),
            0,
        )]
    });
}

#[test]
fn test_excl_tilde() {
    assert_lex("!~", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::NotMatch),
            pos_in(src, b"!~", 0),
            0,
        )]
    });
}

#[test]
fn test_percent() {
    assert_lex("%", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Mod),
            pos_in(src, b"%", 0),
            0,
        )]
    });
}

#[test]
fn test_amp_infix_spaced() {
    assert_lex_for(
        " & ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::BitwiseAnd),
                pos_in(src, b"&", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_amp_prefix_spaced() {
    assert_lex_except(
        " & ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 1)],
    );
}

#[test]
fn test_amp_infix_left_spaced() {
    assert_lex_for(
        " &",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::BitwiseAnd),
                pos_in(src, b"&", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_amp_prefix_left_spaced() {
    assert_lex_except(
        " &",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 1)],
    );
}

#[test]
fn test_amp_infix_nospaced() {
    assert_lex_for(
        "&",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::BitwiseAnd),
                pos_in(src, b"&", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_amp_prefix_nospaced() {
    assert_lex_except(
        "&",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 0)],
    );
}

#[test]
fn test_amp_amp() {
    assert_lex("&&", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::LogicalAnd),
            pos_in(src, b"&&", 0),
            0,
        )]
    });
}

#[test]
fn test_amp_dot() {
    assert_lex("&.", |src| {
        vec![token(TokenKind::AmpDot, pos_in(src, b"&.", 0), 0)]
    });
}

#[test]
fn test_lparen_spaced() {
    assert_lex_except(
        " ( ",
        &[LexerState::FirstArgument, LexerState::WeakFirstArgument],
        |src| vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 1)],
    );
}

#[test]
fn test_lparen_noarg_spaced() {
    assert_lex_for(
        " ( ",
        &[LexerState::FirstArgument, LexerState::WeakFirstArgument],
        |src| vec![token(TokenKind::LParenRestricted, pos_in(src, b"(", 0), 1)],
    );
}

#[test]
fn test_lparen_left_spaced() {
    assert_lex_except(
        " (",
        &[LexerState::FirstArgument, LexerState::WeakFirstArgument],
        |src| vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 1)],
    );
}

#[test]
fn test_lparen_noarg_left_spaced() {
    assert_lex_for(
        " (",
        &[LexerState::FirstArgument, LexerState::WeakFirstArgument],
        |src| vec![token(TokenKind::LParenRestricted, pos_in(src, b"(", 0), 1)],
    );
}

#[test]
fn test_lparen_nospaced() {
    assert_lex("(", |src| {
        vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 0)]
    });
}

#[test]
fn test_rparen() {
    assert_lex(")", |src| {
        vec![token(TokenKind::RParen, pos_in(src, b")", 0), 0)]
    });
}

#[test]
fn test_star_infix_spaced() {
    assert_lex_for(
        " * ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Mul),
                pos_in(src, b"*", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_star_prefix_spaced() {
    assert_lex_except(
        " * ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 1)],
    );
}

#[test]
fn test_star_infix_left_spaced() {
    assert_lex_for(
        " *",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Mul),
                pos_in(src, b"*", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_star_prefix_left_spaced() {
    assert_lex_except(
        " *",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 1)],
    );
}

#[test]
fn test_star_infix_nospaced() {
    assert_lex_for(
        "*",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Mul),
                pos_in(src, b"*", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_star_prefix_nospaced() {
    assert_lex_except(
        "*",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 0)],
    );
}

#[test]
fn test_star_star_infix_spaced() {
    assert_lex_for(
        " ** ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Pow),
                pos_in(src, b"**", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_star_star_prefix_spaced() {
    assert_lex_except(
        " ** ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 1)],
    );
}

#[test]
fn test_star_star_infix_left_spaced() {
    assert_lex_for(
        " **",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Pow),
                pos_in(src, b"**", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_star_star_prefix_left_spaced() {
    assert_lex_except(
        " **",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 1)],
    );
}

#[test]
fn test_star_star_infix_nospaced() {
    assert_lex_for(
        "**",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Pow),
                pos_in(src, b"**", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_star_star_prefix_nospaced() {
    assert_lex_except(
        "**",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 0)],
    );
}

#[test]
fn test_plus_infix_spaced() {
    assert_lex_for(
        " + ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Add),
                pos_in(src, b"+", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_plus_prefix_spaced() {
    assert_lex_except(
        " + ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::UnOp(UnOpKind::Plus),
                pos_in(src, b"+", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_plus_infix_left_spaced() {
    assert_lex_for(
        " +",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Add),
                pos_in(src, b"+", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_plus_prefix_left_spaced() {
    assert_lex_except(
        " +",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::UnOp(UnOpKind::Plus),
                pos_in(src, b"+", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_plus_infix_nospaced() {
    assert_lex_for(
        "+",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Add),
                pos_in(src, b"+", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_plus_prefix_nospaced() {
    assert_lex_except(
        "+",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::UnOp(UnOpKind::Plus),
                pos_in(src, b"+", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_plus_at_join() {
    assert_lex_for(
        "+@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::MethodName, pos_in(src, b"+@", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_plus_at_separate_infix() {
    assert_lex_for(
        "+@foo",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![
                token(TokenKind::BinOp(BinOpKind::Add), pos_in(src, b"+", 0), 0),
                token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_plus_at_separate_prefix() {
    assert_lex_except(
        "+@foo",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::UnOp(UnOpKind::Plus), pos_in(src, b"+", 0), 0),
                token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_comma() {
    assert_lex(",", |src| {
        vec![token(TokenKind::Comma, pos_in(src, b",", 0), 0)]
    });
}

#[test]
fn test_minus_infix_spaced() {
    assert_lex_for(
        " - ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Sub),
                pos_in(src, b"-", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_minus_prefix_spaced() {
    assert_lex_except(
        " - ",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::UnOp(UnOpKind::Minus),
                pos_in(src, b"-", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_minus_infix_left_spaced() {
    assert_lex_for(
        " -",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Sub),
                pos_in(src, b"-", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_minus_prefix_left_spaced() {
    assert_lex_except(
        " -",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::UnOp(UnOpKind::Minus),
                pos_in(src, b"-", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_minus_infix_nospaced() {
    assert_lex_for(
        "-",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Sub),
                pos_in(src, b"-", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_minus_prefix_nospaced() {
    assert_lex_except(
        "-",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::UnOp(UnOpKind::Minus),
                pos_in(src, b"-", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_minus_at_join() {
    assert_lex_for(
        "-@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::MethodName, pos_in(src, b"-@", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_minus_at_separate_infix() {
    assert_lex_for(
        "-@foo",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![
                token(TokenKind::BinOp(BinOpKind::Sub), pos_in(src, b"-", 0), 0),
                token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_minus_at_separate_prefix() {
    assert_lex_except(
        "-@foo",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::UnOp(UnOpKind::Minus), pos_in(src, b"-", 0), 0),
                token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_arrow() {
    assert_lex("->", |src| {
        vec![token(TokenKind::Arrow, pos_in(src, b"->", 0), 0)]
    });
}

#[test]
fn test_dot() {
    assert_lex(".", |src| {
        vec![token(TokenKind::Dot, pos_in(src, b".", 0), 0)]
    });
}

#[test]
fn test_dot_dot() {
    assert_lex("..", |src| {
        vec![token(TokenKind::DotDot, pos_in(src, b"..", 0), 0)]
    });
}

#[test]
fn test_dot_dot_dot() {
    assert_lex("...", |src| {
        vec![token(TokenKind::DotDotDot, pos_in(src, b"...", 0), 0)]
    });
}

#[test]
fn test_slash_spaced() {
    assert_lex_for(
        " / ",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Div),
                pos_in(src, b"/", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_regexp_begin_spaced() {
    assert_lex_except(
        " / ",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"/", 0), 1),
                token(TokenKind::StringContent, pos_in(src, " ", 1), 1),
            ]
        },
    );
}

#[test]
fn test_slash_left_spaced() {
    assert_lex_for(
        " /",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Div),
                pos_in(src, b"/", 0),
                1,
            )]
        },
    );
}

#[test]
fn test_regexp_begin_left_spaced() {
    assert_lex_except(
        " /",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::StringBegin, pos_in(src, b"/", 0), 1)],
    );
}

#[test]
fn test_slash_nospaced() {
    assert_lex_for(
        "/",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::Div),
                pos_in(src, b"/", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_regexp_begin_nospaced() {
    assert_lex_except(
        "/",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0)],
    );
}

#[test]
fn test_colon() {
    assert_lex(":", |src| {
        vec![token(TokenKind::Colon, pos_in(src, b":", 0), 0)]
    });
}

#[test]
fn test_colon_colon_infix_spaced() {
    assert_lex_for(
        " :: ",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 1)],
    );
}

#[test]
fn test_colon_colon_prefix_spaced() {
    assert_lex_except(
        " :: ",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 1)],
    );
}

#[test]
fn test_colon_colon_infix_left_spaced() {
    assert_lex_for(
        " ::",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 1)],
    );
}

#[test]
fn test_colon_colon_prefix_left_spaced() {
    assert_lex_except(
        " ::",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 1)],
    );
}

#[test]
fn test_colon_colon_infix_nospaced() {
    assert_lex_for(
        "::",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 0)],
    );
}

#[test]
fn test_colon_colon_prefix_nospaced() {
    assert_lex_except(
        "::",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 0)],
    );
}

#[test]
fn test_lex_semicolon() {
    assert_lex(";", |src| {
        vec![token(TokenKind::Semicolon, pos_in(src, b";", 0), 0)]
    });
}

#[test]
fn test_lt() {
    assert_lex("<", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Lt),
            pos_in(src, b"<", 0),
            0,
        )]
    });
}

#[test]
fn test_lt_lt() {
    assert_lex("<<", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::LShift),
            pos_in(src, b"<<", 0),
            0,
        )]
    });
}

#[test]
fn test_lt_eq() {
    assert_lex("<=", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Le),
            pos_in(src, b"<=", 0),
            0,
        )]
    });
}

#[test]
fn test_lt_eq_gt() {
    assert_lex("<=>", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Cmp),
            pos_in(src, b"<=>", 0),
            0,
        )]
    });
}

#[test]
fn test_eq() {
    assert_lex("=", |src| {
        vec![token(TokenKind::Eq, pos_in(src, b"=", 0), 0)]
    });
}

#[test]
fn test_eq_eq() {
    assert_lex("==", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Eq),
            pos_in(src, b"==", 0),
            0,
        )]
    });
}

#[test]
fn test_eq_eq_eq() {
    assert_lex("===", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Incl),
            pos_in(src, b"===", 0),
            0,
        )]
    });
}

#[test]
fn test_fat_arrow() {
    assert_lex("=>", |src| {
        vec![token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0)]
    });
}

#[test]
fn test_eq_match() {
    assert_lex("=~", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Match),
            pos_in(src, b"=~", 0),
            0,
        )]
    });
}

#[test]
fn test_gt() {
    assert_lex(">", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Gt),
            pos_in(src, b">", 0),
            0,
        )]
    });
}

#[test]
fn test_gt_eq() {
    assert_lex(">=", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Ge),
            pos_in(src, b">=", 0),
            0,
        )]
    });
}

#[test]
fn test_gt_gt() {
    assert_lex(">>", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::RShift),
            pos_in(src, b">>", 0),
            0,
        )]
    });
}

#[test]
fn test_question_simple() {
    assert_lex("?", |src| {
        vec![token(TokenKind::Question, pos_in(src, b"?", 0), 0)]
    });
}

#[test]
fn test_question_separate() {
    assert_lex_for(
        "?a",
        &[LexerState::WeakFirstArgument, LexerState::End],
        |src| {
            vec![
                token(TokenKind::Question, pos_in(src, b"?", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"a", 0), 0),
            ]
        },
    );
}

#[test]
fn test_char_literal() {
    assert_lex_except(
        "?a",
        &[LexerState::WeakFirstArgument, LexerState::End],
        |src| vec![token(TokenKind::CharLiteral, pos_in(src, b"?a", 0), 0)],
    );
}

#[test]
fn test_at() {
    assert_lex("@", |src| {
        vec![token(TokenKind::At, pos_in(src, b"@", 0), 0)]
    });
}

#[test]
fn test_lbracket_infix_spaced() {
    assert_lex_for(
        " [ ",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 1)],
    );
}

#[test]
fn test_lbracket_prefix_spaced() {
    assert_lex_except(
        " [ ",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 1)],
    );
}

#[test]
fn test_lbracket_infix_left_spaced() {
    assert_lex_for(
        " [",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 1)],
    );
}

#[test]
fn test_lbracket_prefix_left_spaced() {
    assert_lex_except(
        " [",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 1)],
    );
}

#[test]
fn test_lbracket_infix_nospaced() {
    assert_lex_for(
        "[",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 0)],
    );
}

#[test]
fn test_lbracket_prefix_nospaced() {
    assert_lex_except(
        "[",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0)],
    );
}

#[test]
fn test_aref_join() {
    assert_lex_for(
        "[]",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::MethodName, pos_in(src, b"[]", 0), 0)],
    );
}

#[test]
fn test_aref_separate_infix() {
    assert_lex_for(
        "[]",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![
                token(TokenKind::LBracket, pos_in(src, b"[", 0), 0),
                token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
            ]
        },
    );
}

#[test]
fn test_aref_separate_prefix() {
    assert_lex_except(
        "[]",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0),
                token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
            ]
        },
    );
}

#[test]
fn test_aset_join() {
    assert_lex_for(
        "[]=",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::MethodName, pos_in(src, b"[]=", 0), 0)],
    );
}

#[test]
fn test_aset_separate_infix() {
    assert_lex_for(
        "[]=",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
        ],
        |src| {
            vec![
                token(TokenKind::LBracket, pos_in(src, b"[", 0), 0),
                token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
                token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
            ]
        },
    );
}

#[test]
fn test_aset_separate_prefix() {
    assert_lex_except(
        "[]=",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0),
                token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
                token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
            ]
        },
    );
}

#[test]
fn test_rbracket() {
    assert_lex("]", |src| {
        vec![token(TokenKind::RBracket, pos_in(src, b"]", 0), 0)]
    });
}

#[test]
fn test_caret() {
    assert_lex("^", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::BitwiseXor),
            pos_in(src, b"^", 0),
            0,
        )]
    });
}

#[test]
fn test_lbrace() {
    assert_lex("{", |src| {
        vec![token(TokenKind::LBrace, pos_in(src, b"{", 0), 0)]
    });
}

#[test]
fn test_vert() {
    assert_lex("|", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::BitwiseOr),
            pos_in(src, b"|", 0),
            0,
        )]
    });
}

#[test]
fn test_vert_vert() {
    assert_lex_for(
        "||",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::LogicalOr),
                pos_in(src, b"||", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_split_vert_vert() {
    assert_lex_except(
        "||",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(
                    TokenKind::BinOp(BinOpKind::BitwiseOr),
                    pos_in(src, b"|", 0),
                    0,
                ),
                token(
                    TokenKind::BinOp(BinOpKind::BitwiseOr),
                    pos_in(src, b"|", 1),
                    0,
                ),
            ]
        },
    );
}

#[test]
fn test_rbrace() {
    assert_lex("}", |src| {
        vec![token(TokenKind::RBrace, pos_in(src, b"}", 0), 0)]
    });
}

#[test]
fn test_tilde() {
    assert_lex("~", |src| {
        vec![token(
            TokenKind::UnOp(UnOpKind::BitwiseNot),
            pos_in(src, b"~", 0),
            0,
        )]
    });
}

#[test]
fn test_tilde_at_join() {
    assert_lex_for(
        "~@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::MethodName, pos_in(src, b"~@", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_tilde_at_separate() {
    assert_lex_except(
        "~@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(
                    TokenKind::UnOp(UnOpKind::BitwiseNot),
                    pos_in(src, b"~", 0),
                    0,
                ),
                token(TokenKind::IvarName, pos_in(src, b"@foo", 0), 0),
            ]
        },
    );
}
