use crate::{ast::pos_in, parser::lexer::TokenKind};

use super::{assert_lex_except, assert_lex_for, token, LexerStates};

#[test]
fn test_lex_keyword_encoding() {
    assert_lex_for("__ENCODING__", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordCapitalDoubleUnderscoreEncoding,
            pos_in(src, b"__ENCODING__", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_line() {
    assert_lex_for("__LINE__", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordCapitalDoubleUnderscoreLine,
            pos_in(src, b"__LINE__", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_file() {
    assert_lex_for("__FILE__", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordCapitalDoubleUnderscoreFile,
            pos_in(src, b"__FILE__", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_cap_begin() {
    assert_lex_for("BEGIN", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordCapitalBegin,
            pos_in(src, b"BEGIN", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_cap_end() {
    assert_lex_for("END", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordCapitalEnd,
            pos_in(src, b"END", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_alias() {
    assert_lex_for("alias", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordAlias, pos_in(src, b"alias", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_and() {
    assert_lex_for("and", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordAnd, pos_in(src, b"and", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_begin() {
    assert_lex_for("begin", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordBegin, pos_in(src, b"begin", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_break() {
    assert_lex_for("break", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordBreak, pos_in(src, b"break", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_case() {
    assert_lex_for("case", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordCase, pos_in(src, b"case", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_class() {
    assert_lex_for("class", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordClass, pos_in(src, b"class", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_def() {
    assert_lex_for("def", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordDef, pos_in(src, b"def", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_defined_q() {
    assert_lex_for("defined?", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordDefinedQ,
            pos_in(src, b"defined?", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_do() {
    assert_lex_for("do", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordDo, pos_in(src, b"do", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_else() {
    assert_lex_for("else", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordElse, pos_in(src, b"else", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_elsif() {
    assert_lex_for("elsif", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordElsif, pos_in(src, b"elsif", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_end() {
    assert_lex_for("end", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordEnd, pos_in(src, b"end", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_ensure() {
    assert_lex_for("ensure", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordEnsure,
            pos_in(src, b"ensure", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_false() {
    assert_lex_for("false", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordFalse, pos_in(src, b"false", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_for() {
    assert_lex_for("for", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordFor, pos_in(src, b"for", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_if_prefix() {
    assert_lex_except("if", LexerStates::BeginOpt | LexerStates::END_ALL, |src| {
        vec![token(TokenKind::KeywordIf, pos_in(src, b"if", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_if_infix() {
    assert_lex_for("if", LexerStates::BeginOpt | LexerStates::END_ALL, |src| {
        vec![token(TokenKind::KeywordIfInfix, pos_in(src, b"if", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_in() {
    assert_lex_for("in", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordIn, pos_in(src, b"in", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_module() {
    assert_lex_for("module", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordModule,
            pos_in(src, b"module", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_next() {
    assert_lex_for("next", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordNext, pos_in(src, b"next", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_nil() {
    assert_lex_for("nil", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordNil, pos_in(src, b"nil", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_not() {
    assert_lex_for("not", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordNot, pos_in(src, b"not", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_or() {
    assert_lex_for("or", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordOr, pos_in(src, b"or", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_redo() {
    assert_lex_for("redo", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordRedo, pos_in(src, b"redo", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_rescue() {
    assert_lex_for("rescue", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordRescue,
            pos_in(src, b"rescue", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_retry() {
    assert_lex_for("retry", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordRetry, pos_in(src, b"retry", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_return() {
    assert_lex_for("return", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::KeywordReturn,
            pos_in(src, b"return", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_keyword_self() {
    assert_lex_for("self", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordSelf, pos_in(src, b"self", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_super() {
    assert_lex_for("super", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordSuper, pos_in(src, b"super", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_then() {
    assert_lex_for("then", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordThen, pos_in(src, b"then", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_true() {
    assert_lex_for("true", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordTrue, pos_in(src, b"true", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_undef() {
    assert_lex_for("undef", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordUndef, pos_in(src, b"undef", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_unless_prefix() {
    assert_lex_except(
        "unless",
        LexerStates::BeginOpt | LexerStates::END_ALL,
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
        LexerStates::BeginOpt | LexerStates::END_ALL,
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
        LexerStates::BeginOpt | LexerStates::END_ALL,
        |src| vec![token(TokenKind::KeywordUntil, pos_in(src, b"until", 0), 0)],
    );
}

#[test]
fn test_lex_keyword_until_infix() {
    assert_lex_for(
        "until",
        LexerStates::BeginOpt | LexerStates::END_ALL,
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
    assert_lex_for("when", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordWhen, pos_in(src, b"when", 0), 0)]
    });
}

#[test]
fn test_lex_keyword_while_prefix() {
    assert_lex_except(
        "while",
        LexerStates::BeginOpt | LexerStates::END_ALL,
        |src| vec![token(TokenKind::KeywordWhile, pos_in(src, b"while", 0), 0)],
    );
}

#[test]
fn test_lex_keyword_while_infix() {
    assert_lex_for(
        "while",
        LexerStates::BeginOpt | LexerStates::END_ALL,
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
    assert_lex_for("yield", LexerStates::ALL, |src| {
        vec![token(TokenKind::KeywordYield, pos_in(src, b"yield", 0), 0)]
    });
}
