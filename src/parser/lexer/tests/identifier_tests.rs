use crate::{
    ast::pos_in,
    encoding::EStrRef,
    parser::lexer::{tests::assert_lex_with_diag, BinOpKind, TokenKind},
    Diagnostic, Encoding,
};

use super::{assert_lex, token, LexerStates};

const METH_FOR_DEF_ALL: LexerStates = LexerStates::EMPTY
    .or(LexerStates::MethForDef)
    .or(LexerStates::MethOrSymbolForDef);

const LABELABLE: LexerStates = LexerStates::EMPTY
    .or(LexerStates::BeginLabelable)
    .or(LexerStates::FirstArgument)
    .or(LexerStates::WeakFirstArgument);

#[test]
fn test_lex_ident_simple() {
    assert_lex("foo_123", LexerStates::ALL, |src| {
        vec![token(TokenKind::Identifier, pos_in(src, b"foo_123", 0), 0)]
    });
}

#[test]
fn test_lex_ident_underscore() {
    assert_lex("_", LexerStates::ALL, |src| {
        vec![token(TokenKind::Identifier, pos_in(src, b"_", 0), 0)]
    });
}

#[test]
fn test_lex_ident_start_with_underscore() {
    assert_lex("_foo123", LexerStates::ALL, |src| {
        vec![token(TokenKind::Identifier, pos_in(src, b"_foo123", 0), 0)]
    });
}

#[test]
fn test_lex_ident_underscore_num() {
    assert_lex("_123", LexerStates::ALL, |src| {
        vec![token(TokenKind::Identifier, pos_in(src, b"_123", 0), 0)]
    });
}

#[test]
fn test_lex_ident_non_ascii() {
    assert_lex("あ", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Identifier,
            pos_in(src, b"\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ident_invalid_non_ascii() {
    assert_lex_with_diag(
        EStrRef::from_bytes(b"\xE3\x81", Encoding::UTF_8),
        LexerStates::ALL,
        |src| vec![token(TokenKind::Identifier, pos_in(src, b"\xE3\x81", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"\xE3\x81", 0),
                message: "The identifier contains invalid characters".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_ident_ambiguous_non_ascii() {
    // 835C = ソ (which bytewise includes a backslash)
    assert_lex(
        EStrRef::from_bytes(b"\x83\x5C", Encoding::Windows_31J),
        LexerStates::ALL,
        |src| vec![token(TokenKind::Identifier, pos_in(src, b"\x83\x5C", 0), 0)],
    );
}

#[test]
fn test_lex_const_simple() {
    assert_lex("Foo_123", LexerStates::ALL, |src| {
        vec![token(TokenKind::Const, pos_in(src, b"Foo_123", 0), 0)]
    });
}

#[test]
fn test_lex_const_non_ascii() {
    assert_lex("Ω", LexerStates::ALL, |src| {
        vec![token(TokenKind::Const, pos_in(src, b"\xCE\xA9", 0), 0)]
    });
}

#[test]
fn test_lex_const_invalid_non_ascii() {
    assert_lex_with_diag(
        EStrRef::from_bytes(b"\xCE\xA9\xE3\x81", Encoding::UTF_8),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Const,
                pos_in(src, b"\xCE\xA9\xE3\x81", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"\xCE\xA9\xE3\x81", 0),
                message: "The identifier contains invalid characters".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_const_ambiguous_non_ascii() {
    // 835C = ソ (which bytewise includes a backslash)
    assert_lex(
        EStrRef::from_bytes(b"A\x83\x5C", Encoding::Windows_31J),
        LexerStates::ALL,
        |src| vec![token(TokenKind::Const, pos_in(src, b"A\x83\x5C", 0), 0)],
    );
}

#[test]
fn test_lex_ident_bang_simple() {
    assert_lex("foo123!", LexerStates::ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"foo123!", 0), 0)]
    });
}

#[test]
fn test_lex_ident_bang_capital() {
    assert_lex("Foo123!", LexerStates::ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"Foo123!", 0), 0)]
    });
}

#[test]
fn test_lex_ident_bang_keyword_like() {
    assert_lex("self!", LexerStates::ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"self!", 0), 0)]
    });
}

#[test]
fn test_lex_ident_bang_eq() {
    assert_lex("foo123!=", LexerStates::ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::NotEq), pos_in(src, b"!=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_q_simple() {
    assert_lex("foo123?", LexerStates::ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"foo123?", 0), 0)]
    });
}

#[test]
fn test_lex_ident_q_capital() {
    assert_lex("Foo123?", LexerStates::ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"Foo123?", 0), 0)]
    });
}

#[test]
fn test_lex_ident_q_keyword_like() {
    assert_lex("self?", LexerStates::ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"self?", 0), 0)]
    });
}

#[test]
fn test_lex_ident_q_eq() {
    assert_lex("foo123?=", LexerStates::ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::Question, pos_in(src, b"?", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_join() {
    assert_lex("foo123=", METH_FOR_DEF_ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"foo123=", 0), 0)]
    });
}

#[test]
fn test_lex_ident_eq_separate() {
    assert_lex("foo123=", !METH_FOR_DEF_ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_tilde() {
    assert_lex("foo123=~", LexerStates::ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Match), pos_in(src, b"=~", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_gt() {
    assert_lex("foo123=>", LexerStates::ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_eq() {
    assert_lex("foo123==", LexerStates::ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Eq), pos_in(src, b"==", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_eq_gt_join() {
    assert_lex("foo123==>", METH_FOR_DEF_ALL, |src| {
        vec![
            token(TokenKind::MethodName, pos_in(src, b"foo123=", 0), 0),
            token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_eq_gt_separate() {
    assert_lex("foo123==>", !METH_FOR_DEF_ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Eq), pos_in(src, b"==", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Gt), pos_in(src, b">", 0), 0),
        ]
    });
}

#[test]
fn test_lex_label_simple() {
    assert_lex("foo123:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"foo123:", 0), 0)]
    });
}

#[test]
fn test_lex_nolabel_simple() {
    assert_lex("foo123:", !LABELABLE, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::Colon, pos_in(src, b":", 0), 0),
        ]
    });
}

#[test]
fn test_lex_label_cap() {
    assert_lex("Bar:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"Bar:", 0), 0)]
    });
}

#[test]
fn test_lex_nolabel_cap() {
    assert_lex("Bar:", !LABELABLE, |src| {
        vec![
            token(TokenKind::Const, pos_in(src, b"Bar", 0), 0),
            token(TokenKind::Colon, pos_in(src, b":", 0), 0),
        ]
    });
}

#[test]
fn test_lex_label_keyword_like() {
    assert_lex("case:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"case:", 0), 0)]
    });
}

#[test]
fn test_lex_nolabel_keyword() {
    assert_lex("self:", !LABELABLE, |src| {
        vec![
            token(TokenKind::KeywordSelf, pos_in(src, b"self", 0), 0),
            token(TokenKind::Colon, pos_in(src, b":", 0), 0),
        ]
    });
}

#[test]
fn test_lex_label_bang() {
    assert_lex("foo!:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"foo!:", 0), 0)]
    });
}

#[test]
fn test_lex_label_nobang() {
    assert_lex("foo!:", !LABELABLE, |src| {
        vec![
            token(TokenKind::MethodName, pos_in(src, b"foo!", 0), 0),
            token(TokenKind::Colon, pos_in(src, b":", 0), 0),
        ]
    });
}

#[test]
fn test_lex_label_question() {
    assert_lex("foo?:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"foo?:", 0), 0)]
    });
}

#[test]
fn test_lex_nolabel_question() {
    assert_lex("foo?:", !LABELABLE, |src| {
        vec![
            token(TokenKind::MethodName, pos_in(src, b"foo?", 0), 0),
            token(TokenKind::Colon, pos_in(src, b":", 0), 0),
        ]
    });
}
