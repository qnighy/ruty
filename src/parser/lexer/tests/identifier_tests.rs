use crate::{
    ast::pos_in,
    encoding::EStrRef,
    parser::lexer::{BinOpKind, LexerState, TokenKind},
    Diagnostic, Encoding,
};

use super::{
    assert_lex, assert_lex_except, assert_lex_for, lex_all_with_diag_from, token, LexerStates,
};

const METH_FOR_DEF_ALL: LexerStates = LexerStates::EMPTY
    .or(LexerStates::MethForDef)
    .or(LexerStates::MethOrSymbolForDef);

const LABELABLE: LexerStates = LexerStates::EMPTY
    .or(LexerStates::BeginLabelable)
    .or(LexerStates::FirstArgument)
    .or(LexerStates::WeakFirstArgument);

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
    assert_lex_for("foo123=", METH_FOR_DEF_ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"foo123=", 0), 0)]
    });
}

#[test]
fn test_lex_ident_eq_separate() {
    assert_lex_except("foo123=", METH_FOR_DEF_ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
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
    assert_lex_for("foo123==>", METH_FOR_DEF_ALL, |src| {
        vec![
            token(TokenKind::MethodName, pos_in(src, b"foo123=", 0), 0),
            token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
        ]
    });
}

#[test]
fn test_lex_ident_eq_eq_gt_separate() {
    assert_lex_except("foo123==>", METH_FOR_DEF_ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Eq), pos_in(src, b"==", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Gt), pos_in(src, b">", 0), 0),
        ]
    });
}

#[test]
fn test_lex_label_simple() {
    assert_lex_for("foo123:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"foo123:", 0), 0)]
    });
}

#[test]
fn test_lex_label_cap() {
    assert_lex_for("Bar:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"Bar:", 0), 0)]
    });
}

#[test]
fn test_lex_label_keyword_like() {
    assert_lex_for("case:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"case:", 0), 0)]
    });
}

#[test]
fn test_lex_label_bang() {
    assert_lex_for("foo!:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"foo!:", 0), 0)]
    });
}

#[test]
fn test_lex_label_question() {
    assert_lex_for("foo?:", LABELABLE, |src| {
        vec![token(TokenKind::Label, pos_in(src, b"foo?:", 0), 0)]
    });
}
