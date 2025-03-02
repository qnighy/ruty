use crate::{
    ast::pos_in,
    parser::lexer::{BinOpKind, TokenKind},
};

use super::{assert_lex, token, LexerStates};

#[test]
fn test_slash_spaced() {
    assert_lex(
        " / ",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
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
    assert_lex(
        " / ",
        LexerStates::BEGIN_ALL | LexerStates::FirstArgument,
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
    assert_lex(
        " /",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
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
    assert_lex(
        " /",
        LexerStates::BEGIN_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::StringBegin, pos_in(src, b"/", 0), 1)],
    );
}

#[test]
fn test_slash_nospaced() {
    assert_lex("/", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Div),
            pos_in(src, b"/", 0),
            0,
        )]
    });
}

#[test]
fn test_regexp_begin_nospaced() {
    assert_lex("/", LexerStates::BEGIN_ALL, |src| {
        vec![token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0)]
    });
}

#[test]
fn test_regexp_string_tokens() {
    assert_lex("/ foo /", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"/", 1), 0),
        ]
    });
}

#[test]
fn test_op_assign_div_left_spaced() {
    assert_lex(" /=", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Div),
            pos_in(src, b"/=", 0),
            1,
        )]
    });
}

#[test]
fn test_regexp_begin_op_assign_like_left_spaced() {
    assert_lex(" /=", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"/", 0), 1),
            token(TokenKind::StringContent, pos_in(src, b"=", 0), 1),
        ]
    });
}

#[test]
fn test_op_assign_div_nospaced() {
    assert_lex("/=", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Div),
            pos_in(src, b"/=", 0),
            0,
        )]
    });
}

#[test]
fn test_regexp_begin_op_assign_like_nospaced() {
    assert_lex("/=", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"/", 0), 0),
            token(TokenKind::StringContent, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_percent() {
    assert_lex("%", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Mod),
            pos_in(src, b"%", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_mod() {
    assert_lex("%=", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Mod),
            pos_in(src, b"%=", 0),
            0,
        )]
    });
}
