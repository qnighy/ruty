use crate::{
    ast::pos_in,
    parser::lexer::{BinOpKind, NonLocalKind, TokenKind, UnOpKind},
};

use super::{assert_lex, token, LexerStates};

#[test]
fn test_plus_infix_spaced() {
    assert_lex(" + ", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Add),
            pos_in(src, b"+", 0),
            1,
        )]
    });
}

#[test]
fn test_plus_prefix_spaced() {
    assert_lex(" + ", LexerStates::BEGIN_ALL, |src| {
        vec![token(
            TokenKind::UnOp(UnOpKind::Plus),
            pos_in(src, b"+", 0),
            1,
        )]
    });
}

#[test]
fn test_plus_infix_left_spaced() {
    assert_lex(
        " +",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
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
    assert_lex(
        " +",
        LexerStates::BEGIN_ALL | LexerStates::FirstArgument,
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
    assert_lex("+", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Add),
            pos_in(src, b"+", 0),
            0,
        )]
    });
}

#[test]
fn test_plus_prefix_nospaced() {
    assert_lex("+", LexerStates::BEGIN_ALL, |src| {
        vec![token(
            TokenKind::UnOp(UnOpKind::Plus),
            pos_in(src, b"+", 0),
            0,
        )]
    });
}

#[test]
fn test_plus_at_join() {
    assert_lex("+@foo", LexerStates::METH_ALL, |src| {
        vec![
            token(TokenKind::MethodName, pos_in(src, b"+@", 0), 0),
            token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
        ]
    });
}

#[test]
fn test_plus_at_separate_infix() {
    assert_lex("+@foo", LexerStates::END_ALL, |src| {
        vec![
            token(TokenKind::BinOp(BinOpKind::Add), pos_in(src, b"+", 0), 0),
            token(
                TokenKind::NonLocal(NonLocalKind::Ivar),
                pos_in(src, b"@foo", 0),
                0,
            ),
        ]
    });
}

#[test]
fn test_plus_at_separate_prefix() {
    assert_lex("+@foo", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::UnOp(UnOpKind::Plus), pos_in(src, b"+", 0), 0),
            token(
                TokenKind::NonLocal(NonLocalKind::Ivar),
                pos_in(src, b"@foo", 0),
                0,
            ),
        ]
    });
}

#[test]
fn test_minus_infix_spaced() {
    assert_lex(" - ", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Sub),
            pos_in(src, b"-", 0),
            1,
        )]
    });
}

#[test]
fn test_minus_prefix_spaced() {
    assert_lex(" - ", LexerStates::BEGIN_ALL, |src| {
        vec![token(
            TokenKind::UnOp(UnOpKind::Minus),
            pos_in(src, b"-", 0),
            1,
        )]
    });
}

#[test]
fn test_minus_infix_left_spaced() {
    assert_lex(
        " -",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
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
    assert_lex(
        " -",
        LexerStates::BEGIN_ALL | LexerStates::FirstArgument,
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
    assert_lex("-", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Sub),
            pos_in(src, b"-", 0),
            0,
        )]
    });
}

#[test]
fn test_minus_prefix_nospaced() {
    assert_lex("-", LexerStates::BEGIN_ALL, |src| {
        vec![token(
            TokenKind::UnOp(UnOpKind::Minus),
            pos_in(src, b"-", 0),
            0,
        )]
    });
}

#[test]
fn test_minus_at_join() {
    assert_lex("-@foo", LexerStates::METH_ALL, |src| {
        vec![
            token(TokenKind::MethodName, pos_in(src, b"-@", 0), 0),
            token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
        ]
    });
}

#[test]
fn test_minus_at_separate_infix() {
    assert_lex("-@foo", LexerStates::END_ALL, |src| {
        vec![
            token(TokenKind::BinOp(BinOpKind::Sub), pos_in(src, b"-", 0), 0),
            token(
                TokenKind::NonLocal(NonLocalKind::Ivar),
                pos_in(src, b"@foo", 0),
                0,
            ),
        ]
    });
}

#[test]
fn test_minus_at_separate_prefix() {
    assert_lex("-@foo", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::UnOp(UnOpKind::Minus), pos_in(src, b"-", 0), 0),
            token(
                TokenKind::NonLocal(NonLocalKind::Ivar),
                pos_in(src, b"@foo", 0),
                0,
            ),
        ]
    });
}

#[test]
fn test_arrow() {
    assert_lex("->", LexerStates::ALL, |src| {
        vec![token(TokenKind::Arrow, pos_in(src, b"->", 0), 0)]
    });
}
