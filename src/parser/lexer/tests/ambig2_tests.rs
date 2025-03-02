use crate::{
    ast::pos_in,
    parser::lexer::{BinOpKind, TokenKind},
};

use super::{assert_lex, assert_lex_except, assert_lex_for, token, LexerStates};

#[test]
fn test_amp_infix_spaced() {
    assert_lex_for(" & ", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::BitwiseAnd),
            pos_in(src, b"&", 0),
            1,
        )]
    });
}

#[test]
fn test_amp_prefix_spaced() {
    assert_lex_except(" & ", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 1)]
    });
}

#[test]
fn test_amp_infix_left_spaced() {
    assert_lex_for(
        " &",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
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
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
        |src| vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 1)],
    );
}

#[test]
fn test_amp_infix_nospaced() {
    assert_lex_for("&", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::BitwiseAnd),
            pos_in(src, b"&", 0),
            0,
        )]
    });
}

#[test]
fn test_amp_prefix_nospaced() {
    assert_lex_except("&", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(TokenKind::AmpPrefix, pos_in(src, b"&", 0), 0)]
    });
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
fn test_star_infix_spaced() {
    assert_lex_for(" * ", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Mul),
            pos_in(src, b"*", 0),
            1,
        )]
    });
}

#[test]
fn test_star_prefix_spaced() {
    assert_lex_except(" * ", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 1)]
    });
}

#[test]
fn test_star_infix_left_spaced() {
    assert_lex_for(
        " *",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
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
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
        |src| vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 1)],
    );
}

#[test]
fn test_star_infix_nospaced() {
    assert_lex_for("*", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Mul),
            pos_in(src, b"*", 0),
            0,
        )]
    });
}

#[test]
fn test_star_prefix_nospaced() {
    assert_lex_except("*", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(TokenKind::StarPrefix, pos_in(src, b"*", 0), 0)]
    });
}

#[test]
fn test_star_star_infix_spaced() {
    assert_lex_for(
        " ** ",
        LexerStates::END_ALL | LexerStates::METH_ALL,
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
        LexerStates::END_ALL | LexerStates::METH_ALL,
        |src| vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 1)],
    );
}

#[test]
fn test_star_star_infix_left_spaced() {
    assert_lex_for(
        " **",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
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
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
        |src| vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 1)],
    );
}

#[test]
fn test_star_star_infix_nospaced() {
    assert_lex_for("**", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Pow),
            pos_in(src, b"**", 0),
            0,
        )]
    });
}

#[test]
fn test_star_star_prefix_nospaced() {
    assert_lex_except("**", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(TokenKind::StarStarPrefix, pos_in(src, b"**", 0), 0)]
    });
}
