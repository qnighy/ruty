use crate::{ast::pos_in, parser::lexer::TokenKind};

use super::{assert_lex, token, LexerStates};

const FIRST_ARG_ALL: LexerStates = LexerStates::EMPTY
    .or(LexerStates::FirstArgument)
    .or(LexerStates::WeakFirstArgument);

#[test]
fn test_lparen_spaced() {
    assert_lex(" ( ", !FIRST_ARG_ALL, |src| {
        vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 1)]
    });
}

#[test]
fn test_lparen_noarg_spaced() {
    assert_lex(" ( ", FIRST_ARG_ALL, |src| {
        vec![token(TokenKind::LParenRestricted, pos_in(src, b"(", 0), 1)]
    });
}

#[test]
fn test_lparen_left_spaced() {
    assert_lex(" (", !FIRST_ARG_ALL, |src| {
        vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 1)]
    });
}

#[test]
fn test_lparen_noarg_left_spaced() {
    assert_lex(" (", FIRST_ARG_ALL, |src| {
        vec![token(TokenKind::LParenRestricted, pos_in(src, b"(", 0), 1)]
    });
}

#[test]
fn test_lparen_nospaced() {
    assert_lex("(", LexerStates::ALL, |src| {
        vec![token(TokenKind::LParen, pos_in(src, b"(", 0), 0)]
    });
}

#[test]
fn test_rparen() {
    assert_lex(")", LexerStates::ALL, |src| {
        vec![token(TokenKind::RParen, pos_in(src, b")", 0), 0)]
    });
}

#[test]
fn test_lbracket_infix_spaced() {
    assert_lex(
        " [ ",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
        |src| vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 1)],
    );
}

#[test]
fn test_lbracket_prefix_spaced() {
    assert_lex(
        " [ ",
        LexerStates::BEGIN_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 1)],
    );
}

#[test]
fn test_lbracket_infix_left_spaced() {
    assert_lex(
        " [",
        (LexerStates::END_ALL | LexerStates::METH_ALL) & !LexerStates::FirstArgument,
        |src| vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 1)],
    );
}

#[test]
fn test_lbracket_prefix_left_spaced() {
    assert_lex(
        " [",
        LexerStates::BEGIN_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 1)],
    );
}

#[test]
fn test_lbracket_infix_nospaced() {
    assert_lex("[", LexerStates::END_ALL | LexerStates::METH_ALL, |src| {
        vec![token(TokenKind::LBracket, pos_in(src, b"[", 0), 0)]
    });
}

#[test]
fn test_lbracket_prefix_nospaced() {
    assert_lex("[", LexerStates::BEGIN_ALL, |src| {
        vec![token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0)]
    });
}

#[test]
fn test_aref_join() {
    assert_lex("[]", LexerStates::METH_ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"[]", 0), 0)]
    });
}

#[test]
fn test_aref_separate_infix() {
    assert_lex("[]", LexerStates::END_ALL, |src| {
        vec![
            token(TokenKind::LBracket, pos_in(src, b"[", 0), 0),
            token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
        ]
    });
}

#[test]
fn test_aref_separate_prefix() {
    assert_lex("[]", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0),
            token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
        ]
    });
}

#[test]
fn test_aset_join() {
    assert_lex("[]=", LexerStates::METH_ALL, |src| {
        vec![token(TokenKind::MethodName, pos_in(src, b"[]=", 0), 0)]
    });
}

#[test]
fn test_aset_separate_infix() {
    assert_lex("[]=", LexerStates::END_ALL, |src| {
        vec![
            token(TokenKind::LBracket, pos_in(src, b"[", 0), 0),
            token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_aset_separate_prefix() {
    assert_lex("[]=", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::LBracketPrefix, pos_in(src, b"[", 0), 0),
            token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_rbracket() {
    assert_lex("]", LexerStates::ALL, |src| {
        vec![token(TokenKind::RBracket, pos_in(src, b"]", 0), 0)]
    });
}

#[test]
fn test_lbrace() {
    assert_lex("{", LexerStates::ALL, |src| {
        vec![token(TokenKind::LBrace, pos_in(src, b"{", 0), 0)]
    });
}

#[test]
fn test_rbrace() {
    assert_lex("}", LexerStates::ALL, |src| {
        vec![token(TokenKind::RBrace, pos_in(src, b"}", 0), 0)]
    });
}
