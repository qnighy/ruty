use crate::{
    ast::pos_in,
    parser::lexer::{BinOpKind, LexerState, TokenKind},
};

use super::{assert_lex, assert_lex_except, assert_lex_for, token};

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
fn test_op_assign_mod() {
    assert_lex("%=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Mod),
            pos_in(src, b"%=", 0),
            0,
        )]
    });
}
