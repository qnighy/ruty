use crate::{ast::pos_in, parser::lexer::TokenKind};

use super::{assert_lex_except, assert_lex_for, token, LexerStates};

#[test]
fn test_lex_static_symbol_ident_like() {
    assert_lex_except(
        ":foo123",
        LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_const_like() {
    assert_lex_except(
        ":Baz",
        LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":Baz", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_bang() {
    assert_lex_for(":foo123!", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123!", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ident_question() {
    assert_lex_for(":foo123?", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123?", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ident_eq() {
    assert_lex_for(":foo123=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_unary_plus() {
    assert_lex_for(":+@", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":+@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_unary_minus() {
    assert_lex_for(":-@", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":-@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neg_implicit() {
    assert_lex_for(":!", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neg_explicit() {
    assert_lex_for(":!@", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_not_implicit() {
    assert_lex_for(":~", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_not_explicit() {
    assert_lex_for(":~@", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":~@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_power() {
    assert_lex_for(":**", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":**", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_mult() {
    assert_lex_for(":*", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":*", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_div() {
    assert_lex_for(":/", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":/", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_mod() {
    assert_lex_for(":%", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":%", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_plus() {
    assert_lex_for(":+", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":+", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_minus() {
    assert_lex_for(":-", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":-", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_lshift() {
    assert_lex_for(":<<", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<<", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_rshift() {
    assert_lex_for(":>>", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>>", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_and() {
    assert_lex_for(":&", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":&", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_or() {
    assert_lex_for(":|", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":|", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_xor() {
    assert_lex_for(":^", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":^", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_lt() {
    assert_lex_for(":<", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_le() {
    assert_lex_for(":<=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_gt() {
    assert_lex_for(":>", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ge() {
    assert_lex_for(":>=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_eq_cmp() {
    assert_lex_for(":==", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":==", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neq() {
    assert_lex_for(":!=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_eqeqeq() {
    assert_lex_for(":===", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":===", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_match() {
    assert_lex_for(":=~", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":=~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_nmatch() {
    assert_lex_for(":!~", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_aref() {
    assert_lex_for(":[]", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":[]", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_aset() {
    assert_lex_for(":[]=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":[]=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ivar() {
    assert_lex_for(":@foo", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":@foo", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_cvar() {
    assert_lex_for(":@@foo", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":@@foo", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_gvar() {
    assert_lex_for(":$foo", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":$foo", 0), 0)]
    });
}
