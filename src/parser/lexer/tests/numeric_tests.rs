use crate::{
    ast::pos_in,
    parser::lexer::{LexerState, TokenKind},
};

use super::{assert_lex, assert_lex_except, token};

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
