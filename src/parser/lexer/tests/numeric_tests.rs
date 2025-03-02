use num_bigint::BigInt;
use ordered_float::NotNan;

use crate::{
    ast::{pos_in, Decimal, NumericValue},
    parser::lexer::{NumericToken, TokenKind},
};

use super::{assert_lex, token, LexerStates};

#[test]
fn test_lex_integer_simple() {
    assert_lex("123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(123)),
                imaginary: false,
            }),
            pos_in(src, b"123", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_positive() {
    assert_lex("+123", LexerStates::BEGIN_ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(123)),
                imaginary: false,
            }),
            pos_in(src, b"+123", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_underscore() {
    assert_lex("1_2_3", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(123)),
                imaginary: false,
            }),
            pos_in(src, b"1_2_3", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_hex_small() {
    assert_lex("0xff", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(255)),
                imaginary: false,
            }),
            pos_in(src, b"0xff", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_hex_capital() {
    assert_lex("0XFF", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(255)),
                imaginary: false,
            }),
            pos_in(src, b"0XFF", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_explicit_dec_small() {
    assert_lex("0d0129", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(129)),
                imaginary: false,
            }),
            pos_in(src, b"0d0129", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_explicit_dec_capital() {
    assert_lex("0D0129", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(129)),
                imaginary: false,
            }),
            pos_in(src, b"0D0129", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_explicit_oct_small() {
    assert_lex("0o0127", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(0o127)),
                imaginary: false,
            }),
            pos_in(src, b"0o0127", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_explicit_oct_capital() {
    assert_lex("0O0127", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(0o127)),
                imaginary: false,
            }),
            pos_in(src, b"0O0127", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_oct() {
    assert_lex("0127", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(0o127)),
                imaginary: false,
            }),
            pos_in(src, b"0127", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_simple() {
    assert_lex("1.0", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.0).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"1.0", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_positive() {
    assert_lex("+1.0", LexerStates::BEGIN_ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.0).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"+1.0", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_zero() {
    assert_lex("0.5", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(0.5).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"0.5", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_with_exponent_simple() {
    assert_lex("1e-3", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.0e-3).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"1e-3", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_with_exponent_capital() {
    assert_lex("1E-3", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.0e-3).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"1E-3", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_with_exponent_nosign() {
    assert_lex("1e3", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.0e3).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"1e3", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_with_exponent_negative() {
    assert_lex("1e-3", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.0e-3).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"1e-3", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_with_exponent_positive() {
    assert_lex("1e+3", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.0e3).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"1e+3", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_with_exponent_leading_zero() {
    assert_lex("1e+09", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.0e9).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"1e+09", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_float_with_point_and_exponent() {
    assert_lex("1.53e-4", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.53e-4).unwrap()),
                imaginary: false,
            }),
            pos_in(src, b"1.53e-4", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_rational_simple() {
    assert_lex("3r", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Rational(Decimal::from(3)),
                imaginary: false,
            }),
            pos_in(src, b"3r", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_rational_positive() {
    assert_lex("+3r", LexerStates::BEGIN_ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Rational(Decimal::from(3)),
                imaginary: false,
            }),
            pos_in(src, b"+3r", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_rational_float() {
    assert_lex("3.52r", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Rational(Decimal::from_fraction_and_exponent(
                    BigInt::from(352),
                    -2,
                )),
                imaginary: false,
            }),
            pos_in(src, b"3.52r", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_rational_with_base() {
    assert_lex("0xFFr", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Rational(Decimal::from(255)),
                imaginary: false,
            }),
            pos_in(src, b"0xFFr", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_imaginary_integer_simple() {
    assert_lex("123i", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(123)),
                imaginary: true,
            }),
            pos_in(src, b"123i", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_imaginary_integer_positive() {
    assert_lex("+123i", LexerStates::BEGIN_ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(123)),
                imaginary: true,
            }),
            pos_in(src, b"+123i", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_imaginary_float_simple() {
    assert_lex("1.5i", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Float(NotNan::new(1.5).unwrap()),
                imaginary: true,
            }),
            pos_in(src, b"1.5i", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_imaginary_rational_simple() {
    assert_lex("1.5ri", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Rational(Decimal::from_fraction_and_exponent(
                    BigInt::from(15),
                    -1,
                )),
                imaginary: true,
            }),
            pos_in(src, b"1.5ri", 0),
            0,
        )]
    });
}
