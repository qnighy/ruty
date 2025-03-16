use num_bigint::BigInt;
use ordered_float::NotNan;

use crate::{
    ast::{pos_in, Decimal, NumericValue},
    parser::{
        lexer::{NumericToken, TokenKind},
        BinOpKind, UnOpKind,
    },
    Diagnostic,
};

use super::{assert_lex, assert_lex_with_diag, token, LexerStates};

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
fn test_lex_integer_split_double_positive() {
    assert_lex("++123", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::UnOp(UnOpKind::Plus), pos_in(src, b"+", 0), 0),
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: false,
                }),
                pos_in(src, b"+123", 0),
                0,
            ),
        ]
    });
}

#[test]
fn test_lex_integer_split_negative() {
    assert_lex("-123", LexerStates::BEGIN_ALL, |src| {
        vec![
            token(TokenKind::UnOp(UnOpKind::Minus), pos_in(src, b"-", 0), 0),
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: false,
                }),
                pos_in(src, b"123", 0),
                0,
            ),
        ]
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
fn test_lex_integer_invalid_double_underscores() {
    assert_lex_with_diag(
        "1__2",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(12)),
                    imaginary: false,
                }),
                pos_in(src, b"1__2", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1__2", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_invalid_hex_double_underscores() {
    assert_lex_with_diag(
        "0xA__B",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0xAB)),
                    imaginary: false,
                }),
                pos_in(src, b"0xA__B", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0xA__B", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_invalid_trailing_underscore() {
    assert_lex_with_diag(
        "123_",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: false,
                }),
                pos_in(src, b"123_", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"123_", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_invalid_hex_trailing_underscore() {
    assert_lex_with_diag(
        "0xABC_",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0xABC)),
                    imaginary: false,
                }),
                pos_in(src, b"0xABC_", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0xABC_", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
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
fn test_lex_integer_hex_invalid_empty_body() {
    assert_lex_with_diag(
        "0x",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0)),
                    imaginary: false,
                }),
                pos_in(src, b"0x", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0x", 0),
                message: "Invalid Numeric literal: the body cannot be empty".to_owned(),
            }]
        },
    );
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
fn test_lex_integer_explicit_dec_invalid_empty_body() {
    assert_lex_with_diag(
        "0d",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0)),
                    imaginary: false,
                }),
                pos_in(src, b"0d", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0d", 0),
                message: "Invalid Numeric literal: the body cannot be empty".to_owned(),
            }]
        },
    );
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
fn test_lex_integer_explicit_oct_invalid_empty_body() {
    assert_lex_with_diag(
        "0o",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0)),
                    imaginary: false,
                }),
                pos_in(src, b"0o", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0o", 0),
                message: "Invalid Numeric literal: the body cannot be empty".to_owned(),
            }]
        },
    );
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
fn test_lex_integer_oct_invalid_digit() {
    assert_lex_with_diag(
        "0o078",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0o077)),
                    imaginary: false,
                }),
                pos_in(src, b"0o078", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0o078", 0),
                message: "Invalid Numeric literal: invalid digit in this base".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_bin_small() {
    assert_lex("0b01110110010", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(0b01110110010)),
                imaginary: false,
            }),
            pos_in(src, b"0b01110110010", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_bin_capital() {
    assert_lex("0B01110110010", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::Numeric(NumericToken {
                value: NumericValue::Integer(BigInt::from(0b01110110010)),
                imaginary: false,
            }),
            pos_in(src, b"0B01110110010", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_integer_bin_invalid_digit() {
    assert_lex_with_diag(
        "0b10102",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0b10101)),
                    imaginary: false,
                }),
                pos_in(src, b"0b10102", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0b10102", 0),
                message: "Invalid Numeric literal: invalid digit in this base".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_explicit_bin_invalid_empty_body() {
    assert_lex_with_diag(
        "0b",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0)),
                    imaginary: false,
                }),
                pos_in(src, b"0b", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0b", 0),
                message: "Invalid Numeric literal: the body cannot be empty".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_invalid_suffix() {
    assert_lex_with_diag(
        "123foo",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: false,
                }),
                pos_in(src, b"123foo", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"123foo", 0),
                message: "Invalid Numeric literal: unknown suffix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_split_keyword_r() {
    // This ensures we do not split at `123r` simply because `r` looks
    // like a rational number suffix.
    assert_lex("123rescue", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: false,
                }),
                pos_in(src, b"123", 0),
                0,
            ),
            token(TokenKind::KeywordRescue, pos_in(src, b"rescue", 0), 0),
        ]
    });
}

#[test]
fn test_lex_integer_split_keyword_i() {
    // This ensures we do not split at `123i` simply because `i` looks
    // like an imaginary number suffix.
    assert_lex("123in", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: false,
                }),
                pos_in(src, b"123", 0),
                0,
            ),
            token(TokenKind::KeywordIn, pos_in(src, b"in", 0), 0),
        ]
    });
}

#[test]
fn test_lex_integer_split_keyword_e() {
    // This ensures we do not split at `123e` simply because `e` looks
    // like an exponent.
    assert_lex("123else", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: false,
                }),
                pos_in(src, b"123", 0),
                0,
            ),
            token(TokenKind::KeywordElse, pos_in(src, b"else", 0), 0),
        ]
    });
}

#[test]
fn test_lex_integer_hex_split_keyword() {
    assert_lex("0xEDor", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0xED)),
                    imaginary: false,
                }),
                pos_in(src, b"0xED", 0),
                0,
            ),
            token(TokenKind::KeywordOr, pos_in(src, b"or", 0), 0),
        ]
    });
}

#[test]
fn test_lex_integer_hex_invalid_split_keyword() {
    assert_lex_with_diag(
        "0xEDand",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(0xEDA)),
                    imaginary: false,
                }),
                pos_in(src, b"0xEDand", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0xEDand", 0),
                message: "Invalid Numeric literal: unknown suffix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_invalid_ri_suffix() {
    // Imaginary/Rational prefix and keyword cannot be combined
    assert_lex_with_diag(
        "123iin",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: true,
                }),
                pos_in(src, b"123iin", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"123iin", 0),
                message: "Invalid Numeric literal: unknown suffix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_dot() {
    assert_lex("123.to_s", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(123)),
                    imaginary: false,
                }),
                pos_in(src, b"123", 0),
                0,
            ),
            token(TokenKind::Dot, pos_in(src, b".", 0), 0),
            token(TokenKind::Identifier, pos_in(src, b"to_s", 0), 0),
        ]
    });
}

#[test]
fn test_lex_integer_error_reporting_extend_plus() {
    assert_lex_with_diag(
        "+1__2",
        LexerStates::BEGIN_ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(12)),
                    imaginary: false,
                }),
                pos_in(src, b"+1__2", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"+1__2", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_integer_error_reporting_extend_minus() {
    assert_lex_with_diag(
        "-1__2",
        LexerStates::BEGIN_ALL,
        |src| {
            vec![
                token(TokenKind::UnOp(UnOpKind::Minus), pos_in(src, b"-", 0), 0),
                token(
                    TokenKind::Numeric(NumericToken {
                        value: NumericValue::Integer(BigInt::from(12)),
                        imaginary: false,
                    }),
                    pos_in(src, b"1__2", 0),
                    0,
                ),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"-1__2", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
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
fn test_lex_float_split_keyword_r() {
    // This ensures we do not split at `123.0r` simply because `r` looks
    // like a rational number suffix.
    assert_lex("123.0rescue", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(123.0).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"123.0", 0),
                0,
            ),
            token(TokenKind::KeywordRescue, pos_in(src, b"rescue", 0), 0),
        ]
    });
}

#[test]
fn test_lex_float_split_keyword_i() {
    // This ensures we do not split at `123.0i` simply because `i` looks
    // like an imaginary number suffix.
    assert_lex("123.0in", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(123.0).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"123.0", 0),
                0,
            ),
            token(TokenKind::KeywordIn, pos_in(src, b"in", 0), 0),
        ]
    });
}

#[test]
fn test_lex_float_split_keyword_e() {
    // This ensures we do not split at `123.0e` simply because `e` looks
    // like an exponent.
    assert_lex("123.0else", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(123.0).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"123.0", 0),
                0,
            ),
            token(TokenKind::KeywordElse, pos_in(src, b"else", 0), 0),
        ]
    });
}

#[test]
fn test_lex_float_invalid_empty_integral() {
    assert_lex_with_diag(
        ".5",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(0.5).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b".5", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b".5", 0),
                message: "Invalid Numeric literal: the integral part cannot be empty".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_positive_empty_integral() {
    assert_lex_with_diag(
        "+.5",
        LexerStates::BEGIN_ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(0.5).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"+.5", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"+.5", 0),
                message: "Invalid Numeric literal: the integral part cannot be empty".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_double_underscores_in_integral() {
    assert_lex_with_diag(
        "1__4.32",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(14.32).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"1__4.32", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1__4.32", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_trailing_underscore_in_integral() {
    assert_lex_with_diag(
        "1_.32",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.32).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"1_.32", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1_.32", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_trailing_zero_underscore_in_integral() {
    assert_lex_with_diag(
        "0_.32",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(0.32).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"0_.32", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0_.32", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_double_underscores_in_fraction() {
    assert_lex_with_diag(
        "14.3__2",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(14.32).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"14.3__2", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"14.3__2", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_trailing_underscore_in_fraction() {
    assert_lex_with_diag(
        "2.5_",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(2.5).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"2.5_", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"2.5_", 0),
                message: "Invalid Numeric literal: invalid underscore placement".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_hex_prefix() {
    assert_lex_with_diag(
        "0x1.5",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.5).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"0x1.5", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0x1.5", 0),
                message: "Invalid Numeric literal: Float or Rational literal should not have a base prefix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_dec_prefix() {
    assert_lex_with_diag(
        "0d1.5",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.5).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"0d1.5", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0d1.5", 0),
                message: "Invalid Numeric literal: Float or Rational literal should not have a base prefix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_implicit_oct_prefix() {
    assert_lex_with_diag(
        "01.5",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.5).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"01.5", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"01.5", 0),
                message: "Invalid Numeric literal: decimal literal cannot have leading zeros"
                    .to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_explicit_oct_prefix() {
    assert_lex_with_diag(
        "0o1.5",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.5).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"0o1.5", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0o1.5", 0),
                message: "Invalid Numeric literal: Float or Rational literal should not have a base prefix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_explicit_bin_prefix() {
    assert_lex_with_diag(
        "0b1.5",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.5).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"0b1.5", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"0b1.5", 0),
                message: "Invalid Numeric literal: Float or Rational literal should not have a base prefix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_duplicate_points() {
    assert_lex_with_diag(
        "3.1.4",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(3.14).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"3.1.4", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"3.1.4", 0),
                message: "Invalid Numeric literal: it cannot have multiple decimal points"
                    .to_owned(),
            }]
        },
    );
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
fn test_lex_float_invalid_broken_nosign_exponent() {
    assert_lex_with_diag(
        "1.0e ",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.0).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"1.0e", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1.0e", 0),
                message: "Invalid Numeric literal: unknown suffix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_broken_positive_exponent() {
    assert_lex_with_diag(
        "1.0e+ ",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::Numeric(NumericToken {
                        value: NumericValue::Float(NotNan::new(1.0).unwrap()),
                        imaginary: false,
                    }),
                    pos_in(src, b"1.0e", 0),
                    0,
                ),
                token(TokenKind::BinOp(BinOpKind::Add), pos_in(src, b"+", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1.0e", 0),
                message: "Invalid Numeric literal: unknown suffix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_broken_negative_exponent() {
    assert_lex_with_diag(
        "1.0e- ",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::Numeric(NumericToken {
                        value: NumericValue::Float(NotNan::new(1.0).unwrap()),
                        imaginary: false,
                    }),
                    pos_in(src, b"1.0e", 0),
                    0,
                ),
                token(TokenKind::BinOp(BinOpKind::Sub), pos_in(src, b"-", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1.0e", 0),
                message: "Invalid Numeric literal: unknown suffix".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_duplicate_exponents_nosign() {
    assert_lex_with_diag(
        "1e3e4",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.0e3).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"1e3e4", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1e3e4", 0),
                message: "Invalid Numeric literal: exponent part cannot be itself exponential"
                    .to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_duplicate_exponents_positive_sign() {
    assert_lex_with_diag(
        "1e+3e+4",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.0e3).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"1e+3e+4", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1e+3e+4", 0),
                message: "Invalid Numeric literal: exponent part cannot be itself exponential"
                    .to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_duplicate_exponents_negative_sign() {
    assert_lex_with_diag(
        "1e-3e-4",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(1.0e-3).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"1e-3e-4", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1e-3e-4", 0),
                message: "Invalid Numeric literal: exponent part cannot be itself exponential"
                    .to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_float_invalid_with_exponent_and_point() {
    assert_lex_with_diag(
        "5e3.2",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Float(NotNan::new(5.0e3).unwrap()),
                    imaginary: false,
                }),
                pos_in(src, b"5e3.2", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"5e3.2", 0),
                message: "Invalid Numeric literal: exponent part cannot have a decimal point"
                    .to_owned(),
            }]
        },
    );
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
fn test_lex_rational_invalid_duplicate_r() {
    assert_lex_with_diag(
        "3rr",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Rational(Decimal::from(3)),
                    imaginary: false,
                }),
                pos_in(src, b"3rr", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"3rr", 0),
                message: "Invalid Numeric literal: it cannot have duplicate suffixes".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_rational_invalid_capital_r() {
    assert_lex_with_diag(
        "3R",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Rational(Decimal::from(3)),
                    imaginary: false,
                }),
                pos_in(src, b"3R", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"3R", 0),
                message: "Invalid Numeric literal: the suffixes should be lowercase".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_rational_invalid_exponent() {
    assert_lex_with_diag(
        "3e-1r",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Rational(Decimal::from_fraction_and_exponent(
                        BigInt::from(3),
                        -1,
                    )),
                    imaginary: false,
                }),
                pos_in(src, b"3e-1r", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"3e-1r", 0),
                message: "Invalid Numeric literal: Rational literal cannot have an exponent"
                    .to_owned(),
            }]
        },
    );
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

#[test]
fn test_lex_imaginary_invalid_duplicate_i() {
    assert_lex_with_diag(
        "9ii",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(9)),
                    imaginary: true,
                }),
                pos_in(src, b"9ii", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"9ii", 0),
                message: "Invalid Numeric literal: it cannot have duplicate suffixes".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_imaginary_invalid_capital_i() {
    assert_lex_with_diag(
        "9I",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Integer(BigInt::from(9)),
                    imaginary: true,
                }),
                pos_in(src, b"9I", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"9I", 0),
                message: "Invalid Numeric literal: the suffixes should be lowercase".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_imaginary_rational_invalid_ir() {
    assert_lex_with_diag(
        "1.5ir",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::Numeric(NumericToken {
                    value: NumericValue::Rational(Decimal::from_fraction_and_exponent(
                        BigInt::from(15),
                        -1,
                    )),
                    imaginary: true,
                }),
                pos_in(src, b"1.5ir", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"1.5ir", 0),
                message: "Invalid Numeric literal: the suffixes are in the wrong order".to_owned(),
            }]
        },
    );
}
