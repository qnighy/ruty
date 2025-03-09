use crate::{
    ast::pos_in,
    encoding::EStrRef,
    parser::{lexer::TokenKind, BinOpKind},
    Diagnostic, Encoding,
};

use super::{assert_lex, assert_lex_with_diag, token, LexerStates};

#[test]
fn test_lex_static_symbol_ident_like_simple() {
    assert_lex(
        ":foo_123",
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":foo_123", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_like_underscore() {
    assert_lex(
        ":_",
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":_", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_like_start_with_underscore() {
    assert_lex(
        ":_foo123",
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":_foo123", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_like_underscore_num() {
    assert_lex(
        ":_123",
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":_123", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_like_non_ascii() {
    assert_lex(
        ":あ",
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| {
            vec![token(
                TokenKind::Symbol,
                pos_in(src, b":\xE3\x81\x82", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_static_symbol_ident_like_invalid_non_ascii() {
    assert_lex_with_diag(
        EStrRef::from_bytes(b":\xE3\x81", Encoding::UTF_8),
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":\xE3\x81", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":\xE3\x81", 0),
                message: "The symbol contains invalid characters".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_ident_like_ambiguous_non_ascii() {
    // 835C = ソ (which bytewise includes a backslash)
    assert_lex(
        EStrRef::from_bytes(b":\x83\x5C", Encoding::Windows_31J),
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":\x83\x5C", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_like_invalid_numeric() {
    assert_lex_with_diag(
        ":123foo",
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":123foo", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":123foo", 0),
                message: "The symbol cannot start with a digit".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_keyword_like() {
    assert_lex(
        ":self",
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":self", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_const_like() {
    assert_lex(
        ":Baz",
        !(LexerStates::End | LexerStates::MethForDef | LexerStates::MethForCall),
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":Baz", 0), 0)],
    );
}

#[test]
fn test_lex_static_symbol_ident_bang() {
    assert_lex(":foo123!", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123!", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ident_split_bang_eq() {
    assert_lex(":foo123!=", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::NotEq), pos_in(src, b"!=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_static_symbol_ident_question() {
    assert_lex(":foo123?", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123?", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ident_split_question_eq() {
    assert_lex(":foo123?=", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0),
            token(TokenKind::Question, pos_in(src, b"?", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_static_symbol_ident_eq() {
    assert_lex(":foo123=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":foo123=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_split_ident_eq_tilde() {
    assert_lex(":foo123=~", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Match), pos_in(src, b"=~", 0), 0),
        ]
    });
}

#[test]
fn test_lex_static_symbol_split_ident_eq_gt() {
    assert_lex(":foo123=>", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0),
            token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
        ]
    });
}

#[test]
fn test_lex_static_symbol_split_ident_eq_eq() {
    assert_lex(":foo123==", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::Symbol, pos_in(src, b":foo123", 0), 0),
            token(TokenKind::BinOp(BinOpKind::Eq), pos_in(src, b"==", 0), 0),
        ]
    });
}

#[test]
fn test_lex_static_symbol_join_ident_eq_eq_gt() {
    assert_lex(":foo123==>", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::Symbol, pos_in(src, b":foo123=", 0), 0),
            token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
        ]
    });
}

#[test]
fn test_lex_static_symbol_unary_plus() {
    assert_lex(":+@", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":+@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_unary_minus() {
    assert_lex(":-@", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":-@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neg_implicit() {
    assert_lex(":!", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neg_explicit() {
    assert_lex(":!@", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_not_implicit() {
    assert_lex(":~", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_not_explicit() {
    assert_lex(":~@", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":~@", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_power() {
    assert_lex(":**", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":**", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_mult() {
    assert_lex(":*", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":*", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_div() {
    assert_lex(":/", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":/", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_mod() {
    assert_lex(":%", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":%", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_plus() {
    assert_lex(":+", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":+", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_minus() {
    assert_lex(":-", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":-", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_lshift() {
    assert_lex(":<<", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<<", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_rshift() {
    assert_lex(":>>", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>>", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_and() {
    assert_lex(":&", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":&", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_or() {
    assert_lex(":|", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":|", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_bitwise_xor() {
    assert_lex(":^", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":^", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_lt() {
    assert_lex(":<", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_le() {
    assert_lex(":<=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_gt() {
    assert_lex(":>", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_ge() {
    assert_lex(":>=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":>=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_eq_cmp() {
    assert_lex(":==", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":==", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_neq() {
    assert_lex(":!=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_eqeqeq() {
    assert_lex(":===", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":===", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_lteqgt() {
    assert_lex(":<=>", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":<=>", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_match() {
    assert_lex(":=~", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":=~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_nmatch() {
    assert_lex(":!~", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":!~", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_aref() {
    assert_lex(":[]", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":[]", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_aset() {
    assert_lex(":[]=", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":[]=", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_backtick() {
    assert_lex(":`", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":`", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_invalid_pow_eq() {
    assert_lex_with_diag(
        ":**=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":**=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":**=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_mul_eq() {
    assert_lex_with_diag(
        ":*=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":*=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":*=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_div_eq() {
    assert_lex_with_diag(
        ":/=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":/=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":/=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_mod_eq() {
    assert_lex_with_diag(
        ":%=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":%=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":%=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_split_add_eq() {
    // This testcase is a simplified one, but the behavior is necessary
    // to parse e.g. `{:+=>1}`.
    assert_lex(":+=", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::Symbol, pos_in(src, b":+", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_static_symbol_split_sub_eq() {
    // This testcase is a simplified one, but the behavior is necessary
    // to parse e.g. `{:-=>1}`.
    assert_lex(":-=", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::Symbol, pos_in(src, b":-", 0), 0),
            token(TokenKind::Eq, pos_in(src, b"=", 0), 0),
        ]
    });
}

#[test]
fn test_lex_static_symbol_invalid_lshift_eq() {
    assert_lex_with_diag(
        ":<<=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":<<=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":<<=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_rshift_eq() {
    assert_lex_with_diag(
        ":>>=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":>>=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":>>=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_bitwise_and_eq() {
    assert_lex_with_diag(
        ":&=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":&=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":&=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_bitwise_or_eq() {
    assert_lex_with_diag(
        ":|=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":|=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":|=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_bitwise_xor_eq() {
    assert_lex_with_diag(
        ":^=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":^=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":^=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_logical_and_eq() {
    assert_lex_with_diag(
        ":&&=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":&&=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":&&=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_logical_or_eq() {
    assert_lex_with_diag(
        ":||=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":||=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":||=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_logical_and_op() {
    assert_lex_with_diag(
        ":&&",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":&&", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":&&", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_logical_or_op() {
    assert_lex_with_diag(
        ":||",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":||", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":||", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_semicolon() {
    assert_lex_with_diag(
        ":;",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::Semicolon, pos_in(src, b";", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_comma() {
    assert_lex_with_diag(
        ":,",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::Comma, pos_in(src, b",", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_question() {
    assert_lex_with_diag(
        ":?",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::Question, pos_in(src, b"?", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_eq() {
    assert_lex_with_diag(
        ":=",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":=", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":=", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_fat_arrow() {
    assert_lex_with_diag(
        ":=>",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":=>", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":=>", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_dot() {
    assert_lex_with_diag(
        ":.",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::Dot, pos_in(src, b".", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_amp_dot() {
    assert_lex_with_diag(
        ":&.",
        LexerStates::Begin,
        |src| vec![token(TokenKind::Symbol, pos_in(src, b":&.", 0), 0)],
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":&.", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_lparen() {
    assert_lex_with_diag(
        ":(",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::LParen, pos_in(src, b"(", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_rparen() {
    assert_lex_with_diag(
        ":)",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::RParen, pos_in(src, b")", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_lbracket() {
    assert_lex_with_diag(
        ":[",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::LBracket, pos_in(src, b"[", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_rbracket() {
    assert_lex_with_diag(
        ":]",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_lbrace() {
    assert_lex_with_diag(
        ":{",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::LBrace, pos_in(src, b"{", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_invalid_rbrace() {
    assert_lex_with_diag(
        ":}",
        LexerStates::Begin,
        |src| {
            vec![
                token(TokenKind::Symbol, pos_in(src, b":", 0), 0),
                token(TokenKind::RBrace, pos_in(src, b"}", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b":", 0),
                message: "Invalid symbol".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_static_symbol_ivar() {
    assert_lex(":@foo", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":@foo", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_cvar() {
    assert_lex(":@@foo", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":@@foo", 0), 0)]
    });
}

#[test]
fn test_lex_static_symbol_gvar() {
    assert_lex(":$foo", LexerStates::Begin, |src| {
        vec![token(TokenKind::Symbol, pos_in(src, b":$foo", 0), 0)]
    });
}
