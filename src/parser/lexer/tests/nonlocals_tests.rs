use crate::{
    ast::pos_in,
    encoding::EStrRef,
    parser::{
        lexer::{NonLocalKind, TokenKind},
        BinOpKind,
    },
    Diagnostic, Encoding,
};

use super::{assert_lex, assert_lex_with_diag, token, LexerStates};

#[test]
fn test_lex_ivar_name_simple() {
    assert_lex("@foo_123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Ivar),
            pos_in(src, b"@foo_123", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ivar_name_underscore() {
    assert_lex("@_", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Ivar),
            pos_in(src, b"@_", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ivar_name_start_with_underscore() {
    assert_lex("@_foo123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Ivar),
            pos_in(src, b"@_foo123", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ivar_name_cap() {
    assert_lex("@Baz", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Ivar),
            pos_in(src, b"@Baz", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ivar_name_non_ascii() {
    assert_lex("@あ", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Ivar),
            pos_in(src, b"@\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ivar_name_invalid_non_ascii() {
    assert_lex_with_diag(
        EStrRef::from_bytes(b"@\xE3\x81", Encoding::UTF_8),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Ivar),
                pos_in(src, b"@\xE3\x81", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"@\xE3\x81", 0),
                message: "The instance variable name contains invalid characters".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_ivar_name_ambiguous_non_ascii() {
    // 835C = ソ (which bytewise includes a backslash)
    assert_lex(
        EStrRef::from_bytes(b"@\x83\x5C", Encoding::Windows_31J),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Ivar),
                pos_in(src, b"@\x83\x5C", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_ivar_name_invalid_digit() {
    assert_lex_with_diag(
        EStrRef::from("@123"),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Ivar),
                pos_in(src, b"@123", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"@123", 0),
                message: "Invalid instance variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_cvar_name_simple() {
    assert_lex("@@foo_123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Cvar),
            pos_in(src, b"@@foo_123", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_cvar_name_underscore() {
    assert_lex("@@_", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Cvar),
            pos_in(src, b"@@_", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_cvar_name_start_with_underscore() {
    assert_lex("@@_foo123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Cvar),
            pos_in(src, b"@@_foo123", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_cvar_name_cap() {
    assert_lex("@@Baz", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Cvar),
            pos_in(src, b"@@Baz", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_cvar_name_non_ascii() {
    assert_lex("@@あ", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Cvar),
            pos_in(src, b"@@\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_cvar_name_invalid_non_ascii() {
    assert_lex_with_diag(
        EStrRef::from_bytes(b"@@\xE3\x81", Encoding::UTF_8),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Cvar),
                pos_in(src, b"@@\xE3\x81", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"@@\xE3\x81", 0),
                message: "The class variable name contains invalid characters".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_cvar_name_ambiguous_non_ascii() {
    // 835C = ソ (which bytewise includes a backslash)
    assert_lex(
        EStrRef::from_bytes(b"@@\x83\x5C", Encoding::Windows_31J),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Cvar),
                pos_in(src, b"@@\x83\x5C", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_cvar_name_invalid_digit() {
    assert_lex_with_diag(
        EStrRef::from("@@123"),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Cvar),
                pos_in(src, b"@@123", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"@@123", 0),
                message: "Invalid class variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_cvar_name_invalid_empty() {
    assert_lex_with_diag(
        EStrRef::from("@@"),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Cvar),
                pos_in(src, b"@@", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"@@", 0),
                message: "Invalid class variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_name_simple() {
    assert_lex("$foo_123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$foo_123", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_name_start_with_underscore() {
    assert_lex("$_", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$_", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_name_cap() {
    assert_lex("$Baz", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$Baz", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_non_ascii() {
    assert_lex("$あ", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_invalid_non_ascii() {
    assert_lex_with_diag(
        EStrRef::from_bytes(b"$\xE3\x81", Encoding::UTF_8),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$\xE3\x81", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$\xE3\x81", 0),
                message: "The global variable name contains invalid characters".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_ambiguous_non_ascii() {
    // 835C = ソ (which bytewise includes a backslash)
    assert_lex(
        EStrRef::from_bytes(b"$\x83\x5C", Encoding::Windows_31J),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$\x83\x5C", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_gvar_underscore() {
    assert_lex("$_", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$_", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_bang() {
    assert_lex("$!", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$!", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_dquote() {
    assert_lex("$\"", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$\"", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_dollar() {
    assert_lex("$$", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$$", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_amp() {
    assert_lex("$&", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$&", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_quote() {
    assert_lex("$'", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$'", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_star() {
    assert_lex("$*", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$*", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_plus() {
    assert_lex("$+", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$+", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_comma() {
    assert_lex("$,", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$,", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_dot() {
    assert_lex("$.", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$.", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_slash() {
    assert_lex("$/", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$/", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_colon() {
    assert_lex("$:", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$:", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_semicolon() {
    assert_lex("$;", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$;", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_lt() {
    assert_lex("$<", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$<", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_eq() {
    assert_lex("$=", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$=", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_gt() {
    assert_lex("$>", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$>", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_question() {
    assert_lex("$?", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$?", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_at() {
    assert_lex("$@", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$@", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_backslash() {
    assert_lex("$\\", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$\\", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_backtick() {
    assert_lex("$`", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$`", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_tilde() {
    assert_lex("$~", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$~", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_invalid_number_sign() {
    assert_lex_with_diag(
        "$#",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_percent() {
    assert_lex_with_diag(
        "$%",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(TokenKind::BinOp(BinOpKind::Mod), pos_in(src, b"%", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_lparen() {
    assert_lex_with_diag(
        "$(",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(TokenKind::LParen, pos_in(src, b"(", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_rparen() {
    assert_lex_with_diag(
        "$)",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(TokenKind::RParen, pos_in(src, b")", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_lbracket() {
    assert_lex_with_diag(
        "$[",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(TokenKind::LBracket, pos_in(src, b"[", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_rbracket() {
    assert_lex_with_diag(
        "$]",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(TokenKind::RBracket, pos_in(src, b"]", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_caret() {
    assert_lex_with_diag(
        "$^",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(
                    TokenKind::BinOp(BinOpKind::BitwiseXor),
                    pos_in(src, b"^", 0),
                    0,
                ),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_lbrace() {
    assert_lex_with_diag(
        "${",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(TokenKind::LBrace, pos_in(src, b"{", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_rbrace() {
    assert_lex_with_diag(
        "$}",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(TokenKind::RBrace, pos_in(src, b"}", 0), 0),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_vert() {
    assert_lex_with_diag(
        "$|",
        LexerStates::ALL,
        |src| {
            vec![
                token(
                    TokenKind::NonLocal(NonLocalKind::Gvar),
                    pos_in(src, b"$", 0),
                    0,
                ),
                token(
                    TokenKind::BinOp(BinOpKind::BitwiseOr),
                    pos_in(src, b"|", 0),
                    0,
                ),
            ]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_stray_eof() {
    assert_lex_with_diag(
        "$",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_stray_space() {
    assert_lex_with_diag(
        "$ ",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$", 0),
                message: "Invalid global variable name".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_zero() {
    assert_lex("$0", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$0", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_invalid_zeronum() {
    assert_lex_with_diag(
        "$06",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$06", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$06", 0),
                message: "Global variable name cannot start with a digit".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_invalid_zeroalpha() {
    assert_lex_with_diag(
        "$0foo",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$0foo", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$0foo", 0),
                message: "Global variable name cannot start with a digit".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_num_simple() {
    assert_lex("$5", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$5", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_num_large() {
    assert_lex("$123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$123", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_num_invalid_nonkeyword_numalpha() {
    assert_lex_with_diag(
        "$123foo",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$123foo", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$123foo", 0),
                message: "Global variable name cannot start with a digit".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_num_keyword_numalpha() {
    assert_lex("$123or", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$123", 0),
                0,
            ),
            token(TokenKind::KeywordOr, pos_in(src, b"or", 0), 0),
        ]
    });
}

#[test]
fn test_lex_gvar_dashed_simple() {
    assert_lex("$-I", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$-I", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_dashed_digit() {
    assert_lex("$-9", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$-9", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_dashed_invalid_missing_ident() {
    assert_lex_with_diag(
        "$-",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$-", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"$-", 0),
                message: "A letter must follow `$-`".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_dashed_invalid_long_ident() {
    assert_lex_with_diag(
        "$-foo",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$-foo", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"foo", 0),
                message: "Only a single letter can follow `$-`".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_dashed_splittable_long_ident() {
    assert_lex("$-Ior", LexerStates::ALL, |src| {
        vec![
            token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$-I", 0),
                0,
            ),
            token(TokenKind::KeywordOr, pos_in(src, b"or", 0), 0),
        ]
    });
}

#[test]
fn test_lex_gvar_dashed_non_ascii() {
    assert_lex("$-あ", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$-\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_dashed_invalid_non_ascii() {
    assert_lex_with_diag(
        EStrRef::from_bytes(b"$-\xE3\x81", Encoding::UTF_8),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$-\xE3\x81", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"\xE3\x81", 0),
                message: "The global variable name contains invalid characters".to_owned(),
            }]
        },
    );
}

#[test]
fn test_lex_gvar_dashed_ambiguous_non_ascii() {
    // 835C = ソ (which bytewise includes a backslash)
    assert_lex(
        EStrRef::from_bytes(b"$-\x83\x5C", Encoding::Windows_31J),
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$-\x83\x5C", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_lex_gvar_dashed_invalid_long_non_ascii() {
    assert_lex_with_diag(
        "$-あい",
        LexerStates::ALL,
        |src| {
            vec![token(
                TokenKind::NonLocal(NonLocalKind::Gvar),
                pos_in(src, b"$-\xE3\x81\x82\xE3\x81\x84", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"\xE3\x81\x82\xE3\x81\x84", 0),
                message: "Only a single letter can follow `$-`".to_owned(),
            }]
        },
    );
}
