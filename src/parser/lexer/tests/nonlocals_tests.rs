use crate::{
    ast::pos_in,
    encoding::EStrRef,
    parser::lexer::{NonLocalKind, TokenKind},
    Diagnostic, Encoding,
};

use super::{assert_lex, assert_lex_with_diag, token, LexerStates};

#[test]
fn test_lex_ivar_name_simple() {
    assert_lex("@foo123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Ivar),
            pos_in(src, b"@foo123", 0),
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
    assert_lex("@@foo123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Cvar),
            pos_in(src, b"@@foo123", 0),
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
fn test_lex_gvar_name_simple() {
    assert_lex("$foo123", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$foo123", 0),
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
fn test_lex_gvar_dashed_non_ascii() {
    assert_lex("$-あ", LexerStates::ALL, |src| {
        vec![token(
            TokenKind::NonLocal(NonLocalKind::Gvar),
            pos_in(src, b"$-\xE3\x81\x82", 0),
            0,
        )]
    });
}
