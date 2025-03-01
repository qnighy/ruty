use crate::{
    ast::pos_in,
    encoding::EStrRef,
    parser::lexer::{LexerState, TokenKind},
    Diagnostic, Encoding,
};

use super::{assert_lex, lex_all_with_diag_from, token};

#[test]
fn test_lex_ivar_name_simple() {
    assert_lex("@foo123", |src| {
        vec![token(TokenKind::IvarName, pos_in(src, b"@foo123", 0), 0)]
    });
}

#[test]
fn test_lex_ivar_name_cap() {
    assert_lex("@Baz", |src| {
        vec![token(TokenKind::IvarName, pos_in(src, b"@Baz", 0), 0)]
    });
}

#[test]
fn test_lex_ivar_name_non_ascii() {
    assert_lex("@あ", |src| {
        vec![token(
            TokenKind::IvarName,
            pos_in(src, b"@\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_ivar_name_invalid_non_ascii() {
    let src = EStrRef::from_bytes(b"@\xE3\x81", Encoding::UTF_8);
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"@\xE3\x81", 0),
            message: "The instance variable name contains invalid characters".to_owned(),
        }]
    );
}

#[test]
fn test_lex_ivar_name_invalid_digit() {
    let src = EStrRef::from("@123");
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"@123", 0),
            message: "Invalid instance variable name".to_owned(),
        }]
    );
}

#[test]
fn test_lex_cvar_name_simple() {
    assert_lex("@@foo123", |src| {
        vec![token(TokenKind::CvarName, pos_in(src, b"@@foo123", 0), 0)]
    });
}

#[test]
fn test_lex_cvar_name_cap() {
    assert_lex("@@Baz", |src| {
        vec![token(TokenKind::CvarName, pos_in(src, b"@@Baz", 0), 0)]
    });
}

#[test]
fn test_lex_cvar_name_non_ascii() {
    assert_lex("@@あ", |src| {
        vec![token(
            TokenKind::CvarName,
            pos_in(src, b"@@\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_cvar_name_invalid_non_ascii() {
    let src = EStrRef::from_bytes(b"@@\xE3\x81", Encoding::UTF_8);
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"@@\xE3\x81", 0),
            message: "The class variable name contains invalid characters".to_owned(),
        }]
    );
}

#[test]
fn test_lex_cvar_name_invalid_digit() {
    let src = EStrRef::from("@@123");
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"@@123", 0),
            message: "Invalid class variable name".to_owned(),
        }]
    );
}

#[test]
fn test_lex_gvar_name_simple() {
    assert_lex("$foo123", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$foo123", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_name_cap() {
    assert_lex("$Baz", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$Baz", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_non_ascii() {
    assert_lex("$あ", |src| {
        vec![token(
            TokenKind::GvarName,
            pos_in(src, b"$\xE3\x81\x82", 0),
            0,
        )]
    });
}

#[test]
fn test_lex_gvar_invalid_non_ascii() {
    let src = EStrRef::from_bytes(b"$\xE3\x81", Encoding::UTF_8);
    let (_, diag) = lex_all_with_diag_from(src, LexerState::Begin);
    assert_eq!(
        diag,
        vec![Diagnostic {
            range: pos_in(src, b"$\xE3\x81", 0),
            message: "The global variable name contains invalid characters".to_owned(),
        }]
    );
}

#[test]
fn test_lex_gvar_underscore() {
    assert_lex("$_", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$_", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_bang() {
    assert_lex("$!", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$!", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dquote() {
    assert_lex("$\"", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$\"", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dollar() {
    assert_lex("$$", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$$", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_amp() {
    assert_lex("$&", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$&", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_quote() {
    assert_lex("$'", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$'", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_star() {
    assert_lex("$*", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$*", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_plus() {
    assert_lex("$+", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$+", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_comma() {
    assert_lex("$,", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$,", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dot() {
    assert_lex("$.", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$.", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_slash() {
    assert_lex("$/", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$/", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_colon() {
    assert_lex("$:", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$:", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_semicolon() {
    assert_lex("$;", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$;", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_lt() {
    assert_lex("$<", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$<", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_eq() {
    assert_lex("$=", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$=", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_gt() {
    assert_lex("$>", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$>", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_question() {
    assert_lex("$?", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$?", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_at() {
    assert_lex("$@", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$@", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_backslash() {
    assert_lex("$\\", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$\\", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_backtick() {
    assert_lex("$`", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$`", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_tilde() {
    assert_lex("$~", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$~", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_zero() {
    assert_lex("$0", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$0", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_num_simple() {
    assert_lex("$5", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$5", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_num_large() {
    assert_lex("$123", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$123", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dashed_simple() {
    assert_lex("$-I", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$-I", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dashed_digit() {
    assert_lex("$-9", |src| {
        vec![token(TokenKind::GvarName, pos_in(src, b"$-9", 0), 0)]
    });
}

#[test]
fn test_lex_gvar_dashed_non_ascii() {
    assert_lex("$-あ", |src| {
        vec![token(
            TokenKind::GvarName,
            pos_in(src, b"$-\xE3\x81\x82", 0),
            0,
        )]
    });
}
