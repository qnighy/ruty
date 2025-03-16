use crate::{ast::pos_in, encoding::EStrRef, parser::lexer::TokenKind, Diagnostic, Encoding};

use super::{assert_lex, assert_lex_with_diag, token, LexerStates};

const LABELABLE: LexerStates = LexerStates::EMPTY
    .or(LexerStates::BeginLabelable)
    .or(LexerStates::FirstArgument)
    .or(LexerStates::WeakFirstArgument);

#[test]
fn test_quote_string_tokens_simple_nolabelable() {
    assert_lex("' foo '", !LABELABLE, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_simple_labelable() {
    assert_lex("' foo '", LABELABLE, |src| {
        vec![
            token(TokenKind::StringBeginLabelable, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_labelled() {
    assert_lex("' foo ':", LABELABLE, |src| {
        vec![
            token(TokenKind::StringBeginLabelable, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEndColon, pos_in(src, b"':", 0), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_escaped() {
    assert_lex("'\\''", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, "\\'", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 2), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_backslashes() {
    assert_lex("'\\\\'", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, "\\\\", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
        ]
    });
}

#[test]
fn test_double_quote_string_tokens_simple() {
    assert_lex("\" foo \"", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"\"", 1), 0),
        ]
    });
}

#[test]
fn test_double_quote_string_tokens_dynamic() {
    assert_lex("\" foo #{bar}", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringInterpolationBegin, pos_in(src, "#{", 0), 0),
            token(TokenKind::Identifier, pos_in(src, b"bar", 0), 0),
            token(TokenKind::RBrace, pos_in(src, b"}", 0), 0),
        ]
    });
}

#[test]
fn test_char_literal_simple() {
    assert_lex(
        "?a",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::CharLiteral, pos_in(src, b"?a", 0), 0)],
    );
}

#[test]
fn test_char_literal_non_ascii() {
    assert_lex(
        "?あ",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| {
            vec![token(
                TokenKind::CharLiteral,
                pos_in(src, b"?\xE3\x81\x82", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_char_literal_invalid_non_ascii() {
    assert_lex_with_diag(
        EStrRef::from_bytes(b"?\xE3\x81", Encoding::UTF_8),
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| {
            vec![token(
                TokenKind::CharLiteral,
                pos_in(src, b"?\xE3\x81", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"\xE3\x81", 0),
                message: "The character literal contains invalid characters".to_owned(),
            }]
        },
    );
}
