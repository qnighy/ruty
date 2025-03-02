use crate::{ast::pos_in, parser::lexer::TokenKind};

use super::{assert_lex_for, token, LexerStates};

const LABELABLE: LexerStates = LexerStates::EMPTY
    .or(LexerStates::BeginLabelable)
    .or(LexerStates::FirstArgument)
    .or(LexerStates::WeakFirstArgument);

#[test]
fn test_quote_string_tokens_simple_nolabelable() {
    assert_lex_for("' foo '", !LABELABLE, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_simple_labelable() {
    assert_lex_for("' foo '", LABELABLE, |src| {
        vec![
            token(TokenKind::StringBeginLabelable, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_labelled() {
    assert_lex_for("' foo ':", LABELABLE, |src| {
        vec![
            token(TokenKind::StringBeginLabelable, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEndColon, pos_in(src, b"':", 0), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_escaped() {
    assert_lex_for("'\\''", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, "\\'", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 2), 0),
        ]
    });
}

#[test]
fn test_quote_string_tokens_backslashes() {
    assert_lex_for("'\\\\'", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"'", 0), 0),
            token(TokenKind::StringContent, pos_in(src, "\\\\", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"'", 1), 0),
        ]
    });
}

#[test]
fn test_double_quote_string_tokens_simple() {
    assert_lex_for("\" foo \"", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringEnd, pos_in(src, b"\"", 1), 0),
        ]
    });
}

#[test]
fn test_double_quote_string_tokens_dynamic() {
    assert_lex_for("\" foo #{bar}", LexerStates::Begin, |src| {
        vec![
            token(TokenKind::StringBegin, pos_in(src, b"\"", 0), 0),
            token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
            token(TokenKind::StringInterpolationBegin, pos_in(src, "#{", 0), 0),
            token(TokenKind::Identifier, pos_in(src, b"bar", 0), 0),
            token(TokenKind::RBrace, pos_in(src, b"}", 0), 0),
        ]
    });
}
