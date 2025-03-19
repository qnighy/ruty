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

#[test]
fn test_char_ambiguous_non_ascii() {
    // 835C = ソ (which bytewise includes a backslash)
    assert_lex(
        EStrRef::from_bytes(b"?\x83\x5C", Encoding::Windows_31J),
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| {
            vec![token(
                TokenKind::CharLiteral,
                pos_in(src, b"?\x83\x5C", 0),
                0,
            )]
        },
    );
}

// When the first letter is in ASCII
// Note: the behavior is currently different
//       between parse.y and Prism.
#[test]
fn test_char_literal_split_before_long_ascii_ident() {
    // This behavior is important to parse e.g. `f ?abc : d`
    // where `f` is not a known-defined local variable.
    assert_lex(
        "?abc",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| {
            vec![
                token(TokenKind::Question, pos_in(src, b"?", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"abc", 0), 0),
            ]
        },
    );
}

// When the first letter is in ASCII
// Note: the behavior is currently different
//       between parse.y and Prism.
#[test]
fn test_char_literal_split_before_long_non_ascii_ident() {
    // This behavior is important to parse e.g. `f ?abc : d`
    // where `f` is not a known-defined local variable.
    assert_lex(
        "?abcあ",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| {
            vec![
                token(TokenKind::Question, pos_in(src, b"?", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"abc\xE3\x81\x82", 0), 0),
            ]
        },
    );
}

// When the first letter is not in ASCII
#[test]
fn test_char_literal_split_after_non_ascii_letter() {
    assert_lex(
        "?あand",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| {
            vec![
                token(TokenKind::CharLiteral, pos_in(src, b"?\xE3\x81\x82", 0), 0),
                token(TokenKind::KeywordAnd, pos_in(src, b"and", 0), 0),
            ]
        },
    );
}

// When the first letter is not in ASCII.
// If the suffix to be separated is not a suffix/infix-like keyword,
// Concatenate it for better reporting.
#[test]
fn test_char_literal_invalid_long_non_ascii_ident() {
    assert_lex_with_diag(
        "?あabc",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| {
            vec![token(
                TokenKind::CharLiteral,
                pos_in(src, b"?\xE3\x81\x82abc", 0),
                0,
            )]
        },
        |src| {
            vec![Diagnostic {
                range: pos_in(src, b"?\xE3\x81\x82abc", 0),
                message: "Character literal too long".to_owned(),
            }]
        },
    );
}

#[test]
fn test_char_literal_symbol_simple() {
    assert_lex(
        "?/",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::CharLiteral, pos_in(src, b"?/", 0), 0)],
    );
}

#[test]
fn test_char_literal_symbol_force_split() {
    assert_lex(
        "?<=>",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| {
            vec![
                token(TokenKind::CharLiteral, pos_in(src, b"?<", 0), 0),
                token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0),
            ]
        },
    );
}

#[test]
fn test_char_literal_digit() {
    assert_lex(
        "?9",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::CharLiteral, pos_in(src, b"?9", 0), 0)],
    );
}

#[test]
fn test_char_literal_c0ctrl() {
    assert_lex(
        "?\x1F",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::CharLiteral, pos_in(src, b"?\x1F", 0), 0)],
    );
}

#[test]
fn test_no_char_literal_lf() {
    assert_lex(
        "?\n",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::Question, pos_in(src, b"?", 0), 0)],
    );
}

#[test]
fn test_no_char_literal_space() {
    assert_lex(
        "? ",
        LexerStates::BEGIN_ALL | LexerStates::METH_ALL | LexerStates::FirstArgument,
        |src| vec![token(TokenKind::Question, pos_in(src, b"?", 0), 0)],
    );
}
