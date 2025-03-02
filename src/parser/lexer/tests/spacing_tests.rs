use num_bigint::BigInt;

use crate::{
    ast::{pos_in, NumericValue},
    parser::lexer::{NumericToken, TokenKind},
};

use super::{assert_lex_except, assert_lex_for, token, LexerStates};

#[test]
fn test_lex_eof_nul() {
    assert_lex_for("\0", LexerStates::ALL, |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"\0", 0), 0)]
    });
}

#[test]
fn test_lex_eof_eot() {
    assert_lex_for("\x04", LexerStates::ALL, |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"\x04", 0), 0)]
    });
}

#[test]
fn test_lex_eof_sub() {
    assert_lex_for("\x1A", LexerStates::ALL, |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"\x1A", 0), 0)]
    });
}

#[test]
fn test_lex_eof_end_token_first_line() {
    assert_lex_for("__END__", LexerStates::ALL, |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_eof_end_token_second_line() {
    assert_lex_for("foo\n__END__", LexerStates::ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
            token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
            token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0),
        ]
    });
}

#[test]
fn test_lex_eof_end_token_eof() {
    assert_lex_for("__END__", LexerStates::ALL, |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_eof_end_token_lf() {
    assert_lex_for("__END__\n", LexerStates::ALL, |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_eof_end_token_crlf() {
    assert_lex_for("__END__\r\n", LexerStates::ALL, |src| {
        vec![token(TokenKind::EOF, pos_in(src, b"__END__", 0), 0)]
    });
}

#[test]
fn test_lex_non_eof_space_after_end_token() {
    assert_lex_for("__END__ \n", LexerStates::ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 0),
            token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
        ]
    });
}

#[test]
fn test_lex_non_eof_space_before_end_token() {
    assert_lex_for(" __END__\n", LexerStates::ALL, |src| {
        vec![
            token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 1),
            token(TokenKind::Newline, pos_in(src, b"\n", 0), 1),
        ]
    });
}

#[test]
fn test_lex_non_eof_cr_after_end_token() {
    assert_lex_for("__END__\r", LexerStates::ALL, |src| {
        vec![token(TokenKind::Identifier, pos_in(src, b"__END__", 0), 0)]
    });
}

fn one() -> NumericToken {
    NumericToken {
        value: NumericValue::Integer(BigInt::from(1)),
        imaginary: false,
    }
}

#[test]
fn test_lex_spaces_space() {
    assert_lex_for(" 1", LexerStates::ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 1)]
    });
}

#[test]
fn test_lex_spaces_tab() {
    assert_lex_for("\t1", LexerStates::ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 8)]
    });
}

#[test]
fn test_lex_spaces_tab_and_space() {
    assert_lex_for("\t 1", LexerStates::ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 9)]
    });
}

#[test]
fn test_lex_spaces_space_and_tab() {
    assert_lex_for(" \t1", LexerStates::ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 8)]
    });
}

#[test]
fn test_lex_spaces_vtab() {
    assert_lex_for("\x0B1", LexerStates::ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 0)]
    });
}

#[test]
fn test_lex_spaces_ff() {
    assert_lex_for("\x0C1", LexerStates::ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 0)]
    });
}

#[test]
fn test_lex_spaces_cr() {
    assert_lex_for("\r1", LexerStates::ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 0)]
    });
}

#[test]
fn test_lex_spaces_multiple() {
    assert_lex_for("  1", LexerStates::ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 2)]
    });
}

#[test]
fn test_lex_spaces_lf() {
    assert_lex_except("\n1", LexerStates::BeginOpt | LexerStates::END_ALL, |src| {
        vec![token(TokenKind::Numeric(one()), pos_in(src, b"1", 0), 0)]
    });
}

#[test]
fn test_lex_semicolon_lf() {
    assert_lex_for("\n", LexerStates::BeginOpt | LexerStates::END_ALL, |src| {
        vec![token(TokenKind::Newline, pos_in(src, b"\n", 0), 0)]
    });
}

#[test]
fn test_lex_semicolon_crlf() {
    assert_lex_for(
        "\r\n",
        LexerStates::BeginOpt | LexerStates::END_ALL,
        |src| vec![token(TokenKind::Newline, pos_in(src, b"\r\n", 0), 0)],
    );
}

#[test]
fn test_lex_skip_lf_before_dot() {
    assert_lex_for(
        "\n  .bar",
        LexerStates::BeginOpt | LexerStates::END_ALL | LexerStates::METH_ALL,
        |src| {
            vec![
                token(TokenKind::Dot, pos_in(src, b".", 0), 2),
                token(TokenKind::Identifier, pos_in(src, b"bar", 0), 2),
            ]
        },
    );
}

#[test]
fn test_lex_lf_before_dot_dot() {
    assert_lex_for(
        "\n  ..bar",
        LexerStates::BeginOpt | LexerStates::END_ALL,
        |src| {
            vec![
                token(TokenKind::Newline, pos_in(src, b"\n", 0), 0),
                token(TokenKind::DotDot, pos_in(src, b"..", 0), 2),
                token(TokenKind::Identifier, pos_in(src, b"bar", 0), 2),
            ]
        },
    );
}

#[test]
fn test_lex_comments() {
    assert_lex_except(
        "# comment2\nfoo bar # comment1\n# comment3\nbaz\n",
        LexerStates::BeginOpt | LexerStates::END_ALL | LexerStates::METH_ALL,
        |src| {
            vec![
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"bar", 0), 0),
                token(TokenKind::Newline, pos_in(src, b"\n", 1), 0),
                token(TokenKind::Identifier, pos_in(src, b"baz", 0), 0),
                token(TokenKind::Newline, pos_in(src, b"\n", 3), 0),
            ]
        },
    );
}
