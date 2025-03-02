use crate::{
    ast::pos_in,
    parser::lexer::{BinOpKind, LexerState, NonLocalKind, TokenKind, UnOpKind},
};

use super::{assert_lex, assert_lex_except, assert_lex_for, token};

#[test]
fn test_backtick_string_tokens() {
    assert_lex_except(
        "` foo `",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::StringBegin, pos_in(src, b"`", 0), 0),
                token(TokenKind::StringContent, pos_in(src, " foo ", 0), 0),
                token(TokenKind::StringEnd, pos_in(src, b"`", 1), 0),
            ]
        },
    );
}

#[test]
fn test_backtick_op_name() {
    assert_lex_for(
        "`",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::MethodName, pos_in(src, b"`", 0), 0)],
    );
}

#[test]
fn test_op_assign_pow() {
    assert_lex("**=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Pow),
            pos_in(src, b"**=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_mult() {
    assert_lex("*=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Mul),
            pos_in(src, b"*=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_add() {
    assert_lex("+=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Add),
            pos_in(src, b"+=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_sub() {
    assert_lex("-=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::Sub),
            pos_in(src, b"-=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_lshift() {
    assert_lex("<<=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::LShift),
            pos_in(src, b"<<=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_rshift() {
    assert_lex(">>=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::RShift),
            pos_in(src, b">>=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_bitwise_and() {
    assert_lex("&=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::BitwiseAnd),
            pos_in(src, b"&=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_bitwise_or() {
    assert_lex("|=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::BitwiseOr),
            pos_in(src, b"|=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_bitwise_xor() {
    assert_lex("^=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::BitwiseXor),
            pos_in(src, b"^=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_logical_and() {
    assert_lex("&&=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::LogicalAnd),
            pos_in(src, b"&&=", 0),
            0,
        )]
    });
}

#[test]
fn test_op_assign_logical_or() {
    assert_lex("||=", |src| {
        vec![token(
            TokenKind::OpAssign(BinOpKind::LogicalOr),
            pos_in(src, b"||=", 0),
            0,
        )]
    });
}

#[test]
fn test_excl() {
    assert_lex("!", |src| {
        vec![token(
            TokenKind::UnOp(UnOpKind::Not),
            pos_in(src, b"!", 0),
            0,
        )]
    });
}

#[test]
fn test_excl_at_join() {
    assert_lex_for(
        "!@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::MethodName, pos_in(src, b"!@", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_excl_at_separate() {
    assert_lex_except(
        "!@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::UnOp(UnOpKind::Not), pos_in(src, b"!", 0), 0),
                token(
                    TokenKind::NonLocal(NonLocalKind::Ivar),
                    pos_in(src, b"@foo", 0),
                    0,
                ),
            ]
        },
    );
}

#[test]
fn test_excl_eq() {
    assert_lex("!=", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::NotEq),
            pos_in(src, b"!=", 0),
            0,
        )]
    });
}

#[test]
fn test_excl_tilde() {
    assert_lex("!~", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::NotMatch),
            pos_in(src, b"!~", 0),
            0,
        )]
    });
}

#[test]
fn test_comma() {
    assert_lex(",", |src| {
        vec![token(TokenKind::Comma, pos_in(src, b",", 0), 0)]
    });
}

#[test]
fn test_dot() {
    assert_lex(".", |src| {
        vec![token(TokenKind::Dot, pos_in(src, b".", 0), 0)]
    });
}

#[test]
fn test_dot_dot() {
    assert_lex("..", |src| {
        vec![token(TokenKind::DotDot, pos_in(src, b"..", 0), 0)]
    });
}

#[test]
fn test_dot_dot_dot() {
    assert_lex("...", |src| {
        vec![token(TokenKind::DotDotDot, pos_in(src, b"...", 0), 0)]
    });
}

#[test]
fn test_colon() {
    assert_lex(":", |src| {
        vec![token(TokenKind::Colon, pos_in(src, b":", 0), 0)]
    });
}

#[test]
fn test_colon_colon_infix_spaced() {
    assert_lex_for(
        " :: ",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 1)],
    );
}

#[test]
fn test_colon_colon_prefix_spaced() {
    assert_lex_except(
        " :: ",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 1)],
    );
}

#[test]
fn test_colon_colon_infix_left_spaced() {
    assert_lex_for(
        " ::",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 1)],
    );
}

#[test]
fn test_colon_colon_prefix_left_spaced() {
    assert_lex_except(
        " ::",
        &[
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 1)],
    );
}

#[test]
fn test_colon_colon_infix_nospaced() {
    assert_lex_for(
        "::",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColon, pos_in(src, b"::", 0), 0)],
    );
}

#[test]
fn test_colon_colon_prefix_nospaced() {
    assert_lex_except(
        "::",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| vec![token(TokenKind::ColonColonPrefix, pos_in(src, b"::", 0), 0)],
    );
}

#[test]
fn test_lex_semicolon() {
    assert_lex(";", |src| {
        vec![token(TokenKind::Semicolon, pos_in(src, b";", 0), 0)]
    });
}

#[test]
fn test_lt() {
    assert_lex("<", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Lt),
            pos_in(src, b"<", 0),
            0,
        )]
    });
}

#[test]
fn test_lt_lt() {
    assert_lex("<<", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::LShift),
            pos_in(src, b"<<", 0),
            0,
        )]
    });
}

#[test]
fn test_lt_eq() {
    assert_lex("<=", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Le),
            pos_in(src, b"<=", 0),
            0,
        )]
    });
}

#[test]
fn test_lt_eq_gt() {
    assert_lex("<=>", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Cmp),
            pos_in(src, b"<=>", 0),
            0,
        )]
    });
}

#[test]
fn test_eq() {
    assert_lex("=", |src| {
        vec![token(TokenKind::Eq, pos_in(src, b"=", 0), 0)]
    });
}

#[test]
fn test_eq_eq() {
    assert_lex("==", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Eq),
            pos_in(src, b"==", 0),
            0,
        )]
    });
}

#[test]
fn test_eq_eq_eq() {
    assert_lex("===", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Incl),
            pos_in(src, b"===", 0),
            0,
        )]
    });
}

#[test]
fn test_fat_arrow() {
    assert_lex("=>", |src| {
        vec![token(TokenKind::FatArrow, pos_in(src, b"=>", 0), 0)]
    });
}

#[test]
fn test_eq_match() {
    assert_lex("=~", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Match),
            pos_in(src, b"=~", 0),
            0,
        )]
    });
}

#[test]
fn test_gt() {
    assert_lex(">", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Gt),
            pos_in(src, b">", 0),
            0,
        )]
    });
}

#[test]
fn test_gt_eq() {
    assert_lex(">=", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::Ge),
            pos_in(src, b">=", 0),
            0,
        )]
    });
}

#[test]
fn test_gt_gt() {
    assert_lex(">>", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::RShift),
            pos_in(src, b">>", 0),
            0,
        )]
    });
}

#[test]
fn test_question_simple() {
    assert_lex("?", |src| {
        vec![token(TokenKind::Question, pos_in(src, b"?", 0), 0)]
    });
}

#[test]
fn test_question_separate() {
    assert_lex_for(
        "?a",
        &[LexerState::WeakFirstArgument, LexerState::End],
        |src| {
            vec![
                token(TokenKind::Question, pos_in(src, b"?", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"a", 0), 0),
            ]
        },
    );
}

#[test]
fn test_char_literal() {
    assert_lex_except(
        "?a",
        &[LexerState::WeakFirstArgument, LexerState::End],
        |src| vec![token(TokenKind::CharLiteral, pos_in(src, b"?a", 0), 0)],
    );
}

#[test]
fn test_at() {
    assert_lex("@", |src| {
        vec![token(TokenKind::At, pos_in(src, b"@", 0), 0)]
    });
}

#[test]
fn test_caret() {
    assert_lex("^", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::BitwiseXor),
            pos_in(src, b"^", 0),
            0,
        )]
    });
}

#[test]
fn test_vert() {
    assert_lex("|", |src| {
        vec![token(
            TokenKind::BinOp(BinOpKind::BitwiseOr),
            pos_in(src, b"|", 0),
            0,
        )]
    });
}

#[test]
fn test_vert_vert() {
    assert_lex_for(
        "||",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![token(
                TokenKind::BinOp(BinOpKind::LogicalOr),
                pos_in(src, b"||", 0),
                0,
            )]
        },
    );
}

#[test]
fn test_split_vert_vert() {
    assert_lex_except(
        "||",
        &[
            LexerState::FirstArgument,
            LexerState::WeakFirstArgument,
            LexerState::End,
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(
                    TokenKind::BinOp(BinOpKind::BitwiseOr),
                    pos_in(src, b"|", 0),
                    0,
                ),
                token(
                    TokenKind::BinOp(BinOpKind::BitwiseOr),
                    pos_in(src, b"|", 1),
                    0,
                ),
            ]
        },
    );
}

#[test]
fn test_tilde() {
    assert_lex("~", |src| {
        vec![token(
            TokenKind::UnOp(UnOpKind::BitwiseNot),
            pos_in(src, b"~", 0),
            0,
        )]
    });
}

#[test]
fn test_tilde_at_join() {
    assert_lex_for(
        "~@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(TokenKind::MethodName, pos_in(src, b"~@", 0), 0),
                token(TokenKind::Identifier, pos_in(src, b"foo", 0), 0),
            ]
        },
    );
}

#[test]
fn test_tilde_at_separate() {
    assert_lex_except(
        "~@foo",
        &[
            LexerState::MethForDef,
            LexerState::MethOrSymbolForDef,
            LexerState::MethForCall,
        ],
        |src| {
            vec![
                token(
                    TokenKind::UnOp(UnOpKind::BitwiseNot),
                    pos_in(src, b"~", 0),
                    0,
                ),
                token(
                    TokenKind::NonLocal(NonLocalKind::Ivar),
                    pos_in(src, b"@foo", 0),
                    0,
                ),
            ]
        },
    );
}
