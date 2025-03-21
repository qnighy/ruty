use std::fmt;

use num_bigint::BigInt;
#[allow(unused)]
use pretty_assertions::{assert_eq, assert_ne};

use crate::{
    ast::{
        pos_in, Arg, ArgList, CallExpr, CallStyle, ConstReceiver, Expr, ExprArg, FalseExpr,
        IntegerType, InterpolationContent, LocalVariableExpr, LocalVariableWriteTarget, NilExpr,
        NilStyle, NumericExpr, NumericValue, ParenParen, Program, RegexpExpr, SelfExpr, SeqExpr,
        Stmt, StmtSep, StmtSepKind, StringContent, StringExpr, TextContent, TrueExpr, Type,
        TypeAnnotation, WriteExpr, WriteTarget, XStringExpr, DUMMY_RANGE,
    },
    encoding::EStrRef,
    CharPlus, Diagnostic, EString,
};

use super::{parse, parse_expr, parse_type, symbol};

fn p_program(input: EStrRef<'_>, locals: &[&str]) -> (Program, Vec<Diagnostic>) {
    let locals = locals.iter().map(|&s| symbol(s)).collect::<Vec<_>>();
    let mut diag = Vec::new();
    let program = parse(&mut diag, input, &locals);
    (program, diag)
}

fn pp_program(input: EStrRef<'_>, locals: &[&str]) -> (String, Vec<String>) {
    let locals = locals.iter().map(|&s| symbol(s)).collect::<Vec<_>>();
    let mut diag = Vec::new();
    let program = parse(&mut diag, input, &locals);
    let program = {
        struct Disp(Program);
        impl fmt::Display for Disp {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt_expr(f, &self.0.body, 10)
            }
        }
        format!("{}", Disp(program))
    };
    let diag = diag.into_iter().map(|d| d.message).collect();
    (program, diag)
}

fn p_expr(input: EStrRef<'_>, locals: &[&str]) -> (Expr, Vec<Diagnostic>) {
    let locals = locals.iter().map(|&s| symbol(s)).collect::<Vec<_>>();
    let mut diag = Vec::new();
    let expr = parse_expr(&mut diag, input, &locals);
    (expr, diag)
}

fn p_type(input: EStrRef<'_>) -> (Type, Vec<Diagnostic>) {
    let mut diag = Vec::new();
    let ty = parse_type(&mut diag, input);
    (ty, diag)
}

fn fmt_expr(f: &mut fmt::Formatter<'_>, expr: &Expr, outer_level: i32) -> fmt::Result {
    let level = match expr {
        Expr::Seq(_) => 0,
        Expr::Nil(_) => 0,
        Expr::False(_) => 0,
        Expr::True(_) => 0,
        Expr::Numeric(_) => 0,
        Expr::String(_) => 0,
        Expr::Regexp(_) => 0,
        Expr::XString(_) => 0,
        Expr::LocalVariable(_) => 0,
        Expr::Const(expr) => match expr.receiver {
            ConstReceiver::None => 0,
            ConstReceiver::Expr(_) => 1,
            ConstReceiver::Object => 0,
        },
        Expr::Self_(_) => 0,
        Expr::SourceEncoding(_) => 0,
        Expr::SourceFile(_) => 0,
        Expr::SourceLine(_) => 0,
        Expr::Call(_) => 1,
        Expr::Write(_) => 2,
        Expr::And(_) => 2,
        Expr::Or(_) => 2,
        Expr::If(_) => 0,
        Expr::While(_) => 0,
        Expr::Until(_) => 0,
        Expr::Error(_) => 0,
    };
    if outer_level < level {
        write!(f, "(")?;
    }
    match expr {
        Expr::Seq(expr) => {
            write!(f, "(")?;
            for stmt in &expr.statements {
                fmt_expr(f, &stmt.expr, 10)?;
                write!(f, "; ")?;
            }
            write!(f, ")")?;
        }
        Expr::Nil(_) => {
            write!(f, "nil")?;
        }
        Expr::False(_) => {
            write!(f, "false")?;
        }
        Expr::True(_) => {
            write!(f, "true")?;
        }
        Expr::Numeric(expr) => {
            match expr.value {
                NumericValue::Integer(ref value) => {
                    write!(f, "{}", value)?;
                }
                NumericValue::Float(value) => {
                    write!(f, "{:?}", f64::from(value))?;
                }
                NumericValue::Rational(ref value) => {
                    write!(f, "{}r", value)?;
                }
            }
            if expr.imaginary {
                write!(f, "i")?;
            }
        }
        Expr::String(expr) => {
            write!(f, "\"")?;
            fmt_string_contents(f, &expr.contents, '"')?;
            write!(f, "\"")?;
        }
        Expr::Regexp(expr) => {
            write!(f, "/")?;
            fmt_string_contents(f, &expr.contents, '/')?;
            write!(f, "/")?;
        }
        Expr::XString(expr) => {
            write!(f, "`")?;
            fmt_string_contents(f, &expr.contents, '`')?;
            write!(f, "`")?;
        }
        Expr::LocalVariable(expr) => {
            fmt_name(f, &expr.name)?;
        }
        Expr::Const(expr) => {
            match &expr.receiver {
                ConstReceiver::None => {}
                ConstReceiver::Expr(receiver) => {
                    fmt_expr(f, receiver, level)?;
                    write!(f, "::")?;
                }
                ConstReceiver::Object => {
                    write!(f, "::")?;
                }
            }
            fmt_name(f, &expr.name)?;
        }
        Expr::Self_(_) => {
            write!(f, "self")?;
        }
        Expr::SourceEncoding(_) => {
            write!(f, "__ENCODING__")?;
        }
        Expr::SourceFile(_) => {
            write!(f, "__FILE__")?;
        }
        Expr::SourceLine(_) => {
            write!(f, "__LINE__")?;
        }
        Expr::Call(expr) => {
            if expr.private {
                write!(f, "self")?;
            } else if matches!(&*expr.receiver, Expr::Self_(_)) {
                write!(f, "(self)")?;
            } else {
                fmt_expr(f, &expr.receiver, level)?;
            }
            if expr.optional {
                write!(f, "&.")?;
            } else {
                write!(f, ".")?;
            }
            fmt_name(f, &expr.method)?;
            write!(f, "(")?;
            for (i, Arg::Expr(arg)) in expr.args.args.iter().enumerate() {
                let arg = &arg.expr;
                fmt_expr(f, arg, 10)?;
                if i + 1 < expr.args.args.len() {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")?;
        }
        Expr::Write(expr) => {
            fmt_write_target(f, &expr.lhs)?;
            write!(f, " = ")?;
            fmt_expr(f, &expr.rhs, level - 1)?;
        }
        Expr::And(expr) => {
            fmt_expr(f, &expr.lhs, level - 1)?;
            write!(f, " && ")?;
            fmt_expr(f, &expr.rhs, level - 1)?;
        }
        Expr::Or(expr) => {
            fmt_expr(f, &expr.lhs, level - 1)?;
            write!(f, " || ")?;
            fmt_expr(f, &expr.rhs, level - 1)?;
        }
        Expr::If(expr) => {
            write!(f, "if ")?;
            fmt_expr(f, &expr.cond, 10)?;
            write!(f, " then ")?;
            fmt_expr(f, &expr.then, 10)?;
            write!(f, " else ")?;
            fmt_expr(f, &expr.else_, 10)?;
            write!(f, " end")?;
        }
        Expr::While(expr) => {
            write!(f, "while ")?;
            fmt_expr(f, &expr.cond, 10)?;
            write!(f, " do ")?;
            fmt_expr(f, &expr.body, 10)?;
            write!(f, " end")?;
        }
        Expr::Until(expr) => {
            write!(f, "until ")?;
            fmt_expr(f, &expr.cond, 10)?;
            write!(f, " do ")?;
            fmt_expr(f, &expr.body, 10)?;
            write!(f, " end")?;
        }
        Expr::Error(_) => {
            write!(f, "<error>")?;
        }
    }
    if outer_level < level {
        write!(f, ")")?;
    }
    Ok(())
}

fn fmt_write_target(f: &mut fmt::Formatter<'_>, expr: &WriteTarget) -> fmt::Result {
    match expr {
        WriteTarget::LocalVariable(expr) => fmt_name(f, &expr.name)?,
        WriteTarget::Error(_) => write!(f, "<error>")?,
    }
    Ok(())
}

fn fmt_name(f: &mut fmt::Formatter<'_>, name: &EString) -> fmt::Result {
    for ch in name.chars() {
        match ch {
            CharPlus::Unicode(ch) => write!(f, "{}", ch)?,
            CharPlus::NonUnicode(bytes) | CharPlus::Invalid(bytes) | CharPlus::Shift(bytes) => {
                for _ in bytes.iter() {
                    write!(f, "\u{FFFD}")?;
                }
            }
        }
    }
    Ok(())
}

fn fmt_string_contents(
    f: &mut fmt::Formatter<'_>,
    contents: &[StringContent],
    delim: char,
) -> fmt::Result {
    for content in contents {
        match content {
            StringContent::Text(content) => {
                for ch in content.value.chars() {
                    match ch {
                        CharPlus::Unicode('\x07') => write!(f, "\\a")?,
                        CharPlus::Unicode('\x08') => write!(f, "\\b")?,
                        CharPlus::Unicode('\t') => write!(f, "\\t")?,
                        CharPlus::Unicode('\n') => write!(f, "\\n")?,
                        CharPlus::Unicode('\x0B') => write!(f, "\\v")?,
                        CharPlus::Unicode('\x0C') => write!(f, "\\f")?,
                        CharPlus::Unicode('\r') => write!(f, "\\r")?,
                        CharPlus::Unicode('\x1B') => write!(f, "\\e")?,
                        CharPlus::Unicode('"') if delim == '"' => write!(f, "\\\"")?,
                        CharPlus::Unicode('/') if delim == '/' => write!(f, "\\/")?,
                        CharPlus::Unicode('`') if delim == '`' => write!(f, "\\`")?,
                        CharPlus::Unicode('#') => write!(f, "\\#")?,
                        CharPlus::Unicode('\\') => write!(f, "\\\\")?,
                        CharPlus::Unicode(ch)
                            if ('\x00'..='\x1F').contains(&ch) || ch == '\x7F' =>
                        {
                            write!(f, "\\x{:02X}", ch as u32)?;
                        }
                        CharPlus::Unicode(ch) => write!(f, "{}", ch)?,
                        CharPlus::NonUnicode(bytes)
                        | CharPlus::Invalid(bytes)
                        | CharPlus::Shift(bytes) => {
                            for &byte in bytes.iter() {
                                write!(f, "\\x{:02X}", byte)?;
                            }
                        }
                    }
                }
            }
            StringContent::Interpolation(content) => {
                write!(f, "#{{")?;
                fmt_expr(f, &content.expr, 10)?;
                write!(f, "}}")?;
            }
        }
    }
    Ok(())
}

#[test]
fn test_parse_toplevel_stmts() {
    assert_eq!(
        pp_program(EStrRef::from("x; y\nz"), &["x", "y", "z"]),
        ("(x; y; z; )".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from(""), &["x", "y", "z"]),
        ("nil".to_owned(), vec![])
    );

    let src = EStrRef::from("x; y\nz");
    assert_eq!(
        p_program(src, &["x", "y", "z"]),
        (
            Program {
                range: pos_in(src, b"x; y\nz", 0),
                locals: vec![],
                body: SeqExpr {
                    range: pos_in(src, b"x; y\nz", 0),
                    parens: vec![],
                    statements: vec![
                        Stmt {
                            range: pos_in(src, b"x;", 0),
                            separator_prefix: vec![],
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"x", 0),
                                parens: vec![],
                                name: symbol("x"),
                                type_annotation: None,
                            }
                            .into(),
                            separator_suffix: vec![StmtSep {
                                range: pos_in(src, b";", 0),
                                kind: StmtSepKind::Semicolon,
                            }],
                        },
                        Stmt {
                            range: pos_in(src, b"y\n", 0),
                            separator_prefix: vec![],
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"y", 0),
                                parens: vec![],
                                name: symbol("y"),
                                type_annotation: None,
                            }
                            .into(),
                            separator_suffix: vec![StmtSep {
                                range: pos_in(src, b"\n", 0),
                                kind: StmtSepKind::Newline,
                            }],
                        },
                        Stmt {
                            range: pos_in(src, b"z", 0),
                            separator_prefix: vec![],
                            expr: LocalVariableExpr {
                                range: pos_in(src, b"z", 0),
                                parens: vec![],
                                name: symbol("z"),
                                type_annotation: None,
                            }
                            .into(),
                            separator_suffix: vec![],
                        },
                    ],
                }
                .into(),
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_parenthesized_expr() {
    assert_eq!(
        pp_program(EStrRef::from("(x)"), &["x"]),
        ("x".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("(\nx\n)"), &["x"]),
        ("x".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("(x;)"), &["x"]),
        ("x".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("(;x)"), &["x"]),
        ("x".to_owned(), vec![])
    );

    let src = EStrRef::from("(x)");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            LocalVariableExpr {
                range: pos_in(src, b"x", 0),
                parens: vec![ParenParen {
                    range: pos_in(src, b"(x)", 0),
                    open_range: pos_in(src, b"(", 0),
                    separator_prefix: vec![],
                    separator_suffix: vec![],
                    close_range: pos_in(src, b")", 0),
                }
                .into()],
                name: symbol("x"),
                type_annotation: None,
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("(\nx\n)");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            LocalVariableExpr {
                range: pos_in(src, b"x", 0),
                parens: vec![ParenParen {
                    range: pos_in(src, b"(\nx\n)", 0),
                    open_range: pos_in(src, b"(", 0),
                    separator_prefix: vec![],
                    separator_suffix: vec![StmtSep {
                        range: pos_in(src, b"\n", 1),
                        kind: StmtSepKind::Newline,
                    }],
                    close_range: pos_in(src, b")", 0),
                }
                .into()],
                name: symbol("x"),
                type_annotation: None,
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("(x;)");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            LocalVariableExpr {
                range: pos_in(src, b"x", 0),
                parens: vec![ParenParen {
                    range: pos_in(src, b"(x;)", 0),
                    open_range: pos_in(src, b"(", 0),
                    separator_prefix: vec![],
                    separator_suffix: vec![StmtSep {
                        range: pos_in(src, b";", 0),
                        kind: StmtSepKind::Semicolon,
                    }],
                    close_range: pos_in(src, b")", 0),
                }
                .into()],
                name: symbol("x"),
                type_annotation: None,
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("(;x)");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            LocalVariableExpr {
                range: pos_in(src, b"x", 0),
                parens: vec![ParenParen {
                    range: pos_in(src, b"(;x)", 0),
                    open_range: pos_in(src, b"(", 0),
                    separator_prefix: vec![StmtSep {
                        range: pos_in(src, b";", 0),
                        kind: StmtSepKind::Semicolon,
                    }],
                    separator_suffix: vec![],
                    close_range: pos_in(src, b")", 0),
                }
                .into()],
                name: symbol("x"),
                type_annotation: None,
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_parenthesized_seq_expr() {
    assert_eq!(
        pp_program(EStrRef::from("(x\ny)"), &["x", "y"]),
        ("(x; y; )".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("(x;y)"), &["x", "y"]),
        ("(x; y; )".to_owned(), vec![])
    );

    let src = EStrRef::from("(x\ny)");
    assert_eq!(
        p_expr(src, &["x", "y"]),
        (
            SeqExpr {
                range: pos_in(src, b"x\ny", 0),
                parens: vec![ParenParen {
                    range: pos_in(src, b"(x\ny)", 0),
                    open_range: pos_in(src, b"(", 0),
                    separator_prefix: vec![],
                    separator_suffix: vec![],
                    close_range: pos_in(src, b")", 0),
                }
                .into()],
                statements: vec![
                    Stmt {
                        range: pos_in(src, b"x\n", 0),
                        separator_prefix: vec![],
                        expr: LocalVariableExpr {
                            range: pos_in(src, b"x", 0),
                            parens: vec![],
                            name: symbol("x"),
                            type_annotation: None,
                        }
                        .into(),
                        separator_suffix: vec![StmtSep {
                            range: pos_in(src, b"\n", 0),
                            kind: StmtSepKind::Newline
                        }],
                    },
                    Stmt {
                        range: pos_in(src, b"y", 0),
                        separator_prefix: vec![],
                        expr: LocalVariableExpr {
                            range: pos_in(src, b"y", 0),
                            parens: vec![],
                            name: symbol("y"),
                            type_annotation: None,
                        }
                        .into(),
                        separator_suffix: vec![],
                    }
                ],
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_nil_expr() {
    assert_eq!(
        pp_program(EStrRef::from("nil"), &[]),
        ("nil".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("()"), &[]),
        ("nil".to_owned(), vec![])
    );

    let src = EStrRef::from("nil");
    assert_eq!(
        p_expr(src, &[]),
        (
            NilExpr {
                range: pos_in(src, b"nil", 0),
                parens: vec![],

                style: NilStyle::Keyword,
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_false_true_expr() {
    assert_eq!(
        pp_program(EStrRef::from("false"), &[]),
        ("false".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("true"), &[]),
        ("true".to_owned(), vec![])
    );

    let src = EStrRef::from("false");
    assert_eq!(
        p_expr(src, &[]),
        (
            FalseExpr {
                range: pos_in(src, b"false", 0),
                parens: vec![],
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("true");
    assert_eq!(
        p_expr(src, &[]),
        (
            TrueExpr {
                range: pos_in(src, b"true", 0),
                parens: vec![],
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_variable_expr() {
    assert_eq!(
        pp_program(EStrRef::from("x"), &["x"]),
        ("x".to_owned(), vec![])
    );

    let src = EStrRef::from("x");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            LocalVariableExpr {
                range: pos_in(src, b"x", 0),
                parens: vec![],
                name: symbol("x"),
                type_annotation: None,
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("x @ Integer");
    assert_eq!(
        p_expr(src, &[]),
        (
            LocalVariableExpr {
                range: pos_in(src, b"x @ Integer", 0),
                parens: vec![],
                name: symbol("x"),
                type_annotation: Some(TypeAnnotation {
                    range: pos_in(src, b"@ Integer", 0),
                    type_: IntegerType {
                        range: pos_in(src, b"Integer", 0)
                    }
                    .into()
                }),
            }
            .into(),
            vec![],
        )
    )
}

#[test]
fn test_parse_integer_expr() {
    assert_eq!(
        pp_program(EStrRef::from("42"), &[]),
        ("42".to_owned(), vec![])
    );

    let src = EStrRef::from("42");
    assert_eq!(
        p_expr(src, &[]),
        (
            NumericExpr {
                range: pos_in(src, b"42", 0),
                parens: vec![],
                value: NumericValue::Integer(BigInt::from(42)),
                imaginary: false,
            }
            .into(),
            vec![],
        )
    )
}

#[test]
fn test_parse_string_expr() {
    assert_eq!(
        pp_program(EStrRef::from("'foo'"), &[]),
        ("\"foo\"".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("\"foo\""), &[]),
        ("\"foo\"".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("'foo #{bar} baz'"), &[]),
        ("\"foo \\#{bar} baz\"".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("\"foo #{bar} baz\""), &["bar"]),
        ("\"foo #{bar} baz\"".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("\"foo #{0; 1} baz\""), &[]),
        ("\"foo #{(0; 1; )} baz\"".to_owned(), vec![])
    );

    let src = EStrRef::from("'foo'");
    assert_eq!(
        p_expr(src, &[]),
        (
            StringExpr {
                range: pos_in(src, b"'foo'", 0),
                parens: vec![],
                open_range: pos_in(src, b"'", 0),
                close_range: pos_in(src, b"'", 1),
                contents: vec![StringContent::Text(TextContent {
                    range: pos_in(src, b"foo", 0),
                    value: symbol("foo"),
                })],
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("\"foo\"");
    assert_eq!(
        p_expr(src, &[]),
        (
            StringExpr {
                range: pos_in(src, b"\"foo\"", 0),
                parens: vec![],
                open_range: pos_in(src, b"\"", 0),
                close_range: pos_in(src, b"\"", 1),
                contents: vec![StringContent::Text(TextContent {
                    range: pos_in(src, b"foo", 0),
                    value: symbol("foo"),
                })],
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("\"foo #{bar} baz\"");
    assert_eq!(
        p_expr(src, &["bar"]),
        (
            StringExpr {
                range: pos_in(src, b"\"foo #{bar} baz\"", 0),
                parens: vec![],
                open_range: pos_in(src, b"\"", 0),
                close_range: pos_in(src, b"\"", 1),
                contents: vec![
                    StringContent::Text(TextContent {
                        range: pos_in(src, b"foo ", 0),
                        value: symbol("foo "),
                    }),
                    StringContent::Interpolation(InterpolationContent {
                        range: pos_in(src, b"#{bar}", 0),
                        open_range: pos_in(src, b"#{", 0),
                        close_range: pos_in(src, b"}", 0),
                        expr: LocalVariableExpr {
                            range: pos_in(src, b"bar", 0),
                            parens: vec![],
                            name: symbol("bar"),
                            type_annotation: None,
                        }
                        .into(),
                    }),
                    StringContent::Text(TextContent {
                        range: pos_in(src, b" baz", 0),
                        value: symbol(" baz"),
                    }),
                ],
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_regexp_expr() {
    assert_eq!(
        pp_program(EStrRef::from("/foo/"), &[]),
        ("/foo/".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("/foo #{bar} baz/"), &["bar"]),
        ("/foo #{bar} baz/".to_owned(), vec![])
    );

    let src = EStrRef::from("/foo/");
    assert_eq!(
        p_expr(src, &[]),
        (
            RegexpExpr {
                range: pos_in(src, b"/foo/", 0),
                parens: vec![],
                open_range: pos_in(src, b"/", 0),
                close_range: pos_in(src, b"/", 1),
                contents: vec![StringContent::Text(TextContent {
                    range: pos_in(src, b"foo", 0),
                    value: symbol("foo"),
                })],
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_xstring_expr() {
    assert_eq!(
        pp_program(EStrRef::from("`foo`"), &[]),
        ("`foo`".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("`foo #{(bar)} baz`"), &["bar"]),
        ("`foo #{bar} baz`".to_owned(), vec![])
    );

    let src = EStrRef::from("`foo`");
    assert_eq!(
        p_expr(src, &[]),
        (
            XStringExpr {
                range: pos_in(src, b"`foo`", 0),
                parens: vec![],
                open_range: pos_in(src, b"`", 0),
                close_range: pos_in(src, b"`", 1),
                contents: vec![StringContent::Text(TextContent {
                    range: pos_in(src, b"foo", 0),
                    value: symbol("foo"),
                })],
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_const() {
    assert_eq!(
        pp_program(EStrRef::from("Foo"), &[]),
        ("Foo".to_owned(), vec![])
    );
    // assert_eq!(
    //     pp_program(EStrRef::from("::Foo"), &[]),
    //     ("::Foo".to_owned(), vec![])
    // );
    assert_eq!(
        pp_program(EStrRef::from("0::Foo"), &[]),
        ("0::Foo".to_owned(), vec![])
    );
}

#[test]
fn test_func_call() {
    assert_eq!(
        pp_program(EStrRef::from("foo()"), &[]),
        ("self.foo()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo()"), &["foo"]),
        ("self.foo()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo(1)"), &[]),
        ("self.foo(1)".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo(1,)"), &[]),
        ("self.foo(1)".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo(1,2)"), &[]),
        ("self.foo(1, 2)".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("Foo()"), &[]),
        ("self.Foo()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo!"), &[]),
        ("self.foo!()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo?"), &[]),
        ("self.foo?()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo!(1)"), &[]),
        ("self.foo!(1)".to_owned(), vec![])
    );

    let src = EStrRef::from("foo()");
    assert_eq!(
        p_expr(src, &[]),
        (
            CallExpr {
                range: pos_in(src, b"foo()", 0),
                parens: vec![],
                style: CallStyle::ImplicitSelf,
                private: true,
                optional: false,
                receiver: Box::new(
                    SelfExpr {
                        range: DUMMY_RANGE,
                        parens: vec![],
                    }
                    .into()
                ),
                method: symbol("foo"),
                method_range: pos_in(src, b"foo", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: Some(
                        ParenParen {
                            range: pos_in(src, b"()", 0),
                            open_range: pos_in(src, b"(", 0),
                            separator_prefix: vec![],
                            separator_suffix: vec![],
                            close_range: pos_in(src, b")", 0),
                        }
                        .into()
                    ),
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_cmd_func_call() {
    assert_eq!(
        pp_program(EStrRef::from("foo 1, 2"), &[]),
        ("self.foo(1, 2)".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("Foo 1, 2"), &[]),
        ("self.Foo(1, 2)".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo! 1, 2"), &[]),
        ("self.foo!(1, 2)".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("foo? 1, 2"), &[]),
        ("self.foo?(1, 2)".to_owned(), vec![])
    );

    let src = EStrRef::from("foo 1, 2");
    assert_eq!(
        p_expr(src, &[]),
        (
            CallExpr {
                range: pos_in(src, b"foo 1, 2", 0),
                parens: vec![],
                style: CallStyle::ImplicitSelf,
                private: true,
                optional: false,
                receiver: Box::new(
                    SelfExpr {
                        range: DUMMY_RANGE,
                        parens: vec![],
                    }
                    .into()
                ),
                method: symbol("foo"),
                method_range: pos_in(src, b"foo", 0),
                args: ArgList {
                    range: pos_in(src, b"1, 2", 0),
                    paren: None,
                    args: vec![
                        ExprArg {
                            range: pos_in(src, b"1", 0),
                            comma: Some(pos_in(src, b",", 0)),
                            expr: Box::new(
                                NumericExpr {
                                    range: pos_in(src, b"1", 0),
                                    parens: vec![],
                                    value: NumericValue::Integer(BigInt::from(1)),
                                    imaginary: false,
                                }
                                .into()
                            ),
                        }
                        .into(),
                        ExprArg {
                            range: pos_in(src, b"2", 0),
                            comma: None,
                            expr: Box::new(
                                NumericExpr {
                                    range: pos_in(src, b"2", 0),
                                    parens: vec![],
                                    value: NumericValue::Integer(BigInt::from(2)),
                                    imaginary: false,
                                }
                                .into(),
                            ),
                        }
                        .into(),
                    ]
                },
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_method_call() {
    assert_eq!(
        pp_program(EStrRef::from("x.foo"), &["x"]),
        ("x.foo()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x.foo()"), &["x"]),
        ("x.foo()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x.Foo"), &["x"]),
        ("x.Foo()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x.Foo()"), &["x"]),
        ("x.Foo()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x.foo!"), &["x"]),
        ("x.foo!()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x.foo!()"), &["x"]),
        ("x.foo!()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x&.foo"), &["x"]),
        ("x&.foo()".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x&.foo()"), &["x"]),
        ("x&.foo()".to_owned(), vec![])
    );

    let src = EStrRef::from("x.foo()");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            CallExpr {
                range: pos_in(src, b"x.foo()", 0),
                parens: vec![],
                style: CallStyle::Dot,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("foo"),
                method_range: pos_in(src, b"foo", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: Some(
                        ParenParen {
                            range: pos_in(src, b"()", 0),
                            open_range: pos_in(src, b"(", 0),
                            separator_prefix: vec![],
                            separator_suffix: vec![],
                            close_range: pos_in(src, b")", 0),
                        }
                        .into()
                    ),
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("x.foo");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            CallExpr {
                range: pos_in(src, b"x.foo", 0),
                parens: vec![],
                style: CallStyle::Dot,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("foo"),
                method_range: pos_in(src, b"foo", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_cmd_method_call() {
    let src = EStrRef::from("x.foo 1, 2");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            CallExpr {
                range: pos_in(src, b"x.foo 1, 2", 0),
                parens: vec![],
                style: CallStyle::Dot,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("foo"),
                method_range: pos_in(src, b"foo", 0),
                args: ArgList {
                    range: pos_in(src, b"1, 2", 0),
                    paren: None,
                    args: vec![
                        ExprArg {
                            range: pos_in(src, b"1", 0),
                            comma: Some(pos_in(src, b",", 0)),
                            expr: Box::new(
                                NumericExpr {
                                    range: pos_in(src, b"1", 0),
                                    parens: vec![],
                                    value: NumericValue::Integer(BigInt::from(1)),
                                    imaginary: false,
                                }
                                .into()
                            ),
                        }
                        .into(),
                        ExprArg {
                            range: pos_in(src, b"2", 0),
                            comma: None,
                            expr: Box::new(
                                NumericExpr {
                                    range: pos_in(src, b"2", 0),
                                    parens: vec![],
                                    value: NumericValue::Integer(BigInt::from(2)),
                                    imaginary: false,
                                }
                                .into(),
                            ),
                        }
                        .into(),
                    ]
                },
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("x.foo");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            CallExpr {
                range: pos_in(src, b"x.foo", 0),
                parens: vec![],
                style: CallStyle::Dot,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("foo"),
                method_range: pos_in(src, b"foo", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_assignment_expr() {
    let src = EStrRef::from("x = 42");
    assert_eq!(
        p_expr(src, &[]),
        (
            WriteExpr {
                range: pos_in(src, b"x = 42", 0),
                parens: vec![],
                lhs: Box::new(
                    LocalVariableWriteTarget {
                        range: pos_in(src, b"x", 0),
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                rhs: Box::new(
                    NumericExpr {
                        range: pos_in(src, b"42", 0),
                        parens: vec![],
                        value: NumericValue::Integer(BigInt::from(42)),
                        imaginary: false,
                    }
                    .into()
                ),
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("x @ Integer = 42");
    assert_eq!(
        p_expr(src, &[]),
        (
            WriteExpr {
                range: pos_in(src, b"x @ Integer = 42", 0),
                parens: vec![],
                lhs: Box::new(
                    LocalVariableWriteTarget {
                        range: pos_in(src, b"x @ Integer", 0),
                        name: symbol("x"),
                        type_annotation: Some(TypeAnnotation {
                            range: pos_in(src, b"@ Integer", 0),
                            type_: IntegerType {
                                range: pos_in(src, b"Integer", 0),
                            }
                            .into()
                        }),
                    }
                    .into()
                ),
                rhs: Box::new(
                    NumericExpr {
                        range: pos_in(src, b"42", 0),
                        parens: vec![],
                        value: NumericValue::Integer(BigInt::from(42)),
                        imaginary: false,
                    }
                    .into()
                ),
            }
            .into(),
            vec![],
        )
    )
}

#[test]
fn test_parse_mult_left_associative() {
    assert_eq!(
        pp_program(EStrRef::from("x * y * z"), &["x", "y", "z"]),
        ("x.*(y).*(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_div_left_associative() {
    assert_eq!(
        pp_program(EStrRef::from("x / y / z"), &["x", "y", "z"]),
        ("x./(y)./(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_mod_left_associative() {
    assert_eq!(
        pp_program(EStrRef::from("x % y % z"), &["x", "y", "z"]),
        ("x.%(y).%(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_mult_div() {
    assert_eq!(
        pp_program(EStrRef::from("x * y / z"), &["x", "y", "z"]),
        ("x.*(y)./(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_mult_mod() {
    assert_eq!(
        pp_program(EStrRef::from("x * y % z"), &["x", "y", "z"]),
        ("x.*(y).%(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_div_mult() {
    assert_eq!(
        pp_program(EStrRef::from("x / y * z"), &["x", "y", "z"]),
        ("x./(y).*(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_div_mod() {
    assert_eq!(
        pp_program(EStrRef::from("x / y % z"), &["x", "y", "z"]),
        ("x./(y).%(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_mod_mult() {
    assert_eq!(
        pp_program(EStrRef::from("x % y * z"), &["x", "y", "z"]),
        ("x.%(y).*(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_mult_pow() {
    assert_eq!(
        pp_program(EStrRef::from("x * y ** z"), &["x", "y", "z"]),
        ("x.*(y.**(z))".to_owned(), vec![])
    );
}

#[test]
fn test_parse_pow_mult() {
    assert_eq!(
        pp_program(EStrRef::from("x ** y * z"), &["x", "y", "z"]),
        ("x.**(y).*(z)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_multiplicative_operators() {
    let src = EStrRef::from("x * y / z % w");
    assert_eq!(
        p_expr(src, &["x", "y", "z", "w"]),
        (
            CallExpr {
                range: pos_in(src, b"x * y / z % w", 0),
                parens: vec![],
                style: CallStyle::BinOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    CallExpr {
                        range: pos_in(src, b"x * y / z", 0),
                        parens: vec![],
                        style: CallStyle::BinOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(
                            CallExpr {
                                range: pos_in(src, b"x * y", 0),
                                parens: vec![],
                                style: CallStyle::BinOp,
                                private: false,
                                optional: false,
                                receiver: Box::new(
                                    LocalVariableExpr {
                                        range: pos_in(src, b"x", 0),
                                        parens: vec![],
                                        name: symbol("x"),
                                        type_annotation: None,
                                    }
                                    .into()
                                ),
                                method: symbol("*"),
                                method_range: pos_in(src, b"*", 0),
                                args: ArgList {
                                    range: pos_in(src, b"y", 0),
                                    paren: None,
                                    args: vec![ExprArg {
                                        range: pos_in(src, b"y", 0),
                                        comma: None,
                                        expr: Box::new(
                                            LocalVariableExpr {
                                                range: pos_in(src, b"y", 0),
                                                parens: vec![],
                                                name: symbol("y"),
                                                type_annotation: None,
                                            }
                                            .into()
                                        ),
                                    }
                                    .into()],
                                },
                            }
                            .into()
                        ),
                        method: symbol("/"),
                        method_range: pos_in(src, b"/", 0),
                        args: ArgList {
                            range: pos_in(src, b"z", 0),
                            paren: None,
                            args: vec![ExprArg {
                                range: pos_in(src, b"z", 0),
                                comma: None,
                                expr: Box::new(
                                    LocalVariableExpr {
                                        range: pos_in(src, b"z", 0),
                                        parens: vec![],
                                        name: symbol("z"),
                                        type_annotation: None,
                                    }
                                    .into()
                                ),
                            }
                            .into(),],
                        },
                    }
                    .into()
                ),
                method: symbol("%"),
                method_range: pos_in(src, b"%", 0),
                args: ArgList {
                    range: pos_in(src, b"w", 0),
                    paren: None,
                    args: vec![ExprArg {
                        range: pos_in(src, b"w", 0),
                        comma: None,
                        expr: Box::new(
                            LocalVariableExpr {
                                range: pos_in(src, b"w", 0),
                                parens: vec![],
                                name: symbol("w"),
                                type_annotation: None,
                            }
                            .into()
                        ),
                    }
                    .into()],
                },
            }
            .into(),
            vec![],
        )
    );

    let src = EStrRef::from("x ** y * z ** w");
    assert_eq!(
        p_expr(src, &["x", "y", "z", "w"]),
        (
            CallExpr {
                range: pos_in(src, b"x ** y * z ** w", 0),
                parens: vec![],
                style: CallStyle::BinOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    CallExpr {
                        range: pos_in(src, b"x ** y", 0),
                        parens: vec![],
                        style: CallStyle::BinOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(
                            LocalVariableExpr {
                                range: pos_in(src, b"x", 0),
                                parens: vec![],
                                name: symbol("x"),
                                type_annotation: None,
                            }
                            .into()
                        ),
                        method: symbol("**"),
                        method_range: pos_in(src, b"**", 0),
                        args: ArgList {
                            range: pos_in(src, b"y", 0),
                            paren: None,
                            args: vec![ExprArg {
                                range: pos_in(src, b"y", 0),
                                comma: None,
                                expr: Box::new(
                                    LocalVariableExpr {
                                        range: pos_in(src, b"y", 0),
                                        parens: vec![],
                                        name: symbol("y"),
                                        type_annotation: None,
                                    }
                                    .into()
                                ),
                            }
                            .into()],
                        },
                    }
                    .into()
                ),
                method: symbol("*"),
                method_range: pos_in(src, b"*", 2),
                args: ArgList {
                    range: pos_in(src, b"z ** w", 0),
                    paren: None,
                    args: vec![ExprArg {
                        range: pos_in(src, b"z ** w", 0),
                        comma: None,
                        expr: Box::new(
                            CallExpr {
                                range: pos_in(src, b"z ** w", 0),
                                parens: vec![],
                                style: CallStyle::BinOp,
                                private: false,
                                optional: false,
                                receiver: Box::new(
                                    LocalVariableExpr {
                                        range: pos_in(src, b"z", 0),
                                        parens: vec![],
                                        name: symbol("z"),
                                        type_annotation: None,
                                    }
                                    .into()
                                ),
                                method: symbol("**"),
                                method_range: pos_in(src, b"**", 1),
                                args: ArgList {
                                    range: pos_in(src, b"w", 0),
                                    paren: None,
                                    args: vec![ExprArg {
                                        range: pos_in(src, b"w", 0),
                                        comma: None,
                                        expr: Box::new(
                                            LocalVariableExpr {
                                                range: pos_in(src, b"w", 0),
                                                parens: vec![],
                                                name: symbol("w"),
                                                type_annotation: None,
                                            }
                                            .into()
                                        ),
                                    }
                                    .into()],
                                },
                            }
                            .into()
                        )
                    }
                    .into()]
                },
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_exponential_operator() {
    let src = EStrRef::from("x ** y ** z");
    assert_eq!(
        p_expr(src, &["x", "y", "z"]),
        (
            CallExpr {
                range: pos_in(src, b"x ** y ** z", 0),
                parens: vec![],
                style: CallStyle::BinOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("**"),
                method_range: pos_in(src, b"**", 0),
                args: ArgList {
                    range: pos_in(src, b"y ** z", 0),
                    paren: None,
                    args: vec![ExprArg {
                        range: pos_in(src, b"y ** z", 0),
                        comma: None,
                        expr: Box::new(
                            CallExpr {
                                range: pos_in(src, b"y ** z", 0),
                                parens: vec![],
                                style: CallStyle::BinOp,
                                private: false,
                                optional: false,
                                receiver: Box::new(
                                    LocalVariableExpr {
                                        range: pos_in(src, b"y", 0),
                                        parens: vec![],
                                        name: symbol("y"),
                                        type_annotation: None,
                                    }
                                    .into()
                                ),
                                method: symbol("**"),
                                method_range: pos_in(src, b"**", 1),
                                args: ArgList {
                                    range: pos_in(src, b"z", 0),
                                    paren: None,
                                    args: vec![ExprArg {
                                        range: pos_in(src, b"z", 0),
                                        comma: None,
                                        expr: Box::new(
                                            LocalVariableExpr {
                                                range: pos_in(src, b"z", 0),
                                                parens: vec![],
                                                name: symbol("z"),
                                                type_annotation: None,
                                            }
                                            .into()
                                        ),
                                    }
                                    .into()],
                                },
                            }
                            .into()
                        ),
                    }
                    .into()]
                },
            }
            .into(),
            vec![],
        )
    );
    let src = EStrRef::from("+x ** +y");
    assert_eq!(
        p_expr(src, &["x", "y"]),
        (
            CallExpr {
                range: pos_in(src, b"+x ** +y", 0),
                parens: vec![],
                style: CallStyle::BinOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    CallExpr {
                        range: pos_in(src, b"+x", 0),
                        parens: vec![],
                        style: CallStyle::UnOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(
                            LocalVariableExpr {
                                range: pos_in(src, b"x", 0),
                                parens: vec![],
                                name: symbol("x"),
                                type_annotation: None,
                            }
                            .into()
                        ),
                        method: symbol("+@"),
                        method_range: pos_in(src, b"+", 0),
                        args: ArgList {
                            range: DUMMY_RANGE,
                            paren: None,
                            args: vec![]
                        },
                    }
                    .into()
                ),
                method: symbol("**"),
                method_range: pos_in(src, b"**", 0),
                args: ArgList {
                    range: pos_in(src, b"+y", 0),
                    paren: None,
                    args: vec![ExprArg {
                        range: pos_in(src, b"+y", 0),
                        comma: None,
                        expr: Box::new(
                            CallExpr {
                                range: pos_in(src, b"+y", 0),
                                parens: vec![],
                                style: CallStyle::UnOp,
                                private: false,
                                optional: false,
                                receiver: Box::new(
                                    LocalVariableExpr {
                                        range: pos_in(src, b"y", 0),
                                        parens: vec![],
                                        name: symbol("y"),
                                        type_annotation: None,
                                    }
                                    .into()
                                ),
                                method: symbol("+@"),
                                method_range: pos_in(src, b"+", 1),
                                args: ArgList {
                                    range: DUMMY_RANGE,
                                    paren: None,
                                    args: vec![]
                                },
                            }
                            .into()
                        ),
                    }
                    .into()],
                },
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_power_right_associative() {
    assert_eq!(
        pp_program(EStrRef::from("x ** y ** z"), &["x", "y", "z"]),
        ("x.**(y.**(z))".to_owned(), vec![])
    );
}

#[test]
fn test_parse_uplus_power() {
    assert_eq!(
        pp_program(EStrRef::from("+x ** y"), &["x", "y"]),
        ("x.+@().**(y)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_uminus_power() {
    assert_eq!(
        pp_program(EStrRef::from("-x ** y"), &["x", "y"]),
        ("x.**(y).-@()".to_owned(), vec![])
    );
}

#[test]
fn test_parse_not_power() {
    assert_eq!(
        pp_program(EStrRef::from("!x ** y"), &["x", "y"]),
        ("x.!().**(y)".to_owned(), vec![])
    );
}

#[test]
fn test_parse_bitwise_not_power() {
    assert_eq!(
        pp_program(EStrRef::from("~x ** y"), &["x", "y"]),
        ("x.~().**(y)".to_owned(), vec![])
    );
}

#[test]
fn test_reparse_minus_exp() {
    let src = EStrRef::from("-x ** -y ** z");
    assert_eq!(
        p_expr(src, &["x", "y", "z"]),
        (
            CallExpr {
                range: pos_in(src, b"-x ** -y ** z", 0),
                parens: vec![],
                style: CallStyle::UnOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    CallExpr {
                        range: pos_in(src, b"x ** -y ** z", 0),
                        parens: vec![],
                        style: CallStyle::BinOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(
                            LocalVariableExpr {
                                range: pos_in(src, b"x", 0),
                                parens: vec![],
                                name: symbol("x"),
                                type_annotation: None,
                            }
                            .into()
                        ),
                        method: symbol("**"),
                        method_range: pos_in(src, b"**", 0),
                        args: ArgList {
                            range: pos_in(src, b"-y ** z", 0),
                            paren: None,
                            args: vec![ExprArg {
                                range: pos_in(src, b"-y ** z", 0),
                                comma: None,
                                expr: Box::new(
                                    CallExpr {
                                        range: pos_in(src, b"-y ** z", 0),
                                        parens: vec![],
                                        style: CallStyle::UnOp,
                                        private: false,
                                        optional: false,
                                        receiver: Box::new(
                                            CallExpr {
                                                range: pos_in(src, b"y ** z", 0),
                                                parens: vec![],
                                                style: CallStyle::BinOp,
                                                private: false,
                                                optional: false,
                                                receiver: Box::new(
                                                    LocalVariableExpr {
                                                        range: pos_in(src, b"y", 0),
                                                        parens: vec![],
                                                        name: symbol("y"),
                                                        type_annotation: None,
                                                    }
                                                    .into()
                                                ),
                                                method: symbol("**"),
                                                method_range: pos_in(src, b"**", 1),
                                                args: ArgList {
                                                    range: pos_in(src, b"z", 0),
                                                    paren: None,
                                                    args: vec![ExprArg {
                                                        range: pos_in(src, b"z", 0),
                                                        comma: None,
                                                        expr: Box::new(
                                                            LocalVariableExpr {
                                                                range: pos_in(src, b"z", 0),
                                                                parens: vec![],
                                                                name: symbol("z"),
                                                                type_annotation: None,
                                                            }
                                                            .into()
                                                        )
                                                    }
                                                    .into()]
                                                },
                                            }
                                            .into()
                                        ),
                                        method: symbol("-@"),
                                        method_range: pos_in(src, b"-", 1),
                                        args: ArgList {
                                            range: DUMMY_RANGE,
                                            paren: None,
                                            args: vec![]
                                        },
                                    }
                                    .into()
                                )
                            }
                            .into()]
                        },
                    }
                    .into()
                ),
                method: symbol("-@"),
                method_range: pos_in(src, b"-", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_uplus_uplus() {
    assert_eq!(
        pp_program(EStrRef::from("++x"), &["x"]),
        ("x.+@().+@()".to_owned(), vec![])
    );
}

#[test]
fn test_parse_uminus_uminus() {
    assert_eq!(
        pp_program(EStrRef::from("--x"), &["x"]),
        ("x.-@().-@()".to_owned(), vec![])
    );
}

#[test]
fn test_parse_not_not() {
    assert_eq!(
        pp_program(EStrRef::from("!!x"), &["x"]),
        ("x.!().!()".to_owned(), vec![])
    );
}

#[test]
fn test_parse_bitwise_not_bitwise_not() {
    assert_eq!(
        pp_program(EStrRef::from("~~x"), &["x"]),
        ("x.~().~()".to_owned(), vec![])
    );
}

#[test]
fn test_parse_uplus_uminus() {
    assert_eq!(
        pp_program(EStrRef::from("+-x"), &["x"]),
        ("x.-@().+@()".to_owned(), vec![])
    );
}

#[test]
fn test_parse_uminus_uplus() {
    assert_eq!(
        pp_program(EStrRef::from("-+x"), &["x"]),
        ("x.+@().-@()".to_owned(), vec![])
    );
}

#[test]
fn test_parse_unary_operators() {
    let src = EStrRef::from("+x");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            CallExpr {
                range: pos_in(src, b"+x", 0),
                parens: vec![],
                style: CallStyle::UnOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("+@"),
                method_range: pos_in(src, b"+", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );

    let src = EStrRef::from("-x");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            CallExpr {
                range: pos_in(src, b"-x", 0),
                parens: vec![],
                style: CallStyle::UnOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("-@"),
                method_range: pos_in(src, b"-", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );

    let src = EStrRef::from("!x");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            CallExpr {
                range: pos_in(src, b"!x", 0),
                parens: vec![],
                style: CallStyle::UnOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("!"),
                method_range: pos_in(src, b"!", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );

    let src = EStrRef::from("~x");
    assert_eq!(
        p_expr(src, &["x"]),
        (
            CallExpr {
                range: pos_in(src, b"~x", 0),
                parens: vec![],
                style: CallStyle::UnOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    LocalVariableExpr {
                        range: pos_in(src, b"x", 0),
                        parens: vec![],
                        name: symbol("x"),
                        type_annotation: None,
                    }
                    .into()
                ),
                method: symbol("~"),
                method_range: pos_in(src, b"~", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );

    let src = EStrRef::from("!-+~0");
    assert_eq!(
        p_expr(src, &[]),
        (
            CallExpr {
                range: pos_in(src, b"!-+~0", 0),
                parens: vec![],
                style: CallStyle::UnOp,
                private: false,
                optional: false,
                receiver: Box::new(
                    CallExpr {
                        range: pos_in(src, b"-+~0", 0),
                        parens: vec![],
                        style: CallStyle::UnOp,
                        private: false,
                        optional: false,
                        receiver: Box::new(
                            CallExpr {
                                range: pos_in(src, b"+~0", 0),
                                parens: vec![],
                                style: CallStyle::UnOp,
                                private: false,
                                optional: false,
                                receiver: Box::new(
                                    CallExpr {
                                        range: pos_in(src, b"~0", 0),
                                        parens: vec![],
                                        style: CallStyle::UnOp,
                                        private: false,
                                        optional: false,
                                        receiver: Box::new(
                                            NumericExpr {
                                                range: pos_in(src, b"0", 0),
                                                parens: vec![],
                                                value: NumericValue::Integer(BigInt::from(0)),
                                                imaginary: false,
                                            }
                                            .into()
                                        ),
                                        method: symbol("~"),
                                        method_range: pos_in(src, b"~", 0),
                                        args: ArgList {
                                            range: DUMMY_RANGE,
                                            paren: None,
                                            args: vec![]
                                        },
                                    }
                                    .into()
                                ),
                                method: symbol("+@"),
                                method_range: pos_in(src, b"+", 0),
                                args: ArgList {
                                    range: DUMMY_RANGE,
                                    paren: None,
                                    args: vec![]
                                },
                            }
                            .into()
                        ),
                        method: symbol("-@"),
                        method_range: pos_in(src, b"-", 0),
                        args: ArgList {
                            range: DUMMY_RANGE,
                            paren: None,
                            args: vec![]
                        },
                    }
                    .into()
                ),
                method: symbol("!"),
                method_range: pos_in(src, b"!", 0),
                args: ArgList {
                    range: DUMMY_RANGE,
                    paren: None,
                    args: vec![]
                },
            }
            .into(),
            vec![],
        )
    );
}

#[test]
fn test_parse_logical_or() {
    assert_eq!(
        pp_program(EStrRef::from("x || y"), &["x", "y"]),
        ("x || y".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x || y || z"), &["x", "y", "z"]),
        ("(x || y) || z".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x or y"), &["x", "y"]),
        ("x || y".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x or y or z"), &["x", "y", "z"]),
        ("(x || y) || z".to_owned(), vec![])
    );
}

#[test]
fn test_parse_logical_and() {
    assert_eq!(
        pp_program(EStrRef::from("x && y"), &["x", "y"]),
        ("x && y".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x && y && z"), &["x", "y", "z"]),
        ("(x && y) && z".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x and y"), &["x", "y"]),
        ("x && y".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("x and y and z"), &["x", "y", "z"]),
        ("(x && y) && z".to_owned(), vec![])
    );
}

#[test]
fn test_parse_if_exprs() {
    assert_eq!(
        pp_program(EStrRef::from("if 0; 1 end"), &[]),
        ("if 0 then 1 else nil end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("if 0 then 1 end"), &[]),
        ("if 0 then 1 else nil end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("if 0; then 1 end"), &[]),
        ("if 0 then 1 else nil end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("if 0 then 1 else 2 end"), &[]),
        ("if 0 then 1 else 2 end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("if 0 then 1; 2 else 3; 4 end"), &[]),
        ("if 0 then (1; 2; ) else (3; 4; ) end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("if 0 then 1 elsif 2 then 3 end"), &[]),
        (
            "if 0 then 1 else if 2 then 3 else nil end end".to_owned(),
            vec![]
        )
    );
    assert_eq!(
        pp_program(EStrRef::from("if 0 then 1 elsif 2 then 3 else 4 end"), &[]),
        (
            "if 0 then 1 else if 2 then 3 else 4 end end".to_owned(),
            vec![]
        )
    );
    assert_eq!(
        pp_program(EStrRef::from("unless 0 then 1 end"), &[]),
        ("if 0 then nil else 1 end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("unless 0 then 1 else 2 end"), &[]),
        ("if 0 then 2 else 1 end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("0 if 1"), &[]),
        ("if 1 then 0 else nil end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("0 unless 1"), &[]),
        ("if 1 then nil else 0 end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("0 ? 1 : 2"), &[]),
        ("if 0 then 1 else 2 end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("0 ? 1 : 2 ? 3 : 4"), &[]),
        (
            "if 0 then 1 else if 2 then 3 else 4 end end".to_owned(),
            vec![]
        )
    );
}

#[test]
fn test_parse_while_until_exprs() {
    assert_eq!(
        pp_program(EStrRef::from("while 0; 1 end"), &[]),
        ("while 0 do 1 end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("0 while 1"), &[]),
        ("while 1 do 0 end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("until 0; 1 end"), &[]),
        ("until 0 do 1 end".to_owned(), vec![])
    );
    assert_eq!(
        pp_program(EStrRef::from("0 until 1"), &[]),
        ("until 1 do 0 end".to_owned(), vec![])
    );
}

#[test]
fn test_parse_integer_type() {
    let src = EStrRef::from("Integer");
    assert_eq!(
        p_type(src),
        (
            IntegerType {
                range: pos_in(src, b"Integer", 0),
            }
            .into(),
            vec![],
        )
    )
}
