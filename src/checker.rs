use std::{collections::HashMap, sync::LazyLock};

use crate::{
    ast::{
        ErrorType, Expr, FalseType, IntegerType, NilType, Program, RegexpType, StringContent,
        StringType, TrueType, Type, TypeAnnotation, WriteTarget, DUMMY_RANGE,
    },
    Diagnostic, EString,
};

pub fn typecheck_program(diag: &mut Vec<Diagnostic>, program: &Program) {
    typecheck_expr(diag, &program.body);
}

fn typecheck_expr(diag: &mut Vec<Diagnostic>, expr: &Expr) -> Type {
    match expr {
        Expr::Seq(expr) => {
            for stmt in &expr.statements[..expr.statements.len() - 1] {
                typecheck_expr(diag, &stmt.expr);
            }
            typecheck_expr(diag, &expr.statements[expr.statements.len() - 1].expr)
        }
        Expr::Nil(_) => NilType { range: DUMMY_RANGE }.into(),
        Expr::False(_) => FalseType { range: DUMMY_RANGE }.into(),
        Expr::True(_) => TrueType { range: DUMMY_RANGE }.into(),
        Expr::Integer(_) => IntegerType { range: DUMMY_RANGE }.into(),
        Expr::String(expr) => {
            for content in &expr.contents {
                match content {
                    StringContent::Interpolation(content) => {
                        typecheck_expr(diag, &content.expr);
                    }
                    StringContent::Text(_) => {}
                }
            }
            StringType { range: DUMMY_RANGE }.into()
        }
        Expr::Regexp(expr) => {
            for content in &expr.contents {
                match content {
                    StringContent::Interpolation(content) => {
                        typecheck_expr(diag, &content.expr);
                    }
                    StringContent::Text(_) => {}
                }
            }
            RegexpType { range: DUMMY_RANGE }.into()
        }
        Expr::XString(expr) => {
            for content in &expr.contents {
                match content {
                    StringContent::Interpolation(content) => {
                        typecheck_expr(diag, &content.expr);
                    }
                    StringContent::Text(_) => {}
                }
            }
            StringType { range: DUMMY_RANGE }.into()
        }
        Expr::LocalVariable(_) => {
            diag.push(Diagnostic {
                range: *expr.range(),
                message: format!("undefined variable"),
            });
            ErrorType { range: DUMMY_RANGE }.into()
        }
        Expr::Call(expr) => {
            let receiver_type = typecheck_expr(diag, &expr.receiver);
            let arg_types = expr
                .args
                .iter()
                .map(|arg| typecheck_expr(diag, arg))
                .collect::<Vec<_>>();
            let primary_module = match &receiver_type {
                Type::Nil(_) => Module::NilClass,
                Type::False(_) => Module::FalseClass,
                Type::True(_) => Module::TrueClass,
                Type::Integer(_) => Module::Integer,
                Type::String(_) => Module::String,
                Type::Regexp(_) => Module::Regexp,
                Type::Error(_) => return ErrorType { range: DUMMY_RANGE }.into(),
                _ => {
                    diag.push(Diagnostic {
                        range: *expr.receiver.range(),
                        message: format!("undefined method"),
                    });
                    return ErrorType { range: DUMMY_RANGE }.into();
                }
            };
            let meth = primary_module.ancestors().iter().find_map(|&module| {
                INSTANCE_METHODS
                    .get(&(module, expr.method.clone()))
                    .cloned()
            });
            let Some(meth) = meth else {
                diag.push(Diagnostic {
                    range: expr.method_range,
                    message: format!("undefined method"),
                });
                return ErrorType { range: DUMMY_RANGE }.into();
            };
            if meth.arg_types.len() != arg_types.len() {
                diag.push(Diagnostic {
                    range: expr.range,
                    message: format!("wrong number of arguments"),
                });
            } else {
                for (arg_type, expected_type) in arg_types.iter().zip(&meth.arg_types) {
                    match (arg_type, expected_type) {
                        (Type::Error(_), _) => {}
                        (_, Type::Error(_)) => {}
                        (Type::Nil(_), Type::Nil(_)) => {}
                        (Type::False(_), Type::False(_)) => {}
                        (Type::True(_), Type::True(_)) => {}
                        (Type::Integer(_), Type::Integer(_)) => {}
                        (Type::String(_), Type::String(_)) => {}
                        (Type::Regexp(_), Type::Regexp(_)) => {}
                        _ => {
                            diag.push(Diagnostic {
                                range: expr.range,
                                message: format!("type mismatch"),
                            });
                        }
                    }
                }
            }
            meth.return_type.clone()
        }
        Expr::Write(expr) => {
            let rhs_type = typecheck_expr(diag, &expr.rhs);
            let annot = match &*expr.lhs {
                WriteTarget::LocalVariable(target) => &target.type_annotation,
                WriteTarget::Error(_) => &None,
            };
            if let Some(TypeAnnotation {
                type_: lhs_type, ..
            }) = annot
            {
                match (lhs_type, &rhs_type) {
                    (_, Type::Error(_)) => {}
                    (Type::Error(_), _) => {
                        return rhs_type.clone();
                    }
                    (Type::Nil(_), Type::Nil(_)) => {}
                    (Type::False(_), Type::False(_)) => {}
                    (Type::True(_), Type::True(_)) => {}
                    (Type::Integer(_), Type::Integer(_)) => {}
                    (Type::String(_), Type::String(_)) => {}
                    (Type::Regexp(_), Type::Regexp(_)) => {}
                    _ => {
                        diag.push(Diagnostic {
                            range: *expr.lhs.range(),
                            message: format!("type mismatch"),
                        });
                    }
                }
                lhs_type.clone()
            } else {
                rhs_type
            }
        }
        Expr::Error(_) => ErrorType { range: DUMMY_RANGE }.into(),
        _ => {
            diag.push(Diagnostic {
                range: *expr.range(),
                message: format!("TODO: typechecker not implemented for this expression"),
            });
            ErrorType { range: DUMMY_RANGE }.into()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MethodSignature {
    arg_types: Vec<Type>,
    return_type: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Module {
    BasicObject,
    Kernel,
    Object,
    NilClass,
    FalseClass,
    TrueClass,
    Comparable,
    Numeric,
    Integer,
    String,
    Regexp,
}

impl Module {
    fn ancestors(&self) -> Vec<Module> {
        use Module::*;
        match self {
            BasicObject => vec![],
            Kernel => vec![Kernel],
            Object => vec![Object, Kernel, BasicObject],
            NilClass => vec![NilClass, Object, Kernel, BasicObject],
            FalseClass => vec![FalseClass, Object, Kernel, BasicObject],
            TrueClass => vec![TrueClass, Object, Kernel, BasicObject],
            Comparable => vec![Comparable],
            Numeric => vec![Numeric, Comparable, Object, Kernel, BasicObject],
            Integer => vec![Integer, Numeric, Comparable, Object, Kernel, BasicObject],
            String => vec![String, Comparable, Object, Kernel, BasicObject],
            Regexp => vec![Regexp, Object, Kernel, BasicObject],
        }
    }
}

static INSTANCE_METHODS: LazyLock<HashMap<(Module, EString), MethodSignature>> =
    LazyLock::new(|| {
        vec![
            (
                (Module::BasicObject, "!".to_owned()),
                MethodSignature {
                    arg_types: vec![],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::BasicObject, "==".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::BasicObject, "!=".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Kernel, "`".to_owned()),
                MethodSignature {
                    arg_types: vec![StringType { range: DUMMY_RANGE }.into()],
                    return_type: StringType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Object, "<=>".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Object, "==".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Object, "===".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::NilClass, "&".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::NilClass, "|".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::NilClass, "^".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::NilClass, "=~".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    return_type: NilType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::FalseClass, "&".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::FalseClass, "|".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::FalseClass, "^".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::TrueClass, "&".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::TrueClass, "|".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    return_type: TrueType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::TrueClass, "^".to_owned()),
                MethodSignature {
                    // TODO: any type
                    arg_types: vec![NilType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Comparable, "<".to_owned()),
                MethodSignature {
                    arg_types: vec![
                        // TODO: any type
                        IntegerType { range: DUMMY_RANGE }.into(),
                    ],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Comparable, "<=".to_owned()),
                MethodSignature {
                    arg_types: vec![
                        // TODO: any type
                        IntegerType { range: DUMMY_RANGE }.into(),
                    ],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Comparable, ">".to_owned()),
                MethodSignature {
                    arg_types: vec![
                        // TODO: any type
                        IntegerType { range: DUMMY_RANGE }.into(),
                    ],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Comparable, ">=".to_owned()),
                MethodSignature {
                    arg_types: vec![
                        // TODO: any type
                        IntegerType { range: DUMMY_RANGE }.into(),
                    ],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Numeric, "+@".to_owned()),
                MethodSignature {
                    arg_types: vec![],
                    // TODO: self type
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "-@".to_owned()),
                MethodSignature {
                    arg_types: vec![],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "~".to_owned()),
                MethodSignature {
                    arg_types: vec![],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "**".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "*".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "/".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "%".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "+".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "-".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "<<".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, ">>".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "&".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "|".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "^".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "<".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "<=".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, ">".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, ">=".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "<=>".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: IntegerType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "==".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::Integer, "!=".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    // TODO: boolean
                    return_type: FalseType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::String, "+@".to_owned()),
                MethodSignature {
                    arg_types: vec![],
                    return_type: StringType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::String, "-@".to_owned()),
                MethodSignature {
                    arg_types: vec![],
                    return_type: StringType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::String, "*".to_owned()),
                MethodSignature {
                    arg_types: vec![IntegerType { range: DUMMY_RANGE }.into()],
                    return_type: StringType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::String, "+".to_owned()),
                MethodSignature {
                    arg_types: vec![StringType { range: DUMMY_RANGE }.into()],
                    return_type: StringType { range: DUMMY_RANGE }.into(),
                },
            ),
            (
                (Module::String, "<<".to_owned()),
                MethodSignature {
                    arg_types: vec![StringType { range: DUMMY_RANGE }.into()],
                    return_type: StringType { range: DUMMY_RANGE }.into(),
                },
            ),
        ]
        .into_iter()
        .map(|((module, meth), sig)| ((module, EString::from(meth).asciified()), sig))
        .collect()
    });

#[cfg(test)]
mod tests {
    use crate::{
        ast::{pos_in, StringType},
        encoding::EStrRef,
        parse_expr,
    };

    use super::*;

    fn typecheck_expr_text(s: &str) -> (Type, Vec<Diagnostic>) {
        let mut diag = Vec::new();
        let expr = parse_expr(&mut diag, EStrRef::from(s), &[]);
        let ty = typecheck_expr(&mut diag, &expr);
        (ty, diag)
    }

    #[test]
    fn test_typecheck_integer() {
        assert_eq!(
            typecheck_expr_text("42"),
            (IntegerType { range: DUMMY_RANGE }.into(), vec![]),
        );
    }

    #[test]
    fn test_typecheck_assignment() {
        assert_eq!(
            typecheck_expr_text("x = 42"),
            (IntegerType { range: DUMMY_RANGE }.into(), vec![]),
        );
        let src = "x @ Integer = 42";
        assert_eq!(
            typecheck_expr_text(src),
            (
                IntegerType {
                    range: pos_in(src.as_bytes(), b"Integer", 0)
                }
                .into(),
                vec![],
            ),
        );
        let src = "x @ String = 42";
        assert_eq!(
            typecheck_expr_text(src),
            (
                StringType {
                    range: pos_in(src.as_bytes(), b"String", 0),
                }
                .into(),
                vec![Diagnostic {
                    range: pos_in(src.as_bytes(), b"x @ String", 0),
                    message: "type mismatch".to_string(),
                }],
            ),
        );
    }
}
