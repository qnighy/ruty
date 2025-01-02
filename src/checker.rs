use crate::{
    ast::{
        ErrorType, Expr, IntegerType, NilType, Program, StmtList, Type, TypeAnnotation,
        WriteTarget, DUMMY_RANGE,
    },
    Diagnostic,
};

pub fn typecheck_program(diag: &mut Vec<Diagnostic>, program: &Program) {
    typecheck_stmt_list(diag, &program.stmt_list);
}

fn typecheck_expr(diag: &mut Vec<Diagnostic>, expr: &Expr) -> Type {
    match expr {
        Expr::Seq(expr) => typecheck_stmt_list(diag, &expr.stmt_list),
        Expr::LocalVariable(_) => {
            diag.push(Diagnostic {
                range: *expr.range(),
                message: format!("undefined variable"),
            });
            ErrorType { range: DUMMY_RANGE }.into()
        }
        Expr::Integer(_) => IntegerType { range: DUMMY_RANGE }.into(),
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
                    (Type::Integer(_), Type::Integer(_)) => {}
                    (Type::String(_), Type::String(_)) => {}
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

fn typecheck_stmt_list(diag: &mut Vec<Diagnostic>, stmt_list: &StmtList) -> Type {
    let stmts = &stmt_list.stmts;
    if stmts.is_empty() {
        NilType { range: DUMMY_RANGE }.into()
    } else {
        for stmt in &stmts[..stmts.len() - 1] {
            typecheck_expr(diag, &stmt.expr);
        }
        typecheck_expr(diag, &stmts[stmts.len() - 1].expr)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{pos_in, StringType},
        parse_expr,
    };

    use super::*;

    fn typecheck_expr_text(s: &str) -> (Type, Vec<Diagnostic>) {
        let mut diag = Vec::new();
        let expr = parse_expr(&mut diag, s.as_bytes());
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
                    range: pos_in(src.as_bytes(), b"Integer")
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
                    range: pos_in(src.as_bytes(), b"String"),
                }
                .into(),
                vec![Diagnostic {
                    range: pos_in(src.as_bytes(), b"x @ String"),
                    message: "type mismatch".to_string(),
                }],
            ),
        );
    }
}
