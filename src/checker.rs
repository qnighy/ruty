use anyhow::Error;

use crate::{
    ast::{ErrorType, Expr, IntegerType, Type, TypeAnnotation, WriteTarget, DUMMY_RANGE},
    Diagnostic,
};

pub fn typecheck_expr(diag: &mut Vec<Diagnostic>, expr: &Expr) -> Result<Type, Error> {
    match expr {
        Expr::Integer(_) => Ok(IntegerType { range: DUMMY_RANGE }.into()),
        Expr::LocalVariable(_) => todo!("type of local variable"),
        Expr::Write(expr) => {
            let rhs_type = typecheck_expr(diag, &expr.rhs)?;
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
                        return Ok(rhs_type.clone());
                    }
                    (Type::Integer(_), Type::Integer(_)) => {}
                    (Type::String(_), Type::String(_)) => {}
                    _ => {
                        diag.push(Diagnostic {
                            range: *expr.lhs.range(),
                            message: format!("type mismatch"),
                        });
                    }
                }
                Ok(lhs_type.clone())
            } else {
                Ok(rhs_type)
            }
        }
        Expr::Error(_) => Ok(ErrorType { range: DUMMY_RANGE }.into()),
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
        let ty = typecheck_expr(&mut diag, &expr).unwrap();
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
