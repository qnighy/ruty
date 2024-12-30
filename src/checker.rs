use anyhow::Error;

use crate::ast::{Expr, IntegerType, Type, WriteTarget, DUMMY_RANGE};

pub fn typecheck_expr(expr: &Expr) -> Result<Type, Error> {
    match expr {
        Expr::Integer(_) => Ok(IntegerType { range: DUMMY_RANGE }.into()),
        Expr::LocalVariable(_) => todo!("type of local variable"),
        Expr::Write(expr) => {
            let rhs_type = typecheck_expr(&expr.rhs)?;
            let annot = match &*expr.lhs {
                WriteTarget::LocalVariable(target) => &target.type_annotation,
            };
            if let Some(lhs_type) = annot {
                match (lhs_type, rhs_type) {
                    (Type::Integer(_), Type::Integer(_)) => Ok(lhs_type.clone()),
                    (Type::String(_), Type::String(_)) => Ok(lhs_type.clone()),
                    _ => Err(Error::msg("type mismatch")),
                }
            } else {
                Ok(rhs_type)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::pos_in, parse_expr};

    use super::*;

    fn typecheck_expr_text(s: &str) -> Result<Type, Error> {
        let expr = parse_expr(s.as_bytes())?;
        typecheck_expr(&expr)
    }

    #[test]
    fn test_typecheck_integer() {
        assert_eq!(
            typecheck_expr_text("42").unwrap(),
            IntegerType { range: DUMMY_RANGE }.into()
        );
    }

    #[test]
    fn test_typecheck_assignment() {
        assert_eq!(
            typecheck_expr_text("x = 42").unwrap(),
            IntegerType { range: DUMMY_RANGE }.into()
        );
        let src = "x @ Integer = 42";
        assert_eq!(
            typecheck_expr_text(src).unwrap(),
            IntegerType {
                range: pos_in(src.as_bytes(), b"Integer")
            }
            .into()
        );
        assert!(typecheck_expr_text("x @ String = 42").is_err());
    }
}
