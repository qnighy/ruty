use anyhow::Error;

use crate::ast::{Expr, IntegerType, Type, WriteTarget};

pub fn typecheck_expr(expr: &Expr) -> Result<Type, Error> {
    match expr {
        Expr::Integer(_) => Ok(IntegerType {}.into()),
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
    use crate::parse_expr;

    use super::*;

    fn typecheck_expr_text(s: &str) -> Result<Type, Error> {
        let expr = parse_expr(s.as_bytes())?;
        typecheck_expr(&expr)
    }

    #[test]
    fn test_typecheck_integer() {
        assert_eq!(typecheck_expr_text("42").unwrap(), IntegerType {}.into());
    }

    #[test]
    fn test_typecheck_assignment() {
        assert_eq!(
            typecheck_expr_text("x = 42").unwrap(),
            IntegerType {}.into()
        );
        assert_eq!(
            typecheck_expr_text("x @ Integer = 42").unwrap(),
            IntegerType {}.into()
        );
        assert!(typecheck_expr_text("x @ String = 42").is_err());
    }
}
