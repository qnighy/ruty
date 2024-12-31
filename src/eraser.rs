use crate::ast::{CodeRange, Expr, WriteTarget};

pub fn erase_type(src: &[u8], tree: &Expr) -> Vec<u8> {
    let mut ranges = Vec::new();
    collect_ranges_expr(&mut ranges, tree);
    let mut erased = Vec::<u8>::with_capacity(src.len());
    let mut last = 0;
    for &range in &ranges {
        erased.extend_from_slice(&src[last..range.start]);
        // TODO: count by EAW
        // TODO: take LFs into account
        let num_chars = range.end - range.start;
        for _ in 0..num_chars {
            erased.push(b' ');
        }
        last = range.end;
    }
    erased.extend_from_slice(&src[last..]);
    erased
}

fn collect_ranges_expr(ranges: &mut Vec<CodeRange>, expr: &Expr) {
    match expr {
        Expr::LocalVariable(expr) => {
            if let Some(ta) = &expr.type_annotation {
                ranges.push(ta.range);
            }
        }
        Expr::Integer(_) => {}
        Expr::Write(expr) => {
            collect_ranges_write_target(ranges, &*expr.lhs);
            collect_ranges_expr(ranges, &*expr.rhs);
        }
        Expr::Error(_) => {}
    }
}

fn collect_ranges_write_target(ranges: &mut Vec<CodeRange>, target: &WriteTarget) {
    match target {
        WriteTarget::LocalVariable(target) => {
            if let Some(ta) = &target.type_annotation {
                ranges.push(ta.range);
            }
        }
        WriteTarget::Error(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_erase_type() {
        assert_eq!(et("x"), "x");
        assert_eq!(et("42"), "42");
        assert_eq!(et("x = 42"), "x = 42");
        assert_eq!(et("x @ Integer = 42"), "x           = 42");
    }

    fn et(src: &str) -> String {
        let mut diag = Vec::new();
        let expr = crate::parse_expr(&mut diag, src.as_bytes());
        assert_eq!(diag, vec![]);
        let erased = erase_type(src.as_bytes(), &expr);
        String::from_utf8(erased).unwrap()
    }
}
