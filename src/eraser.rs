use crate::ast::{CodeRange, ConstReceiver, Expr, Program, StringContent, WriteTarget};

pub fn erase_type(src: &[u8], program: &Program) -> Vec<u8> {
    let mut ranges = Vec::new();
    collect_ranges_program(&mut ranges, program);
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

fn collect_ranges_program(ranges: &mut Vec<CodeRange>, program: &Program) {
    collect_ranges_stmt_list(ranges, &program.stmt_list);
}

fn collect_ranges_expr(ranges: &mut Vec<CodeRange>, expr: &Expr) {
    match expr {
        Expr::Seq(expr) => {
            collect_ranges_stmt_list(ranges, &expr.stmt_list);
        }
        Expr::Nil(_) => {}
        Expr::False(_) => {}
        Expr::True(_) => {}
        Expr::Integer(_) => {}
        Expr::String(expr) => {
            collect_ranges_string_contents(ranges, &expr.contents);
        }
        Expr::Regexp(expr) => {
            collect_ranges_string_contents(ranges, &expr.contents);
        }
        Expr::XString(expr) => {
            collect_ranges_string_contents(ranges, &expr.contents);
        }
        Expr::LocalVariable(expr) => {
            if let Some(ta) = &expr.type_annotation {
                ranges.push(ta.range);
            }
        }
        Expr::Const(expr) => {
            if let ConstReceiver::Expr(recv) = &expr.receiver {
                collect_ranges_expr(ranges, recv);
            }
        }
        Expr::Self_(_) => {}
        Expr::SourceEncoding(_) => {}
        Expr::SourceFile(_) => {}
        Expr::SourceLine(_) => {}
        Expr::Call(expr) => {
            collect_ranges_expr(ranges, &expr.receiver);
            for arg in &expr.args {
                collect_ranges_expr(ranges, arg);
            }
        }
        Expr::Write(expr) => {
            collect_ranges_write_target(ranges, &*expr.lhs);
            collect_ranges_expr(ranges, &*expr.rhs);
        }
        Expr::Error(_) => {}
    }
}

fn collect_ranges_string_contents(ranges: &mut Vec<CodeRange>, contents: &[StringContent]) {
    for content in contents {
        match content {
            StringContent::Text(_) => {}
            StringContent::Interpolation(content) => {
                collect_ranges_stmt_list(ranges, &content.stmt_list);
            }
        }
    }
}

fn collect_ranges_stmt_list(ranges: &mut Vec<CodeRange>, stmt_list: &crate::ast::StmtList) {
    for stmt in &stmt_list.stmts {
        collect_ranges_expr(ranges, &stmt.expr);
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
    use crate::encoding::EStrRef;

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
        let program = crate::parse(&mut diag, EStrRef::from(src), &[]);
        assert_eq!(diag, vec![]);
        let erased = erase_type(src.as_bytes(), &program);
        String::from_utf8(erased).unwrap()
    }
}
