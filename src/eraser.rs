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
    collect_ranges_expr(ranges, &program.body);
}

fn collect_ranges_expr(ranges: &mut Vec<CodeRange>, expr: &Expr) {
    match expr {
        Expr::Seq(expr) => {
            for stmt in &expr.statements {
                collect_ranges_expr(ranges, &stmt.expr);
            }
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
        Expr::And(expr) => {
            collect_ranges_expr(ranges, &expr.lhs);
            collect_ranges_expr(ranges, &expr.rhs);
        }
        Expr::Or(expr) => {
            collect_ranges_expr(ranges, &expr.lhs);
            collect_ranges_expr(ranges, &expr.rhs);
        }
        Expr::If(expr) => {
            collect_ranges_expr(ranges, &expr.cond);
            collect_ranges_expr(ranges, &expr.then);
            collect_ranges_expr(ranges, &expr.else_);
        }
        Expr::While(expr) => {
            collect_ranges_expr(ranges, &expr.cond);
            collect_ranges_expr(ranges, &expr.body);
        }
        Expr::Until(expr) => {
            collect_ranges_expr(ranges, &expr.cond);
            collect_ranges_expr(ranges, &expr.body);
        }
        Expr::Error(_) => {}
    }
}

fn collect_ranges_string_contents(ranges: &mut Vec<CodeRange>, contents: &[StringContent]) {
    for content in contents {
        match content {
            StringContent::Text(_) => {}
            StringContent::Interpolation(content) => {
                collect_ranges_expr(ranges, &content.expr);
            }
        }
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
