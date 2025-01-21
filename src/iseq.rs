//! Instruction sequence for typechecking.
//! It does not necessarily correspond to the bytecode format
//! of the same name in CRuby.

use std::collections::HashMap;

use crate::{
    ast::{CodeRange, ConstReceiver, Expr, Program, WriteTarget, DUMMY_RANGE},
    EString,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ISeq {
    pub(crate) num_locals: usize,
    pub(crate) instructions: Vec<Instr>,
}

impl ISeq {
    fn push(&mut self, instr: Instr) -> usize {
        self.instructions.push(instr);
        self.instructions.len() - 1
    }
    fn reserve(&mut self) -> (usize, ReservedId) {
        let id = self.instructions.len();
        self.instructions.push(Instr {
            kind: InstrKind::Error,
            range: DUMMY_RANGE,
        });
        (id, ReservedId { reserved_id: id })
    }
    fn commit(&mut self, reserved_id: ReservedId, instr: Instr) {
        self.instructions[reserved_id.reserved_id] = instr;
    }
}

#[must_use]
#[derive(Debug)]
struct ReservedId {
    reserved_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Instr {
    pub(crate) kind: InstrKind,
    pub(crate) range: CodeRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum InstrKind {
    Label {
        from: Vec<usize>,
    },
    LoadConstNil,
    LoadConstTrue,
    LoadConstFalse,
    LoadConstInteger {
        value: i32,
    },
    LoadConstString {
        value: EString,
    },
    LoadSourceEncoding,
    LoadSourceFile,
    LoadSourceLine,
    LoadObjectClass,
    ReadLocal {
        local_id: usize,
    },
    WriteLocal {
        local_id: usize,
        value_id: usize,
    },
    ReadConst {
        name: EString,
    },
    WriteConst {
        name: EString,
        value_id: usize,
    },
    ReadConstUnder {
        name: EString,
        receiver_id: usize,
    },
    WriteConstUnder {
        name: EString,
        receiver_id: usize,
        value_id: usize,
    },
    Call {
        receiver_id: usize,
        method_name: EString,
        arg_ids: Vec<usize>,
        private: bool,
    },
    Return {
        value_id: usize,
    },
    Jump {
        to: usize,
    },
    JumpValue {
        to: usize,
        value_id: usize,
    },
    Branch {
        branch_type: BranchType,
        cond_id: usize,
        then_id: usize,
        else_id: usize,
    },
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum BranchType {
    Truthy,
    NotNil,
}

pub(crate) fn iseq_from_program(program: &Program) -> ISeq {
    let mut iseq = ISeq {
        num_locals: program.locals.len() + 1,
        instructions: Vec::new(),
    };
    let mut locals_map = HashMap::new();
    for (i, name) in program.locals.iter().enumerate() {
        locals_map.insert(name.clone(), i + 1);
    }
    let result_id = compile_expr(&mut iseq, &program.body, &locals_map);
    iseq.push(Instr {
        kind: InstrKind::Return {
            value_id: result_id,
        },
        range: *program.body.range(),
    });
    iseq
}

fn compile_expr(iseq: &mut ISeq, expr: &Expr, locals_map: &HashMap<EString, usize>) -> usize {
    match expr {
        Expr::Seq(expr) => {
            let mut last_id = usize::MAX;
            for stmt in &expr.statements {
                last_id = compile_expr(iseq, &stmt.expr, locals_map);
            }
            last_id
        }
        Expr::Nil(expr) => iseq.push(Instr {
            kind: InstrKind::LoadConstNil,
            range: expr.range,
        }),
        Expr::False(expr) => iseq.push(Instr {
            kind: InstrKind::LoadConstFalse,
            range: expr.range,
        }),
        Expr::True(expr) => iseq.push(Instr {
            kind: InstrKind::LoadConstTrue,
            range: expr.range,
        }),
        Expr::Integer(expr) => iseq.push(Instr {
            kind: InstrKind::LoadConstInteger { value: expr.value },
            range: expr.range,
        }),
        Expr::String(expr) => {
            // TODO
            iseq.push(Instr {
                kind: InstrKind::Error,
                range: expr.range,
            })
        }
        Expr::Regexp(expr) => {
            // TODO
            iseq.push(Instr {
                kind: InstrKind::Error,
                range: expr.range,
            })
        }
        Expr::XString(expr) => {
            // TODO
            iseq.push(Instr {
                kind: InstrKind::Error,
                range: expr.range,
            })
        }
        Expr::LocalVariable(expr) => {
            let local_id = locals_map.get(&expr.name).copied().unwrap_or(0);
            iseq.push(Instr {
                kind: InstrKind::ReadLocal { local_id },
                range: expr.range,
            })
        }
        Expr::Const(expr) => match &expr.receiver {
            ConstReceiver::Expr(receiver) => {
                let receiver_id = compile_expr(iseq, receiver, locals_map);
                iseq.push(Instr {
                    kind: InstrKind::ReadConstUnder {
                        name: expr.name.clone(),
                        receiver_id,
                    },
                    range: expr.range,
                })
            }
            ConstReceiver::None => iseq.push(Instr {
                kind: InstrKind::ReadConst {
                    name: expr.name.clone(),
                },
                range: expr.range,
            }),
            ConstReceiver::Object => {
                let klass_id = iseq.push(Instr {
                    kind: InstrKind::LoadObjectClass,
                    range: expr.range,
                });
                iseq.push(Instr {
                    kind: InstrKind::ReadConstUnder {
                        name: expr.name.clone(),
                        receiver_id: klass_id,
                    },
                    range: expr.range,
                })
            }
        },
        Expr::Self_(expr) => iseq.push(Instr {
            kind: InstrKind::ReadLocal { local_id: 0 },
            range: expr.range,
        }),
        Expr::SourceEncoding(expr) => iseq.push(Instr {
            kind: InstrKind::LoadSourceEncoding,
            range: expr.range,
        }),
        Expr::SourceFile(expr) => iseq.push(Instr {
            kind: InstrKind::LoadSourceFile,
            range: expr.range,
        }),
        Expr::SourceLine(expr) => iseq.push(Instr {
            kind: InstrKind::LoadSourceLine,
            range: expr.range,
        }),
        Expr::Call(expr) => {
            if expr.optional {
                let receiver_id = compile_expr(iseq, &expr.receiver, locals_map);
                compile_branch(
                    iseq,
                    BranchType::NotNil,
                    receiver_id,
                    |iseq| {
                        let arg_ids = expr
                            .args
                            .iter()
                            .map(|arg| compile_expr(iseq, arg, locals_map))
                            .collect();
                        iseq.push(Instr {
                            kind: InstrKind::Call {
                                receiver_id,
                                method_name: expr.method.clone(),
                                arg_ids,
                                private: expr.private,
                            },
                            range: expr.range,
                        })
                    },
                    |iseq| {
                        iseq.push(Instr {
                            kind: InstrKind::LoadConstNil,
                            range: expr.range,
                        })
                    },
                    expr.range,
                )
            } else {
                let receiver_id = compile_expr(iseq, &expr.receiver, locals_map);
                let arg_ids = expr
                    .args
                    .iter()
                    .map(|arg| compile_expr(iseq, arg, locals_map))
                    .collect();
                iseq.push(Instr {
                    kind: InstrKind::Call {
                        receiver_id,
                        method_name: expr.method.clone(),
                        arg_ids,
                        private: expr.private,
                    },
                    range: expr.range,
                })
            }
        }
        Expr::Write(expr) => match &*expr.lhs {
            WriteTarget::LocalVariable(lhs) => {
                let local_id = locals_map.get(&lhs.name).copied().unwrap_or(0);
                let value_id = compile_expr(iseq, &expr.rhs, locals_map);
                iseq.push(Instr {
                    kind: InstrKind::WriteLocal { local_id, value_id },
                    range: expr.range,
                })
            }
            WriteTarget::Error(lhs) => iseq.push(Instr {
                kind: InstrKind::Error,
                range: lhs.range,
            }),
        },
        Expr::And(expr) => {
            let lhs_id = compile_expr(iseq, &expr.lhs, locals_map);
            compile_branch(
                iseq,
                BranchType::Truthy,
                lhs_id,
                |iseq| compile_expr(iseq, &expr.rhs, locals_map),
                |iseq| lhs_id,
                expr.range,
            )
        }
        Expr::Or(expr) => {
            let lhs_id = compile_expr(iseq, &expr.lhs, locals_map);
            compile_branch(
                iseq,
                BranchType::Truthy,
                lhs_id,
                |iseq| lhs_id,
                |iseq| compile_expr(iseq, &expr.rhs, locals_map),
                expr.range,
            )
        }
        Expr::If(expr) => {
            let cond_id = compile_expr(iseq, &expr.cond, locals_map);
            compile_branch(
                iseq,
                BranchType::Truthy,
                cond_id,
                |iseq| compile_expr(iseq, &expr.then, locals_map),
                |iseq| compile_expr(iseq, &expr.else_, locals_map),
                expr.range,
            )
        }
        Expr::While(expr) => compile_loop(iseq, false, &expr.cond, &expr.body, locals_map),
        Expr::Until(expr) => compile_loop(iseq, true, &expr.cond, &expr.body, locals_map),
        Expr::Error(expr) => iseq.push(Instr {
            kind: InstrKind::Error,
            range: expr.range,
        }),
    }
}

fn compile_loop(
    iseq: &mut ISeq,
    flip: bool,
    cond: &Expr,
    body: &Expr,
    locals_map: &HashMap<EString, usize>,
) -> usize {
    let (jump_into_loop_id, jump_into_loop_res) = iseq.reserve();

    let (loop_label_id, loop_label_res) = iseq.reserve();
    let cond_id = compile_expr(iseq, cond, &HashMap::new());
    let (branch_id, branch_res) = iseq.reserve();

    let body_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![branch_id],
        },
        range: *body.range(),
    });
    let body_id = compile_expr(iseq, body, locals_map);
    let body_jump_id = iseq.push(Instr {
        kind: InstrKind::JumpValue {
            to: loop_label_id,
            value_id: body_id,
        },
        range: *body.range(),
    });

    let end_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![branch_id],
        },
        range: *body.range(),
    });

    iseq.commit(
        jump_into_loop_res,
        Instr {
            kind: InstrKind::Jump { to: loop_label_id },
            range: *cond.range(),
        },
    );
    iseq.commit(
        loop_label_res,
        Instr {
            kind: InstrKind::Label {
                from: vec![jump_into_loop_id, body_jump_id],
            },
            range: *cond.range(),
        },
    );
    let (then_id, else_id) = if flip {
        (end_label_id, body_label_id)
    } else {
        (body_label_id, end_label_id)
    };
    iseq.commit(
        branch_res,
        Instr {
            kind: InstrKind::Branch {
                branch_type: BranchType::Truthy,
                cond_id,
                then_id,
                else_id,
            },
            range: *cond.range(),
        },
    );

    end_label_id
}

fn compile_branch<F1, F2>(
    iseq: &mut ISeq,
    branch_type: BranchType,
    cond_id: usize,
    gen_then: F1,
    gen_else: F2,
    range: CodeRange,
) -> usize
where
    F1: FnOnce(&mut ISeq) -> usize,
    F2: FnOnce(&mut ISeq) -> usize,
{
    let (branch_id, branch_res) = iseq.reserve();
    let then_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![branch_id],
        },
        range,
    });
    let then_id = gen_then(iseq);
    let (then_jump_id, then_jump_res) = iseq.reserve();
    let else_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![branch_id],
        },
        range,
    });
    let else_id = gen_else(iseq);
    let (else_jump_id, else_jump_res) = iseq.reserve();
    let end_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![then_jump_id, else_jump_id],
        },
        range,
    });
    iseq.commit(
        branch_res,
        Instr {
            kind: InstrKind::Branch {
                branch_type,
                cond_id,
                then_id: then_label_id,
                else_id: else_label_id,
            },
            range,
        },
    );
    iseq.commit(
        then_jump_res,
        Instr {
            kind: InstrKind::JumpValue {
                to: end_label_id,
                value_id: then_id,
            },
            range,
        },
    );
    iseq.commit(
        else_jump_res,
        Instr {
            kind: InstrKind::JumpValue {
                to: end_label_id,
                value_id: else_id,
            },
            range,
        },
    );
    end_label_id
}

#[cfg(test)]
mod tests {
    use crate::encoding::EStrRef;

    use super::*;

    #[allow(unused)]
    use pretty_assertions::{assert_eq, assert_ne};

    fn symbol(name: &str) -> EString {
        EString::from(name).asciified()
    }

    fn iseq_from_src(src: &str) -> ISeq {
        let mut diag = Vec::new();
        let program = crate::parse(&mut diag, EStrRef::from(src), &[]);
        assert_eq!(&*diag, &[]);
        let mut iseq = iseq_from_program(&program);
        for instr in &mut iseq.instructions {
            instr.range = DUMMY_RANGE;
        }
        iseq
    }

    fn i(kind: InstrKind) -> Instr {
        Instr {
            kind,
            range: DUMMY_RANGE,
        }
    }

    #[test]
    fn test_iseq_from_nil() {
        assert_eq!(
            iseq_from_src("nil"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstNil),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_false() {
        assert_eq!(
            iseq_from_src("false"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstFalse),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_true() {
        assert_eq!(
            iseq_from_src("true"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstTrue),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_integer() {
        assert_eq!(
            iseq_from_src("42"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstInteger { value: 42 }),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_local_variable() {
        assert_eq!(
            iseq_from_src("foo = 42; foo"),
            ISeq {
                num_locals: 2,
                instructions: vec![
                    i(InstrKind::LoadConstInteger { value: 42 }),
                    i(InstrKind::WriteLocal {
                        local_id: 1,
                        value_id: 0,
                    }),
                    i(InstrKind::ReadLocal { local_id: 1 }),
                    i(InstrKind::Return { value_id: 2 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_const() {
        assert_eq!(
            iseq_from_src("Object"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::ReadConst {
                        name: symbol("Object"),
                    }),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_relative_const() {
        assert_eq!(
            iseq_from_src("Gem::Version"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::ReadConst {
                        name: symbol("Gem"),
                    }),
                    i(InstrKind::ReadConstUnder {
                        name: symbol("Version"),
                        receiver_id: 0,
                    }),
                    i(InstrKind::Return { value_id: 1 }),
                ],
            }
        );
    }

    // #[test]
    // fn test_iseq_from_absolute_const() {
    //     assert_eq!(
    //         iseq_from_src("::Integer"),
    //         ISeq {
    //             num_locals: 1,
    //             instructions: vec![
    //                 i(InstrKind::LoadObjectClass),
    //                 i(InstrKind::ReadConstUnder {
    //                     name: symbol("Integer"),
    //                     receiver_id: 0,
    //                 }),
    //                 i(InstrKind::Return { value_id: 1 }),
    //             ],
    //         }
    //     );
    // }

    #[test]
    fn test_iseq_from_self() {
        assert_eq!(
            iseq_from_src("self"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::ReadLocal { local_id: 0 }),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_source_encoding() {
        assert_eq!(
            iseq_from_src("__ENCODING__"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadSourceEncoding),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_source_file() {
        assert_eq!(
            iseq_from_src("__FILE__"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadSourceFile),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_source_line() {
        assert_eq!(
            iseq_from_src("__LINE__"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadSourceLine),
                    i(InstrKind::Return { value_id: 0 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_func_call() {
        assert_eq!(
            iseq_from_src("puts 1"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::ReadLocal { local_id: 0 }),
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Call {
                        receiver_id: 0,
                        method_name: symbol("puts"),
                        arg_ids: vec![1],
                        private: true,
                    }),
                    i(InstrKind::Return { value_id: 2 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_optional_func_call() {
        assert_eq!(
            iseq_from_src("self&.puts 1"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::ReadLocal { local_id: 0 }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::NotNil,
                        cond_id: 0,
                        then_id: 2,
                        else_id: 6,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Call {
                        receiver_id: 0,
                        method_name: symbol("puts"),
                        arg_ids: vec![3],
                        private: true,
                    }),
                    i(InstrKind::JumpValue { to: 9, value_id: 4 }),
                    // Else...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::LoadConstNil),
                    i(InstrKind::JumpValue { to: 9, value_id: 7 }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![5, 8] }),
                    i(InstrKind::Return { value_id: 9 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_method_call() {
        assert_eq!(
            iseq_from_src("1.to_s"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Call {
                        receiver_id: 0,
                        method_name: symbol("to_s"),
                        arg_ids: vec![],
                        private: false,
                    }),
                    i(InstrKind::Return { value_id: 1 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_optional_method_call() {
        assert_eq!(
            iseq_from_src("1&.to_s"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::NotNil,
                        cond_id: 0,
                        then_id: 2,
                        else_id: 5,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::Call {
                        receiver_id: 0,
                        method_name: symbol("to_s"),
                        arg_ids: vec![],
                        private: false,
                    }),
                    i(InstrKind::JumpValue { to: 8, value_id: 3 }),
                    // Else...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::LoadConstNil),
                    i(InstrKind::JumpValue { to: 8, value_id: 6 }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![4, 7] }),
                    i(InstrKind::Return { value_id: 8 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_and() {
        assert_eq!(
            iseq_from_src("1 && 2"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 0,
                        then_id: 2,
                        else_id: 5,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::LoadConstInteger { value: 2 }),
                    i(InstrKind::JumpValue { to: 7, value_id: 3 }),
                    // Else...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::JumpValue { to: 7, value_id: 0 }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![4, 6] }),
                    i(InstrKind::Return { value_id: 7 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_or() {
        assert_eq!(
            iseq_from_src("1 || 2"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 0,
                        then_id: 2,
                        else_id: 4,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::JumpValue { to: 7, value_id: 0 }),
                    // Else...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::LoadConstInteger { value: 2 }),
                    i(InstrKind::JumpValue { to: 7, value_id: 5 }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![3, 6] }),
                    i(InstrKind::Return { value_id: 7 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_if() {
        assert_eq!(
            iseq_from_src("if 1; 2; else 3; end"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 0,
                        then_id: 2,
                        else_id: 5,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::LoadConstInteger { value: 2 }),
                    i(InstrKind::JumpValue { to: 8, value_id: 3 }),
                    // Else...
                    i(InstrKind::Label { from: vec![1] }),
                    i(InstrKind::LoadConstInteger { value: 3 }),
                    i(InstrKind::JumpValue { to: 8, value_id: 6 }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![4, 7] }),
                    i(InstrKind::Return { value_id: 8 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_while() {
        assert_eq!(
            iseq_from_src("while 1; 2; end"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::Jump { to: 1 }),
                    // Condition part
                    i(InstrKind::Label { from: vec![0, 6] }),
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 2,
                        then_id: 4,
                        else_id: 7,
                    }),
                    // Loop body
                    i(InstrKind::Label { from: vec![3] }),
                    i(InstrKind::LoadConstInteger { value: 2 }),
                    i(InstrKind::JumpValue { to: 1, value_id: 5 }),
                    // After the branch
                    i(InstrKind::Label { from: vec![3] }),
                    i(InstrKind::Return { value_id: 7 }),
                ],
            }
        );
    }

    #[test]
    fn test_iseq_from_until() {
        assert_eq!(
            iseq_from_src("until 1; 2; end"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::Jump { to: 1 }),
                    // Condition part
                    i(InstrKind::Label { from: vec![0, 6] }),
                    i(InstrKind::LoadConstInteger { value: 1 }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 2,
                        then_id: 7,
                        else_id: 4,
                    }),
                    // Loop body
                    i(InstrKind::Label { from: vec![3] }),
                    i(InstrKind::LoadConstInteger { value: 2 }),
                    i(InstrKind::JumpValue { to: 1, value_id: 5 }),
                    // After the branch
                    i(InstrKind::Label { from: vec![3] }),
                    i(InstrKind::Return { value_id: 7 }),
                ],
            }
        );
    }
}
