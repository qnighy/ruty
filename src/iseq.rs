//! Instruction sequence for typechecking.
//! It does not necessarily correspond to the bytecode format
//! of the same name in CRuby.

use std::collections::{BTreeSet, HashMap};

use num_bigint::BigInt;

use crate::{
    ast::{Arg, CodeRange, ConstReceiver, Expr, NumericValue, Program, WriteTarget, DUMMY_RANGE},
    Diagnostic, EString,
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
            live_in: BTreeSet::new(),
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
    pub(crate) live_in: BTreeSet<Var>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Var {
    Local(usize),
    Expr(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum InstrKind {
    Entry,
    Label {
        from: Vec<usize>,
    },
    LoadConstNil,
    LoadConstTrue,
    LoadConstFalse,
    LoadConstInteger {
        value: BigInt,
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

// Receives `diag` to report TODOs. Probably we do not need this once
// we have implemented all the features.
pub(crate) fn iseq_from_program(program: &Program, diag: &mut Vec<Diagnostic>) -> ISeq {
    let mut iseq = ISeq {
        num_locals: program.locals.len() + 1,
        instructions: Vec::new(),
    };
    let mut locals_map = HashMap::new();
    for (i, name) in program.locals.iter().enumerate() {
        locals_map.insert(name.clone(), i + 1);
    }
    iseq.push(Instr {
        kind: InstrKind::Entry,
        range: DUMMY_RANGE,
        live_in: BTreeSet::new(),
    });
    let result_id = compile_expr(&mut iseq, &program.body, &locals_map, diag);
    iseq.push(Instr {
        kind: InstrKind::Return {
            value_id: result_id,
        },
        range: *program.body.range(),
        live_in: BTreeSet::new(),
    });
    iseq
}

fn compile_expr(
    iseq: &mut ISeq,
    expr: &Expr,
    locals_map: &HashMap<EString, usize>,
    diag: &mut Vec<Diagnostic>,
) -> usize {
    match expr {
        Expr::Seq(expr) => {
            let mut last_id = usize::MAX;
            for stmt in &expr.statements {
                last_id = compile_expr(iseq, &stmt.expr, locals_map, diag);
            }
            last_id
        }
        Expr::Nil(expr) => iseq.push(Instr {
            kind: InstrKind::LoadConstNil,
            range: expr.range,
            live_in: BTreeSet::new(),
        }),
        Expr::False(expr) => iseq.push(Instr {
            kind: InstrKind::LoadConstFalse,
            range: expr.range,
            live_in: BTreeSet::new(),
        }),
        Expr::True(expr) => iseq.push(Instr {
            kind: InstrKind::LoadConstTrue,
            range: expr.range,
            live_in: BTreeSet::new(),
        }),
        Expr::Numeric(expr) => {
            if expr.imaginary {
                diag.push(Diagnostic {
                    range: expr.range,
                    message: "Iseq: Imaginary number is not implemented yet".to_owned(),
                });
                iseq.push(Instr {
                    kind: InstrKind::Error,
                    range: expr.range,
                    live_in: BTreeSet::new(),
                })
            } else {
                match expr.value {
                    NumericValue::Integer(ref value) => iseq.push(Instr {
                        kind: InstrKind::LoadConstInteger {
                            value: value.clone(),
                        },
                        range: expr.range,
                        live_in: BTreeSet::new(),
                    }),
                    NumericValue::Float(_) => {
                        diag.push(Diagnostic {
                            range: expr.range,
                            message: "Iseq: Float literal is not implemented yet".to_owned(),
                        });
                        iseq.push(Instr {
                            kind: InstrKind::Error,
                            range: expr.range,
                            live_in: BTreeSet::new(),
                        })
                    }
                    NumericValue::Rational(_, _) => {
                        diag.push(Diagnostic {
                            range: expr.range,
                            message: "Iseq: Rational literal is not implemented yet".to_owned(),
                        });
                        iseq.push(Instr {
                            kind: InstrKind::Error,
                            range: expr.range,
                            live_in: BTreeSet::new(),
                        })
                    }
                }
            }
        }
        Expr::String(expr) => {
            // TODO
            diag.push(Diagnostic {
                range: expr.range,
                message: "Iseq: String literal is not implemented yet".to_owned(),
            });
            iseq.push(Instr {
                kind: InstrKind::Error,
                range: expr.range,
                live_in: BTreeSet::new(),
            })
        }
        Expr::Regexp(expr) => {
            // TODO
            diag.push(Diagnostic {
                range: expr.range,
                message: "Iseq: Regexp literal is not implemented yet".to_owned(),
            });
            iseq.push(Instr {
                kind: InstrKind::Error,
                range: expr.range,
                live_in: BTreeSet::new(),
            })
        }
        Expr::XString(expr) => {
            // TODO
            diag.push(Diagnostic {
                range: expr.range,
                message: "Iseq: Backtick is not implemented yet".to_owned(),
            });
            iseq.push(Instr {
                kind: InstrKind::Error,
                range: expr.range,
                live_in: BTreeSet::new(),
            })
        }
        Expr::LocalVariable(expr) => {
            let local_id = locals_map.get(&expr.name).copied().unwrap_or(0);
            iseq.push(Instr {
                kind: InstrKind::ReadLocal { local_id },
                range: expr.range,
                live_in: BTreeSet::new(),
            })
        }
        Expr::Const(expr) => match &expr.receiver {
            ConstReceiver::Expr(receiver) => {
                let receiver_id = compile_expr(iseq, receiver, locals_map, diag);
                iseq.push(Instr {
                    kind: InstrKind::ReadConstUnder {
                        name: expr.name.clone(),
                        receiver_id,
                    },
                    range: expr.range,
                    live_in: BTreeSet::new(),
                })
            }
            ConstReceiver::None => iseq.push(Instr {
                kind: InstrKind::ReadConst {
                    name: expr.name.clone(),
                },
                range: expr.range,
                live_in: BTreeSet::new(),
            }),
            ConstReceiver::Object => {
                let klass_id = iseq.push(Instr {
                    kind: InstrKind::LoadObjectClass,
                    range: expr.range,
                    live_in: BTreeSet::new(),
                });
                iseq.push(Instr {
                    kind: InstrKind::ReadConstUnder {
                        name: expr.name.clone(),
                        receiver_id: klass_id,
                    },
                    range: expr.range,
                    live_in: BTreeSet::new(),
                })
            }
        },
        Expr::Self_(expr) => iseq.push(Instr {
            kind: InstrKind::ReadLocal { local_id: 0 },
            range: expr.range,
            live_in: BTreeSet::new(),
        }),
        Expr::SourceEncoding(expr) => iseq.push(Instr {
            kind: InstrKind::LoadSourceEncoding,
            range: expr.range,
            live_in: BTreeSet::new(),
        }),
        Expr::SourceFile(expr) => iseq.push(Instr {
            kind: InstrKind::LoadSourceFile,
            range: expr.range,
            live_in: BTreeSet::new(),
        }),
        Expr::SourceLine(expr) => iseq.push(Instr {
            kind: InstrKind::LoadSourceLine,
            range: expr.range,
            live_in: BTreeSet::new(),
        }),
        Expr::Call(expr) => {
            if expr.optional {
                let receiver_id = compile_expr(iseq, &expr.receiver, locals_map, diag);
                compile_branch(
                    iseq,
                    BranchType::NotNil,
                    receiver_id,
                    |iseq, diag| {
                        let arg_ids = expr
                            .args
                            .args
                            .iter()
                            .map(|Arg::Expr(arg)| compile_expr(iseq, &arg.expr, locals_map, diag))
                            .collect();
                        iseq.push(Instr {
                            kind: InstrKind::Call {
                                receiver_id,
                                method_name: expr.method.clone(),
                                arg_ids,
                                private: expr.private,
                            },
                            range: expr.range,
                            live_in: BTreeSet::new(),
                        })
                    },
                    |iseq, _diag| {
                        iseq.push(Instr {
                            kind: InstrKind::LoadConstNil,
                            range: expr.range,
                            live_in: BTreeSet::new(),
                        })
                    },
                    expr.range,
                    diag,
                )
            } else {
                let receiver_id = compile_expr(iseq, &expr.receiver, locals_map, diag);
                let arg_ids = expr
                    .args
                    .args
                    .iter()
                    .map(|Arg::Expr(arg)| compile_expr(iseq, &arg.expr, locals_map, diag))
                    .collect();
                iseq.push(Instr {
                    kind: InstrKind::Call {
                        receiver_id,
                        method_name: expr.method.clone(),
                        arg_ids,
                        private: expr.private,
                    },
                    range: expr.range,
                    live_in: BTreeSet::new(),
                })
            }
        }
        Expr::Write(expr) => match &*expr.lhs {
            WriteTarget::LocalVariable(lhs) => {
                let local_id = locals_map.get(&lhs.name).copied().unwrap_or(0);
                let value_id = compile_expr(iseq, &expr.rhs, locals_map, diag);
                iseq.push(Instr {
                    kind: InstrKind::WriteLocal { local_id, value_id },
                    range: expr.range,
                    live_in: BTreeSet::new(),
                })
            }
            WriteTarget::Error(lhs) => iseq.push(Instr {
                kind: InstrKind::Error,
                range: lhs.range,
                live_in: BTreeSet::new(),
            }),
        },
        Expr::And(expr) => {
            let lhs_id = compile_expr(iseq, &expr.lhs, locals_map, diag);
            compile_branch(
                iseq,
                BranchType::Truthy,
                lhs_id,
                |iseq, diag| compile_expr(iseq, &expr.rhs, locals_map, diag),
                |_iseq, _diag| lhs_id,
                expr.range,
                diag,
            )
        }
        Expr::Or(expr) => {
            let lhs_id = compile_expr(iseq, &expr.lhs, locals_map, diag);
            compile_branch(
                iseq,
                BranchType::Truthy,
                lhs_id,
                |_iseq, _diag| lhs_id,
                |iseq, diag| compile_expr(iseq, &expr.rhs, locals_map, diag),
                expr.range,
                diag,
            )
        }
        Expr::If(expr) => {
            let cond_id = compile_expr(iseq, &expr.cond, locals_map, diag);
            compile_branch(
                iseq,
                BranchType::Truthy,
                cond_id,
                |iseq, diag| compile_expr(iseq, &expr.then, locals_map, diag),
                |iseq, diag| compile_expr(iseq, &expr.else_, locals_map, diag),
                expr.range,
                diag,
            )
        }
        Expr::While(expr) => compile_loop(iseq, false, &expr.cond, &expr.body, locals_map, diag),
        Expr::Until(expr) => compile_loop(iseq, true, &expr.cond, &expr.body, locals_map, diag),
        Expr::Error(expr) => iseq.push(Instr {
            kind: InstrKind::Error,
            range: expr.range,
            live_in: BTreeSet::new(),
        }),
    }
}

fn compile_loop(
    iseq: &mut ISeq,
    flip: bool,
    cond: &Expr,
    body: &Expr,
    locals_map: &HashMap<EString, usize>,
    diag: &mut Vec<Diagnostic>,
) -> usize {
    let (jump_into_loop_id, jump_into_loop_res) = iseq.reserve();

    let (loop_label_id, loop_label_res) = iseq.reserve();
    let cond_id = compile_expr(iseq, cond, locals_map, diag);
    let (branch_id, branch_res) = iseq.reserve();

    let body_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![branch_id],
        },
        range: *body.range(),
        live_in: BTreeSet::new(),
    });
    let body_id = compile_expr(iseq, body, locals_map, diag);
    let body_jump_id = iseq.push(Instr {
        kind: InstrKind::JumpValue {
            to: loop_label_id,
            value_id: body_id,
        },
        range: *body.range(),
        live_in: BTreeSet::new(),
    });

    let end_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![branch_id],
        },
        range: *body.range(),
        live_in: BTreeSet::new(),
    });

    iseq.commit(
        jump_into_loop_res,
        Instr {
            kind: InstrKind::Jump { to: loop_label_id },
            range: *cond.range(),
            live_in: BTreeSet::new(),
        },
    );
    iseq.commit(
        loop_label_res,
        Instr {
            kind: InstrKind::Label {
                from: vec![jump_into_loop_id, body_jump_id],
            },
            range: *cond.range(),
            live_in: BTreeSet::new(),
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
            live_in: BTreeSet::new(),
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
    diag: &mut Vec<Diagnostic>,
) -> usize
where
    F1: FnOnce(&mut ISeq, &mut Vec<Diagnostic>) -> usize,
    F2: FnOnce(&mut ISeq, &mut Vec<Diagnostic>) -> usize,
{
    let (branch_id, branch_res) = iseq.reserve();
    let then_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![branch_id],
        },
        range,
        live_in: BTreeSet::new(),
    });
    let then_id = gen_then(iseq, diag);
    let (then_jump_id, then_jump_res) = iseq.reserve();
    let else_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![branch_id],
        },
        range,
        live_in: BTreeSet::new(),
    });
    let else_id = gen_else(iseq, diag);
    let (else_jump_id, else_jump_res) = iseq.reserve();
    let end_label_id = iseq.push(Instr {
        kind: InstrKind::Label {
            from: vec![then_jump_id, else_jump_id],
        },
        range,
        live_in: BTreeSet::new(),
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
            live_in: BTreeSet::new(),
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
            live_in: BTreeSet::new(),
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
            live_in: BTreeSet::new(),
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
        let mut diag = Vec::<Diagnostic>::new();
        let mut iseq = iseq_from_program(&program, &mut diag);
        assert_eq!(&*diag, &[]);
        for instr in &mut iseq.instructions {
            instr.range = DUMMY_RANGE;
        }
        iseq
    }

    fn i(kind: InstrKind) -> Instr {
        Instr {
            kind,
            range: DUMMY_RANGE,
            live_in: BTreeSet::new(),
        }
    }

    #[test]
    fn test_iseq_from_nil() {
        assert_eq!(
            iseq_from_src("nil"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstNil),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstFalse),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstTrue),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(42)
                    }),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(42)
                    }),
                    i(InstrKind::WriteLocal {
                        local_id: 1,
                        value_id: 1,
                    }),
                    i(InstrKind::ReadLocal { local_id: 1 }),
                    i(InstrKind::Return { value_id: 3 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::ReadConst {
                        name: symbol("Object"),
                    }),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::ReadConst {
                        name: symbol("Gem"),
                    }),
                    i(InstrKind::ReadConstUnder {
                        name: symbol("Version"),
                        receiver_id: 1,
                    }),
                    i(InstrKind::Return { value_id: 2 }),
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
    //                 i(InstrKind::Entry),
    //                 i(InstrKind::LoadObjectClass),
    //                 i(InstrKind::ReadConstUnder {
    //                     name: symbol("Integer"),
    //                     receiver_id: 1,
    //                 }),
    //                 i(InstrKind::Return { value_id: 2 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::ReadLocal { local_id: 0 }),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadSourceEncoding),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadSourceFile),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadSourceLine),
                    i(InstrKind::Return { value_id: 1 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::ReadLocal { local_id: 0 }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Call {
                        receiver_id: 1,
                        method_name: symbol("puts"),
                        arg_ids: vec![2],
                        private: true,
                    }),
                    i(InstrKind::Return { value_id: 3 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::ReadLocal { local_id: 0 }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::NotNil,
                        cond_id: 1,
                        then_id: 3,
                        else_id: 7,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Call {
                        receiver_id: 1,
                        method_name: symbol("puts"),
                        arg_ids: vec![4],
                        private: true,
                    }),
                    i(InstrKind::JumpValue {
                        to: 10,
                        value_id: 5
                    }),
                    // Else...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::LoadConstNil),
                    i(InstrKind::JumpValue {
                        to: 10,
                        value_id: 8
                    }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![6, 9] }),
                    i(InstrKind::Return { value_id: 10 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Call {
                        receiver_id: 1,
                        method_name: symbol("to_s"),
                        arg_ids: vec![],
                        private: false,
                    }),
                    i(InstrKind::Return { value_id: 2 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::NotNil,
                        cond_id: 1,
                        then_id: 3,
                        else_id: 6,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::Call {
                        receiver_id: 1,
                        method_name: symbol("to_s"),
                        arg_ids: vec![],
                        private: false,
                    }),
                    i(InstrKind::JumpValue { to: 9, value_id: 4 }),
                    // Else...
                    i(InstrKind::Label { from: vec![2] }),
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
    fn test_iseq_from_and() {
        assert_eq!(
            iseq_from_src("1 && 2"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 1,
                        then_id: 3,
                        else_id: 6,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(2),
                    }),
                    i(InstrKind::JumpValue { to: 8, value_id: 4 }),
                    // Else...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::JumpValue { to: 8, value_id: 1 }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![5, 7] }),
                    i(InstrKind::Return { value_id: 8 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 1,
                        then_id: 3,
                        else_id: 5,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::JumpValue { to: 8, value_id: 1 }),
                    // Else...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(2)
                    }),
                    i(InstrKind::JumpValue { to: 8, value_id: 6 }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![4, 7] }),
                    i(InstrKind::Return { value_id: 8 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 1,
                        then_id: 3,
                        else_id: 6,
                    }),
                    // Then...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(2),
                    }),
                    i(InstrKind::JumpValue { to: 9, value_id: 4 }),
                    // Else...
                    i(InstrKind::Label { from: vec![2] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(3),
                    }),
                    i(InstrKind::JumpValue { to: 9, value_id: 7 }),
                    // After the branch:
                    i(InstrKind::Label { from: vec![5, 8] }),
                    i(InstrKind::Return { value_id: 9 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::Jump { to: 2 }),
                    // Condition part
                    i(InstrKind::Label { from: vec![1, 7] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 3,
                        then_id: 5,
                        else_id: 8,
                    }),
                    // Loop body
                    i(InstrKind::Label { from: vec![4] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(2)
                    }),
                    i(InstrKind::JumpValue { to: 2, value_id: 6 }),
                    // After the branch
                    i(InstrKind::Label { from: vec![4] }),
                    i(InstrKind::Return { value_id: 8 }),
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
                    i(InstrKind::Entry),
                    i(InstrKind::Jump { to: 2 }),
                    // Condition part
                    i(InstrKind::Label { from: vec![1, 7] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(1)
                    }),
                    i(InstrKind::Branch {
                        branch_type: BranchType::Truthy,
                        cond_id: 3,
                        then_id: 8,
                        else_id: 5,
                    }),
                    // Loop body
                    i(InstrKind::Label { from: vec![4] }),
                    i(InstrKind::LoadConstInteger {
                        value: BigInt::from(2)
                    }),
                    i(InstrKind::JumpValue { to: 2, value_id: 6 }),
                    // After the branch
                    i(InstrKind::Label { from: vec![4] }),
                    i(InstrKind::Return { value_id: 8 }),
                ],
            }
        );
    }
}
