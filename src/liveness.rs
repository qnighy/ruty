use std::collections::BTreeSet;

use crate::{
    iseq::{ISeq, InstrKind, Var},
    util::get_many_mut,
};

pub(crate) fn liveness_analysis(iseq: &mut ISeq) {
    loop {
        let mut changed = false;

        for i in (0..iseq.instructions.len()).rev() {
            let expr_id = Var::Expr(i);
            match iseq.instructions[i].kind {
                InstrKind::Return { value_id } => {
                    add_live_in(
                        &mut changed,
                        &mut iseq.instructions[i].live_in,
                        Var::Expr(value_id),
                    );
                }
                InstrKind::Jump { to } => {
                    let label_id = Var::Expr(to);
                    let [instr, target_instr] =
                        get_many_mut(&mut *iseq.instructions, [i, to]).unwrap();
                    for &var in &target_instr.live_in {
                        if var == label_id {
                            // label_id is a special value which we should
                            // reinterpret as expr_id.
                            // And for Jump, value for expr_id is constant nil
                            // which means there is no liveness propagation here.
                        } else {
                            add_live_in(&mut changed, &mut instr.live_in, var);
                        }
                    }
                }
                InstrKind::JumpValue { to, value_id } => {
                    let label_id = Var::Expr(to);
                    let [instr, target_instr] =
                        get_many_mut(&mut *iseq.instructions, [i, to]).unwrap();
                    for &var in &target_instr.live_in {
                        if var == label_id {
                            // label_id is a special value which we should
                            // reinterpret as expr_id.
                            // And for JumpValue, the value for expr_id
                            // is determined from the value for value_id.
                            add_live_in(&mut changed, &mut instr.live_in, Var::Expr(value_id));
                        } else {
                            add_live_in(&mut changed, &mut instr.live_in, var);
                        }
                    }
                }
                InstrKind::Branch {
                    branch_type: _,
                    cond_id,
                    then_id,
                    else_id,
                } => {
                    let then_label_id = Var::Expr(then_id);
                    let else_label_id = Var::Expr(else_id);
                    let [instr, then_instr, else_instr] =
                        get_many_mut(&mut *iseq.instructions, [i, then_id, else_id]).unwrap();
                    for &var in &then_instr.live_in {
                        if var == then_label_id {
                            // nothing
                        } else {
                            add_live_in(&mut changed, &mut instr.live_in, var);
                        }
                    }
                    for &var in &else_instr.live_in {
                        if var == else_label_id {
                            // nothing
                        } else {
                            add_live_in(&mut changed, &mut instr.live_in, var);
                        }
                    }
                    add_live_in(&mut changed, &mut instr.live_in, Var::Expr(cond_id));
                }
                _ => {
                    let [instr, next_instr] =
                        get_many_mut(&mut *iseq.instructions, [i, i + 1]).unwrap();
                    match instr.kind {
                        InstrKind::Entry
                        | InstrKind::LoadConstNil
                        | InstrKind::LoadConstTrue
                        | InstrKind::LoadConstFalse
                        | InstrKind::LoadConstInteger { value: _ }
                        | InstrKind::LoadConstString { value: _ }
                        | InstrKind::LoadSourceEncoding
                        | InstrKind::LoadSourceFile
                        | InstrKind::LoadSourceLine
                        | InstrKind::LoadObjectClass
                        | InstrKind::Error
                        | InstrKind::ReadConst { name: _ } => {
                            for &var in &next_instr.live_in {
                                if var != expr_id {
                                    add_live_in(&mut changed, &mut instr.live_in, var);
                                }
                            }
                        }
                        InstrKind::WriteConst {
                            name: _,
                            value_id: input_expr_id,
                        }
                        | InstrKind::ReadConstUnder {
                            name: _,
                            receiver_id: input_expr_id,
                        } => {
                            for &var in &next_instr.live_in {
                                if var != expr_id {
                                    add_live_in(&mut changed, &mut instr.live_in, var);
                                }
                            }
                            add_live_in(&mut changed, &mut instr.live_in, Var::Expr(input_expr_id));
                        }
                        InstrKind::WriteConstUnder {
                            name: _,
                            receiver_id: input_expr1_id,
                            value_id: input_expr2_id,
                        } => {
                            for &var in &next_instr.live_in {
                                if var != expr_id {
                                    add_live_in(&mut changed, &mut instr.live_in, var);
                                }
                            }
                            add_live_in(
                                &mut changed,
                                &mut instr.live_in,
                                Var::Expr(input_expr1_id),
                            );
                            add_live_in(
                                &mut changed,
                                &mut instr.live_in,
                                Var::Expr(input_expr2_id),
                            );
                        }
                        InstrKind::ReadLocal { local_id } => {
                            for &var in &next_instr.live_in {
                                if var == expr_id {
                                    add_live_in(
                                        &mut changed,
                                        &mut instr.live_in,
                                        Var::Local(local_id),
                                    );
                                } else {
                                    add_live_in(&mut changed, &mut instr.live_in, var);
                                }
                            }
                        }
                        InstrKind::WriteLocal { local_id, value_id } => {
                            for &var in &next_instr.live_in {
                                if var == expr_id || var == Var::Local(local_id) {
                                    add_live_in(
                                        &mut changed,
                                        &mut instr.live_in,
                                        Var::Expr(value_id),
                                    );
                                } else {
                                    add_live_in(&mut changed, &mut instr.live_in, var);
                                }
                            }
                        }
                        InstrKind::Call {
                            receiver_id,
                            method_name: _,
                            ref arg_ids,
                            private: _,
                        } => {
                            for &var in &next_instr.live_in {
                                if var != expr_id {
                                    add_live_in(&mut changed, &mut instr.live_in, var);
                                }
                            }
                            add_live_in(&mut changed, &mut instr.live_in, Var::Expr(receiver_id));
                            for &arg_id in arg_ids {
                                add_live_in(&mut changed, &mut instr.live_in, Var::Expr(arg_id));
                            }
                        }
                        InstrKind::Label { from: _ } => {
                            // Special case: label's live_in may contain reference to
                            // the instruction itself (i.e. Var::Expr(i))
                            // whose actually meaning is any of the branch/jump
                            // instruction targeting the label.
                            for &var in &next_instr.live_in {
                                add_live_in(&mut changed, &mut instr.live_in, var);
                            }
                        }

                        InstrKind::Return { .. } => unreachable!(),
                        InstrKind::Jump { .. } => unreachable!(),
                        InstrKind::JumpValue { .. } => unreachable!(),
                        InstrKind::Branch { .. } => unreachable!(),
                    };
                }
            };
        }

        if !changed {
            break;
        }
    }
}

fn add_live_in(changed: &mut bool, live_in: &mut BTreeSet<Var>, var: Var) {
    if !live_in.contains(&var) {
        live_in.insert(var);
        *changed = true;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::DUMMY_RANGE,
        encoding::EStrRef,
        iseq::{iseq_from_program, BranchType, Instr},
    };

    use super::*;

    #[allow(unused)]
    use pretty_assertions::{assert_eq, assert_ne};

    fn iseq_from_src(src: &str) -> ISeq {
        let mut diag = Vec::new();
        let program = crate::parse(&mut diag, EStrRef::from(src), &[]);
        assert_eq!(&*diag, &[]);
        let mut iseq = iseq_from_program(&program);
        for instr in &mut iseq.instructions {
            instr.range = DUMMY_RANGE;
        }
        liveness_analysis(&mut iseq);
        iseq
    }

    fn i(kind: InstrKind, live_in: &[Var]) -> Instr {
        Instr {
            kind,
            range: DUMMY_RANGE,
            live_in: live_in.iter().copied().collect(),
        }
    }

    #[test]
    fn test_liveness_from_local_variable() {
        assert_eq!(
            iseq_from_src("foo = 42; foo"),
            ISeq {
                num_locals: 2,
                instructions: vec![
                    i(InstrKind::Entry, &[]),
                    i(InstrKind::LoadConstInteger { value: 42 }, &[]),
                    i(
                        InstrKind::WriteLocal {
                            local_id: 1,
                            value_id: 1,
                        },
                        &[Var::Expr(1)]
                    ),
                    i(InstrKind::ReadLocal { local_id: 1 }, &[Var::Local(1)]),
                    i(InstrKind::Return { value_id: 3 }, &[Var::Expr(3)]),
                ],
            }
        );
    }

    #[test]
    fn test_used_and_unused_lv() {
        assert_eq!(
            iseq_from_src("foo = 42; bar = foo; bar"),
            ISeq {
                num_locals: 3,
                instructions: vec![
                    i(InstrKind::Entry, &[]),
                    i(InstrKind::LoadConstInteger { value: 42 }, &[]),
                    i(
                        InstrKind::WriteLocal {
                            local_id: 1,
                            value_id: 1,
                        },
                        &[Var::Expr(1)]
                    ),
                    i(InstrKind::ReadLocal { local_id: 1 }, &[Var::Local(1)]),
                    i(
                        InstrKind::WriteLocal {
                            local_id: 2,
                            value_id: 3,
                        },
                        &[Var::Expr(3)]
                    ),
                    i(InstrKind::ReadLocal { local_id: 2 }, &[Var::Local(2)]),
                    i(InstrKind::Return { value_id: 5 }, &[Var::Expr(5)]),
                ],
            }
        );
        assert_eq!(
            iseq_from_src("foo = 42; bar = foo; 42"),
            ISeq {
                num_locals: 3,
                instructions: vec![
                    i(InstrKind::Entry, &[]),
                    i(InstrKind::LoadConstInteger { value: 42 }, &[]),
                    i(
                        InstrKind::WriteLocal {
                            local_id: 1,
                            value_id: 1,
                        },
                        &[]
                    ),
                    i(InstrKind::ReadLocal { local_id: 1 }, &[]),
                    i(
                        InstrKind::WriteLocal {
                            local_id: 2,
                            value_id: 3,
                        },
                        &[]
                    ),
                    i(InstrKind::LoadConstInteger { value: 42 }, &[]),
                    i(InstrKind::Return { value_id: 5 }, &[Var::Expr(5)]),
                ],
            }
        );
    }

    #[test]
    fn test_cond() {
        assert_eq!(
            iseq_from_src("1 ? 2 : 3"),
            ISeq {
                num_locals: 1,
                instructions: vec![
                    i(InstrKind::Entry, &[]),
                    i(InstrKind::LoadConstInteger { value: 1 }, &[]),
                    i(
                        InstrKind::Branch {
                            branch_type: BranchType::Truthy,
                            cond_id: 1,
                            then_id: 3,
                            else_id: 6,
                        },
                        &[Var::Expr(1)]
                    ),
                    i(InstrKind::Label { from: vec![2] }, &[]),
                    i(InstrKind::LoadConstInteger { value: 2 }, &[]),
                    i(InstrKind::JumpValue { to: 9, value_id: 4 }, &[Var::Expr(4)]),
                    i(InstrKind::Label { from: vec![2] }, &[]),
                    i(InstrKind::LoadConstInteger { value: 3 }, &[]),
                    i(InstrKind::JumpValue { to: 9, value_id: 7 }, &[Var::Expr(7)]),
                    i(InstrKind::Label { from: vec![5, 8] }, &[Var::Expr(9)]),
                    i(InstrKind::Return { value_id: 9 }, &[Var::Expr(9)]),
                ],
            }
        );
    }
}
