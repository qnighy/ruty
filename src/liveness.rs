use crate::{
    iseq::{ISeq, InstrKind, Var},
    util::get_many_mut,
};

pub(crate) fn liveness_analysis(iseq: &mut ISeq) {
    loop {
        let mut changed = false;

        for i in (0..iseq.instructions.len()).rev() {
            match iseq.instructions[i].kind {
                InstrKind::Return { value_id } => {
                    add_live_in(
                        &mut changed,
                        &mut iseq.instructions[i].live_in,
                        Var::Expr(value_id),
                    );
                }
                InstrKind::Jump { to } => {
                    let [instr, target_instr] =
                        get_many_mut(&mut *iseq.instructions, [i, to]).unwrap();
                    for &var in &target_instr.live_in {
                        add_live_in(&mut changed, &mut instr.live_in, var);
                    }
                }
                InstrKind::JumpValue { to, value_id } => {
                    let [instr, target_instr] =
                        get_many_mut(&mut *iseq.instructions, [i, to]).unwrap();
                    for &var in &target_instr.live_in {
                        add_live_in(&mut changed, &mut instr.live_in, var);
                    }
                    add_live_in(&mut changed, &mut instr.live_in, Var::Expr(value_id));
                }
                InstrKind::Branch {
                    branch_type: _,
                    cond_id,
                    then_id,
                    else_id,
                } => {
                    let [instr, then_instr, else_instr] =
                        get_many_mut(&mut *iseq.instructions, [i, then_id, else_id]).unwrap();
                    for &var in &then_instr.live_in {
                        add_live_in(&mut changed, &mut instr.live_in, var);
                    }
                    for &var in &else_instr.live_in {
                        add_live_in(&mut changed, &mut instr.live_in, var);
                    }
                    add_live_in(&mut changed, &mut instr.live_in, Var::Expr(cond_id));
                }
                _ => {
                    let expr_id = Var::Expr(i);
                    let [instr, next_instr] =
                        get_many_mut(&mut *iseq.instructions, [i, i + 1]).unwrap();
                    let mut reads_buf = Vec::new();
                    let (reads, writes): (&[Var], &[Var]) = match instr.kind {
                        InstrKind::Entry => (&[], &[]),
                        InstrKind::Label { from: _ } => (&[], &[]),
                        InstrKind::LoadConstNil => (&[], &[]),
                        InstrKind::LoadConstTrue => (&[], &[]),
                        InstrKind::LoadConstFalse => (&[], &[]),
                        InstrKind::LoadConstInteger { value: _ } => (&[], &[]),
                        InstrKind::LoadConstString { value: _ } => (&[], &[]),
                        InstrKind::LoadSourceEncoding => (&[], &[]),
                        InstrKind::LoadSourceFile => (&[], &[]),
                        InstrKind::LoadSourceLine => (&[], &[]),
                        InstrKind::LoadObjectClass => (&[], &[]),
                        InstrKind::ReadLocal { local_id } => (&[Var::Local(local_id)], &[]),
                        InstrKind::WriteLocal { local_id, value_id } => {
                            (&[Var::Expr(value_id)], &[Var::Local(local_id)])
                        }
                        InstrKind::ReadConst { name: _ } => (&[], &[]),
                        InstrKind::WriteConst { name: _, value_id } => {
                            (&[Var::Expr(value_id)], &[])
                        }
                        InstrKind::ReadConstUnder {
                            name: _,
                            receiver_id,
                        } => (&[Var::Expr(receiver_id)], &[]),
                        InstrKind::WriteConstUnder {
                            name: _,
                            receiver_id,
                            value_id,
                        } => (&[Var::Expr(receiver_id), Var::Expr(value_id)], &[]),
                        InstrKind::Call {
                            receiver_id,
                            method_name: _,
                            ref arg_ids,
                            private: _,
                        } => {
                            reads_buf.push(Var::Expr(receiver_id));
                            reads_buf.extend(arg_ids.iter().copied().map(Var::Expr));
                            (&reads_buf, &[])
                        }
                        InstrKind::Return { .. } => unreachable!(),
                        InstrKind::Jump { .. } => unreachable!(),
                        InstrKind::JumpValue { .. } => unreachable!(),
                        InstrKind::Branch { .. } => unreachable!(),
                        InstrKind::Error => (&[], &[]),
                    };
                    for &var in &next_instr.live_in {
                        if !writes.contains(&var) && var != expr_id {
                            add_live_in(&mut changed, &mut instr.live_in, var);
                        }
                    }
                    for &var in reads {
                        add_live_in(&mut changed, &mut instr.live_in, var);
                    }
                }
            };
        }

        if !changed {
            break;
        }
    }
}

fn add_live_in(changed: &mut bool, live_in: &mut Vec<Var>, var: Var) {
    if !live_in.contains(&var) {
        live_in.push(var);
        *changed = true;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::DUMMY_RANGE,
        encoding::EStrRef,
        iseq::{iseq_from_program, Instr},
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
            live_in: live_in.to_owned(),
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
}
