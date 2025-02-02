use std::{
    collections::{hash_map::Entry, BTreeMap, BTreeSet, HashMap},
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign},
    sync::LazyLock,
};

use crate::{
    ast::Program,
    iseq::{iseq_from_program, BranchType, ISeq, InstrKind, Var},
    liveness_analysis,
    util::get_many_mut,
    Diagnostic, EString,
};

pub fn typecheck_program(diag: &mut Vec<Diagnostic>, program: &Program) -> Type {
    let mut iseq = iseq_from_program(program);
    liveness_analysis(&mut iseq);
    typecheck_iseq(diag, &iseq)
}

fn typecheck_iseq(diag: &mut Vec<Diagnostic>, iseq: &ISeq) -> Type {
    let mut return_type = Type {
        union_of: BTreeSet::new(),
    };
    let mut flow_types = iseq
        .instructions
        .iter()
        .map(|_| FlowType {
            union_of: BTreeSet::new(),
        })
        .collect::<Vec<_>>();

    // The first instruction should be Entry
    flow_types[0].push(FlowTypeTerm {
        types: iseq.instructions[0]
            .live_in
            .iter()
            .map(|&var| (var, TypeTerm::NilClass))
            .collect(),
    });

    loop {
        let mut changed = false;
        for (expr_id, instr) in iseq.instructions.iter().enumerate() {
            match &instr.kind {
                InstrKind::Entry => {
                    let to_live_in = &iseq.instructions[expr_id + 1].live_in;
                    let [flow_type, to_flow_type] =
                        get_many_mut(&mut flow_types, [expr_id, expr_id + 1]).unwrap();
                    changed |= to_flow_type.push_mapped0(to_live_in, flow_type);
                }
                InstrKind::Label { from } => {
                    let to_live_in = &iseq.instructions[expr_id + 1].live_in;
                    let [flow_type, to_flow_type] =
                        get_many_mut(&mut flow_types, [expr_id, expr_id + 1]).unwrap();
                    changed |= to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |term| {
                        for &from in &from[..from.len() - 1] {
                            if let Some(ty) = term.types.get(&Var::Expr(from)) {
                                return *ty;
                            }
                        }
                        term.types[&Var::Expr(from[from.len() - 1])]
                    });
                }
                InstrKind::Return { value_id } => {
                    for flow_term in &flow_types[expr_id].union_of {
                        let value_type = flow_term.types[&Var::Expr(*value_id)];
                        return_type |= value_type.into();
                    }
                }
                InstrKind::Jump { to } => {
                    let to_live_in = &iseq.instructions[*to].live_in;
                    let [flow_type, to_flow_type] =
                        get_many_mut(&mut flow_types, [expr_id, *to]).unwrap();
                    changed |= to_flow_type
                        .push_mapped(to_live_in, flow_type, expr_id, |_| TypeTerm::NilClass);
                }
                InstrKind::JumpValue { to, value_id } => {
                    let to_live_in = &iseq.instructions[*to].live_in;
                    let [flow_type, to_flow_type] =
                        get_many_mut(&mut flow_types, [expr_id, *to]).unwrap();
                    changed |=
                        to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |from_term| {
                            from_term.types[&Var::Expr(*value_id)]
                        });
                }
                InstrKind::Branch {
                    branch_type,
                    cond_id,
                    then_id,
                    else_id,
                } => {
                    let then_live_in = &iseq.instructions[*then_id].live_in;
                    let else_live_in = &iseq.instructions[*else_id].live_in;
                    let [flow_type, then_flow_type, else_flow_type] =
                        get_many_mut(&mut flow_types, [expr_id, *then_id, *else_id]).unwrap();
                    // changed |=
                    //     to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |from_term| {
                    //         TypeTerm::NilClass
                    //     });
                    for flow_term in &flow_type.union_of {
                        let cond_type = flow_term.types[&Var::Expr(*cond_id)];
                        if cond_type == TypeTerm::NilClass
                            || (cond_type == TypeTerm::FalseClass
                                && *branch_type == BranchType::Truthy)
                        {
                            // "else" case
                            changed |= else_flow_type.push(FlowTypeTerm {
                                types: else_live_in
                                    .iter()
                                    .map(|&var| {
                                        (
                                            var,
                                            if var == Var::Expr(expr_id) {
                                                TypeTerm::NilClass
                                            } else {
                                                flow_term.types[&var]
                                            },
                                        )
                                    })
                                    .collect(),
                            });
                        } else {
                            // "then" case
                            changed |= then_flow_type.push(FlowTypeTerm {
                                types: then_live_in
                                    .iter()
                                    .map(|&var| {
                                        (
                                            var,
                                            if var == Var::Expr(expr_id) {
                                                TypeTerm::NilClass
                                            } else {
                                                flow_term.types[&var]
                                            },
                                        )
                                    })
                                    .collect(),
                            });
                        }
                    }
                }
                _ => {
                    let to_live_in = &iseq.instructions[expr_id + 1].live_in;
                    let [flow_type, to_flow_type] =
                        get_many_mut(&mut flow_types, [expr_id, expr_id + 1]).unwrap();
                    match &instr.kind {
                        InstrKind::Entry => unreachable!(),
                        InstrKind::Label { .. } => unreachable!(),
                        InstrKind::LoadConstNil => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    TypeTerm::NilClass
                                });
                        }
                        InstrKind::LoadConstTrue => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    TypeTerm::TrueClass
                                });
                        }
                        InstrKind::LoadConstFalse => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    TypeTerm::FalseClass
                                });
                        }
                        InstrKind::LoadConstInteger { value: _ } => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    TypeTerm::Integer
                                });
                        }
                        InstrKind::LoadConstString { value: _ } => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    TypeTerm::String
                                });
                        }
                        InstrKind::LoadSourceEncoding => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    // TODO: this is wrong assumption; use Encoding class
                                    TypeTerm::String
                                });
                        }
                        InstrKind::LoadSourceFile => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    TypeTerm::String
                                });
                        }
                        InstrKind::LoadSourceLine => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    TypeTerm::Integer
                                });
                        }
                        InstrKind::LoadObjectClass => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    // TODO: this is wrong assumption; use Class class
                                    TypeTerm::Class
                                });
                        }
                        InstrKind::ReadLocal { local_id } => {
                            changed |= to_flow_type.push_mapped(
                                to_live_in,
                                flow_type,
                                expr_id,
                                |from_term| from_term.types[&Var::Local(*local_id)],
                            );
                        }
                        InstrKind::WriteLocal { local_id, value_id } => {
                            for flow_term in &flow_type.union_of {
                                let value_type = flow_term.types[&Var::Expr(*value_id)];
                                changed |= to_flow_type.push(FlowTypeTerm {
                                    types: to_live_in
                                        .iter()
                                        .map(|&var| {
                                            (
                                                var,
                                                if var == Var::Local(*local_id) {
                                                    value_type
                                                } else {
                                                    flow_term.types[&var]
                                                },
                                            )
                                        })
                                        .collect(),
                                });
                            }
                        }
                        InstrKind::ReadConst { name: _ } => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    // TODO: impl const typing
                                    TypeTerm::NilClass
                                });
                        }
                        InstrKind::WriteConst {
                            name: _,
                            value_id: _,
                        } => {
                            // TODO: impl const typing
                            changed |= to_flow_type.push_mapped0(to_live_in, flow_type);
                        }
                        InstrKind::ReadConstUnder {
                            name: _,
                            receiver_id: _,
                        } => {
                            changed |=
                                to_flow_type.push_mapped(to_live_in, flow_type, expr_id, |_| {
                                    // TODO: impl const typing
                                    TypeTerm::NilClass
                                });
                        }
                        InstrKind::WriteConstUnder {
                            name: _,
                            receiver_id: _,
                            value_id: _,
                        } => {
                            // TODO: impl const typing
                            changed |= to_flow_type.push_mapped0(to_live_in, flow_type);
                        }
                        InstrKind::Call {
                            receiver_id,
                            method_name,
                            arg_ids,
                            // TODO: privacy check
                            private: _,
                        } => {
                            for flow_term in &flow_type.union_of {
                                let receiver_type = flow_term.types[&Var::Expr(*receiver_id)];
                                // TODO: cloning is inefficient
                                let method_name = method_name.clone();
                                let meth = Module::from_type_term(receiver_type)
                                    .ancestors()
                                    .into_iter()
                                    .find_map(|m| INSTANCE_METHODS.get(&(m, method_name.clone())));
                                let Some(meth) = meth else {
                                    diag.push(Diagnostic {
                                        message: format!(
                                            "undefined method `{:?}` for `{:?}`",
                                            method_name, receiver_type
                                        ),
                                        range: iseq.instructions[expr_id].range,
                                    });
                                    continue;
                                };
                                let mut return_type: Option<Type> = None;
                                for sig in &meth.intersection_of {
                                    if sig.param_types.len() != arg_ids.len() {
                                        continue;
                                    }
                                    let matched = arg_ids.iter().zip(&sig.param_types).all(
                                        |(&arg_id, arg_type)| {
                                            arg_type
                                                .union_of
                                                .contains(&flow_term.types[&Var::Expr(arg_id)])
                                        },
                                    );
                                    if !matched {
                                        continue;
                                    }
                                    if let Some(return_type) = &mut return_type {
                                        *return_type &= sig.return_type.clone();
                                    } else {
                                        return_type = Some(sig.return_type.clone());
                                    }
                                }
                                let Some(return_type) = return_type else {
                                    diag.push(Diagnostic {
                                        message: format!(
                                            "no matching signature for method `{:?}` for `{:?}`",
                                            method_name, receiver_type
                                        ),
                                        range: iseq.instructions[expr_id].range,
                                    });
                                    continue;
                                };
                                for return_type_term in &return_type.union_of {
                                    changed |= to_flow_type.push(FlowTypeTerm {
                                        types: to_live_in
                                            .iter()
                                            .map(|&var| {
                                                (
                                                    var,
                                                    if var == Var::Expr(expr_id) {
                                                        *return_type_term
                                                    } else {
                                                        flow_term.types[&var]
                                                    },
                                                )
                                            })
                                            .collect(),
                                    });
                                }
                            }
                        }
                        InstrKind::Return { .. } => unreachable!(),
                        InstrKind::Jump { .. } => unreachable!(),
                        InstrKind::JumpValue { .. } => unreachable!(),
                        InstrKind::Branch { .. } => unreachable!(),
                        InstrKind::Error => {
                            // Treat as no-op
                            changed |= to_flow_type.push_mapped0(to_live_in, flow_type);
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    return_type
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TypeTerm {
    NilClass,
    FalseClass,
    TrueClass,
    Integer,
    String,
    Class,
    Symbol,
    Regexp,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Type {
    union_of: BTreeSet<TypeTerm>,
}

impl From<TypeTerm> for Type {
    fn from(term: TypeTerm) -> Self {
        Type {
            union_of: [term].iter().copied().collect(),
        }
    }
}

impl BitAnd for Type {
    type Output = Type;
    fn bitand(mut self, rhs: Self) -> Self::Output {
        self &= rhs;
        self
    }
}

impl BitAndAssign for Type {
    fn bitand_assign(&mut self, rhs: Self) {
        self.union_of.retain(|term| rhs.union_of.contains(term));
    }
}

impl BitOr for Type {
    type Output = Type;
    fn bitor(self, rhs: Self) -> Self::Output {
        Type {
            union_of: self
                .union_of
                .iter()
                .copied()
                .chain(rhs.union_of.iter().copied())
                .collect(),
        }
    }
}

impl BitOrAssign for Type {
    fn bitor_assign(&mut self, rhs: Self) {
        for &term in &rhs.union_of {
            self.union_of.insert(term);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct FlowType {
    union_of: BTreeSet<FlowTypeTerm>,
}

impl FlowType {
    fn push(&mut self, term: FlowTypeTerm) -> bool {
        self.union_of.insert(term)
    }

    fn push_mapped0(&mut self, live_out: &[Var], base_type: &FlowType) -> bool {
        let mut changed = false;
        for base_term in &base_type.union_of {
            changed |= self.push(FlowTypeTerm {
                types: live_out
                    .iter()
                    .map(|&var| (var, base_term.types[&var]))
                    .collect(),
            })
        }
        changed
    }

    fn push_mapped(
        &mut self,
        live_out: &[Var],
        base_type: &FlowType,
        expr_id: usize,
        mut expr_type: impl FnMut(&FlowTypeTerm) -> TypeTerm,
    ) -> bool {
        let mut changed = false;
        for base_term in &base_type.union_of {
            changed |= self.push(FlowTypeTerm {
                types: live_out
                    .iter()
                    .map(|&var| {
                        (
                            var,
                            if var == Var::Expr(expr_id) {
                                expr_type(base_term)
                            } else {
                                base_term.types[&var]
                            },
                        )
                    })
                    .collect(),
            })
        }
        changed
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct FlowTypeTerm {
    types: BTreeMap<Var, TypeTerm>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MethodSignature {
    intersection_of: Vec<MethodSignatureFactor>,
}

impl From<MethodSignatureFactor> for MethodSignature {
    fn from(term: MethodSignatureFactor) -> Self {
        MethodSignature {
            intersection_of: vec![term],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MethodSignatureFactor {
    param_types: Vec<Type>,
    return_type: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Module {
    BasicObject,
    Kernel,
    Object,
    Module,
    Class,
    NilClass,
    FalseClass,
    TrueClass,
    Comparable,
    Numeric,
    Integer,
    String,
    Symbol,
    Regexp,
}

impl Module {
    fn from_type_term(term: TypeTerm) -> Module {
        match term {
            TypeTerm::NilClass => Module::NilClass,
            TypeTerm::FalseClass => Module::FalseClass,
            TypeTerm::TrueClass => Module::TrueClass,
            TypeTerm::Integer => Module::Integer,
            TypeTerm::String => Module::String,
            TypeTerm::Symbol => Module::Symbol,
            TypeTerm::Regexp => Module::Regexp,
            TypeTerm::Class => Module::Class,
        }
    }

    fn ancestors(&self) -> Vec<Module> {
        use self::Module;
        use Module::*;
        match self {
            BasicObject => vec![],
            Kernel => vec![Kernel],
            Object => vec![Object, Kernel, BasicObject],
            Module => vec![Module, Object, Kernel, BasicObject],
            Class => vec![Class, Module, Object, Kernel, BasicObject],
            NilClass => vec![NilClass, Object, Kernel, BasicObject],
            FalseClass => vec![FalseClass, Object, Kernel, BasicObject],
            TrueClass => vec![TrueClass, Object, Kernel, BasicObject],
            Comparable => vec![Comparable],
            Numeric => vec![Numeric, Comparable, Object, Kernel, BasicObject],
            Integer => vec![Integer, Numeric, Comparable, Object, Kernel, BasicObject],
            String => vec![String, Comparable, Object, Kernel, BasicObject],
            Symbol => vec![Symbol, Comparable, Object, Kernel, BasicObject],
            Regexp => vec![Regexp, Object, Kernel, BasicObject],
        }
    }
}

fn sig(
    m: Module,
    name: &str,
    arg_types: Vec<Type>,
    return_type: Type,
) -> ((Module, EString), MethodSignature) {
    let s = MethodSignatureFactor {
        param_types: arg_types,
        return_type,
    }
    .into();
    ((m, EString::from(name).asciified()), s)
}

fn union2(t1: TypeTerm, t2: TypeTerm) -> Type {
    Type {
        union_of: [t1, t2].iter().copied().collect(),
    }
}

fn opt(mut t: Type) -> Type {
    t.union_of.insert(TypeTerm::NilClass);
    t
}

static INSTANCE_METHODS: LazyLock<HashMap<(Module, EString), MethodSignature>> =
    LazyLock::new(|| {
        use Module as M;
        let never_ty = || Type {
            union_of: BTreeSet::new(),
        };
        let nil_ty = || -> Type { TypeTerm::NilClass.into() };
        let true_ty = || -> Type { TypeTerm::TrueClass.into() };
        let false_ty = || -> Type { TypeTerm::FalseClass.into() };
        let bool_ty = || true_ty() | false_ty();
        let class_ty = || -> Type { TypeTerm::Class.into() };
        let integer_ty = || -> Type { TypeTerm::Integer.into() };
        let string_ty = || -> Type { TypeTerm::String.into() };
        let symbol_ty = || -> Type { TypeTerm::Symbol.into() };
        // let regexp_ty = || -> Type { TypeTerm::Regexp.into() };
        let unknown_ty = || Type {
            union_of: [
                TypeTerm::NilClass,
                TypeTerm::FalseClass,
                TypeTerm::TrueClass,
                TypeTerm::Integer,
                TypeTerm::String,
                TypeTerm::Symbol,
                TypeTerm::Regexp,
            ]
            .iter()
            .copied()
            .collect(),
        };
        let sigs = vec![
            sig(M::BasicObject, "!", vec![], bool_ty()),
            sig(M::BasicObject, "!=", vec![unknown_ty()], bool_ty()),
            sig(M::BasicObject, "==", vec![unknown_ty()], bool_ty()),
            sig(M::BasicObject, "__id__", vec![], integer_ty()),
            // TODO: BasicObject#__send__
            sig(M::BasicObject, "equal?", vec![unknown_ty()], bool_ty()),
            // TODO: BasicObject#instance_eval
            // TODO: BasicObject#instance_exec
            // TODO: BasicObject#method_missing
            // TODO: BasicObject#singleton_method_added
            // TODO: BasicObject#singleton_method_removed
            // TODO: BasicObject#singleton_method_undefined
            // TODO: Kernel#Array
            // TODO: Kernel#Complex
            // TODO: Kernel#Float
            // TODO: Kernel#Hash
            // TODO: Kernel#Integer
            // TODO: Kernel#Rational
            sig(M::Kernel, "String", vec![string_ty()], string_ty()),
            sig(M::Kernel, "__callee__", vec![], opt(symbol_ty())),
            sig(M::Kernel, "__dir__", vec![], opt(string_ty())),
            sig(M::Kernel, "__method__", vec![], opt(symbol_ty())),
            sig(M::Kernel, "`", vec![], string_ty()),
            sig(M::Kernel, "abort", vec![], never_ty()),
            sig(M::Kernel, "abort", vec![string_ty()], never_ty()),
            // TODO: Kernel#at_exit
            // TODO: Kernel#autoload
            // TODO: Kernel#autoload?
            // TODO: Kernel#binding
            sig(M::Kernel, "block_given?", vec![], bool_ty()),
            sig(M::Kernel, "iterator?", vec![], bool_ty()),
            // TODO: Kernel#caller
            // TODO: Kernel#caller_locations
            // TODO: Kernel#catch
            sig(M::Kernel, "chomp", vec![], string_ty()),
            sig(M::Kernel, "chomp", vec![string_ty()], string_ty()),
            sig(M::Kernel, "chop", vec![], string_ty()),
            // TODO: Kernel#eval
            // TODO: Kernel#exec
            sig(M::Kernel, "exit", vec![], never_ty()),
            sig(M::Kernel, "exit", vec![bool_ty()], never_ty()),
            sig(M::Kernel, "exit!", vec![], never_ty()),
            sig(M::Kernel, "exit!", vec![bool_ty()], never_ty()),
            sig(M::Kernel, "fail", vec![], never_ty()),
            sig(M::Kernel, "fail", vec![string_ty()], never_ty()),
            // TODO: Kernel#fail with cause
            // TODO: Kernel#fail with error_type
            sig(M::Kernel, "raise", vec![], never_ty()),
            sig(M::Kernel, "raise", vec![string_ty()], never_ty()),
            // TODO: Kernel#raise with cause
            // TODO: Kernel#raise with error_type
            // TODO: Kernel#fork
            // TODO: Kernel#format
            // TODO: Kernel#sprintf
            sig(M::Kernel, "gets", vec![], opt(string_ty())),
            sig(M::Kernel, "gets", vec![string_ty()], opt(string_ty())),
            // TODO: Kernel#global_variables
            // TODO: Kernel#gsub
            // TODO: Kernel#lambda
            // TODO: Kernel#proc
            sig(M::Kernel, "load", vec![string_ty()], true_ty()),
            sig(M::Kernel, "load", vec![string_ty(), bool_ty()], true_ty()),
            // TODO: Kernel#local_variables
            // TODO: Kernel#loop
            // TODO: Kernel#open
            // TODO: Kernel#p
            // TODO: Kernel#pp
            // TODO: Kernel#print
            // TODO: Kernel#printf
            // TODO: Kernel#putc
            // TODO: Kernel#puts
            // TODO: Kernel#rand
            sig(M::Kernel, "readline", vec![], string_ty()),
            sig(M::Kernel, "readline", vec![string_ty()], string_ty()),
            // TODO: Kernel#readlines
            sig(M::Kernel, "require", vec![string_ty()], bool_ty()),
            sig(M::Kernel, "require_relative", vec![string_ty()], bool_ty()),
            // TODO: Kernel#select
            // TODO: Kernel#set_trace_func
            sig(M::Kernel, "sleep", vec![], integer_ty()),
            sig(M::Kernel, "sleep", vec![integer_ty()], integer_ty()),
            // TODO: Kernel#spawn
            sig(M::Kernel, "srand", vec![], integer_ty()),
            sig(M::Kernel, "srand", vec![integer_ty()], integer_ty()),
            // TODO: Kernel#sub
            // TODO: Kernel#syscall
            // TODO: Kernel#system
            // TODO: Kernel#test
            // TODO: Kernel#throw
            // TODO: Kernel#trace_var
            // TODO: Kernel#trap
            // TODO: Kernel#untrace_var
            // TODO: Kernel#warn
            sig(M::Object, "!~", vec![unknown_ty()], bool_ty()),
            sig(M::Object, "<=>", vec![unknown_ty()], opt(integer_ty())),
            sig(M::Object, "==", vec![unknown_ty()], bool_ty()),
            sig(M::Object, "===", vec![unknown_ty()], bool_ty()),
            // TODO: Object#_dump (phantom)
            sig(M::Object, "class", vec![], TypeTerm::Class.into()),
            // TODO: Object#clone
            // TODO: Object#dup
            // TODO: Object#define_singleton_method
            // TODO: Object#display
            // TODO: Object#to_enum
            // TODO: Object#enum_for
            sig(M::Object, "eql?", vec![unknown_ty()], bool_ty()),
            sig(M::Object, "equal?", vec![unknown_ty()], bool_ty()),
            // TODO: Object#extend
            // TODO: return value
            sig(M::Object, "freeze", vec![], unknown_ty()),
            sig(M::Object, "frozen?", vec![], bool_ty()),
            sig(M::Object, "hash", vec![], integer_ty()),
            sig(M::Object, "inspect", vec![], string_ty()),
            sig(
                M::Object,
                "instance_of?",
                vec![TypeTerm::Class.into()],
                bool_ty(),
            ),
            sig(
                M::Object,
                "instance_variable_defined?",
                vec![union2(TypeTerm::String, TypeTerm::Symbol)],
                bool_ty(),
            ),
            sig(
                M::Object,
                "instance_variable_get",
                vec![union2(TypeTerm::String, TypeTerm::Symbol)],
                unknown_ty(),
            ),
            sig(
                M::Object,
                "instance_variable_set",
                vec![union2(TypeTerm::String, TypeTerm::Symbol), unknown_ty()],
                unknown_ty(),
            ),
            // TODO: Object#instance_variables
            // TODO: generalize to Module
            sig(M::Object, "is_a?", vec![class_ty()], bool_ty()),
            sig(M::Object, "kind_of?", vec![class_ty()], bool_ty()),
            // TODO: self type
            sig(M::Object, "itself", vec![], unknown_ty()),
            // TODO: Object#marshal_dump (phantom)
            // TODO: Object#marshal_load (phantom)
            // TODO: Object#method
            // TODO: Object#methods
            // Note: NilClass/FalseClass overrides this
            sig(M::Object, "nil?", vec![], true_ty()),
            sig(M::Object, "object_id", vec![], integer_ty()),
            // TODO: Object#private_methods
            // TODO: Object#protected_methods
            // TODO: Object#public_method
            // TODO: Object#public_methods
            // TODO: Object#public_send
            sig(
                M::Object,
                "remove_instance_variable",
                vec![string_ty() | symbol_ty()],
                unknown_ty(),
            ),
            sig(
                M::Object,
                "respond_to?",
                vec![string_ty() | symbol_ty()],
                bool_ty(),
            ),
            sig(
                M::Object,
                "respond_to?",
                vec![string_ty() | symbol_ty(), bool_ty()],
                bool_ty(),
            ),
            // TODO: Object#send
            sig(M::Object, "singleton_class", vec![], TypeTerm::Class.into()),
            // TODO: Object#singleton_method
            // TODO: Object#singleton_methods
            // TODO: Object#tap
            // TODO: Object#yield_self
            // TODO: Object#then
            // TODO: Object#to_a (phantom)
            // TODO: Object#to_ary (phantom)
            // TODO: Object#to_hash (phantom)
            // TODO: Object#to_int (phantom)
            // TODO: Object#to_io (phantom)
            // TODO: Object#to_proc (phantom)
            // TODO: Object#to_regexp (phantom)
            // TODO: Object#to_s (phantom)
            // TODO: Object#to_str (phantom)
            // TODO: Object#initialize
            // TODO: Object#initialize_copy
            // TODO: Object#respond_to_missing?
            // TODO: Module methods
            // TODO: Class methods
            sig(M::NilClass, "!", vec![], true_ty()),
            sig(M::NilClass, "&", vec![unknown_ty()], false_ty()),
            sig(M::NilClass, "=~", vec![unknown_ty()], nil_ty()),
            sig(M::NilClass, "^", vec![unknown_ty()], bool_ty()),
            sig(M::NilClass, "|", vec![unknown_ty()], bool_ty()),
            sig(M::NilClass, "nil?", vec![], true_ty()),
            // TODO: NilClass#rationalize
            // TODO: NilClass#to_a
            // TODO: NilClass#to_c
            // TODO: NilClass#to_f
            // TODO: NilClass#to_h
            sig(M::NilClass, "to_i", vec![], integer_ty()),
            // TODO: NilClass#to_r
            sig(M::NilClass, "to_i", vec![], string_ty()),
            sig(M::FalseClass, "!", vec![], true_ty()),
            sig(M::FalseClass, "&", vec![unknown_ty()], false_ty()),
            sig(M::FalseClass, "^", vec![unknown_ty()], bool_ty()),
            sig(M::FalseClass, "|", vec![unknown_ty()], bool_ty()),
            sig(M::FalseClass, "to_s", vec![], string_ty()),
            sig(M::FalseClass, "inspect", vec![], string_ty()),
            sig(M::TrueClass, "&", vec![unknown_ty()], bool_ty()),
            sig(M::TrueClass, "^", vec![unknown_ty()], bool_ty()),
            sig(M::TrueClass, "|", vec![unknown_ty()], true_ty()),
            sig(M::TrueClass, "to_s", vec![], string_ty()),
            sig(M::TrueClass, "inspect", vec![], string_ty()),
            sig(M::Comparable, "<", vec![unknown_ty()], bool_ty()),
            sig(M::Comparable, "<=", vec![unknown_ty()], bool_ty()),
            sig(M::Comparable, "==", vec![unknown_ty()], bool_ty()),
            sig(M::Comparable, ">", vec![unknown_ty()], bool_ty()),
            sig(M::Comparable, ">=", vec![unknown_ty()], bool_ty()),
            sig(
                M::Comparable,
                "between?",
                vec![unknown_ty(), unknown_ty()],
                bool_ty(),
            ),
            // TODO: Comparable#clamp
            // TODO: generalize methods below to Numeric
            // TODO: self type
            sig(M::Integer, "+@", vec![], integer_ty()),
            sig(M::Integer, "-@", vec![], integer_ty()),
            sig(M::Integer, "~", vec![], integer_ty()),
            sig(M::Integer, "**", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "*", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "pow", vec![integer_ty()], integer_ty()),
            sig(
                M::Integer,
                "pow",
                vec![integer_ty(), integer_ty()],
                integer_ty(),
            ),
            sig(M::Integer, "/", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "%", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "modulo", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "+", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "-", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "&", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "|", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "^", vec![integer_ty()], integer_ty()),
            sig(M::Integer, ">>", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "<<", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "<=>", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "<", vec![integer_ty()], bool_ty()),
            sig(M::Integer, "<=", vec![integer_ty()], bool_ty()),
            sig(M::Integer, ">", vec![integer_ty()], bool_ty()),
            sig(M::Integer, ">=", vec![integer_ty()], bool_ty()),
            sig(M::Integer, "[]", vec![integer_ty()], integer_ty()),
            sig(
                M::Integer,
                "[]",
                vec![integer_ty(), integer_ty()],
                integer_ty(),
            ),
            // TODO: Integer#[](Range)
            sig(M::Integer, "abs", vec![], integer_ty()),
            sig(M::Integer, "magnitude", vec![], integer_ty()),
            sig(M::Integer, "abs2", vec![], integer_ty()),
            sig(M::Integer, "allbits?", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "anybits?", vec![integer_ty()], integer_ty()),
            // TODO: arg/angle/phase may return Float
            sig(M::Integer, "arg", vec![], integer_ty()),
            sig(M::Integer, "angle", vec![], integer_ty()),
            sig(M::Integer, "phase", vec![], integer_ty()),
            sig(M::Integer, "bit_length", vec![], integer_ty()),
            sig(M::Integer, "ceil", vec![], integer_ty()),
            sig(M::Integer, "ceil", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "ceildiv", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "chr", vec![], string_ty()),
            // TODO: Integer#chr(Encoding)
            // TODO: Numeric#coerce
            sig(M::Integer, "conj", vec![], integer_ty()),
            sig(M::Integer, "conjugate", vec![], integer_ty()),
            sig(M::Integer, "denominator", vec![], integer_ty()),
            // TODO: Integer#digits
            sig(M::Integer, "div", vec![integer_ty()], integer_ty()),
            // TODO: Numeric#divmod
            // TODO: Integer#downto
            sig(M::Integer, "eql?", vec![unknown_ty()], bool_ty()),
            sig(M::Integer, "even?", vec![], bool_ty()),
            // TODO: Numeric#fdiv
            sig(M::Integer, "finite?", vec![], true_ty()),
            sig(M::Integer, "floor", vec![], integer_ty()),
            sig(M::Integer, "floor", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "gcd", vec![integer_ty()], integer_ty()),
            // TODO: Integer#gcdlcm
            // TODO: Numeric#i
            sig(M::Integer, "imag", vec![], integer_ty()),
            sig(M::Integer, "imaginary", vec![], integer_ty()),
            sig(M::Integer, "inspect", vec![], string_ty()),
            sig(M::Integer, "inspect", vec![integer_ty()], string_ty()),
            sig(M::Integer, "to_s", vec![], string_ty()),
            sig(M::Integer, "to_s", vec![integer_ty()], string_ty()),
            sig(M::Integer, "infinite?", vec![], nil_ty()),
            sig(M::Integer, "integer?", vec![], true_ty()),
            sig(M::Integer, "lcm", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "negative?", vec![], bool_ty()),
            sig(M::Integer, "next", vec![], integer_ty()),
            sig(M::Integer, "succ", vec![], integer_ty()),
            sig(M::Integer, "nobits?", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "nonzero?", vec![], bool_ty()),
            sig(M::Integer, "numerator", vec![], integer_ty()),
            sig(M::Integer, "odd?", vec![], bool_ty()),
            sig(M::Integer, "ord", vec![], integer_ty()),
            // TODO: Numeric#polar
            sig(M::Integer, "positive?", vec![], bool_ty()),
            sig(M::Integer, "pred", vec![], integer_ty()),
            // TODO: Numeric#quo
            // TODO: Numeric#rationalize
            sig(M::Integer, "real", vec![], integer_ty()),
            sig(M::Integer, "real?", vec![], true_ty()),
            // TODO: Numeric#rect, Numeric#rectangular
            sig(M::Integer, "remainder", vec![integer_ty()], integer_ty()),
            sig(M::Integer, "round", vec![], integer_ty()),
            sig(M::Integer, "size", vec![], integer_ty()),
            // TODO: Numeric#step
            // TODO: Numeric#to_c
            // TODO: Numeric#to_f
            sig(M::Integer, "to_i", vec![], integer_ty()),
            sig(M::Integer, "to_int", vec![], integer_ty()),
            // TODO: Numeric#to_r
            sig(M::Integer, "truncate", vec![], integer_ty()),
            sig(M::Integer, "truncate", vec![integer_ty()], integer_ty()),
            // TODO: Integer#upto
            sig(M::Integer, "zero?", vec![], bool_ty()),
        ];

        let mut sig_map = HashMap::<(Module, EString), MethodSignature>::new();
        for (k, v) in sigs {
            match sig_map.entry(k) {
                Entry::Vacant(entry) => {
                    entry.insert(v);
                }
                Entry::Occupied(mut entry) => {
                    entry.get_mut().intersection_of.extend(v.intersection_of);
                }
            }
        }
        sig_map
    });

#[cfg(test)]
mod tests {
    use crate::{ast::pos_in, encoding::EStrRef, parse};

    use super::*;

    fn typecheck_program_text(s: &str) -> (Type, Vec<Diagnostic>) {
        let mut diag = Vec::new();
        let program = parse(&mut diag, EStrRef::from(s), &[]);
        let ty = typecheck_program(&mut diag, &program);
        (ty, diag)
    }

    fn integer_ty() -> Type {
        TypeTerm::Integer.into()
    }
    fn string_ty() -> Type {
        TypeTerm::String.into()
    }

    #[test]
    fn test_typecheck_integer() {
        assert_eq!(typecheck_program_text("42"), (integer_ty(), vec![]),);
    }

    #[test]
    fn test_typecheck_assignment() {
        assert_eq!(typecheck_program_text("x = 42; x"), (integer_ty(), vec![]),);
    }
}
