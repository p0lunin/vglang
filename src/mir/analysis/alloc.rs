use crate::mir::ctx::{Ctx, Fid};
use crate::mir::{
    Assigment, Case, DiscriminantOf, Function, Location, Statement, Variable, Vid, Vty, VtyKind,
};
use itertools::Itertools;
use smallvec::smallvec;
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};

macro_rules! hash_set {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_set = HashSet::new();
            $(
                temp_set.insert($x);
            )*
            temp_set
        }
    };
}

pub fn insert_heap_allocs(ctx: Ctx) -> Ctx {
    todo!()
}

fn insert_heap_allocs_fn(
    mut fun: Function,
    usages: &[FuncUsageResult],
    ctx: &Ctx,
) -> (Function, FuncUsageResult) {
    let (stmts, vars, usage) =
        insert_heap_allocs_stmts_first(fun.args.len() as u8, fun.vars, fun.stmts, usages, ctx);
    fun.stmts = stmts;
    fun.vars = vars;
    (fun, usage)
}

fn insert_heap_allocs_stmts_first(
    fc_args_count: u8,
    mut initial_vars: Vec<Vty>,
    initial_statements: Vec<Statement>,
    usages: &[FuncUsageResult],
    ctx: &Ctx,
) -> (Vec<Statement>, Vec<Vty>, FuncUsageResult) {
    let mut allocated = HashSet::new();
    let mut refs = HashMap::new();
    for id in 0..fc_args_count as usize {
        if initial_vars[id].must_be_deallocated(ctx) {
            refs.insert(Vid(id), HashSet::with_capacity(10));
        }
    }
    let (stmts, usage) = insert_heap_allocs_stmts(
        fc_args_count,
        &mut initial_vars,
        initial_statements,
        &mut allocated,
        &mut refs,
        usages,
        ctx,
    );
    (stmts, initial_vars, usage)
}

fn insert_heap_allocs_stmts(
    fc_args_count: u8,
    vars: &mut Vec<Vty>,
    mut stmts: Vec<Statement>,
    allocated: &mut HashSet<Vid>,
    refs: &mut HashMap<Vid, HashSet<Vid>>,
    usages: &[FuncUsageResult],
    ctx: &Ctx,
) -> (Vec<Statement>, FuncUsageResult) {
    let mut out = vec![];
    let mut push_variable = |v: Variable, ass: Assigment| out.push(Statement::Variable(v, ass));

    for i in 0..stmts.len() {
        match stmts[i].clone() {
            Statement::Variable(v, ass) => {
                match ass {
                    Assigment::Alloc(_) => {
                        allocated.insert(v.id);
                        refs.insert(v.id, HashSet::new());

                        push_variable(v, ass);
                    }
                    Assigment::Map(_, _, ref m) => {
                        let mut to_insert = HashSet::new();
                        for (reff, _) in refs.iter() {
                            if m.contains(reff) {
                                to_insert.insert(*reff);
                            }
                        }
                        if to_insert.len() > 0 {
                            refs.insert(v.id, to_insert);
                        }

                        push_variable(v, ass);
                    }
                    Assigment::Discriminant(_) => {
                        /* discriminant does not create refs */
                        push_variable(v, ass);
                    }
                    Assigment::BinOp(_, _, _) => {
                        /* binop does not create refs */
                        push_variable(v, ass);
                    }
                    Assigment::Function(_) => {
                        /* function assigment does not create refs */
                        push_variable(v, ass);
                    }
                    Assigment::Field { .. } => {
                        /* we cannot get field from the pointer */
                        // TODO: we can get heaped field so we must check this also
                        push_variable(v, ass);
                    }
                    Assigment::Call(func, arguments) => {
                        let usage = &usages[func.inner()];
                        debug_assert_eq!(usage.arguments_usage.len(), arguments.len());

                        let mut new_arguments = Vec::with_capacity(arguments.len());
                        let mut this_offset = 0;
                        usage.arguments_usage.iter().zip(arguments.iter()).for_each(
                            |(usage, arg)| {
                                match usage {
                                    ArgumentUsage::Used => {
                                        new_arguments.push(*arg);
                                    }
                                    ArgumentUsage::Unknown => {
                                        // TODO: it would be better if function to which pointer is
                                        // passed will clone the values itself. In this case we do
                                        // not clone value if it isn't necessary.

                                        let cloned_vid = Vid(arg.0 + this_offset + 1);
                                        debug_assert_ne!(vars[arg.0].location, Location::Stack);
                                        push_variable(
                                            Variable::new(vars[arg.0].clone(), cloned_vid),
                                            Assigment::Clone(*arg),
                                        );
                                        this_offset += 1;
                                        new_arguments.push(cloned_vid);
                                        vars.insert(cloned_vid.0, vars[arg.0].clone());
                                        inc_statements_vids(&mut stmts[i..], v.id.0 + this_offset);
                                    }
                                    ArgumentUsage::NotUsed => new_arguments.push(*arg),
                                }
                            },
                        );
                        let return_vid = Vid(v.id.0 + this_offset);
                        if v.ty.must_be_deallocated(ctx) {
                            allocated.insert(return_vid);
                            let used_values = usage
                                .arguments_usage
                                .iter()
                                .zip(new_arguments.iter())
                                .filter_map(|(usage, vid)| match usage {
                                    ArgumentUsage::Used => {
                                        if vars[vid.0].must_be_deallocated(ctx) {
                                            Some(*vid)
                                        } else {
                                            None
                                        }
                                    }
                                    _ => None,
                                })
                                .collect::<Vec<_>>();
                            for used in &used_values {
                                allocated.remove(&used);
                                remove_refs_with_vid_as_leaf(refs, *used);
                            }
                            refs.insert(return_vid, used_values.into_iter().collect());
                        }
                        push_variable(
                            Variable::new(v.ty, return_vid),
                            Assigment::Call(func, new_arguments),
                        );
                    }
                    Assigment::Dereference(_) => {
                        /* dereference does not create refs */
                        push_variable(v, ass);
                    }
                    Assigment::Value(_) => {
                        push_variable(v, ass);
                    }
                    Assigment::Clone(_) => {
                        unreachable!("There cannot be clone before this pass.");
                    }
                }
            }
            Statement::Cases { matched, cases } => {
                let mut cases_usages = vec![];
                let new_cases = cases
                    .into_iter()
                    .map(|mut case| {
                        let Case {
                            pattern,
                            vars: mut case_vars,
                            stmts: case_stmts,
                        } = case;
                        let vars_count = vars.len();

                        vars.extend(case_vars);
                        let (stmts, usage) = insert_heap_allocs_stmts(
                            fc_args_count,
                            vars,
                            case_stmts,
                            &mut allocated.clone(),
                            &mut refs.clone(),
                            usages,
                            ctx,
                        );
                        cases_usages.push(usage);
                        let new_vars = vars.drain(vars_count..).collect();
                        Case {
                            pattern,
                            vars: new_vars,
                            stmts,
                        }
                    })
                    .collect();
                out.push(Statement::Cases {
                    matched,
                    cases: new_cases,
                });
                let usage = merge_func_usages(cases_usages);
                return (out, usage);
            }
            Statement::Return(v) => {
                let args_depends = if refs.contains_key(&v) {
                    let leaves = {
                        let mut leaves = SmallVec::new();
                        find_leaves(&refs, v, &mut leaves);
                        leaves
                    };
                    let args_depends = {
                        let mut depends = SmallVec::<[u8; 8]>::new();
                        for leaf in leaves.iter() {
                            if leaf.0 < fc_args_count as usize {
                                // That means return value depends from the argument
                                depends.push(leaf.0 as u8)
                            } else {
                                allocated.remove(leaf);
                                remove_refs_with_vid_as_leaf(refs, *leaf);
                            }
                        }
                        depends
                    };
                    args_depends
                } else {
                    smallvec![]
                };

                let mut deallocs = vec![];

                let arguments_usage = {
                    let mut usage = smallvec![ArgumentUsage::NotUsed; fc_args_count as usize];

                    for v in args_depends {
                        usage[v as usize] = ArgumentUsage::Used;
                    }
                    for (i, arg) in usage.iter().enumerate() {
                        if *arg == ArgumentUsage::NotUsed && vars[i].location != Location::Stack {
                            deallocs.push(Vid(i));
                        }
                    }
                    usage
                };
                for allocated_var in allocated.iter() {
                    deallocs.push(*allocated_var);
                }
                deallocs.sort_by(|x, y| y.cmp(x));
                deallocs
                    .into_iter()
                    .for_each(|x| out.push(Statement::Dealloc(x)));
                let usage_result = FuncUsageResult { arguments_usage };
                out.push(Statement::Return(v));
                return (out, usage_result); // return statement means there are no more statements
            }
            Statement::Dealloc(_) => {
                // TODO: allow user to call dealloc by itself, like for early dispose.
                unreachable!("There are no expected to have dealloc before this pass.")
            }
        }
    }
    unreachable!("There are must be return statement in the end of the statements.")
}

fn merge_func_usages(usages: Vec<FuncUsageResult>) -> FuncUsageResult {
    usages
        .into_iter()
        .fold1(|acc, usage| FuncUsageResult {
            arguments_usage: acc
                .arguments_usage
                .into_iter()
                .zip(usage.arguments_usage.into_iter())
                .map(|(acc, usage)| match acc {
                    ArgumentUsage::Unknown => ArgumentUsage::Unknown,
                    ArgumentUsage::Used => match usage {
                        ArgumentUsage::NotUsed | ArgumentUsage::Unknown => ArgumentUsage::Unknown,
                        ArgumentUsage::Used => ArgumentUsage::Used,
                    },
                    ArgumentUsage::NotUsed => match usage {
                        ArgumentUsage::Used | ArgumentUsage::Unknown => ArgumentUsage::Unknown,
                        ArgumentUsage::NotUsed => ArgumentUsage::NotUsed,
                    },
                })
                .collect(),
        })
        .unwrap()
}

fn inc_statements_vids(stmts: &mut [Statement], from: usize) {
    let inc_vid = |vid: &mut Vid| {
        if vid.0 >= from {
            vid.0 += 1;
        }
    };
    for stmt in stmts {
        match stmt {
            Statement::Variable(v, x) => {
                inc_vid(&mut v.id);
                match x {
                    Assigment::Function(_) => {}
                    Assigment::Discriminant(d) => match d {
                        DiscriminantOf::Variable(v) => inc_vid(v),
                        _ => {}
                    },
                    Assigment::Field { var, .. } => inc_vid(var),
                    Assigment::Map(_, v, vids) => {
                        inc_vid(v);
                        vids.iter_mut().for_each(|v| inc_vid(v));
                    }
                    Assigment::Call(_, vids) => {
                        vids.iter_mut().for_each(|v| inc_vid(v));
                    }
                    Assigment::Alloc(vid) => inc_vid(vid),
                    Assigment::BinOp(_, _, _) => {
                        unimplemented!()
                    }
                    Assigment::Dereference(vid) => inc_vid(vid),
                    Assigment::Value(_) => {}
                    Assigment::Clone(vid) => inc_vid(vid),
                }
            }
            Statement::Cases { matched, cases } => {
                inc_vid(matched);
                for case in cases {
                    inc_statements_vids(&mut case.stmts, from);
                }
            }
            Statement::Return(vid) => inc_vid(vid),
            Statement::Dealloc(vid) => inc_vid(vid),
        }
    }
}

fn remove_refs_with_vid_as_leaf(map: &mut HashMap<Vid, HashSet<Vid>>, leaf: Vid) {
    let mut to_remove = SmallVec::<[Vid; 10]>::new();
    to_remove.push(leaf);
    let mut i = 0_usize;
    loop {
        let must_be_remove = to_remove[i];
        map.remove(&must_be_remove);
        map.iter_mut().for_each(|(x, refs)| {
            if refs.remove(&must_be_remove) {
                if refs.len() == 0 {
                    to_remove.push(*x);
                }
            }
        });
        i += 1;
        if i == to_remove.len() {
            break;
        }
    }
}

fn find_leaves(map: &HashMap<Vid, HashSet<Vid>>, from: Vid, leaves: &mut SmallVec<[Vid; 4]>) {
    let childs = map.get(&from).unwrap();
    if childs.is_empty() {
        if !leaves.contains(&from) {
            leaves.push(from);
        }
    } else {
        childs.iter().for_each(|child| {
            // There are no possibility to create self-reference, so we always find leaves
            find_leaves(map, *child, leaves);
        })
    }
}
/*
fn find_roots(map: &HashMap<Vid, SmallVec<[Vid; 4]>>) -> Vec<Vid> {
    let mut have_parents: HashSet<&Vid> = HashSet::with_capacity(map.len());
    let mut possible_roots: HashSet<&Vid> = HashSet::with_capacity(map.len());
    for (vid, refs) in map {
        for rf in refs {
            have_parents.insert(rf);
            if possible_roots.contains(rf) {
                possible_roots.remove(rf);
            }
        }
        if !have_parents.contains(vid) {
            possible_roots.insert(vid);
        }
    }
    let mut roots = possible_roots
        .into_iter()
        .map(|vid| *vid)
        .collect::<Vec<_>>();
    roots.sort();
    roots
}*/

#[derive(Debug, PartialEq)]
struct FuncUsageResult {
    arguments_usage: SmallVec<[ArgumentUsage; 8]>,
}

impl FuncUsageResult {
    pub fn new(arguments_usage: SmallVec<[ArgumentUsage; 8]>) -> Self {
        FuncUsageResult { arguments_usage }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ArgumentUsage {
    /// Argument not returned from the function.
    Used,
    /// Argument returned from the function.
    NotUsed,
    /// Argument can be returned from the function or not.
    Unknown,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::ctx::Eid;
    use crate::mir::{
        EnumVariantDiscriminant, ProgramBuilder, UserEnum, UserEnumVariant, Vty, VtyKind,
    };

    #[test]
    fn find_leaves_test() {
        let mut map = HashMap::new();
        map.insert(Vid(0), hash_set![]);
        map.insert(Vid(1), hash_set![]);
        map.insert(Vid(2), hash_set![]);
        map.insert(Vid(3), hash_set![Vid(0)]);
        map.insert(Vid(4), hash_set![Vid(3)]);
        map.insert(Vid(5), hash_set![Vid(3), Vid(4)]);
        map.insert(Vid(6), hash_set![Vid(1), Vid(2)]);
        let test = |from: usize, expected: SmallVec<[Vid; 4]>| {
            let mut leaves = smallvec![];
            find_leaves(&map, Vid(from), &mut leaves);
            leaves.sort();
            assert_eq!(leaves, expected);
        };
        test(3, smallvec![Vid(0)]);
        test(4, smallvec![Vid(0)]);
        test(5, smallvec![Vid(0)]);
        test(6, smallvec![Vid(1), Vid(2)]);
    }
    /*
        #[test]
        fn find_roots_test() {
            let mut map = HashMap::new();
            map.insert(Vid(0), smallvec![]);
            map.insert(Vid(1), smallvec![]);
            map.insert(Vid(2), smallvec![Vid(0)]);
            map.insert(Vid(3), smallvec![Vid(1)]);
            map.insert(Vid(4), smallvec![Vid(2), Vid(3)]);

            map.insert(Vid(5), smallvec![]);
            map.insert(Vid(6), smallvec![]);
            map.insert(Vid(7), smallvec![Vid(5), Vid(6)]);

            map.insert(Vid(8), smallvec![]);

            let expected = vec![Vid(4), Vid(7), Vid(8)];
            let roots = find_roots(&map);
            assert_eq!(expected, roots);
        }
    */
    #[test]
    fn remove_refs_with_vid_as_leaf_test() {
        let mut map = HashMap::new();
        map.insert(Vid(0), HashSet::new());
        map.insert(Vid(1), HashSet::new());
        map.insert(Vid(2), hash_set![Vid(0)]);
        map.insert(Vid(3), hash_set![Vid(0), Vid(1)]);

        remove_refs_with_vid_as_leaf(&mut map, Vid(0));
        assert!(!map.contains_key(&Vid(0)));
        assert!(map.contains_key(&Vid(1)));
        assert!(!map.contains_key(&Vid(2)));
        assert!(map.contains_key(&Vid(3)));
    }

    #[test]
    fn insert_deallocs_test() {
        let ctx = Ctx::new();
        let (stmts, vars) = ProgramBuilder::new(&ctx, vec![])
            .int_ass(123) // _0
            .alloc_ass(Vid(0)) // _1
            .unit_ass() // _2
            .return_(Vid(2));
        let (expected, expected_vars) = ProgramBuilder::new(&ctx, vec![])
            .int_ass(123) // _0
            .alloc_ass(Vid(0)) // _1
            .unit_ass() // _2
            // NB: dealloc
            .dealloc(Vid(1))
            .return_(Vid(2));
        let (new_stmt, new_vars, _) = insert_heap_allocs_stmts_first(0, vars, stmts, &[], &ctx);

        assert_eq!(expected, new_stmt);
        assert_eq!(expected_vars, new_vars);
    }

    #[test]
    fn insert_deallocs_on_arguments_test() {
        let ctx = Ctx::new();
        let (stmts, vars) = ProgramBuilder::new(&ctx, vec![Vty::heap(VtyKind::Int)])
            // _0 = argument
            .unit_ass() // _1
            .return_(Vid(1));
        let (expected, expected_vars) = ProgramBuilder::new(&ctx, vec![Vty::heap(VtyKind::Int)])
            // _0 = argument
            .unit_ass() // _1
            // NB: dealloc
            .dealloc(Vid(0))
            .return_(Vid(1));
        let (new_stmt, new_vars, _) = insert_heap_allocs_stmts_first(1, vars, stmts, &[], &ctx);

        assert_eq!(expected, new_stmt);
        assert_eq!(expected_vars, new_vars);
    }

    #[test]
    fn insert_clones_test() {
        let mut ctx = Ctx::new();
        ctx.enums
            .push(UserEnum::new(vec![UserEnumVariant::new(vec![])]));
        ctx.functions.push(Function::new(
            vec![Variable::new(Vty::heap(VtyKind::Enum(Eid(0))), Vid(0))],
            Vty::unit(),
            vec![], // does not important
            vec![], // does not important
        ));
        let usages = vec![FuncUsageResult::new(smallvec![ArgumentUsage::Unknown])];
        // _0 = D[..]
        // _1 = @map[_0]
        // _2 = @alloc _1
        // _3 = @call f0 (_2)
        // _4 = ()
        // ret _4
        let (stmts, vars) = ProgramBuilder::new(&ctx, vec![])
            .discriminant_ass(DiscriminantOf::UserEnumField(EnumVariantDiscriminant::new(
                Eid(0),
                0,
            )))
            .map_ass(Eid(0), Vid(0), vec![])
            .alloc_ass(Vid(1))
            .call_ass(Fid(0), vec![Vid(2)])
            .unit_ass()
            .return_(Vid(4));

        // _0 = D[..]
        // _1 = @map[_0]
        // _2 = @alloc _1
        // _3 = @clone _2 NB!
        // _4 = @call f0 (_3) NB!
        // _5 = ()
        // @dealloc _2 NB!
        // ret _5
        let (expected, expected_vars) = ProgramBuilder::new(&ctx, vec![])
            .discriminant_ass(DiscriminantOf::UserEnumField(EnumVariantDiscriminant::new(
                Eid(0),
                0,
            )))
            .map_ass(Eid(0), Vid(0), vec![])
            .alloc_ass(Vid(1))
            .clone_ass(Vid(2))
            .call_ass(Fid(0), vec![Vid(3)])
            .unit_ass()
            .dealloc(Vid(2))
            .return_(Vid(5));
        let (new_stmt, new_vars, _) = insert_heap_allocs_stmts_first(0, vars, stmts, &usages, &ctx);

        assert_eq!(new_stmt, expected);
        assert_eq!(new_vars, expected_vars);
    }

    #[test]
    fn func_usage_result_test() {
        let mut ctx = Ctx::new();
        // _0 = arg0
        // _1 = arg1
        // ret _1
        let (stmts, vars) =
            ProgramBuilder::new(&ctx, vec![Vty::heap(VtyKind::Int), Vty::heap(VtyKind::Int)])
                .return_(Vid(1));

        // _0 = arg0
        // _1 = arg1
        // @dealloc _0 NB!
        // ret _1
        let (expected, expected_vars) =
            ProgramBuilder::new(&ctx, vec![Vty::heap(VtyKind::Int), Vty::heap(VtyKind::Int)])
                .dealloc(Vid(0))
                .return_(Vid(1));
        let (new_stmt, new_vars, usage) = insert_heap_allocs_stmts_first(2, vars, stmts, &[], &ctx);

        assert_eq!(new_stmt, expected);
        assert_eq!(new_vars, expected_vars);
        assert_eq!(
            usage,
            FuncUsageResult::new(smallvec![ArgumentUsage::NotUsed, ArgumentUsage::Used,])
        )
    }

    #[test]
    fn returned_allocated_value_was_dealloc_if_not_used_test() {
        let mut ctx = Ctx::new();
        ctx.enums
            .push(UserEnum::new(vec![UserEnumVariant::new(vec![Vty::heap(
                VtyKind::Int,
            )])]));
        ctx.functions.push(Function::new(
            vec![Variable::new(Vty::heap(VtyKind::Int), Vid(0))],
            // Note that we test _stacked_ value, but enum contains heaped value,
            // so it must be deallocated still.
            Vty::stack(VtyKind::Enum(Eid(0))),
            vec![], // does not important
            vec![], // does not important
        ));
        let usages = vec![FuncUsageResult::new(smallvec![ArgumentUsage::Used])];
        // _0 = 5
        // _1 = @alloc _0
        // _2 = @call f0 (_1)
        // _3 = ()
        // ret _3
        let (stmts, vars) = ProgramBuilder::new(&ctx, vec![])
            .int_ass(5)
            .alloc_ass(Vid(0))
            .call_ass(Fid(0), vec![Vid(1)])
            .unit_ass()
            .return_(Vid(3));

        // _0 = 5
        // _1 = @alloc _0
        // _2 = @call f0 (_1)
        // _3 = ()
        // @dealloc _2 NB!
        // ret _3
        let (expected, expected_vars) = ProgramBuilder::new(&ctx, vec![])
            .int_ass(5)
            .alloc_ass(Vid(0))
            .call_ass(Fid(0), vec![Vid(1)])
            .unit_ass()
            .dealloc(Vid(2))
            .return_(Vid(3));
        let (new_stmt, new_vars, _) = insert_heap_allocs_stmts_first(0, vars, stmts, &usages, &ctx);

        assert_eq!(new_stmt, expected);
        assert_eq!(new_vars, expected_vars);
    }

    #[test]
    fn cases_test() {
        let mut ctx = Ctx::new();
        // _0 = arg1
        // _1 = arg2
        // _2 = arg3
        // cases _0 { // does not important for the checker
        //  case _0: {
        //   ret _1
        //  }
        //  case _0: {
        //   _3 = () // there are no typechecking so we can write this
        //   ret _3
        //  }
        // }
        let (stmts, vars) = ProgramBuilder::new(
            &ctx,
            vec![
                Vty::heap(VtyKind::Int),
                Vty::heap(VtyKind::Int),
                Vty::heap(VtyKind::Int),
            ],
        )
        .cases(
            Vid(0),
            vec![
                Case::new(Vid(0), ProgramBuilder::new(&ctx, vec![]).return_(Vid(1))),
                Case::new(
                    Vid(0),
                    ProgramBuilder::new(&ctx, vec![]).unit_ass().return_(Vid(3)),
                ),
            ],
        );

        // _0 = arg1
        // _1 = arg2
        // _2 = arg3
        // cases _0 { // does not important for the checker
        //  case _0: {
        //   @dealloc _2 NB!
        //   @dealloc _0 NB!
        //   ret _1
        //  }
        //  case _0: {
        //   _3 = () // there are no typechecking so we can write this
        //   @dealloc _2 NB!
        //   @dealloc _1 NB!
        //   @dealloc _0 NB!
        //   ret _3
        //  }
        // }
        let (expected, expected_vars) = ProgramBuilder::new(
            &ctx,
            vec![
                Vty::heap(VtyKind::Int),
                Vty::heap(VtyKind::Int),
                Vty::heap(VtyKind::Int),
            ],
        )
        .cases(
            Vid(0),
            vec![
                Case::new(
                    Vid(0),
                    ProgramBuilder::new(&ctx, vec![])
                        .dealloc(Vid(2))
                        .dealloc(Vid(0))
                        .return_(Vid(1)),
                ),
                Case::new(
                    Vid(0),
                    ProgramBuilder::new(&ctx, vec![])
                        .unit_ass()
                        .dealloc(Vid(2))
                        .dealloc(Vid(1))
                        .dealloc(Vid(0))
                        .return_(Vid(3)),
                ),
            ],
        );
        let (new_stmt, new_vars, usage) = insert_heap_allocs_stmts_first(3, vars, stmts, &[], &ctx);

        assert_eq!(new_stmt, expected);
        assert_eq!(new_vars, expected_vars);
        assert_eq!(
            usage,
            FuncUsageResult::new(smallvec![
                ArgumentUsage::NotUsed,
                ArgumentUsage::Unknown,
                ArgumentUsage::NotUsed
            ])
        )
    }
}
