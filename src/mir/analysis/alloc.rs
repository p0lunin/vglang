use crate::mir::ctx::{Ctx, Fid};
use crate::mir::{Function, Statement, Assigment, Vid, Location, Variable, VtyKind, Vty, DiscriminantOf};
use std::collections::{HashMap, HashSet};
use smallvec::SmallVec;
use smallvec::smallvec;
use itertools::Itertools;

pub fn insert_heap_allocs(ctx: Ctx) -> Ctx {
    todo!()
}

fn insert_heap_allocs_fn(mut fun: Function, usages: &[FuncUsageResult]) -> (Function, FuncUsageResult) {
    let (stmts, vars, usage) = insert_heap_allocs_stmts(fun.args.len() as u8, fun.vars, fun.stmts, usages);
    fun.stmts = stmts;
    fun.vars = vars;
    (fun, usage)
}

fn insert_heap_allocs_stmts(fc_args_count: u8, vars: Vec<Vty>, mut stmts: Vec<Statement>, usages: &[FuncUsageResult])
    -> (Vec<Statement>, Vec<Vty>, FuncUsageResult)
{
    let mut out = vec![];
    let mut allocated = SmallVec::<[Vid; 4]>::new();
    let mut refs: HashMap<Vid, SmallVec<[Vid; 4]>> = HashMap::with_capacity(30);
    let mut vars = vars;
    let mut push_variable = |v: Variable, ass: Assigment| {
        out.push(Statement::Variable(v, ass))
    };

    for i in 0..stmts.len() {
        match stmts[i].clone() {
            Statement::Variable(v, ass) => {
                match ass {
                    Assigment::Alloc(_) => {
                        allocated.push(v.id);
                        refs.insert(v.id, smallvec![]);

                        push_variable(v, ass);
                    }
                    Assigment::Map(_, _, ref m) => {
                        let mut to_insert = smallvec![];
                        for (reff, _) in refs.iter() {
                            if m.contains(reff) {
                                to_insert.push(*reff);
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
                        push_variable(v, ass);
                    }
                    Assigment::Call(func, arguments) => {
                        let usage = &usages[func.inner()];
                        debug_assert_eq!(usage.arguments_usage.len(), arguments.len());

                        let mut new_arguments = Vec::with_capacity(arguments.len());
                        let mut this_offset = 0;
                        usage.arguments_usage.iter().zip(arguments.iter()).for_each(|(usage, arg)| {
                            match usage {
                                ArgumentUsage::Used => {
                                    new_arguments.push(*arg);
                                    if let Some(_) = refs.iter().find(|(x, _)| *x == arg) {
                                        refs.entry(v.id)
                                            .or_default()
                                            .push(*arg);
                                    }
                                }
                                ArgumentUsage::Unknown => {
                                    let cloned_vid = Vid::new(arg.0 + this_offset + 1);
                                    debug_assert_ne!(vars[arg.0].location, Location::Stack);
                                    refs.insert(cloned_vid, smallvec![*arg]);
                                    push_variable(Variable::new(vars[arg.0].clone(), cloned_vid), Assigment::Clone(*arg));
                                    this_offset += 1;
                                    new_arguments.push(cloned_vid);
                                    vars.insert(cloned_vid.0, vars[arg.0].clone());
                                    inc_statements_vids(&mut stmts[i..], v.id.0 + this_offset);
                                }
                                ArgumentUsage::NotUsed => {
                                    new_arguments.push(*arg)
                                }
                            }
                        });
                        push_variable(Variable::new(v.ty, Vid(v.id.0 + this_offset)), Assigment::Call(func, new_arguments));
                    }
                    Assigment::Dereference(_) => {
                        /* dereference does not create refs */
                        push_variable(v, ass);
                    }
                    Assigment::Value(_) => {
                        push_variable(v, ass);
                    }
                    Assigment::Clone(_) => { unreachable!("There cannot be clone before this pass."); }
                }
            }
            Statement::Case { .. } => { unimplemented!() }
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
                                let (idx, _) = allocated.iter().find_position(|x| *x == leaf)
                                    .expect("Leaf can be only an input arg or an allocated var");
                                allocated.remove(idx);
                            }
                        }
                        depends
                    };
                    args_depends
                } else { smallvec![] };
                let arguments_usage = {
                    let mut usage = smallvec![ArgumentUsage::NotUsed; fc_args_count as usize];
                    for v in args_depends {
                        usage[v as usize] = ArgumentUsage::Used;
                    }
                    usage
                };
                for allocated_var in allocated {
                    out.push(Statement::Dealloc(allocated_var));
                }
                let usage_result = FuncUsageResult {
                    arguments_usage
                };
                out.push(Statement::Return(v));
                return (out, vars, usage_result); // return statement means there are no more statements
            }
            Statement::Dealloc(_) => {
                // TODO: allow user to call dealloc by itself, like for early dispose.
                unreachable!("There are no expected to have dealloc before this pass.")
            }
        }
    }
    unreachable!("There are must be return statement in the end of the statements.")
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
                    Assigment::Discriminant(d) => {
                        match d {
                            DiscriminantOf::Variable(v) => {
                                inc_vid(v)
                            },
                            _ => {}
                        }
                    }
                    Assigment::Field { var, .. } => {
                        inc_vid(var)
                    }
                    Assigment::Map(_, v, vids) => {
                        inc_vid(v);
                        vids.iter_mut().for_each(|v| inc_vid(v));
                    }
                    Assigment::Call(_, vids) => {
                        vids.iter_mut().for_each(|v| inc_vid(v));
                    }
                    Assigment::Alloc(vid) => {
                        inc_vid(vid)
                    }
                    Assigment::BinOp(_, _, _) => { unimplemented!() }
                    Assigment::Dereference(vid) => {
                        inc_vid(vid)
                    }
                    Assigment::Value(_) => {}
                    Assigment::Clone(vid) => {
                        inc_vid(vid)
                    }
                }
            }
            Statement::Case { .. } => { unimplemented!() }
            Statement::Return(vid) => {
                inc_vid(vid)
            }
            Statement::Dealloc(vid) => {
                inc_vid(vid)
            }
        }
    }
}

fn find_leaves(map: &HashMap<Vid, SmallVec<[Vid; 4]>>, from: Vid, leaves: &mut SmallVec<[Vid; 4]>) {
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
}

struct FuncUsageResult {
    arguments_usage: SmallVec<[ArgumentUsage; 8]>,
}

impl FuncUsageResult {
    pub fn new(arguments_usage: SmallVec<[ArgumentUsage; 8]>) -> Self {
        FuncUsageResult { arguments_usage }
    }
}

#[derive(Debug, Clone, Copy)]
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
    use crate::mir::{Vty, ProgramBuilder, VtyKind, UserEnum, UserEnumVariant};
    use crate::mir::ctx::Eid;

    #[test]
    fn find_leaves_test() {
        let mut map = HashMap::new();
        map.insert(Vid::new(0), smallvec![]);
        map.insert(Vid::new(1), smallvec![]);
        map.insert(Vid::new(2), smallvec![]);
        map.insert(Vid::new(3), smallvec![Vid::new(0)]);
        map.insert(Vid::new(4), smallvec![Vid::new(3)]);
        map.insert(Vid::new(5), smallvec![Vid::new(3), Vid::new(4)]);
        map.insert(Vid::new(6), smallvec![Vid::new(1), Vid::new(2)]);
        let test = |from: usize, expected: SmallVec<[Vid; 4]>| {
            let mut leaves = smallvec![];
            find_leaves(&map, Vid::new(from), &mut leaves);
            assert_eq!(leaves, expected);
        };
        test(3, smallvec![Vid::new(0)]);
        test(4, smallvec![Vid::new(0)]);
        test(5, smallvec![Vid::new(0)]);
        test(6, smallvec![Vid::new(1), Vid::new(2)]);
    }

    #[test]
    fn find_roots_test() {
        let mut map = HashMap::new();
        map.insert(Vid::new(0), smallvec![]);
        map.insert(Vid::new(1), smallvec![]);
        map.insert(Vid::new(2), smallvec![Vid::new(0)]);
        map.insert(Vid::new(3), smallvec![Vid::new(1)]);
        map.insert(Vid::new(4), smallvec![Vid::new(2), Vid::new(3)]);

        map.insert(Vid::new(5), smallvec![]);
        map.insert(Vid::new(6), smallvec![]);
        map.insert(Vid::new(7), smallvec![Vid::new(5), Vid::new(6)]);

        map.insert(Vid::new(8), smallvec![]);

        let expected = vec![Vid::new(4), Vid::new(7), Vid::new(8)];
        let roots = find_roots(&map);
        assert_eq!(expected, roots);
    }

    #[test]
    fn insert_deallocs_test() {
        let ctx = Ctx::new();
        let (stmts, vars) = ProgramBuilder::new(&ctx, vec![])
            .int_ass(123) // _0
            .alloc_ass(Vid::new(0)) // _1
            .unit_ass() // _2
            .return_(Vid::new(2));
        let (expected, expected_vars) = ProgramBuilder::new(&ctx, vec![])
            .int_ass(123) // _0
            .alloc_ass(Vid::new(0)) // _1
            .unit_ass() // _2
            // NB: dealloc
            .dealloc(Vid::new(1))
            .return_(Vid::new(2));
        let (new_stmt, new_vars, _) = insert_heap_allocs_stmts(0, vars, stmts, &[]);

        assert_eq!(expected, new_stmt);
        assert_eq!(expected_vars, new_vars);
    }

    #[test]
    fn insert_clones_test() {
        let mut ctx = Ctx::new();
        ctx.enums.push(UserEnum::new(vec![
            UserEnumVariant::new(vec![]),
        ]));
        ctx.functions.push(Function::new(
            vec![Variable::new(Vty::heap(VtyKind::Enum(Eid(0))), Vid(0))],
            Vty::unit(),
            vec![], // does not important
            vec![], // does not important
        ));
        let usages = vec![
            FuncUsageResult::new(smallvec![ArgumentUsage::Unknown])
        ];
        // _0 = D[..]
        // _1 = @map[_0]
        // _2 = @alloc _1
        // _3 = @call f0 (_2)
        // _4 = ()
        // ret _4
        let (stmts, vars) = ProgramBuilder::new(&ctx, vec![])
            .discriminant_ass(DiscriminantOf::UserEnumField(Eid(0), 0))
            .map_ass(Eid(0), Vid(0), vec![Vid(0)])
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
            .discriminant_ass(DiscriminantOf::UserEnumField(Eid(0), 0))
            .map_ass(Eid(0), Vid(0), vec![Vid(0)])
            .alloc_ass(Vid(1))
            .clone_ass(Vid(2))
            .call_ass(Fid(0), vec![Vid(3)])
            .unit_ass()
            .dealloc(Vid(2))
            .return_(Vid(5));
        let (new_stmt, new_vars, _) = insert_heap_allocs_stmts(0, vars, stmts, &usages);

        assert_eq!(new_stmt, expected);
        assert_eq!(new_vars, expected_vars);
    }
}