mod z3_converter;

use crate::common::{Context, Error, HasName};
use crate::interpreter::z3_converter::{Consts, ProofContext};
use crate::ir::objects::{Arg, DataVariant, FunctionDefinition, FunctionObject, Object, Var};
use crate::ir::types::Type;
use crate::ir::{parse_expr, Expr, ExprKind};
use crate::peg_error_to_showed;
use crate::syntax::parse_token;
use itertools::Itertools;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::ops;
use std::ops::Deref;
use std::rc::Rc;
use z3::Solver;

#[derive(Debug)]
pub struct Interpreter<'a> {
    // enums: Vec<Rc<EnumInstance>>,
    functions: &'a [FunctionObject],
    ctx: &'a Context<'a, Object>,
}

impl<'a> Interpreter<'a> {
    pub fn new(functions: &'a [FunctionObject], ctx: &'a Context<'a, Object>) -> Self {
        Interpreter { functions, ctx }
    }
}

impl Interpreter<'_> {
    pub fn eval(&self, expr: &Expr, bc_ctx: &Context<'_, ByteCode>) -> Result<ByteCode, Error> {
        match &expr.kind {
            ExprKind::Int(i) => Ok(ByteCode::Int(*i)),
            ExprKind::Ident(i) => match bc_ctx.find(i.as_str()) {
                Some(b) => Ok(b.clone()),
                None => match self.ctx.find(i.as_str()) {
                    Some(o) => match o {
                        Object::FunctionDefinition(def) => {
                            self.eval_func(vec![], Callable::Func(def.clone()), def.ftype.clone())
                        }
                        Object::Enum(_) => unimplemented!(),
                        Object::EnumVariant(_) => unimplemented!(),
                        Object::Arg(_) => unimplemented!(),
                        Object::Var(_) => unimplemented!(),
                        Object::Type(_) => unimplemented!(),
                    },
                    None => Err(Error::custom(expr.span, format!("Not found {}", i))),
                },
            },
            ExprKind::Application(l, r) => self.eval(l.as_ref(), bc_ctx).and_then(|left| {
                self.eval(r.as_ref(), bc_ctx).and_then(|right| match left {
                    ByteCode::ApplicationFunction(mut args, func, ty) => {
                        args.push(right);
                        let ty = match ty.deref() {
                            Type::Function(f) => f.return_value.clone(),
                            otherwise => ty,
                        };
                        self.eval_func(args, func, ty)
                    }
                    _ => unimplemented!(),
                })
            }),
            ExprKind::Add(l, r) => self.eval(l, bc_ctx).and_then(|left| {
                self.eval(r, bc_ctx)
                    .and_then(|right| ByteCode::add(left, right))
            }),
            _ => unimplemented!(),
        }
    }
    fn eval_func(
        &self,
        args: Vec<ByteCode>,
        cal: Callable,
        ty: Rc<Type>,
    ) -> Result<ByteCode, Error> {
        match ty.deref() {
            Type::Function(_) => Ok(ByteCode::ApplicationFunction(args, cal, ty)),
            _ => match cal {
                Callable::Func(def) => {
                    let imp = &self.functions.iter().find(|f| f.def == def).unwrap();
                    self.eval(
                        &imp.body,
                        &Context {
                            objects: imp
                                .args
                                .iter()
                                .zip(args.into_iter())
                                .map(|(arg, data)| ByteCode::Arg(arg.clone(), Box::new(data)))
                                .collect(),
                            parent: None,
                        },
                    )
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Callable {
    Func(Rc<FunctionDefinition>),
}

impl HasName for Callable {
    fn name(&self) -> &str {
        match self {
            Callable::Func(f) => f.name.as_str(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ByteCode {
    ApplicationFunction(Vec<ByteCode>, Callable, Rc<Type>),
    Int(i128),
    Var(Rc<Var>, Box<ByteCode>),
    Arg(Rc<Arg>, Box<ByteCode>),
}

impl ByteCode {
    fn add(self, other: Self) -> Result<Self, Error> {
        match (self, other) {
            (ByteCode::Int(i), ByteCode::Int(i2)) => Ok(ByteCode::Int(i + i2)),
            (ByteCode::Var(_, bc), right) => bc.add(right),
            (left, ByteCode::Var(_, bc)) => left.add(*bc),
            (ByteCode::Arg(_, bc), right) => bc.add(right),
            (left, ByteCode::Arg(_, bc)) => left.add(*bc),
            _ => unimplemented!(),
        }
    }
}

impl HasName for ByteCode {
    fn name(&self) -> &str {
        match self {
            ByteCode::ApplicationFunction(_, f, _) => f.name(),
            ByteCode::Int(_) => "",
            ByteCode::Var(v, _) => v.name.as_str(),
            ByteCode::Arg(a, _) => a.name.as_str(),
        }
    }
}

pub fn proof_func(
    f: &FunctionObject,
    objects: &[FunctionObject],
    objs_ctx: &Context<'_, Object>,
) -> Result<(), Error> {
    let body = &f.body;
    let config = z3::Config::new();
    let ctx = z3::Context::new(&config);
    let solver = z3::Solver::new(&ctx);
    let mut ctx = ProofContext {
        solver,
        prefix: f.def.name.clone().inner() + "_",
        interpreter: Interpreter::new(objects, objs_ctx),
    };
    let mut consts = Consts::new();

    f.args.iter().for_each(|arg| {
        ctx.declare_var(
            ctx.prefix.clone() + arg.name.as_str(),
            arg.ty.deref(),
            &mut consts,
        );
    });

    ctx.proof_check(
        body.clone(),
        f.def.ftype.clone().get_return_value().deref(),
        &mut consts,
    )
}
