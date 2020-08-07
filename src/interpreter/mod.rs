use crate::common::{Context, Error, HasName, Searchable, SearchableByPath, Spanned};
use crate::ir::objects::{
    Arg, DataDef, DataVariant, FunctionDefinition, FunctionObject, Object, Var,
};
use crate::ir::patmat::Pattern;
use crate::ir::types::Type;
use crate::ir::{parse_expr, Expr, ExprKind};
use crate::peg_error_to_showed;
use crate::syntax::ast::Token;
use crate::syntax::parse_token;
use itertools::Itertools;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops;
use std::ops::Deref;
use std::rc::Rc;

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
    pub fn execute_code(&self, token: Token) -> Result<ByteCode, Error> {
        let ir = parse_expr(&token, self.ctx, &mut HashMap::new(), None)?
            .ok_or_else(|| Error::cannot_infer_type(token.span))?;
        self.eval(&ir, &Context::new())
    }

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
                        Object::Enum(e) => unimplemented!(),
                        Object::EnumVariant(_) => unimplemented!(),
                        Object::Arg(_) => unimplemented!(),
                        Object::Var(_) => unimplemented!(),
                        Object::Type(_) => unimplemented!(),
                    },
                    None => Err(Error::custom(expr.span, format!("Not found {}", i))),
                },
            },
            ExprKind::Application(l, r) => self.eval(l.as_ref(), bc_ctx).and_then(|left| {
                self.eval(r.as_ref(), bc_ctx)
                    .and_then(|right| match left.inner() {
                        ByteCode::ApplicationFunction(mut args, func, ty) => {
                            args.push(right);
                            let ty = match ty.deref() {
                                Type::Function(f) => f.return_value.clone(),
                                otherwise => ty,
                            };
                            self.eval_func(args, func, ty)
                        }
                        t => {
                            dbg!(t);
                            unimplemented!()
                        }
                    })
            }),
            ExprKind::Add(l, r) => self.eval(l, bc_ctx).and_then(|left| {
                self.eval(r, bc_ctx)
                    .and_then(|right| ByteCode::add(left, right))
            }),
            ExprKind::Let { var, assign, expr } => {
                let assigned = self.eval(assign.as_ref(), bc_ctx)?;
                let ctx = Context {
                    objects: vec![ByteCode::Var(var.clone(), Box::new(assigned))],
                    parent: Some(bc_ctx),
                };
                self.eval(expr.as_ref(), &ctx)
            }
            ExprKind::CaseExpr { cond, arms } => {
                let cond = self.eval(&cond, bc_ctx)?;
                self.apply_pattern(cond.clone(), arms, bc_ctx)
            }
            ExprKind::DataVariant(v) => {
                self.eval_func(vec![], Callable::DataVariant(v.clone()), v.get_type())
            }
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
                Callable::DataVariant(v) => Ok(ByteCode::DataVariant(v, args)),
            },
        }
    }
    fn apply_pattern(
        &self,
        expr: ByteCode,
        arms: &[(Spanned<Pattern>, Expr)],
        ctx: &Context<'_, ByteCode>,
    ) -> Result<ByteCode, Error> {
        for (pat, e) in arms {
            match is_aplyable(&expr, pat) {
                true => {
                    let ctx = self.create_scope_for_pattern(pat, expr, ctx);
                    return self.eval(e, &ctx);
                }
                false => {}
            }
        }
        unreachable!()
    }

    fn create_scope_for_pattern<'a>(
        &self,
        pattern: &Pattern,
        obj: ByteCode,
        top: &'a Context<'a, ByteCode>,
    ) -> Context<'a, ByteCode> {
        let mut scope = Context {
            objects: vec![],
            parent: Some(top),
        };
        match pattern {
            Pattern::Otherwise => {}
            Pattern::Bind(i, pat) => {
                scope.objects.push(ByteCode::Var(
                    Rc::new(Var {
                        name: i.clone(),
                        ty: obj.ty(),
                    }),
                    Box::new(obj.clone()),
                ));
                scope.objects.append(
                    &mut self
                        .create_scope_for_pattern(pat.as_ref(), obj, top)
                        .objects,
                );
            }
            Pattern::Variant(path, pats) => match self.ctx.find_by_path(path) {
                Some(Object::EnumVariant(_)) => {
                    let (v, datas) = match obj.clone().inner() {
                        ByteCode::DataVariant(v, datas) => (v, datas),
                        _ => unreachable!(),
                    };
                    pats.iter().zip(datas.iter()).for_each(|(pat, dat)| {
                        let mut res = self.create_scope_for_pattern(pat, dat.clone(), &scope);
                        scope.objects.append(&mut res.objects);
                    })
                }
                _ => unreachable!(),
            },
            Pattern::Ident(i) => scope.objects.push(ByteCode::Var(
                Rc::new(Var {
                    name: i.clone(),
                    ty: obj.ty(),
                }),
                Box::new(obj.clone()),
            )),
        };
        scope
    }
}

fn is_aplyable(expr: &ByteCode, pat: &Pattern) -> bool {
    match (&expr.clone().inner(), pat) {
        (_, Pattern::Otherwise) => true,
        (_, Pattern::Bind(_, pat)) => is_aplyable(expr, pat.as_ref()),
        (ByteCode::DataVariant(v1, datas), Pattern::Variant(v2, pats)) => {
            v1.name.as_str() == v2.end()
                && datas
                    .iter()
                    .zip(pats.iter())
                    .all(|(d, p)| is_aplyable(d, p))
        }
        (_, Pattern::Ident(_)) => true,
        c => {
            dbg!(c);
            unreachable!()
        }
    }
}

#[derive(Debug, Clone)]
pub enum Callable {
    Func(Rc<FunctionDefinition>),
    DataVariant(Rc<DataVariant>),
}

impl Display for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Callable::Func(d) => write!(f, "{}", d.name),
            Callable::DataVariant(v) => write!(f, "{}", v.name),
        }
    }
}

impl HasName for Callable {
    fn name(&self) -> &str {
        match self {
            Callable::Func(f) => f.name.as_str(),
            Callable::DataVariant(v) => v.name.as_str(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ByteCode {
    ApplicationFunction(Vec<ByteCode>, Callable, Rc<Type>),
    Int(i128),
    Var(Rc<Var>, Box<ByteCode>),
    Arg(Rc<Arg>, Box<ByteCode>),
    DataVariant(Rc<DataVariant>, Vec<ByteCode>),
    DataType(Rc<DataDef>),
}

impl ByteCode {
    fn ty(&self) -> Rc<Type> {
        match self {
            ByteCode::ApplicationFunction(_, _, ty) => ty.clone(),
            ByteCode::Int(_) => Rc::new(Type::Int),
            ByteCode::Var(v, _) => v.ty.clone(),
            ByteCode::Arg(a, _) => a.ty.clone(),
            ByteCode::DataVariant(v, _) => Rc::new(Type::Data(v.dty.clone())),
            ByteCode::DataType(d) => Rc::new(Type::Type),
        }
    }

    fn inner(self) -> Self {
        match self {
            ByteCode::Var(_, b) => *b,
            ByteCode::Arg(_, b) => *b,
            _ => self,
        }
    }
}

impl Display for ByteCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ByteCode::ApplicationFunction(bc, cal, ty) => {
                write!(f, "{} ({}): {}", cal, bc.iter().join(","), ty)
            }
            ByteCode::Int(i) => write!(f, "{}: Int", i),
            ByteCode::Var(v, bc) => write!(f, "{}: {}", v, bc),
            ByteCode::Arg(a, bc) => write!(f, "{}: {}", a, bc),
            ByteCode::DataVariant(v, bc) => write!(f, "{} {}", v, bc.iter().join(" ")),
            ByteCode::DataType(d) => write!(f, "{}", d.ty),
        }
    }
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
            ByteCode::DataVariant(_, _) => "",
            ByteCode::DataType(d) => d.ty.name.as_str(),
        }
    }
}
