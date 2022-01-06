use crate::arena::{Arena, Id};
use crate::common::global_context::ScopeCtx;
use crate::common::{
    BinOp, DisplayScope, Error, Find, HasName, LocalContext, PathToFind, Searchable, Spanned,
};
use crate::ir::objects::{Arg, DataDef, FunctionObject, Object, Var};
use crate::ir::patmat::Pattern;
use crate::ir::types::{Concrete, Type};
use crate::ir::{parse_expr, Expr, ExprKind};
use crate::syntax::ast::Token;
use itertools::Itertools;
use smallvec::smallvec;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug)]
pub struct Interpreter<'a, 'b> {
    ctx: &'a mut ScopeCtx<'b>,
}

impl<'a, 'b> Interpreter<'a, 'b> {
    pub fn new(ctx: &'a mut ScopeCtx<'b>) -> Self {
        Interpreter { ctx }
    }
}

impl Interpreter<'_, '_> {
    pub fn execute_code(&mut self, token: Token) -> Result<ByteCode, Error> {
        let ir = parse_expr(&token, &mut self.ctx, &mut HashMap::new(), None)?
            .ok_or_else(|| Error::cannot_infer_type(token.span))?;
        self.eval(&ir, &LocalContext::new())
    }
    // List.Cons 1 List.Nil
    pub fn eval(
        &mut self,
        expr: &Expr,
        bc_ctx: &LocalContext<'_, ByteCode>,
    ) -> Result<ByteCode, Error> {
        match &expr.kind {
            ExprKind::Int(i) => Ok(ByteCode::Int(*i)),
            ExprKind::Ident(i) => match bc_ctx.find(i.as_str()) {
                Some(b) => Ok(b.clone()),
                None => match self.ctx.find(PathToFind::name(i.as_str())) {
                    Some(o) => match o {
                        Object::Function(def) => {
                            let ty = self.ctx.global.get_func(def).get_type(&mut self.ctx.types);
                            self.eval_func(vec![], Callable::Func(def), ty)
                        }
                        Object::Enum(id) => {
                            let e = self.ctx.global.get_data(id);
                            match e.ty.generics.len() {
                                0 => Ok(ByteCode::DataType(Concrete::new(id, vec![]))),
                                _ => {
                                    let e_ty = e.as_ty(&mut self.ctx);
                                    Ok(ByteCode::ApplicationFunction(
                                        vec![],
                                        Callable::DataType(id),
                                        e_ty,
                                    ))
                                }
                            }
                        }
                        Object::Arg(_) => unimplemented!(),
                        Object::Var(_) => unimplemented!(),
                        Object::EnumVariant(_, _) => unimplemented!(),
                        Object::Generic(g) => {
                            let gen = Type::Generic(self.ctx.get_generic(g).clone());
                            Ok(ByteCode::Type(self.ctx.alloc_type(gen)))
                        }
                    },
                    None => Err(Error::custom(expr.span, format!("Not found {}", i))),
                },
            },
            ExprKind::Application(l, r) => self.eval(l.as_ref(), bc_ctx).and_then(|left| {
                self.eval(r.as_ref(), bc_ctx)
                    .and_then(|right| match left.inner() {
                        ByteCode::ApplicationFunction(mut args, func, ty) => {
                            args.push(right);
                            let ty = match self.ctx.get_type(ty) {
                                Type::Function(f) => f.return_value.clone(),
                                _otherwise => ty,
                            };
                            self.eval_func(args, func, ty)
                        }
                        _t => {
                            dbg!(_t);
                            unimplemented!()
                        }
                    })
            }),
            ExprKind::Let { var, assign, expr } => {
                let assigned = self.eval(assign.as_ref(), bc_ctx)?;
                let ctx = LocalContext {
                    objects: vec![ByteCode::Var(var.clone(), Box::new(assigned))],
                    parent: Some(bc_ctx),
                };
                self.eval(expr.as_ref(), &ctx)
            }
            ExprKind::CaseExpr { cond, arms } => {
                let cond = self.eval(&cond, bc_ctx)?;
                self.apply_pattern(cond.clone(), arms, bc_ctx)
            }
            ExprKind::DataVariant(v, idx) => {
                let v_ty = self.ctx.global.get_data(*v).get_variant_ty(*idx, self.ctx);
                self.eval_func(vec![], Callable::DataVariant(v.clone(), *idx), v_ty)
            }
            ExprKind::BinOp(l, r, op) => self.eval(l, bc_ctx).and_then(|left| {
                self.eval(r, bc_ctx)
                    .and_then(|right| ByteCode::bin_op(left, right, op.clone(), self.ctx))
            }),
            ExprKind::Neg(x) => match self.eval(x, bc_ctx)? {
                ByteCode::Int(i) => Ok(ByteCode::Int(-i)),
                _ => unreachable!("This must be checked by typecheker."),
            },
            ExprKind::Type(ty) => Ok(ByteCode::Type(*ty)),
            x => {
                dbg!(x);
                unimplemented!()
            }
        }
    }
    fn eval_func(
        &mut self,
        args: Vec<ByteCode>,
        cal: Callable,
        ty: Id<Type>,
    ) -> Result<ByteCode, Error> {
        match self.ctx.get_type(ty) {
            Type::Function(_) => Ok(ByteCode::ApplicationFunction(args, cal, ty)),
            _ => match cal {
                Callable::Func(def) => {
                    let imp = self.ctx.global.get_func(def);
                    self.eval(
                        &imp.body,
                        &LocalContext {
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
                Callable::DataVariant(v, idx) => {
                    let types = args.iter().map(|arg| arg.ty(&mut self.ctx.types)).collect();
                    Ok(ByteCode::DataVariant(Concrete::new(v, types), idx, args))
                }
                Callable::DataType(d) => {
                    let ty = self.ctx.alloc_type(Type::Data(Concrete::new(
                        d.clone(),
                        args.into_iter()
                            .map(|bc| match bc {
                                ByteCode::Type(ty) => ty.clone(),
                                _ => unimplemented!(),
                            })
                            .collect(),
                    )));

                    Ok(ByteCode::Type(ty))
                }
            },
        }
    }
    fn apply_pattern(
        &mut self,
        expr: ByteCode,
        arms: &[(Spanned<Pattern>, Expr)],
        ctx: &LocalContext<'_, ByteCode>,
    ) -> Result<ByteCode, Error> {
        for (pat, e) in arms {
            match is_aplyable(&expr, pat, self.ctx) {
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
        &mut self,
        pattern: &Pattern,
        obj: ByteCode,
        top: &'a LocalContext<'a, ByteCode>,
    ) -> LocalContext<'a, ByteCode> {
        let mut scope = LocalContext {
            objects: vec![],
            parent: Some(top),
        };
        match pattern {
            Pattern::Otherwise => {}
            Pattern::Bind(i, pat) => {
                scope.objects.push(ByteCode::Var(
                    Var {
                        name: i.clone(),
                        ty: obj.ty(&mut self.ctx.types),
                    },
                    Box::new(obj.clone()),
                ));
                scope.objects.append(
                    &mut self
                        .create_scope_for_pattern(pat.as_ref(), obj, top)
                        .objects,
                );
            }
            Pattern::Variant(path, pats) => {
                match self.ctx.find(PathToFind::from_path(path)) {
                    Some(Object::EnumVariant(_, _)) => {
                        let datas = match obj.clone().inner() {
                            ByteCode::DataVariant(_, _, datas) => datas,
                            _ => unreachable!(),
                        };
                        pats.iter().zip(datas.iter()).for_each(|(pat, dat)| {
                            let mut res = self.create_scope_for_pattern(pat, dat.clone(), &scope);
                            {
                                // https://github.com/rust-lang/rust/issues/59159
                                let objs = &mut res.objects;
                                scope.objects.append(objs);
                            }
                        })
                    }
                    _ => unreachable!(),
                }
            }
            Pattern::Ident(i) => scope.objects.push(ByteCode::Var(
                Var {
                    name: i.clone(),
                    ty: obj.ty(&mut self.ctx.types),
                },
                Box::new(obj.clone()),
            )),
        };
        scope
    }
}

fn is_aplyable(expr: &ByteCode, pat: &Pattern, ctx: &ScopeCtx) -> bool {
    match (&expr.clone().inner(), pat) {
        (_, Pattern::Otherwise) => true,
        (_, Pattern::Bind(_, pat)) => is_aplyable(expr, pat.as_ref(), ctx),
        (ByteCode::DataVariant(v1, idx, datas), Pattern::Variant(v2, pats)) => {
            ctx.global.get_data_variant(v1.base, *idx).name.as_str() == v2.end()
                && datas
                    .iter()
                    .zip(pats.iter())
                    .all(|(d, p)| is_aplyable(d, p, ctx))
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
    Func(Id<FunctionObject>),
    DataType(Id<DataDef>),
    DataVariant(Id<DataDef>, usize),
}

impl<'a> DisplayScope<'a> for Callable {
    type Scope = ScopeCtx<'a>;

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        match self {
            Callable::Func(c) => scope.global.get_func(*c).display_value(f, scope.global)?,
            Callable::DataType(c) => scope.global.get_data(*c).display_value(f, &())?,
            Callable::DataVariant(c, idx) => scope
                .global
                .get_data_variant(*c, *idx)
                .display_value(f, scope)?,
        };
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ByteCode {
    ApplicationFunction(Vec<ByteCode>, Callable, Id<Type>),
    Int(i128),
    Var(Var, Box<ByteCode>),
    Arg(Arg, Box<ByteCode>),
    DataVariant(Concrete<DataDef>, usize, Vec<ByteCode>),
    DataType(Concrete<DataDef>),
    Type(Id<Type>),
}

impl ByteCode {
    fn ty(&self, ctx: &mut Arena<Type>) -> Id<Type> {
        match self {
            ByteCode::ApplicationFunction(_, _, ty) => *ty,
            ByteCode::Int(_) => ctx.alloc(Type::Int),
            ByteCode::Var(v, _) => v.ty,
            ByteCode::Arg(a, _) => a.ty,
            ByteCode::DataVariant(v, _, _) => {
                ctx.alloc(Type::Data(Concrete::new(v.base, v.generics.clone())))
            }
            ByteCode::DataType(_) => ctx.alloc(Type::Type),
            ByteCode::Type(_) => ctx.alloc(Type::Type),
        }
    }

    fn inner(self) -> Self {
        match self {
            ByteCode::Var(_, b) => b.inner(),
            ByteCode::Arg(_, b) => b.inner(),
            _ => self,
        }
    }
}

fn int_to_bool(bc: ByteCode, ro: &ScopeCtx) -> ByteCode {
    match bc {
        ByteCode::Int(0) => match ro.find(PathToFind::new("False", smallvec!["Bool"])) {
            Some(Object::EnumVariant(v, x)) => ByteCode::DataVariant(Concrete::base(v), x, vec![]),
            _ => unreachable!(),
        },
        ByteCode::Int(1) => match ro.find(PathToFind::new("True", smallvec!["Bool"])) {
            Some(Object::EnumVariant(v, x)) => ByteCode::DataVariant(Concrete::base(v), x, vec![]),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

impl<'a> DisplayScope<'a> for ByteCode {
    type Scope = ScopeCtx<'a>;

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        match self {
            ByteCode::ApplicationFunction(bc, cal, ty) => {
                cal.display_value(f, scope)?;
                write!(
                    f,
                    "({}): ",
                    bc.iter().map(|bc| bc.display_value_string(scope)).join(",")
                )?;
                scope
                    .get_type(*ty)
                    .display_value(f, &(scope.types(), scope.global))?;
            }
            ByteCode::Int(i) => write!(f, "{}", i)?,
            ByteCode::Var(_, bc) => bc.display_value(f, scope)?,
            ByteCode::Arg(_, bc) => bc.display_value(f, scope)?,
            ByteCode::DataVariant(v, idx, bc) => {
                let name = scope.global.get_data_variant(v.base, *idx).name.as_str();
                if bc.len() == 0 {
                    f.write_str(name)?;
                } else {
                    write!(
                        f,
                        "({} {})",
                        name,
                        bc.iter()
                            .map(|x| x.display_value_string(scope))
                            .join(" ")
                            .as_str()
                    )?;
                };
            }
            ByteCode::DataType(d) => d.display_value(f, &(scope.types(), scope.global))?,
            ByteCode::Type(ty) => scope
                .get_type(*ty)
                .display_value(f, &(scope.types(), scope.global))?,
        }
        Ok(())
    }
}

impl ByteCode {
    fn bin_op(self, other: Self, op: BinOp, ro: &ScopeCtx) -> Result<Self, Error> {
        match op {
            BinOp::Add => self.add(other),
            BinOp::Sub => self.sub(other),
            BinOp::Pow => self.pow(other),
            BinOp::Div => self.div(other),
            BinOp::Le => self.le(other).map(|x| int_to_bool(x, ro)),
            BinOp::Gr => self.gr(other).map(|x| int_to_bool(x, ro)),
            BinOp::Eq => self.eq(other).map(|x| int_to_bool(x, ro)),
            _ => {
                dbg!(op);
                unimplemented!()
            }
        }
    }

    fn add(self, other: Self) -> Result<Self, Error> {
        match (self.value(), other.value()) {
            (ByteCode::Int(i), ByteCode::Int(i2)) => Ok(ByteCode::Int(i + i2)),
            _ => unimplemented!(),
        }
    }
    fn sub(self, other: Self) -> Result<Self, Error> {
        match (self.value(), other.value()) {
            (ByteCode::Int(i), ByteCode::Int(i2)) => Ok(ByteCode::Int(i - i2)),
            _ => unimplemented!(),
        }
    }
    fn div(self, other: Self) -> Result<Self, Error> {
        match (self.value(), other.value()) {
            (ByteCode::Int(i), ByteCode::Int(i2)) => Ok(ByteCode::Int(i / i2)),
            _ => unimplemented!(),
        }
    }
    fn pow(self, other: Self) -> Result<Self, Error> {
        match (self.value(), other.value()) {
            (ByteCode::Int(i), ByteCode::Int(i2)) => Ok(ByteCode::Int(i.pow(i2 as u32))),
            _ => unimplemented!(),
        }
    }
    fn le(self, other: Self) -> Result<Self, Error> {
        match (self.value(), other.value()) {
            (ByteCode::Int(i), ByteCode::Int(i2)) => Ok(ByteCode::Int((i < i2) as i128)),
            _ => unimplemented!(),
        }
    }
    fn gr(self, other: Self) -> Result<Self, Error> {
        match (self.value(), other.value()) {
            (ByteCode::Int(i), ByteCode::Int(i2)) => Ok(ByteCode::Int((i > i2) as i128)),
            _ => {
                unimplemented!()
            }
        }
    }
    fn eq(self, other: Self) -> Result<Self, Error> {
        match (self.value(), other.value()) {
            (ByteCode::Int(i), ByteCode::Int(i2)) => Ok(ByteCode::Int((i == i2) as i128)),
            _ => {
                unimplemented!()
            }
        }
    }
    fn value(self) -> Self {
        match self {
            ByteCode::Arg(_, x) | ByteCode::Var(_, x) => (*x).value(),
            _ => self,
        }
    }
}

impl HasName for ByteCode {
    fn name(&self) -> &str {
        match self {
            ByteCode::ApplicationFunction(_, _, _) => "",
            ByteCode::Int(_) => "",
            ByteCode::Var(v, _) => v.name.as_str(),
            ByteCode::Arg(a, _) => a.name.as_str(),
            ByteCode::DataVariant(_, _, _) => "",
            ByteCode::DataType(_) => "",
            ByteCode::Type(_) => "",
        }
    }
}
