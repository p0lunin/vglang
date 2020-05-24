use crate::error::Error;
use crate::parser::{Ast, FunctionDef, FunctionImpl, Token};
use crate::r#enum::{CreateEnumVariantFunc, EnumType, EnumVariant, EnumVariantInstance};
use crate::spanned::{AddSpan, Span, Spanned};
use crate::type_check::Context;
use crate::types::{parse_type_helper, Function, Int, Type, TypeKind, TypeType, VecType};
use either::Either;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum AllObject {
    Type(Rc<Object<Rc<Spanned<Type>>>>),
    FunctionDefinition(Rc<FunctionDefinition>),
    Enum(Rc<EnumType>),
    EnumVariant(Rc<EnumVariant>),
    EnumVariantInstance(Rc<EnumVariantInstance>),
    CreateEnumVariantFunc(CreateEnumVariantFunc),
    Function(Rc<Object<FunctionObject>>),
    CurriedFunction(Rc<CurriedFunction>),
    Arg(Rc<Object<Arg>>),
    Var(Rc<Object<Var>>),
}

impl Display for AllObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            AllObject::Type(t) => f.write_str(&format!("{}", t.object)),
            AllObject::Function(t) => f.write_str(&format!("{}", t)),
            AllObject::Var(v) => unimplemented!(),
            AllObject::FunctionDefinition(f) => unimplemented!(),
            AllObject::CurriedFunction(_) => unimplemented!(),
            AllObject::Arg(_) => unimplemented!(),
            AllObject::Enum(e) => Display::fmt(e, f),
            AllObject::EnumVariant(e) => Display::fmt(e, f),
            AllObject::CreateEnumVariantFunc(e) => unimplemented!(),
            AllObject::EnumVariantInstance(_) => unimplemented!(),
        }
    }
}

impl AllObject {
    pub fn name(&self) -> &str {
        match self {
            AllObject::Type(t) => &t.name,
            AllObject::Function(t) => &t.name,
            AllObject::Var(t) => &t.name,
            AllObject::FunctionDefinition(t) => &t.name,
            AllObject::CurriedFunction(f) => &f.orig.name(),
            AllObject::Arg(a) => &a.name,
            AllObject::Enum(e) => e.name(),
            AllObject::EnumVariant(e) => e.name(),
            AllObject::CreateEnumVariantFunc(e) => e.name(),
            AllObject::EnumVariantInstance(e) => e.name(),
        }
    }
    pub fn call(&self) -> Rc<Spanned<Type>> {
        match self {
            AllObject::Type(t) => (*t.object).clone(),
            AllObject::Function(t) => t.object.ftype.clone(),
            AllObject::Var(t) => t.object.get_type(),
            AllObject::FunctionDefinition(f) => f.ftype.clone(),
            AllObject::CurriedFunction(f) => f.ftype.clone(),
            AllObject::Arg(a) => a.object.atype.clone(),
            AllObject::Enum(e) => Rc::new(Spanned::new(Type::Enum(e.clone()), e.span())),
            AllObject::EnumVariant(e) => {
                Rc::new(Spanned::new(Type::EnumVariant(e.clone()), e.span()))
            }
            AllObject::CreateEnumVariantFunc(f) => f.call(),
            AllObject::EnumVariantInstance(e) => {
                Rc::new(Spanned::new(Type::EnumVariantInstance(e.clone()), e.span()))
            }
        }
    }
    pub fn call_with_arg_expr(&self, arg: Expr, span: Span) -> Result<AllObject, Error> {
        match self {
            AllObject::Function(f) => Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                ftype: f.object.ftype.try_curry().ok_or(Error::Span(span))?.clone(),
                scope: vec![arg],
                orig: Callable::Func(f.clone()),
            }))),
            AllObject::Type(_) => Err(Error::Span(span)),
            AllObject::FunctionDefinition(def) => {
                Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                    ftype: def.ftype.try_curry().ok_or(Error::Span(span))?.clone(),
                    scope: vec![arg],
                    orig: Callable::FuncDef(def.clone()),
                })))
            }
            AllObject::CurriedFunction(f) => {
                let mut scope = f.scope.clone();
                scope.push(arg);
                let new_type = f.ftype.try_curry().ok_or(Error::Span(span))?;
                Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                    ftype: new_type,
                    scope,
                    orig: f.orig.clone(),
                })))
            }
            AllObject::Var(v) => v.object.data.call_with_arg_expr(arg, span),
            AllObject::Arg(a) => Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                ftype: a.object.atype.try_curry().ok_or(Error::Span(span))?,
                scope: vec![arg],
                orig: Callable::Arg(a.clone()),
            }))),
            AllObject::Enum(_) => Err(Error::Span(span)), // TODO: implement
            AllObject::EnumVariant(e) => Ok(e.call_with_arg_expr(arg)),
            AllObject::CreateEnumVariantFunc(f) => f.call_with_arg_expr(arg),
            AllObject::EnumVariantInstance(_) => Err(Error::Span(span)),
        }
    }
    pub fn try_get_member(&self, member: &str, span: Span) -> Result<AllObject, Error> {
        match self {
            AllObject::Enum(e) => e.try_get_member(member).ok_or(Error::Span(span)),
            AllObject::Var(v) => v.object.data.try_get_member(member, span),
            _ => Err(Error::Span(span)),
        }
    }
    pub fn type_check_self(&self, ctx: &Context) -> Result<Rc<Spanned<Type>>, Error> {
        match self {
            AllObject::EnumVariantInstance(i) => i.type_check_self(ctx),
            _ => Ok(self.call()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Object<T> {
    pub name: Spanned<String>,
    pub object: Spanned<T>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CurriedFunction {
    pub ftype: Rc<Spanned<Type>>,
    pub scope: Vec<Expr>,
    pub orig: Callable,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: Spanned<String>,
    pub ftype: Rc<Spanned<Type>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    Func(Rc<Object<FunctionObject>>),
    FuncDef(Rc<FunctionDefinition>),
    Arg(Rc<Object<Arg>>),
}

impl Callable {
    pub fn name(&self) -> &Spanned<String> {
        match self {
            Callable::Func(f) => &f.name,
            Callable::FuncDef(def) => &def.name,
            Callable::Arg(a) => &a.name,
        }
    }
    pub fn ftype(&self) -> Rc<Spanned<Type>> {
        match self {
            Callable::Func(f) => f.object.ftype.clone(),
            Callable::FuncDef(def) => def.ftype.clone(),
            Callable::Arg(a) => a.object.atype.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionObject {
    pub args: Vec<Rc<Object<Arg>>>,
    pub ftype: Rc<Spanned<Type>>,
    pub body: Rc<Expr>,
}

impl FunctionObject {
    pub fn get_return_type(&self) -> &Type {
        self.ftype.get_return_value()
    }

    pub fn create_ctx<'a>(&self, top: &'a Context) -> Context<'a> {
        let args = self.get_args();
        Context {
            objects: args,
            parent: Some(top),
        }
    }

    fn get_args(&self) -> Vec<AllObject> {
        self.args
            .iter()
            .map(|arg| AllObject::Arg(arg.clone()))
            .collect()
    }
}

impl Display for Object<FunctionObject> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Function:\n")?;
        write!(f, "Name: {}\n", self.name)?;
        write!(f, "Type: {}\n", self.object.ftype)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(Spanned<i128>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Gr(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
    GrOrEq(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    LeOrEq(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Object(Spanned<AllObject>),
}

macro_rules! impl_op {
    ($self:tt, $other:tt, $op:tt, $variant:tt) => {
        match ($self, $other) {
            (Expr::Int(i), Expr::Int(n)) => {
                Expr::Int(Spanned::new(*i $op *n, i.span.extend(&n.span)))
            }
            (l, r) => Expr::$variant(Box::new(l), Box::new(r)),
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(i) => i.span,
            Expr::Add(l, r) => l.span().extend(&r.span()),
            Expr::Sub(l, r) => l.span().extend(&r.span()),
            Expr::Object(o) => o.span,
            Expr::Mul(l, r) => l.span().extend(&r.span()),
            Expr::Div(l, r) => l.span().extend(&r.span()),
            Expr::Pow(l, r) => l.span().extend(&r.span()),
            Expr::And(l, r) => l.span().extend(&r.span()),
            Expr::Or(l, r) => l.span().extend(&r.span()),
            Expr::Gr(l, r) => l.span().extend(&r.span()),
            Expr::Eq(l, r) => l.span().extend(&r.span()),
            Expr::NotEq(l, r) => l.span().extend(&r.span()),
            Expr::GrOrEq(l, r) => l.span().extend(&r.span()),
            Expr::Le(l, r) => l.span().extend(&r.span()),
            Expr::LeOrEq(l, r) => l.span().extend(&r.span()),
            Expr::Neg(l) => l.span(),
        }
    }
    pub fn add(self, other: Expr) -> Self {
        impl_op!(self, other, +, Add)
    }
    pub fn mul(self, other: Expr) -> Self {
        impl_op!(self, other, *, Mul)
    }
    pub fn sub(self, other: Expr) -> Self {
        impl_op!(self, other, -, Sub)
    }
    pub fn div(self, other: Expr) -> Self {
        impl_op!(self, other, /, Div)
    }
    pub fn pow(self, other: Expr) -> Self {
        match (self, other) {
            (Expr::Int(i), Expr::Int(n)) => Expr::Int(Spanned::new(
                (*i as f64).powi(*n as i32) as i128,
                i.span.extend(&n.span),
            )),
            (l, r) => Expr::Pow(Box::new(l), Box::new(r)),
        }
    }
    pub fn and(self, other: Expr) -> Self {
        Expr::And(Box::new(self), Box::new(other))
    }
    pub fn or(self, other: Expr) -> Self {
        Expr::Or(Box::new(self), Box::new(other))
    }
    pub fn eq(self, other: Expr) -> Self {
        Expr::Eq(Box::new(self), Box::new(other))
    }
    pub fn not_eq(self, other: Expr) -> Self {
        Expr::NotEq(Box::new(self), Box::new(other))
    }
    pub fn gr(self, other: Expr) -> Self {
        Expr::Gr(Box::new(self), Box::new(other))
    }
    pub fn gr_or_eq(self, other: Expr) -> Self {
        Expr::GrOrEq(Box::new(self), Box::new(other))
    }
    pub fn le(self, other: Expr) -> Self {
        Expr::Le(Box::new(self), Box::new(other))
    }
    pub fn le_or_eq(self, other: Expr) -> Self {
        Expr::LeOrEq(Box::new(self), Box::new(other))
    }
    pub fn neg(self) -> Self {
        Expr::Neg(Box::new(self))
    }
    pub fn try_get_type(&self) -> Option<Type> {
        match self {
            Expr::Int(i) => Some(Type::Int(TypeKind::from_kinds(Spanned::new(
                VecType::one(Int::Value(**i)),
                i.span,
            )))),
            Expr::Object(o) => Some((**o.call()).clone()),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub data: AllObject,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Arg {
    pub atype: Rc<Spanned<Type>>,
}

impl Var {
    pub fn new(data: AllObject) -> Self {
        Self { data }
    }
    pub fn get_type(&self) -> Rc<Spanned<Type>> {
        self.data.call()
    }
}

pub fn parse_function(
    def: FunctionDef,
    fimpl: FunctionImpl,
    ctx: &Context,
) -> Result<AllObject, Error> {
    let FunctionDef(ident, def) = def;
    let name = ident.map(|i| i.0);
    let def_span = def.span;
    let func_type = Rc::new(Spanned::new(parse_type_helper(*def, ctx)?, def_span));
    let count_args = func_type.count_args();
    let FunctionImpl(impl_name, args, body) = fimpl;
    let arg_types = func_type.args_types();
    match args.len() == count_args as usize {
        false => Err(Error::Custom(
            impl_name.span,
            format!("Expected {} arguments, found {}", count_args, args.len()),
            "-here".to_owned(),
        )),
        true => {
            let args = args
                .into_iter()
                .zip(arg_types.clone().into_iter())
                .map(|(v, t)| {
                    let span = v.span;
                    Rc::new(Object {
                        name: v.map(|i| i.0),
                        object: Spanned::new(Arg { atype: t }, span),
                    })
                })
                .collect::<Vec<_>>();
            let ctx = Context {
                objects: func_type
                    .types_in_scope()
                    .into_iter()
                    .map(|(name, ty)| {
                        let span = ty.span;
                        AllObject::Type(Rc::new(Object {
                            name,
                            object: Spanned::new(ty, span),
                        }))
                    })
                    .collect(),
                parent: Some(ctx),
            };
            let mut ctx = Context {
                objects: args.iter().map(|v| AllObject::Arg(v.clone())).collect(),
                parent: Some(&ctx),
            };
            ctx.objects
                .push(AllObject::FunctionDefinition(Rc::new(FunctionDefinition {
                    name: name.clone(),
                    ftype: func_type.clone(),
                })));
            let expr = parse_expr(body.0, &ctx)?;

            let span = name.span;
            Ok(AllObject::Function(Rc::new(Object {
                name,
                object: Spanned::new(
                    FunctionObject {
                        args,
                        ftype: func_type,
                        body: Rc::new(expr),
                    },
                    span,
                ),
            })))
        }
    }
}

pub fn parse_expr(token: Token, ctx: &Context) -> Result<Expr, Error> {
    match token.ast {
        Ast::Int(i) => Ok(Expr::Int(Spanned::new(i, token.span))),
        Ast::Add(l, r) => Ok(parse_expr(*l, ctx)?.add(parse_expr(*r, ctx)?)),
        Ast::Sub(l, r) => Ok(parse_expr(*l, ctx)?.sub(parse_expr(*r, ctx)?)),
        Ast::Ident(i) => match ctx.find(&i.0) {
            Some(o) => Ok(Expr::Object(o.clone().add_span(token.span))),
            _ => Err(Error::Custom(
                token.span,
                format!("{} not found", i.0),
                "-here".to_owned(),
            )),
        },
        Ast::CallFunction(func, arg) => {
            let left_expr = parse_expr(*func, ctx)?;
            let arg_expr = parse_expr(*arg, ctx)?;
            match left_expr {
                Expr::Object(o) => Ok(Expr::Object(
                    o.call_with_arg_expr(arg_expr, token.span)?
                        .add_span(token.span),
                )),
                _ => Err(Error::Span(token.span)),
            }
        }
        Ast::Dot(l, r) => {
            let left_expr = parse_expr(*l, ctx)?;
            let right_expr = match r.ast {
                Ast::Ident(i) => i.0,
                _ => return Err(Error::Span(r.span)),
            };
            let span = token.span;
            match left_expr {
                Expr::Object(o) => o
                    .try_get_member(right_expr.as_str(), span)
                    .map(|o| Spanned::new(o, span))
                    .map(Expr::Object),
                _ => Err(Error::Span(span)),
            }
        }
        Ast::Parenthesis(p) => parse_expr(*p, ctx),
        Ast::Mul(l, r) => Ok(parse_expr(*l, ctx)?.mul(parse_expr(*r, ctx)?)),
        Ast::Div(l, r) => Ok(parse_expr(*l, ctx)?.div(parse_expr(*r, ctx)?)),
        Ast::Pow(l, r) => Ok(parse_expr(*l, ctx)?.pow(parse_expr(*r, ctx)?)),
        Ast::And(l, r) => Ok(parse_expr(*l, ctx)?.and(parse_expr(*r, ctx)?)),
        Ast::Or(l, r) => Ok(parse_expr(*l, ctx)?.or(parse_expr(*r, ctx)?)),
        Ast::Gr(l, r) => Ok(parse_expr(*l, ctx)?.gr(parse_expr(*r, ctx)?)),
        Ast::Le(l, r) => Ok(parse_expr(*l, ctx)?.le(parse_expr(*r, ctx)?)),
        Ast::GrEq(l, r) => Ok(parse_expr(*l, ctx)?.gr_or_eq(parse_expr(*r, ctx)?)),
        Ast::LeEq(l, r) => Ok(parse_expr(*l, ctx)?.le_or_eq(parse_expr(*r, ctx)?)),
        Ast::Eq(l, r) => Ok(parse_expr(*l, ctx)?.eq(parse_expr(*r, ctx)?)),
        Ast::NotEq(l, r) => Ok(parse_expr(*l, ctx)?.not_eq(parse_expr(*r, ctx)?)),
        Ast::Neg(t) => Ok(parse_expr(*t, ctx)?.neg()),
        Ast::Double(_) => unimplemented!(),
        Ast::Val => Err(Error::Span(token.span)),
        Ast::Slice(_) => unimplemented!(),
        Ast::Implication(_, _) => Err(Error::Span(token.span)),
        Ast::Named(_, _) => Err(Error::Span(token.span)),
    }
}
