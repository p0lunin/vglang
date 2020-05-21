use crate::error::Error;
use crate::parser::{Ast, FunctionDef, FunctionImpl, Token};
use crate::spanned::{AddSpan, Span, Spanned};
use crate::type_check::Context;
use crate::types::{parse_type_helper, Function, Type, TypeType};
use either::Either;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum AllObject {
    Type(Rc<Object<Rc<Spanned<Type>>>>),
    FunctionDefinition(Rc<FunctionDefinition>),
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
    Object(Spanned<AllObject>),
}

impl Expr {
    pub fn add(self, other: Expr) -> Self {
        match (self, other) {
            (Expr::Int(i), Expr::Int(n)) => {
                Expr::Int(Spanned::new(*i + *n, i.span.extend(&n.span)))
            }
            (l, r) => Expr::Add(Box::new(l), Box::new(r)),
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

#[derive(Debug, PartialEq, Clone)]
pub struct IntObject {
    pub data: Spanned<i128>,
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
        Ast::Sub(l, r) => Ok(Expr::Sub(
            Box::new(parse_expr(*l, ctx)?),
            Box::new(parse_expr(*r, ctx)?),
        )),
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
        _ => unimplemented!(),
    }
}
