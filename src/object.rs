use crate::error::Error;
use crate::parser::{Ast, FunctionDef, FunctionImpl, Ident, Token};
use crate::spanned::Spanned;
use crate::type_check::Context;
use crate::types::{
    parse_type, parse_type_helper, Int, Type, TypeKind, TypeOperable, TypeType, VecType,
};
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum AllObject {
    Type(Rc<Object<Rc<Spanned<Type>>>>),
    Function(Rc<Object<FunctionObject>>),
    Var(Rc<Object<Var>>),
}

impl Display for AllObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            AllObject::Type(t) => f.write_str(&format!("{}", t.object)),
            AllObject::Function(t) => f.write_str(&format!("{}", t.object)),
            AllObject::Var(v) => unimplemented!(),
        }
    }
}

impl AllObject {
    pub fn name(&self) -> &str {
        match self {
            AllObject::Type(t) => t.object.name(),
            AllObject::Function(f) => &f.object.name,
            AllObject::Var(v) => v.object.0.as_str(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Object<T: Objectable> {
    pub object: T,
    pub object_type: Rc<Spanned<T::Type>>,
}

pub trait Objectable {
    type Type;
}

impl Objectable for Rc<Spanned<Type>> {
    type Type = TypeType;
}

impl Objectable for TypeType {
    type Type = TypeType;
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionObject {
    pub name: Spanned<String>,
    pub args: Vec<Rc<Object<Var>>>,
    pub return_value: Rc<Spanned<Type>>,
    pub body: Expr,
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Function:\n")?;
        write!(f, "Name: {}\n", self.name)?;
        write!(f, "Args:\n")?;
        self.args.iter().for_each(|a| {
            write!(f, "    {}\n", a.object_type);
        });
        write!(f, "Return: {}\n", self.return_value)?;
        Ok(())
    }
}

impl Objectable for FunctionObject {
    type Type = Type;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(Object<IntObject>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    CallFunction(Rc<Object<FunctionObject>>),
    Var(Rc<Object<Var>>),
    Type(Rc<Object<Rc<Spanned<Type>>>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var(pub Spanned<String>);
impl Objectable for Var {
    type Type = Type;
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntObject {
    pub data: Spanned<i128>,
}
impl Objectable for IntObject {
    type Type = Type;
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
    let mut arg_types = func_type.args_types();
    let return_type = arg_types.pop().unwrap();
    match args.len() == count_args as usize {
        false => Err(Error::Custom(
            impl_name.span,
            format!("Expected {} arguments, found {}", count_args, args.len()),
            "-here".to_owned(),
        )),
        true => {
            let args = args
                .into_iter()
                .zip(arg_types.into_iter())
                .map(|(v, t)| {
                    Rc::new(Object {
                        object: Var(v.map(|i| i.0)),
                        object_type: t,
                    })
                })
                .collect::<Vec<_>>();
            let ctx = Context {
                objects: args.iter().map(|v| AllObject::Var(v.clone())).collect(),
                parent: Some(ctx),
            };
            Ok(AllObject::Function(Rc::new(Object {
                object: FunctionObject {
                    name,
                    args,
                    return_value: return_type,
                    body: parse_expr(body.0, &ctx)?,
                },
                object_type: func_type,
            })))
        }
    }
}

pub fn parse_expr(token: Token, ctx: &Context) -> Result<Expr, Error> {
    match token.ast {
        Ast::Int(i) => Ok(Expr::Int(Object {
            object: IntObject {
                data: Spanned::new(i, token.span),
            },
            object_type: Rc::new(Spanned::new(
                Type::Int(TypeKind::from_kinds(Spanned::new(
                    VecType::one(Int::Value(i)),
                    token.span,
                ))),
                token.span,
            )),
        })),
        Ast::Add(l, r) => Ok(Expr::Add(
            Box::new(parse_expr(*l, ctx)?),
            Box::new(parse_expr(*r, ctx)?),
        )),
        Ast::Sub(l, r) => Ok(Expr::Sub(
            Box::new(parse_expr(*l, ctx)?),
            Box::new(parse_expr(*r, ctx)?),
        )),
        Ast::Ident(i) => match ctx.find(&i.0) {
            Some(o) => match o {
                AllObject::Function(f) => Ok(Expr::CallFunction(f.clone())),
                AllObject::Var(v) => Ok(Expr::Var(v.clone())),
                AllObject::Type(t) => Ok(Expr::Type(
                    Rc::new(Object {
                        object: Rc::new(Spanned::new(Type::AnotherType(Spanned::new(t.object.clone(), token.span)), token.span)),
                        object_type: t.object_type.clone(),
                    })
                )),
                _ => unimplemented!(),
            },
            _ => Err(Error::Custom(
                token.span,
                format!("{} not found", i.0),
                "-here".to_owned(),
            )),
        },
        _ => unimplemented!(),
    }
}
