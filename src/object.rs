use crate::error::Error;
use crate::parser::{Ast, FunctionDef, FunctionImpl, Ident, Token};
use crate::spanned::Spanned;
use crate::types::{
    parse_type, parse_type_helper, Int, Type, TypeKind, TypeOperable, TypeType, VecType,
};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum AllObject {
    Type(Object<Rc<Spanned<Type>>>),
    Function(Object<FunctionObject>),
    Var(Object<Var>),
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
    pub args: Vec<Object<Var>>,
    pub return_value: Rc<Spanned<Type>>,
    pub body: Expr,
}
impl Objectable for FunctionObject {
    type Type = Type;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(Object<IntObject>),
    Add(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var(Spanned<Ident>);
impl Objectable for Var {
    type Type = Type;
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntObject {
    data: Spanned<i128>,
}
impl Objectable for IntObject {
    type Type = Type;
}

pub fn parse_function(
    def: FunctionDef,
    fimpl: FunctionImpl,
    types: &[Rc<Spanned<Type>>],
) -> Result<AllObject, Error> {
    let FunctionDef(ident, def) = def;
    let name = ident.map(|i| i.0);
    let def_span = def.span;
    let func_type = Rc::new(Spanned::new(parse_type_helper(*def, types)?, def_span));
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
        true => Ok(AllObject::Function(Object {
            object: FunctionObject {
                name,
                args: args
                    .into_iter()
                    .zip(arg_types.into_iter())
                    .map(|(v, t)| Object {
                        object: Var(v),
                        object_type: t,
                    })
                    .collect(),
                return_value: return_type,
                body: parse_expr(body.0)?,
            },
            object_type: func_type,
        })),
    }
}

pub fn parse_expr(token: Token) -> Result<Expr, Error> {
    match token.ast {
        Ast::Int(i) => Ok(Expr::Int(Object {
            object: IntObject {
                data: Spanned::new(i, token.span),
            },
            object_type: Rc::new(Spanned::new(
                Type::Int(TypeKind::from_kinds(VecType::one(Spanned::new(
                    Int::Value(Spanned::new(i, token.span)),
                    token.span,
                )))),
                token.span,
            )),
        })),
        Ast::Add(l, r) => Ok(Expr::Add(
            Box::new(parse_expr(*l)?),
            Box::new(parse_expr(*r)?),
        )),
        _ => unimplemented!(),
    }
}
