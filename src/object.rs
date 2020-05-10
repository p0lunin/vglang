use crate::error::Error;
use crate::parser::{Ast, FunctionDef, FunctionImpl, Token};
use crate::spanned::{Span, Spanned};
use crate::type_check::Context;
use crate::types::{parse_type_helper, Int, Type, TypeKind, TypeType, VecType, OneTypeKind, Function};
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
    pub fn get_type(&self) -> Rc<Spanned<Type>> {
        match self {
            AllObject::Type(t) => (*t.object).clone(),
            AllObject::Function(t) => t.object_type.clone(),
            AllObject::Var(t) => t.object_type.clone(),
        }
    }
    pub fn call(&self) -> Rc<Spanned<Type>> {
        match self {
            AllObject::Type(t) => (*t.object).clone(),
            AllObject::Function(t) => t.call(),
            AllObject::Var(t) => t.object_type.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Object<T: Objectable> {
    pub object: Spanned<T>,
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
    pub arg: Option<Rc<Object<Var>>>,
    pub return_value: Rc<AllObject>,
    pub body: Option<Expr>,
}

impl FunctionObject {
    pub fn get_return_type(&self) -> &Type {
        match &*self.return_value {
            AllObject::Type(t) => &t.object,
            AllObject::Function(f) => f.object.get_return_type(),
            _ => unreachable!(),
        }
    }

    pub fn get_body(&self) -> Expr {
        match &self.body {
            Some(expr) => expr.clone(),
            None => match &*self.return_value {
                AllObject::Function(f) => f.object.get_body(),
                _ => unreachable!(),
            }
        }
    }

    pub fn create_ctx<'a>(&self, top: &'a Context) -> Context<'a> {
        let args = self.get_args();
        Context {
            objects: args,
            parent: Some(top),
        }
    }

    fn get_args(&self) -> Vec<AllObject> {
        let mut args = vec![];
        match &self.arg {
            Some(arg) => args.push(AllObject::Var(arg.clone())),
            None => {
                match &*self.return_value {
                    AllObject::Type(t) => {},
                    AllObject::Function(f) => args.append(f.object.get_args().as_mut()),
                    _ => unreachable!(),
                }
            }
        };
        args
    }
}

impl Object<FunctionObject> {
    pub fn call(&self) -> Rc<Spanned<Type>> {
        self.object_type.clone()
    }
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Function:\n")?;
        write!(f, "Name: {}\n", self.name)?;
        write!(f, "Arg: {}\n", self.arg.as_ref().map(|var| var.object_type.to_string()).unwrap_or("None".to_owned()))?;
        write!(f, "Return: \n{}", self.return_value)?;
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
    CallFunction(Spanned<Rc<Object<FunctionObject>>>),
    Var(Spanned<Rc<Object<Var>>>),
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
    match args.len() == count_args as usize {
        false => Err(Error::Custom(
            impl_name.span,
            format!("Expected {} arguments, found {}", count_args, args.len()),
            "-here".to_owned(),
        )),
        true => {
            let arg_names = args.iter().map(|i| Spanned::new(i.0.clone(), i.span)).collect::<Vec<_>>();
            let args = args
                .into_iter()
                .zip(arg_types.clone().into_iter())
                .map(|(v, t)| {
                    let span = v.span;
                    Rc::new(Object {
                        object: Spanned::new(Var(v.map(|i| i.0)), span),
                        object_type: t,
                    })
                })
                .collect::<Vec<_>>();
            let ctx = Context {
                objects: args.iter().map(|v| AllObject::Var(v.clone())).collect(),
                parent: Some(ctx),
            };
            let expr = parse_expr(body.0, &ctx)?;
            Ok(parse_function_helper(name, arg_names, func_type, impl_name.span, expr))
        }
    }
}

fn parse_function_helper(
    name: Spanned<String>,
    mut arg_names: Vec<Spanned<String>>,
    mut func_type: Rc<Spanned<Type>>,
    span: Span,
    body: Expr,
) -> AllObject {
    match &**func_type {
        Type::Function(t) => {
            let Function { get_value, return_value } = t.kind.clone().inner();
            AllObject::Function(Rc::new(Object {
                object: Spanned::new(
                    FunctionObject {
                        name: name.clone(),
                        arg: Some(Rc::new(Object {
                            object: Spanned::new(Var(arg_names.remove(0)), span),
                            object_type: get_value,
                        })),
                        return_value: Rc::new(parse_function_helper(name, arg_names, return_value, span, body)),
                        body: None,
                    },
                    span,
                ),
                object_type: func_type,
            }))
        }
        t => {
            AllObject::Function(Rc::new(Object {
                object: Spanned::new(
                    FunctionObject {
                        name,
                        arg: None,
                        return_value: Rc::new(AllObject::Type(Rc::new(Object {
                            object: Spanned::new(func_type.clone(), span),
                            object_type: Rc::new(Spanned::new(TypeType, span)),
                        }))),
                        body: Some(body),
                    },
                    span,
                ),
                object_type: func_type,
            }))
        }
    }
}

pub fn parse_expr(token: Token, ctx: &Context) -> Result<Expr, Error> {
    match token.ast {
        Ast::Int(i) => Ok(Expr::Int(Object {
            object: Spanned::new(
                IntObject {
                    data: Spanned::new(i, token.span),
                },
                token.span,
            ),
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
                AllObject::Function(f) => {
                    Ok(Expr::CallFunction(Spanned::new(f.clone(), token.span)))
                }
                AllObject::Var(v) => Ok(Expr::Var(Spanned::new(v.clone(), token.span))),
                AllObject::Type(t) => Ok(Expr::Type(Rc::new(Object {
                    object: Spanned::new(
                        Rc::new(Spanned::new(
                            Type::AnotherType(t.object.clone()),
                            token.span,
                        )),
                        token.span,
                    ),
                    object_type: t.object_type.clone(),
                }))),
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
