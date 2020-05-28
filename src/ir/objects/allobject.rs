use std::rc::Rc;
use crate::ir::objects::common::Object;
use std::cell::RefCell;
use crate::ir::objects::{EnumType, FunctionDefinition, EnumInstance, EnumVariant, EnumVariantInstance, CreateEnumVariantFunc, CreateEnumInstanceFunc, FunctionObject, CurriedFunction, Arg, Var, Callable};
use std::fmt::{Display, Formatter};
use crate::common::{Error, Span, Context, HasName};
use crate::ir::expr::Expr;
use crate::ir::types::Type;

#[derive(Debug, PartialEq, Clone)]
pub enum AllObject {
    Type(Rc<Object<Rc<RefCell<Type>>>>),
    FunctionDefinition(Rc<FunctionDefinition>),
    Enum(Rc<EnumType>),
    EnumInstance(Rc<EnumInstance>),
    EnumVariant(Rc<EnumVariant>),
    EnumVariantInstance(Rc<EnumVariantInstance>),
    CreateEnumVariantFunc(CreateEnumVariantFunc),
    CreateEnumInstanceFunc(CreateEnumInstanceFunc),
    Function(Rc<Object<FunctionObject>>),
    CurriedFunction(Rc<CurriedFunction>),
    Arg(Rc<Object<Arg>>),
    Var(Rc<Object<Var>>),
}

impl Display for AllObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            AllObject::Type(t) => f.write_str(&format!("{}", t.object.borrow())),
            AllObject::Function(t) => f.write_str(&format!("{}", t)),
            AllObject::Var(_) => unimplemented!(),
            AllObject::FunctionDefinition(_) => unimplemented!(),
            AllObject::CurriedFunction(_) => unimplemented!(),
            AllObject::Arg(_) => unimplemented!(),
            AllObject::Enum(e) => Display::fmt(e, f),
            AllObject::EnumVariant(e) => Display::fmt(e, f),
            AllObject::CreateEnumVariantFunc(_) => unimplemented!(),
            AllObject::EnumVariantInstance(_) => unimplemented!(),
            AllObject::EnumInstance(_) => unimplemented!(),
            AllObject::CreateEnumInstanceFunc(_) => unimplemented!(),
        }
    }
}

impl HasName for AllObject {
    fn name(&self) -> &str {
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
            AllObject::EnumInstance(e) => e.name(),
            AllObject::CreateEnumInstanceFunc(e) => e.name(),
        }
    }
}

impl AllObject {
    pub fn call(&self) -> Result<AllObject, Error> {
        match self {
            AllObject::Enum(e) => e.call(),
            t => Ok(t.clone()),
        }
    }
    pub fn get_type(&self) -> Rc<RefCell<Type>> {
        match self {
            AllObject::Type(t) => t.object.clone(),
            AllObject::Function(t) => t.object.ftype.clone(),
            AllObject::Var(t) => t.object.get_type(),
            AllObject::FunctionDefinition(f) => f.ftype.clone(),
            AllObject::CurriedFunction(f) => f.ftype.clone(),
            AllObject::Arg(a) => a.object.atype.clone(),
            AllObject::Enum(e) => Rc::new(RefCell::new(Type::Enum(e.clone()))),
            AllObject::EnumVariant(e) => Rc::new(RefCell::new(Type::EnumVariant(e.clone()))),
            AllObject::CreateEnumVariantFunc(f) => f.call(),
            AllObject::EnumVariantInstance(e) => {
                Rc::new(RefCell::new(Type::EnumVariantInstance(e.clone())))
            }
            AllObject::EnumInstance(e) => e.call(),
            AllObject::CreateEnumInstanceFunc(e) => e.call(),
        }
    }
    pub fn call_with_arg_expr(&self, arg: Expr, span: Span) -> Result<AllObject, Error> {
        match self {
            AllObject::Function(f) => Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                ftype: f
                    .object
                    .ftype
                    .borrow()
                    .try_curry()
                    .ok_or(Error::Span(span))?
                    .clone(),
                scope: vec![arg],
                orig: Callable::Func(f.clone()),
            }))),
            AllObject::Type(_) => Err(Error::Span(span)),
            AllObject::FunctionDefinition(def) => {
                Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                    ftype: def
                        .ftype
                        .borrow()
                        .try_curry()
                        .ok_or(Error::Span(span))?
                        .clone(),
                    scope: vec![arg],
                    orig: Callable::FuncDef(def.clone()),
                })))
            }
            AllObject::CurriedFunction(f) => {
                let mut scope = f.scope.clone();
                scope.push(arg);
                let new_type = f.ftype.borrow().try_curry().ok_or(Error::Span(span))?;
                Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                    ftype: new_type,
                    scope,
                    orig: f.orig.clone(),
                })))
            }
            AllObject::Var(v) => v.object.data.call_with_arg_expr(arg, span),
            AllObject::Arg(a) => Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                ftype: a
                    .object
                    .atype
                    .borrow()
                    .try_curry()
                    .ok_or(Error::Span(span))?,
                scope: vec![arg],
                orig: Callable::Arg(a.clone()),
            }))),
            AllObject::Enum(e) => e.call_with_arg_expr(arg, span),
            AllObject::EnumVariant(e) => Ok(e.call_with_arg_expr(arg)),
            AllObject::CreateEnumVariantFunc(f) => f.call_with_arg_expr(arg),
            AllObject::EnumVariantInstance(_) => Err(Error::Span(span)),
            AllObject::EnumInstance(_) => Err(Error::Span(span)),
            AllObject::CreateEnumInstanceFunc(e) => e.call_with_arg_expr(arg)
        }
    }
    pub fn try_get_member(&self, member: &str, span: Span) -> Result<AllObject, Error> {
        match self {
            AllObject::EnumInstance(e) => e.try_get_member(member).ok_or(Error::Span(span)),
            AllObject::Var(v) => v.object.data.try_get_member(member, span),
            AllObject::Enum(e) => e.try_get_member(member).ok_or(Error::Span(span)),
            _ => Err(Error::Span(span)),
        }
    }
    pub fn type_check_self(&self, ctx: &Context<'_, AllObject>) -> Result<Rc<RefCell<Type>>, Error> {
        match self {
            AllObject::EnumVariantInstance(i) => i.type_check_self(ctx),
            _ => Ok(self.get_type()),
        }
    }
}