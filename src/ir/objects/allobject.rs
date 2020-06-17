use crate::common::{Context, Error, HasName, Span};
use crate::ir::expr::Expr;
use crate::ir::objects::{
    Arg, Callable, CreateEnumInstanceFunc, CreateEnumVariantFunc, CurriedFunction, EnumInstance,
    EnumType, EnumVariant, EnumVariantInstance, FunctionDefinition, FunctionObject, TypeObject,
    Var,
};
use crate::ir::types::Type;
use crate::ir::IrContext;
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Write};
use std::mem;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum AllObject {
    Type(Rc<TypeObject>),
    FunctionDefinition(Rc<FunctionDefinition>),
    Enum(Rc<RefCell<EnumType>>),
    EnumInstance(Rc<EnumInstance>),
    EnumVariant(Rc<EnumVariant>),
    EnumVariantInstance(Rc<EnumVariantInstance>),
    CreateEnumVariantFunc(CreateEnumVariantFunc),
    CreateEnumInstanceFunc(CreateEnumInstanceFunc),
    CurriedFunction(Rc<CurriedFunction>),
    Arg(Rc<Arg>),
    Var(Rc<Var>),
}

impl Display for AllObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            AllObject::Type(t) => f.write_str(&format!("{}", t)),
            AllObject::Var(v) => f.write_str(&format!("{}", v)),
            AllObject::FunctionDefinition(o) => Display::fmt(&o, f),
            AllObject::CurriedFunction(_) => unimplemented!(),
            AllObject::Arg(_) => unimplemented!(),
            AllObject::Enum(e) => Display::fmt(&e.borrow(), f),
            AllObject::EnumVariant(e) => Display::fmt(e, f),
            AllObject::CreateEnumVariantFunc(_) => unimplemented!(),
            AllObject::EnumVariantInstance(e) => Display::fmt(e, f),
            AllObject::EnumInstance(e) => Display::fmt(e, f),
            AllObject::CreateEnumInstanceFunc(_) => unimplemented!(),
        }
    }
}

impl HasName for AllObject {
    fn name(&self) -> &str {
        match self {
            AllObject::Type(t) => &t.name,
            AllObject::Var(t) => &t.name,
            AllObject::FunctionDefinition(t) => &t.name,
            AllObject::CurriedFunction(f) => &f.orig.name(),
            AllObject::Arg(a) => &a.name,
            AllObject::Enum(e) => unsafe { mem::transmute(e.borrow().name()) },
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
            AllObject::Enum(e) => EnumType::call(e),
            t => Ok(t.clone()),
        }
    }
    pub fn get_type(&self) -> Rc<RefCell<Type>> {
        match self {
            AllObject::Type(t) => t.ttype.clone(),
            AllObject::Var(t) => t.ty.clone(),
            AllObject::FunctionDefinition(f) => f.ftype.clone(),
            AllObject::CurriedFunction(f) => f.ftype.clone(),
            AllObject::Arg(a) => a.atype.clone(),
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
    pub fn call_with_arg_expr(
        &self,
        arg: Expr,
        ctx: &mut IrContext,
        span: Span,
    ) -> Result<AllObject, Error> {
        match self {
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
                    instance: RefCell::new(None),
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
                    instance: RefCell::new(None),
                })))
            }
            AllObject::Var(v) => unimplemented!(),
            AllObject::Arg(a) => Ok(AllObject::CurriedFunction(Rc::new(CurriedFunction {
                ftype: a.atype.borrow().try_curry().ok_or(Error::Span(span))?,
                scope: vec![arg],
                orig: Callable::Arg(a.clone()),
                instance: RefCell::new(None),
            }))),
            AllObject::Enum(e) => EnumType::call_with_arg_expr(e, arg, span),
            AllObject::EnumVariant(e) => Ok(e.call_with_arg_expr(arg)),
            AllObject::CreateEnumVariantFunc(f) => f.call_with_arg_expr(arg),
            AllObject::EnumVariantInstance(_) => Err(Error::Span(span)),
            AllObject::EnumInstance(_) => Err(Error::Span(span)),
            AllObject::CreateEnumInstanceFunc(e) => e.call_with_arg_expr(arg, ctx),
        }
    }
    pub fn try_get_member(&self, member: &str, span: Span) -> Result<AllObject, Error> {
        match self {
            AllObject::EnumInstance(e) => e.try_get_member(member).ok_or(Error::Span(span)),
            AllObject::Var(v) => v.try_get_member(member).ok_or(Error::Span(span)),
            AllObject::Enum(e) => EnumType::try_get_member(e, member).ok_or(Error::Span(span)),
            _ => Err(Error::Span(span)),
        }
    }
    pub fn type_check_self(
        &self,
        ctx: &Context<'_, AllObject>,
        ir_ctx: &mut IrContext,
    ) -> Result<Rc<RefCell<Type>>, Error> {
        match self {
            AllObject::EnumVariantInstance(i) => i.type_check_self(ctx, ir_ctx),
            _ => Ok(self.get_type()),
        }
    }
}
