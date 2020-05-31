use crate::common::{Context, Spanned};
use crate::ir::expr::Expr;
use crate::ir::objects::{AllObject, Arg};
use crate::ir::types::Type;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct CurriedFunction {
    pub ftype: Rc<RefCell<Type>>,
    pub scope: Vec<Expr>,
    pub orig: Callable,
    pub instance: RefCell<Option<Rc<FunctionInstanceObject>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: Spanned<String>,
    pub ftype: Rc<RefCell<Type>>,
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Function:\n")?;
        write!(f, "Name: {}\n", self.name)?;
        write!(f, "Type: {}\n", self.ftype.borrow())?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    FuncDef(Rc<FunctionDefinition>),
    Arg(Rc<Arg>),
}

impl Callable {
    pub fn name(&self) -> &Spanned<String> {
        match self {
            Callable::FuncDef(def) => &def.name,
            Callable::Arg(a) => &a.name,
        }
    }
    pub fn ftype(&self) -> Rc<RefCell<Type>> {
        match self {
            Callable::FuncDef(def) => def.ftype.clone(),
            Callable::Arg(a) => a.atype.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionObject {
    pub def: Rc<FunctionDefinition>,
    pub generics: Vec<Spanned<String>>,
    pub args: Vec<Rc<Arg>>,
    pub body: Rc<Expr>,
}

impl FunctionObject {
    pub fn get_return_type(&self) -> Ref<Type> {
        let borrowed = self.def.ftype.borrow();
        Type::get_return_value(borrowed)
    }

    pub fn create_ctx<'a>(&self, top: &'a Context<'a, AllObject>) -> Context<'a, AllObject> {
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

    pub fn monomorphize(
        self: &Rc<Self>,
        generics: &HashMap<String, Rc<RefCell<Type>>>,
    ) -> FunctionInstanceObject {
        FunctionInstanceObject {
            orig: Callable::FuncDef(self.def.clone()),
            ftype: monomorphize_type(&self.def.ftype, generics),
        }
    }
}

fn monomorphize_type(
    ty: &Rc<RefCell<Type>>,
    generics: &HashMap<String, Rc<RefCell<Type>>>,
) -> Rc<RefCell<Type>> {
    match Type::get_inner_cell(ty).borrow().deref() {
        Type::Generic(n) => generics.get(n.as_str()).unwrap().clone(),
        _ => ty.clone(),
    }
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.def.fmt(f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionInstanceObject {
    pub orig: Callable,
    pub ftype: Rc<RefCell<Type>>,
}
