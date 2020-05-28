use std::rc::Rc;
use std::cell::{RefCell, Ref};
use crate::ir::types::Type;
use crate::ir::expr::Expr;
use crate::common::{Spanned, Context};
use crate::ir::objects::{Object, Arg, AllObject};
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct CurriedFunction {
    pub ftype: Rc<RefCell<Type>>,
    pub scope: Vec<Expr>,
    pub orig: Callable,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: Spanned<String>,
    pub ftype: Rc<RefCell<Type>>,
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
    pub fn ftype(&self) -> Rc<RefCell<Type>> {
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
    pub ftype: Rc<RefCell<Type>>,
    pub body: Rc<Expr>,
}

impl FunctionObject {
    pub fn get_return_type(&self) -> Ref<Type> {
        let borrowed = self.ftype.borrow();
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
}

impl Display for Object<FunctionObject> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Function:\n")?;
        write!(f, "Name: {}\n", self.name)?;
        write!(f, "Type: {}\n", self.object.ftype.borrow())?;
        Ok(())
    }
}