use crate::common::{Context, Spanned};
use crate::ir::expr::Expr;
use crate::ir::objects::{Arg, Object};
use crate::ir::types::Type;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: Spanned<String>,
    pub ftype: Rc<Type>,
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Function:\n")?;
        write!(f, "Name: {}\n", self.name)?;
        write!(f, "Type: {}\n", self.ftype)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionObject {
    pub def: Rc<FunctionDefinition>,
    // pub generics: Vec<Spanned<String>>,
    pub args: Vec<Rc<Arg>>,
    pub body: Expr,
}

impl FunctionObject {
    pub fn create_ctx<'a>(&self, top: &'a Context<'a, Object>) -> Context<'a, Object> {
        let args = self.get_args();
        Context {
            objects: args,
            parent: Some(top),
        }
    }

    fn get_args(&self) -> Vec<Object> {
        self.args
            .iter()
            .map(|arg| Object::Arg(arg.clone()))
            .collect()
    }
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.def.fmt(f)
    }
}
