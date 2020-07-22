use crate::common::Spanned;
use crate::ir::types::Type;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Arg {
    pub name: Spanned<String>,
    pub ty: Rc<Type>,
}

impl Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeObject {
    pub name: String,
    pub def: Rc<Type>,
}

impl Display for TypeObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.def.fmt(f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub name: String,
    pub ty: Rc<Type>,
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}
