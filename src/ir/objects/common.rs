use crate::common::{Spanned, PathToFind, Find};
use crate::ir::types::Type;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::arena::Id;
use crate::common::global_context::ScopeCtx;

#[derive(Debug, PartialEq, Clone)]
pub struct Arg {
    pub name: Spanned<String>,
    pub ty: Id<Type>,
}

impl Arg {
    pub fn get_type(&self) -> Id<Type> {
        self.ty
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

impl Find for (Id<Arg>, &Arg) {
    type Item = Id<Arg>;

    fn find(&self, path: PathToFind) -> Option<Self::Item> {
        match path.has_segments() {
            true => None,
            false => (self.1.name.as_str() == path.endpoint)
                .then(|| self.0)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub ty: Id<Type>,
}

impl Var {
    pub fn new(name: String, ty: Id<Type>) -> Self {
        Var { name, ty }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

impl Find for (Id<Var>, &Var) {
    type Item = Id<Var>;

    fn find(&self, path: PathToFind) -> Option<Self::Item> {
        match path.has_segments() {
            true => None,
            false => (self.1.name.as_str() == path.endpoint)
                .then(|| self.0)
        }
    }
}
