use crate::common::Spanned;
use crate::ir::objects::AllObject;
use crate::ir::types::Type;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Arg {
    pub name: Spanned<String>,
    pub atype: Rc<RefCell<Type>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeObject {
    pub name: Spanned<String>,
    pub ttype: Rc<RefCell<Type>>,
}

impl Display for TypeObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.ttype.borrow().fmt(f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub name: Spanned<String>,
    pub data: AllObject,
}

impl Var {
    pub fn new(name: Spanned<String>, data: AllObject) -> Self {
        Self { name, data }
    }
    pub fn get_type(&self) -> Rc<RefCell<Type>> {
        self.data.get_type()
    }
}
