use crate::common::{Error, Spanned};
use crate::ir::objects::{AllObject, EnumInstance, EnumType};
use crate::ir::types::Type;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
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
    pub ty: Rc<RefCell<Type>>,
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "({}: {})", self.name, self.ty.borrow())
    }
}

impl Var {
    pub fn try_get_member(&self, name: &str) -> Option<AllObject> {
        match Type::get_inner_cell(&self.ty).borrow().deref() {
            Type::Enum(e) => EnumType::try_get_member(e, name),
            Type::EnumInstance(e) => e.try_get_member(name),
            _ => None,
        }
    }
}
