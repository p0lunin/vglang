use crate::common::Spanned;
use crate::ir::objects::AllObject;
use std::cell::RefCell;
use std::rc::Rc;
use crate::ir::types::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Object<T> {
    pub name: Spanned<String>,
    pub object: T,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Arg {
    pub atype: Rc<RefCell<Type>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub data: AllObject,
}

impl Var {
    pub fn new(data: AllObject) -> Self {
        Self { data }
    }
    pub fn get_type(&self) -> Rc<RefCell<Type>> {
        self.data.get_type()
    }
}