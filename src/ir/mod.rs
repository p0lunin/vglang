mod expr;
pub mod objects;
pub mod patmat;
mod transform;
pub mod types;

pub use self::{
    expr::{parse_expr, Expr, ExprKind},
    transform::{parse_function, parse_tokens},
};

use crate::ir::objects::{DataDef, FunctionObject};
use std::rc::Rc;

#[derive(Debug)]
pub struct Implementations {
    pub functions: Vec<FunctionObject>,
    pub data_defs: Vec<Rc<DataDef>>,
}

impl Implementations {
    pub fn new() -> Self {
        Implementations {
            functions: vec![],
            data_defs: vec![],
        }
    }
}

impl Implementations {
    pub fn add_function(&mut self, f: FunctionObject) {
        self.functions.push(f)
    }
    pub fn add_data(&mut self, d: Rc<DataDef>) {
        self.data_defs.push(d)
    }
}
