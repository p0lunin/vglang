mod expr;
pub mod objects;
mod transform;
//  mod type_check;
pub mod types;

pub use self::{
    expr::{parse_expr, Expr, ExprKind},
    transform::{parse_function, parse_tokens},
};

use crate::ir::objects::FunctionObject;

#[derive(Debug)]
pub struct Implementations {
    pub functions: Vec<FunctionObject>,
}

impl Implementations {
    pub fn new() -> Self {
        Implementations { functions: vec![] }
    }
}

impl Implementations {
    pub fn add_function(&mut self, f: FunctionObject) {
        self.functions.push(f)
    }
}
