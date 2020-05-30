mod expr;
pub mod objects;
mod transform;
mod type_check;
pub mod types;

pub use self::{
    expr::{parse_expr, Expr},
    transform::{parse_function, parse_tokens},
    type_check::*,
};

use crate::ir::objects::{EnumInstance, FunctionInstanceObject};
use std::rc::Rc;

#[derive(Debug)]
pub struct IrContext {
    pub specialized_enums: Vec<Rc<EnumInstance>>,
    pub specialized_functions: Vec<Rc<FunctionInstanceObject>>,
}

impl IrContext {
    pub fn new() -> Self {
        Self {
            specialized_enums: vec![],
            specialized_functions: vec![],
        }
    }
    pub fn create_specialized_enum(&mut self, inst: EnumInstance) -> Rc<EnumInstance> {
        self.specialized_enums
            .iter()
            .find(|i| i.as_ref() == &inst)
            .map(|r| r.clone())
            .unwrap_or_else(|| {
                let enum_inst = Rc::new(inst);
                self.specialized_enums.push(enum_inst.clone());
                enum_inst
            })
    }
    pub fn create_specialized_function(
        &mut self,
        inst: FunctionInstanceObject,
    ) -> Rc<FunctionInstanceObject> {
        self.specialized_functions
            .iter()
            .find(|i| i.as_ref() == &inst)
            .map(|r| r.clone())
            .unwrap_or_else(|| {
                let f_inst = Rc::new(inst);
                self.specialized_functions.push(f_inst.clone());
                f_inst
            })
    }
}
