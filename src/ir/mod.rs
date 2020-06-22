mod expr;
pub mod objects;
mod transform;
mod type_check;
pub mod types;

pub use self::{
    expr::{parse_expr, Expr, ExprKind},
    transform::{parse_function, parse_tokens},
    type_check::*,
};

use crate::common::Error;
use crate::ir::objects::{EnumInstance, FunctionInstanceObject, FunctionObject};
use std::rc::Rc;

#[derive(Debug)]
pub struct IrContext {
    pub functions: Vec<FunctionObject>,
    pub specialized_enums: Vec<Rc<EnumInstance>>,
    pub specialized_functions: Vec<Rc<FunctionInstanceObject>>,
}

fn append<T>(mut l: Vec<T>, mut r: Vec<T>) -> Vec<T> {
    l.append(&mut r);
    l
}

impl IrContext {
    pub fn new() -> Self {
        Self {
            functions: vec![],
            specialized_enums: vec![],
            specialized_functions: vec![],
        }
    }
    pub fn extend(self, other: Self) -> Self {
        Self {
            functions: append(self.functions, other.functions),
            specialized_enums: append(self.specialized_enums, other.specialized_enums),
            specialized_functions: append(self.specialized_functions, other.specialized_functions)
        }
    }
    pub fn add_function(&mut self, function: FunctionObject) -> Result<(), Error> {
        match self
            .functions
            .iter()
            .find(|f| f.def.name.as_str() == function.def.name.as_str())
        {
            Some(f) => Err(Error::Custom(
                f.def.name.span,
                format!("Function with name {} already defined", f.def.name),
                "here".to_owned(),
            )),
            _ => {
                self.functions.push(function);
                Ok(())
            }
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
