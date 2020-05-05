use crate::error::Error;
use crate::parser::{FunctionDef, FunctionImpl};
use crate::spanned::Spanned;
use crate::types::{Type, TypeKind, TypeOperable};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub get_value: Rc<Spanned<Type>>,
    pub return_value: Rc<Spanned<Type>>,
}

impl TypeOperable<Function> for TypeKind<Function> {
    fn add(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "+ is not allowed for `Function` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    fn and(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "& is not allowed for `Function` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    fn or(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "| is not allowed for `Function` value".to_owned(),
            "-here".to_owned(),
        ))
    }
}

fn check_def(def: &Spanned<FunctionDef>) -> Result<(), Error> {
    unimplemented!()
}

fn check_impl(def: &Spanned<FunctionDef>, fimpl: &Spanned<FunctionImpl>) -> Result<(), Error> {
    unimplemented!()
}
