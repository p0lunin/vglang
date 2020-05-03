use crate::error::Error;
use crate::parser::{FunctionDef, FunctionImpl};
use crate::spanned::Spanned;
use crate::types::Value;

#[derive(Debug)]
pub struct Function {
    name: Spanned<String>,
    def: Spanned<FunctionDef>,
    impls: Vec<Spanned<FunctionImpl>>,
}

#[derive(Debug)]
struct FunctionKind {
    var: Value,
    next: Option<Box<FunctionKind>>,
}

fn check_def(def: &Spanned<FunctionDef>) -> Result<(), Error> {
    unimplemented!()
}

fn check_impl(def: &Spanned<FunctionDef>, fimpl: &Spanned<FunctionImpl>) -> Result<(), Error> {
    unimplemented!()
}
