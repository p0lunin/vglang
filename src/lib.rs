mod error;
mod error_builder;
mod object;
mod parser;
mod spanned;
mod type_check;
mod types;

pub use error::peg_error_to_showed;
pub use parser::parse;

use crate::error::Error;
use crate::object::{parse_function, AllObject};
use crate::parser::TopLevelToken;
use crate::spanned::Spanned;
use crate::types::Type;
use itertools::Itertools;
use std::collections::{LinkedList, VecDeque};
use std::rc::Rc;

pub fn parse_tokens(
    tokens: Vec<Spanned<TopLevelToken>>,
) -> Result<(Vec<Rc<Spanned<Type>>>, Vec<AllObject>), Vec<Error>> {
    let mut errors = vec![];
    let mut types = vec![];
    let mut function_defs = vec![];
    let mut function_impls = VecDeque::new();
    tokens.into_iter().for_each(|token| {
        let span = token.span;
        match token.inner() {
            TopLevelToken::Type(t) => match types::parse_type(Spanned::new(t, span), &types) {
                Ok(t) => types.push(t),
                Err(err) => errors.push(err),
            },
            TopLevelToken::NewLine | TopLevelToken::Comment => {}
            TopLevelToken::FunctionDef(f) => function_defs.push(f),
            TopLevelToken::FunctionImpl(i) => function_impls.push_front(i),
        }
    });
    let mut objects = vec![];
    function_defs.into_iter().for_each(|d| {
        let (idx, _) = match function_impls.iter().find_position(|i| i.0 == d.0) {
            Some(d) => d,
            None => {
                errors.push(Error::Custom(
                    d.0.span,
                    format!("Cannot find impl for function {}", d.0.inner().0),
                    "-here".to_owned(),
                ));
                return;
            }
        };
        let fimpl = function_impls.remove(idx).unwrap();
        match parse_function(d, fimpl, &types) {
            Ok(o) => objects.push(o),
            Err(e) => errors.push(e),
        }
    });

    if errors.is_empty() {
        Ok((types, objects))
    } else {
        Err(errors)
    }
}
