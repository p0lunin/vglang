mod r#enum;
mod error;
mod error_builder;
mod object;
mod parser;
mod spanned;
mod type_check;
mod types;

pub use error::peg_error_to_showed;
pub use parser::parse;
pub use type_check::type_check_objects;

use crate::error::Error;
use crate::object::{parse_function, AllObject, Object};
use crate::parser::TopLevelToken;
use crate::r#enum::EnumType;
use crate::spanned::Spanned;
use crate::type_check::Context;
use crate::types::{Type, TypeType};
use itertools::Itertools;
use std::collections::{LinkedList, VecDeque};
use std::rc::Rc;

pub fn parse_tokens<'a>(tokens: Vec<Spanned<TopLevelToken>>) -> Result<Context<'a>, Vec<Error>> {
    let mut errors = vec![];
    let mut ctx = Context {
        objects: vec![],
        parent: None,
    };
    let mut function_defs = vec![];
    let mut function_impls = VecDeque::new();
    tokens.into_iter().for_each(|token| {
        let span = token.span;
        match token.inner() {
            TopLevelToken::Type(ty) => {
                let name = ty.0.clone().map(|i| i.0);
                match types::parse_type(Spanned::new(ty, span), &ctx) {
                    Ok(t) => {
                        let span = t.span;
                        ctx.objects.push(AllObject::Type(Rc::new(Object {
                            name,
                            object: Spanned::new(t, span),
                        })))
                    }
                    Err(err) => errors.push(err),
                }
            }
            TopLevelToken::NewLine | TopLevelToken::Comment => {}
            TopLevelToken::FunctionDef(f) => function_defs.push(f),
            TopLevelToken::FunctionImpl(i) => function_impls.push_front(i),
            TopLevelToken::EnumDecl(e) => match EnumType::from_ast(e, &ctx) {
                Ok(e) => ctx.objects.push(AllObject::Enum(Rc::new(e))),
                Err(err) => errors.push(err),
            },
        }
    });
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
        match parse_function(d, fimpl, &ctx) {
            Ok(o) => ctx.objects.push(o),
            Err(e) => errors.push(e),
        }
    });

    if errors.is_empty() {
        Ok(ctx)
    } else {
        Err(errors)
    }
}
