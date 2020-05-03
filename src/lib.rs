mod error;
mod error_builder;
mod parser;
mod spanned;
mod types;
mod functions;

pub use error::peg_error_to_showed;
pub use parser::parse;

use crate::error::Error;
use crate::parser::TopLevelToken;
use crate::spanned::Spanned;
use crate::types::Value;
use std::rc::Rc;

pub fn parse_tokens(
    tokens: Vec<Spanned<TopLevelToken>>,
) -> Result<Vec<Spanned<Value>>, Vec<Error>> {
    let mut errors = vec![];
    let mut types = vec![];
    tokens.into_iter().for_each(|token| {
        let span = token.span;
        match token.inner() {
            TopLevelToken::Type(t) => match types::parse_type(Spanned::new(t, span), &types) {
                Ok(t) => types.push(t),
                Err(err) => errors.push(err),
            },
            TopLevelToken::NewLine | TopLevelToken::Comment => {}
            _ => unreachable!(),
        }
    });

    if errors.is_empty() {
        Ok(types)
    } else {
        Err(errors)
    }
}
