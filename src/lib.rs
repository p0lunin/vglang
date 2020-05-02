mod error;
mod error_builder;
mod parser;
// mod state;
mod spanned;
mod types;

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
    // let mut state_tokens = vec![];
    tokens.into_iter().for_each(|token| {
        let span = token.span;
        match token.inner() {
            TopLevelToken::Type(t) => match types::parse_type(Spanned::new(t, span), &types) {
                Ok(t) => types.push(t),
                Err(err) => errors.push(err),
            },
            // TopLevelToken::State { .. } => state_tokens.push(token),
            TopLevelToken::NewLine | TopLevelToken::Comment => {}
            _ => unreachable!(),
        }
    });
    /*let states = state_tokens.into_iter().fold(vec![], |mut vec, token| {
        match parse_state(token, &types) {
            Ok(state) => vec.push(state),
            Err(mut errs) => errors.append(&mut errs),
        };
        vec
    });*/

    if errors.is_empty() {
        Ok(types)
    } else {
        Err(errors)
    }
}
