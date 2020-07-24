mod common;
pub mod interpreter;
mod ir;
mod syntax;

use crate::common::{peg_error_to_showed, Context};
use crate::ir::objects::Object;
use crate::ir::{parse_tokens, Implementations};
use crate::syntax::parse_text;
use itertools::Itertools;
use std::fs::File;
use std::io::Read;

pub fn compile_file<'a>(
    path_to_file: &str,
    top: Option<&'a Context<'a, Object>>,
) -> Result<(Context<'a, Object>, Implementations), String> {
    let mut file = match File::open(path_to_file) {
        Ok(f) => f,
        Err(e) => {
            return Err(e.to_string());
        }
    };
    let mut data = String::new();
    file.read_to_string(&mut data).unwrap();
    let ast = match parse_text(&data) {
        Ok(d) => d,
        Err(e) => {
            return Err(peg_error_to_showed(e, &data));
        }
    };
    let (ctx, impls) = match parse_tokens(ast, top) {
        Ok(t) => t,
        Err(errs) => {
            return Err(errs.into_iter().map(|e| e.display(&data)).join("\n"));
        }
    };
    Ok((ctx, impls))
}
