mod common;
pub mod interpreter;
mod ir;
mod syntax;

pub use crate::ir::Implementations;
pub use interpreter::Interpreter;

use crate::common::{peg_error_to_showed, Context, Error};
use crate::interpreter::ByteCode;
use crate::ir::objects::Object;
use crate::ir::parse_tokens;
use crate::syntax::{parse_text, parse_token};
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

pub fn eval(interpreter: &mut Interpreter, text: &str) -> Result<ByteCode, String> {
    let token = parse_token(text).map_err(|e| peg_error_to_showed(e, text))?;
    interpreter.execute_code(token).map_err(|e| e.display(text))
}
