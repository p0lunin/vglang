mod common;
pub mod interpreter;
mod ir;
mod syntax;

pub use crate::ir::Implementations;
pub use interpreter::Interpreter;

use crate::common::{peg_error_to_showed, Context};
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
    compile_code(data.as_str(), top)
}

pub fn compile_code<'a>(code: &str, top: Option<&'a Context<'a, Object>>,) -> Result<(Context<'a, Object>, Implementations), String> {
    let ast = match parse_text(&code) {
        Ok(d) => d,
        Err(e) => {
            return Err(peg_error_to_showed(e, &code));
        }
    };
    let (ctx, impls) = match parse_tokens(ast, top) {
        Ok(t) => t,
        Err(errs) => {
            return Err(errs.into_iter().map(|e| e.display(&code)).join("\n"));
        }
    };
    Ok((ctx, impls))
}

pub fn eval(interpreter: &mut Interpreter, text: &str) -> Result<ByteCode, String> {
    let token = parse_token(text).map_err(|e| peg_error_to_showed(e, text))?;
    interpreter.execute_code(token).map_err(|e| e.display(text))
}

pub fn load_core() -> (Context<'static, Object>, Implementations) {
    let core = include_str!("../core/core.vg");
    compile_code(core, None)
        .expect("Core should be valid")
}
