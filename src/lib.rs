mod common;
pub mod interpreter;
mod ir;
mod syntax;

use crate::common::{peg_error_to_showed, Context};
use crate::ir::objects::AllObject;
use crate::ir::{parse_tokens, type_check_objects, IrContext};
use crate::syntax::parse_text;
use itertools::Itertools;
use std::fs::File;
use std::io::Read;

pub fn compile_file<'a>(
    path_to_file: &str,
    top: Option<&'a Context<'a, AllObject>>,
) -> Result<(Context<'a, AllObject>, IrContext), String> {
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
    let (ctx, mut ir_ctx) = match parse_tokens(ast, top) {
        Ok(t) => t,
        Err(errs) => {
            return Err(errs.into_iter().map(|e| e.display(&data)).join("\n"));
        }
    };
    match type_check_objects(Some(&ctx), &mut ir_ctx) {
        Err(e) => Err(e.display(&data)),
        Ok(()) => Ok((ctx, ir_ctx)),
    }
}
