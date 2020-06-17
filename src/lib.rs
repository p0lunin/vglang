mod common;
pub mod interpreter;
mod ir;
mod syntax;


use crate::common::{Context, peg_error_to_showed};
use std::fs::File;
use crate::ir::objects::AllObject;
use std::io::Read;
use itertools::Itertools;
use crate::ir::{IrContext, parse_tokens, type_check_objects};
use crate::syntax::parse_text;

pub fn compile_file<'a>(path_to_file: &str, top: Option<&'a Context<'a, AllObject>>) -> Result<(Context<'a, AllObject>, IrContext), String> {
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
        Err(e) => {
            Err(e.display(&data))
        }
        Ok(()) => Ok((ctx, ir_ctx))
    }
}

