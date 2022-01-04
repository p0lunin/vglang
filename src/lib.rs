mod common;
pub mod interpreter;
mod ir;
mod syntax;
mod arena;

pub use crate::ir::Implementations;
pub use interpreter::Interpreter;
pub use common::global_context::{GlobalCtx, ScopeCtx};
pub use common::DisplayScope;

use crate::common::{peg_error_to_showed, LocalContext};
use crate::interpreter::ByteCode;
use crate::ir::objects::{Object, FunctionObject};
use crate::ir::parse_tokens;
use crate::syntax::{parse_text, parse_token};
use itertools::Itertools;
use std::fs::File;
use std::io::Read;
use crate::common::global_context::{ScopeCtxInner};
use crate::arena::Id;

pub fn compile_file(
    path_to_file: &str,
    global: &mut GlobalCtx
) -> Result<Vec<Id<FunctionObject>>, String> {
    let mut file = match File::open(path_to_file) {
        Ok(f) => f,
        Err(e) => {
            return Err(e.to_string());
        }
    };
    let mut data = String::new();
    file.read_to_string(&mut data).unwrap();
    compile_code(data.as_str(), global)
}

pub fn compile_code<'a>(code: &str, global: &mut GlobalCtx) -> Result<Vec<Id<FunctionObject>>, String> {
    let ast = match parse_text(&code) {
        Ok(d) => d,
        Err(e) => {
            return Err(peg_error_to_showed(e, &code));
        }
    };
    let ids = match parse_tokens(ast, global) {
        Ok(t) => t,
        Err(errs) => {
            return Err(errs.into_iter().map(|e| e.display(&code)).join("\n"));
        }
    };
    Ok(ids)
}

pub fn eval(interpreter: &mut Interpreter, text: &str) -> Result<ByteCode, String> {
    let token = parse_token(text).map_err(|e| peg_error_to_showed(e, text))?;
    interpreter.execute_code(token).map_err(|e| e.display(text))
}

pub fn load_core(global: &mut GlobalCtx) -> Vec<Id<FunctionObject>> {
    let core = include_str!("../core/core.vg");
    compile_code(core, global)
        .expect("Core should be valid")
}
