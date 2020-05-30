mod common;
pub mod interpreter;
mod ir;
mod syntax;

pub use common::peg_error_to_showed;
pub use ir::{parse_tokens, type_check_objects};
pub use syntax::parse_text;
