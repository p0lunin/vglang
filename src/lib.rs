mod syntax;
mod ir;
mod common;

pub use common::peg_error_to_showed;
pub use ir::{type_check_objects, parse_tokens};
pub use syntax::parse_text;
