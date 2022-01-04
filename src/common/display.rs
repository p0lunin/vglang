use crate::common::global_context::ScopeCtx;
use std::fmt::Write;
use crate::arena::Arena;
use crate::ir::types::Type;

pub trait DisplayScope<'a> {
    type Scope;
    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result;
    fn display_value_string(&self, scope: &Self::Scope) -> String {
        let mut str = String::new();
        self.display_value(&mut str, scope);
        str
    }
}
