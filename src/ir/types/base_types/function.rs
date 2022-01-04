use crate::ir::types::Type;
use std::fmt::{Write};
use crate::arena::{Id, Arena};
use crate::common::DisplayScope;
use crate::common::global_context::ScopeCtx;
use crate::GlobalCtx;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub get_value: Id<Type>,
    pub return_value: Id<Type>,
}

impl Function {
    pub fn is_part_of(&self, other: &Function, ctx: &ScopeCtx) -> bool {
        ctx.get_type(self.get_value).is_part_of(ctx.get_type(other.get_value), ctx)
            && ctx.get_type(self.return_value).is_part_of(ctx.get_type(other.return_value), ctx)
    }
}

impl<'a> DisplayScope<'a> for Function {
    type Scope = (&'a Arena<Type>, &'a GlobalCtx);

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        f.write_str("(")?;
        scope.0.get(self.get_value).unwrap().display_value(f, scope)?;
        f.write_str(" -> ")?;
        scope.0.get(self.get_value).unwrap().display_value(f, scope)?;
        f.write_str(")")?;
        Ok(())
    }
}
