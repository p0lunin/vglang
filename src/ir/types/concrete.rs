use crate::ir::types::Type;
use std::fmt::{Write, Debug, Formatter};
use itertools::Itertools;
use crate::arena::{Id, Arena};
use crate::common::DisplayScope;
use crate::common::global_context::ScopeCtx;
use crate::ir::objects::DataDef;
use crate::GlobalCtx;

pub struct Concrete<T> {
    pub base: Id<T>,
    pub generics: Vec<Id<Type>>,
}
impl<T> Debug for Concrete<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Concrete")
            .field("base", &self.base)
            .field("generics", &self.generics)
            .finish()
    }
}
impl<T> Clone for Concrete<T> {
    fn clone(&self) -> Self {
        Self {
            base: self.base,
            generics: self.generics.clone(),
        }
    }
}

impl<T> Concrete<T> {
    pub fn new(base: Id<T>, generics: Vec<Id<Type>>) -> Self {
        Concrete { base, generics }
    }
    pub fn base(base: Id<T>) -> Self {
        Concrete { base, generics: vec![] }
    }
}

impl Concrete<DataDef> {
    pub fn eq(&self, other: &Concrete<DataDef>, ctx: &ScopeCtx) -> bool {
        ctx.global.get_data(self.base).ty == ctx.global.get_data(other.base).ty &&
            self.generics.iter().zip(other.generics.iter()).all(|(x, y)| {
                ctx.get_type(*x).eq(ctx.get_type(*y), ctx)
            })
    }
}

impl<'a> DisplayScope<'a> for Concrete<DataDef> {
    type Scope = (&'a Arena<Type>, &'a GlobalCtx);

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        scope.1.get_data(self.base).display_value(f, &())?;
        if self.generics.len() != 0 {
            f.write_fmt(format_args!(
                "<{}>",
                self.generics.iter()
                    .map(|x| scope.0.get(*x).unwrap().display_value_string(scope))
                    .join(", ")
            ))?;
        }
        Ok(())
    }
}
