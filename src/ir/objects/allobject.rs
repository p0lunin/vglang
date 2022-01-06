use crate::arena::Id;
use crate::common::global_context::ScopeCtx;
use crate::common::DisplayScope;
use crate::ir::objects::{Arg, DataDef, FunctionObject, Var};
use crate::ir::types::{Generic, Type};
use std::fmt::Write;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Function(Id<FunctionObject>),
    Enum(Id<DataDef>),
    EnumVariant(Id<DataDef>, usize),
    Generic(Id<Generic>),
    Arg(Id<Arg>),
    Var(Id<Var>),
}

impl Object {
    pub fn get_type(self, ctx: &mut ScopeCtx) -> Id<Type> {
        match self {
            Object::Function(f) => ctx.global.get_func(f).get_type(&mut ctx.types),
            Object::Arg(a) => ctx.get_arg(a).get_type(),
            Object::Enum(e) => ctx.global.get_data(e).as_ty(ctx),
            Object::Var(v) => ctx.get_var(v).ty,
            Object::EnumVariant(id, idx) => ctx.global.get_data(id).get_variant_ty(idx, ctx),
            Object::Generic(_) => ctx.alloc_type(Type::Type),
        }
    }
}

impl<'a> DisplayScope<'a> for Object {
    type Scope = ScopeCtx<'a>;

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        match *self {
            Object::Function(o) => scope.global.get_func(o).display_value(f, scope.global),
            Object::Enum(e) => scope.global.get_data(e).display_value(f, &()),
            Object::EnumVariant(id, idx) => scope
                .global
                .get_data_variant(id, idx)
                .display_value(f, scope),
            Object::Generic(g) => f.write_str(scope.get_generic(g).name.as_str()),
            Object::Arg(a) => f.write_str(scope.get_arg(a).name.as_str()),
            Object::Var(v) => f.write_str(scope.get_var(v).name.as_str()),
        }
    }
}
