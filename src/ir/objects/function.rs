use crate::common::{Spanned, Find, PathToFind, DisplayScope};
use crate::ir::expr::Expr;
use crate::ir::objects::{Arg};
use crate::ir::types::{Type, Concrete};
use std::fmt::{Write};
use crate::arena::{Id, Arena};
use crate::common::global_context::{ScopeCtx, ScopeCtxInner};
use crate::GlobalCtx;
use crate::ir::types::base_types::Function;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: Spanned<String>,
    pub ftype: Id<Type>,
}

impl<'a> DisplayScope<'a> for FunctionDefinition {
    type Scope = (&'a Arena<Type>, &'a GlobalCtx);

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        f.write_str(self.name.as_str())?;
        f.write_str(":")?;
        scope.0.get(self.ftype).unwrap().display_value(f, scope)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub def: FunctionDefinition,
    // pub generics: Vec<Spanned<String>>,
    pub args: Vec<Arg>,
    pub body: Expr,
    pub ctx: ScopeCtxInner,
}

impl FunctionObject {
    pub fn get_type(&self, ctx: &mut Arena<Type>) -> Id<Type> {
        fty(self.def.ftype, &self.ctx.types, ctx)
    }
    /*pub fn create_ctx<'a>(&self, top: &'a LocalContext<'a, Object>) -> LocalContext<'a, Object> {
        let args = self.get_args();
        LocalContext {
            objects: args,
            parent: Some(top),
        }
    }

    fn get_args(&self) -> Vec<Object> {
        self.args
            .iter()
            .map(|arg| Object::Arg(*arg))
            .collect()
    }*/
}

fn fty(ty: Id<Type>, old: &Arena<Type>, new: &mut Arena<Type>) -> Id<Type> {
    match old.get(ty).unwrap() {
        Type::Generic(g) => new.alloc(Type::Unknown(Some(g.clone()))),
        Type::Function(f) => {
            let g = fty(f.get_value, old, new);
            let r = fty(f.return_value, old, new);
            new.alloc(Type::Function(Function {
                get_value: g,
                return_value: r,
            }))
        }
        Type::Data(Concrete { base, generics }) => {
            let gens = generics.iter().cloned().map(|x| {
                fty(x, old, new)
            }).collect();
            new.alloc(Type::Data(Concrete::new(
                *base,
                gens
            )))
        }
        x => new.alloc(x.clone())
    }
}

impl Find for (Id<FunctionObject>, &FunctionObject) {
    type Item = Id<FunctionObject>;

    fn find(&self, path: PathToFind) -> Option<Self::Item> {
        match path.has_segments() {
            true => None,
            false => (self.1.def.name.as_str() == path.endpoint)
                .then(|| self.0)
        }
    }
}

impl<'a> DisplayScope<'a> for FunctionObject {
    type Scope = GlobalCtx;

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        self.def.display_value(f, &(&self.ctx.types, scope))
    }
}
