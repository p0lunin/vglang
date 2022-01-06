use crate::arena::{Arena, Id};
use crate::common::global_context::ScopeCtx;
use crate::common::{DisplayScope, Find, PathToFind, Spanned};
use crate::ir::objects::DataDef;
use crate::ir::types::base_types::Function;
use crate::ir::types::Concrete;
use crate::syntax::ast;
use crate::GlobalCtx;
use std::fmt::{Display, Formatter, Write};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Type {
    Function(Function),
    Unknown(Option<Generic>),
    Int,
    Data(Concrete<DataDef>),
    Type,
    Never,
    Generic(Generic),
}

impl Type {
    pub fn move_into_scope(&self, old: &Arena<Type>, new: &mut Arena<Type>) -> Id<Type> {
        match self {
            Type::Function(f) => {
                let g = old.get(f.get_value).unwrap().move_into_scope(old, new);
                let r = old.get(f.return_value).unwrap().move_into_scope(old, new);
                new.alloc(Type::Function(Function {
                    get_value: g,
                    return_value: r,
                }))
            }
            Type::Data(d) => {
                let gens = d
                    .generics
                    .iter()
                    .map(|x| old.get(*x).unwrap().move_into_scope(old, new))
                    .collect();
                new.alloc(Type::Data(Concrete::new(d.base, gens)))
            }
            x => new.alloc(x.clone()),
        }
    }
    pub fn eq(&self, other: &Type, ctx: &ScopeCtx) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) | (Type::Never, Type::Never) | (Type::Type, Type::Type) => true,
            (Type::Unknown(g1), Type::Unknown(g2)) if g1 == g2 => true,
            (Type::Generic(g11), Type::Generic(g22)) if g11 == g22 => true,
            (Type::Function(f1), Type::Function(f2)) => {
                ctx.get_type(f1.get_value)
                    .eq(ctx.get_type(f2.get_value), ctx)
                    && ctx
                        .get_type(f1.return_value)
                        .eq(ctx.get_type(f2.return_value), ctx)
            }
            (Type::Data(d1), Type::Data(d2)) => d1.eq(d2, ctx),
            _ => false,
        }
    }
}

impl<'a> DisplayScope<'a> for Type {
    type Scope = (&'a Arena<Type>, &'a GlobalCtx);

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        match self {
            Type::Function(func) => func.display_value(f, scope),
            Type::Int => f.write_str("Int"),
            Type::Type => f.write_str("Type"),
            Type::Generic(g) => write!(f, "{}", g),
            Type::Data(g) => g.display_value(f, scope),
            Type::Never => f.write_str("Never"),
            Type::Unknown(_) => f.write_str("_"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Generic {
    pub(crate) name: Spanned<String>,
}

impl Generic {
    pub fn parse(g: ast::Generic) -> Self {
        Self { name: g.name }
    }
}

impl Display for Generic {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        Display::fmt(&self.name, f)
    }
}

impl Find for (Id<Generic>, &Generic) {
    type Item = Id<Generic>;

    fn find(&self, path: PathToFind) -> Option<Self::Item> {
        match path.has_segments() {
            true => None,
            false => (self.1.name.as_str() == path.endpoint).then(|| self.0),
        }
    }
}

impl Type {
    pub fn update_set_generic_func_unknowns(
        this: Id<Type>,
        g: &str,
        ty: Id<Type>,
        ctx: &mut ScopeCtx,
    ) -> Id<Type> {
        match ctx.get_type(this) {
            Type::Unknown(Some(gen)) => {
                if gen.name.as_str() == g {
                    ty
                } else {
                    this
                }
            }
            Type::Function(f) => {
                let gtv = f.get_value;
                let rtv = f.return_value;
                let gt = Type::update_set_generic_func_unknowns(gtv, g, ty, ctx);
                let rt = Type::update_set_generic_func_unknowns(rtv, g, ty, ctx);
                ctx.alloc_type(Type::Function(Function {
                    get_value: gt,
                    return_value: rt,
                }))
            }
            Type::Data(d) => {
                let base = d.base;
                let gens = d.generics.clone();
                let gens = gens
                    .into_iter()
                    .map(|gen| Type::update_set_generic_func_unknowns(gen, g, ty, ctx))
                    .collect();
                ctx.alloc_type(Type::Data(Concrete::new(base, gens)))
            }
            _ => this,
        }
    }
    pub fn update_set_generic_func(
        this: Id<Type>,
        g: &str,
        ty: Id<Type>,
        ctx: &mut ScopeCtx,
    ) -> Id<Type> {
        match ctx.get_type(this) {
            Type::Generic(gen) => {
                if gen.name.as_str() == g {
                    ty
                } else {
                    this
                }
            }
            Type::Function(f) => {
                let gtv = f.get_value;
                let rtv = f.return_value;
                let gt = Type::update_set_generic_func(gtv, g, ty, ctx);
                let rt = Type::update_set_generic_func(rtv, g, ty, ctx);
                ctx.alloc_type(Type::Function(Function {
                    get_value: gt,
                    return_value: rt,
                }))
            }
            Type::Data(d) => {
                let base = d.base;
                let gens = d.generics.clone();
                let gens = gens
                    .into_iter()
                    .map(|gen| Type::update_set_generic_func(gen, g, ty, ctx))
                    .collect();
                ctx.alloc_type(Type::Data(Concrete::new(base, gens)))
            }
            _ => this,
        }
    }

    /*pub fn expr_ty(self: &Rc<Self>) -> Rc<Type> {
        match self.as_ref() {
            Type::Generic(g) => Rc::new(Type::Unknown(Some(g.clone()))),
            Type::Function(f) => {
                Rc::new(Type::Function(Function {
                    get_value: f.get_value.expr_ty(),
                    return_value: f.return_value.expr_ty()
                }))
            }
            Type::Data(d) => {
                Rc::new(Type::Data(Concrete::new(
                    d.base.clone(),
                    d.generics.iter().map(|g| g.expr_ty()).collect()
                )))
            }
            _ => self.clone()
        }
    }*/
}

impl Type {
    pub fn is_part_of(&self, other: &Type, ctx: &ScopeCtx) -> bool {
        match (self, other) {
            (_, Type::Never | Type::Unknown(_)) => true,
            (Type::Function(l), Type::Function(r)) => l.is_part_of(r, ctx),
            (Type::Generic(l), Type::Generic(r)) => l.name.as_str() == r.name.as_str(),
            (Type::Data(d1), Type::Data(d2)) => {
                // if ctx[d1.base] == ctx[d2.base], then d1.base == d2.base
                d1.base == d2.base
                    && d1
                        .generics
                        .iter()
                        .zip(d2.generics.iter())
                        .all(|(x, y)| ctx.get_type(*x).is_part_of(ctx.get_type(*y), ctx))
            }
            (Type::Int, Type::Int) => true,
            (Type::Type, Type::Type) => true,
            _ => false,
        }
    }

    pub fn get_return_value<'a>(&'a self, ctx: &'a ScopeCtx) -> &'a Type {
        match self {
            Type::Function(f) => ctx.get_type(f.return_value).get_return_value(ctx),
            _ => self,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Type::Function(_) => true,
            _ => false,
        }
    }

    pub fn unknown() -> Rc<Type> {
        Rc::new(Type::Unknown(None))
    }

    pub fn typ() -> Rc<Type> {
        Rc::new(Type::Type)
    }
}
/*
impl Type {
    pub fn add(&self, value: &Type) -> Result<Self, String> {
        match self {
            Type::Int(i) => i.add(value).map(Type::Int),
            _ => Err(format!("Add")),
        }
    }

    pub fn sub(&self, value: &Type) -> Result<Self, String> {
        match self {
            Type::Int(i) => i.add(value).map(Type::Int),
            _ => Err(format!("Add")),
        }
    }

    pub fn and(&self, value: &Type) -> Result<Self, String> {
        match self {
            Type::Int(i) => i.add(value).map(Type::Int),
            _ => Err(format!("Add")),
        }
    }

    pub fn or(&self, value: &Type) -> Result<Self, String> {
        match self {
            Type::Int(i) => i.add(value).map(Type::Int),
            _ => Err(format!("Add")),
        }
    }

    pub fn div(&self, value: &Type) -> Result<Self, String> {
        match self {
            Type::Int(i) => i.add(value).map(Type::Int),
            _ => Err(format!("Add")),
        }
    }

    pub fn mul(&self, value: &Type) -> Result<Self, String> {
        match self {
            Type::Int(i) => i.add(value).map(Type::Int),
            _ => Err(format!("Add")),
        }
    }

    pub fn pow(&self, value: &Type) -> Result<Self, String> {
        match self {
            Type::Int(i) => i.add(value).map(Type::Int),
            _ => Err(format!("Add")),
        }
    }

    pub fn neg(&self) -> Result<Self, String> {
        match self {
            Type::Int(i) => i.neg().map(Type::Int),
            _ => Err(format!("Add")),
        }
    }

    pub fn op_implication(self, value: Type) -> Result<Self, Error> {
        Ok(self.implication(value))
    }
}
*/
impl Type {
    pub fn count_args(&self, ctx: &ScopeCtx) -> u8 {
        match self {
            Type::Function(t) => 1 + ctx.get_type(t.return_value).count_args(ctx),
            _ => 0,
        }
    }
    // TODO: need?
    pub fn args_types(id: Id<Type>, ctx: &ScopeCtx) -> Vec<Id<Type>> {
        match ctx.get_type(id) {
            Type::Function(f) => {
                let mut vec = vec![];
                vec.push(f.get_value.clone());
                vec.extend(Type::args_types(f.return_value, ctx));
                vec
            }
            _ => vec![id],
        }
    }
    /*pub fn types_in_scope(self: &Rc<Type>) -> Vec<(String, Rc<Type>)> {
        let mut types = vec![];
        match self.deref() {
            Type::Function(f) => {
                let Function {
                    get_value,
                    return_value,
                } = f;
                types.append(&mut Type::types_in_scope(get_value));
                types.append(&mut Type::types_in_scope(return_value));
            }
            Type::Named(name, ty) => types.push((name.clone(), ty.clone())),
            _ => {}
        };
        types
    }*/
}

/*
impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Int(i) => i
                .name
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("anonymous type"),
            Type::Type => i
                .name
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("anonymous type"),
            Type::Unknown(i) => i
                .name
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("anonymous type"),
            Type::Function(i) => i
                .name
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("anonymous type"),
            Type::AnotherType(i) => {
                let borrowed = i.borrow();
                unsafe { std::mem::transmute(borrowed.name()) }
            }
            Type::ParenthesisType(t) => {
                let borrowed = t.borrow();
                unsafe { std::mem::transmute(borrowed.name()) }
            }
            Type::Named(t, _) => t.as_str(),
            Type::Enum(e) => unsafe { mem::transmute(e.borrow().name()) },
            Type::EnumVariant(e) => e.name(),
            Type::EnumVariantInstance(e) => e.name(),
            Type::Generic(g) => &g,
            Type::EnumInstance(e) => e.name(),
        }
    }
}
*/
