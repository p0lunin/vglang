use crate::common::{Error, PathToFind, Find, DisplayScope};
use crate::ir::objects::{Object};
use crate::ir::types::base_types::Function;
use crate::ir::types::{Generic, Type, Concrete};
use crate::syntax::ast::{EnumDecl, EnumVariant, Token};
use std::fmt::{Debug, Write};
use crate::ir::expr::{parse_type};
use crate::arena::{Id, Arena};
use crate::common::global_context::{ScopeCtx, GlobalCtx};

#[derive(Debug, Clone)]
pub struct DataType {
    pub name: String,
    pub generics: Arena<Generic>,
    local_types: Arena<Type>,
}

impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl DataType {
    pub fn new(name: String, generics: Arena<Generic>) -> Self {
        DataType { name, generics, local_types: Arena::new() }
    }
    pub fn as_ty(&self, ctx: &mut ScopeCtx) -> Id<Type> {
        data_ty(self.generics.as_slice(), ctx)
    }
}

fn data_ty<T>(generics: &[T], ctx: &mut ScopeCtx) -> Id<Type> {
    match generics {
        [] => ctx.alloc_type(Type::Type),
        [_, xs @ ..] => {
            let g = ctx.alloc_type(Type::Type);
            let r = data_ty(xs, ctx);
            ctx.alloc_type(Type::Function(Function {
                get_value: g,
                return_value: r,
            }))
        },
    }
}

impl<'a> DisplayScope<'a> for DataType {
    type Scope = ();

    fn display_value(&self, f: &mut impl Write, _: &Self::Scope) -> std::fmt::Result {
       write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct DataDef {
    pub ty: DataType,
    pub variants: Vec<DataVariant>,
}

impl DataDef {
    pub fn parse(def: EnumDecl, ctx: &mut GlobalCtx) -> Result<Id<DataDef>, Error> {
        let EnumDecl {
            name,
            variants,
            generics,
        } = def;

        let ty = DataType::new(
            name.inner(),
            generics
                .into_iter()
                .map(|g| Generic::parse(g.inner()))
                .collect(),
        );
        let data_id = ctx.alloc_data(DataDef {
            ty,
            variants: vec![],
        });
        let mut scope_ctx = ScopeCtx::new(
            ctx
        );
        ctx.get_data(data_id)
            .ty
            .generics
            .iter()
            .for_each(|g| {
                scope_ctx.alloc_generic(g.clone());
            });

        let variants = variants
            .into_iter()
            .map(|v| {
                DataVariant::parse(v.inner(), data_id, &mut scope_ctx)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let local_types = scope_ctx.inner().types;
        let this = ctx.get_data_mut(data_id);
        this.variants = variants;
        this.ty.local_types = local_types;

        Ok(data_id)
    }
    pub fn get_field(&self, name: &str) -> Option<&DataVariant> {
        self.variants.iter().find(|v| v.name == name)
    }
    pub fn get_variant_ty(&self, variant: usize, ctx: &mut ScopeCtx) -> Id<Type> {
        let v = &self.variants[variant];

        let gens = self
            .ty
            .generics
            .iter()
            .map(|g| ctx.alloc_type(Type::Unknown(Some(g.clone()))))
            .collect();
        let mut ty = ctx.alloc_type(Type::Data(Concrete::new(
            v.dty,
            gens
        )));
        for t in v.data.iter().rev() {
            let tt = self.ty.local_types.get(t.clone()).unwrap();
            let g = match tt {
                Type::Generic(g) => ctx.alloc_type(Type::Unknown(Some(g.clone()))),
                _ => Type::move_into_scope(tt, &self.ty.local_types, &mut ctx.types),
            };
            ty = ctx.alloc_type(Type::Function(Function {
                get_value: g,
                return_value: ty,
            }))
        }
        ty
    }
    pub fn as_ty(&self, ctx: &mut ScopeCtx) -> Id<Type> {
        self.ty.as_ty(ctx)
    }
}

impl<'a> Find for (Id<DataDef>, &'a DataDef) {
    type Item = Object;

    fn find(&self, path: PathToFind) -> Option<Self::Item> {
        match path.segments.as_slice() {
            [] => (self.1.ty.name.as_str() == path.endpoint).then(|| Object::Enum(self.0)),
            [x] => {
                if self.1.ty.name.as_str() == *x {
                    self.1.variants.iter().enumerate().find_map(|(ind, v)| {
                        (v.name.as_str() == path.endpoint).then(|| Object::EnumVariant(self.0, ind))
                    })
                } else {
                    None
                }
            },
            [..] => None
        }
    }
}

impl<'a> DisplayScope<'a> for DataDef {
    type Scope = ();

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        self.ty.display_value(f, scope)
    }
}

#[derive(Debug, Clone)]
pub struct DataVariant {
    pub dty: Id<DataDef>,
    pub name: String,
    data: Vec<Id<Type>>,
}

impl DataVariant {
    pub fn parse(
        ast: EnumVariant<Token>,
        dty: Id<DataDef>,
        ctx: &mut ScopeCtx,
    ) -> Result<Self, Error> {
        let EnumVariant { name, datas } = ast;
        Ok(Self {
            dty,
            name: name.inner(),
            data: datas
                .into_iter()
                .map(|t| {
                    parse_type(&t, ctx)
                })
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    pub fn data(&self, ctx: &mut ScopeCtx) -> Vec<Id<Type>> {
        let dty = ctx.global.get_data(self.dty);
        self.data.iter().map(|x| {
            dty.ty.local_types.get(*x).unwrap()
                .clone().move_into_scope(&dty.ty.local_types, &mut ctx.types)
        }
        ).collect()
    }
}

impl<'a> DisplayScope<'a> for DataVariant {
    type Scope = ScopeCtx<'a>;

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        f.write_str(scope.global.get_data(self.dty).ty.name.as_str())?;
        f.write_str(".")?;
        f.write_str(self.name.as_str())?;

        Ok(())
    }
}
