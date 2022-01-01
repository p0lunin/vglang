use crate::common::{Context, Error};
use crate::ir::objects::{Object, TypeObject};
use crate::ir::parse_expr;
use crate::ir::types::base_types::Function;
use crate::ir::types::{Generic, Type, Concrete};
use crate::syntax::ast::{EnumDecl, EnumVariant, Token};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use crate::ir::expr::interpret_expr_as_ty;
use crate::Implementations;
use itertools::Itertools;

#[derive(Debug, PartialEq, Clone)]
pub struct DataType {
    pub name: String,
    pub generics: Vec<Generic>,
}

impl DataType {
    pub fn new(name: String, generics: Vec<Generic>) -> Self {
        DataType { name, generics }
    }
    pub fn as_ty(self: &Rc<Self>) -> Rc<Type> {
        data_ty(self.clone(), &self.generics)
    }
}

fn data_ty<T>(ty: Rc<DataType>, generics: &[T]) -> Rc<Type> {
    match generics {
        [] => Rc::new(Type::Type),
        [_, xs @ ..] => Rc::new(Type::Function(Function {
            get_value: Rc::new(Type::Type),
            return_value: data_ty(ty, xs),
        })),
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.generics.as_slice() {
            [] => f.write_str(self.name.as_str()),
            xs => {
                f.write_str("(")?;
                f.write_str(self.name.as_str())?;
                f.write_fmt(format_args!("<{}>", self.generics.iter().map(|x| x.name.as_str()).join(", ")))?;
                f.write_str(")")?;
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct DataDef {
    pub ty: Rc<DataType>,
    pub variants: Vec<Rc<DataVariant>>,
}

impl DataDef {
    pub fn parse(def: EnumDecl, ctx: &Context<'_, Object>) -> Result<Self, Error> {
        let EnumDecl {
            name,
            variants,
            generics,
        } = def;
        let ty = Rc::new(DataType {
            name: name.inner(),
            generics: generics
                .into_iter()
                .map(|g| Generic::parse(g.inner()))
                .collect(),
        });
        let mut objects = ty
            .generics
            .iter()
            .map(|x| {
                Object::Type(Rc::new(TypeObject {
                    name: x.name.as_str().to_string(),
                    def: Rc::new(Type::Generic(x.clone())),
                }))
            })
            .collect::<Vec<_>>();
        objects.push(Object::EnumDecl(ty.clone()));

        let ctx = Context {
            objects,
            parent: Some(ctx),
        };
        Ok(Self {
            ty: ty.clone(),
            variants: variants
                .into_iter()
                .map(|v| DataVariant::parse(v.inner(), ty.clone(), &ctx).map(Rc::new))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
    pub fn get_field(&self, name: &str) -> Option<Rc<DataVariant>> {
        self.variants.iter().find(|v| v.name == name).map(Rc::clone)
    }
    pub fn as_ty(&self) -> Rc<Type> {
        self.ty.as_ty()
    }
}

impl Display for DataDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        Display::fmt(&self.ty, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataVariant {
    pub dty: Rc<DataType>,
    pub name: String,
    pub data: Vec<Rc<Type>>,
}

impl DataVariant {
    pub fn get_type(&self) -> Rc<Type> {
        let mut ty = Rc::new(Type::Data(Concrete::new(
            self.dty.clone(),
            self.dty.generics.iter().map(|g| Rc::new(Type::Unknown(Some(g.clone())))).collect()
        )));
        for t in self.data.iter().rev() {
            ty = Rc::new(Type::Function(Function {
                get_value: match t.as_ref() {
                    Type::Generic(_) => Rc::new(Type::Never),
                    _ => t.clone(),
                },
                return_value: ty,
            }))
        }
        ty
    }

    pub fn parse(
        ast: EnumVariant<Token>,
        dty: Rc<DataType>,
        ctx: &Context<'_, Object>,
    ) -> Result<Self, Error> {
        let EnumVariant { name, datas } = ast;
        Ok(Self {
            dty,
            name: name.inner(),
            data: datas
                .into_iter()
                .map(|t| {
                    parse_expr(&t, ctx, &mut HashMap::new(), None)
                        .map(|x| x.unwrap())
                        .and_then(|e| interpret_expr_as_ty(e, ctx, &Implementations::new()))
                })
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    pub fn update_generic(mut self, gen: &Generic, ty: &Rc<Type>) -> Self {
        self.data.iter_mut().for_each(|x| {
            *x = x.clone().update_set_generic_func(gen.name.as_str(), ty);
        });
        self
    }
}

impl Display for DataVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}.{}", self.dty, self.name)
    }
}
