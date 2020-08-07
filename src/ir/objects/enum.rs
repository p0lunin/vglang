use crate::common::{Context, Error, Span};
use crate::ir::objects::Object;
use crate::ir::types::base_types::Function;
use crate::ir::types::{Generic, Type};
use crate::ir::{parse_expr, Expr};
use crate::syntax::ast::{EnumDecl, EnumVariant, Token};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct DataType {
    pub name: String,
    pub generics: Vec<Rc<Type>>,
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.generics.as_slice() {
            [] => f.write_str(self.name.as_str()),
            xs => {
                f.write_str("(")?;
                f.write_str(self.name.as_str())?;
                f.write_str(" ")?;
                xs.iter().for_each(|x| {
                    write!(f, " {}", x).unwrap();
                });
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
                .map(|g| Rc::new(Type::Generic(Generic::parse(g.inner()))))
                .collect(),
        });
        Ok(Self {
            ty: ty.clone(),
            variants: variants
                .into_iter()
                .map(|v| DataVariant::parse(v.inner(), ty.clone(), ctx).map(Rc::new))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
    pub fn get_field(&self, name: &str) -> Option<Rc<DataVariant>> {
        self.variants.iter().find(|v| v.name == name).map(Rc::clone)
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
        let mut ty = Rc::new(Type::Data(self.dty.clone()));
        for t in self.data.iter() {
            ty = Rc::new(Type::Function(Function {
                get_value: t.clone(),
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
                        .and_then(|e| e.convert_to_type(ctx))
                })
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl Display for DataVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}.{}", self.dty, self.name)
    }
}
