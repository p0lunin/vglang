use crate::common::{HasName, Searchable};
use crate::ir::objects::{
    Arg, DataDef, DataVariant, FunctionDefinition, TypeObject, Var,
};
use crate::ir::types::Type;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    FunctionDefinition(Rc<FunctionDefinition>),
    Enum(Rc<DataDef>),
    EnumVariant(Rc<DataVariant>),
    Arg(Rc<Arg>),
    Var(Rc<Var>),
    Type(Rc<TypeObject>),
}

impl Searchable for Object {
    type Item = Self;

    fn find(&self, name: &str) -> Option<Self::Item> {
        match self {
            Object::FunctionDefinition(_) => None,
            Object::Enum(e) => e.get_field(name).map(Object::EnumVariant),
            Object::EnumVariant(_) => None,
            Object::Arg(_) => None,
            Object::Var(_) => None,
            Object::Type(_) => None,
        }
    }
}

impl Object {
    pub fn get_type(&self) -> Rc<Type> {
        match self {
            Object::FunctionDefinition(f) => f.ftype.clone(),
            Object::Type(_t) => Type::typ(),
            Object::Arg(a) => a.ty.clone(),
            Object::Enum(e) => Rc::new(Type::Data(e.ty.clone())),
            Object::EnumVariant(e) => Rc::new(Type::Data(e.dty.clone())),
            Object::Var(v) => v.ty.clone(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Object::Type(t) => f.write_str(&format!("{}", t)),
            Object::Var(v) => f.write_str(&format!("{}", v)),
            Object::FunctionDefinition(o) => Display::fmt(&o, f),
            Object::Arg(_) => unimplemented!(),
            Object::Enum(e) => Display::fmt(e, f),
            Object::EnumVariant(e) => Display::fmt(e, f),
        }
    }
}

impl HasName for Object {
    fn name(&self) -> &str {
        match self {
            Object::Type(t) => &t.name,
            Object::Var(t) => &t.name,
            Object::FunctionDefinition(t) => &t.name,
            Object::Arg(a) => &a.name,
            Object::Enum(e) => e.ty.name.as_str(),
            Object::EnumVariant(e) => e.name.as_str(),
        }
    }
}
