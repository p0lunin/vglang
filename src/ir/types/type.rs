use crate::common::Spanned;
use crate::ir::objects::{DataType, DataVariant};
use crate::ir::types::base_types::Function;
use crate::ir::Expr;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Function(Function),
    Unknown,
    Int,
    Data(Rc<DataType>),
    DataVariant(Rc<DataVariant>),
    Type,
    Never,
    Generic(Generic),
    Expr(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Generic {
    pub(crate) name: Spanned<String>,
}

impl Display for Generic {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        Display::fmt(&self.name, f)
    }
}

impl Type {
    pub fn is_part_of(&self, other: &Type) -> bool {
        match (self, other) {
            (_, Type::Never) => true,
            (Type::Function(l), Type::Function(r)) => l.is_part_of(r),
            (Type::Int, Type::Int) => true,
            (_, Type::Type) => true,
            (Type::Generic(l), Type::Generic(r)) => l.name.as_str() == r.name.as_str(),
            _ => false,
        }
    }

    pub fn get_return_value(self: Rc<Type>) -> Rc<Type> {
        match self.as_ref() {
            Type::Function(f) => f.return_value.clone().get_return_value(),
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
        Rc::new(Type::Unknown)
    }

    pub fn typ() -> Rc<Type> {
        Rc::new(Type::Type)
    }

    /*pub fn int(val: i128) -> Rc<Self> {
        Rc::new(Type::Int(Int::Value(val)))
    }*/
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Function(func) => Display::fmt(func, f),
            Type::Int => f.write_str("Int"),
            Type::Type => f.write_str("Type"),
            Type::Generic(g) => Display::fmt(g, f),
            Type::Data(g) => Display::fmt(g, f),
            Type::DataVariant(g) => Display::fmt(g, f),
            Type::Never => f.write_str("Never"),
            Type::Unknown => f.write_str("Unknown"),
            Type::Expr(e) => unimplemented!(),
        }
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
    pub fn count_args(&self) -> u8 {
        match self {
            Type::Function(t) => 1 + t.return_value.count_args(),
            _ => 0,
        }
    }
    // TODO: need?
    pub fn args_types(self: &Rc<Type>) -> Vec<Rc<Type>> {
        match self.deref() {
            Type::Function(f) => {
                let mut vec = vec![];
                vec.push(f.get_value.clone());
                vec.extend(Type::args_types(&f.return_value));
                vec
            }
            _ => vec![self.clone()],
        }
    }
    pub fn types_in_scope(self: &Rc<Type>) -> Vec<(Spanned<String>, Rc<Type>)> {
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
            Type::Expr(e) => unimplemented!(),
            _ => {}
        };
        types
    }
}

impl Type {
    pub fn implication(self, other: Self) -> Self {
        Type::Function(Function {
            get_value: Rc::new(self),
            return_value: Rc::new(other),
        })
    }
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
