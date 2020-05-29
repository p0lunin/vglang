use crate::common::{Error, Span, Spanned};
use crate::ir::objects::{EnumInstance, EnumType, EnumVariant, EnumVariantInstance};
use crate::ir::types::base_types::{Function, Int, TypeType, Unknown};
use crate::ir::types::{OneTypeKind, TypeKind, TypeOperable};
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Function(OneTypeKind<Function>),
    Int(TypeKind<Int>),
    Enum(Rc<RefCell<EnumType>>),
    EnumInstance(Rc<EnumInstance>),
    EnumVariant(Rc<EnumVariant>),
    EnumVariantInstance(Rc<EnumVariantInstance>),
    Type(OneTypeKind<TypeType>),
    Unknown(OneTypeKind<Unknown>),
    ParenthesisType(Rc<RefCell<Type>>),
    AnotherType(Spanned<Rc<RefCell<Type>>>),
    Named(Spanned<String>, Rc<RefCell<Type>>),
    Generic(Spanned<String>),
}

impl Type {
    pub fn is_part_of(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Function(l), Type::Function(r)) => l.kind.is_part_of(&r.kind),
            (Type::Int(l), Type::Int(r)) => match (l.kinds.is_empty(), r.kinds.is_empty()) {
                (false, true) => true,
                (true, false) => false,
                _ => l
                    .kinds
                    .iter()
                    .all(|l| r.kinds.iter().any(|r| l.is_part_of(r))),
            },
            (_, Type::Type(_)) => true,
            (Type::AnotherType(l), r) => l.borrow().is_part_of(r),
            (l, Type::AnotherType(r)) => l.is_part_of(r.borrow().deref()),
            (Type::ParenthesisType(l), r) => l.borrow().is_part_of(r),
            (l, Type::ParenthesisType(r)) => l.is_part_of(r.borrow().deref()),
            (Type::Named(_, l), Type::Named(_, r)) => l.borrow().is_part_of(r.borrow().deref()),
            (Type::Named(_, l), r) => l.borrow().is_part_of(r),
            (l, Type::Named(_, r)) => l.is_part_of(r.borrow().deref()),
            (Type::EnumVariant(v), Type::EnumVariant(e)) => Rc::ptr_eq(v, e),
            (Type::EnumVariantInstance(v), Type::Enum(e)) => e.borrow().has_variant(v),
            (Type::EnumVariantInstance(v), Type::EnumInstance(e)) => e.has_variant(v),
            (Type::EnumVariantInstance(v), Type::EnumVariant(e)) => Rc::ptr_eq(v.origin(), e),
            (Type::EnumVariantInstance(v), Type::EnumVariantInstance(e)) => {
                Rc::ptr_eq(v.origin(), e.origin())
            }
            (Type::EnumInstance(e), Type::EnumInstance(v)) => e.is_part_of(v),
            (Type::Generic(l), Type::Generic(r)) => l.as_str() == r.as_str(),
            _ => false,
        }
    }

    pub fn try_curry(&self) -> Option<Rc<RefCell<Type>>> {
        match self {
            Type::Function(f) => Some(f.kind.return_value.clone()),
            Type::ParenthesisType(t) => t.borrow().try_curry(),
            Type::AnotherType(t) => t.borrow().try_curry(),
            Type::Named(_, t) => t.borrow().try_curry(),
            _ => None,
        }
    }

    pub fn try_curry_with_arg(&self) -> Option<(Rc<RefCell<Type>>, Rc<RefCell<Type>>)> {
        match self {
            Type::Function(f) => Some((f.kind.get_value.clone(), f.kind.return_value.clone())),
            Type::ParenthesisType(t) => t.borrow().try_curry_with_arg(),
            Type::AnotherType(t) => t.borrow().try_curry_with_arg(),
            Type::Named(_, t) => t.borrow().try_curry_with_arg(),
            _ => None,
        }
    }

    pub fn get_return_value(this: Ref<Type>) -> Ref<Type> {
        match this.deref() {
            Type::Function(f) => unsafe {
                std::mem::transmute(Type::get_return_value(f.kind.return_value.borrow()))
            },
            Type::ParenthesisType(t) => unsafe { std::mem::transmute(t.borrow()) },
            Type::AnotherType(t) => unsafe { std::mem::transmute(t.borrow()) },
            Type::Named(_, t) => unsafe { std::mem::transmute(t.borrow()) },
            _ => this,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Type::Function(_) => true,
            _ => false,
        }
    }

    pub fn monomorhize(
        this: &Rc<RefCell<Type>>,
        generics: &HashMap<String, Rc<RefCell<Type>>>,
    ) -> Rc<RefCell<Type>> {
        match this.borrow().deref() {
            Type::Function(f) => Rc::new(RefCell::new(Type::Function(OneTypeKind {
                name: f.name.clone(),
                kind: Spanned::new(
                    Function {
                        get_value: Type::monomorhize(&f.kind.get_value, generics),
                        return_value: Type::monomorhize(&f.kind.return_value, generics),
                    },
                    f.kind.span,
                ),
            }))),
            Type::Generic(g) => generics.get(g.as_str()).unwrap().clone(),
            _ => this.clone(),
        }
    }

    pub fn unknown(span: Span) -> Type {
        Type::Unknown(OneTypeKind {
            name: None,
            kind: Spanned::new(Unknown, span),
        })
    }

    pub fn type_type() -> Type {
        Type::Type(OneTypeKind {
            name: None,
            kind: Spanned::new(TypeType, Span::new(0, 0)),
        })
    }
}

macro_rules! apply_op {
    ($self:tt, $value:tt, $op:tt) => {
        match $self {
            Type::Int(t) => t.$op($value).map(Type::Int),
            Type::Type(t) => t.$op($value).map(Type::Type),
            Type::Unknown(t) => t.$op($value).map(Type::Unknown),
            Type::Function(t) => t.$op($value).map(Type::Function),
            Type::AnotherType(t) => {
                let mut inner = t.borrow().clone();
                inner.remove_name();
                inner.$op($value)
            }
            Type::ParenthesisType(t) => Ok(Type::ParenthesisType(Rc::new(RefCell::new(
                t.borrow().deref().clone().$op($value)?,
            )))),
            Type::Named(_, t) => {
                let mut inner = t.borrow().clone();
                inner.remove_name();
                inner.$op($value)
            }
            Type::Enum(e) => Err(format!(
                "Cannot {} enum type {} to {}",
                stringify!($op),
                e.borrow(),
                $value
            )),
            Type::EnumVariant(e) => Err(format!(
                "Cannot {} enum variant type {} to {}",
                stringify!($op),
                e,
                $value
            )),
            Type::EnumVariantInstance(e) => Err(format!(
                "Cannot {} enum variant instance type {} to {}",
                stringify!($op),
                e,
                $value
            )),
            Type::Generic(g) => Err(format!(
                "Cannot {} generic type {} to {}",
                stringify!($op),
                g,
                $value
            )),
            Type::EnumInstance(e) => Err(format!(
                "Cannot {} enum instance type {} to {}",
                stringify!($op),
                e,
                $value
            )),
        }
    };
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Function(func) => Display::fmt(func, f),
            Type::Int(t) => Display::fmt(t, f),
            Type::Type(_) => f.write_str("Type"),
            Type::Unknown(_) => f.write_str("Unknown"),
            Type::AnotherType(t) => Display::fmt(t.borrow().deref(), f),
            Type::ParenthesisType(t) => Display::fmt(t.borrow().deref(), f),
            Type::Named(name, def) => {
                f.write_str(&format!("{{{}: {}}}", name, def.borrow().deref()))
            }
            Type::Enum(e) => Display::fmt(e.borrow().deref(), f),
            Type::EnumVariant(e) => Display::fmt(e, f),
            Type::EnumVariantInstance(e) => Display::fmt(e, f),
            Type::Generic(g) => Display::fmt(g, f),
            Type::EnumInstance(e) => Display::fmt(e, f),
        }
    }
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Int(t) => t.span(),
            Type::Type(t) => t.span(),
            Type::Unknown(u) => u.span(),
            Type::Function(t) => t.span(),
            Type::AnotherType(t) => t.borrow().span(),
            Type::ParenthesisType(t) => t.borrow().span(),
            Type::Named(name, def) => name.span.extend(&def.borrow().span()),
            Type::Enum(e) => e.borrow().span(),
            Type::EnumVariant(e) => e.span(),
            Type::EnumVariantInstance(e) => e.span(),
            Type::Generic(g) => g.span,
            Type::EnumInstance(e) => e.span(),
        }
    }

    pub fn set_name(&mut self, name: Spanned<String>) {
        match self {
            Type::Int(t) => t.name = Some(name),
            Type::Type(t) => t.name = Some(name),
            Type::Unknown(t) => t.name = Some(name),
            Type::Function(t) => t.name = Some(name),
            Type::AnotherType(_) => unreachable!(),
            Type::ParenthesisType(_) => unreachable!(),
            Type::Named(_, _) => unreachable!(),
            Type::Enum(_) => unreachable!(),
            Type::EnumVariant(_) => unreachable!(),
            Type::EnumVariantInstance(_) => unreachable!(),
            Type::Generic(_) => unreachable!(),
            Type::EnumInstance(_) => unreachable!(),
        }
    }

    pub fn remove_name(&mut self) {
        match self {
            Type::Int(t) => t.name = None,
            Type::Type(t) => t.name = None,
            Type::Unknown(t) => t.name = None,
            Type::Function(t) => t.name = None,
            Type::AnotherType(_) => unreachable!(),
            Type::ParenthesisType(_) => unreachable!(),
            Type::Named(_, _) => unreachable!(),
            Type::Enum(_) => unreachable!(),
            Type::EnumVariant(_) => unreachable!(),
            Type::EnumVariantInstance(_) => unreachable!(),
            Type::Generic(_) => unreachable!(),
            Type::EnumInstance(_) => unreachable!(),
        }
    }

    pub fn add(self, value: Type) -> Result<Self, String> {
        let value = value.get_inner();
        apply_op!(self, value, add)
    }

    pub fn sub(self, value: Type) -> Result<Self, String> {
        let value = value.get_inner().neg()?;
        apply_op!(self, value, add)
    }

    pub fn and(self, value: Type) -> Result<Self, String> {
        let value = value.get_inner();
        apply_op!(self, value, and)
    }

    pub fn or(self, value: Type) -> Result<Self, String> {
        let value = value.get_inner();
        apply_op!(self, value, or)
    }

    pub fn div(self, value: Type) -> Result<Self, String> {
        let value = value.get_inner();
        apply_op!(self, value, div)
    }

    pub fn mul(self, value: Type) -> Result<Self, String> {
        let value = value.get_inner();
        apply_op!(self, value, mul)
    }

    pub fn pow(self, value: Type) -> Result<Self, String> {
        let value = value.get_inner();
        apply_op!(self, value, pow)
    }

    pub fn neg(self) -> Result<Self, String> {
        match self {
            Type::Int(t) => t.neg().map(Type::Int),
            Type::Type(t) => t.neg().map(Type::Type),
            Type::Unknown(t) => t.neg().map(Type::Unknown),
            Type::Function(t) => t.neg().map(Type::Function),
            Type::AnotherType(t) => {
                let mut inner = t.borrow().clone();
                inner.remove_name();
                inner.neg()
            }
            Type::ParenthesisType(t) => Ok(Type::ParenthesisType(Rc::new(RefCell::new(
                t.borrow().deref().clone().neg()?,
            )))),
            Type::Named(_, t) => {
                let mut inner = t.borrow().clone();
                inner.remove_name();
                inner.neg()
            }
            Type::Enum(e) => Err(format!("Cannot neg enum type {}", e.borrow())),
            Type::EnumVariant(e) => Err(format!("Cannot neg enum variant type {}", e)),
            Type::EnumVariantInstance(e) => {
                Err(format!("Cannot neg enum variant instance type {}", e))
            }
            Type::Generic(g) => Err(format!("Cannot neg generic type {}", g)),
            Type::EnumInstance(e) => Err(format!("Cannot enum type {}", e)),
        }
    }

    pub fn op_implication(self, value: Type) -> Result<Self, Error> {
        Ok(self.implication(value))
    }

    pub(crate) fn get_inner_cell(this: &Rc<RefCell<Self>>) -> Rc<RefCell<Type>> {
        match this.borrow().deref() {
            Type::AnotherType(t) => Type::get_inner_cell(t),
            Type::Named(_, n) => Type::get_inner_cell(n),
            Type::ParenthesisType(t) => Type::get_inner_cell(t),
            _ => this.clone(),
        }
    }

    pub(crate) fn get_inner(&self) -> Type {
        match self {
            Type::AnotherType(t) => t.borrow().get_inner(),
            Type::Named(_, n) => n.borrow().get_inner(),
            Type::ParenthesisType(t) => t.borrow().get_inner(),
            t => t.clone(),
        }
    }

    pub fn count_args(&self) -> u8 {
        match self {
            Type::Function(t) => 1 + t.kind.return_value.borrow().count_args(),
            _ => 0,
        }
    }
}

impl Type {
    pub fn args_types(this: &Rc<RefCell<Type>>) -> Vec<Rc<RefCell<Type>>> {
        match this.borrow().deref() {
            Type::Function(t) => {
                let mut vec = vec![];
                let f = &t.kind;
                vec.push(f.get_value.clone());
                vec.extend(Type::args_types(&f.return_value));
                vec
            }
            _ => vec![this.clone()],
        }
    }
    pub fn types_in_scope(this: &Rc<RefCell<Type>>) -> Vec<(Spanned<String>, Rc<RefCell<Type>>)> {
        let mut types = vec![];
        match this.borrow().deref() {
            Type::Named(name, _) => types.push((name.clone(), this.clone())),
            Type::Function(f) => {
                let Function {
                    get_value,
                    return_value,
                } = &*f.kind;
                types.append(&mut Type::types_in_scope(get_value));
                types.append(&mut Type::types_in_scope(return_value));
            }
            _ => {}
        };
        types
    }
}

impl Type {
    pub fn implication(self, other: Self) -> Self {
        let span = self.span().extend(&other.span());
        Type::Function(OneTypeKind::from_kind(Spanned::new(
            Function {
                get_value: Rc::new(RefCell::new(self)),
                return_value: Rc::new(RefCell::new(other)),
            },
            span,
        )))
    }
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Int(i) => i
                .name
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("anonymous type"),
            Type::Type(i) => i
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
