use crate::error::{Error, SpannedError};
use crate::object::{parse_expr, AllObject};
use crate::parser;
use crate::parser::{Ast, Token};
use crate::r#enum::{EnumType, EnumVariant, EnumVariantInstance, EnumInstance};
use crate::spanned::AddSpan;
use crate::spanned::{Span, Spanned};
use crate::type_check::Context;
use crate::types::function::Function;
use crate::types::int::{Int, OneRangeIntBound, Slice};
use crate::types::vec_type::VecType;
use std::cell::{Ref, RefCell};
use std::convert::TryFrom;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Function(OneTypeKind<Function>),
    Int(TypeKind<Int>),
    Enum(Rc<EnumType>),
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
            (Type::Named(_, l), Type::Named(_, r)) => l == r,
            (Type::Named(_, l), r) => l.borrow().is_part_of(r),
            (l, Type::Named(_, r)) => l.is_part_of(r.borrow().deref()),
            (Type::EnumVariant(v), Type::EnumVariant(e)) => Rc::ptr_eq(v, e),
            (Type::EnumVariantInstance(v), Type::EnumInstance(e)) => e.has_variant(v),
            (Type::EnumVariantInstance(v), Type::EnumVariant(e)) => Rc::ptr_eq(v.origin(), e),
            (Type::EnumVariantInstance(v), Type::EnumVariantInstance(e)) => {
                Rc::ptr_eq(v.origin(), e.origin())
            }
            (t, Type::Generic(g)) => true,
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
                e,
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
            Type::Enum(e) => Display::fmt(e, f),
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
            Type::Enum(e) => e.span(),
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
            Type::AnotherType(t) => unreachable!(),
            Type::ParenthesisType(t) => unreachable!(),
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
            Type::AnotherType(t) => unreachable!(),
            Type::ParenthesisType(t) => unreachable!(),
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
            Type::Enum(e) => Err(format!("Cannot neg enum type {}", e)),
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

pub trait TypeOperable<T>: Sized {
    fn add(self, right: Type) -> Result<Self, String>;
    fn neg(self) -> Result<Self, String>;
    fn and(self, right: Type) -> Result<Self, String>;
    fn or(self, right: Type) -> Result<Self, String>;
    fn mul(self, right: Type) -> Result<Self, String>;
    fn sub(self, right: Type) -> Result<Self, String>;
    fn div(self, right: Type) -> Result<Self, String>;
    fn pow(self, right: Type) -> Result<Self, String>;
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
            Type::Enum(e) => e.name(),
            Type::EnumVariant(e) => e.name(),
            Type::EnumVariantInstance(e) => e.name(),
            Type::Generic(g) => &g,
            Type::EnumInstance(e) => e.name()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeKind<T> {
    pub name: Option<Spanned<String>>,
    pub kinds: Spanned<VecType<T>>,
}

impl<T: Display + Debug> Display for TypeKind<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.name {
            Some(name) => f.write_str(&format!("({}: ", name))?,
            None => f.write_str("(")?,
        };
        match self.kinds.is_empty() {
            true => f.write_str("Int")?,
            false => {
                f.write_str(
                    &self
                        .kinds
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" | "),
                )?;
            }
        }
        f.write_str(")")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OneTypeKind<T> {
    pub name: Option<Spanned<String>>,
    pub kind: Spanned<T>,
}

impl<T: Display> Display for OneTypeKind<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        Display::fmt(&self.kind, f)
    }
}

impl<T> OneTypeKind<T> {
    pub fn from_kind(kind: Spanned<T>) -> Self {
        Self { name: None, kind }
    }
    pub fn fmap<F: FnOnce(Spanned<T>) -> Result<Spanned<T>, E>, E>(self, f: F) -> Result<Self, E> {
        let OneTypeKind { name, kind } = self;
        f(kind).map(|kind| Self { kind, name })
    }
    pub fn span(&self) -> Span {
        self.kind.span
    }
}

impl<T> TypeKind<T> {
    pub fn empty(span: Span) -> Self {
        Self {
            name: None,
            kinds: Spanned::new(VecType(vec![]), span),
        }
    }
    pub fn from_kinds(kinds: Spanned<VecType<T>>) -> Self {
        Self { name: None, kinds }
    }
    pub fn fmap<F: FnMut(T) -> Result<T, E>, E>(self, f: F) -> Result<Self, E> {
        let TypeKind { name, kinds } = self;
        let Spanned { val, span } = kinds;
        val.into_iter()
            .map(f)
            .collect::<Result<VecType<T>, E>>()
            .map(|kinds| Self {
                kinds: kinds.add_span(span),
                name,
            })
    }
    pub fn span(&self) -> Span {
        self.kinds.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeType;

impl TypeOperable<TypeType> for OneTypeKind<TypeType> {
    fn add(self, right: Type) -> Result<Self, String> {
        Err("+ is not allowed for `Type` value".to_owned())
    }

    fn neg(self) -> Result<Self, String> {
        Err("- is not allowed for `Type` value".to_owned())
    }

    fn and(self, right: Type) -> Result<Self, String> {
        Err("& is not allowed for `Type` value".to_owned())
    }

    fn or(self, right: Type) -> Result<Self, String> {
        Err("| is not allowed for `Type` value".to_owned())
    }

    fn mul(self, right: Type) -> Result<Self, String> {
        Err("* is not allowed for `Type` value".to_owned())
    }

    fn sub(self, right: Type) -> Result<Self, String> {
        Err("- is not allowed for `Type` value".to_owned())
    }

    fn div(self, right: Type) -> Result<Self, String> {
        Err("/ is not allowed for `Type` value".to_owned())
    }

    fn pow(self, right: Type) -> Result<Self, String> {
        Err("^ is not allowed for `Type` value".to_owned())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unknown;

impl TypeOperable<Unknown> for OneTypeKind<Unknown> {
    fn add(self, right: Type) -> Result<Self, String> {
        Err("+ is not allowed for `Unknown` value".to_owned())
    }

    fn neg(self) -> Result<Self, String> {
        Err("- is not allowed for `Unknown` value".to_owned())
    }

    fn and(self, right: Type) -> Result<Self, String> {
        Err("& is not allowed for `Unknown` value".to_owned())
    }

    fn or(self, right: Type) -> Result<Self, String> {
        Err("| is not allowed for `Unknown` value".to_owned())
    }

    fn mul(self, right: Type) -> Result<Self, String> {
        Err("* is not allowed for `Unknown` value".to_owned())
    }

    fn sub(self, right: Type) -> Result<Self, String> {
        Err("- is not allowed for `Unknown` value".to_owned())
    }

    fn div(self, right: Type) -> Result<Self, String> {
        Err("/ is not allowed for `Unknown` value".to_owned())
    }

    fn pow(self, right: Type) -> Result<Self, String> {
        Err("^ is not allowed for `Unknown` value".to_owned())
    }
}

pub fn parse_type(
    type_def: Spanned<parser::Type>,
    ctx: &Context,
) -> Result<Rc<RefCell<Type>>, Error> {
    let span = type_def.span;
    let parser::Type(name, def) = type_def.inner();
    let name_span = name.span;
    let name = Spanned::new(name.inner().0, name_span);
    let mut t_type = parse_type_helper(def, ctx)?;
    t_type.set_name(name);
    Ok(Rc::new(RefCell::new(t_type)))
}

pub fn parse_type_helper(token: Token, ctx: &Context) -> Result<Type, Error> {
    let span = token.span;
    match token.ast {
        Ast::And(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.and(right).spanned_err(span))
        }),
        Ast::Or(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.or(right).spanned_err(span))
        }),
        Ast::Add(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.add(right).spanned_err(span))
        }),
        Ast::Sub(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.sub(right).spanned_err(span))
        }),
        Ast::Neg(t) => parse_type_helper(*t, ctx).and_then(|left| left.neg().spanned_err(span)),
        Ast::Int(i) => Ok(Type::Int(TypeKind {
            name: None,
            kinds: Spanned::new(VecType::one(Int::Value(i)), token.span),
        })),
        Ast::Parenthesis(t) => {
            parse_type_helper(*t, ctx).map(|ty| Type::ParenthesisType(Rc::new(RefCell::new(ty))))
        }
        Ast::Gr(l, r) => match ((*l), (*r)) {
            (Token { ast: Ast::Val, .. }, a) => get_arithmetic_val(&a)
                .map(|value| one_bound(OneRangeIntBound::Low(value + 1), span)),
            (a, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&a)
                .map(|value| one_bound(OneRangeIntBound::High(value - 1), span)),
            _ => Err(Error::Span(token.span)),
        },
        Ast::Le(l, r) => match ((*l), (*r)) {
            (Token { ast: Ast::Val, .. }, a) => get_arithmetic_val(&a)
                .map(|value| one_bound(OneRangeIntBound::High(value - 1), span)),
            (a, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&a)
                .map(|value| one_bound(OneRangeIntBound::Low(value + 1), span)),
            _ => Err(Error::Span(token.span)),
        },
        Ast::GrEq(l, r) => {
            match ((*l), (*r)) {
                (Token { ast: Ast::Val, .. }, a) => get_arithmetic_val(&a)
                    .map(|value| one_bound(OneRangeIntBound::Low(value), span)),
                (a, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&a)
                    .map(|value| one_bound(OneRangeIntBound::High(value), span)),
                _ => Err(Error::Span(token.span)),
            }
        }
        Ast::LeEq(l, r) => {
            match ((*l), (*r)) {
                (Token { ast: Ast::Val, .. }, a) => get_arithmetic_val(&a)
                    .map(|value| one_bound(OneRangeIntBound::High(value), span)),
                (a, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&a)
                    .map(|value| one_bound(OneRangeIntBound::Low(value), span)),
                _ => Err(Error::Span(token.span)),
            }
        }
        Ast::Eq(l, r) => match (*l, *r) {
            (Token { ast: Ast::Val, .. }, t) => Ok(Type::Int(TypeKind {
                name: None,
                kinds: Spanned::new(
                    VecType::one(Int::Value(get_arithmetic_val(&t)?)),
                    token.span,
                ),
            })),
            (t, Token { ast: Ast::Val, .. }) => Ok(Type::Int(TypeKind {
                name: None,
                kinds: Spanned::new(
                    VecType::one(Int::Value(get_arithmetic_val(&t)?)),
                    token.span,
                ),
            })),
            _ => Err(Error::Span(token.span)),
        },
        Ast::NotEq(l, r) => match (*l, *r) {
            (Token { ast: Ast::Val, .. }, t) => get_arithmetic_val(&t)
                .map(|value| one_bound(OneRangeIntBound::NotEqual(value), span)),
            (t, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&t)
                .map(|value| one_bound(OneRangeIntBound::NotEqual(value), span)),
            _ => Err(Error::Span(token.span)),
        },
        Ast::Slice(s) => {
            let from = *s.first;
            let step = *s.second - *s.first;
            let to = *s.last;
            Ok(match step {
                1 => Type::Int(TypeKind::from_kinds(Spanned::new(
                    VecType::one(Int::KnownBound {
                        low: from,
                        high: to,
                    }),
                    span,
                ))),
                _ => Type::Int(TypeKind::from_kinds(Spanned::new(
                    VecType::one(Int::Slice(Slice { from, step, to })),
                    span,
                ))),
            })
        }
        Ast::Implication(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.op_implication(right))
        }),
        Ast::Named(name, def) => Ok(Type::Named(
            Spanned::new(name.0.clone(), name.span),
            Rc::new(RefCell::new(parse_type_helper(*def, ctx)?)),
        )),
        _ => parse_expr(token, ctx).and_then(
            |e| e.call(span).and_then(
                |e| e.try_get_type().map(|t|
                    Type::AnotherType(Spanned::new(t, span))
                ).ok_or(Error::Span(span))
            )),
    }
}

fn one_bound(bound: OneRangeIntBound, span: Span) -> Type {
    Type::Int(TypeKind::from_kinds(Spanned::new(
        VecType::one(Int::Bound(bound)),
        span,
    )))
}

fn get_arithmetic_val(token: &Token) -> Result<i128, Error> {
    token.eval_arithmetic().map_err(|s| Error::Span(s))
}

impl Token {
    pub fn eval_arithmetic(&self) -> Result<i128, Span> {
        match &self.ast {
            Ast::Int(i) => Ok(*i),
            Ast::Add(l, r) => l
                .eval_arithmetic()
                .and_then(|res1| r.eval_arithmetic().and_then(|res2| Ok(res1 + res2))),
            Ast::Sub(l, r) => l
                .eval_arithmetic()
                .and_then(|res1| r.eval_arithmetic().and_then(|res2| Ok(res1 - res2))),
            Ast::Mul(l, r) => l
                .eval_arithmetic()
                .and_then(|res1| r.eval_arithmetic().and_then(|res2| Ok(res1 * res2))),
            Ast::Div(l, r) => l
                .eval_arithmetic()
                .and_then(|res1| r.eval_arithmetic().and_then(|res2| Ok(res1 / res2))),
            Ast::Pow(l, r) => l.eval_arithmetic().and_then(|res1| {
                r.eval_arithmetic()
                    .and_then(|res2| match u32::try_from(res2) {
                        Ok(n) => Ok(res1.pow(n)),
                        Err(_) => Err(r.span),
                    })
            }),
            Ast::Neg(t) => Ok(-t.eval_arithmetic()?),
            _ => Err(self.span),
        }
    }
}
