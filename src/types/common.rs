use crate::error::Error;
use crate::parser;
use crate::parser::{Ast, Token};
use crate::spanned::{Span, Spanned};
use std::cmp::{max, min};
use std::convert::TryFrom;
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};
use crate::types::vec_type::VecType;
use crate::types::int::{OneRangeIntBound, Slice, Int};
use crate::types::function::Function;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Function(TypeKind<Function>),
    Int(TypeKind<Int>),
    Type(TypeKind<TypeType>),
    Unknown(TypeKind<Unknown>),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Int(t) => t.span(),
            Type::Type(t) => t.span(),
            Type::Unknown(u) => u.span(),
            Type::Function(t) => t.span(),
        }
    }

    pub fn set_name(&mut self, name: Spanned<String>) {
        match self {
            Type::Int(t) => t.name = Some(name),
            Type::Type(t) => t.name = Some(name),
            Type::Unknown(t) => t.name = Some(name),
            Type::Function(t) => t.name = Some(name),
        }
    }

    pub fn op_add(self, value: Type) -> Result<Self, Error> {
        match self {
            Type::Int(t) => t.add(value).map(Type::Int),
            Type::Type(t) => t.add(value).map(Type::Type),
            Type::Unknown(t) => t.add(value).map(Type::Unknown),
            Type::Function(t) => t.add(value).map(Type::Function),
        }
    }

    pub fn op_and(self, value: Type) -> Result<Self, Error> {
        match self {
            Type::Int(t) => t.and(value).map(Type::Int),
            Type::Type(t) => t.and(value).map(Type::Type),
            Type::Unknown(t) => t.and(value).map(Type::Unknown),
            Type::Function(t) => t.and(value).map(Type::Function),
        }
    }

    pub fn op_or(self, value: Type) -> Result<Self, Error> {
        match self {
            Type::Int(t) => t.or(value).map(Type::Int),
            Type::Type(t) => t.or(value).map(Type::Type),
            Type::Unknown(t) => t.or(value).map(Type::Unknown),
            Type::Function(t) => t.or(value).map(Type::Function),
        }
    }

    pub fn op_implication(self, value: Type) -> Result<Self, Error> {
        Ok(self.implication(value))
    }

    pub fn count_args(&self) -> u8 {
        match self {
            Type::Function(t) => 1 + t.kinds.first().unwrap().return_value.count_args(),
            _ => 0,
        }
    }
}

pub trait TypeOperable<T>: Sized {
    fn add(self, right: Type) -> Result<Self, Error>;
    fn and(self, right: Type) -> Result<Self, Error>;
    fn or(self, right: Type) -> Result<Self, Error>;
}

impl Type {
    pub fn implication(self, other: Self) -> Self {
        let span = self.span().extend(&other.span());
        Type::Function(TypeKind::from_kinds(VecType::one(Spanned::new(Function {
            get_value: self,
            return_value: other
        }, span))))
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
        }
    }
    // TODO: remove it
    pub fn main_type(&self) -> MainType {
        match self {
            Type::Int(_) => MainType::Int,
            Type::Type(_) => MainType::Type,
            Type::Unknown(_) => MainType::Unknown,
            Type::Function(_) => MainType::Function,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeKind<T> {
    pub name: Option<Spanned<String>>,
    pub kinds: VecType<Spanned<T>>,
}

impl<T> TypeKind<T> {
    pub fn empty() -> Self {
        Self {
            name: None,
            kinds: VecType(vec![]),
        }
    }
    pub fn from_kinds(kinds: VecType<Spanned<T>>) -> Self {
        Self { name: None, kinds }
    }
    pub fn fmap<F: FnMut(Spanned<T>) -> Result<Spanned<T>, E>, E>(self, f: F) -> Result<Self, E> {
        let TypeKind { name, kinds } = self;
        kinds
            .into_iter()
            .map(f)
            .collect::<Result<VecType<Spanned<T>>, E>>()
            .map(|kinds| Self { kinds, name })
    }
    pub fn span(&self) -> Span {
        self.kinds
            .first()
            .unwrap()
            .span
            .extend(&self.kinds.last().unwrap().span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeType;

impl TypeOperable<TypeType> for TypeKind<TypeType> {
    fn add(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "+ is not allowed for `Type` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    fn and(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "& is not allowed for `Type` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    fn or(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "| is not allowed for `Type` value".to_owned(),
            "-here".to_owned(),
        ))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unknown;

impl TypeOperable<Unknown> for TypeKind<Unknown> {
    fn add(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "+ is not allowed for `Unknown` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    fn and(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "& is not allowed for `Unknown` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    fn or(self, right: Type) -> Result<Self, Error> {
        Err(Error::Custom(
            right.span(),
            "| is not allowed for `Unknown` value".to_owned(),
            "-here".to_owned(),
        ))
    }
}

#[derive(Debug, PartialEq)]
pub enum MainType {
    Int,
    Function,
    Type,
    Unknown,
}

pub fn parse_type(
    type_def: Spanned<parser::Type>,
    types: &[Spanned<Type>],
) -> Result<Spanned<Type>, Error> {
    let span = type_def.span;
    let parser::Type(name, def) = type_def.inner();
    let name_span = name.span;
    let name = Spanned::new(name.inner().0, name_span);
    let mut t_type = parse_type_helper(def, types)?;
    t_type.set_name(name);
    Ok(Spanned::new(t_type, span))
}

pub fn parse_type_helper(token: Token, types: &[Spanned<Type>]) -> Result<Type, Error> {
    let span = token.span;
    match token.ast {
        Ast::And(l, r) => parse_type_helper(*l, types)
            .and_then(|left| parse_type_helper(*r, types).and_then(|right| left.op_and(right))),
        Ast::Or(l, r) => parse_type_helper(*l, types)
            .and_then(|left| parse_type_helper(*r, types).and_then(|right| left.op_or(right))),
        Ast::Add(l, r) => parse_type_helper(*l, types)
            .and_then(|left| parse_type_helper(*r, types).and_then(|right| left.op_add(right))),
        Ast::Int(i) => Ok(Type::Int(TypeKind {
            name: None,
            kinds: VecType::one(Spanned::new(
                Int::Value(Spanned::new(i, token.span)),
                token.span,
            )),
        })),
        Ast::Parenthesis(t) => parse_type_helper(*t, types),
        Ast::Ident(i) => match i.0.as_str() {
            "Int" => Ok(Type::Int(TypeKind::empty())),
            "Type" => Ok(Type::Type(TypeKind::empty())),
            _ => Err(Error::Span(token.span)),
        },
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
                kinds: VecType::one(Spanned::new(
                    Int::Value(get_arithmetic_val(&t)?),
                    token.span,
                )),
            })),
            (t, Token { ast: Ast::Val, .. }) => Ok(Type::Int(TypeKind {
                name: None,
                kinds: VecType::one(Spanned::new(
                    Int::Value(get_arithmetic_val(&t)?),
                    token.span,
                )),
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
                1 => Type::Int(TypeKind::from_kinds(VecType::one(Spanned::new(
                    Int::KnownBound {
                        low: s.first,
                        high: s.last,
                    },
                    span,
                )))),
                _ => Type::Int(TypeKind::from_kinds(VecType::one(Spanned::new(
                    Int::Slice(Spanned::new(Slice { from, step, to }, span)),
                    span,
                )))),
            })
        }
        Ast::Implication(l, r) => {
            parse_type_helper(*l, types).and_then(|left| {
                parse_type_helper(*r, types).and_then(|right| {
                    left.op_implication(right)
                })
            })
        }
        t => {
            dbg!(t);
            unimplemented!()
        }
    }
}

fn one_bound(bound: OneRangeIntBound, span: Span) -> Type {
    Type::Int(TypeKind::from_kinds(VecType::one(Spanned::new(
        Int::Bound(bound),
        span,
    ))))
}

fn get_arithmetic_val(token: &Token) -> Result<Spanned<i128>, Error> {
    Ok(Spanned::new(
        token.eval_arithmetic().map_err(|s| Error::Span(s))?,
        token.span,
    ))
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
