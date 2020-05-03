use crate::error::Error;
use crate::parser;
use crate::parser::{Ast, Token};
use crate::spanned::{Span, Spanned};
use std::cmp::{max, min};
use std::convert::TryFrom;
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(Type<Int>),
    Type(Type<()>),
    Unknown(Type<()>),
}

impl Value {
    pub fn span(&self) -> Span {
        match self {
            Value::Int(t) => t.span(),
            Value::Type(t) => t.span(),
            Value::Unknown(u) => u.span(),
        }
    }

    pub fn set_name(&mut self, name: Spanned<String>) {
        match self {
            Value::Int(t) => t.name = Some(name),
            Value::Type(t) => t.name = Some(name),
            Value::Unknown(t) => t.name = Some(name),
        }
    }

    pub fn op_add(self, value: Value) -> Result<Self, Error> {
        match self {
            Value::Int(t) => int::add(t, value).map(Value::Int),
            Value::Type(t) => ttype::add(t, value).map(Value::Type),
            Value::Unknown(u) => unknown::add(u, value).map(Value::Type),
        }
    }

    pub fn op_and(self, value: Value) -> Result<Self, Error> {
        match self {
            Value::Int(t) => int::and(t, value).map(Value::Int),
            Value::Type(t) => ttype::and(t, value).map(Value::Type),
            Value::Unknown(u) => unknown::and(u, value).map(Value::Type),
        }
    }

    pub fn op_or(self, value: Value) -> Result<Self, Error> {
        match self {
            Value::Int(t) => int::or(t, value).map(Value::Int),
            Value::Type(t) => ttype::or(t, value).map(Value::Type),
            Value::Unknown(u) => unknown::or(u, value).map(Value::Type),
        }
    }
}

mod int {
    use super::{Error, Int, Type};
    use crate::types::Value;
    use itertools::Itertools;

    pub fn add(left: Type<Int>, right: Value) -> Result<Type<Int>, Error> {
        let Type { name, kinds } = left;
        let kinds = kinds
            .0
            .into_iter()
            .map(|t| t.add(right.clone()))
            .collect::<Result<Vec<_>, Error>>()?
            .into_iter()
            .flatten()
            .collect();
        Ok(Type { name, kinds })
    }

    pub fn and(left: Type<Int>, right: Value) -> Result<Type<Int>, Error> {
        let ty = match right {
            Value::Int(i) => i,
            _ => return Err(Error::Span(left.span().extend(&right.span()))),
        };
        let Type { name, kinds } = left;
        let kinds = kinds
            .0
            .into_iter()
            .cartesian_product(ty.kinds.0.into_iter())
            .map(|(t, i)| t.extend_with_other(i.clone()))
            .collect::<Result<Vec<_>, Error>>()?
            .into_iter()
            .flatten()
            .collect();
        Ok(Type { name, kinds })
    }

    pub fn or(left: Type<Int>, right: Value) -> Result<Type<Int>, Error> {
        let Type { name, kinds } = left;
        match right {
            Value::Int(i) => Ok(Type {
                name,
                kinds: kinds.with(i.kinds),
            }),
            _ => Err(Error::Span(right.span())),
        }
    }
}

// Rust not allowed 'type' in identifiers
mod ttype {
    use super::{Error, Type};
    use crate::types::Value;

    pub fn add(_: Type<()>, right: Value) -> Result<Type<()>, Error> {
        Err(Error::Custom(
            right.span(),
            "+ is not allowed for `Type` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    pub fn and(_: Type<()>, right: Value) -> Result<Type<()>, Error> {
        Err(Error::Custom(
            right.span(),
            "& is not allowed for `Type` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    pub fn or(_: Type<()>, right: Value) -> Result<Type<()>, Error> {
        Err(Error::Custom(
            right.span(),
            "| is not allowed for `Type` value".to_owned(),
            "-here".to_owned(),
        ))
    }
}

mod unknown {
    use super::{Error, Type, Value};

    pub fn add(_: Type<()>, right: Value) -> Result<Type<()>, Error> {
        Err(Error::Custom(
            right.span(),
            "+ is not allowed for `Unknown` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    pub fn and(_: Type<()>, right: Value) -> Result<Type<()>, Error> {
        Err(Error::Custom(
            right.span(),
            "& is not allowed for `Unknown` value".to_owned(),
            "-here".to_owned(),
        ))
    }

    pub fn or(_: Type<()>, right: Value) -> Result<Type<()>, Error> {
        Err(Error::Custom(
            right.span(),
            "| is not allowed for `Unknown` value".to_owned(),
            "-here".to_owned(),
        ))
    }
}

impl Value {
    pub fn name(&self) -> &str {
        match self {
            Value::Int(i) => i
                .name
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("anonymous type"),
            Value::Type(i) => i
                .name
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("anonymous type"),
            Value::Unknown(i) => i
                .name
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("anonymous type"),
        }
    }
    pub fn main_type(&self) -> MainType {
        match self {
            Value::Int(_) => MainType::Int,
            Value::Type(_) => MainType::Type,
            Value::Unknown(_) => MainType::Unknown,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Type<T> {
    name: Option<Spanned<String>>,
    kinds: VecType<Spanned<T>>,
}

impl<T> Type<T> {
    pub fn empty() -> Self {
        Self {
            name: None,
            kinds: VecType(vec![]),
        }
    }
    fn from_kinds(kinds: VecType<Spanned<T>>) -> Self {
        Self { name: None, kinds }
    }
    pub fn fmap<F: FnMut(Spanned<T>) -> Result<Spanned<T>, E>, E>(self, f: F) -> Result<Self, E> {
        let Type { name, kinds } = self;
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

#[derive(Debug, PartialEq, Clone)]
pub enum Int {
    Value(Spanned<i128>),
    Bound(OneRangeIntBound),
    KnownBound {
        low: Spanned<i128>,
        high: Spanned<i128>,
    },
    Slice(Spanned<Slice>),
}

impl Spanned<Int> {
    fn add(self, val: Value) -> Result<VecType<Self>, Error> {
        let val = match val {
            Value::Int(i) => i,
            _ => return Err(Error::Span(val.span())),
        };
        let span = self.span;
        let inner = self.inner();
        val.kinds
            .0
            .into_iter()
            .map(|val| {
                let _span = val.span;
                let val = match val.inner() {
                    Int::Value(v) => *v,
                    _ => return Err(Error::Span(_span)),
                };
                Ok(Spanned::new(
                    match inner.clone() {
                        Int::Value(v) => Int::Value(v + val),
                        Int::Bound(b) => Int::Bound(
                            b.add(val)
                                .map_err(|e| Error::Custom(span, e, "-here".to_string()))?,
                        ),
                        Int::KnownBound { low, high } => Int::KnownBound {
                            low: low + val,
                            high: high + val,
                        },
                        Int::Slice(s) => Int::Slice(Spanned::new(
                            Slice {
                                from: s.from + val,
                                step: s.step,
                                to: s.to + val,
                            },
                            s.span,
                        )),
                    },
                    span,
                ))
            })
            .collect()
    }
}

impl Spanned<Int> {
    fn try_convert_to_value(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
        match &*self {
            Int::Value(i) if *i == value => Ok(VecType::one(self)),
            Int::Bound(OneRangeIntBound::None) => {
                Ok(VecType::one(Spanned::new(Int::Value(value), self.span)))
            }
            Int::Bound(OneRangeIntBound::Low(low)) if *low <= value => {
                Ok(VecType::one(Spanned::new(Int::Value(value), self.span)))
            }
            Int::Bound(OneRangeIntBound::High(high)) if *high >= value => {
                Ok(VecType::one(Spanned::new(Int::Value(value), self.span)))
            }
            Int::Bound(OneRangeIntBound::NotEqual(i)) => {
                Spanned::new(Int::Bound(OneRangeIntBound::High(value)), self.span)
                    .try_add_not_eq_bound(i.clone())
            }
            Int::KnownBound { low, high } if *low <= value && *high >= value => {
                Ok(VecType::one(Spanned::new(Int::Value(value), self.span)))
            }
            Int::Slice(s) if s.contain(*value) => {
                Ok(VecType::one(Spanned::new(Int::Value(value), s.span)))
            }
            _ => Err(Error::NotHaveType(self.span)),
        }
    }
    fn try_add_low_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
        let span = self.span;
        match self.inner() {
            Int::Value(i) if i >= value => Ok(VecType::one(Spanned::new(Int::Value(i), span))),
            Int::Bound(OneRangeIntBound::None) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::Low(value)),
                span,
            ))),
            Int::Bound(OneRangeIntBound::Low(low)) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::Low(max(low, value))),
                span,
            ))),
            Int::Bound(OneRangeIntBound::High(high)) if high >= value => Ok(VecType::one(
                Spanned::new(Int::KnownBound { low: value, high }, span),
            )),
            Int::Bound(OneRangeIntBound::NotEqual(i)) => {
                Spanned::new(Int::Bound(OneRangeIntBound::Low(value)), span).try_add_not_eq_bound(i)
            }
            Int::KnownBound { low, high } if value <= high => Ok(VecType::one(Spanned::new(
                Int::KnownBound {
                    low: max(value, low),
                    high,
                },
                span,
            ))),
            Int::Slice(s) => Ok(VecType::one(Spanned::new(
                Int::Slice(s.try_change_low_bound(*value)?),
                span,
            ))),
            _ => Err(Error::NotHaveType(span)),
        }
    }
    fn try_add_high_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
        let span = self.span;
        match self.inner() {
            Int::Value(i) if i <= value => Ok(VecType::one(Spanned::new(Int::Value(i), span))),
            Int::Bound(OneRangeIntBound::None) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::High(value)),
                span,
            ))),
            Int::Bound(OneRangeIntBound::High(high)) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::High(min(high, value))),
                span,
            ))),
            Int::Bound(OneRangeIntBound::Low(low)) if low <= value => Ok(VecType::one(
                Spanned::new(Int::KnownBound { low, high: value }, span),
            )),
            Int::Bound(OneRangeIntBound::NotEqual(i)) => {
                Spanned::new(Int::Bound(OneRangeIntBound::High(value)), span)
                    .try_add_not_eq_bound(i)
            }
            Int::KnownBound { low, high } if low <= value => Ok(VecType::one(Spanned::new(
                Int::KnownBound {
                    low,
                    high: min(value, high),
                },
                span,
            ))),
            Int::Slice(s) => Ok(VecType::one(Spanned::new(
                Int::Slice(s.try_change_high_bound(*value)?),
                span,
            ))),
            _ => Err(Error::NotHaveType(span)),
        }
    }
    fn extend_with_other(self, other: Self) -> Result<VecType<Self>, Error> {
        match other.inner() {
            Int::Value(i) => self.try_convert_to_value(i),
            Int::Bound(OneRangeIntBound::None) => Ok(VecType::one(self)),
            Int::Bound(OneRangeIntBound::Low(l)) => self.try_add_low_bound(l),
            Int::Bound(OneRangeIntBound::High(h)) => self.try_add_high_bound(h),
            Int::KnownBound { low, high } => self
                .try_add_low_bound(low)
                .and_then(|res| res.try_add_high_bound(high)),
            Int::Bound(OneRangeIntBound::NotEqual(i)) => self.try_add_not_eq_bound(i),
            Int::Slice(s) => self.try_add_slice_bound(s),
        }
    }
    fn try_add_not_eq_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
        let span = self.span;
        match self.inner() {
            Int::Value(i) if i != value => Ok(VecType::one(Spanned::new(Int::Value(i), span))),
            Int::Bound(OneRangeIntBound::None) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::NotEqual(value)),
                span,
            ))),
            Int::Bound(OneRangeIntBound::High(high)) if *high - 1 > *value => Ok(VecType(vec![
                Spanned::new(
                    Int::KnownBound {
                        low: high,
                        high: value.clone() - 1,
                    },
                    span,
                ),
                Spanned::new(Int::Bound(OneRangeIntBound::High(value.clone() + 1)), span),
            ])),
            Int::Bound(OneRangeIntBound::High(high)) if *high - 1 == *value => Ok(VecType(vec![
                Spanned::new(Int::Value(high), span),
                Spanned::new(Int::Bound(OneRangeIntBound::High(value.clone() + 1)), span),
            ])),
            Int::Bound(OneRangeIntBound::High(high)) if *high == *value => Ok(VecType::one(
                Spanned::new(Int::Bound(OneRangeIntBound::High(high + 1)), span),
            )),
            Int::Bound(OneRangeIntBound::High(high)) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::High(high)),
                span,
            ))),
            Int::Bound(OneRangeIntBound::Low(low)) if *low + 1 < *value => Ok(VecType(vec![
                Spanned::new(
                    Int::KnownBound {
                        low,
                        high: value.clone() - 1,
                    },
                    span,
                ),
                Spanned::new(Int::Bound(OneRangeIntBound::High(value.clone() + 1)), span),
            ])),
            Int::Bound(OneRangeIntBound::Low(low)) if *low + 1 == *value => Ok(VecType(vec![
                Spanned::new(Int::Value(low), span),
                Spanned::new(Int::Bound(OneRangeIntBound::Low(value.clone() + 1)), span),
            ])),
            Int::Bound(OneRangeIntBound::Low(low)) if *low == *value => Ok(VecType::one(
                Spanned::new(Int::Bound(OneRangeIntBound::Low(low + 1)), span),
            )),
            Int::Bound(OneRangeIntBound::Low(low)) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::Low(low)),
                span,
            ))),
            Int::KnownBound { low, high } if *low + 1 < *value && *value < *high - 1 => {
                Ok(VecType(vec![
                    Spanned::new(
                        Int::KnownBound {
                            low,
                            high: value.clone() - 1,
                        },
                        span,
                    ),
                    Spanned::new(
                        Int::KnownBound {
                            low: value + 1,
                            high,
                        },
                        span,
                    ),
                ]))
            }
            Int::KnownBound { low, high } if *low + 1 == *value => Ok(VecType(vec![
                Spanned::new(Int::Value(low), span),
                Spanned::new(
                    Int::KnownBound {
                        low: value + 1,
                        high,
                    },
                    span,
                ),
            ])),
            Int::KnownBound { low, high } if *low == *value => Ok(VecType::one(Spanned::new(
                Int::KnownBound {
                    low: value + 1,
                    high,
                },
                span,
            ))),
            Int::KnownBound { low, high } if *high - 1 == *value => Ok(VecType(vec![
                Spanned::new(Int::Value(high), span),
                Spanned::new(
                    Int::KnownBound {
                        low,
                        high: value - 1,
                    },
                    span,
                ),
            ])),
            Int::KnownBound { low, high } if *high == *value => Ok(VecType::one(Spanned::new(
                Int::KnownBound {
                    low,
                    high: value - 1,
                },
                span,
            ))),
            Int::KnownBound { low, high } => Ok(VecType::one(Spanned::new(
                Int::KnownBound { low, high },
                span,
            ))),
            Int::Slice(s) => {
                let res = s.try_split(*value)?;
                Ok(res
                    .into_iter()
                    .map(|e| {
                        let span = e.span;
                        Spanned::new(Int::Slice(e), span)
                    })
                    .collect())
            }
            _ => Err(Error::NotHaveType(span)),
        }
    }
    fn try_add_slice_bound(self, value: Spanned<Slice>) -> Result<VecType<Self>, Error> {
        let span = self.span;
        match self.inner() {
            Int::Value(i) if value.contain(*i) => {
                Ok(VecType::one(Spanned::new(Int::Value(i), span)))
            }
            Int::Bound(OneRangeIntBound::None) => {
                Ok(VecType::one(Spanned::new(Int::Slice(value), span)))
            }
            Int::Bound(OneRangeIntBound::Low(low)) => Ok(VecType::one(Spanned::new(
                Int::Slice(value.try_change_low_bound(*low)?),
                span,
            ))),
            Int::Bound(OneRangeIntBound::High(high)) => Ok(VecType::one(Spanned::new(
                Int::Slice(value.try_change_high_bound(*high)?),
                span,
            ))),
            Int::Bound(OneRangeIntBound::NotEqual(i)) => {
                let span = value.span;
                Spanned::new(Int::Slice(value), span).try_add_not_eq_bound(i)
            }
            Int::KnownBound { low, high } => {
                let slice = value
                    .try_change_low_bound(*low)?
                    .try_change_high_bound(*high)?;
                Ok(VecType::one(Spanned::new(Int::Slice(slice), span)))
            }
            Int::Slice(_) => unimplemented!(),
            _ => Err(Error::NotHaveType(span)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum OneRangeIntBound {
    None,
    NotEqual(Spanned<i128>),
    Low(Spanned<i128>),
    High(Spanned<i128>),
}

impl OneRangeIntBound {
    pub fn add(self, val: i128) -> Result<Self, String> {
        match self {
            OneRangeIntBound::None => Err(format!("try add {} to empty bound", val)),
            OneRangeIntBound::Low(l) => Ok(OneRangeIntBound::Low(l + val)),
            OneRangeIntBound::NotEqual(l) => Ok(OneRangeIntBound::NotEqual(l + val)),
            OneRangeIntBound::High(h) => Ok(OneRangeIntBound::High(h + val)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Slice {
    pub from: i128,
    pub step: i128,
    pub to: i128,
}

impl Spanned<Slice> {
    pub fn contain(&self, v: i128) -> bool {
        v >= self.from && v <= self.to && (v - self.from) % self.step == 0
    }
    pub fn try_change_low_bound(self, v: i128) -> Result<Self, Error> {
        match v > self.to {
            true => Err(Error::VoidType(self.span)),
            false => {
                let span = self.span;
                let inner = self.inner();
                Ok(Spanned::new(Slice { from: v, ..inner }, span))
            }
        }
    }
    pub fn try_change_high_bound(self, v: i128) -> Result<Self, Error> {
        match v < self.from {
            true => Err(Error::VoidType(self.span)),
            false => {
                let span = self.span;
                let inner = self.inner();
                Ok(Spanned::new(Slice { to: v, ..inner }, span))
            }
        }
    }
    pub fn try_split(self, v: i128) -> Result<Vec<Self>, Error> {
        match self.contain(v) {
            true => {
                let span = self.span;
                let inner = self.inner();

                unimplemented!()
            }
            false => unimplemented!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MainType {
    Int,
    Type,
    Unknown,
}

pub fn parse_type(
    type_def: Spanned<parser::Type>,
    types: &[Spanned<Value>],
) -> Result<Spanned<Value>, Error> {
    let span = type_def.span;
    let parser::Type(name, def) = type_def.inner();
    let name_span = name.span;
    let name = Spanned::new(name.inner().0, name_span);
    let mut t_type = parse_type_helper(def, types)?;
    t_type.set_name(name);
    Ok(Spanned::new(t_type, span))
}

fn parse_type_helper(token: Token, types: &[Spanned<Value>]) -> Result<Value, Error> {
    let span = token.span;
    match token.ast {
        Ast::And(l, r) => parse_type_helper(*l, types)
            .and_then(|left| parse_type_helper(*r, types).and_then(|right| left.op_and(right))),
        Ast::Or(l, r) => parse_type_helper(*l, types)
            .and_then(|left| parse_type_helper(*r, types).and_then(|right| left.op_or(right))),
        Ast::Add(l, r) => parse_type_helper(*l, types)
            .and_then(|left| parse_type_helper(*r, types).and_then(|right| left.op_add(right))),
        Ast::Int(i) => Ok(Value::Int(Type {
            name: None,
            kinds: VecType::one(Spanned::new(
                Int::Value(Spanned::new(i, token.span)),
                token.span,
            )),
        })),
        Ast::Parenthesis(t) => parse_type_helper(*t, types),
        Ast::Ident(i) => match i.0.as_str() {
            "Int" => Ok(Value::Int(Type::empty())),
            "Type" => Ok(Value::Type(Type::empty())),
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
            (Token { ast: Ast::Val, .. }, t) => Ok(Value::Int(Type {
                name: None,
                kinds: VecType::one(Spanned::new(
                    Int::Value(get_arithmetic_val(&t)?),
                    token.span,
                )),
            })),
            (t, Token { ast: Ast::Val, .. }) => Ok(Value::Int(Type {
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
                1 => Value::Int(Type::from_kinds(VecType::one(Spanned::new(
                    Int::KnownBound {
                        low: s.first,
                        high: s.last,
                    },
                    span,
                )))),
                _ => Value::Int(Type::from_kinds(VecType::one(Spanned::new(
                    Int::Slice(Spanned::new(Slice { from, step, to }, span)),
                    span,
                )))),
            })
        }
        t => {
            dbg!(t);
            unimplemented!()
        }
    }
}

fn one_bound(bound: OneRangeIntBound, span: Span) -> Value {
    Value::Int(Type::from_kinds(VecType::one(Spanned::new(
        Int::Bound(bound),
        span,
    ))))
}

#[derive(Debug, PartialEq, Clone)]
struct VecType<T>(Vec<T>);

impl<T> From<Vec<T>> for VecType<T> {
    fn from(v: Vec<T>) -> Self {
        Self(v)
    }
}

impl<T> Deref for VecType<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for VecType<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> IntoIterator for VecType<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> VecType<T> {
    pub fn one(t: T) -> Self {
        Self(vec![t])
    }
}

impl<T> FromIterator<T> for VecType<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl VecType<Spanned<Int>> {
    pub fn try_convert_to_value(self, value: Spanned<i128>) -> Result<Self, Error> {
        self.0
            .into_iter()
            .map(|i| i.try_convert_to_value(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|res| res.into_iter().next().unwrap())
    }
    pub fn try_add_low_bound(self, value: Spanned<i128>) -> Result<Self, Error> {
        self.0
            .into_iter()
            .map(|i| i.try_add_low_bound(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|r| r.into_iter().flatten().collect::<Self>())
    }
    pub fn try_add_high_bound(self, value: Spanned<i128>) -> Result<Self, Error> {
        self.0
            .into_iter()
            .map(|i| i.try_add_high_bound(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|r| r.into_iter().flatten().collect::<Self>())
    }
    pub fn try_add_not_eq_bound(self, value: Spanned<i128>) -> Result<Self, Error> {
        self.0
            .into_iter()
            .map(|i| i.try_add_not_eq_bound(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|r| r.into_iter().flatten().collect::<Self>())
    }
    pub fn try_add_slice_bound(self, value: Spanned<Slice>) -> Result<Self, Error> {
        self.0
            .into_iter()
            .map(|i| i.try_add_slice_bound(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|r| r.into_iter().flatten().collect::<Self>())
    }
    pub fn with(self, other: Self) -> Self {
        let mut vec = self.0;
        vec.extend_from_slice(other.as_slice());
        Self(vec)
    }
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
