use crate::error::Error;
use crate::parser;
use crate::parser::{Ast, Token};
use crate::spanned::{Spanned, Span};
use std::cmp::{max, min};
use std::ops::{Deref, DerefMut};
use std::iter::FromIterator;
use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
pub enum AllType {
    Int(Type<Int>),
}

impl AllType {
    pub fn name(&self) -> &str {
        match self {
            AllType::Int(i) => i.name.as_str(),
        }
    }
    pub fn main_type(&self) -> MainType {
        match self {
            AllType::Int(_) => MainType::Int,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Type<T> {
    name: Spanned<String>,
    kinds: Vec<Spanned<T>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Int {
    Value(Spanned<i128>),
    Bound(OneRangeIntBound, Vec<IntBound>),
    KnownBound {
        low: Spanned<i128>,
        high: Spanned<i128>,
        bounds: Vec<IntBound>,
    },
    Slice(Spanned<Slice>),
}

impl Int {
    fn is_empty(&self) -> bool {
        match self {
            Int::Bound(OneRangeIntBound::None, v) => v.is_empty(),
            _ => false,
        }
    }
}

impl Spanned<Int> {
    fn try_convert_to_value(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
        match &*self {
            Int::Value(i) if *i == value => Ok(VecType::one(self)),
            Int::Bound(OneRangeIntBound::None, bounds) => {
                Ok(VecType::one(Spanned::new(Int::Value(value), self.span)))
            }
            Int::Bound(OneRangeIntBound::Low(low), bounds) if *low <= value => {
                Ok(VecType::one(Spanned::new(Int::Value(value), self.span)))
            }
            Int::Bound(OneRangeIntBound::High(high), bounds) if *high >= value => {
                Ok(VecType::one(Spanned::new(Int::Value(value), self.span)))
            }
            Int::Bound(OneRangeIntBound::NotEqual(i), bounds) => Spanned::new(
                Int::Bound(OneRangeIntBound::High(value), bounds.clone()),
                self.span,
            )
            .try_add_not_eq_bound(i.clone()),
            Int::KnownBound { low, high, bounds } if *low <= value && *high >= value => {
                Ok(VecType::one(Spanned::new(Int::Value(value), self.span)))
            }
            _ => Err(Error::NotHaveType(self.span)),
        }
    }
    fn try_add_low_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
        let span = self.span;
        match self.inner() {
            Int::Value(i) if i >= value => Ok(VecType::one(Spanned::new(Int::Value(i), span))),
            Int::Bound(OneRangeIntBound::None, bounds) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::Low(value), bounds),
                span,
            ))),
            Int::Bound(OneRangeIntBound::Low(low), bounds) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::Low(max(low, value)), bounds),
                span,
            ))),
            Int::Bound(OneRangeIntBound::High(high), bounds) if high >= value => {
                Ok(VecType::one(Spanned::new(
                    Int::KnownBound {
                        low: value,
                        high,
                        bounds,
                    },
                    span,
                )))
            }
            Int::Bound(OneRangeIntBound::NotEqual(i), bounds) => {
                Spanned::new(Int::Bound(OneRangeIntBound::Low(value), bounds), span)
                    .try_add_not_eq_bound(i)
            }
            Int::KnownBound { low, high, bounds } if value <= high => {
                Ok(VecType::one(Spanned::new(
                    Int::KnownBound {
                        low: max(value, low),
                        high,
                        bounds,
                    },
                    span,
                )))
            }
            _ => Err(Error::NotHaveType(span)),
        }
    }
    fn try_add_high_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
        let span = self.span;
        match self.inner() {
            Int::Value(i) if i <= value => Ok(VecType::one(Spanned::new(Int::Value(i), span))),
            Int::Bound(OneRangeIntBound::None, bounds) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::High(value), bounds),
                span,
            ))),
            Int::Bound(OneRangeIntBound::High(high), bounds) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::High(min(high, value)), bounds),
                span,
            ))),
            Int::Bound(OneRangeIntBound::Low(low), bounds) if low <= value => {
                Ok(VecType::one(Spanned::new(
                    Int::KnownBound {
                        low,
                        high: value,
                        bounds,
                    },
                    span,
                )))
            }
            Int::Bound(OneRangeIntBound::NotEqual(i), bounds) => {
                Spanned::new(Int::Bound(OneRangeIntBound::High(value), bounds), span)
                    .try_add_not_eq_bound(i)
            }
            Int::KnownBound { low, high, bounds } if low <= value => {
                Ok(VecType::one(Spanned::new(
                    Int::KnownBound {
                        low,
                        high: min(value, high),
                        bounds,
                    },
                    span,
                )))
            }
            _ => Err(Error::NotHaveType(span)),
        }
    }
    fn extend_with_other(self, other: Self) -> Result<VecType<Self>, Error> {
        match other.inner() {
            Int::Value(i) => self.try_convert_to_value(i),
            Int::Bound(OneRangeIntBound::None, _) => Ok(VecType::one(self)),
            Int::Bound(OneRangeIntBound::Low(l), _) => self.try_add_low_bound(l),
            Int::Bound(OneRangeIntBound::High(h), _) => self.try_add_high_bound(h),
            Int::KnownBound {
                low,
                high,
                bounds: _,
            } => self
                .try_add_low_bound(low)
                .and_then(|res| res.try_add_high_bound(high)),
            Int::Bound(OneRangeIntBound::NotEqual(i), _) => self.try_add_not_eq_bound(i),
            Int::Slice(_) => unimplemented!(),
        }
    }
    fn try_add_not_eq_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
        let span = self.span;
        match self.inner() {
            Int::Value(i) if i != value => Ok(VecType::one(Spanned::new(Int::Value(i), span))),
            Int::Bound(OneRangeIntBound::None, bounds) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::NotEqual(value), bounds),
                span,
            ))),
            Int::Bound(OneRangeIntBound::High(high), bounds) if *high - 1 > *value => {
                Ok(VecType(vec![
                    Spanned::new(
                        Int::KnownBound {
                            low: high,
                            high: value.clone() - 1,
                            bounds: bounds.clone(),
                        },
                        span,
                    ),
                    Spanned::new(
                        Int::Bound(OneRangeIntBound::High(value.clone() + 1), bounds),
                        span,
                    ),
                ]))
            }
            Int::Bound(OneRangeIntBound::High(high), bounds) if *high - 1 == *value => {
                Ok(VecType(vec![
                    Spanned::new(Int::Value(high), span),
                    Spanned::new(
                        Int::Bound(OneRangeIntBound::High(value.clone() + 1), bounds),
                        span,
                    ),
                ]))
            }
            Int::Bound(OneRangeIntBound::High(high), bounds) if *high == *value => {
                Ok(VecType::one(Spanned::new(
                    Int::Bound(OneRangeIntBound::High(high + 1), bounds),
                    span,
                )))
            }
            Int::Bound(OneRangeIntBound::High(high), bounds) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::High(high), bounds),
                span,
            ))),
            Int::Bound(OneRangeIntBound::Low(low), bounds) if *low + 1 < *value => {
                Ok(VecType(vec![
                    Spanned::new(
                        Int::KnownBound {
                            low,
                            high: value.clone() - 1,
                            bounds: bounds.clone(),
                        },
                        span,
                    ),
                    Spanned::new(
                        Int::Bound(OneRangeIntBound::High(value.clone() + 1), bounds),
                        span,
                    ),
                ]))
            }
            Int::Bound(OneRangeIntBound::Low(low), bounds) if *low + 1 == *value => {
                Ok(VecType(vec![
                    Spanned::new(Int::Value(low), span),
                    Spanned::new(
                        Int::Bound(OneRangeIntBound::Low(value.clone() + 1), bounds),
                        span,
                    ),
                ]))
            }
            Int::Bound(OneRangeIntBound::Low(low), bounds) if *low == *value => Ok(VecType::one(
                Spanned::new(Int::Bound(OneRangeIntBound::Low(low + 1), bounds), span),
            )),
            Int::Bound(OneRangeIntBound::Low(low), bounds) => Ok(VecType::one(Spanned::new(
                Int::Bound(OneRangeIntBound::Low(low), bounds),
                span,
            ))),
            Int::KnownBound { low, high, bounds } if *low + 1 < *value && *value < *high - 1 => {
                Ok(VecType(vec![
                    Spanned::new(
                        Int::KnownBound {
                            low,
                            high: value.clone() - 1,
                            bounds: bounds.clone(),
                        },
                        span,
                    ),
                    Spanned::new(
                        Int::KnownBound {
                            low: value + 1,
                            high,
                            bounds,
                        },
                        span,
                    ),
                ]))
            }
            Int::KnownBound { low, high, bounds } if *low + 1 == *value => Ok(VecType(vec![
                Spanned::new(Int::Value(low), span),
                Spanned::new(
                    Int::KnownBound {
                        low: value + 1,
                        high,
                        bounds,
                    },
                    span,
                ),
            ])),
            Int::KnownBound { low, high, bounds } if *low == *value => {
                Ok(VecType::one(Spanned::new(
                    Int::KnownBound {
                        low: value + 1,
                        high,
                        bounds,
                    },
                    span,
                )))
            }
            Int::KnownBound { low, high, bounds } if *high - 1 == *value => Ok(VecType(vec![
                Spanned::new(Int::Value(high), span),
                Spanned::new(
                    Int::KnownBound {
                        low,
                        high: value - 1,
                        bounds,
                    },
                    span,
                ),
            ])),
            Int::KnownBound { low, high, bounds } if *high == *value => {
                Ok(VecType::one(Spanned::new(
                    Int::KnownBound {
                        low,
                        high: value - 1,
                        bounds,
                    },
                    span,
                )))
            }
            Int::KnownBound { low, high, bounds } => Ok(VecType::one(Spanned::new(
                Int::KnownBound { low, high, bounds },
                span,
            ))),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Slice {
    pub from: i128,
    pub step: i128,
    pub to: i128,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntBound {
    Modulo(Spanned<i128>, Token),
}

#[derive(Debug, PartialEq)]
pub enum MainType {
    Int,
}

pub fn parse_type(
    type_def: Spanned<parser::Type>,
    types: &[Spanned<AllType>],
) -> Result<Spanned<AllType>, Error> {
    let span = type_def.span;
    let parser::Type(name, def) = type_def.inner();
    let name_span = name.span;
    let name = Spanned::new(name.inner().0, name_span);
    Ok(Spanned::new(parse_int(name, def, types)?, span))
}

fn parse_int(
    name: Spanned<String>,
    token: Token,
    types: &[Spanned<AllType>],
) -> Result<AllType, Error> {
    let span = token.span;
    let all_types = parse_int_with_cur(
        token,
        types,
        VecType::one(Spanned::new(Int::Bound(OneRangeIntBound::None, vec![]), span)),
    )?;
    Ok(AllType::Int(Type {
        name,
        kinds: all_types.to_vec(),
    }))
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
    pub fn to_vec(self) -> Vec<T> { self.0 }
}

impl<T> FromIterator<T> for VecType<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
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
    pub fn with(self, other: Self) -> Self {
        let mut vec = self.0;
        vec.extend_from_slice(other.as_slice());
        Self(vec)
    }
}

fn parse_int_with_cur(
    token: Token,
    types: &[Spanned<AllType>],
    cur: VecType<Spanned<Int>>,
) -> Result<VecType<Spanned<Int>>, Error> {
    match token.ast {
        Ast::Ident(i) => Ok(cur),
        Ast::Int(i) => cur.try_convert_to_value(Spanned::new(i, token.span)),
        Ast::Gr(l, r) => match ((*l), (*r)) {
            (Token { ast: Ast::Val, .. }, a) => {
                get_arithmetic_val(&a).and_then(|value| cur.try_add_low_bound(value + 1))
            }
            (a, Token { ast: Ast::Val, .. }) => {
                get_arithmetic_val(&a).and_then(|value| cur.try_add_high_bound(value - 1))
            }
            r => Err(Error::Span(token.span)),
        },
        Ast::Le(l, r) => match ((*l), (*r)) {
            (Token { ast: Ast::Val, .. }, a) => {
                get_arithmetic_val(&a).and_then(|value| cur.try_add_high_bound(value - 1))
            }
            (a, Token { ast: Ast::Val, .. }) => {
                get_arithmetic_val(&a).and_then(|value| cur.try_add_low_bound(value + 1))
            }
            _ => Err(Error::Span(token.span)),
        },
        Ast::GrEq(l, r) => match ((*l), (*r)) {
            (Token { ast: Ast::Val, .. }, a) => {
                get_arithmetic_val(&a).and_then(|value| cur.try_add_low_bound(value))
            }
            (a, Token { ast: Ast::Val, .. }) => {
                get_arithmetic_val(&a).and_then(|value| cur.try_add_high_bound(value))
            }
            r => Err(Error::Span(token.span)),
        },
        Ast::LeEq(l, r) => match ((*l), (*r)) {
            (Token { ast: Ast::Val, .. }, a) => {
                get_arithmetic_val(&a).and_then(|value| cur.try_add_high_bound(value))
            }
            (a, Token { ast: Ast::Val, .. }) => {
                get_arithmetic_val(&a).and_then(|value| cur.try_add_low_bound(value))
            }
            _ => Err(Error::Span(token.span)),
        },
        Ast::And(l, r) => {
            parse_int_with_cur(*l, types, cur).and_then(|res| parse_int_with_cur(*r, types, res))
        }
        Ast::Or(l, r) => {
            let cur_l = parse_int_with_cur(*l, types, cur.clone())?;
            let cur_r = parse_int_with_cur(*r, types, cur.clone())?;
            Ok(cur_l.with(cur_r))
        }
        Ast::Parenthesis(t) => parse_int_with_cur(*t, types, cur),
        Ast::Eq(l, r) => match (*l, *r) {
            (Token { ast: Ast::Val, .. }, t) => {
                get_arithmetic_val(&t).and_then(|value| cur.try_convert_to_value(value))
            }
            (t, Token { ast: Ast::Val, .. }) => {
                get_arithmetic_val(&t).and_then(|value| cur.try_convert_to_value(value))
            }
            _ => Err(Error::Span(token.span)),
        },
        Ast::NotEq(l, r) => match (*l, *r) {
            (Token { ast: Ast::Val, .. }, t) => {
                get_arithmetic_val(&t).and_then(|value| cur.try_add_not_eq_bound(value))
            }
            (t, Token { ast: Ast::Val, .. }) => {
                get_arithmetic_val(&t).and_then(|value| cur.try_add_not_eq_bound(value))
            }
            _ => Err(Error::Span(token.span)),
        },
        t => {
            dbg!(t);
            unimplemented!()
        }
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

pub fn get_type(token: Token, types: &[Spanned<AllType>]) -> Result<MainType, Error> {
    token
        .check_type(types)
        .and_then(|t| t.ok_or(Error::NotHaveType(token.span)))
}

impl Token {
    fn check_type(&self, types: &[Spanned<AllType>]) -> Result<Option<MainType>, Error> {
        match &self.ast {
            Ast::Ident(i) => match i.0.as_str() {
                "Int" => Ok(Some(MainType::Int)),
                i => Ok(types.iter().find(|t| t.name() == i).map(|t| t.main_type())),
            },
            Ast::And(l, r) => {
                let type1 = l.check_type(types)?;
                let type2 = r.check_type(types)?;
                match (type1, type2) {
                    (None, None) => Ok(None),
                    (Some(t), None) => Ok(Some(t)),
                    (None, Some(t)) => Ok(Some(t)),
                    (Some(t1), Some(t2)) if t1 == t2 => Ok(Some(t1)),
                    (Some(t1), Some(t2)) => Err(Error::DifferentTypes(l.span, r.span)),
                }
            }
            Ast::Or(l, r) => {
                let type1 = l.check_type(types)?;
                let type2 = r.check_type(types)?;
                match (type1, type2) {
                    (None, None) => Err(Error::NotHaveType(l.span)),
                    (Some(t), None) => Err(Error::NotHaveType(r.span)),
                    (None, Some(t)) => Err(Error::NotHaveType(l.span)),
                    (Some(t1), Some(t2)) if t1 == t2 => Ok(Some(t1)),
                    (Some(t1), Some(t2)) => Err(Error::DifferentTypes(l.span, r.span)),
                }
            }
            _ => Ok(None),
        }
    }
}
