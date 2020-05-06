use crate::error::Error;
use crate::spanned::Spanned;
use crate::types::common::Type;
use crate::types::vec_type::VecType;
use crate::types::{TypeKind, TypeOperable};
use itertools::Itertools;
use std::cmp::{max, min};
use std::ops::Add;
use std::fmt::{Display, Formatter};

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

impl Int {
    pub fn is_part_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Int::Slice(l), r) => {
                match r {
                    Int::Value(v) => (**v - l.to) % l.step == 0,
                    Int::Slice(r) => l.from >= r.from && l.to <= r.to && r.step % l.step == 0,
                    _ => false
                }
            }
            (l, Int::Slice(r)) => unimplemented!(),
            _ => {
                let (minx, maxx) = self.min_and_max();
                let (miny, maxy) = other.min_and_max();
                minx >= miny && maxx <= maxy
            }
        }
    }
    pub fn min_and_max(&self) -> (i128, i128) {
        match self {
            Int::Value(v) => (**v, **v),
            Int::Bound(b) => match b {
                OneRangeIntBound::High(h) => (i128::min_value(), **h),
                OneRangeIntBound::Low(l) => (**l, i128::max_value()),
                _ => (i128::min_value(), i128::max_value())
            }
            Int::KnownBound { low, high } => (**low, **high),
            Int::Slice(s) => (s.from, s.to)
        }
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Int::Value(i) => i.fmt(f),
            Int::Bound(i) => i.fmt(f),
            Int::KnownBound { low, high } => f.write_str(&format!("(val>={} & val<={})", low, high)),
            Int::Slice(s) => f.write_str(&format!("val={{from: {}; step: {}; to: {};}}", s.from, s.step, s.to)),
        }
    }
}

impl TypeOperable<Int> for TypeKind<Int> {
    fn add(self, right: Type) -> Result<Self, Error> {
        let TypeKind { name, kinds } = self;
        let kinds = kinds
            .inner()
            .into_iter()
            .map(|t| t.add(right.clone()))
            .collect::<Result<Vec<_>, Error>>()?
            .into_iter()
            .flatten()
            .collect();
        Ok(TypeKind { name, kinds })
    }

    fn and(self, right: Type) -> Result<Self, Error> {
        let ty = match right {
            Type::Int(i) => i,
            _ => return Err(Error::Span(self.span().extend(&right.span()))),
        };
        let TypeKind { name, kinds } = self;
        let kinds = kinds
            .inner()
            .into_iter()
            .cartesian_product(ty.kinds.inner().into_iter())
            .map(|(t, i)| t.extend_with_other(i.clone()))
            .collect::<Result<Vec<_>, Error>>()?
            .into_iter()
            .flatten()
            .collect();
        Ok(TypeKind { name, kinds })
    }

    fn or(self, right: Type) -> Result<Self, Error> {
        let TypeKind { name, kinds } = self;
        match right {
            Type::Int(i) => Ok(TypeKind {
                name,
                kinds: kinds.with(i.kinds),
            }),
            _ => Err(Error::Span(right.span())),
        }
    }
}

impl Spanned<Int> {
    pub(crate) fn add(self, val: Type) -> Result<VecType<Self>, Error> {
        let val = match val {
            Type::Int(i) => i,
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
    pub(crate) fn try_convert_to_value(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
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
    pub(crate) fn try_add_low_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
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
    pub(crate) fn try_add_high_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
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
    pub(crate) fn extend_with_other(self, other: Self) -> Result<VecType<Self>, Error> {
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
    pub(crate) fn try_add_not_eq_bound(self, value: Spanned<i128>) -> Result<VecType<Self>, Error> {
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
                Spanned::new(Int::Bound(OneRangeIntBound::Low(value.clone() + 1)), span),
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
    pub(crate) fn try_add_slice_bound(self, value: Spanned<Slice>) -> Result<VecType<Self>, Error> {
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

impl Display for OneRangeIntBound {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            OneRangeIntBound::None => write!(f, "Int"),
            OneRangeIntBound::NotEqual(i) => write!(f, "val!={}", i),
            OneRangeIntBound::Low(i) => write!(f, "val>={}", i),
            OneRangeIntBound::High(i) => write!(f, "val<={}", i),
        }
    }
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
