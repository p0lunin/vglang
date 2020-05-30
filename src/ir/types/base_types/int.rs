use crate::common::{AddSpan, Spanned, VecType};
use crate::ir::types::{Type, TypeKind, TypeOperable};
use itertools::Itertools;
use std::cell::RefCell;
use std::cmp::{max, min};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Int {
    Value(i128),
    Bound(OneRangeIntBound),
    KnownBound { low: i128, high: i128 },
    Slice(Slice),
}

impl Spanned<i128> {
    pub fn get_type(&self) -> Rc<RefCell<Type>> {
        Rc::new(RefCell::new(Type::Int(TypeKind::from_kinds(Spanned::new(
            VecType::one(Int::Value(self.val)),
            self.span,
        )))))
    }
}

impl Int {
    pub fn is_part_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Int::Slice(l), r) => match r {
                Int::Value(v) => (*v - l.to) % l.step == 0,
                Int::Slice(r) => l.from >= r.from && l.to <= r.to && r.step % l.step == 0,
                _ => false,
            },
            (_, Int::Slice(_)) => unimplemented!(),
            _ => {
                let (minx, maxx) = self.min_and_max();
                let (miny, maxy) = other.min_and_max();
                minx >= miny && maxx <= maxy
            }
        }
    }
    pub fn min_and_max(&self) -> (i128, i128) {
        match self {
            Int::Value(v) => (*v, *v),
            Int::Bound(b) => match b {
                OneRangeIntBound::High(h) => (i128::min_value(), *h),
                OneRangeIntBound::Low(l) => (*l, i128::max_value()),
                _ => (i128::min_value(), i128::max_value()),
            },
            Int::KnownBound { low, high } => (*low, *high),
            Int::Slice(s) => (s.from, s.to),
        }
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Int::Value(i) => i.fmt(f),
            Int::Bound(i) => i.fmt(f),
            Int::KnownBound { low, high } => {
                f.write_str(&format!("(val>={} & val<={})", low, high))
            }
            Int::Slice(s) => f.write_str(&format!(
                "val={{from: {}; step: {}; to: {};}}",
                s.from, s.step, s.to
            )),
        }
    }
}

impl TypeOperable<Int> for TypeKind<Int> {
    fn add(self, right: Type) -> Result<Self, String> {
        let TypeKind { name, kinds } = self;
        let Spanned { val, span } = kinds;
        let kinds = val
            .inner()
            .into_iter()
            .map(|t| t.add(right.clone()))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<VecType<_>>()
            .add_span(span);
        Ok(TypeKind { name, kinds })
    }

    fn neg(self) -> Result<Self, String> {
        let TypeKind { name, kinds } = self;
        let Spanned { val, span } = kinds;
        let kinds = val
            .inner()
            .into_iter()
            .map(|t| t.neg())
            .collect::<Result<VecType<_>, _>>()?
            .add_span(span);
        Ok(TypeKind { name, kinds })
    }

    fn and(self, right: Type) -> Result<Self, String> {
        let ty = match right {
            Type::Int(i) => i,
            _ => {
                return Err(format!(
                    "Operator & not implement for types {} and {}",
                    self, right
                ))
            }
        };
        let TypeKind { name, kinds } = self;
        let Spanned { val, span } = kinds;
        let kinds = val
            .into_iter()
            .cartesian_product(ty.kinds.inner().into_iter())
            .map(|(t, i)| t.extend_with_other(i.clone()))
            .collect::<Result<Vec<_>, String>>()?
            .into_iter()
            .flatten()
            .collect::<VecType<_>>()
            .add_span(span);
        Ok(TypeKind { name, kinds })
    }

    fn or(self, right: Type) -> Result<Self, String> {
        let TypeKind { name, kinds } = self;
        let new_span = kinds.span.extend(&right.span());
        match right {
            Type::Int(i) => Ok(TypeKind {
                name,
                kinds: kinds.inner().with(i.kinds.inner()).add_span(new_span),
            }),
            _ => Err(format!(
                "Operator | not implement for types {} and {}",
                TypeKind { name, kinds },
                right
            )),
        }
    }

    fn mul(self, right: Type) -> Result<Self, String> {
        let TypeKind { name, kinds } = self;
        let Spanned { val, span } = kinds;
        let kinds = val
            .inner()
            .into_iter()
            .map(|t| t.mul(right.clone()))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<VecType<_>>()
            .add_span(span);
        Ok(TypeKind { name, kinds })
    }

    fn sub(self, right: Type) -> Result<Self, String> {
        let TypeKind { name, kinds } = self;
        let Spanned { val, span } = kinds;
        let kinds = val
            .inner()
            .into_iter()
            .map(|t| t.sub(right.clone()))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<VecType<_>>()
            .add_span(span);
        Ok(TypeKind { name, kinds })
    }

    fn div(self, right: Type) -> Result<Self, String> {
        let TypeKind { name, kinds } = self;
        let Spanned { val, span } = kinds;
        let kinds = val
            .inner()
            .into_iter()
            .map(|t| t.div(right.clone()))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<VecType<_>>()
            .add_span(span);
        Ok(TypeKind { name, kinds })
    }

    fn pow(self, right: Type) -> Result<Self, String> {
        let TypeKind { name, kinds } = self;
        let Spanned { val, span } = kinds;
        let kinds = val
            .inner()
            .into_iter()
            .map(|t| t.pow(right.clone()))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<VecType<_>>()
            .add_span(span);
        Ok(TypeKind { name, kinds })
    }
}

impl Int {
    pub(crate) fn add(self, val: Type) -> Result<VecType<Self>, String> {
        let val = match val {
            Type::Int(i) => i,
            _ => return Err(format!("Cannot use + for `{}` and `{}` types", self, val)),
        };
        val.kinds
            .inner()
            .into_iter()
            .map(|val| {
                let val = match val {
                    Int::Value(v) => v,
                    _ => return Err(format!("Cannot use + for `{}` and `{}` types", self, val)),
                };
                Ok(match self.clone() {
                    Int::Value(v) => Int::Value(v + val),
                    Int::Bound(b) => Int::Bound(b.add(val)?),
                    Int::KnownBound { low, high } => Int::KnownBound {
                        low: low + val,
                        high: high + val,
                    },
                    Int::Slice(s) => Int::Slice(Slice {
                        from: s.from + val,
                        step: s.step,
                        to: s.to + val,
                    }),
                })
            })
            .collect()
    }
    pub(crate) fn mul(self, val: Type) -> Result<VecType<Self>, String> {
        let val = match val {
            Type::Int(i) => i,
            _ => return Err(format!("Cannot use * for `{}` and `{}` types", self, val)),
        };
        val.kinds
            .inner()
            .into_iter()
            .map(|val| {
                let val = match val {
                    Int::Value(v) => v,
                    _ => return Err(format!("Cannot use * for `{}` and `{}` types", self, val)),
                };
                Ok(match self.clone() {
                    Int::Value(v) => Int::Value(v * val),
                    Int::Bound(b) => Int::Bound(b.mul(val)?),
                    Int::KnownBound { low, high } => Int::KnownBound {
                        low: low * val,
                        high: high * val,
                    },
                    Int::Slice(s) => Int::Slice(Slice {
                        from: s.from * val,
                        step: s.step * val,
                        to: s.to * val,
                    }),
                })
            })
            .collect()
    }
    pub(crate) fn sub(self, val: Type) -> Result<VecType<Self>, String> {
        let val = match val {
            Type::Int(i) => i,
            _ => return Err(format!("Cannot use - for `{}` and `{}` types", self, val)),
        };
        val.kinds
            .inner()
            .into_iter()
            .map(|val| {
                let val = match val {
                    Int::Value(v) => v,
                    _ => return Err(format!("Cannot use - for `{}` and `{}` types", self, val)),
                };
                Ok(match self.clone() {
                    Int::Value(v) => Int::Value(v - val),
                    Int::Bound(b) => Int::Bound(b.add(-val)?),
                    Int::KnownBound { low, high } => Int::KnownBound {
                        low: low - val,
                        high: high - val,
                    },
                    Int::Slice(s) => Int::Slice(Slice {
                        from: s.from - val,
                        step: s.step,
                        to: s.to - val,
                    }),
                })
            })
            .collect()
    }
    pub(crate) fn div(self, val: Type) -> Result<VecType<Self>, String> {
        let val = match val {
            Type::Int(i) => i,
            _ => return Err(format!("Cannot use / for `{}` and `{}` types", self, val)),
        };
        val.kinds
            .inner()
            .into_iter()
            .map(|val| {
                let val = match val {
                    Int::Value(v) => v,
                    _ => return Err(format!("Cannot use / for `{}` and `{}` types", self, val)),
                };
                Ok(match self.clone() {
                    Int::Value(v) => Int::Value(v + val),
                    Int::Bound(b) => Int::Bound(b.div(val)?),
                    Int::KnownBound { low, high } => Int::KnownBound {
                        low: low / val,
                        high: high / val,
                    },
                    Int::Slice(s) => Int::Slice(Slice {
                        from: s.from / val,
                        step: s.step,
                        to: s.to / val,
                    }),
                })
            })
            .collect()
    }
    pub(crate) fn pow(self, val: Type) -> Result<VecType<Self>, String> {
        let val = match val {
            Type::Int(i) => i,
            _ => return Err(format!("Cannot use ^ for `{}` and `{}` types", self, val)),
        };
        val.kinds
            .inner()
            .into_iter()
            .map(|val| {
                let val = match val {
                    Int::Value(v) => v,
                    _ => return Err(format!("Cannot use ^ for `{}` and `{}` types", self, val)),
                };
                Ok(match self.clone() {
                    Int::Value(v) => Int::Value(v.pow(val as u32)),
                    Int::Bound(b) => Int::Bound(b.pow(val)?),
                    Int::KnownBound { low, high } => Int::KnownBound {
                        low: low.pow(val as u32),
                        high: high.pow(val as u32),
                    },
                    Int::Slice(s) => Int::Slice(Slice {
                        from: s.from.pow(val as u32),
                        step: s.step.pow(val as u32),
                        to: s.to.pow(val as u32),
                    }),
                })
            })
            .collect()
    }
    pub fn neg(self) -> Result<Self, String> {
        match self {
            Int::Value(v) => Ok(Int::Value(-v)),
            Int::Bound(bound) => match bound {
                OneRangeIntBound::High(h) => Ok(Int::Bound(OneRangeIntBound::High(-h))),
                OneRangeIntBound::Low(l) => Ok(Int::Bound(OneRangeIntBound::Low(-l))),
                OneRangeIntBound::NotEqual(n) => Ok(Int::Bound(OneRangeIntBound::NotEqual(-n))),
                OneRangeIntBound::None => Err(format!("Cannot apply - for `Unknown` type")),
            },
            Int::KnownBound { low, high } => Ok(Int::KnownBound {
                low: -low,
                high: -high,
            }),
            Int::Slice(s) => Ok(Int::Slice(Slice {
                to: -s.to,
                step: s.step,
                from: -s.from,
            })),
        }
    }
}

impl Int {
    pub(crate) fn try_convert_to_value(self, value: i128) -> Result<VecType<Self>, String> {
        match self {
            Int::Value(i) if i == value => Ok(VecType::one(self)),
            Int::Bound(OneRangeIntBound::None) => Ok(VecType::one(Int::Value(value))),
            Int::Bound(OneRangeIntBound::Low(low)) if low <= value => {
                Ok(VecType::one(Int::Value(value)))
            }
            Int::Bound(OneRangeIntBound::High(high)) if high >= value => {
                Ok(VecType::one(Int::Value(value)))
            }
            Int::Bound(OneRangeIntBound::NotEqual(i)) => {
                Int::Bound(OneRangeIntBound::High(value)).try_add_not_eq_bound(i.clone())
            }
            Int::KnownBound { low, high } if low <= value && high >= value => {
                Ok(VecType::one(Int::Value(value)))
            }
            Int::Slice(s) if s.contain(value) => Ok(VecType::one(Int::Value(value))),
            _ => Err(format!("Void type!")),
        }
    }
    pub(crate) fn try_add_low_bound(self, value: i128) -> Result<VecType<Self>, String> {
        match self {
            Int::Value(i) if i >= value => Ok(VecType::one(Int::Value(i))),
            Int::Bound(OneRangeIntBound::None) => {
                Ok(VecType::one(Int::Bound(OneRangeIntBound::Low(value))))
            }
            Int::Bound(OneRangeIntBound::Low(low)) => Ok(VecType::one(Int::Bound(
                OneRangeIntBound::Low(max(low, value)),
            ))),
            Int::Bound(OneRangeIntBound::High(high)) if high >= value => {
                Ok(VecType::one(Int::KnownBound { low: value, high }))
            }
            Int::Bound(OneRangeIntBound::NotEqual(i)) => {
                Int::Bound(OneRangeIntBound::Low(value)).try_add_not_eq_bound(i)
            }
            Int::KnownBound { low, high } if value <= high => Ok(VecType::one(Int::KnownBound {
                low: max(value, low),
                high,
            })),
            Int::Slice(s) => Ok(VecType::one(Int::Slice(s.try_change_low_bound(value)?))),
            _ => Err(format!("Void type!")),
        }
    }
    pub(crate) fn try_add_high_bound(self, value: i128) -> Result<VecType<Self>, String> {
        match self {
            Int::Value(i) if i <= value => Ok(VecType::one(Int::Value(i))),
            Int::Bound(OneRangeIntBound::None) => {
                Ok(VecType::one(Int::Bound(OneRangeIntBound::High(value))))
            }
            Int::Bound(OneRangeIntBound::High(high)) => Ok(VecType::one(Int::Bound(
                OneRangeIntBound::High(min(high, value)),
            ))),
            Int::Bound(OneRangeIntBound::Low(low)) if low <= value => {
                Ok(VecType::one(Int::KnownBound { low, high: value }))
            }
            Int::Bound(OneRangeIntBound::NotEqual(i)) => {
                Int::Bound(OneRangeIntBound::High(value)).try_add_not_eq_bound(i)
            }
            Int::KnownBound { low, high } if low <= value => Ok(VecType::one(Int::KnownBound {
                low,
                high: min(value, high),
            })),
            Int::Slice(s) => Ok(VecType::one(Int::Slice(s.try_change_high_bound(value)?))),
            _ => Err(format!("Void type!")),
        }
    }
    pub(crate) fn extend_with_other(self, other: Self) -> Result<VecType<Self>, String> {
        match other {
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
    pub(crate) fn try_add_not_eq_bound(self, value: i128) -> Result<VecType<Self>, String> {
        match self {
            Int::Value(i) if i != value => Ok(VecType::one(Int::Value(i))),
            Int::Bound(OneRangeIntBound::None) => {
                Ok(VecType::one(Int::Bound(OneRangeIntBound::NotEqual(value))))
            }
            Int::Bound(OneRangeIntBound::High(high)) if high - 1 > value => Ok(VecType(vec![
                Int::KnownBound {
                    low: high,
                    high: value - 1,
                },
                Int::Bound(OneRangeIntBound::High(value + 1)),
            ])),
            Int::Bound(OneRangeIntBound::High(high)) if high - 1 == value => Ok(VecType(vec![
                Int::Value(high),
                Int::Bound(OneRangeIntBound::High(value + 1)),
            ])),
            Int::Bound(OneRangeIntBound::High(high)) if high == value => {
                Ok(VecType::one(Int::Bound(OneRangeIntBound::High(high + 1))))
            }
            Int::Bound(OneRangeIntBound::High(high)) => {
                Ok(VecType::one(Int::Bound(OneRangeIntBound::High(high))))
            }
            Int::Bound(OneRangeIntBound::Low(low)) if low + 1 < value => Ok(VecType(vec![
                Int::KnownBound {
                    low,
                    high: value - 1,
                },
                Int::Bound(OneRangeIntBound::Low(value + 1)),
            ])),
            Int::Bound(OneRangeIntBound::Low(low)) if low + 1 == value => Ok(VecType(vec![
                Int::Value(low),
                Int::Bound(OneRangeIntBound::Low(value + 1)),
            ])),
            Int::Bound(OneRangeIntBound::Low(low)) if low == value => {
                Ok(VecType::one(Int::Bound(OneRangeIntBound::Low(low + 1))))
            }
            Int::Bound(OneRangeIntBound::Low(low)) => {
                Ok(VecType::one(Int::Bound(OneRangeIntBound::Low(low))))
            }
            Int::KnownBound { low, high } if low + 1 < value && value < high - 1 => {
                Ok(VecType(vec![
                    Int::KnownBound {
                        low,
                        high: value - 1,
                    },
                    Int::KnownBound {
                        low: value + 1,
                        high,
                    },
                ]))
            }
            Int::KnownBound { low, high } if low + 1 == value => Ok(VecType(vec![
                Int::Value(low),
                Int::KnownBound {
                    low: value + 1,
                    high,
                },
            ])),
            Int::KnownBound { low, high } if low == value => Ok(VecType::one(Int::KnownBound {
                low: value + 1,
                high,
            })),
            Int::KnownBound { low, high } if high - 1 == value => Ok(VecType(vec![
                Int::Value(high),
                Int::KnownBound {
                    low,
                    high: value - 1,
                },
            ])),
            Int::KnownBound { low, high } if high == value => Ok(VecType::one(Int::KnownBound {
                low,
                high: value - 1,
            })),
            Int::KnownBound { low, high } => Ok(VecType::one(Int::KnownBound { low, high })),
            Int::Slice(s) => {
                let res = s.try_split(value)?;
                Ok(res.into_iter().map(Int::Slice).collect())
            }
            _ => Err(format!("Void type!")),
        }
    }
    pub(crate) fn try_add_slice_bound(self, value: Slice) -> Result<VecType<Self>, String> {
        match self {
            Int::Value(i) if value.contain(i) => Ok(VecType::one(Int::Value(i))),
            Int::Bound(OneRangeIntBound::None) => Ok(VecType::one(Int::Slice(value))),
            Int::Bound(OneRangeIntBound::Low(low)) => {
                Ok(VecType::one(Int::Slice(value.try_change_low_bound(low)?)))
            }
            Int::Bound(OneRangeIntBound::High(high)) => {
                Ok(VecType::one(Int::Slice(value.try_change_high_bound(high)?)))
            }
            Int::Bound(OneRangeIntBound::NotEqual(i)) => Int::Slice(value).try_add_not_eq_bound(i),
            Int::KnownBound { low, high } => {
                let slice = value
                    .try_change_low_bound(low)?
                    .try_change_high_bound(high)?;
                Ok(VecType::one(Int::Slice(slice)))
            }
            Int::Slice(_) => unimplemented!(),
            _ => Err(format!("Void type!")),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum OneRangeIntBound {
    None,
    NotEqual(i128),
    Low(i128),
    High(i128),
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
    fn map<F: Fn(i128) -> i128>(self, label: &str, f: F) -> Result<Self, String> {
        match self {
            OneRangeIntBound::None => Err(format!("try {} to empty bound", label)),
            OneRangeIntBound::Low(l) => Ok(OneRangeIntBound::Low(f(l))),
            OneRangeIntBound::NotEqual(l) => Ok(OneRangeIntBound::NotEqual(f(l))),
            OneRangeIntBound::High(h) => Ok(OneRangeIntBound::High(f(h))),
        }
    }

    pub fn add(self, val: i128) -> Result<Self, String> {
        self.map("add", |v| v + val)
    }
    pub fn mul(self, val: i128) -> Result<Self, String> {
        self.map("mul", |v| v * val)
    }
    pub fn div(self, val: i128) -> Result<Self, String> {
        self.map("mul", |v| v / val)
    }
    pub fn pow(self, val: i128) -> Result<Self, String> {
        self.map("mul", |v| v.pow(val as u32))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Slice {
    pub from: i128,
    pub step: i128,
    pub to: i128,
}

impl Slice {
    pub fn contain(&self, v: i128) -> bool {
        v >= self.from && v <= self.to && (v - self.from) % self.step == 0
    }
    pub fn try_change_low_bound(self, v: i128) -> Result<Self, String> {
        match v > self.to {
            true => Err(format!("Void type!")),
            false => Ok(Slice { from: v, ..self }),
        }
    }
    pub fn try_change_high_bound(self, v: i128) -> Result<Self, String> {
        match v < self.from {
            true => Err(format!("Void type!")),
            false => Ok(Slice { to: v, ..self }),
        }
    }
    pub fn try_split(self, v: i128) -> Result<Vec<Self>, String> {
        match self.contain(v) {
            true => unimplemented!(),
            false => unimplemented!(),
        }
    }
}

impl VecType<Int> {
    pub fn try_convert_to_value(self, value: i128) -> Result<Self, String> {
        self.0
            .into_iter()
            .map(|i| i.try_convert_to_value(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|res| res.into_iter().next().unwrap())
    }
    pub fn try_add_low_bound(self, value: i128) -> Result<Self, String> {
        self.0
            .into_iter()
            .map(|i| i.try_add_low_bound(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|r| r.into_iter().flatten().collect::<Self>())
    }
    pub fn try_add_high_bound(self, value: i128) -> Result<Self, String> {
        self.0
            .into_iter()
            .map(|i| i.try_add_high_bound(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|r| r.into_iter().flatten().collect::<Self>())
    }
    pub fn try_add_not_eq_bound(self, value: i128) -> Result<Self, String> {
        self.0
            .into_iter()
            .map(|i| i.try_add_not_eq_bound(value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(|r| r.into_iter().flatten().collect::<Self>())
    }
    pub fn try_add_slice_bound(self, value: Slice) -> Result<Self, String> {
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