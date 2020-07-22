use crate::ir::types::{Type, TypeOperable};
use itertools::Itertools;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Sub};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Int {
    Infinite,
    Value(i128),
    LowBound(i128),
    HighBound(i128),
    Slice(Slice),
    Or(Box<Int>, Box<Int>),
    And(Box<Int>, Box<Int>),
}

pub fn get_type_i128(this: i128) -> Rc<Type> {
    Rc::new(Type::Int(Int::Value(this)))
}

impl Int {
    pub fn is_part_of(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Int::Infinite) => true,
            (Int::Value(i1), Int::Value(i2)) => i1 == i2,
            (Int::Value(i), Int::LowBound(l)) => i >= l,
            (Int::Value(i), Int::HighBound(h)) => i <= h,
            (r, Int::Slice(l)) => match r {
                Int::Value(v) => (*v - l.to) % l.step == 0,
                Int::Slice(r) => l.from >= r.from && l.to <= r.to && r.step % l.step == 0,
                _ => false,
            },
            (Int::Slice(_), _) => unimplemented!(),
            (Int::LowBound(l), Int::LowBound(r)) => l >= r,
            (Int::HighBound(l), Int::HighBound(r)) => l <= r,
            (Int::Or(i1, i2), r) => i1.is_part_of(r) && i2.is_part_of(r),
            (i, Int::Or(l, r)) => i.is_part_of(l) || i.is_part_of(r),
            (Int::And(i1, i2), r) => i1.is_part_of(r) && i2.is_part_of(r),
            (i, Int::And(l, r)) => i.is_part_of(l) && i.is_part_of(r),
            _ => false,
        }
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Int::Value(i) => i.fmt(f),
            Int::Slice(s) => write!(
                f,
                "val={{from: {}; step: {}; to: {};}}",
                s.from, s.step, s.to
            ),
            Int::Or(l, r) => write!(f, "({}) | ({})", l, r),
            Int::Infinite => f.write_str("Int"),
            Int::LowBound(l) => write!(f, ">={}", l),
            Int::HighBound(h) => write!(f, "<={}", h),
            Int::And(l, r) => write!(f, "{} & {}", l, r),
        }
    }
}

impl TypeOperable for Int {
    fn add(&self, right: &Type) -> Result<Self, String> {
        match right {
            Type::Int(i) => self.add_int(&i),
            _ => Err(format!("Cannot add {} to {}", self, right)),
        }
    }

    fn neg(&self) -> Result<Self, String> {
        self.neg_int()
    }

    fn and(&self, right: &Type) -> Result<Self, String> {
        match right {
            Type::Int(i) => Ok(Int::And(Box::new(self.clone()), Box::new(i.clone()))),
            _ => Err(format!("Cannot and {} to {}", self, right)),
        }
    }

    fn or(&self, right: &Type) -> Result<Self, String> {
        match right {
            Type::Int(i) => Ok(Int::Or(Box::new(self.clone()), Box::new(i.clone()))),
            _ => Err(format!("Cannot or {} to {}", self, right)),
        }
    }

    fn mul(&self, right: &Type) -> Result<Self, String> {
        match right {
            Type::Int(i) => self.mul_int(&i),
            _ => Err(format!("Cannot mul {} to {}", self, right)),
        }
    }

    fn sub(&self, right: &Type) -> Result<Self, String> {
        match right {
            Type::Int(i) => self.sub_int(&i),
            _ => Err(format!("Cannot sub {} to {}", self, right)),
        }
    }

    fn div(&self, right: &Type) -> Result<Self, String> {
        match right {
            Type::Int(i) => self.div_int(&i),
            _ => Err(format!("Cannot div {} to {}", self, right)),
        }
    }

    fn pow(&self, right: &Type) -> Result<Self, String> {
        match right {
            Type::Int(i) => self.pow_int(&i),
            _ => Err(format!("Cannot pow {} to {}", self, right)),
        }
    }
}

fn int_op<FD, FS>(left: &Int, right: &Int, default: FD, for_step: FS) -> Result<Int, String>
where
    FD: Fn(i128, i128) -> i128 + Clone,
    FS: Fn(i128, i128) -> i128 + Clone,
{
    let val = *match right {
        Int::Value(v) => v,
        _ => return Err(format!("Cannot use + for `{}` and `{}` types", left, right)),
    };
    Ok(match left {
        Int::Value(v) => Int::Value(default(*v, val)),
        Int::Slice(s) => Int::Slice(Slice {
            from: default(s.from, val),
            step: for_step(s.step, val),
            to: default(s.to, val),
        }),
        Int::Or(l, r) => Int::Or(
            Box::new(int_op(l, right, default.clone(), for_step.clone())?),
            Box::new(int_op(r, right, default, for_step)?),
        ),
        Int::Infinite => Int::Infinite,
        Int::LowBound(l) => Int::LowBound(default(*l, val)),
        Int::HighBound(l) => Int::LowBound(default(*l, val)),
        Int::And(l, r) => Int::Or(
            Box::new(int_op(l, right, default.clone(), for_step.clone())?),
            Box::new(int_op(r, right, default, for_step)?),
        ),
    })
}

impl Int {
    pub(crate) fn add_int(&self, int: &Int) -> Result<Self, String> {
        int_op(self, int, Add::add, |x, y| x)
    }
    pub(crate) fn mul_int(&self, int: &Int) -> Result<Self, String> {
        int_op(self, int, Add::add, |x, y| x * y)
    }
    pub(crate) fn sub_int(&self, int: &Int) -> Result<Self, String> {
        int_op(self, int, Sub::sub, |x, y| x)
    }
    pub(crate) fn div_int(&self, int: &Int) -> Result<Self, String> {
        int_op(self, int, Div::div, |x, y| x / y)
    }
    pub(crate) fn pow_int(&self, int: &Int) -> Result<Self, String> {
        int_op(self, int, |x, y| x.pow(y as u32), |x, y| x.pow(y as u32))
    }
    pub fn neg_int(&self) -> Result<Self, String> {
        Ok(match self {
            Int::Infinite => Self::Infinite,
            Int::Value(v) => Int::Value(-v),
            Int::LowBound(v) => Int::LowBound(-v),
            Int::HighBound(v) => Int::HighBound(-v),
            Int::Slice(s) => unimplemented!(),
            Int::Or(l, r) => Int::Or(Box::new(l.neg()?), Box::new(r.neg()?)),
            Int::And(l, r) => Int::And(Box::new(l.neg()?), Box::new(r.neg()?)),
        })
    }
}
/*
impl Int {
    pub(crate) fn try_convert_to_value(self, value: i128) -> Result<Self, String> {
        match self {
            Int::Value(i) if i == value => Ok(Int::Value(i)),
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
            //_ => Err(format!("Void type!")),
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
*/

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
