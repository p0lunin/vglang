use crate::error::Error;
use crate::spanned::Spanned;
use crate::types::int::{Int, Slice};
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};

#[derive(Debug, PartialEq, Clone)]
pub struct VecType<T>(pub Vec<T>);

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
    pub fn inner(self) -> Vec<T> {
        self.0
    }
}

impl<T> FromIterator<T> for VecType<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
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
