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
    pub fn _one(t: T) -> Self {
        Self(vec![t])
    }
    pub fn _inner(self) -> Vec<T> {
        self.0
    }
}

impl<T> FromIterator<T> for VecType<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}
