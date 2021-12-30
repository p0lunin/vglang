use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Deref, DerefMut, Neg, Sub};
use std::borrow::Borrow;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn extend(&self, another: &Span) -> Span {
        Self {
            start: self.start,
            end: another.end,
        }
    }

    #[cfg(test)]
    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }
}

pub struct Spanned<T> {
    pub val: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(val: T, span: Span) -> Self {
        Spanned { val, span }
    }
    pub fn inner(self) -> T {
        self.val
    }
    pub fn map<F: FnOnce(T) -> U, U>(self, f: F) -> Spanned<U> {
        Spanned {
            val: f(self.val),
            span: self.span,
        }
    }
    pub fn as_ref(&self) -> &T {
        &self.val
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.val.fmt(f)
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.val.fmt(f)
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.val.eq(&other.val)
    }
}

impl<T: PartialOrd> PartialOrd for Spanned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.val.partial_cmp(&other.val)
    }
}

impl<T: Eq> Eq for Spanned<T> {}

impl<T: Ord> Ord for Spanned<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.val.cmp(&other.val)
    }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Spanned::new(self.val.clone(), self.span)
    }
}

impl<T> Borrow<T> for Spanned<T> {
    fn borrow(&self) -> &T {
        &self.val
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.val
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.val
    }
}

impl<T: Sub<Output = T>> Sub<T> for Spanned<T> {
    type Output = Self;

    fn sub(self, rhs: T) -> Self::Output {
        Self {
            val: self.val - rhs,
            span: self.span,
        }
    }
}

impl<T: Add<Output = T>> Add<T> for Spanned<T> {
    type Output = Self;

    fn add(self, rhs: T) -> Self::Output {
        Self {
            val: self.val + rhs,
            span: self.span,
        }
    }
}

impl<T: Neg> Neg for Spanned<T> {
    type Output = Spanned<T::Output>;

    fn neg(self) -> Self::Output {
        Spanned {
            val: -self.val,
            span: self.span,
        }
    }
}

pub trait AddSpan: Sized {
    fn add_span(self, span: Span) -> Spanned<Self> {
        Spanned::new(self, span)
    }
}

impl<T> AddSpan for T {}
