use crate::common::{Spanned, VecType, Span, AddSpan};
use std::fmt::{Debug, Display, Formatter};

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