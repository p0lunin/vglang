use std::rc::Rc;
use crate::ir::types::Type;
use std::fmt::{Display, Formatter};
use itertools::Itertools;

#[derive(Debug, PartialEq, Clone)]
pub struct Concrete<T> {
    pub base: Rc<T>,
    pub generics: Vec<Rc<Type>>,
}

impl<T> Concrete<T> {
    pub fn new(base: Rc<T>, generics: Vec<Rc<Type>>) -> Self {
        Concrete { base, generics }
    }
    pub fn base(base: Rc<T>) -> Self {
        Concrete { base, generics: vec![] }
    }
}

impl<T: Display> Display for Concrete<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.base, f)?;
        f.write_fmt(format_args!("({})", self.generics.iter().map(|x| x.to_string()).join(", ")))
    }
}