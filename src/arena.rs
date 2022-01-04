use std::marker::PhantomData;
use std::fmt::{Debug, Formatter};
use crate::common::{Find, PathToFind};
use std::iter::FromIterator;

#[derive(Debug, PartialEq, Clone)]
pub struct Arena<T> {
    items: Vec<T>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Arena {
            items: vec![],
        }
    }
    pub fn alloc(&mut self, object: T) -> Id<T> {
        let id = self.next_id();
        self.items.push(object);
        id
    }
    #[inline(always)]
    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.items.get(id.0)
    }
    #[inline(always)]
    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.items.get_mut(id.0)
    }
    #[inline(always)]
    pub unsafe fn get_unchecked(&self, id: Id<T>) -> &T {
        self.items.get_unchecked(id.0)
    }
    #[inline(always)]
    pub fn next_id(&self) -> Id<T> {
        Id::new(self.items.len())
    }
    #[inline(always)]
    pub fn as_slice(&self) -> &[T] {
        self.items.as_slice()
    }
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.items.len()
    }
    pub fn remove_lasts(&mut self, remove_count: usize) {
        self.items.drain(self.items.len()-remove_count..).for_each(|_| ());
    }
    #[inline(always)]
    pub fn pop(&mut self) {
        self.items.pop().unwrap();
    }
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.items.iter()
    }
    pub fn iter_with_ids(&self) -> impl DoubleEndedIterator<Item = (Id<T>, &T)> {
        self.items.iter().enumerate().map(|(id, x)| (Id::new(id), x))
    }
    pub fn extend_by(&mut self, other: Arena<T>)  {
        self.items.extend(other.items)
    }
}

impl<T, Item> Find for Arena<T>
where
    for<'a> (Id<T>, &'a T): Find<Item = Item>,
{
    type Item = Item;

    fn find(&self, path: PathToFind) -> Option<Self::Item> {
        self.iter_with_ids().find_map(|x| x.find(path.clone()))
    }
}

impl<A> FromIterator<A> for Arena<A> {
    fn from_iter<T: IntoIterator<Item=A>>(iter: T) -> Self {
        Self {
            items: iter.into_iter().collect()
        }
    }
}

#[repr(transparent)]
pub struct Id<T>(usize, PhantomData<T>);

impl<T> Id<T> {
    fn new(field0: usize) -> Self {
        Id(field0, PhantomData)
    }
}

// T may not implement these, so we cannot use derives
impl<T> Debug for Id<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id(self.0.clone(), PhantomData)
    }
}
impl<T> Copy for Id<T> { }

impl<T> Id<T> {
    #[inline(always)]
    pub fn into_type<U>(self) -> Id<U> {
        unsafe { std::mem::transmute(self) }
    }
}
