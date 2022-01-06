use crate::ir::objects::Object;
use crate::syntax::ast::Path;
use std::ops::Deref;

#[derive(Debug, PartialEq, Clone)]
pub struct LocalContext<'a, T: 'a> {
    pub objects: Vec<T>,
    pub parent: Option<&'a LocalContext<'a, T>>,
}

impl<'a, T: 'a> LocalContext<'a, T> {
    pub fn new() -> Self {
        LocalContext {
            objects: vec![],
            parent: None,
        }
    }
}

impl<'a> LocalContext<'a, Object> {
    /*pub fn find_ty(&self, name: &str) -> Option<Rc<Type>> {
        let obj = self.find(name)?;
        match obj {
            Object::EnumDecl(e) => { Some(e.as_ty()) }
            Object::Enum(e) => { Some(e.as_ty()) }
            Object::Type(ty) => {
                Some(ty.def.clone())
            }
            _ => None
        }
    }*/
}

impl<'a, T: HasName + Clone + 'a> Searchable for LocalContext<'a, T> {
    type Item = T;
    fn find(&self, name: &str) -> Option<T> {
        let this = self.objects.iter().find(|t| t.name() == name);
        match (this, self.parent) {
            (Some(t), _) => Some(t.clone()),
            (_, Some(p)) => p.find(name),
            _ => None,
        }
    }
}

pub trait HasName {
    fn name(&self) -> &str;
}

pub trait Searchable {
    type Item;
    fn find(&self, name: &str) -> Option<Self::Item>;
}

pub trait SearchableByPath: Searchable {
    fn find_by_path(&self, path: &Path) -> Option<Self::Item>;
}
/*
impl<T: SearchableByPath> SearchableByPath for T
    where
        T::Item: SearchableByPath<Item = T::Item>
{
    fn find_by_path(&self, path: &Path) -> Option<Self::Item> {
        match path {
            Path::Place(s) => self.find(s.as_str()),
            Path::Path(s, path) => {
                self.find(s.as_str()).and_then(|t| t.find_by_path(path))
            }
        }
    }
}*/

impl<T: Searchable> SearchableByPath for T
where
    T::Item: Searchable<Item = T::Item>,
{
    fn find_by_path(&self, path: &Path) -> Option<Self::Item> {
        match path {
            Path::Place(s) => self.find(s.as_str()),
            Path::Path(s, path) => match path.deref() {
                Path::Path(_, _) => None,
                Path::Place(s2) => self.find(s.as_str()).and_then(|t| t.find(s2)),
            },
        }
    }
}
