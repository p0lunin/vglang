#[derive(Debug, PartialEq, Clone)]
pub struct Context<'a, T: 'a> {
    pub objects: Vec<T>,
    pub parent: Option<&'a Context<'a, T>>,
}

impl<'a, T: HasName + 'a> Context<'a, T> {
    pub fn find(&'a self, name: &str) -> Option<&'a T> {
        let this = self.objects.iter().find(|t| t.name() == name);
        match (this, self.parent) {
            (Some(t), _) => Some(t),
            (_, Some(p)) => p.find(name),
            _ => None,
        }
    }
}

pub trait HasName {
    fn name(&self) -> &str;
}
