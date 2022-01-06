use crate::syntax::ast::Path;
use smallvec::SmallVec;

pub trait Find {
    type Item;
    fn find(&self, path: PathToFind) -> Option<Self::Item>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct PathToFind<'a> {
    pub endpoint: &'a str,
    pub segments: SmallVec<[&'a str; 4]>,
}

impl<'a> PathToFind<'a> {
    pub fn new(endpoint: &'a str, segments: SmallVec<[&'a str; 4]>) -> Self {
        PathToFind { endpoint, segments }
    }
    pub fn name(endpoint: &'a str) -> Self {
        Self::new(endpoint, SmallVec::new())
    }
    pub fn has_segments(&self) -> bool {
        !self.segments.is_empty()
    }
}

impl PathToFind<'_> {
    pub fn from_path(path: &Path) -> PathToFind {
        let mut endpoint = "";
        let mut segments = SmallVec::new();
        from_path(path, &mut endpoint, &mut segments);
        PathToFind::new(endpoint, segments)
    }
}

fn from_path<'a>(path: &'a Path, endpoint: &mut &'a str, segments: &mut SmallVec<[&'a str; 4]>) {
    match path {
        Path::Place(s) => *endpoint = s.as_str(),
        Path::Path(s, p) => {
            from_path(p, endpoint, segments);
            segments.push(s.as_str())
        }
    }
}
