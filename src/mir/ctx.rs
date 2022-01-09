use crate::mir::{Function, UserEnum, UserEnumVariant, Vty};

pub type Id = usize;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Fid(pub Id); // function
impl Fid {
    pub fn inner(self) -> Id {
        self.0
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Eid(pub Id); // enum

pub struct Ctx {
    pub functions: Vec<Function>,
    pub enums: Vec<UserEnum>,
    pub main: Function,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            functions: vec![],
            enums: vec![],
            main: Function::empty(),
        }
    }
}

impl Ctx {
    pub fn enum_(&self, id: Eid) -> &UserEnum {
        &self.enums[id.0]
    }
    pub fn enum_variant(&self, id: Eid, variant: u8) -> &UserEnumVariant {
        &self.enum_(id).variants[variant as usize]
    }
    pub fn function(&self, id: Fid) -> &Function {
        &self.functions[id.0]
    }
}
