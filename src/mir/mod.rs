use crate::common::BinOp;

pub type Id = usize;
pub struct Fid(Id); // function
pub struct Vid(Id); // variable
pub struct Eid(Id); // enum
pub struct Sid(Id); // statement

pub struct Function {
    args: Vec<Variable>,
    out_ty: Vty,
    stmts: Vec<Statement>,
}

pub struct Variable {
    ty: Vty,
    id: Vid,
}

pub enum Statement {
    Variable(Variable, Assigment),
    Case {
        matched: Vid,
        pattern: Vid,
        stmts: Vec<Sid>,
    },
    Return(Vid),
}

pub enum Assigment {
    Function(Fid),
    Discriminant(DiscriminantOf),
    Field {
        r#enum: UserEnum,
        field: u8,
        var: Vid,
    },
    Map(Vec<Vid>),
    Call(Fid, Vid),
    Alloc(Vid),
    BinOp(Vid, BinOp, Vid),
}

pub enum DiscriminantOf {
    Variable(Vid),
    UserEnum(UserEnum),
}

pub struct UserEnum {
    fields: Vec<VtyKind>,
}

pub struct Vty {
    kind: VtyKind,
    location: Location,
}

pub enum VtyKind {
    Discriminant,
    Int,
    Enum(Eid),
    Function { args: Vec<Vty>, output: Box<Vty> },
}

pub enum Location {
    Stack,
    Heap,
}
