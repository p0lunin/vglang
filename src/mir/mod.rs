mod ctx;
mod analysis;

use crate::common::BinOp;
use crate::mir::ctx::{Id, Eid, Fid, Ctx};

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Vid(pub Id); // variable

pub struct Function {
    args: Vec<Variable>,
    out_ty: Vty,
    vars: Vec<Vty>,
    stmts: Vec<Statement>,
}

impl Function {
    pub fn new(args: Vec<Variable>, out_ty: Vty, vars: Vec<Vty>, stmts: Vec<Statement>) -> Self {
        Function { args, out_ty, vars, stmts }
    }
    pub fn empty() -> Self {
        Function { args: vec![], vars: vec![], out_ty: Vty::stack(VtyKind::Discriminant), stmts: vec![] }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    ty: Vty,
    id: Vid,
}

impl Variable {
    pub fn new(ty: Vty, id: Vid) -> Self {
        Variable { ty, id }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Variable(Variable, Assigment),
    Case {
        matched: Vid,
        pattern: Vid,
        stmts: Vec<Statement>,
    },
    Return(Vid),
    Dealloc(Vid),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assigment {
    Function(Fid),
    Discriminant(DiscriminantOf),
    Field {
        r#enum: Eid,
        variant: u8,
        field: u8,
        var: Vid,
    },
    Map(Eid, Vid, Vec<Vid>),
    Call(Fid, Vec<Vid>),
    Alloc(Vid),
    BinOp(Vid, BinOp, Vid),
    Dereference(Vid),
    Value(Value),
    Clone(Vid),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i128),
    Unit,
}

impl Value {
    pub fn ty(&self) -> VtyKind {
        match self {
            Value::Int(_) => VtyKind::Int,
            Value::Unit => VtyKind::Unit,
        }
    }
}

impl Assigment {
    fn get_var_ty(&self, ctx: &Ctx, vars: &[Vty]) -> Vty {
        match self {
            Assigment::Function(f) => {
                let fty = ctx.function(*f);
                Vty::heap(VtyKind::Function {
                    args: fty.args.iter().map(|x| x.ty.clone()).collect(),
                    output: Box::new(fty.out_ty.clone())
                })
            }
            Assigment::Discriminant(_) => {
                Vty::stack(VtyKind::Discriminant)
            }
            Assigment::Field {
                r#enum,
                variant,
                field,
                var: _,
            } => {
                let variant = ctx.enum_variant(*r#enum, *variant);
                variant.fields[*field as usize].clone()
            }
            Assigment::Map(e, _, _) => {
                Vty::stack(VtyKind::Enum(*e))
            }
            Assigment::Call(f, _) => {
                ctx.function(*f).out_ty.clone()
            }
            Assigment::Alloc(v) => {
                debug_assert_eq!(vars[v.0].location, Location::Stack);
                Vty::heap(vars[v.0].kind.clone())
            }
            Assigment::BinOp(_, _, _) => {
                todo!()
            }
            Assigment::Dereference(v) => {
                debug_assert_eq!(vars[v.0].location, Location::Heap);
                Vty::stack(vars[v.0].kind.clone())
            }
            Assigment::Value(v) => Vty::stack(v.ty()),
            Assigment::Clone(v) => {
                debug_assert!(vars[v.0].location == Location::Heap || vars[v.0].location == Location::Rc);
                Vty::heap(vars[v.0].kind.clone())
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DiscriminantOf {
    Variable(Vid),
    UserEnumField(Eid, u8),
}

pub struct UserEnum {
    variants: Vec<UserEnumVariant>
}

impl UserEnum {
    pub fn new(variants: Vec<UserEnumVariant>) -> Self {
        UserEnum { variants }
    }
}

pub struct UserEnumVariant {
    fields: Vec<Vty>,
}

impl UserEnumVariant {
    pub fn new(fields: Vec<Vty>) -> Self {
        UserEnumVariant { fields }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Vty {
    kind: VtyKind,
    location: Location,
}

impl Vty {
    pub fn unit() -> Self {
        Vty { kind: VtyKind::Unit, location: Location::Stack }
    }
    pub fn stack(kind: VtyKind) -> Self {
        Vty { kind, location: Location::Stack }
    }
    pub fn heap(kind: VtyKind) -> Self {
        Vty { kind, location: Location::Heap }
    }
    pub fn rc(kind: VtyKind) -> Self {
        Vty { kind, location: Location::Rc }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum VtyKind {
    Discriminant,
    Int,
    Unit,
    Enum(Eid),
    Function { args: Vec<Vty>, output: Box<Vty> },
}

#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub enum Location {
    Stack,
    Heap,
    Rc,
}

pub struct ProgramBuilder<'a> {
    ctx: &'a Ctx,
    statements: Vec<Statement>,
    vars: Vec<Vty>,
    current_var: Id,
}

impl<'a> ProgramBuilder<'a> {
    pub fn new(ctx: &'a Ctx, args: Vec<Vty>) -> Self {
        let current_var = args.len();
        Self { ctx, statements: vec![], vars: args, current_var }
    }
    fn add(mut self, stmt: Statement) -> Self {
        self.statements.push(stmt);
        self
    }
    pub fn var(mut self, ass: Assigment) -> Self {
        let vid = Vid(self.next_id());
        let ty = ass.get_var_ty(self.ctx, &self.vars);
        self.vars.push(ty.clone());
        self.add(Statement::Variable(Variable::new(ty, vid), ass))
    }
    pub fn return_(self, id: Vid) -> (Vec<Statement>, Vec<Vty>) {
        let program = self.add(Statement::Return(id));
        (program.statements, program.vars)
    }
    pub fn dealloc(self, id: Vid) -> Self {
        self.add(Statement::Dealloc(id))
    }
    pub fn function_ass(self, id: Fid) -> Self {
        self.var(Assigment::Function(id))
    }
    pub fn discriminant_ass(self, d: DiscriminantOf) -> Self {
        self.var(Assigment::Discriminant(d))
    }
    pub fn field_ass(self, d: DiscriminantOf) -> Self {
        self.var(Assigment::Discriminant(d))
    }
    pub fn map_ass(self, eid: Eid, disc: Vid, values: Vec<Vid>) -> Self {
        self.var(Assigment::Map(eid, disc, values))
    }
    pub fn call_ass(self, fid: Fid, values: Vec<Vid>) -> Self {
        self.var(Assigment::Call(fid, values))
    }
    pub fn alloc_ass(self, id: Vid) -> Self {
        self.var(Assigment::Alloc(id))
    }
    pub fn clone_ass(self, id: Vid) -> Self {
        self.var(Assigment::Clone(id))
    }
    pub fn deref_ass(self, id: Vid) -> Self {
        self.var(Assigment::Dereference(id))
    }
    pub fn int_ass(self, value: i128) -> Self {
        self.var(Assigment::Value(Value::Int(value)))
    }
    pub fn unit_ass(self) -> Self {
        self.var(Assigment::Value(Value::Unit))
    }
    fn next_id(&mut self) -> Id {
        let id = self.current_var;
        self.current_var += 1;
        id
    }
}
