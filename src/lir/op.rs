pub(super) type Id = usize;
pub(super) type Bytes = usize;

#[derive(Debug, Clone)]
pub(super) struct OpId(pub Id);

pub(super) struct Scope {
    pub vars: Vec<Var>,
}

impl Scope {
    pub fn new(vars: Vec<Var>) -> Self {
        Scope { vars }
    }
}

#[derive(Debug, Clone)]
pub(super) struct Var {
    pub size: Bytes,
    pub location: VarTy,
}

impl Var {
    pub fn new(size: usize) -> Self {
        Var { size, location: VarTy::Stack }
    }
}

pub(super) enum VarTy {
    Stack,
    Heap
}

#[derive(Debug, Clone)]
pub(super) enum Op {
    Copy {
        source: Id,
        destination: Id,
    },
    Alloc {
        size: Bytes,
        destination: Id,
    },
    Dealloc {
        address: usize,
    },
    Write {
        value: Box<[u8]>,
        destination: Id,
    },
    CaseArm {
        pattern: Id,
        matched: Id,
        jump: OpId,
    },
    Exit,
}

pub(super) struct Program {
    pub scope: ProgramScope,
    pub ops: Vec<Op>,
}

impl Program {
    pub fn new(scope: ProgramScope, ops: Vec<Op>) -> Self {
        Program { scope, ops }
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! program {
    (@inner cp $source:literal $dest:literal) => {{
        Op::Copy {
            source: $source,
            destination: $dest,
        }
    }};
    (@inner wr $value:expr => $dest:literal) => {{
        Op::Write {
            value: $value.into(),
            destination: $dest,
        }
    }};
    (@inner cs $pat:literal $matched:literal $jump:literal) => {{
        Op::CaseArm {
            pattern: $pat,
            matched: $matched,
            jump: crate::lir::op::OpId($jump),
        }
    }};
    (@inner ex) => {{
        Op::Exit
    }};
    ($( $op:ident ($($t:tt)*) )*) => {
        vec![$(
            program! { @inner $op $($t)* },
        )*]
    };
}

pub(super) struct ProgramScope {
    pub vars: Vec<(Bytes, Var)>,
    pub total_size: Bytes,
}

impl ProgramScope {
    pub fn new(vars: Vec<Var>) -> Self {
        let mut varss = vec![];
        let total_size = vars.into_iter().fold(0, |mut acc, var| {
            let size = var.size;
            varss.push((acc, var));
            acc + size
        });
        ProgramScope { vars: varss, total_size }
    }
}