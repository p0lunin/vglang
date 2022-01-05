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

pub(super)struct Var {
    pub size: Bytes,
    pub location: Location,
}

impl Var {
    pub fn new(size: usize) -> Self {
        Var { size, location: Location::Stack }
    }
}

pub(super) enum Location {
    Stack,
    Heap,
    GcHeap,
}

#[derive(Debug, Clone)]
pub(super) enum Op {
    Move {
        source: Id,
        destination: Id,
        on: OnMove,
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

#[derive(Debug, Clone)]
pub(super) enum OnMove {
    Move,
    Copy,
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
    (@inner mv $source:literal $dest:literal) => {
        {
            Op::Move {
               source: $source,
                destination: $dest,
                on: OnMove::Move,
            }
        }
    };
    (@inner cp $source:literal $dest:literal) => {{
        Op::Move {
            source: $source,
            destination: $dest,
            on: OnMove::Copy,
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
            jump: crate::mir::op::OpId($jump),
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