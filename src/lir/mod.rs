type Id = usize;
type Bytes = usize;

#[derive(Debug, Clone)]
struct Var {
    size: Bytes,
    offset: Bytes,
    ty: VarTy,
}

#[derive(Debug, Clone)]
enum VarTy {
    Stacked,
    Boxed,
}

#[derive(Debug, Clone)]
enum Value {
    Const(Vec<u8>),
    Var(Var)
}

#[derive(Debug, Clone)]
enum Op {
    Write {
        source: Value,
        destination: Var,
    },
    Call(Function),
    Case {
        tag: Value,
        expected: Value,
        arm: Id, // op
    },
    BinOp {
        op1: Value,
        op2: Value,
        dest: Var,
        op: BinOp,
    },
    Exit,
}

#[derive(Debug, Clone)]
enum BinOp {
    Eq,
    AddI128
}

#[derive(Debug, Clone)]
struct Scope {
    vars: Vec<Var>,
}

#[derive(Debug, Clone)]
enum Function {
    Builtin(u64),
    User(Id) // user function
}

#[derive(Debug, Clone)]
struct UserFunction {
    scope: Scope,
    ops: Vec<Op>,
}

#[derive(Debug, Clone)]
struct State {
    stack: Box<[u8]>,
}

impl State {
    pub fn new(stack_size: usize) -> Self {
        State { stack: vec![0; stack_size].into() }
    }
}

fn interpret(program: Vec<Op>, state: &mut State) {
    let mut cur_id = 0;
    let mut op = program[0].clone();
    loop {
        match op {
            Op::Write { source, destination } => {
                let data: Box<[u8]> = match source {
                    Value::Const(data) => data.into(),
                    Value::Var(Var { size, offset, ty: VarTy::Stacked}) => {
                        let data = Box::from(&state.stack[offset..offset+size]);
                        data
                    },
                    Value::Var(Var { size, offset, ty: VarTy::Boxed}) => {
                        unimplemented!()
                    }
                };
                match destination.ty {
                    VarTy::Stacked => {
                        if state.stack.len() < destination.offset + data.len() {
                            panic!("stack overflow error");
                        }
                        let stack_ref = &mut state.stack[destination.offset] as *mut u8;
                        unsafe { std::ptr::copy(data.as_ptr(), stack_ref, data.len()) }
                    }
                    VarTy::Boxed => { unimplemented!() }
                }

                cur_id += 1;
                op = program[cur_id].clone();
            }
            Op::Call(_) => {}
            Op::Case { .. } => {}
            Op::BinOp { .. } => {}
            Op::Exit => return,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn interpreter_write() {
        let program = vec![
            Op::Write {
                source: Value::Const(vec![5;24]),
                destination: Var {
                    size: not_important(),
                    offset: 0,
                    ty: VarTy::Stacked
                }
            },
            Op::Exit,
        ];
        let mut state = State::new(4096);
        interpret(program, &mut state);
        assert_eq!(&state.stack[0..24], &[5; 24]);
        assert!(state.stack[24..4096].iter().all(|x| *x == 0));
    }

    fn not_important() -> usize {
        usize::MAX
    }
}