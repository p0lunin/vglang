use crate::mir::op::{Program, Op, OnMove, ProgramScope, Id};

struct Interpreter {
    stack: Box<[u8]>,
}

impl Interpreter {
    pub unsafe fn eval(program: Program) -> Box<[u8]> {
        let Program { scope, ops } = program;
        let mut stack = {
            let stack = vec![0; scope.total_size];
            let stack: Box<[u8]> = stack.into();
            stack
        };

        let mut cur_id = 0;
        loop {
            let op = ops[cur_id].clone();
            match op {
                Op::Exit => break,
                Op::Write { value, destination } => {
                    let (dst, _) = Interpreter::get_variable(destination, &scope, &mut stack);
                    unsafe {
                        std::ptr::copy(
                            value.as_ptr(),
                            dst,
                            value.len(),
                        )
                    };
                    cur_id += 1;
                },
                Op::Move {
                    source, destination, on
                } => {
                    if source == destination {
                        cur_id += 1;
                        continue;
                    }
                    let (src, len) = Interpreter::get_variable(source, &scope, &mut stack);
                    let (dest, len2) = Interpreter::get_variable(destination, &scope, &mut stack);
                    debug_assert_eq!(len, len2);

                    let len = scope.vars[source].1.size;
                    std::ptr::copy_nonoverlapping(
                        src,
                        dest,
                        len,
                    );
                    match on {
                        OnMove::Move => {},
                        OnMove::Copy => {}
                    }
                    cur_id += 1;
                }
                Op::CaseArm { pattern, matched, jump } => {
                    let (pat, pat_len) = Interpreter::get_variable(pattern, &scope, &mut stack);
                    let (matched, m_len) = Interpreter::get_variable(matched, &scope, &mut stack);
                    debug_assert_eq!(pat_len, m_len);

                    match std::slice::from_raw_parts(pat, pat_len) == std::slice::from_raw_parts(matched, m_len) {
                        true => {
                            cur_id = jump.0;
                        },
                        false => {
                            cur_id += 1;
                        }
                    }

                }
            }
        }
        stack
    }

    fn get_variable(variable: Id, scope: &ProgramScope, stack: &mut [u8]) -> (*mut u8, usize) {
        (&mut stack[scope.vars[variable].0] as *mut u8, scope.vars[variable].1.size)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::op::Var;
    use crate::program;

    #[test]
    fn case_eq() {
        let program = Program::new(
            ProgramScope::new(
                vec![
                    Var::new(1),
                    Var::new(1),
                    Var::new(1),
                    Var::new(1),
                    Var::new(1),
                ]
            ),
            program! {
                wr ([1;1] => 0)
                wr ([1;1] => 1)
                wr ([2;1] => 2)
                cs (0 1 5)
                ex ()
                mv (1 3)
                cp (2 4)
                ex ()
            }
        );

        let stack = unsafe { Interpreter::eval(program) };
        assert_eq!(&stack[0..5], &[1, 1, 2, 1, 2])
    }
}
