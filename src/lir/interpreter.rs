use crate::lir::op::{Program, Op, ProgramScope, Id, Var, Bytes, Scope, VarTy};
use std::alloc::alloc;
use std::alloc::Layout;
use std::mem::{ManuallyDrop, size_of};
use core::ptr;

struct Interpreter {
    ops: Vec<Op>,
    stack: Box<[u8]>,
    current_scope: ProgramScope,
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            ops: program.ops,
            current_scope: program.scope,
            stack: {
                let stack = Vec::with_capacity(4096);
                let stack: Box<[u8]> = stack.into();
                stack
            }
        }
    }
    pub unsafe fn eval(mut self) -> Box<[u8]> {
        let mut cur_id = 0;
        loop {
            let op = self.ops[cur_id].clone();
            match op {
                Op::Exit => break,
                Op::Write { value, destination } => {
                    let (ptr, size) = self.get_variable(destination);
                    debug_assert_eq!(size, value.len());
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            value.as_ptr(),
                            ptr,
                            value.len(),
                        )
                    };
                    cur_id += 1;
                },
                Op::Copy {
                    source, destination
                } => {
                    if source == destination {
                        cur_id += 1;
                        continue;
                    }
                    let (src, len) = self.get_variable(source);
                    let (dest, len2) = self.get_variable(destination);
                    debug_assert_eq!(len, len2);

                    std::ptr::copy_nonoverlapping(
                        src,
                        dest,
                        len,
                    );
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
                Op::Alloc { size, destination } => {
                    let (var, var_size) = Interpreter::get_variable(destination, &scope, &mut stack);
                    debug_assert_eq!(var_size, size);

                    let data = {
                        let mut vc = ManuallyDrop::new(Vec::<u8>::with_capacity(size));
                        vc.as_mut_ptr()
                    };
                    let pointer = data as usize;
                    ptr::copy_nonoverlapping(
                        &pointer as *const usize as *const u8,
                        var,
                        size_of::<usize>(),
                    )
                }
                Op::Dealloc { .. } => {}
            }
        }
        stack
    }

    unsafe fn get_variable(&mut self, variable: Id) -> (*mut u8, usize) {
        let (offset, var) = self.current_scope.vars[variable].clone();
        let mut ptr = &mut self.stack[offset] as *mut u8;
        let size = match var.location {
            VarTy::Stack => var.size,
            VarTy::Heap => {
                let size = *(ptr.offset(size_of::<usize>() as isize) as *const usize);
                ptr = *(ptr as *mut *mut u8);
                size
            }
        };
        (ptr, size)
    }
}

enum VarPlace {
    Stack,
    Heap { size: Bytes }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir::op::Var;
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
                cp (2 4)
                ex ()
            }
        );

        let stack = unsafe { Interpreter::eval(program) };
        assert_eq!(&stack[0..5], &[1, 1, 2, 1, 2])
    }
}
