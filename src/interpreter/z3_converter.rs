use crate::common;
use crate::common::{Error, Span};
use crate::interpreter::{ByteCode, Interpreter};
use crate::ir::types::Type;
use crate::ir::{Expr, ExprKind};
use std::convert::TryInto;
use std::ops::Deref;
use std::rc::Rc;
use typed_arena::Arena;
use z3::ast::{Ast, Bool};
use z3::{ast, Config, Context, SatResult, Solver};

#[derive(Debug, Clone)]
pub enum Z3Ast<'a> {
    Int(ast::Int<'a>),
    Bool(ast::Bool<'a>),
    Array(ast::Array<'a>),
}

impl<'a> Z3Ast<'a> {
    pub fn to_dynamic(&self) -> ast::Dynamic<'a> {
        match self {
            Z3Ast::Int(i) => i.clone().into(),
            Z3Ast::Bool(i) => i.clone().into(),
            Z3Ast::Array(i) => i.clone().into(),
        }
    }

    pub fn _eq(&self, other: &Self) -> Option<Bool> {
        match (self, other) {
            (Z3Ast::Int(i1), Z3Ast::Int(i2)) => Some(i1._eq(i2)),
            _ => unimplemented!(),
        }
    }
}

pub struct Consts<'a> {
    pub(crate) available_consts: Vec<(String, Z3Ast<'a>)>,
    pub named_with_refinement: Vec<(String, Z3Ast<'a>, Z3Ast<'a>)>,
    pub(crate) values: Arena<Z3Ast<'a>>,
}

impl<'a> Consts<'a> {
    pub fn new() -> Self {
        Consts {
            available_consts: vec![],
            named_with_refinement: vec![],
            values: Arena::new(),
        }
    }

    pub fn find<T: AsRef<str>>(&self, name: T) -> Option<&Z3Ast<'a>> {
        self.available_consts
            .iter()
            .map(|(e, r)| (e, r))
            .chain(
                self.named_with_refinement
                    .iter()
                    .map(|(name, ast, _)| (name, ast)),
            )
            .find(|n| n.0.as_str() == name.as_ref())
            .map(|res| res.1)
    }
}

impl<'a> Consts<'a> {
    fn add(&self, ast: Z3Ast<'a>) -> &Z3Ast<'a> {
        self.values.alloc(ast)
    }
    fn add_named(&mut self, ast: Z3Ast<'a>, name: String) {
        self.available_consts.push((name, ast));
    }
    fn last_named(&self) -> &Z3Ast<'a> {
        &self.available_consts.last().unwrap().1
    }
    fn add_refinement(&mut self, ast: Z3Ast<'a>, name: String, expr: Z3Ast<'a>) {
        self.named_with_refinement.push((name, ast, expr))
    }
}

#[derive(Debug)]
pub struct ProofContext<'a> {
    pub(crate) solver: Solver<'a>,
    pub(crate) prefix: String,
    pub interpreter: Interpreter<'a>,
}

impl<'a> ProofContext<'a> {
    pub fn declare_var(
        &mut self,
        name: String,
        ty: &Type,
        consts: &mut Consts<'a>,
    ) -> Result<(), Error> {
        match ty {
            Type::Int => {
                consts.add_named(
                    Z3Ast::Int(ast::Int::new_const(
                        self.solver.get_context(),
                        name.as_str(),
                    )),
                    name,
                );
                Ok(())
            }
            Type::Named(_, expr) => self.declare_var(name, expr, consts),
            Type::Expr(e) => {
                let var = match e.ty.deref() {
                    Type::Int => Z3Ast::Int(ast::Int::new_const(
                        self.solver.get_context(),
                        name.as_str(),
                    )),
                    _ => unimplemented!(),
                };
                let expr = self.expr_to_z3(e.clone(), consts)?;
                consts.add_refinement(var, name, expr);
                Ok(())
            }
            _ => unimplemented!(),
        }
    }
    pub fn proof_check(
        &mut self,
        expr: Expr,
        expected_type: &Type,
        consts: &mut Consts<'a>,
    ) -> Result<(), Error> {
        let typ_expr = match expected_type {
            Type::Function(_) => unimplemented!(),
            Type::Expr(e) => e,
            _ => return Ok(()),
        };
        self.check_two_exprs_eq(expr, typ_expr.clone(), consts)
    }
    fn check_two_exprs_eq(
        &mut self,
        left: Expr,
        right: Expr,
        consts: &mut Consts<'a>,
    ) -> Result<(), Error> {
        let span = left.span;

        let left = self.expr_to_z3(left, consts)?;
        let right = self.expr_to_z3(right, consts)?;

        let left = consts.add(left);
        let right = consts.add(right);

        match (left, right) {
            (Z3Ast::Int(i1), Z3Ast::Int(i2)) => {
                self.assert(i1._eq(i2), consts);
                check_err(self.solver.check(), span)
            }
            (Z3Ast::Bool(i1), Z3Ast::Bool(i2)) => {
                self.assert(i1._eq(i2), consts);
                check_err(self.solver.check(), span)
            }
            _ => unimplemented!(),
        }
    }

    fn expr_to_z3(&mut self, expr: Expr, consts: &mut Consts<'a>) -> Result<Z3Ast<'a>, Error> {
        match expr.kind {
            ExprKind::Int(i) => Ok(Z3Ast::Int(ast::Int::from_i64(
                self.solver.get_context(),
                i as i64,
            ))),
            ExprKind::Add(l, r) => Ok(Z3Ast::Int(
                match (self.expr_to_z3(*l, consts)?, self.expr_to_z3(*r, consts)?) {
                    (Z3Ast::Int(i1), Z3Ast::Int(i2)) => {
                        ast::Int::add(self.solver.get_context(), &[&i1, &i2])
                    }
                    _ => unimplemented!(),
                },
            )),
            ExprKind::Ident(i) => {
                let var_name = self.prefix.clone() + i.as_str();
                match consts.find(var_name) {
                    Some(ast) => match ast {
                        Z3Ast::Int(i) => Ok(Z3Ast::Int(i.clone())),
                        Z3Ast::Array(i) => Ok(Z3Ast::Array(i.clone())),
                        Z3Ast::Bool(i) => Ok(Z3Ast::Bool(i.clone())),
                    },
                    None => Err(Error::custom(expr.span, "Not found")),
                }
            }
            _ => Ok(byte_code_to_z3(
                self.interpreter.eval(
                    &expr,
                    &common::Context {
                        objects: vec![],
                        parent: None,
                    },
                )?,
                self.solver.get_context(),
            )),
        }
    }

    fn assert(&self, expr: ast::Bool, consts: &Consts) {
        let vars = consts
            .available_consts
            .iter()
            .map(|(_, a)| a)
            .chain(consts.named_with_refinement.iter().map(|(_, var, _)| var))
            .map(Z3Ast::to_dynamic)
            .collect::<Vec<_>>();
        let body = match refinements_to_z3(
            self.solver.get_context(),
            consts.named_with_refinement.as_slice(),
        ) {
            Some(b) => b.implies(&expr),
            None => expr.into(),
        };
        self.solver.assert(
            &ast::forall_const(
                self.solver.get_context(),
                vars.iter().collect::<Vec<_>>().as_slice(),
                &[],
                &body.into(),
            )
            .try_into()
            .unwrap(),
        )
    }

    fn make_name<T: AsRef<str>>(&self, name: T) -> String {
        self.prefix.clone() + name.as_ref()
    }

    fn make_sign_name<T: AsRef<str>>(&self, name: T) -> String {
        self.prefix.clone() + "sign_" + name.as_ref()
    }
}

fn refinements_to_z3<'a>(
    ctx: &'a Context,
    refinements: &'a [(String, Z3Ast<'a>, Z3Ast<'a>)],
) -> Option<Bool<'a>> {
    match refinements {
        [] => None,
        [x, xs @ ..] => match refinements_to_z3(ctx, xs) {
            None => x.1._eq(&x.2),
            Some(e) => Some(Bool::and(ctx, &[&e, &x.1._eq(&x.2)?])),
        },
    }
}

fn byte_code_to_z3(bc: ByteCode, ctx: &Context) -> Z3Ast {
    match bc {
        ByteCode::ApplicationFunction(_, _, _) => unimplemented!(),
        ByteCode::Int(i) => Z3Ast::Int(ast::Int::from_i64(ctx, i as i64)),
        ByteCode::Var(_, _) => unimplemented!(),
        ByteCode::Arg(_, _) => unimplemented!(),
    }
}

fn check_err(res: SatResult, span: Span) -> Result<(), Error> {
    match res {
        SatResult::Unknown => Err(Error::custom(span, "Cannot proof this")),
        SatResult::Unsat => Err(Error::custom(span, "Unsatisfied")),
        SatResult::Sat => Ok(()),
    }
}
