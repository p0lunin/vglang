use crate::common::{Context, Error, HasName};
use crate::ir::objects::{
    DataVariant, FunctionObject,
};
use crate::ir::types::Type;
use crate::ir::{parse_expr, Expr, ExprKind};
use crate::peg_error_to_showed;
use crate::syntax::parse_token;
use itertools::Itertools;
use std::fmt::{Display, Formatter};
use std::ops;
use std::ops::Deref;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct Interpreter<'a> {
    enums: Vec<Rc<EnumInstance>>,
    functions: Vec<FunctionObject>,
    ir_ctx: IrContext,
    ctx: Context<'a, AllObject>,
}

impl<'a> Interpreter<'a> {
    pub fn from_ir_context(ctx: Context<'a, AllObject>, ir: IrContext) -> Interpreter<'a> {
        let IrContext {
            functions,
            specialized_enums,
            specialized_functions: _,
        } = &ir;
        Self {
            enums: specialized_enums.clone(),
            functions: functions.clone(),
            ir_ctx: ir,
            ctx,
        }
    }

    pub fn execute_code(&mut self, text: &str) -> Result<ByteCode, String> {
        let tokens = parse_token(text).map_err(|e| peg_error_to_showed(e, text))?;
        let e = parse_expr(tokens, &self.ctx, &mut self.ir_ctx).map_err(|e| e.display(text))?;
        type_check_expr(&e, &self.ctx, &mut self.ir_ctx).map_err(|e| e.display(text))?;
        self.eval_expr(
            e,
            &Context {
                objects: vec![],
                parent: None,
            },
        )
        .map_err(|e| e.display(text))
    }

    fn eval_expr(&self, e: Expr, ctx: &Context<ByteCode>) -> Result<ByteCode, Error> {
        let span = e.span();
        match e.kind {
            ExprKind::Object(o) => match o {
                AllObject::CurriedFunction(f) => self.eval_fn(&f, ctx),
                AllObject::Arg(a) => match ctx.find(a.name.as_str()) {
                    Some(ByteCode::Var(_, b)) => Ok(b.as_ref().clone()),
                    _ => Err(Error::Span(span)),
                },
                AllObject::EnumVariantInstance(inst) => {
                    let datas = inst
                        .data
                        .iter()
                        .map(|e| self.eval_expr(e.clone(), ctx))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(ByteCode::EnumVariant(inst.variant.clone(), datas))
                }
                AllObject::FunctionDefinition(f) => {
                    self.eval_fn(&Rc::new(CurriedFunction {
                        ftype: f.ftype.clone(),
                        scope: vec![],
                        orig: Callable::FuncDef(f),
                        instance: RefCell::new(None)
                    }), ctx)
                }
                o => Ok(ByteCode::Object(o)),
            },
            ExprKind::Int(i) => Ok(ByteCode::Int(i)),
            ExprKind::Add(l, r) => self.eval_arithmetic_op(l, r, ctx, ops::Add::add),
            ExprKind::Sub(l, r) => self.eval_arithmetic_op(l, r, ctx, ops::Sub::sub),
            ExprKind::Mul(l, r) => self.eval_arithmetic_op(l, r, ctx, ops::Mul::mul),
            ExprKind::Div(l, r) => self.eval_arithmetic_op(l, r, ctx, ops::Div::div),
            ExprKind::Pow(l, r) => self.eval_arithmetic_op(l, r, ctx, |l, r| l.pow(r as u32)),
            ExprKind::And(_, _) => unimplemented!(),
            ExprKind::Or(_, _) => unimplemented!(),
            ExprKind::Gr(l, r) => { 
                self.eval_logic_func(l, r, ctx, i128::gt)
            },
            ExprKind::Eq(l, r) => {
                self.eval_logic_func(l, r, ctx, i128::eq)
            },
            ExprKind::NotEq(l, r) => {
                self.eval_logic_func(l, r, ctx, i128::ne)
            },
            ExprKind::GrOrEq(l, r) => {
                self.eval_logic_func(l, r, ctx, i128::ge)
            },
            ExprKind::Le(l, r) => {
                self.eval_logic_func(l, r, ctx, i128::lt)
            },
            ExprKind::LeOrEq(l, r) => {
                self.eval_logic_func(l, r, ctx, i128::le)
            },
            ExprKind::Neg(l) => {
                let span = l.span();
                self.eval_expr(*l, ctx).and_then(|b| match b {
                    ByteCode::Int(i) => Ok(ByteCode::Int(-i)),
                    _ => Err(Error::Span(span)),
                })
            }
            ExprKind::Let { var, assign, expr } => {
                let var_val = self.eval_expr(*assign, ctx)?;
                let var = ByteCode::Var(var.name.clone().inner(), Box::new(var_val));
                let ctx = Context {
                    objects: vec![var],
                    parent: Some(ctx),
                };
                return self.eval_expr(*expr, &ctx);
            }
            ExprKind::IfThenElse {
                condition,
                then_arm,
                else_arm,
            } => {
                let cond = self.eval_expr(*condition, ctx)?;
                match cond {
                    ByteCode::EnumVariant(v, _) => match v.orig.name.as_str() {
                        "True" => self.eval_expr(*then_arm, ctx),
                        "False" => self.eval_expr(*else_arm, ctx),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
        }
    }

    fn eval_fn(&self, f: &Rc<CurriedFunction>, ctx: &Context<ByteCode>) -> Result<ByteCode, Error> {
        match Type::get_inner_cell(&f.ftype).borrow().deref() {
            Type::Function(_) => {
                return Ok(ByteCode::Object(AllObject::CurriedFunction(f.clone())))
            }
            _ => {}
        };
        match &f.orig {
            Callable::FuncDef(f1) => match self.functions.iter().find(|&f2| **f1 == *f2.def) {
                Some(obj) => {
                    let ctx = Context {
                        objects: f
                            .scope
                            .iter()
                            .zip(obj.args.iter())
                            .map(|(e, a)| {
                                self.eval_expr(e.clone(), ctx)
                                    .map(|b| ByteCode::Var(a.name.clone().inner(), Box::new(b)))
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                        parent: Some(ctx),
                    };
                    self.eval_expr(obj.body.as_ref().clone(), &ctx)
                }
                None => Err(Error::Custom(
                    f.orig.name().span,
                    "Not found".to_owned(),
                    "this".to_owned(),
                )),
            },
            _ => unimplemented!(),
        }
    }

    fn eval_logic_func<F: Fn(&i128, &i128) -> bool>(&self, left: Box<Expr>, right: Box<Expr>, ctx: &Context<'_, ByteCode>, f: F) -> Result<ByteCode, Error> {
        let left = self.eval_expr(*left, ctx)?;
        let right = self.eval_expr(*right, ctx)?;
        match (left, right) {
            (ByteCode::Int(i1), ByteCode::Int(i2)) => {
                let bool_enum = match self.ctx.find("Bool").unwrap() {
                    AllObject::Enum(e) => e,
                    _ => unreachable!(),
                }.borrow();
                match f(&i1, &i2) {
                    true => Ok(ByteCode::EnumVariant(
                        bool_enum.variants.iter()
                            .find(|v| v.orig.name.as_str() == "True")
                            .unwrap()
                            .clone(),
                        vec![]
                    )),
                    false => Ok(ByteCode::EnumVariant(
                        bool_enum.variants.iter()
                            .find(|v| v.orig.name.as_str() == "False")
                            .unwrap()
                            .clone(),
                        vec![]
                    ))
                }
            }
            _ => unreachable!()
        }
    }

    fn eval_arithmetic_op<F: Fn(i128, i128) -> i128>(
        &self,
        left: Box<Expr>,
        right: Box<Expr>,
        ctx: &Context<ByteCode>,
        f: F,
    ) -> Result<ByteCode, Error> {
        let new_span = left.span().extend(&right.span());
        let left = self.eval_expr(*left, ctx)?;
        let right = self.eval_expr(*right, ctx)?;
        match (left, right) {
            (ByteCode::Int(i1), ByteCode::Int(i2)) => Ok(ByteCode::Int(f(i1, i2))),
            _ => Err(Error::Span(new_span)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ByteCode {
    Object(AllObject),
    Var(String, Box<ByteCode>),
    EnumVariant(Rc<DataVariant>, Vec<ByteCode>),
    Int(i128),
}

impl HasName for ByteCode {
    fn name(&self) -> &str {
        match self {
            ByteCode::Object(o) => o.name(),
            ByteCode::Var(v, _) => v.as_str(),
            ByteCode::Int(_) => unreachable!(),
            ByteCode::EnumVariant(e, _) => e.name(),
        }
    }
}

impl Display for ByteCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ByteCode::Int(i) => write!(f, "{}", i),
            ByteCode::Object(o) => write!(f, "{}", o),
            ByteCode::EnumVariant(e, data) => {
                write!(f, "{}", e)?;
                match data.as_slice() {
                    [] => {}
                    [xs @ ..] => {
                        write!(f, ": ");
                        write!(f, "({})", xs.iter().map(|d| d.to_string()).join(", "))?;
                    }
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }
}
