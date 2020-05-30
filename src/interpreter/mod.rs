use crate::common::{Context, Error, HasName};
use crate::ir::objects::{
    AllObject, Callable, CurriedFunction, EnumInstance, FunctionInstanceObject, FunctionObject,
};
use crate::ir::types::Type;
use crate::ir::{parse_expr, type_check_expr, Expr, IrContext};
use crate::syntax::parse_token;
use crate::{parse_text, parse_tokens, peg_error_to_showed, type_check_objects};
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub struct Interpreter<'a> {
    enums: Vec<Rc<EnumInstance>>,
    functions: Vec<Rc<FunctionObject>>,
    ir_ctx: IrContext,
    ctx: Context<'a, AllObject>,
}

impl<'a> Interpreter<'a> {
    pub fn from_ir_context(ctx: Context<'a, AllObject>, ir: IrContext) -> Interpreter<'a> {
        let IrContext {
            specialized_enums,
            specialized_functions,
        } = &ir;
        Self {
            enums: specialized_enums.clone(),
            functions: specialized_functions
                .iter()
                .filter_map(|f| match &f.orig {
                    Callable::Func(f) => Some(f.clone()),
                    _ => None,
                })
                .collect(),
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
        match e {
            Expr::Object(o) => {
                let span = o.span;
                match o.inner() {
                    AllObject::CurriedFunction(f) => self.eval_fn(&f, ctx),
                    AllObject::Arg(a) => match ctx.find(a.name.as_str()) {
                        Some(ByteCode::Var(_, b)) => Ok(b.as_ref().clone()),
                        _ => Err(Error::Span(span)),
                    },
                    _ => unimplemented!(),
                }
            }
            Expr::Int(i) => Ok(ByteCode::Int(i.inner())),
            Expr::Add(l, r) => {
                let new_span = l.span().extend(&r.span());
                let left = self.eval_expr(*l, ctx)?;
                let right = self.eval_expr(*r, ctx)?;
                match (left, right) {
                    (ByteCode::Int(i1), ByteCode::Int(i2)) => Ok(ByteCode::Int(i1 + i2)),
                    _ => Err(Error::Span(new_span)),
                }
            }
            _ => unimplemented!(),
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
            Callable::Func(fo) => {
                let args: Vec<ByteCode> = fo
                    .args
                    .iter()
                    .zip(f.scope.iter())
                    .map(|(a, e)| {
                        Ok(ByteCode::Var(
                            a.name.clone().inner(),
                            Box::new(self.eval_expr(e.clone(), ctx)?),
                        ))
                    })
                    .collect::<Result<Vec<ByteCode>, _>>()?;
                let ctx = Context {
                    objects: args,
                    parent: Some(ctx),
                };
                self.eval_expr(fo.body.as_ref().clone(), &ctx)
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ByteCode {
    Object(AllObject),
    Var(String, Box<ByteCode>),
    Int(i128),
}

impl HasName for ByteCode {
    fn name(&self) -> &str {
        match self {
            ByteCode::Object(o) => o.name(),
            ByteCode::Var(v, _) => v.as_str(),
            ByteCode::Int(i) => unreachable!(),
        }
    }
}

impl Display for ByteCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ByteCode::Int(i) => write!(f, "{}", i),
            ByteCode::Object(o) => write!(f, "{}", o),
            _ => unreachable!(),
        }
    }
}
