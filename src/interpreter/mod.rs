use crate::common::{Context, Error, HasName};
use crate::ir::objects::{AllObject, Callable, CurriedFunction, EnumInstance, FunctionInstanceObject, FunctionObject, EnumVariant};
use crate::ir::types::Type;
use crate::ir::{parse_expr, type_check_expr, Expr, IrContext};
use crate::syntax::parse_token;
use crate::{parse_text, parse_tokens, peg_error_to_showed, type_check_objects};
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use itertools::Itertools;

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
            specialized_functions,
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
        ).map_err(|e| e.display(text))
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
                    AllObject::EnumVariantInstance(inst) => {
                        let datas = inst.data.iter().map(|e| {
                            self.eval_expr(e.clone(), ctx)
                        }).collect::<Result<Vec<_>, _>>()?;
                        Ok(ByteCode::EnumVariant(inst.variant.clone(), datas))
                    }
                    o => Ok(ByteCode::Object(o)),
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
            Callable::FuncDef(f1) => match self.functions.iter().find(|&f2| **f1 == *f2.def) {
                Some(obj) => {
                    let ctx = Context {
                        objects: f.scope.iter().zip(obj.args.iter()).map(|(e, a)|
                            self.eval_expr(e.clone(), ctx)
                                .map(|b| ByteCode::Var(a.name.clone().inner(), Box::new(b))))
                            .collect::<Result<Vec<_>, _>>()?,
                        parent: Some(ctx),
                    };
                    self.eval_expr(obj.body.as_ref().clone(), &ctx)
                },
                None => Err(Error::Custom(f.orig.name().span, "Not found".to_owned(), "this".to_owned())),
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ByteCode {
    Object(AllObject),
    Var(String, Box<ByteCode>),
    EnumVariant(Rc<EnumVariant>, Vec<ByteCode>),
    Int(i128),
}

impl HasName for ByteCode {
    fn name(&self) -> &str {
        match self {
            ByteCode::Object(o) => o.name(),
            ByteCode::Var(v, _) => v.as_str(),
            ByteCode::Int(i) => unreachable!(),
            ByteCode::EnumVariant(e, _) => e.name()
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
                    [] => {},
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
