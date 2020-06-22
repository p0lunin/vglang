use crate::common::{Context, Error, Span, Spanned};
use crate::ir::objects::{AllObject, TypeObject, Var};
use crate::ir::types::{Type, TypeKind};
use crate::ir::IrContext;
use crate::syntax::ast::{Ast, Token};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub ty: Rc<RefCell<Type>>,
    span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(ty: Rc<RefCell<Type>>, span: Span, kind: ExprKind) -> Self {
        Expr { ty, span, kind }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Int(i128),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Gr(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
    GrOrEq(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    LeOrEq(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Object(AllObject),
    Let {
        var: Rc<Var>,
        assign: Box<Expr>,
        expr: Box<Expr>,
    },
    IfThenElse {
        condition: Box<Expr>,
        then_arm: Box<Expr>,
        else_arm: Box<Expr>,
    },
}

macro_rules! impl_op {
    ($self:tt, $other:tt, $op:tt, $variant:tt) => {{
        let new_span = $self.span().extend(&$other.span);
        match (&$self.kind, &$other.kind) {
            (ExprKind::Int(i), ExprKind::Int(n)) => {
                let new_val = *i $op *n;
                return Expr {
                    ty: Type::int_val(new_val, new_span),
                    span: new_span,
                    kind: ExprKind::Int(new_val)
                }
            },
            _ => {
                let new_type = $self.ty.borrow().pow($other.ty.borrow().deref())
                    .map(|ty| Rc::new(RefCell::new(ty)))
                    .unwrap_or_else(|_| Type::unknown(new_span));
                Expr::new(
                    new_type,
                    new_span,
                    ExprKind::$variant(Box::new($self), Box::new($other))
                )
            }
        }
    }}
}

impl Expr {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn add(self, other: Expr) -> Self {
        impl_op!(self, other, +, Add)
    }
    pub fn mul(self, other: Expr) -> Self {
        impl_op!(self, other, *, Mul)
    }
    pub fn sub(self, other: Expr) -> Self {
        impl_op!(self, other, -, Sub)
    }
    pub fn div(self, other: Expr) -> Self {
        impl_op!(self, other, /, Div)
    }
    pub fn pow(self, other: Expr) -> Self {
        let new_span = self.span().extend(&other.span);
        match (&self.kind, &other.kind) {
            (ExprKind::Int(i), ExprKind::Int(n)) => {
                let new_val = (*i as f64).powi(*n as i32) as i128;
                return Expr {
                    ty: Type::int_val(new_val, new_span),
                    span: new_span,
                    kind: ExprKind::Int(new_val),
                };
            }
            _ => {
                let new_type = self
                    .ty
                    .borrow()
                    .pow(other.ty.borrow().deref())
                    .map(|ty| Rc::new(RefCell::new(ty)))
                    .unwrap_or_else(|_| Type::unknown(new_span));
                Expr::new(
                    new_type,
                    new_span,
                    ExprKind::Pow(Box::new(self), Box::new(other)),
                )
            }
        }
    }
    pub fn and(self, other: Expr) -> Self {
        unimplemented!()
    }
    pub fn or(self, other: Expr) -> Self {
        unimplemented!()
    }
    pub fn eq(self, other: Expr, bool_type: Rc<RefCell<Type>>) -> Self {
        Expr::new(bool_type, self.span.extend(&other.span), ExprKind::Eq(Box::new(self), Box::new(other)))
    }
    pub fn not_eq(self, other: Expr, bool_type: Rc<RefCell<Type>>) -> Self {
        Expr::new(bool_type, self.span.extend(&other.span), ExprKind::NotEq(Box::new(self), Box::new(other)))
    }
    pub fn gr(self, other: Expr, bool_type: Rc<RefCell<Type>>) -> Self {
        Expr::new(bool_type, self.span.extend(&other.span), ExprKind::Gr(Box::new(self), Box::new(other)))
    }
    pub fn gr_or_eq(self, other: Expr, bool_type: Rc<RefCell<Type>>) -> Self {
        Expr::new(bool_type, self.span.extend(&other.span), ExprKind::GrOrEq(Box::new(self), Box::new(other)))
    }
    pub fn le(self, other: Expr, bool_type: Rc<RefCell<Type>>) -> Self {
        Expr::new(bool_type, self.span.extend(&other.span), ExprKind::Le(Box::new(self), Box::new(other)))
    }
    pub fn le_or_eq(self, other: Expr, bool_type: Rc<RefCell<Type>>) -> Self {
        Expr::new(bool_type, self.span.extend(&other.span), ExprKind::LeOrEq(Box::new(self), Box::new(other)))
    }
    pub fn neg(self) -> Self {
        unimplemented!()
    }
    pub fn call(self) -> Result<Self, Error> {
        let span = self.span;
        match self.kind {
            ExprKind::Object(o) => {
                let new_o = o.call()?;
                Ok(Expr::new(
                    new_o.get_type().clone(),
                    span,
                    ExprKind::Object(new_o),
                ))
            }
            _ => Ok(self),
        }
    }
}

pub fn parse_expr(
    token: Token,
    ctx: &Context<'_, AllObject>,
    ir_ctx: &mut IrContext,
) -> Result<Expr, Error> {
    let bool_type = ctx.find("Bool").unwrap().get_type();
    match token.ast {
        Ast::Int(i) => Ok(Expr::new(
            Type::int_val(i, token.span),
            token.span,
            ExprKind::Int(i),
        )),
        Ast::Add(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.add(parse_expr(*r, ctx, ir_ctx)?)),
        Ast::Sub(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.sub(parse_expr(*r, ctx, ir_ctx)?)),
        Ast::Ident(i) => match i.as_ref() {
            "Int" => Ok(Expr::new(
                Type::type_type(),
                token.span,
                ExprKind::Object(AllObject::Type(Rc::new(TypeObject {
                    name: Spanned::new("".to_owned(), token.span),
                    ttype: Rc::new(RefCell::new(Type::Int(TypeKind::empty(token.span)))),
                }))),
            )),
            "Type" => Ok(Expr::new(
                Type::type_type(),
                token.span,
                ExprKind::Object(AllObject::Type(Rc::new(TypeObject {
                    name: Spanned::new("".to_owned(), token.span),
                    ttype: Type::type_type(),
                }))),
            )),
            name => match ctx.find(name) {
                Some(o) => {
                    let o_cloned = o.clone();
                    Ok(Expr::new(
                        o_cloned.get_type(),
                        token.span,
                        ExprKind::Object(o_cloned),
                    ))
                }
                _ => Err(Error::Custom(
                    token.span,
                    format!("{} not found", i),
                    "-here".to_owned(),
                )),
            },
        },
        Ast::CallFunction(func, arg) => {
            let left_expr = parse_expr(*func, ctx, ir_ctx)?;
            let arg_expr = parse_expr(*arg, ctx, ir_ctx)?;
            match &left_expr.kind {
                ExprKind::Object(o) => {
                    let new_o = o.call_with_arg_expr(arg_expr, ir_ctx, token.span)?;
                    Ok(Expr::new(
                        new_o.get_type(),
                        token.span,
                        ExprKind::Object(new_o),
                    ))
                }
                _ => Err(Error::Span(token.span)),
            }
        }
        Ast::Dot(l, r) => {
            let left_expr = parse_expr(*l, ctx, ir_ctx)?;
            let right_expr = match r.ast {
                Ast::Ident(i) => i,
                _ => return Err(Error::Span(r.span)),
            };
            let span = token.span;
            match left_expr.kind {
                ExprKind::Object(o) => o
                    .try_get_member(right_expr.as_str(), span)
                    .map(|o| Expr::new(o.get_type(), span, ExprKind::Object(o))),
                _ => Err(Error::Span(span)),
            }
        }
        Ast::Parenthesis(p) => parse_expr(*p, ctx, ir_ctx),
        Ast::Mul(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.mul(parse_expr(*r, ctx, ir_ctx)?)),
        Ast::Div(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.div(parse_expr(*r, ctx, ir_ctx)?)),
        Ast::Pow(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.pow(parse_expr(*r, ctx, ir_ctx)?)),
        Ast::And(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.and(parse_expr(*r, ctx, ir_ctx)?)),
        Ast::Or(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.or(parse_expr(*r, ctx, ir_ctx)?)),
        Ast::Gr(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.gr(parse_expr(*r, ctx, ir_ctx)?, bool_type)),
        Ast::Le(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.le(parse_expr(*r, ctx, ir_ctx)?, bool_type)),
        Ast::GrEq(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.gr_or_eq(parse_expr(*r, ctx, ir_ctx)?, bool_type)),
        Ast::LeEq(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.le_or_eq(parse_expr(*r, ctx, ir_ctx)?, bool_type)),
        Ast::Eq(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.eq(parse_expr(*r, ctx, ir_ctx)?, bool_type)),
        Ast::NotEq(l, r) => Ok(parse_expr(*l, ctx, ir_ctx)?.not_eq(parse_expr(*r, ctx, ir_ctx)?, bool_type)),
        Ast::Neg(t) => Ok(parse_expr(*t, ctx, ir_ctx)?.neg()),
        Ast::Double(_) => unimplemented!(),
        Ast::Val => Err(Error::Span(token.span)),
        Ast::Slice(_) => unimplemented!(),
        Ast::Implication(_, _) => Err(Error::Span(token.span)),
        Ast::Named(_, _) => Err(Error::Span(token.span)),
        Ast::Let { var, assign, expr } => {
            let assign = parse_expr(*assign, ctx, ir_ctx)?;
            let var = Rc::new(Var {
                name: var,
                ty: assign.ty.clone(),
            });
            let ctx = Context {
                objects: vec![AllObject::Var(var.clone())],
                parent: Some(ctx),
            };
            let expr = Box::new(parse_expr(*expr, &ctx, ir_ctx)?);
            Ok(Expr::new(
                expr.ty.clone(),
                token.span,
                ExprKind::Let {
                    var,
                    assign: Box::new(assign),
                    expr,
                },
            ))
        }
        Ast::IfThenElse { if_, then, else_ } => {
            let cond = parse_expr(*if_, ctx, ir_ctx)?;
            let bool_type = ctx.find("Bool").unwrap();
            let ty = cond.ty.borrow();
            match ty.is_part_of(bool_type.get_type().borrow().deref()) {
                true => {
                    let then_arm = parse_expr(*then, ctx, ir_ctx)?;
                    let else_arm = parse_expr(*else_, ctx, ir_ctx)?;
                    drop(ty);
                    let if_ty = Rc::new(RefCell::new(
                        then_arm
                            .ty
                            .borrow()
                            .or(else_arm.ty.borrow().deref())
                            .map_err(|e| Error::Custom(cond.span, e, "-here".to_string()))?,
                    ));
                    Ok(Expr::new(
                        if_ty,
                        token.span,
                        ExprKind::IfThenElse {
                            condition: Box::new(cond),
                            then_arm: Box::new(then_arm),
                            else_arm: Box::new(else_arm),
                        },
                    ))
                }
                false => Err(Error::Custom(
                    cond.span,
                    "Condition must be Bool type".to_string(),
                    format!("Expected Bool, found {} type", cond.ty.borrow()),
                )),
            }
        }
    }
}
