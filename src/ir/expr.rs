use crate::common::{Spanned, Span, VecType, Error, Context, AddSpan};
use crate::ir::objects::{AllObject, Object};
use std::rc::Rc;
use std::cell::RefCell;
use crate::ir::types::{Type, TypeKind};
use crate::ir::types::base_types::Int;
use crate::syntax::ast::{Token, Ast};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(Spanned<i128>),
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
    Object(Spanned<AllObject>),
}

macro_rules! impl_op {
    ($self:tt, $other:tt, $op:tt, $variant:tt) => {
        match ($self, $other) {
            (Expr::Int(i), Expr::Int(n)) => {
                Expr::Int(Spanned::new(*i $op *n, i.span.extend(&n.span)))
            }
            (l, r) => Expr::$variant(Box::new(l), Box::new(r)),
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(i) => i.span,
            Expr::Add(l, r) => l.span().extend(&r.span()),
            Expr::Sub(l, r) => l.span().extend(&r.span()),
            Expr::Object(o) => o.span,
            Expr::Mul(l, r) => l.span().extend(&r.span()),
            Expr::Div(l, r) => l.span().extend(&r.span()),
            Expr::Pow(l, r) => l.span().extend(&r.span()),
            Expr::And(l, r) => l.span().extend(&r.span()),
            Expr::Or(l, r) => l.span().extend(&r.span()),
            Expr::Gr(l, r) => l.span().extend(&r.span()),
            Expr::Eq(l, r) => l.span().extend(&r.span()),
            Expr::NotEq(l, r) => l.span().extend(&r.span()),
            Expr::GrOrEq(l, r) => l.span().extend(&r.span()),
            Expr::Le(l, r) => l.span().extend(&r.span()),
            Expr::LeOrEq(l, r) => l.span().extend(&r.span()),
            Expr::Neg(l) => l.span(),
        }
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
        match (self, other) {
            (Expr::Int(i), Expr::Int(n)) => Expr::Int(Spanned::new(
                (*i as f64).powi(*n as i32) as i128,
                i.span.extend(&n.span),
            )),
            (l, r) => Expr::Pow(Box::new(l), Box::new(r)),
        }
    }
    pub fn and(self, other: Expr) -> Self {
        Expr::And(Box::new(self), Box::new(other))
    }
    pub fn or(self, other: Expr) -> Self {
        Expr::Or(Box::new(self), Box::new(other))
    }
    pub fn eq(self, other: Expr) -> Self {
        Expr::Eq(Box::new(self), Box::new(other))
    }
    pub fn not_eq(self, other: Expr) -> Self {
        Expr::NotEq(Box::new(self), Box::new(other))
    }
    pub fn gr(self, other: Expr) -> Self {
        Expr::Gr(Box::new(self), Box::new(other))
    }
    pub fn gr_or_eq(self, other: Expr) -> Self {
        Expr::GrOrEq(Box::new(self), Box::new(other))
    }
    pub fn le(self, other: Expr) -> Self {
        Expr::Le(Box::new(self), Box::new(other))
    }
    pub fn le_or_eq(self, other: Expr) -> Self {
        Expr::LeOrEq(Box::new(self), Box::new(other))
    }
    pub fn neg(self) -> Self {
        Expr::Neg(Box::new(self))
    }
    pub fn try_get_type(&self) -> Option<Rc<RefCell<Type>>> {
        match self {
            Expr::Int(i) => Some(Rc::new(RefCell::new(Type::Int(TypeKind::from_kinds(Spanned::new(
                VecType::one(Int::Value(**i)),
                i.span,
            )))))),
            Expr::Object(o) => Some(o.get_type().clone()),
            _ => None,
        }
    }
    pub fn call(self) -> Result<Self, Error> {
        match self {
            Expr::Object(o) => Ok(Expr::Object(Spanned::new(o.call()?, o.span))),
            e => Ok(e)
        }
    }
}

pub fn parse_expr(token: Token, ctx: &Context<'_, AllObject>) -> Result<Expr, Error> {
    match token.ast {
        Ast::Int(i) => Ok(Expr::Int(Spanned::new(i, token.span))),
        Ast::Add(l, r) => Ok(parse_expr(*l, ctx)?.add(parse_expr(*r, ctx)?)),
        Ast::Sub(l, r) => Ok(parse_expr(*l, ctx)?.sub(parse_expr(*r, ctx)?)),
        Ast::Ident(i) => match i.as_ref() {
            "Int" => Ok(Expr::Object(Spanned::new(AllObject::Type(Rc::new(Object {
                name: Spanned::new("".to_owned(), token.span),
                object: Rc::new(RefCell::new(Type::Int(TypeKind::empty(token.span)))),
            })), token.span))),
            "Type" => Ok(Expr::Object(Spanned::new(AllObject::Type(Rc::new(Object {
                name: Spanned::new("".to_owned(), token.span),
                object: Rc::new(RefCell::new(Type::type_type())),
            })), token.span))),
            name => match ctx.find(name) {
                Some(o) => Ok(Expr::Object(o.clone().add_span(token.span))),
                _ => Err(Error::Custom(
                    token.span,
                    format!("{} not found", i),
                    "-here".to_owned(),
                )),
            }
        },
        Ast::CallFunction(func, arg) => {
            let left_expr = parse_expr(*func, ctx)?;
            let arg_expr = parse_expr(*arg, ctx)?;
            match left_expr {
                Expr::Object(o) => Ok(Expr::Object(
                    o.call_with_arg_expr(arg_expr, token.span)?
                        .add_span(token.span),
                )),
                _ => Err(Error::Span(token.span)),
            }
        }
        Ast::Dot(l, r) => {
            let left_expr = parse_expr(*l, ctx)?;
            let right_expr = match r.ast {
                Ast::Ident(i) => i,
                _ => return Err(Error::Span(r.span)),
            };
            let span = token.span;
            match left_expr {
                Expr::Object(o) => o
                    .try_get_member(right_expr.as_str(), span)
                    .map(|o| Spanned::new(o, span))
                    .map(Expr::Object),
                _ => Err(Error::Span(span)),
            }
        }
        Ast::Parenthesis(p) => parse_expr(*p, ctx),
        Ast::Mul(l, r) => Ok(parse_expr(*l, ctx)?.mul(parse_expr(*r, ctx)?)),
        Ast::Div(l, r) => Ok(parse_expr(*l, ctx)?.div(parse_expr(*r, ctx)?)),
        Ast::Pow(l, r) => Ok(parse_expr(*l, ctx)?.pow(parse_expr(*r, ctx)?)),
        Ast::And(l, r) => Ok(parse_expr(*l, ctx)?.and(parse_expr(*r, ctx)?)),
        Ast::Or(l, r) => Ok(parse_expr(*l, ctx)?.or(parse_expr(*r, ctx)?)),
        Ast::Gr(l, r) => Ok(parse_expr(*l, ctx)?.gr(parse_expr(*r, ctx)?)),
        Ast::Le(l, r) => Ok(parse_expr(*l, ctx)?.le(parse_expr(*r, ctx)?)),
        Ast::GrEq(l, r) => Ok(parse_expr(*l, ctx)?.gr_or_eq(parse_expr(*r, ctx)?)),
        Ast::LeEq(l, r) => Ok(parse_expr(*l, ctx)?.le_or_eq(parse_expr(*r, ctx)?)),
        Ast::Eq(l, r) => Ok(parse_expr(*l, ctx)?.eq(parse_expr(*r, ctx)?)),
        Ast::NotEq(l, r) => Ok(parse_expr(*l, ctx)?.not_eq(parse_expr(*r, ctx)?)),
        Ast::Neg(t) => Ok(parse_expr(*t, ctx)?.neg()),
        Ast::Double(_) => unimplemented!(),
        Ast::Val => Err(Error::Span(token.span)),
        Ast::Slice(_) => unimplemented!(),
        Ast::Implication(_, _) => Err(Error::Span(token.span)),
        Ast::Named(_, _) => Err(Error::Span(token.span)),
    }
}
