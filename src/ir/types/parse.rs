use crate::syntax::ast::{Token, Ast};
use crate::common::{Span, Spanned, VecType, Error, Context, SpannedError};
use std::convert::TryFrom;
use crate::ir::types::base_types::{OneRangeIntBound, Int, Slice};
use crate::ir::types::{Type, TypeKind};
use std::rc::Rc;
use std::cell::RefCell;
use crate::ir::expr::parse_expr;
use crate::syntax::ast;
use crate::ir::objects::AllObject;

pub fn parse_type(
    type_def: Spanned<ast::Type>,
    ctx: &Context<'_, AllObject>,
) -> Result<Rc<RefCell<Type>>, Error> {
    let ast::Type(name, def) = type_def.inner();
    let mut ty = parse_type_helper(def, ctx)?;
    ty.set_name(name);
    Ok(Rc::new(RefCell::new(ty)))
}

pub fn parse_type_helper(token: Token, ctx: &Context<'_, AllObject>) -> Result<Type, Error> {
    let span = token.span;
    match token.ast {
        Ast::And(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.and(right).spanned_err(span))
        }),
        Ast::Or(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.or(right).spanned_err(span))
        }),
        Ast::Add(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.add(right).spanned_err(span))
        }),
        Ast::Sub(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.sub(right).spanned_err(span))
        }),
        Ast::Neg(t) => parse_type_helper(*t, ctx).and_then(|left| left.neg().spanned_err(span)),
        Ast::Int(i) => Ok(Type::Int(TypeKind {
            name: None,
            kinds: Spanned::new(VecType::one(Int::Value(i)), token.span),
        })),
        Ast::Parenthesis(t) => {
            parse_type_helper(*t, ctx).map(|ty| Type::ParenthesisType(Rc::new(RefCell::new(ty))))
        }
        Ast::Gr(l, r) => match ((*l), (*r)) {
            (Token { ast: Ast::Val, .. }, a) => get_arithmetic_val(&a)
                .map(|value| one_bound(OneRangeIntBound::Low(value + 1), span)),
            (a, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&a)
                .map(|value| one_bound(OneRangeIntBound::High(value - 1), span)),
            _ => Err(Error::Span(token.span)),
        },
        Ast::Le(l, r) => match ((*l), (*r)) {
            (Token { ast: Ast::Val, .. }, a) => get_arithmetic_val(&a)
                .map(|value| one_bound(OneRangeIntBound::High(value - 1), span)),
            (a, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&a)
                .map(|value| one_bound(OneRangeIntBound::Low(value + 1), span)),
            _ => Err(Error::Span(token.span)),
        },
        Ast::GrEq(l, r) => {
            match ((*l), (*r)) {
                (Token { ast: Ast::Val, .. }, a) => get_arithmetic_val(&a)
                    .map(|value| one_bound(OneRangeIntBound::Low(value), span)),
                (a, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&a)
                    .map(|value| one_bound(OneRangeIntBound::High(value), span)),
                _ => Err(Error::Span(token.span)),
            }
        }
        Ast::LeEq(l, r) => {
            match ((*l), (*r)) {
                (Token { ast: Ast::Val, .. }, a) => get_arithmetic_val(&a)
                    .map(|value| one_bound(OneRangeIntBound::High(value), span)),
                (a, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&a)
                    .map(|value| one_bound(OneRangeIntBound::Low(value), span)),
                _ => Err(Error::Span(token.span)),
            }
        }
        Ast::Eq(l, r) => match (*l, *r) {
            (Token { ast: Ast::Val, .. }, t) => Ok(Type::Int(TypeKind {
                name: None,
                kinds: Spanned::new(
                    VecType::one(Int::Value(get_arithmetic_val(&t)?)),
                    token.span,
                ),
            })),
            (t, Token { ast: Ast::Val, .. }) => Ok(Type::Int(TypeKind {
                name: None,
                kinds: Spanned::new(
                    VecType::one(Int::Value(get_arithmetic_val(&t)?)),
                    token.span,
                ),
            })),
            _ => Err(Error::Span(token.span)),
        },
        Ast::NotEq(l, r) => match (*l, *r) {
            (Token { ast: Ast::Val, .. }, t) => get_arithmetic_val(&t)
                .map(|value| one_bound(OneRangeIntBound::NotEqual(value), span)),
            (t, Token { ast: Ast::Val, .. }) => get_arithmetic_val(&t)
                .map(|value| one_bound(OneRangeIntBound::NotEqual(value), span)),
            _ => Err(Error::Span(token.span)),
        },
        Ast::Slice(s) => {
            let from = *s.first;
            let step = *s.second - *s.first;
            let to = *s.last;
            Ok(match step {
                1 => Type::Int(TypeKind::from_kinds(Spanned::new(
                    VecType::one(Int::KnownBound {
                        low: from,
                        high: to,
                    }),
                    span,
                ))),
                _ => Type::Int(TypeKind::from_kinds(Spanned::new(
                    VecType::one(Int::Slice(Slice { from, step, to })),
                    span,
                ))),
            })
        }
        Ast::Implication(l, r) => parse_type_helper(*l, ctx).and_then(|left| {
            parse_type_helper(*r, ctx).and_then(|right| left.op_implication(right))
        }),
        Ast::Named(name, def) => Ok(Type::Named(
            name.clone(),
            Rc::new(RefCell::new(parse_type_helper(*def, ctx)?)),
        )),
        _ => parse_expr(token, ctx).and_then(
            |e| e.call().and_then(
                |e| e.try_get_type().map(|t|
                    Type::AnotherType(Spanned::new(t, span))
                ).ok_or(Error::Span(span))
            )),
    }
}

fn one_bound(bound: OneRangeIntBound, span: Span) -> Type {
    Type::Int(TypeKind::from_kinds(Spanned::new(
        VecType::one(Int::Bound(bound)),
        span,
    )))
}

fn get_arithmetic_val(token: &Token) -> Result<i128, Error> {
    token.eval_arithmetic().map_err(|s| Error::Span(s))
}

impl Token {
    pub fn eval_arithmetic(&self) -> Result<i128, Span> {
        match &self.ast {
            Ast::Int(i) => Ok(*i),
            Ast::Add(l, r) => l
                .eval_arithmetic()
                .and_then(|res1| r.eval_arithmetic().and_then(|res2| Ok(res1 + res2))),
            Ast::Sub(l, r) => l
                .eval_arithmetic()
                .and_then(|res1| r.eval_arithmetic().and_then(|res2| Ok(res1 - res2))),
            Ast::Mul(l, r) => l
                .eval_arithmetic()
                .and_then(|res1| r.eval_arithmetic().and_then(|res2| Ok(res1 * res2))),
            Ast::Div(l, r) => l
                .eval_arithmetic()
                .and_then(|res1| r.eval_arithmetic().and_then(|res2| Ok(res1 / res2))),
            Ast::Pow(l, r) => l.eval_arithmetic().and_then(|res1| {
                r.eval_arithmetic()
                    .and_then(|res2| match u32::try_from(res2) {
                        Ok(n) => Ok(res1.pow(n)),
                        Err(_) => Err(r.span),
                    })
            }),
            Ast::Neg(t) => Ok(-t.eval_arithmetic()?),
            _ => Err(self.span),
        }
    }
}
