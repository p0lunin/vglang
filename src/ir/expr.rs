use crate::common::{Context, Error, Span};
use crate::ir::objects::{Object, Var};
use crate::ir::types::base_types::Function;
use crate::ir::types::Type;
use crate::syntax::ast::{Ast, Token};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub ty: Rc<Type>,
    pub span: Span,
    pub kind: ExprKind,
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        Display::fmt(&self.kind, f)
    }
}

impl Expr {
    pub fn new(ty: Rc<Type>, span: Span, kind: ExprKind) -> Self {
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
    Type(Rc<Type>),
    Ident(String),
    Application(Box<Expr>, Box<Expr>),
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

impl Display for ExprKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Ident(i) => Display::fmt(i, f),
            Self::Int(i) => Display::fmt(i, f),
            Self::Add(l, r) => write!(f, "({} + {})", l, r),
            Self::Application(l, r) => write!(f, "({} {})", l, r),
            _ => unimplemented!(),
        }
    }
}

impl Expr {
    pub fn convert_to_type(self) -> Result<Rc<Type>, Error> {
        match self.kind {
            ExprKind::Type(t) => Ok(t),
            ExprKind::Int(_) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Add(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Sub(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Mul(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Div(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Pow(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::And(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Or(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Gr(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Eq(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::NotEq(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::GrOrEq(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Le(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::LeOrEq(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Neg(_) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Ident(_) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Application(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Let {
                var: _,
                assign: _,
                expr: _,
            } => Ok(Rc::new(Type::Expr(self))),
            ExprKind::IfThenElse {
                condition: _,
                then_arm: _,
                else_arm: _,
            } => Ok(Rc::new(Type::Expr(self))),
        }
    }
}

fn op_expr<F: Fn(Box<Expr>, Box<Expr>) -> ExprKind>(
    left: Expr,
    right: Expr,
    func: F,
) -> Result<Expr, String> {
    match (
        left.ty.get_inner_type().deref(),
        right.ty.get_inner_type().deref(),
    ) {
        (Type::Int, Type::Int) => Ok(Expr::new(
            Rc::new(Type::Int),
            left.span.extend(&right.span),
            func(Box::new(left), Box::new(right)),
        )),
        _ => Err(format!(
            "Arithmetic ops only for ints, got {} and {}",
            left.ty, right.ty
        )),
    }
}

// TODO: typeclasses
impl Expr {
    pub fn int(val: i128, span: Span) -> Self {
        Expr::new(Rc::new(Type::Int), span, ExprKind::Int(val))
    }
    pub fn add(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, ExprKind::Add).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn mul(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, ExprKind::Mul).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn sub(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, ExprKind::Sub).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn div(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, ExprKind::Div).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn pow(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, ExprKind::Pow).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn and(self, other: Expr) -> Result<Self, Error> {
        logical_op(self, other, ExprKind::And)
    }
    pub fn or(self, other: Expr) -> Result<Self, Error> {
        logical_op(self, other, ExprKind::Or)
    }
    pub fn eq(self, other: Expr) -> Result<Self, Error> {
        logical_op(self, other, ExprKind::Eq)
    }
    pub fn not_eq(self, other: Expr) -> Result<Self, Error> {
        logical_op(self, other, ExprKind::NotEq)
    }
    pub fn gr(self, other: Expr) -> Result<Self, Error> {
        logical_op(self, other, ExprKind::Gr)
    }
    pub fn gr_or_eq(self, other: Expr) -> Result<Self, Error> {
        logical_op(self, other, ExprKind::GrOrEq)
    }
    pub fn le(self, other: Expr) -> Result<Self, Error> {
        logical_op(self, other, ExprKind::Le)
    }
    pub fn le_or_eq(self, other: Expr) -> Result<Self, Error> {
        logical_op(self, other, ExprKind::LeOrEq)
    }
}

fn logical_op<F>(left: Expr, right: Expr, f: F) -> Result<Expr, Error>
where
    F: Fn(Box<Expr>, Box<Expr>) -> ExprKind,
{
    dbg!(&left);
    match left.ty.deref() {
        Type::Type => Ok(Expr::new(
            Type::typ(),
            left.span.extend(&right.span),
            f(Box::new(left), Box::new(right)),
        )),
        _ => unimplemented!(),
    }
}

fn parse_binary_op<F>(
    l: &Token,
    r: &Token,
    span: Span,
    ctx: &Context<'_, Object>,
    g: &mut HashMap<String, Option<Rc<Type>>>,
    expected: Option<Rc<Type>>,
    f: F,
) -> Result<Option<Expr>, Error>
where
    F: Fn(Expr, Expr) -> Result<Expr, Error>,
{
    let left = parse_expr(l, ctx, g, None)?;
    let right = parse_expr(r, ctx, g, None)?;
    match (left, right) {
        (Some(l), Some(r)) => f(l, r).map(Some),
        (Some(l), None) => unimplemented!(),
        (None, Some(r)) => unimplemented!(),
        (None, None) => {
            let left = parse_expr(l, ctx, g, expected.clone())?
                .ok_or_else(|| Error::cannot_infer_type(span))?;
            let right = parse_expr(r, ctx, g, expected.clone())?
                .ok_or_else(|| Error::cannot_infer_type(span))?;
            f(left, right).map(Some)
        }
    }
}

fn assert_types_equal(this: &Type, expected: Option<Rc<Type>>, span: Span) -> Result<(), Error> {
    match expected {
        Some(t) => match this.is_part_of(&t) {
            true => Ok(()),
            false => Err(Error::Custom(
                span,
                format!("Expected {}, found {} type", t, this),
                "-here".to_owned(),
            )),
        },
        None => Ok(()),
    }
}

fn check_type(e: Expr, expected: Option<Rc<Type>>) -> Result<Option<Expr>, Error> {
    assert_types_equal(&e.ty, expected, e.span)?;
    Ok(Some(e))
}

pub fn parse_type(token: &Token, ctx: &Context<'_, Object>) -> Result<Rc<Type>, Error> {
    parse_expr(token, ctx, &mut HashMap::new(), Some(Type::typ()))
        .and_then(|e| e.ok_or_else(|| Error::cannot_infer_type(token.span)))
        .and_then(|e| e.convert_to_type())
}

pub fn parse_expr(
    token: &Token,
    ctx: &Context<'_, Object>,
    g: &mut HashMap<String, Option<Rc<Type>>>,
    expected: Option<Rc<Type>>,
) -> Result<Option<Expr>, Error> {
    match &token.ast {
        Ast::Int(i) => check_type(Expr::int(*i, token.span), expected),
        Ast::Add(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::add,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::Sub(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::sub,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::Mul(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::mul,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::Div(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::div,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::Pow(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::pow,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::And(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::and,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::Or(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::or,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::Gr(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::gr,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::Le(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::le,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::GrEq(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::gr_or_eq,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::LeEq(l, r) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                Expr::le_or_eq,
            )? {
                Some(e) => check_type(e, expected),
                None => Ok(None),
            }
        }
        Ast::Eq(_, _) => unimplemented!(),
        Ast::NotEq(_, _) => unimplemented!(),
        Ast::Neg(t) => {
            let expr = parse_expr(t, ctx, &mut HashMap::new(), Some(Rc::new(Type::Int)))?; // TODO
            match expr {
                Some(e) => Ok(Some(Expr::new(
                    e.ty.clone(),
                    token.span,
                    ExprKind::Neg(Box::new(e)),
                ))),
                None => unimplemented!(),
            }
        }
        Ast::Parenthesis(t) => parse_expr(t.as_ref(), ctx, g, expected),
        Ast::Double(_) => unimplemented!(),
        Ast::Ident(i) => {
            let expr = match i.as_str() {
                "Int" => Expr::new(
                    Rc::new(Type::Type),
                    token.span,
                    ExprKind::Type(Rc::new(Type::Int)),
                ),
                "Type" => Expr::new(
                    Rc::new(Type::Type),
                    token.span,
                    ExprKind::Type(Rc::new(Type::Type)),
                ),
                _ => match g.get(i) {
                    Some(Some(t)) => Expr::new(t.clone(), token.span, ExprKind::Ident(i.clone())),
                    Some(None) => unreachable!(),
                    None => match ctx.find(i.as_str()) {
                        Some(o) => Expr::new(o.get_type(), token.span, ExprKind::Ident(i.clone())),
                        None => {
                            return Err(Error::Custom(
                                token.span,
                                format!("Cannot find {} in scope", i),
                                "-this".to_string(),
                            ))
                        }
                    },
                },
            };
            check_type(expr, expected)
        }
        Ast::Val => check_type(
            Expr::new(Type::typ(), token.span, ExprKind::Type(Type::unknown())),
            expected,
        ),
        Ast::Slice(_) => unimplemented!(),
        Ast::Implication(l, r) => {
            let exp = Some(Type::typ());
            let left = parse_expr(l, ctx, g, exp.clone())?
                .ok_or_else(|| Error::cannot_infer_type(token.span))?
                .convert_to_type()?;
            let left_types = left.types_in_scope();
            let new_ctx = Context {
                objects: left_types
                    .into_iter()
                    .map(|(name, ty)| Object::Var(Rc::new(Var { name, ty })))
                    .collect(),
                parent: Some(ctx),
            };
            let right = parse_expr(r, &new_ctx, g, exp.clone())?
                .ok_or_else(|| Error::cannot_infer_type(token.span))?;
            let expr = Expr::new(
                Type::typ(),
                token.span,
                ExprKind::Type(Rc::new(Type::Function(Function {
                    get_value: left,
                    return_value: right.convert_to_type()?,
                }))),
            );
            check_type(expr, expected)
        }
        Ast::Named(name, ty) => {
            let ty = parse_expr(ty, ctx, g, None)?.unwrap().convert_to_type()?; // TODO
            Ok(Some(Expr::new(
                Type::typ(),
                token.span,
                ExprKind::Type(Rc::new(Type::Named(name.clone().inner(), ty))),
            )))
        }
        Ast::CallFunction(left, right) => {
            let right = parse_expr(right.as_ref(), ctx, g, None)?;
            match right {
                Some(right) => {
                    let left = parse_expr(
                        left,
                        ctx,
                        g,
                        Some(Rc::new(Type::Function(Function {
                            get_value: right.ty.clone(),
                            return_value: expected.clone().unwrap_or_else(|| Rc::new(Type::Never)),
                        }))),
                    )?;
                    match left {
                        Some(e) => match e.ty.deref() {
                            Type::Function(f) => {
                                let e = Expr::new(
                                    f.return_value.clone(),
                                    token.span,
                                    ExprKind::Application(Box::new(e), Box::new(right)),
                                );
                                check_type(e, expected)
                            }
                            t => Err(Error::Custom(
                                e.span,
                                format!("Expected function, found {}", t),
                                "-here".to_owned(),
                            )),
                        },
                        None => Err(Error::cannot_infer_type(token.span)),
                    }
                }
                None => unimplemented!(),
            }
        }
        Ast::Dot(_, _) => unimplemented!(),
        Ast::Let { var, assign, expr } => {
            let ex = parse_expr(assign.as_ref(), ctx, g, None)?;
            match ex {
                Some(assigned) => {
                    let var = Rc::new(Var {
                        name: (**var).clone(),
                        ty: assigned.ty.clone(),
                    });
                    let ctx = Context {
                        objects: vec![Object::Var(var.clone())],
                        parent: Some(ctx),
                    };
                    let expr_ = parse_expr(expr.as_ref(), &ctx, g, expected.clone())?;
                    match expr_ {
                        Some(e) => {
                            let expr = Expr::new(
                                e.ty.clone(),
                                token.span,
                                ExprKind::Let {
                                    var,
                                    assign: Box::new(assigned),
                                    expr: Box::new(e),
                                },
                            );
                            check_type(expr, expected)
                        }
                        None => Err(Error::cannot_infer_type(expr.span)),
                    }
                }
                None => match g.get(var.as_str()) {
                    Some(Some(_)) | None => {
                        g.insert(var.to_string(), None);
                        let expr_ = match parse_expr(expr.as_ref(), &ctx, g, expected.clone())? {
                            Some(e) => e,
                            None => Err(Error::cannot_infer_type(expr.span))?,
                        };
                        match g.get(var.as_str()) {
                            Some(Some(inferred)) => {
                                let var = Rc::new(Var {
                                    name: (**var).clone(),
                                    ty: inferred.clone(),
                                });
                                match parse_expr(assign.as_ref(), ctx, g, Some(inferred.clone()))? {
                                    Some(assigned) => {
                                        let expr = Expr::new(
                                            expr_.ty.clone(),
                                            token.span,
                                            ExprKind::Let {
                                                var,
                                                assign: Box::new(assigned),
                                                expr: Box::new(expr_),
                                            },
                                        );
                                        check_type(expr, expected)
                                    }
                                    None => Err(Error::cannot_infer_type(assign.span)),
                                }
                            }
                            Some(None) => Err(Error::cannot_infer_type(assign.span)),
                            None => unreachable!(),
                        }
                    }
                    Some(None) => Err(Error::cannot_infer_type(var.span)),
                },
            }
        }
        Ast::IfThenElse {
            if_: _,
            then: _,
            else_: _,
        } => unimplemented!(),
    }
}
