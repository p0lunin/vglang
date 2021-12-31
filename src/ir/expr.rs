use crate::common::{Context, Error, Searchable, SearchableByPath, Span, Spanned};
use crate::ir::objects::{DataVariant, Object, Var};
use crate::ir::types::base_types::Function;
use crate::ir::types::Type;
use crate::syntax::ast::{Ast, Pattern, Token};
use crate::{ir, syntax};
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
    Dot(Box<Expr>, Box<Expr>),
    Type(Rc<Type>),
    Ident(String),
    DataVariant(Rc<DataVariant>),
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
    CaseExpr {
        cond: Box<Expr>,
        arms: Vec<(Spanned<ir::patmat::Pattern>, Expr)>,
    },
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Ident(i) => Display::fmt(i, f),
            Self::Int(i) => Display::fmt(i, f),
            Self::Add(l, r) => write!(f, "({} + {})", l, r),
            Self::Application(l, r) => write!(f, "({} {})", l, r),
            Self::Type(ty) => write!(f, "{}", ty),
            x => {
                dbg!(x);
                unimplemented!()
            }
        }
    }
}

impl Expr {
    pub fn convert_to_type(self, ctx: &Context<'_, Object>) -> Result<Rc<Type>, Error> {
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
            ExprKind::Ident(i) => match ctx.find(i.as_str()).unwrap() {
                Object::Type(t) => Ok(t.def.clone()),
                Object::Enum(e) => Ok(Rc::new(Type::Data(e.ty.clone()))),
                _ => unimplemented!(),
            },
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
            ExprKind::CaseExpr { cond: _, arms: _ } => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Dot(_, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::DataVariant(_) => Ok(Rc::new(Type::Expr(self))),
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
    pub fn and(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        logical_op(self, other, bool_ty, ExprKind::And)
    }
    pub fn or(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        logical_op(self, other, bool_ty, ExprKind::Or)
    }
    pub fn eq(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, ExprKind::Eq).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn not_eq(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, ExprKind::NotEq).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn gr(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, ExprKind::Gr).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn gr_or_eq(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, ExprKind::GrOrEq).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn le(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, ExprKind::Le).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn le_or_eq(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, ExprKind::LeOrEq).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn dot(self, name: String, ctx: &Context<'_, Object>, span: Span) -> Result<Self, Error> {
        match self.kind {
            ExprKind::Ident(i) => {
                let o = ctx.find(i.as_str());
                match o {
                    Some(o) => match o {
                        Object::Enum(e) => {
                            let var = e.get_field(&name).ok_or_else(|| unimplemented!())?;
                            Ok(Expr::new(var.get_type(), span, ExprKind::DataVariant(var)))
                        }
                        _ => unimplemented!(),
                    },
                    None => unreachable!(),
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn logical_op<F>(left: Expr, right: Expr, bool_ty: Rc<Type>, f: F) -> Result<Expr, Error>
where
    F: Fn(Box<Expr>, Box<Expr>) -> ExprKind,
{
    match left.ty.deref() {
        Type::Type => Ok(Expr::new(
            Type::typ(),
            left.span.extend(&right.span),
            f(Box::new(left), Box::new(right)),
        )),
        _ => unimplemented!(),
    }
}

fn compare_op<F>(left: Expr, right: Expr, bool_ty: Rc<Type>, f: F) -> Result<Expr, String>
where
    F: Fn(Box<Expr>, Box<Expr>) -> ExprKind,
{
    match left.ty.deref() {
        Type::Type => return Ok(Expr::new(
            Type::typ(),
            left.span.extend(&right.span),
            f(Box::new(left), Box::new(right)),
        )),
        _ => { },
    }

    match (
        left.ty.get_inner_type().deref(),
        right.ty.get_inner_type().deref(),
    ) {
        (Type::Int, Type::Int) => Ok(Expr::new(
            bool_ty,
            left.span.extend(&right.span),
            f(Box::new(left), Box::new(right)),
        )),
        _ => Err(format!(
            "Compare ops only for ints, got {} and {}",
            left.ty, right.ty
        )),
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
        (Some(_l), None) => unimplemented!(),
        (None, Some(_r)) => unimplemented!(),
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
        .and_then(|e| e.convert_to_type(ctx))
}

/// Parse token tree into expression.
///
/// Returns `Some` if:
/// 1. `expected` is `None` and can infer type.
/// 2. `expected` is `Some(T)` and `typeof(expr) == T`.
///
/// Returns `None` if:
/// 1. `expected` is `None` and cannot infer type.
/// 2. `expected` is `Some(T)` and `typeof(expr) != T`.
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
                |x, y| Expr::and(x, y, ctx.find_ty("Bool").unwrap()),
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
                |x, y| Expr::or(x, y, ctx.find_ty("Bool").unwrap()),
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
                |x, y| Expr::gr(x, y, ctx.find_ty("Bool").unwrap()),
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
                |x, y| Expr::le(x, y, ctx.find_ty("Bool").unwrap()),
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
                |x, y| Expr::gr_or_eq(x, y, ctx.find_ty("Bool").unwrap()),
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
                |x, y| Expr::le_or_eq(x, y, ctx.find_ty("Bool").unwrap()),
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
            let expr = monomorphize(expr, expected.clone());
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
                .convert_to_type(ctx)?;
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
                    return_value: right.convert_to_type(ctx)?,
                }))),
            );
            check_type(expr, expected)
        }
        Ast::Named(name, ty) => {
            let ty = parse_expr(ty, ctx, g, None)?
                .unwrap()
                .convert_to_type(ctx)?; // TODO
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
        Ast::Dot(l, r) => match &r.ast {
            Ast::Ident(i) => {
                let left = parse_expr(l, ctx, g, None)?.unwrap();
                let expr = left.dot(i.clone(), ctx, token.span)?;
                let ty = monomorphize(expr, expected);
                Ok(Some(ty))
            }
            _ => unimplemented!(),
        },
        Ast::Let { var, assign, expr } => {
            parse_let_expr(var, assign, expr, token.span, ctx, g, expected)
        }
        Ast::IfThenElse {
            if_: _,
            then: _,
            else_: _,
        } => unimplemented!(),
        Ast::CaseExpr(e) => {
            let syntax::ast::CaseExpr { cond, arms } = e.as_ref();

            let cond = parse_expr(cond, ctx, g, None)?
                // TODO: we can infer type using match patterns.
                .ok_or_else(|| Error::cannot_infer_type(token.span))?;

            let arms: Vec<(Spanned<ir::patmat::Pattern>, Expr)> = arms
                .iter()
                .map(|arm| {
                    let scope = create_scope(&arm.pat, ctx, cond.ty.clone())?;
                    Ok((
                        ir::patmat::Pattern::parse(arm.pat.clone()),
                        parse_expr(&arm.arm, &scope, g, expected.clone())?
                            .ok_or_else(|| Error::cannot_infer_type(token.span))?,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let ty = arms
                .first()
                .map(|arm| arm.1.ty.clone())
                .unwrap_or_else(|| Rc::new(Type::Never));
            arms.iter()
                .map(|arm| match arm.1.ty == ty {
                    true => Ok(()),
                    false => Err(Error::custom(
                        token.span,
                        format!("Expected type {} but found {}", ty, arm.1.ty),
                    )),
                })
                .collect::<Result<Vec<()>, _>>()?;
            Ok(Some(Expr::new(
                ty,
                token.span,
                ExprKind::CaseExpr {
                    cond: Box::new(cond),
                    arms,
                },
            )))
        }
    }
}

fn monomorphize(mut expr: Expr, expected: Option<Rc<Type>>) -> Expr {
    let expected = match expected {
        Some(ex) => ex,
        None => return expr,
    };
    expr.ty = monomorphize_ty(expr.ty.clone(), expected);
    expr
}

fn monomorphize_ty(ty: Rc<Type>, expected: Rc<Type>) -> Rc<Type> {
    match (ty.as_ref(), expected.as_ref()) {
        (Type::Function(f), Type::Function(ef)) => match f.get_value.as_ref() {
            Type::Generic(g) => {
                return ty.clone().update_set_generic_func(g, &ef.get_value);
            }
            _ => ty.clone(),
        },
        _ => ty.clone(),
    }
}

fn parse_let_expr(
    var: &Spanned<String>,
    assign: &Token,
    expr_token: &Token,
    token_span: Span,
    ctx: &Context<'_, Object>,
    g: &mut HashMap<String, Option<Rc<Type>>>,
    expected_type: Option<Rc<Type>>,
) -> Result<Option<Expr>, Error> {
    let ex = parse_expr(assign, ctx, g, None)?;
    match ex {
        // if `ex` is Some, it means `ex` have some inferred type.
        Some(assign_expr) => {
            let var = Rc::new(Var {
                name: var.as_ref().clone(),
                ty: assign_expr.ty.clone(),
            });
            let ctx = Context {
                objects: vec![Object::Var(var.clone())],
                parent: Some(ctx),
            };
            let expr = parse_expr(expr_token, &ctx, g, expected_type.clone())?;
            match expr {
                Some(e) => {
                    let expr = Expr::new(
                        e.ty.clone(),
                        token_span,
                        ExprKind::Let {
                            var,
                            assign: Box::new(assign_expr),
                            expr: Box::new(e),
                        },
                    );
                    check_type(expr, expected_type)
                }
                None => Err(Error::cannot_infer_type(expr_token.span)),
            }
        }
        None => match g.get(var.as_str()) {
            Some(Some(_)) | None => {
                // If we cannot infer type of var, let's try to parse `in` expressions,
                // and if `g` contains inferred type, it means we can infer it, otherwise
                // we cannot infer variable type.
                g.insert(var.to_string(), None);
                let expr = match parse_expr(expr_token, &ctx, g, expected_type.clone())? {
                    Some(e) => e,
                    None => Err(Error::cannot_infer_type(expr_token.span))?,
                };
                match g.get(var.as_str()) {
                    Some(Some(inferred)) => {
                        let var = Rc::new(Var {
                            name: var.as_ref().clone(),
                            ty: inferred.clone(),
                        });
                        let inferred_clone = inferred.clone();
                        match parse_expr(assign, ctx, g, Some(inferred_clone))? {
                            Some(assigned) => {
                                let expr = Expr::new(
                                    expr.ty.clone(),
                                    token_span,
                                    ExprKind::Let {
                                        var,
                                        assign: Box::new(assigned),
                                        expr: Box::new(expr),
                                    },
                                );
                                check_type(expr, expected_type)
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

// Function creates scope of variables for the pattern recursively.
fn create_scope<'a>(
    pattern: &'a Spanned<Pattern>,
    top: &'a Context<'a, Object>,
    pattern_type: Rc<Type>,
) -> Result<Context<'a, Object>, Error> {
    let ctx = Context {
        objects: create_scope_objects(pattern, top, pattern_type)?,
        parent: Some(top),
    };
    Ok(ctx)
}

fn create_scope_objects<'a>(
    pattern: &'a Spanned<Pattern>,
    top: &'a Context<'a, Object>,
    pattern_type: Rc<Type>,
) -> Result<Vec<Object>, Error> {
    let mut objects = vec![];
    match pattern.deref() {
        Pattern::Otherwise => {}
        Pattern::Bind(s, b) => {
            objects.push(Object::var(s.as_ref().clone(), pattern_type.clone()));
            objects.append(&mut create_scope_objects(b.as_ref(), &top, pattern_type)?);
        }
        Pattern::Variant(path, pats) => match top.find_by_path(path) {
            Some(Object::EnumVariant(v)) => {
                match v.data.len() == pats.len() {
                    true => pats.iter().zip(v.data.iter()).for_each(|(pat, ty)| {
                        match create_scope_objects(pat, &top, ty.clone()) {
                            Ok(mut res) => {
                                // https://github.com/rust-lang/rust/issues/59159
                                let objs = &mut res;
                                objects.append(objs)
                            }
                            _ => {}
                        }
                    }),
                    false => {
                        return Err(Error::custom(
                            pattern.span,
                            format!("Expected {} patterns, found {}", v.data.len(), pats.len()),
                        ))
                    }
                }
            }
            Some(_) => unimplemented!(),
            None => return Err(Error::custom(pattern.span, format!("{} not found", path))),
        },
        Pattern::Ident(s) => objects.push(Object::var(s.clone(), pattern_type.clone())),
    };
    Ok(objects)
}
