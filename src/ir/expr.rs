use crate::common::{Context, Error, Searchable, SearchableByPath, Span, Spanned, BinOp};
use crate::ir::objects::{DataVariant, Object, Var};
use crate::ir::types::base_types::Function;
use crate::ir::types::{Type, Generic, Concrete};
use crate::syntax::ast::{Ast, Pattern, Token};
use crate::{ir, syntax, Implementations, Interpreter};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use crate::interpreter::ByteCode;

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
    BinOp(Box<Expr>, Box<Expr>, BinOp),
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
            Self::BinOp(l, r, op) => write!(f, "({} {} {})", l, op, r),
            Self::Application(l, r) => write!(f, "({} {})", l, r),
            Self::Type(ty) => write!(f, "{}", ty),
            x => {
                dbg!(x);
                unimplemented!()
            }
        }
    }
}
/*
impl Expr {
    pub fn convert_to_type(self, ctx: &Context<'_, Object>) -> Result<Rc<Type>, Error> {
        match self.kind {
            ExprKind::Type(t) => Ok(t),
            ExprKind::Int(_) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::BinOp(_, _, _) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Neg(_) => Ok(Rc::new(Type::Expr(self))),
            ExprKind::Ident(i) => match ctx.find(i.as_str()).unwrap() {
                Object::Type(t) => Ok(t.def.clone()),
                Object::Enum(e) => unimplemented!(),
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
*/
fn op_expr(
    left: Expr,
    right: Expr,
    op: BinOp,
) -> Result<Expr, String> {
    match (
        left.ty.get_inner_type().deref(),
        right.ty.get_inner_type().deref(),
    ) {
        (Type::Int, Type::Int) => Ok(Expr::new(
            Rc::new(Type::Int),
            left.span.extend(&right.span),
            ExprKind::BinOp(Box::new(left), Box::new(right), op),
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
    pub fn bin_op(self, other: Expr, op: BinOp, bool_ty: Rc<Type>) -> Result<Self, Error> {
        match op {
            BinOp::Add => self.add(other),
            BinOp::Mul => self.mul(other),
            BinOp::Sub => self.sub(other),
            BinOp::Div => self.div(other),
            BinOp::Pow => self.pow(other),
            BinOp::And => self.and(other, bool_ty),
            BinOp::Or => self.or(other, bool_ty),
            BinOp::Eq => self.eq(other, bool_ty),
            BinOp::NotEq => self.not_eq(other, bool_ty),
            BinOp::Gr => self.gr(other, bool_ty),
            BinOp::GrOrEq => self.gr_or_eq(other, bool_ty),
            BinOp::Le => self.le(other, bool_ty),
            BinOp::LeOrEq => self.le_or_eq(other, bool_ty),
        }
    }
    pub fn add(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, BinOp::Add).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn mul(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, BinOp::Mul).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn sub(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, BinOp::Sub).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn div(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, BinOp::Div).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn pow(self, other: Expr) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        op_expr(self, other, BinOp::Pow).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn and(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        logical_op(self, other, bool_ty, BinOp::And)
    }
    pub fn or(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        logical_op(self, other, bool_ty, BinOp::Or)
    }
    pub fn eq(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, BinOp::Eq).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn not_eq(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, BinOp::NotEq).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn gr(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, BinOp::Gr).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn gr_or_eq(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, BinOp::GrOrEq).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn le(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, BinOp::Le).map_err(|s| Error::Custom(span, s, "-here".to_string()))
    }
    pub fn le_or_eq(self, other: Expr, bool_ty: Rc<Type>) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        compare_op(self, other, bool_ty, BinOp::LeOrEq).map_err(|s| Error::Custom(span, s, "-here".to_string()))
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

fn logical_op(left: Expr, right: Expr, _bool_ty: Rc<Type>, op: BinOp) -> Result<Expr, Error> {
    match left.ty.deref() {
        Type::Type => Ok(Expr::new(
            Type::typ(),
            left.span.extend(&right.span),
            ExprKind::BinOp(Box::new(left), Box::new(right), op),
        )),
        _ => unimplemented!(),
    }
}

fn compare_op(left: Expr, right: Expr, bool_ty: Rc<Type>, op: BinOp) -> Result<Expr, String> {
    match left.ty.deref() {
        Type::Type => return Ok(Expr::new(
            Type::typ(),
            left.span.extend(&right.span),
            ExprKind::BinOp(Box::new(left), Box::new(right), op),
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
            ExprKind::BinOp(Box::new(left), Box::new(right), op),
        )),
        _ => Err(format!(
            "Compare ops only for ints, got {} and {}",
            left.ty, right.ty
        )),
    }
}

fn parse_binary_op(
    l: &Token,
    r: &Token,
    span: Span,
    ctx: &Context<'_, Object>,
    g: &mut HashMap<String, Option<Rc<Type>>>,
    expected: Option<Rc<Type>>,
    op: BinOp,
) -> Result<Option<Expr>, Error>
{
    let left = parse_expr(l, ctx, g, None)?;
    let right = parse_expr(r, ctx, g, None)?;
    let (l, r) = match (left, right) {
        (Some(l), Some(r)) => (l, r),
        (Some(_l), None) => unimplemented!(),
        (None, Some(_r)) => unimplemented!(),
        (None, None) => {
            let left = parse_expr(l, ctx, g, expected.clone())?
                .ok_or_else(|| Error::cannot_infer_type(span))?;
            let right = parse_expr(r, ctx, g, expected.clone())?
                .ok_or_else(|| Error::cannot_infer_type(span))?;
            (left, right)
        }
    };
    Expr::bin_op(l, r, op, ctx.find_bool()).map(Some)
}

fn assert_types_equal(this: &Type, expected: Option<Rc<Type>>, span: Span) -> Result<(), Error> {
    match expected {
        Some(t) => {
            match this.is_part_of(&t) {
                true => Ok(()),
                false => Err(Error::Custom(
                    span,
                    format!("Expected {}, found {} type", t, this),
                    "-here".to_owned(),
                )),
            }
        },
        None => Ok(()),
    }
}

fn check_type(mut e: Expr, expected: Option<Rc<Type>>, ctx: &Context<Object>) -> Result<Option<Expr>, Error> {
    e.ty = interpret_ty(e.ty.clone(), ctx, &Implementations::new())?;
    assert_types_equal(&e.ty, expected, e.span)?;
    Ok(Some(e))
}

pub fn parse_type(token: &Token, ctx: &Context<'_, Object>, impls: &Implementations) -> Result<Rc<Type>, Error> {
    parse_expr(token, ctx, &mut HashMap::new(), Some(Type::typ()))
        .and_then(|e| e.ok_or_else(|| Error::cannot_infer_type(token.span)))
        .and_then(|e| {
            interpret_expr_as_ty(e, ctx, impls)
        })
}

pub fn interpret_expr_as_ty(e: Expr, ctx: &Context<Object>, impls: &Implementations) -> Result<Rc<Type>, Error> {
    let interpreter = Interpreter::new(&impls.functions, ctx);
    let e2 = interpreter.eval(&e, &Context::new())?;
    match e2 {
        ByteCode::Type(ty) => Ok(ty.clone()),
        ByteCode::DataType(ty) => Ok(Rc::new(Type::Data(ty))),
        _ => unimplemented!()
    }
    /*match e.kind {
        ExprKind::Application(_, _) => {
            let interpreter = Interpreter::new(&impls.functions, ctx);
            let e2 = interpreter.eval(&e, &Context::new())?;
            match e2 {
                ByteCode::Type(ty) => Ok(ty.clone()),
                _ => unimplemented!()
            }
        }
        ExprKind::Type(ty) => {
            interpret_ty(ty, ctx, impls)
        }
        _ => e.convert_to_type(ctx)
    }*/
}

fn interpret_ty(ty: Rc<Type>, ctx: &Context<Object>, impls: &Implementations) -> Result<Rc<Type>, Error> {
    match ty.as_ref() {
        Type::Expr(ex) => interpret_expr_as_ty(ex.clone(), ctx, impls),
        Type::Function(f) => interpret_func_ty(f, ctx, impls),
        _ => Ok(ty.clone())
    }
}

fn interpret_func_ty(f: &Function, ctx: &Context<Object>, impls: &Implementations) -> Result<Rc<Type>, Error> {
    let get_value = match f.get_value.as_ref() {
        Type::Function(f2) => interpret_func_ty(f2, ctx, impls)?,
        Type::Expr(ex) => interpret_expr_as_ty(ex.clone(), ctx, impls)?,
        _ => f.get_value.clone(),
    };
    let return_value = match f.return_value.as_ref() {
        Type::Function(f2) => interpret_func_ty(f2, ctx, impls)?,
        Type::Expr(ex) => interpret_expr_as_ty(ex.clone(), ctx, impls)?,
        _ => f.return_value.clone(),
    };
    Ok(Rc::new(Type::Function(Function {
        get_value,
        return_value
    })))
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
        Ast::Int(i) => check_type(Expr::int(*i, token.span), expected, ctx),
        Ast::BinOp(l, r, op) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                token.span,
                ctx,
                g,
                expected.clone(),
                op.clone(),
            )? {
                Some(e) => {
                    check_type(e, expected, ctx)
                },
                None => Ok(None),
            }
        }
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
                        Some(o) => {
                            Expr::new(o.get_type(), token.span, ExprKind::Ident(i.clone()))
                        },
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
            let expr = infer(expr, expected.clone());
            check_type(expr, expected, ctx)
        }
        Ast::Val => {
            todo!();
            check_type(
                Expr::new(Type::typ(), token.span, ExprKind::Type(Type::unknown())),
                expected,
                ctx
            )
        },
        Ast::Slice(_) => unimplemented!(),
        Ast::Implication(l, r) => {
            let exp = Some(Type::typ());
            let left = parse_expr(l, ctx, g, exp.clone())?
                .ok_or_else(|| Error::cannot_infer_type(token.span))?;
            /*let left_types = left.types_in_scope();
            let new_ctx = Context {
                objects: left_types
                    .into_iter()
                    .map(|(name, ty)| Object::Var(Rc::new(Var { name, ty })))
                    .collect(),
                parent: Some(ctx),
            };*/
            let right = parse_expr(r, ctx, g, exp.clone())?
                .ok_or_else(|| Error::cannot_infer_type(token.span))?;
            let expr = Expr::new(
                Type::typ(),
                token.span,
                ExprKind::Type(Rc::new(Type::Function(Function {
                    get_value: Rc::new(Type::Expr(left)),
                    return_value: Rc::new(Type::Expr(right))
                }))),
            );
            check_type(expr, expected, ctx)
        }
        Ast::Named(name, ty) => {
            /*let ty = parse_expr(ty, ctx, g, None)?
                .unwrap()
                .convert_to_type(ctx)?; // TODO
            Ok(Some(Expr::new(
                Type::typ(),
                token.span,
                ExprKind::Type(Rc::new(Type::Named(name.clone().inner(), ty))),
            )))*/
            unimplemented!()
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
                            return_value: expected.clone().unwrap_or_else(|| Rc::new(Type::Unknown(None))),
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
                                check_type(e, expected, ctx)
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
                let ty = infer(expr, expected);
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

fn infer(mut expr: Expr, expected: Option<Rc<Type>>) -> Expr {
    let expected = match expected {
        Some(ex) => ex,
        None => return expr,
    };
    expr.ty = clarify_generics(expr.ty.clone(), expected.clone());
    expr.ty = infer_ty(expr.ty.clone(), expected);
    expr
}

fn infer_ty(ty: Rc<Type>, expected: Rc<Type>) -> Rc<Type> {
    match (ty.as_ref(), expected.as_ref()) {
        (Type::Unknown(_), _) => expected.clone(),
        (Type::Function(f), Type::Function(ef)) => {
            Rc::new(Type::Function(Function {
                get_value: infer_ty(f.get_value.clone(), ef.get_value.clone()),
                return_value: infer_ty(f.return_value.clone(), ef.return_value.clone()),
            }))
        },
        (Type::Data(d), Type::Data(ed)) => {
            Rc::new(Type::Data(Concrete::new(
                d.base.clone(),
                d.generics.iter().zip(ed.generics.iter()).map(|(ty, ety)| {
                    match ty.as_ref() {
                        Type::Unknown(_) => ety.clone(),
                        _ => ty.clone()
                    }
                }).collect()
            )))
        },
        _ => ty.clone(),
    }
}

fn clarify_generics(mut ty: Rc<Type>, expected: Rc<Type>) -> Rc<Type> {
    let gens = {
        let mut gens = HashMap::new();
        generics_map(ty.clone(), expected, &mut gens);
        gens
    };
    for (gen, gen_ty) in gens {
        ty = ty.update_set_generic_func(&gen, &gen_ty);
    }
    ty
}

fn generics_map(ty: Rc<Type>, expected: Rc<Type>, gens: &mut HashMap<String, Rc<Type>>) {
    match (ty.as_ref(), expected.as_ref()) {
        (Type::Unknown(Some(g)), _) => {
            if !gens.contains_key(g.name.as_ref()) {
                gens.insert(g.name.as_ref().clone(), expected.clone());
            }
        },
        (Type::Generic(g), _) => {
            if !gens.contains_key(g.name.as_ref()) {
                gens.insert(g.name.as_ref().clone(), expected.clone());
            }
        },
        (Type::Function(f), Type::Function(ef)) => {
            generics_map(f.get_value.clone(), ef.get_value.clone(), gens);
            generics_map(f.return_value.clone(), ef.return_value.clone(), gens);
        },
        (Type::Data(d), Type::Data(ed)) => {
            d.generics.iter().zip(ed.generics.iter()).for_each(|(g, ge)| {
                generics_map(g.clone(), ge.clone(), gens)
            })
        }
        _ => { },
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
                    check_type(expr, expected_type, &ctx)
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
                                check_type(expr, expected_type, ctx)
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
    //dbg!(pattern);
    //dbg!(&pattern_type);
    let mut objects = vec![];
    match pattern.deref() {
        Pattern::Otherwise => {}
        Pattern::Bind(s, b) => {
            objects.push(Object::var(s.as_ref().clone(), pattern_type.clone()));
            objects.append(&mut create_scope_objects(b.as_ref(), &top, pattern_type)?);
        }
        Pattern::Variant(path, pats) => match top.find_by_path(path) {
            Some(Object::EnumVariant(v)) => {
                let v = {
                    let concrete = match pattern_type.as_ref() {
                        Type::Data(d) => d,
                        x => {
                            unreachable!()
                        }
                    };
                    let mut v = (*v).clone();
                    for (gen, gen_ty) in concrete.base.generics.iter().zip(concrete.generics.iter()) {
                        v = v.update_generic(
                            gen,
                            gen_ty
                        );
                    }
                    v
                };
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
