use crate::common::{LocalContext, Error, Searchable, Span, Spanned, BinOp, DisplayScope, Find, PathToFind};
use crate::ir::objects::{Object, Var, DataDef};
use crate::ir::types::base_types::Function;
use crate::ir::types::{Type, Concrete};
use crate::syntax::ast::{Ast, Pattern, Token};
use crate::{ir, syntax, Interpreter};
use std::collections::HashMap;
use std::fmt::{Write};
use std::ops::Deref;
use crate::interpreter::ByteCode;
use crate::arena::Id;
use crate::common::global_context::{ScopeCtx, ScopeCtxInner};
use smallvec::smallvec;

#[derive(Debug, Clone)]
pub struct Expr {
    pub ty: Id<Type>,
    pub span: Span,
    pub kind: ExprKind,
}

impl<'a> DisplayScope<'a> for Expr {
    type Scope = ScopeCtx<'a>;

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        DisplayScope::display_value(&self.kind, f, scope)
    }
}

impl Expr {
    pub fn new(ty: Id<Type>, span: Span, kind: ExprKind) -> Self {
        Expr { ty, span, kind }
    }
    // has some useful cases
    pub fn empty() -> Self {
        Expr::new(
            // SAFETY: Id has #[repr(transparent)]
            unsafe { std::mem::transmute(0_usize) },
            Span::new(0, 0),
            ExprKind::Int(0),
        )
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i128),
    BinOp(Box<Expr>, Box<Expr>, BinOp),
    Neg(Box<Expr>),
    Dot(Box<Expr>, Box<Expr>),
    Type(Id<Type>),
    Ident(String),
    DataVariant(Id<DataDef>, usize),
    Application(Box<Expr>, Box<Expr>),
    Let {
        var: Var,
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

impl<'a> DisplayScope<'a> for ExprKind {
    type Scope = ScopeCtx<'a>;

    fn display_value(&self, f: &mut impl Write, scope: &Self::Scope) -> std::fmt::Result {
        match self {
            Self::Ident(i) => f.write_str(i.as_str()),
            Self::Int(i) => f.write_fmt(format_args!("{}", i)),
            Self::BinOp(l, r, op) => {
                l.kind.display_value(f, scope)?;
                f.write_fmt(format_args!("{}", op));
                r.kind.display_value(f, scope)?;
                Ok(())
            },
            Self::Application(l, r) => {
                l.kind.display_value(f, scope)?;
                f.write_str(" ")?;
                r.kind.display_value(f, scope)?;
                Ok(())
            },
            Self::Type(ty) => scope.get_type(*ty).display_value(f, &(scope.types(), scope.global)),
            Self::DataVariant(d, v) => {
                scope.global.get_data_variant(*d, *v).display_value(f, scope)
            },
            x => {
                dbg!(x);
                unimplemented!()
            }
        }
    }
}

// TODO: typeclasses
impl Expr {
    pub fn int(val: i128, span: Span, ctx: &mut ScopeCtx) -> Self {
        Expr::new(ctx.alloc_type(Type::Int), span, ExprKind::Int(val))
    }
    pub fn bin_op(self, other: Expr, op: BinOp, ctx: &mut ScopeCtx) -> Result<Self, Error> {
        let span = self.span.extend(&other.span);
        match op {
            BinOp::Add | BinOp::Mul | BinOp::Sub | BinOp::Div | BinOp::Pow => {
                arithmetic_op(self, other, op, ctx)
                    .map_err(|s| Error::Custom(span, s, "-here".to_string()))
            },
            BinOp::And | BinOp::Or => {
                unimplemented!()
            },
            BinOp::Eq | BinOp::NotEq | BinOp::Gr | BinOp::GrOrEq | BinOp::Le | BinOp::LeOrEq => {
                compare_op(self, other, op, ctx)
                    .map_err(|s| Error::Custom(span, s, "-here".to_string()))
            },
        }
    }
    pub fn dot(self, name: String, ctx: &mut ScopeCtx, span: Span) -> Result<Self, Error> {
        match self.kind {
            ExprKind::Ident(i) => {
                let o = ctx.find(PathToFind::new(name.as_str(), smallvec![i.as_str()]))
                    .ok_or_else(|| Error::Custom(span, format!("Cannot find {}", i), "-here".into()))?;
                match o {
                    Object::EnumVariant(id, idx) => {
                        Ok(Expr::new(ctx.global.get_data(id).get_variant_ty(idx, ctx), span, ExprKind::DataVariant(id, idx)))
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn arithmetic_op(
    left: Expr,
    right: Expr,
    op: BinOp,
    ctx: &ScopeCtx,
) -> Result<Expr, String> {
    match (
        ctx.get_type(left.ty),
        ctx.get_type(right.ty),
    ) {
        (Type::Int, Type::Int) => Ok(Expr::new(
            // if left.ty == Type::Int we can reuse it.
            left.ty,
            left.span.extend(&right.span),
            ExprKind::BinOp(Box::new(left), Box::new(right), op),
        )),
        (x, y) => Err(format!(
            "Arithmetic ops only for ints, got {} and {}",
            x.display_value_string(&(ctx.types(), ctx.global)), y.display_value_string(&(ctx.types(), ctx.global))
        )),
    }
}

fn compare_op(
    left: Expr,
    right: Expr,
    op: BinOp,
    ctx: &mut ScopeCtx,
) -> Result<Expr, String> {
    match (
        ctx.get_type(left.ty),
        ctx.get_type(right.ty),
    ) {
        (Type::Int, Type::Int) => Ok(Expr::new(
            // if left.ty == Type::Int we can reuse it.
            ctx.alloc_bool(),
            left.span.extend(&right.span),
            ExprKind::BinOp(Box::new(left), Box::new(right), op),
        )),
        (x, y) => Err(format!(
            "Arithmetic ops only for ints, got {} and {}",
            x.display_value_string(&(ctx.types(), ctx.global)), y.display_value_string(&(ctx.types(), ctx.global))
        )),
    }
}

fn parse_binary_op(
    l: &Token,
    r: &Token,
    ctx: &mut ScopeCtx,
    g: &mut HashMap<String, Option<Id<Type>>>,
    op: BinOp,
) -> Result<Option<Expr>, Error>
{
    let left = parse_expr(l, ctx, g, None)?;
    let right = parse_expr(r, ctx, g, None)?;
    let (l, r) = match (left, right) {
        (Some(l), Some(r)) => (l, r),
        (Some(_l), None) => return Ok(None),
        (None, Some(_r)) => return Ok(None),
        (None, None) => return Ok(None),
    };
    Expr::bin_op(l, r, op, ctx).map(Some)
}

fn assert_types_equal(this: Id<Type>, expected: Option<Id<Type>>, span: Span, ctx: &ScopeCtx) -> Result<(), Error> {
    match expected {
        Some(ex) => {
            let this = ctx.get_type(this);
            let ex = ctx.get_type(ex);
            //dbg!(1);
            match this.is_part_of(ex, ctx) {
                true => Ok(()),
                false => {
                    //dbg!(2);
                    //dbg!(this.display_value_string(&(&ctx.types, ctx.global)));
                    //dbg!(ex.display_value_string(&(&ctx.types, ctx.global)));
                    Err(Error::Custom(
                        span,
                        format!("Expected {}, found {} type", ex.display_value_string(&(ctx.types(), ctx.global)), this.display_value_string(&(ctx.types(), ctx.global))),
                        "-here".to_owned(),
                    ))
                },
            }
        },
        None => Ok(()),
    }
}

fn check_type(e: Expr, expected: Option<Id<Type>>, ctx: &ScopeCtx) -> Result<Option<Expr>, Error> {
    assert_types_equal(e.ty, expected, e.span, ctx)?;
    Ok(Some(e))
}

pub fn parse_type(token: &Token, ctx: &mut ScopeCtx) -> Result<Id<Type>, Error> {
    let ex = Some(ctx.alloc_type(Type::Type));
    parse_expr(token, ctx, &mut HashMap::new(), ex)
        .and_then(|e| e.ok_or_else(|| Error::cannot_infer_type(token.span)))
        .and_then(|e| {
            interpret_expr_as_ty(e, ctx)
        })
}

pub fn interpret_expr_as_ty(e: Expr, ctx: &mut ScopeCtx) -> Result<Id<Type>, Error> {
    match &e.kind {
        ExprKind::Type(x) => return Ok(*x),
        _ => {}
    };
    let mut interpreter = Interpreter::new(ctx);
    let e2 = interpreter.eval(&e, &LocalContext::new())?;
    match e2 {
        ByteCode::Type(ty) => Ok(ty.clone()),
        ByteCode::DataType(ty) => Ok(ctx.alloc_type(Type::Data(ty))),
        _ => unimplemented!()
    }
}

/// Parse token tree into expression.
///
/// Returns `Some` if:
/// 1. `expected` is `None` and can infer type.
/// 2. `expected` is `Some(T)` and `typeof(expr) == T`.
///
/// Returns `None` if:
/// 1. `expected` is `None` and cannot infer type using this context, but it can be inferred using some additional data.
/// 2. `expected` is `Some(T)` and `typeof(expr) != T`.
///
/// If cannot infer expression type and there are no way to do this, returns `Err`.
pub fn parse_expr(
    token: &Token,
    ctx: &mut ScopeCtx,
    g: &mut HashMap<String, Option<Id<Type>>>,
    expected: Option<Id<Type>>,
) -> Result<Option<Expr>, Error> {
    match &token.ast {
        Ast::Int(i) => check_type(Expr::int(*i, token.span, ctx), expected, ctx),
        Ast::BinOp(l, r, op) => {
            match parse_binary_op(
                l.as_ref(),
                r.as_ref(),
                ctx,
                g,
                op.clone(),
            )? {
                Some(e) => {
                    check_type(e, expected, ctx)
                },
                None => Ok(None),
            }
        }
        Ast::Neg(t) => {
            let ex = ctx.alloc_type(Type::Int);
            let expr = parse_expr(
                t,
                ctx,
                &mut HashMap::new(),
                Some(ex)
            )?;
            match expr {
                Some(e) => Ok(Some(Expr::new(
                    e.ty,
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
                    ctx.alloc_type(Type::Type),
                    token.span,
                    ExprKind::Type(ctx.alloc_type(Type::Int)),
                ),
                "Type" => Expr::new(
                    ctx.alloc_type(Type::Type),
                    token.span,
                    ExprKind::Type(ctx.alloc_type(Type::Type)),
                ),
                _ => match g.get(i) {
                    Some(Some(t)) => Expr::new(t.clone(), token.span, ExprKind::Ident(i.clone())),
                    Some(None) => unreachable!(),
                    None => match ctx.find(PathToFind::name(i.as_str())) {
                        Some(o) => {
                            Expr::new(o.get_type(ctx), token.span, ExprKind::Ident(i.clone()))
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
            let expr = infer(expr, expected.clone(), ctx);
            check_type(expr, expected, ctx)
        }
        Ast::Val => {
            todo!()
        },
        Ast::Slice(_) => unimplemented!(),
        Ast::Implication(l, r) => {
            let exp = Some(ctx.alloc_type(Type::Type));
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
            let l = interpret_expr_as_ty(left, ctx)?;
            let r = interpret_expr_as_ty(right, ctx)?;
            let f = ctx.alloc_type(Type::Function(Function {
                get_value: l,
                return_value: r
            }));
            let expr = Expr::new(
                ctx.alloc_type(Type::Type),
                token.span,
                ExprKind::Type(f)
            );
            check_type(expr, expected, ctx)
        }
        Ast::Named(_name, _ty) => {
            /*let ty = parse_expr(ty, ctx, g, None)?
                .unwrap()
                .convert_to_type(ctx)?;
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
                    let ex_ret = expected.clone().unwrap_or_else(|| ctx.alloc_type(Type::Unknown(None)));
                    let ex = ctx.alloc_type(Type::Function(Function {
                        get_value: right.ty,
                        return_value: ex_ret,
                    }));
                    let left = parse_expr(
                        left,
                        ctx,
                        g,
                        Some(ex),
                    )?;
                    match left {
                        Some(left) => {
                            let expr_ty = {
                                let left_ty = ctx.get_type(left.ty);
                                match left_ty {
                                    Type::Function(f) => {
                                        f.return_value
                                    }
                                    _ => unreachable!()
                                }
                            };
                            let e = Expr::new(
                                expr_ty,
                                token.span,
                                ExprKind::Application(Box::new(left), Box::new(right)),
                            );
                            check_type(e, expected, ctx)
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
                let ty = infer(expr, expected, ctx);
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
                    enter_scope(&arm.pat, ctx, cond.ty.clone())?;

                    let ex = parse_expr(&arm.arm, ctx, g, expected.clone())?
                        .ok_or_else(|| Error::cannot_infer_type(token.span))?;
                    // types stored independently from scope.
                    ctx.leave_scope();

                    Ok((
                        ir::patmat::Pattern::parse(arm.pat.clone()),
                        ex
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let ty = arms
                .first()
                .map(|arm| arm.1.ty.clone())
                .unwrap_or_else(|| unimplemented!());
            let arm1_ty = ctx.get_type(ty);
            arms.iter()
                .skip(1)
                .map(|arm| {
                    let this_arm_ty = ctx.get_type(arm.1.ty);
                    match ctx.get_type(arm.1.ty).eq(arm1_ty, ctx) {
                        true => Ok(()),
                        false => Err(Error::custom(
                            token.span,
                            format!("Expected type {} but found {}", arm1_ty.display_value_string(&(ctx.types(), ctx.global)), this_arm_ty.display_value_string(&(ctx.types(), ctx.global))),
                        )),
                    }
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

fn infer(mut expr: Expr, expected: Option<Id<Type>>, ctx: &mut ScopeCtx) -> Expr {
    let expected = match expected {
        Some(ex) => ex,
        None => return expr,
    };
    expr.ty = clarify_generics(expr.ty.clone(), expected.clone(), ctx);
    expr.ty = infer_ty(expr.ty.clone(), expected, ctx);
    expr
}

fn infer_ty(ty_id: Id<Type>, expected_id: Id<Type>, ctx: &mut ScopeCtx) -> Id<Type> {
    let ty = ctx.get_type(ty_id);
    let expected = ctx.get_type(expected_id);
    match (ty, expected) {
        (Type::Unknown(None), _) => expected_id,
        (Type::Function(f), Type::Function(ef)) => {
            let get_id = f.get_value;
            let ret_id = f.return_value;
            let gete_id = ef.get_value;
            let rete_id = ef.return_value;
            let l = infer_ty(get_id, gete_id, ctx);
            let r = infer_ty(ret_id, rete_id, ctx);
            ctx.alloc_type(Type::Function(Function {
                get_value: l,
                return_value: r,
            }))
        },
        (Type::Data(d), Type::Data(ed)) => {
            ctx.alloc_type(Type::Data(Concrete::new(
                d.base,
                d.generics.iter().zip(ed.generics.iter()).map(|(ty, ety)| {
                    match ctx.get_type(*ty) {
                        Type::Unknown(_) => ety.clone(),
                        _ => ty.clone()
                    }
                }).collect()
            )))
        },
        _ => ty_id,
    }
}

fn clarify_generics(mut ty: Id<Type>, expected: Id<Type>, ctx: &mut ScopeCtx) -> Id<Type> {
    let gens = {
        let mut gens = HashMap::new();
        generics_map(ty.clone(), expected, &mut gens, ctx);
        gens
    };
    for (gen, gen_ty) in gens {
        ty = Type::update_set_generic_func_unknowns(ty, gen.as_str(), gen_ty, ctx);
    }
    ty
}

fn generics_map(ty: Id<Type>, expected: Id<Type>, gens: &mut HashMap<String, Id<Type>>, ctx: &ScopeCtx) {
    match (ctx.get_type(ty), ctx.get_type(expected)) {
        (Type::Unknown(Some(g)), _) => {
            if !gens.contains_key(g.name.as_ref()) {
                gens.insert(g.name.as_ref().clone(), expected.clone());
            }
        },
        (Type::Function(f), Type::Function(ef)) => {
            generics_map(f.get_value.clone(), ef.get_value.clone(), gens, ctx);
            generics_map(f.return_value.clone(), ef.return_value.clone(), gens, ctx);
        },
        (Type::Data(d), Type::Data(ed)) => {
            d.generics.iter().zip(ed.generics.iter()).for_each(|(g, ge)| {
                generics_map(g.clone(), ge.clone(), gens, ctx)
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
    ctx: &mut ScopeCtx,
    g: &mut HashMap<String, Option<Id<Type>>>,
    expected_type: Option<Id<Type>>,
) -> Result<Option<Expr>, Error> {
    let ex = parse_expr(assign, ctx, g, None)?;
    match ex {
        // if `ex` is Some, it means `ex` have some inferred type.
        Some(assign_expr) => {
            let var = Var {
                name: var.as_ref().clone(),
                ty: assign_expr.ty.clone(),
            };
            ctx.enter_scope();
            ctx.alloc_var(var.clone());
            let expr = parse_expr(expr_token, ctx, g, expected_type.clone())?;
            ctx.leave_scope();
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
                let expr = match parse_expr(expr_token, ctx, g, expected_type.clone())? {
                    Some(e) => e,
                    None => Err(Error::cannot_infer_type(expr_token.span))?,
                };
                match g.get(var.as_str()) {
                    Some(Some(inferred)) => {
                        let var = Var {
                            name: var.as_ref().clone(),
                            ty: inferred.clone(),
                        };
                        ctx.enter_scope();
                        ctx.alloc_var(var.clone());
                        let exp = parse_expr(assign, ctx, g, Some(inferred.clone()))?;
                        ctx.leave_scope();
                        match exp {
                            Some(assigned) => {
                                let expr = Expr::new(
                                    expr.ty,
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
fn enter_scope(
    pattern: &Spanned<Pattern>,
    ctx: &mut ScopeCtx,
    pattern_type: Id<Type>,
) -> Result<(), Error> {
    ctx.enter_scope();
    create_scope_objects(pattern, ctx, pattern_type)
}

fn create_scope_objects(
    pattern: &Spanned<Pattern>,
    ctx: &mut ScopeCtx,
    pattern_type: Id<Type>,
) -> Result<(), Error> {
    match pattern.deref() {
        Pattern::Otherwise => {
            Ok(())
        }
        Pattern::Bind(s, b) => {
            ctx.alloc_var(Var::new(s.as_str().into(), pattern_type));
            create_scope_objects(b.as_ref(), ctx, pattern_type)
        }
        Pattern::Variant(path, pats) => {
            match ctx.find(PathToFind::from_path(path)) {
                Some(Object::EnumVariant(id, idx)) => {
                    let types = {
                        let concrete = match ctx.get_type(pattern_type) {
                            Type::Data(d) => d.clone(),
                            _ => {
                                unreachable!()
                            }
                        };
                        let mut types = ctx.global.get_data_variant(id, idx).data(ctx);
                        for (gen, gen_ty) in ctx.global.get_data(concrete.base).ty.generics.iter().zip(concrete.generics.iter()) {
                            types.iter_mut().for_each(|x| {
                                *x = Type::update_set_generic_func(*x, gen.name.as_str(), *gen_ty, ctx);
                            });
                        }
                        types
                    };
                    match types.len() == pats.len() {
                        true => {
                            for (pat, ty) in pats.iter().zip(types.iter()) {
                                create_scope_objects(pat, ctx, ty.clone())?
                            }
                            Ok(())
                        },
                        false => {
                            return Err(Error::custom(
                                pattern.span,
                                format!("Expected {} patterns, found {}", types.len(), pats.len()),
                            ))
                        }
                    }
                }
                Some(_) => unimplemented!(),
                None => return Err(Error::custom(pattern.span, format!("{} not found", path))),
            }
        },
        Pattern::Ident(s) => {
            ctx.alloc_var(Var::new(s.clone(), pattern_type.clone()));
            Ok(())
        },
    }
}
