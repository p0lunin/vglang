use crate::common::{Context, Error, Spanned, SpannedError, Span};
use crate::ir::objects::{
    monomorphization, AllObject, CurriedFunction, EnumInstance, FunctionInstanceObject,
    FunctionObject,
};
use crate::ir::types::base_types::Function;
use crate::ir::types::{OneTypeKind, Type};
use crate::ir::{Expr, ExprKind, IrContext};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

pub fn type_check_objects<'a>(
    ctx: Option<&'a Context<'a, AllObject>>,
    ir_ctx: &mut IrContext,
) -> Result<(), Error> {
    let mut ctx = Context {
        objects: vec![],
        parent: ctx,
    };
    let mut functions = vec![];
    std::mem::swap(&mut functions, &mut ir_ctx.functions);
    functions
        .iter()
        .map(|f| {
            ctx.objects
                .push(AllObject::FunctionDefinition(f.def.clone()));
            type_check_function(&f, &ctx, ir_ctx)?;
            Ok(())
        })
        .collect::<Result<_, _>>()
        .map(|()| {
            std::mem::swap(&mut functions, &mut ir_ctx.functions);
            ()
        })
}

pub fn type_check_function(
    function: &FunctionObject,
    top: &Context<'_, AllObject>,
    ir_ctx: &mut IrContext,
) -> Result<(), Error> {
    let ctx = function.create_ctx(top);
    let body = function.body.as_ref();
    let return_type = function.get_return_type();
    let res_type = type_check_expr(body, &ctx, ir_ctx)?;
    let borrowed = res_type.borrow();
    match borrowed.is_part_of(return_type.deref()) {
        true => Ok(()),
        false => Err(Error::Custom(
            body.span(),
            format!("Expected {} type, found {}", return_type, res_type.borrow()),
            "-here".to_owned(),
        )),
    }
}

macro_rules! binary_op {
    ($l:tt, $r:tt, $ctx:tt, $ir_ctx:tt, $op:tt) => {{
        let left = type_check_expr($l.as_ref(), $ctx, $ir_ctx)?;
        let right = type_check_expr($r.as_ref(), $ctx, $ir_ctx)?;
        let new_span = left.borrow().span().extend(&right.borrow().span());
        let left_br = left.borrow();
        let right_br = left.borrow();
        Ok(Rc::new(RefCell::new(
            left_br.clone().$op(&right_br).spanned_err(new_span)?,
        )))
    }};
}

pub fn type_check_expr(
    expr: &Expr,
    ctx: &Context<'_, AllObject>,
    ir_ctx: &mut IrContext,
) -> Result<Rc<RefCell<Type>>, Error> {
    match &expr.kind {
        ExprKind::Int(_) => Ok(expr.ty.clone()),
        ExprKind::Add(l, r) => binary_op!(l, r, ctx, ir_ctx, add),
        ExprKind::Sub(l, r) => binary_op!(l, r, ctx, ir_ctx, sub),
        ExprKind::Object(o) => type_check_object(o, ctx, ir_ctx),
        ExprKind::Mul(l, r) => binary_op!(l, r, ctx, ir_ctx, mul),
        ExprKind::Div(l, r) => binary_op!(l, r, ctx, ir_ctx, div),
        ExprKind::Pow(l, r) => binary_op!(l, r, ctx, ir_ctx, pow),
        ExprKind::And(_, _) => unimplemented!(),
        ExprKind::Or(_, _) => unimplemented!(),
        ExprKind::Gr(_, _) => Ok(expr.ty.clone()),
        ExprKind::Eq(_, _) => Ok(expr.ty.clone()),
        ExprKind::NotEq(_, _) => Ok(expr.ty.clone()),
        ExprKind::GrOrEq(_, _) => Ok(expr.ty.clone()),
        ExprKind::Le(_, _) => Ok(expr.ty.clone()),
        ExprKind::LeOrEq(_, _) => Ok(expr.ty.clone()),
        ExprKind::Neg(e) => type_check_expr(&e, ctx, ir_ctx).and_then(|ty| {
            Ok(Rc::new(RefCell::new(
                ty.borrow().clone().neg().spanned_err(e.span())?,
            )))
        }),
        ExprKind::Let { var, assign, expr } => {
            let val = type_check_expr(assign.as_ref(), ctx, ir_ctx)?;
            let ctx = Context {
                objects: vec![AllObject::Var(var.clone())],
                parent: Some(ctx),
            };
            type_check_expr(expr, &ctx, ir_ctx)
        }
        ExprKind::IfThenElse {
            condition: _,
            then_arm: _,
            else_arm: _,
        } => Ok(expr.ty.clone()),
    }
}

fn type_check_object(
    obj: &AllObject,
    ctx: &Context<'_, AllObject>,
    ir_ctx: &mut IrContext,
) -> Result<Rc<RefCell<Type>>, Error> {
    match obj {
        AllObject::EnumVariantInstance(v) => v.type_check_self(ctx, ir_ctx),
        AllObject::CurriedFunction(f) => monomorphize_function(f, ctx, ir_ctx).and_then(|o| {
            type_check_func_call(o.ftype.borrow().deref(), &f.scope)?;
            *f.instance.borrow_mut() = Some(o.clone());
            Ok(o.ftype.clone())
        }),
        o => Ok(o.get_type()),
    }
}

fn type_check_func_call(ty: &Type, data: &[Expr]) -> Result<(), Error> {
    match (&ty.get_inner(), data) {
        (Type::Function(f), [x, xs @ ..]) => {
            check_two_types(x.ty.borrow().deref(), f.kind.get_value.borrow().deref(),  x.span())
                .and_then(|_| type_check_func_call(f.kind.return_value.borrow().deref(), xs))
        }
        (t, [x]) => check_two_types(x.ty.borrow().deref(), t, x.span()),
        (_, []) => Ok(()),
        _ => unreachable!(),
    }
}

fn check_two_types(left: &Type, right: &Type, span: Span) -> Result<(), Error> {
    match left.is_part_of(right) {
        true => Ok(()),
        false => Err(Error::Custom(span, format!("Expected {}, found {}", right, left), "-here".to_string()))
    }
}

fn monomorphize_function(
    function: &Rc<CurriedFunction>,
    ctx: &Context<'_, AllObject>,
    ir_ctx: &mut IrContext,
) -> Result<Rc<FunctionInstanceObject>, Error> {
    let generics = vec_generics_to_hashmap(helper(
        &function.orig.ftype(),
        &function.scope,
        ctx,
        ir_ctx,
    )?)?;
    let inst = FunctionInstanceObject {
        orig: function.orig.clone(),
        ftype: monomorphize_type(&function.orig.ftype(), &generics, ctx, ir_ctx)?,
    };
    Ok(ir_ctx.create_specialized_function(inst))
}

fn helper(
    ty: &Rc<RefCell<Type>>,
    exprs: &[Expr],
    ctx: &Context<'_, AllObject>,
    ir_ctx: &mut IrContext,
) -> Result<Vec<(String, Rc<RefCell<Type>>)>, Error> {
    match (Type::get_inner_cell(ty).borrow().deref(), exprs) {
        (Type::Function(f), [x, xs @ ..]) => {
            let mut generics = match f.kind.get_value.borrow().deref() {
                Type::Generic(g) => vec![(g.clone().inner(), type_check_expr(x, ctx, ir_ctx)?)],
                Type::AnotherType(t) => match t.borrow().deref() {
                    Type::Generic(g) => vec![(g.clone().inner(), type_check_expr(x, ctx, ir_ctx)?)],
                    _ => vec![],
                },
                _ => vec![],
            };
            helper(&f.kind.return_value, xs, ctx, ir_ctx).map(|mut res| {
                res.append(&mut generics);
                res
            })
        }
        (Type::Generic(g), [x]) => Ok(vec![(g.clone().inner(), type_check_expr(x, ctx, ir_ctx)?)]),
        (t, [x]) => match x.ty.borrow().is_part_of(t) {
            true => Ok(vec![]),
            false => Err(Error::Span(x.span())),
        },
        (_, []) => Ok(vec![]),
        _ => {
            //dbg!(res);
            unreachable!()
        }
    }
}

fn vec_generics_to_hashmap(
    generics: Vec<(String, Rc<RefCell<Type>>)>,
) -> Result<HashMap<String, Rc<RefCell<Type>>>, Error> {
    let cap = generics.capacity();
    generics
        .into_iter()
        .fold(Ok(HashMap::with_capacity(cap)), |map, (name, val)| {
            let mut map = map?;
            let val_to_insert = match map.get(&name) {
                Some(v) => {
                    let v1_borrowed = v.borrow();
                    let v2_borrowed = val.borrow();
                    let span = v2_borrowed.span().extend(&v1_borrowed.span());
                    if v1_borrowed.is_part_of(v2_borrowed.deref()) {
                        val.clone()
                    } else {
                        Rc::new(RefCell::new(
                            v1_borrowed
                                .deref()
                                .clone()
                                .and(v2_borrowed.deref())
                                .spanned_err(span)?,
                        ))
                    }
                }
                None => val,
            };
            map.insert(name, val_to_insert);
            Ok(map)
        })
}

fn monomorphize_type(
    ty: &Rc<RefCell<Type>>,
    generics: &HashMap<String, Rc<RefCell<Type>>>,
    ctx: &Context<'_, AllObject>,
    ir_ctx: &mut IrContext,
) -> Result<Rc<RefCell<Type>>, Error> {
    match ty.borrow().deref() {
        Type::Generic(g) => match generics.get(g.as_str()) {
            Some(t) => Ok(t.clone()),
            None => unimplemented!(),
        },
        Type::Function(f) => {
            let get_value = monomorphize_type(&f.kind.get_value, generics, ctx, ir_ctx)?;
            let return_value = monomorphize_type(&f.kind.get_value, generics, ctx, ir_ctx)?;
            Ok(Rc::new(RefCell::new(Type::Function(OneTypeKind {
                name: f.name.clone(),
                kind: Spanned::new(
                    Function {
                        get_value,
                        return_value,
                    },
                    f.kind.span,
                ),
            }))))
        }
        Type::EnumInstance(e) => {
            match e.generics.is_empty() {
                true => Ok(ty.clone()),
                false => {
                    let inst = EnumInstance {
                        orig: e.orig.clone(),
                        generics: monomorphization(e.generics.iter(), generics),
                        variants: e.variants.clone(), // TODO: monomorh this
                    };
                    Ok(ir_ctx.create_specialized_enum(inst).call())
                }
            }
        }
        Type::AnotherType(t) => monomorphize_type(&t.val, generics, ctx, ir_ctx),
        Type::ParenthesisType(t) => monomorphize_type(&t, generics, ctx, ir_ctx),
        _ => Ok(ty.clone()),
    }
}
