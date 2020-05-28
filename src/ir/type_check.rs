use crate::ir::objects::{AllObject, FunctionObject};
use crate::common::{Context, Error, SpannedError};
use std::ops::Deref;
use crate::ir::Expr;
use std::rc::Rc;
use std::cell::RefCell;
use crate::ir::types::Type;

pub fn type_check_objects<'a>(
    objects: &[AllObject],
    ctx: Option<&'a Context<'a, AllObject>>,
) -> Result<(), Error> {
    let mut ctx = Context {
        objects: vec![],
        parent: ctx,
    };
    objects
        .iter()
        .map(|object| match object {
            AllObject::Function(f) => {
                ctx.objects.push(AllObject::Function(f.clone()));
                type_check_function(&f.object, &ctx)?;
                Ok(())
            }
            _ => Ok(()),
        })
        .collect()
}

pub fn type_check_function(function: &FunctionObject, top: &Context<'_, AllObject>) -> Result<(), Error> {
    let ctx = function.create_ctx(top);
    let body = function.body.as_ref();
    let return_type = function.get_return_type();
    let res_type = type_check_expr(body, &ctx)?;
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
    ($l:tt, $r:tt, $ctx:tt, $op:tt) => {{
        let left = type_check_expr($l.as_ref(), $ctx)?;
        let right = type_check_expr($r.as_ref(), $ctx)?;
        let new_span = left.borrow().span().extend(&right.borrow().span());
        let left_br = left.borrow();
        let right_br = left.borrow();
        Ok(Rc::new(RefCell::new(
            left_br
                .clone()
                .$op(right_br.clone())
                .spanned_err(new_span)?,
        )))
    }};
}

pub fn type_check_expr(expr: &Expr, ctx: &Context<'_, AllObject>) -> Result<Rc<RefCell<Type>>, Error> {
    match expr {
        Expr::Int(i) => Ok(i.get_type()),
        Expr::Add(l, r) => binary_op!(l, r, ctx, add),
        Expr::Sub(l, r) => binary_op!(l, r, ctx, sub),
        Expr::Object(o) => o.type_check_self(ctx),
        Expr::Mul(l, r) => binary_op!(l, r, ctx, mul),
        Expr::Div(l, r) => binary_op!(l, r, ctx, div),
        Expr::Pow(l, r) => binary_op!(l, r, ctx, pow),
        Expr::And(_, _) => unimplemented!(),
        Expr::Or(_, _) => unimplemented!(),
        Expr::Gr(_, _) => unimplemented!(),
        Expr::Eq(_, _) => unimplemented!(),
        Expr::NotEq(_, _) => unimplemented!(),
        Expr::GrOrEq(_, _) => unimplemented!(),
        Expr::Le(_, _) => unimplemented!(),
        Expr::LeOrEq(_, _) => unimplemented!(),
        Expr::Neg(e) => type_check_expr(&e, ctx).and_then(|ty| {
            Ok(Rc::new(RefCell::new(
                ty.borrow().clone().neg().spanned_err(e.span())?,
            )))
        }),
    }
}
