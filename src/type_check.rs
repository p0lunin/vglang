use crate::error::{Error, SpannedError};
use crate::object::{AllObject, Expr, FunctionObject, Object};
use crate::spanned::Spanned;
use crate::types::{Type, TypeType};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Context<'a> {
    pub objects: Vec<AllObject>,
    pub parent: Option<&'a Context<'a>>,
}

impl<'a> Context<'a> {
    pub fn find(&'a self, name: &str) -> Option<&'a AllObject> {
        let this = self.objects.iter().find(|t| t.name() == name);
        match (this, self.parent) {
            (Some(t), _) => Some(t),
            (_, Some(p)) => p.find(name),
            _ => None,
        }
    }
}

pub fn type_check_objects<'a>(
    objects: &[AllObject],
    ctx: Option<&'a Context<'a>>,
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

pub fn type_check_function(function: &FunctionObject, top: &Context) -> Result<(), Error> {
    let ctx = function.create_ctx(top);
    let body = function.body.as_ref();
    let return_type = function.get_return_type();
    let res_type = type_check_expr(body, &ctx)?;
    match res_type.is_part_of(return_type) {
        true => Ok(()),
        false => Err(Error::Custom(
            res_type.span,
            format!("Expected {} type, found {}", return_type, res_type),
            "-here".to_owned(),
        )),
    }
}

macro_rules! binary_op {
    ($l:tt, $r:tt, $ctx:tt, $op:tt) => {{
        let left = type_check_expr($l.as_ref(), $ctx)?;
        let right = type_check_expr($r.as_ref(), $ctx)?;
        let new_span = left.span.extend(&right.span);
        Ok(Rc::new(Spanned::new(
            (**left)
                .clone()
                .$op((**right).clone())
                .spanned_err(new_span)?,
            new_span,
        )))
    }};
}

pub fn type_check_expr(expr: &Expr, ctx: &Context) -> Result<Rc<Spanned<Type>>, Error> {
    match expr {
        Expr::Int(i) => Ok((i.get_type())),
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
            Ok(Rc::new(Spanned::new(
                (**ty).clone().neg().spanned_err(e.span())?,
                e.span(),
            )))
        }),
    }
}
