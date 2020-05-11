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
        .map(|object| {
            match object {
                AllObject::Function(f) => {
                    ctx.objects.push(AllObject::Function(f.clone()));
                    type_check_function(&f.object, &ctx)?;
                    // TODO: remove clone
                    Ok(())
                }
                _ => Ok(()),
            }
        })
        .collect()
}

pub fn type_check_function(function: &FunctionObject, top: &Context) -> Result<(), Error> {
    let ctx = function.create_ctx(top);
    let body = function.get_body();
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

fn type_check_expr(expr: Expr, ctx: &Context) -> Result<Rc<Spanned<Type>>, Error> {
    match expr {
        Expr::Int(i) => Ok((i.get_type())),
        Expr::Add(l, r) => {
            let left = type_check_expr(*l, ctx)?;
            let right = type_check_expr(*r, ctx)?;
            let new_span = left.span.extend(&right.span);
            Ok(Rc::new(Spanned::new(
                (**left)
                    .clone()
                    .op_add((**right).clone())
                    .spanned_err(new_span)?,
                new_span,
            )))
        }
        Expr::Sub(l, r) => {
            let left = type_check_expr(*l, ctx)?;
            let right = type_check_expr(*r, ctx)?;
            let new_span = left.span.extend(&right.span);
            Ok(Rc::new(Spanned::new(
                (**left)
                    .clone()
                    .op_sub((**right).clone())
                    .spanned_err(new_span)?,
                new_span,
            )))
        }
        Expr::CallFunction(f) => Ok(Rc::new(Spanned::new(
            Type::AnotherType(Spanned::new(f.call(), f.object.span)),
            f.span,
        ))),
        Expr::Var(v) => Ok(Rc::new(Spanned::new(
            Type::AnotherType(Spanned::new(v.object_type.clone(), v.object.0.span)),
            v.span,
        ))),
        Expr::Type(t) => Ok(Rc::new(Spanned::new(
            Type::AnotherType(t.object.clone()),
            t.object.span,
        ))),
        Expr::CallFunctionDef(def) => Ok(ctx.find(def.name.as_str()).unwrap().get_type()),
    }
}
