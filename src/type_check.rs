use crate::error::Error;
use crate::object::{Object, FunctionObject, AllObject, Expr};
use std::rc::Rc;
use crate::spanned::Spanned;
use crate::types::{Type, TypeType};

#[derive(Debug, PartialEq, Clone)]
pub struct Context {
    pub objects: Vec<AllObject>,
    pub parent: Option<Box<Context>>,
}

pub fn context_from_types(types: Vec<Rc<Spanned<Type>>>) -> Box<Context> {
    Box::new(Context {
        objects: types.into_iter().map(|t| {
            let span = t.span;
            AllObject::Type(Object {
                object: t,
                object_type: Rc::new(Spanned::new(TypeType, span)),
            })
        }).collect(),
        parent: None,
    })
}

pub fn type_check_objects(objects: &[AllObject], ctx: Option<Box<Context>>) -> Result<(), Error> {
    objects.iter().map(|object| {
        match object {
            AllObject::Function(f) => {
                // TODO: remove clone
                type_check_function(&f.object, ctx.clone())?;
                Ok(())
            }
            _ => unimplemented!(),
        }
    }).collect()
}

pub fn type_check_function(function: &FunctionObject, top: Option<Box<Context>>) -> Result<Option<Box<Context>>, Error> {
    let FunctionObject { name, args, return_value, body } = function;
    // TODO: Remove copying
    let cur_objects = args.iter().map(|a| AllObject::Var((*a).clone())).collect::<Vec<_>>();
    let ctx = Context {
        objects: cur_objects,
        parent: top,
    };
    // TODO: Remove clone()
    let res_type = type_check_expr(body.clone(), &ctx)?;
    match res_type == *return_value {
        true => Ok(ctx.parent),
        false => Err(Error::Custom(res_type.span, format!("Expected {} type, found {}", return_value, res_type), "-here".to_owned()))
    }
}

fn type_check_expr(expr: Expr, ctx: &Context) -> Result<Rc<Spanned<Type>>, Error> {
    match expr {
        Expr::Int(i) => Ok((i.object_type.clone())),
        Expr::Add(l, r) => {
            let left = type_check_expr(*l, ctx)?;
            let right = type_check_expr(*r, ctx)?;
            let new_span = left.span.extend(&right.span);
            Ok(Rc::new(Spanned::new((**left).clone().op_add((**right).clone())?, new_span)))
        }
    }
}
