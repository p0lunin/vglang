use crate::error::Error;
use crate::object::{Object, FunctionObject, AllObject, Expr};
use std::rc::Rc;
use crate::spanned::Spanned;
use crate::types::{Type, TypeType};

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

pub fn context_from_types<'a>(types: Vec<Rc<Spanned<Type>>>) -> Box<Context<'a>> {
    Box::new(Context {
        objects: types.into_iter().map(|t| {
            let span = t.span;
            AllObject::Type(Rc::new(Object {
                object: t,
                object_type: Rc::new(Spanned::new(TypeType, span)),
            }))
        }).collect(),
        parent: None,
    })
}

pub fn type_check_objects<'a>(objects: &[AllObject], ctx: Option<&'a Context<'a>>) -> Result<(), Error> {
    let mut ctx = Context {
        objects: vec![],
        parent: ctx,
    };
    objects.iter().scan(ctx, |ctx, object| {
        match object {
            AllObject::Function(f) => {
                type_check_function(&f.object, ctx).err()?;
                // TODO: remove clone
                ctx.objects.push(AllObject::Function(f.clone()));
                Some(Ok(()))
            }
            _ => Some(Ok(())),
        }
    }).collect()
}

pub fn type_check_function(function: &FunctionObject, top: &Context) -> Result<(), Error> {
    let FunctionObject { name, args, return_value, body } = function;
    // TODO: Remove copying
    let cur_objects = args.iter().map(|a| AllObject::Var((*a).clone())).collect::<Vec<_>>();
    let ctx = Context {
        objects: cur_objects,
        parent: Some(top),
    };
    // TODO: Remove clone()
    let res_type = type_check_expr(body.clone(), &ctx)?;
    match res_type.is_part_of(return_value) {
        true => Ok(()),
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
        Expr::CallFunction(f) => {
            Ok(f.object_type.args_types().last().unwrap().clone())
        }
    }
}
