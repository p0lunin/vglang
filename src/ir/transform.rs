use crate::common::{LocalContext, Error, Spanned};
use crate::ir::expr::{parse_expr, parse_type};
use crate::ir::objects::{Arg, DataDef, FunctionDefinition, FunctionObject, Object};
use crate::ir::types::{Generic, Type};
use crate::ir::{expr, Implementations, Expr};
use crate::syntax::ast::{FunctionDef, FunctionImpl, TopLevelToken};
use itertools::Itertools;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use crate::common::global_context::{GlobalCtx, ScopeCtx, ScopeCtxInner};
use crate::arena::Id;

pub fn parse_function(
    def: FunctionDef,
    fimpl: FunctionImpl,
    ctx: &mut GlobalCtx,
) -> Result<Id<FunctionObject>, Error> {
    let FunctionDef {
        name,
        generics,
        type_def,
    } = def;
    let FunctionImpl(impl_name, args, body) = fimpl;

    let mut ty_scope = ScopeCtx::new(ctx);
    generics
        .into_iter()
        .for_each(|g| {
            ty_scope.alloc_generic(Generic::parse(g.inner()));
        });
    let func_type = parse_type(
        type_def.as_ref(),
        &mut ty_scope,
    )?;

    let count_args = ty_scope.get_type(func_type).count_args(&ty_scope);
    let arg_types = Type::args_types(func_type, &ty_scope);

    match args.len() == count_args as usize {
        false => Err(Error::Custom(
            impl_name.span,
            format!("Expected {} arguments, found {}", count_args, args.len()),
            "-in this function".to_owned(),
        )),
        true => {
            let mut body_scope = ScopeCtx::new(ctx);
            args.into_iter()
                .zip(arg_types.clone().into_iter())
                .for_each(|(name, t)| {
                    let id = Type::move_into_scope(
                        ty_scope.get_type(t),
                        ty_scope.types(),
                        &mut body_scope.types
                    );
                    body_scope.alloc_arg(Arg { name, ty: id });
                });
            let f_return_type = ty_scope.get_type(func_type).get_return_value(&ty_scope).move_into_scope(ty_scope.types(), &mut body_scope.types);
            let body_scope = body_scope.inner();
            let f_id = ctx.alloc_func(FunctionObject {
                def: FunctionDefinition { name, ftype: func_type },
                args: body_scope.args.iter().cloned().collect(),
                body: Expr::empty(),
                ctx: ty_scope.inner(),
            });
            let mut body_scope = ScopeCtx::from_inner(ctx, body_scope);
            let (expr, inner) = {
                let expr_res = parse_expr(
                    &body.0,
                    &mut body_scope,
                    &mut HashMap::new(),
                    Some(f_return_type),
                );
                match expr_res {
                    Ok(x) => (x.unwrap(), body_scope.inner()),
                    Err(e) => {
                        body_scope.inner();
                        ctx.remove_func();
                        return Err(e);
                    }
                }
            };

            ctx.get_func_mut(f_id).body = expr;
            ctx.insert_func_ctx(inner);

            Ok(f_id)
        }
    }
}

/*
fn initialize_bool() -> (AllObject, Rc<Spanned<Type>>) {
      const BOOL: AllObject = AllObject::Enum(Rc::new(EnumType::from_ast(EnumDecl {
            name: Spanned::new("Bool".to_owned(), Span::new(0, 0)),
            variants: vec![
                Spanned::new(EnumVariant {
                    name: Spanned::new("True".to_owned(), Span::new(0, 0)),
                    kind: EnumVariantKind::Unit
                }, Span::new(0, 0)),
                Spanned::new(EnumVariant {
                    name: Spanned::new("False".to_owned(), Span::new(0, 0)),
                    kind: EnumVariantKind::Unit
                }, Span::new(0, 0)),
            ]
        }, &Context { objects: vec![], parent: None }).unwrap()));
    };
    lazy_static! {
        static ref BOOL_TYPE: Rc<Spanned<Type>> = BOOL.call();
    };
    (BOOL, BOOL_TYPE)
}

fn get_bool_enum() -> AllObject {
    initialize_bool().0
}

fn get_bool_type() -> Rc<Spanned<Type>> {
    initialize_bool().1
}*/

pub fn parse_tokens(
    tokens: Vec<Spanned<TopLevelToken>>,
    global: &mut GlobalCtx,
) -> Result<Vec<Id<FunctionObject>>, Vec<Error>> {
    let mut errors = vec![];
    let mut function_defs = vec![];
    let mut function_impls = VecDeque::new();

    tokens.into_iter().for_each(|token| {
        match token.inner() {
            TopLevelToken::Type(_) => {
                unimplemented!();
                /*let name = ty.0.clone();
                match expr::parse_type(&ty.1, &ctx, &impls) {
                    Ok(ty) => ctx.objects.push(Object::Type(Rc::new(TypeObject {
                        name: name.clone().inner(),
                        def: ty,
                    }))),
                    Err(err) => errors.push(err),
                }*/
            }
            TopLevelToken::NewLine | TopLevelToken::Comment => {}
            TopLevelToken::FunctionDef(f) => function_defs.push(f),
            TopLevelToken::FunctionImpl(i) => function_impls.push_front(i),
            TopLevelToken::EnumDecl(e) => {
                match DataDef::parse(e, global) {
                    Ok(_) => { }
                    Err(e) => errors.push(e),
                }
            },
        }
    });

    let mut ids = vec![];
    function_defs.into_iter().for_each(|d| {
        let (idx, _) = match function_impls.iter().find_position(|i| i.0 == d.name) {
            Some(d) => d,
            None => {
                errors.push(Error::Custom(
                    d.name.span,
                    format!("Cannot find impl for function {}", d.name),
                    "-here".to_owned(),
                ));
                return;
            }
        };
        let fimpl = function_impls.remove(idx).unwrap();
        match parse_function(d, fimpl, global) {
            Ok(id) => {
                ids.push(id);
            },
            Err(e) => {
                errors.push(e)
            },
        }
    });

    if errors.is_empty() {
        Ok(ids)
    } else {
        Err(errors)
    }
}
