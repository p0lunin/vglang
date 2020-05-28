use crate::syntax::ast::{FunctionDef, FunctionImpl, TopLevelToken};
use crate::common::{Context, Error, Spanned};
use crate::ir::objects::{AllObject, Object, Arg, FunctionDefinition, FunctionObject, EnumType};
use std::rc::Rc;
use std::cell::RefCell;
use crate::ir::types::{parse_type_helper, Type};
use crate::ir::expr::parse_expr;
use crate::ir::types;
use std::collections::VecDeque;
use itertools::Itertools;

pub fn parse_function(
    def: FunctionDef,
    fimpl: FunctionImpl,
    ctx: &Context<'_, AllObject>,
) -> Result<AllObject, Error> {
    let FunctionDef(name, def) = def;
    let func_type = Rc::new(RefCell::new(parse_type_helper(*def, ctx)?));
    let count_args = func_type.borrow().count_args();
    let FunctionImpl(impl_name, args, body) = fimpl;
    let arg_types = Type::args_types(&func_type);
    match args.len() == count_args as usize {
        false => Err(Error::Custom(
            impl_name.span,
            format!("Expected {} arguments, found {}", count_args, args.len()),
            "-here".to_owned(),
        )),
        true => {
            let args = args
                .into_iter()
                .zip(arg_types.clone().into_iter())
                .map(|(name, t)| {
                    Rc::new(Object {
                        name,
                        object: Arg { atype: t },
                    })
                })
                .collect::<Vec<_>>();
            let ctx = Context {
                objects: Type::types_in_scope(&func_type)
                    .into_iter()
                    .map(|(name, ty)| AllObject::Type(Rc::new(Object { name, object: ty })))
                    .collect(),
                parent: Some(ctx),
            };
            let mut ctx = Context {
                objects: args.iter().map(|v| AllObject::Arg(v.clone())).collect(),
                parent: Some(&ctx),
            };
            ctx.objects
                .push(AllObject::FunctionDefinition(Rc::new(FunctionDefinition {
                    name: name.clone(),
                    ftype: func_type.clone(),
                })));
            let expr = parse_expr(body.0, &ctx)?;

            Ok(AllObject::Function(Rc::new(Object {
                name,
                object: FunctionObject {
                    args,
                    ftype: func_type,
                    body: Rc::new(expr),
                },
            })))
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

pub fn parse_tokens<'a>(tokens: Vec<Spanned<TopLevelToken>>) -> Result<Context<'a, AllObject>, Vec<Error>> {
    let mut errors = vec![];
    let mut ctx = Context {
        objects: vec![],
        parent: None,
    };
    let mut function_defs = vec![];
    let mut function_impls = VecDeque::new();
    tokens.into_iter().for_each(|token| {
        let span = token.span;
        match token.inner() {
            TopLevelToken::Type(ty) => {
                let name = ty.0.clone();
                match types::parse_type(Spanned::new(ty, span), &ctx) {
                    Ok(ty) => ctx
                        .objects
                        .push(AllObject::Type(Rc::new(Object { name, object: ty }))),
                    Err(err) => errors.push(err),
                }
            }
            TopLevelToken::NewLine | TopLevelToken::Comment => {}
            TopLevelToken::FunctionDef(f) => function_defs.push(f),
            TopLevelToken::FunctionImpl(i) => function_impls.push_front(i),
            TopLevelToken::EnumDecl(e) => match EnumType::from_ast(e, &ctx) {
                Ok(e) => ctx.objects.push(AllObject::Enum(Rc::new(e))),
                Err(err) => errors.push(err),
            },
        }
    });
    function_defs.into_iter().for_each(|d| {
        let (idx, _) = match function_impls.iter().find_position(|i| i.0 == d.0) {
            Some(d) => d,
            None => {
                errors.push(Error::Custom(
                    d.0.span,
                    format!("Cannot find impl for function {}", d.0),
                    "-here".to_owned(),
                ));
                return;
            }
        };
        let fimpl = function_impls.remove(idx).unwrap();
        match parse_function(d, fimpl, &ctx) {
            Ok(o) => ctx.objects.push(o),
            Err(e) => errors.push(e),
        }
    });

    if errors.is_empty() {
        Ok(ctx)
    } else {
        Err(errors)
    }
}
