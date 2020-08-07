use crate::common::{Context, Error, Spanned};
use crate::ir::expr::parse_expr;
use crate::ir::objects::{Arg, DataDef, FunctionDefinition, FunctionObject, Object, TypeObject};
use crate::ir::types::{Generic, Type};
use crate::ir::{expr, Implementations};
use crate::syntax::ast::{FunctionDef, FunctionImpl, TopLevelToken};
use itertools::Itertools;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

pub fn parse_function(
    def: FunctionDef,
    fimpl: FunctionImpl,
    ctx: &Context<'_, Object>,
    impls: &mut Implementations,
) -> Result<Object, Error> {
    let FunctionDef {
        name,
        generics,
        type_def,
    } = def;
    let generics = generics
        .into_iter()
        .map(|g| {
            Object::Type(Rc::new(TypeObject {
                name: g.name.clone().inner(),
                def: Rc::new(Type::Generic(Generic {
                    name: g.inner().name,
                })),
            }))
        })
        .collect();
    let ctx = Context {
        objects: generics,
        parent: Some(&ctx),
    };
    let func_type = parse_expr(
        type_def.as_ref(),
        &ctx,
        &mut HashMap::new(),
        Some(Type::typ()),
    )?
    .unwrap() // TODO
    .convert_to_type(&ctx)?;
    let func_def = Rc::new(FunctionDefinition {
        name: name.clone(),
        ftype: func_type.clone(),
    });
    let count_args = func_type.count_args();
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
                .map(|(name, t)| Rc::new(Arg { name, ty: t }))
                .collect::<Vec<_>>();
            let ctx = Context {
                objects: Type::types_in_scope(&func_type)
                    .into_iter()
                    .map(|(name, ty)| Object::Type(Rc::new(TypeObject { name, def: ty })))
                    .collect(),
                parent: Some(&ctx),
            };
            let mut ctx = Context {
                objects: args.iter().map(|v| Object::Arg(v.clone())).collect(),
                parent: Some(&ctx),
            };
            ctx.objects
                .push(Object::FunctionDefinition(Rc::new(FunctionDefinition {
                    name: name.clone(),
                    ftype: func_type.clone(),
                })));
            let expr = parse_expr(
                &body.0,
                &ctx,
                &mut HashMap::new(),
                Some(func_type.get_return_value()),
            )?
            .unwrap(); // TODO

            let f = FunctionObject {
                def: func_def.clone(),
                //generics: vec![],
                args,
                body: expr,
            };
            impls.add_function(f);

            Ok(Object::FunctionDefinition(func_def))
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

pub fn parse_tokens<'a>(
    tokens: Vec<Spanned<TopLevelToken>>,
    top: Option<&'a Context<'a, Object>>,
) -> Result<(Context<'a, Object>, Implementations), Vec<Error>> {
    let mut impls = Implementations::new();
    let mut errors = vec![];
    let mut ctx = Context {
        objects: vec![],
        parent: top,
    };
    let mut function_defs = vec![];
    let mut function_impls = VecDeque::new();
    tokens.into_iter().for_each(|token| {
        let span = token.span;
        match token.inner() {
            TopLevelToken::Type(ty) => {
                let name = ty.0.clone();
                match expr::parse_type(&ty.1, &ctx) {
                    Ok(ty) => ctx.objects.push(Object::Type(Rc::new(TypeObject {
                        name: name.clone().inner(),
                        def: ty,
                    }))),
                    Err(err) => errors.push(err),
                }
            }
            TopLevelToken::NewLine | TopLevelToken::Comment => {}
            TopLevelToken::FunctionDef(f) => function_defs.push(f),
            TopLevelToken::FunctionImpl(i) => function_impls.push_front(i),
            TopLevelToken::EnumDecl(e) => match DataDef::parse(e, &ctx) {
                Ok(d) => {
                    let d = Rc::new(d);
                    ctx.objects.push(Object::Enum(d.clone()));
                    impls.add_data(d);
                }
                Err(e) => errors.push(e),
            },
        }
    });
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
        match parse_function(d, fimpl, &ctx, &mut impls) {
            Ok(o) => ctx.objects.push(o),
            Err(e) => errors.push(e),
        }
    });

    if errors.is_empty() {
        Ok((ctx, impls))
    } else {
        Err(errors)
    }
}
