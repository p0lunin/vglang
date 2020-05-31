use crate::common::{Context, Error, Spanned};
use crate::ir::expr::parse_expr;
use crate::ir::objects::{
    AllObject, Arg, EnumType, FunctionDefinition, FunctionObject, TypeObject,
};
use crate::ir::types::{parse_type_helper, Type};
use crate::ir::{types, IrContext};
use crate::syntax::ast::{FunctionDef, FunctionImpl, TopLevelToken};
use itertools::Itertools;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

pub fn parse_function(
    def: FunctionDef,
    fimpl: FunctionImpl,
    ctx: &Context<'_, AllObject>,
    ir_ctx: &mut IrContext,
) -> Result<AllObject, Error> {
    let FunctionDef {
        name,
        generics,
        type_def,
    } = def;
    let generics = generics
        .into_iter()
        .map(|g| {
            AllObject::Type(Rc::new(TypeObject {
                name: g.name.clone(),
                ttype: Rc::new(RefCell::new(Type::Generic(g.inner().name))),
            }))
        })
        .collect();
    let ctx = Context {
        objects: generics,
        parent: Some(&ctx),
    };
    let func_type = Rc::new(RefCell::new(parse_type_helper(*type_def, &ctx, ir_ctx)?));
    let func_def = Rc::new(FunctionDefinition {
        name: name.clone(),
        ftype: func_type.clone(),
    });
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
                .map(|(name, t)| Rc::new(Arg { name, atype: t }))
                .collect::<Vec<_>>();
            let ctx = Context {
                objects: Type::types_in_scope(&func_type)
                    .into_iter()
                    .map(|(name, ty)| AllObject::Type(Rc::new(TypeObject { name, ttype: ty })))
                    .collect(),
                parent: Some(&ctx),
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
            let expr = parse_expr(body.0, &ctx, ir_ctx)?;

            ir_ctx.add_function(FunctionObject {
                def: func_def.clone(),
                generics: vec![],
                args,
                body: Rc::new(expr),
            })?;

            Ok(AllObject::FunctionDefinition(func_def))
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
) -> Result<(Context<'a, AllObject>, IrContext), Vec<Error>> {
    let mut ir_ctx = IrContext::new();
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
                match types::parse_type(Spanned::new(ty, span), &ctx, &mut ir_ctx) {
                    Ok(ty) => ctx
                        .objects
                        .push(AllObject::Type(Rc::new(TypeObject { name, ttype: ty }))),
                    Err(err) => errors.push(err),
                }
            }
            TopLevelToken::NewLine | TopLevelToken::Comment => {}
            TopLevelToken::FunctionDef(f) => function_defs.push(f),
            TopLevelToken::FunctionImpl(i) => function_impls.push_front(i),
            TopLevelToken::EnumDecl(e) => match EnumType::from_ast(e, &ctx, &mut ir_ctx) {
                Ok(e) => ctx.objects.push(AllObject::Enum(e)),
                Err(err) => errors.push(err),
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
        match parse_function(d, fimpl, &ctx, &mut ir_ctx) {
            Ok(o) => ctx.objects.push(o),
            Err(e) => errors.push(e),
        }
    });

    if errors.is_empty() {
        Ok((ctx, ir_ctx))
    } else {
        Err(errors)
    }
}
