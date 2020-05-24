use crate::error::Error;
use crate::object::{AllObject, Expr};
use crate::parser;
use crate::parser::{EnumDecl, EnumVariantKind};
use crate::spanned::{Span, Spanned};
use crate::type_check::{type_check_expr, Context};
use crate::types::{parse_type_helper, Function, OneTypeKind, Type};
use std::fmt::{Display, Formatter, Write};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct EnumType {
    decl: EnumDecl,
    variants: Vec<Rc<EnumVariant>>,
}

impl EnumType {
    pub fn from_ast(data: EnumDecl, ctx: &Context) -> Result<Self, Error> {
        let variants = data
            .variants
            .iter()
            .map(|variant| {
                let kind = variant.kind.clone();
                Ok(Rc::new(EnumVariant {
                    enum_name: (*data.name).clone(),
                    orig: (*variant).clone(),
                    data: match kind {
                        EnumVariantKind::Unit => EnumVariantData::Unit,
                        EnumVariantKind::WithData(d) => EnumVariantData::WithData(
                            d.into_iter()
                                .map(|token| {
                                    let span = token.span;
                                    parse_type_helper(token, ctx)
                                        .map(|res| Rc::new(Spanned::new(res, span)))
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                        ),
                    },
                }))
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self {
            decl: data,
            variants,
        })
    }

    pub fn span(&self) -> Span {
        self.decl.name.span
    }

    pub fn name(&self) -> &str {
        &self.decl.name
    }

    pub fn try_get_member(self: &Rc<Self>, name: &str) -> Option<AllObject> {
        self.variants
            .iter()
            .find(|v| v.name() == name)
            .map(|v| match &v.data {
                EnumVariantData::Unit => {
                    AllObject::EnumVariantInstance(Rc::new(EnumVariantInstance {
                        variant: v.clone(),
                        data: vec![],
                    }))
                }
                _ => AllObject::EnumVariant(v.clone()),
            })
    }

    pub fn has_variant(&self, variant: &Rc<EnumVariant>) -> bool {
        self.variants.iter().any(|v| Rc::ptr_eq(v, variant))
    }
}

impl Display for EnumType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(&self.decl.name)
    }
}

fn create_func_from_types(
    types: &[Rc<Spanned<Type>>],
    return_type: Rc<Spanned<Type>>,
) -> Rc<Spanned<Type>> {
    match types {
        [] => return_type,
        [x, xs @ ..] => Rc::new(Spanned::new(
            Type::Function(OneTypeKind::from_kind(Spanned::new(
                Function {
                    get_value: x.clone(),
                    return_value: create_func_from_types(xs, return_type),
                },
                x.span,
            ))),
            x.span,
        )),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumVariant {
    enum_name: String,
    orig: Spanned<parser::EnumVariant>,
    data: EnumVariantData,
}

impl EnumVariant {
    pub fn call_with_arg_expr(self: &Rc<EnumVariant>, expr: Expr) -> AllObject {
        match &self.data {
            EnumVariantData::Unit => unreachable!(),
            EnumVariantData::WithData(d) => {
                let f = CreateEnumVariantFunc {
                    func: create_func_from_types(
                        &d,
                        Rc::new(Spanned::new(Type::EnumVariant(self.clone()), self.span())),
                    ),
                    orig: self.clone(),
                    data: vec![],
                };
                f.call_with_arg_expr(expr).unwrap()
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumVariantInstance {
    variant: Rc<EnumVariant>,
    data: Vec<Expr>,
}

impl EnumVariantInstance {
    pub fn name(&self) -> &str {
        &self.variant.enum_name
    }
    pub fn span(&self) -> Span {
        self.variant.span()
    }
    pub fn origin(&self) -> &Rc<EnumVariant> {
        &self.variant
    }
    pub fn type_check_self(
        self: &Rc<EnumVariantInstance>,
        ctx: &Context,
    ) -> Result<Rc<Spanned<Type>>, Error> {
        self.data
            .iter()
            .zip(self.variant.data.get_data())
            .map(|(expr, ty)| {
                let expr_ty = type_check_expr(expr, ctx)?;
                match expr_ty.is_part_of(&ty) {
                    true => Ok(()),
                    false => Err(Error::Span(expr.span())),
                }
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|_| {
                Rc::new(Spanned::new(
                    Type::EnumVariantInstance(self.clone()),
                    self.span(),
                ))
            })
    }
}

impl Display for EnumVariantInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "instance of {}\n", self.variant)?;
        self.data.iter().for_each(|o| {
            write!(f, "   {}", o.try_get_type().unwrap()).unwrap();
        });
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum EnumVariantData {
    Unit,
    WithData(Vec<Rc<Spanned<Type>>>),
}

impl EnumVariantData {
    pub fn get_data(&self) -> impl Iterator<Item=&Rc<Spanned<Type>>> {
        match self {
            EnumVariantData::Unit => [].iter(),
            EnumVariantData::WithData(d) => d.iter(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CreateEnumVariantFunc {
    func: Rc<Spanned<Type>>,
    orig: Rc<EnumVariant>,
    data: Vec<Expr>,
}

impl CreateEnumVariantFunc {
    pub fn name(&self) -> &str {
        &self.orig.enum_name
    }

    pub fn call(&self) -> Rc<Spanned<Type>> {
        self.func.clone()
    }

    pub fn call_with_arg_expr(&self, arg: Expr) -> Result<AllObject, Error> {
        let new_type = self.func.try_curry().unwrap();
        let mut new_data = Vec::with_capacity(self.data.capacity() + 1);
        new_data.extend_from_slice(&self.data);
        new_data.push(arg);
        match new_type.is_function() {
            true => Ok(AllObject::CreateEnumVariantFunc(CreateEnumVariantFunc {
                func: new_type,
                orig: self.orig.clone(),
                data: new_data,
            })),
            false => Ok(AllObject::EnumVariantInstance(Rc::new(
                EnumVariantInstance {
                    variant: self.orig.clone(),
                    data: new_data,
                },
            ))),
        }
    }
}

impl EnumVariant {
    pub fn span(&self) -> Span {
        self.orig.span
    }

    pub fn name(&self) -> &str {
        &self.orig.name
    }
}

impl Display for EnumVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}.{}", self.enum_name, self.name())
    }
}
