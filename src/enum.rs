use crate::error::Error;
use crate::object::{AllObject, Expr, Object};
use crate::parser;
use crate::parser::{EnumDecl, EnumVariantKind};
use crate::spanned::{Span, Spanned};
use crate::type_check::{type_check_expr, Context};
use crate::types::{parse_type_helper, Function, OneTypeKind, Type, TypeType};
use itertools::repeat_n;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Write};
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Generic {
    name: Spanned<String>,
}

#[derive(Debug, PartialEq)]
pub struct EnumType {
    pub(crate) decl: EnumDecl,
    variants: Vec<Rc<EnumVariant>>,
    generics: Vec<Spanned<Generic>>,
}

impl EnumType {
    pub fn from_ast(data: EnumDecl, ctx: &Context) -> Result<Self, Error> {
        let generics = data
            .generics
            .iter()
            .map(|g| {
                Spanned::new(
                    Generic {
                        name: g.name.clone(),
                    },
                    g.span,
                )
            })
            .collect::<Vec<_>>();
        let ctx = Context {
            objects: generics
                .iter()
                .map(|g| {
                    AllObject::Type(Rc::new(Object {
                        name: g.name.clone(),
                        object: Rc::new(RefCell::new(Type::Generic(g.name.clone()))),
                    }))
                })
                .collect(),
            parent: Some(ctx),
        };
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
                                    parse_type_helper(token, &ctx)
                                        .map(|res| Rc::new(RefCell::new(res)))
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
            generics,
        })
    }

    pub fn span(&self) -> Span {
        self.decl.name.span
    }

    pub fn name(&self) -> &str {
        &self.decl.name
    }

    pub fn call(self: &Rc<Self>, span: Span) -> Result<AllObject, Error> {
        match self.generics.is_empty() {
            true => Ok(AllObject::EnumInstance(Rc::new(EnumInstance {
                orig: self.clone(),
                variants: vec![],
            }))),
            false => {
                let types = self.generics.iter().map(|g| {
                    Rc::new(RefCell::new(Type::Generic(g.name.clone())))
                })
                    .collect::<Vec<_>>();
                Ok(AllObject::CreateEnumInstanceFunc(CreateEnumInstanceFunc {
                    ftype: create_func_from_types(&types, Rc::new(RefCell::new(Type::Enum(self.clone())))),
                    orig: self.clone(),
                    generics: HashMap::new(),
                }))
            }
        }
    }

    pub fn call_with_arg_expr(self: &Rc<Self>, expr: Expr, span: Span) -> Result<AllObject, Error> {
        match self.generics.is_empty() {
            true => Err(Error::Span(span)),
            false => {
                let mut types = self.generics.iter().map(|g| {
                    Rc::new(RefCell::new(Type::Generic(g.name.clone())))
                })
                    .collect::<Vec<_>>();
                let mut map = HashMap::new();
                map.insert(self.generics.first().unwrap().name.clone().inner(), expr);
                types.remove(0);
                Ok(AllObject::CreateEnumInstanceFunc(CreateEnumInstanceFunc {
                    ftype: create_func_from_types(&types, Rc::new(RefCell::new(Type::Enum(self.clone())))),
                    orig: self.clone(),
                    generics: map,
                }))
            }
        }
    }

    pub fn try_get_member(self: &Rc<Self>, name: &str) -> Option<AllObject> {
        self
            .variants
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
}

impl Display for EnumType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(&self.decl.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CreateEnumInstanceFunc {
    ftype: Rc<RefCell<Type>>,
    orig: Rc<EnumType>,
    generics: HashMap<String, Expr>,
}

impl CreateEnumInstanceFunc {
    pub fn name(&self) -> &str {
        &self.orig.name()
    }

    pub fn call(&self) -> Rc<RefCell<Type>> {
        self.ftype.clone()
    }

    pub fn call_with_arg_expr(&self, arg: Expr) -> Result<AllObject, Error> {
        let (left, new_type) = self.ftype.borrow().try_curry_with_arg().unwrap();
        let generic_name = match left.borrow().deref() {
            Type::Generic(g) => g.val.clone(),
            t => {
                dbg!(t);
                unreachable!()
            }
        };
        let mut new_data = HashMap::<String, Expr, _>::with_capacity(self.generics.len() + 1);
        new_data.extend(self.generics.clone().into_iter());
        new_data.insert(generic_name, arg);
        let borrowed = new_type.borrow();
        match borrowed.is_function() {
            true => {
                mem::drop(borrowed);
                Ok(AllObject::CreateEnumInstanceFunc(CreateEnumInstanceFunc {
                    ftype: new_type,
                    orig: self.orig.clone(),
                    generics: new_data,
                }))
            }
            false => {
                let mut generics = HashMap::new();
                new_data.into_iter().for_each(|(name, data)| {
                    generics.insert(name, data.try_get_type().unwrap());
                });
                let variants = self.orig.variants.iter().map(|v| {
                    Rc::new(EnumInstanceVariantInstance {
                        variant: v.clone(),
                        data: dbg!(monomorphization(&v.data, &generics)),
                    })
                }).collect();
                Ok(AllObject::EnumInstance(Rc::new(
                    EnumInstance {
                        orig: self.orig.clone(),
                        variants,
                    },
                )))
            },
        }
    }
}

fn monomorphization(data: &EnumVariantData, generics: &HashMap<String, Rc<RefCell<Type>>>) -> Vec<Rc<RefCell<Type>>> {
    match data {
        EnumVariantData::Unit => vec![],
        EnumVariantData::WithData(d) => {
            d.iter().map(|t| {
                match Type::get_inner_cell(t).borrow().deref() {
                    Type::Generic(n) => generics.get(n.as_str()).unwrap().clone(),
                    _ => t.clone()
                }
            }).collect()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumInstanceVariantInstance {
    variant: Rc<EnumVariant>,
    data: Vec<Rc<RefCell<Type>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumInstance {
    orig: Rc<EnumType>,
    variants: Vec<Rc<EnumInstanceVariantInstance>>,
}

impl EnumInstance {
    pub fn name(&self) -> &str {
        self.orig.name()
    }

    pub fn span(&self) -> Span {
        self.orig.span()
    }

    pub fn call(self: &Rc<EnumInstance>) -> Rc<RefCell<Type>> {
        Rc::new(RefCell::new(Type::EnumInstance(self.clone())))
    }

    pub fn try_get_member(self: &Rc<Self>, name: &str) -> Option<AllObject> {
        self.orig
            .variants
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

    pub fn has_variant(&self, variant: &Rc<EnumVariantInstance>) -> bool {
        self.variants.iter().any(|v| {
            variant.is_part_of(v)
        })
    }
}

impl Display for EnumInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        Display::fmt(&self.orig, f)
    }
}

fn create_func_from_types(
    types: &[Rc<RefCell<Type>>],
    return_type: Rc<RefCell<Type>>,
) -> Rc<RefCell<Type>> {
    match types {
        [] => return_type,
        [x, xs @ ..] => Rc::new(RefCell::new(Type::Function(OneTypeKind::from_kind(
            Spanned::new(
                Function {
                    get_value: x.clone(),
                    return_value: create_func_from_types(xs, return_type),
                },
                x.borrow().span(),
            ),
        )))),
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
                        Rc::new(RefCell::new(Type::EnumVariant(self.clone()))),
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
    ) -> Result<Rc<RefCell<Type>>, Error> {
        self.data
            .iter()
            .zip(self.variant.data.get_data())
            .map(|(expr, ty)| {
                let expr_ty = type_check_expr(expr, ctx)?;
                let borrowed = expr_ty.borrow();
                match borrowed.is_part_of(ty.borrow().deref()) {
                    true => Ok(()),
                    false => Err(Error::Span(expr.span())),
                }
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|_| Rc::new(RefCell::new(Type::EnumVariantInstance(self.clone()))))
    }
    pub fn is_part_of(&self, var: &Rc<EnumInstanceVariantInstance>) -> bool {
        self.variant == var.variant &&
            self.data.iter().zip(var.data.iter()).all(|(e, ty)| {
                e.try_get_type().unwrap().borrow().is_part_of(ty.borrow().deref())
            })
    }
}

impl Display for EnumVariantInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "instance of {}\n", self.variant)?;
        self.data.iter().for_each(|o| {
            write!(f, "   {}", o.try_get_type().unwrap().borrow()).unwrap();
        });
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum EnumVariantData {
    Unit,
    WithData(Vec<Rc<RefCell<Type>>>),
}

impl EnumVariantData {
    pub fn get_data(&self) -> impl Iterator<Item=&Rc<RefCell<Type>>> {
        match self {
            EnumVariantData::Unit => [].iter(),
            EnumVariantData::WithData(d) => d.iter(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CreateEnumVariantFunc {
    func: Rc<RefCell<Type>>,
    orig: Rc<EnumVariant>,
    data: Vec<Expr>,
}

impl CreateEnumVariantFunc {
    pub fn name(&self) -> &str {
        &self.orig.enum_name
    }

    pub fn call(&self) -> Rc<RefCell<Type>> {
        self.func.clone()
    }

    pub fn call_with_arg_expr(&self, arg: Expr) -> Result<AllObject, Error> {
        let new_type = self.func.borrow().try_curry().unwrap();
        let mut new_data = Vec::with_capacity(self.data.capacity() + 1);
        new_data.extend_from_slice(&self.data);
        new_data.push(arg);
        let borrowed = new_type.borrow();
        match borrowed.is_function() {
            true => {
                mem::drop(borrowed);
                Ok(AllObject::CreateEnumVariantFunc(CreateEnumVariantFunc {
                    func: new_type,
                    orig: self.orig.clone(),
                    data: new_data,
                }))
            }
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
