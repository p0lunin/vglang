use crate::common::{Context, Error, Span, Spanned};
use crate::ir::expr::Expr;
use crate::ir::objects::{AllObject, TypeObject};
use crate::ir::type_check::type_check_expr;
use crate::ir::types::base_types::Function;
use crate::ir::types::{parse_type_helper, OneTypeKind, Type};
use crate::ir::IrContext;
use crate::syntax::ast;
use crate::syntax::ast::{EnumDecl, EnumVariantKind};
use itertools::Itertools;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Generic {
    name: Spanned<String>,
}

pub struct EnumType {
    pub(crate) decl: EnumDecl,
    variants: Vec<Rc<EnumVariant>>,
    generics: Vec<Spanned<Generic>>,
}

impl Debug for EnumType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("EnumType")
            .field("generics", &self.generics)
            .field(
                "variants",
                &self.variants.iter().map(|v| &v.orig).collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl PartialEq for EnumType {
    fn eq(&self, other: &Self) -> bool {
        self.decl == other.decl && self.generics == other.generics
    }
}

impl EnumType {
    pub fn from_ast(
        data: EnumDecl,
        ctx: &Context<'_, AllObject>,
        ir_ctx: &mut IrContext,
    ) -> Result<Rc<RefCell<Self>>, Error> {
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
        let enum_def = Rc::new(RefCell::new(EnumType {
            decl: data.clone(),
            variants: vec![],
            generics: generics.clone(),
        }));
        let enum_obj = AllObject::Enum(enum_def.clone());
        let ctx = Context {
            objects: generics
                .iter()
                .map(|g| {
                    AllObject::Type(Rc::new(TypeObject {
                        name: g.name.clone(),
                        ttype: Rc::new(RefCell::new(Type::Generic(g.name.clone()))),
                    }))
                })
                .chain(vec![enum_obj])
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
                                    parse_type_helper(token, &ctx, ir_ctx)
                                        .map(|res| Rc::new(RefCell::new(res)))
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                        ),
                    },
                }))
            })
            .collect::<Result<Vec<_>, _>>()?;
        enum_def.borrow_mut().variants = variants;
        Ok(enum_def)
    }

    pub fn span(&self) -> Span {
        self.decl.name.span
    }

    pub fn name(&self) -> &str {
        &self.decl.name
    }

    pub fn has_variant(&self, variant: &EnumVariantInstance) -> bool {
        self.variants
            .iter()
            .any(|v| Rc::ptr_eq(v, &variant.variant))
    }

    pub fn call(th: &Rc<RefCell<Self>>) -> Result<AllObject, Error> {
        let this = th.borrow();
        match this.generics.is_empty() {
            true => {
                mem::drop(this);
                Ok(AllObject::EnumInstance(Rc::new(EnumInstance {
                    orig: th.clone(),
                    generics: vec![],
                    variants: vec![],
                })))
            }
            false => {
                let types = this
                    .generics
                    .iter()
                    .map(|g| Rc::new(RefCell::new(Type::Generic(g.name.clone()))))
                    .collect::<Vec<_>>();
                mem::drop(this);
                Ok(AllObject::CreateEnumInstanceFunc(CreateEnumInstanceFunc {
                    ftype: create_func_from_types(
                        &types,
                        Rc::new(RefCell::new(Type::Enum(th.clone()))),
                    ),
                    orig: th.clone(),
                    generics: HashMap::new(),
                }))
            }
        }
    }

    pub fn call_with_arg_expr(
        th: &Rc<RefCell<Self>>,
        expr: Expr,
        span: Span,
    ) -> Result<AllObject, Error> {
        let this = th.borrow();
        match this.generics.is_empty() {
            true => Err(Error::Span(span)),
            false => {
                let mut types = this
                    .generics
                    .iter()
                    .map(|g| Rc::new(RefCell::new(Type::Generic(g.name.clone()))))
                    .collect::<Vec<_>>();
                let mut map = HashMap::new();
                map.insert(this.generics.first().unwrap().name.clone().inner(), expr);
                types.remove(0);
                Ok(AllObject::CreateEnumInstanceFunc(CreateEnumInstanceFunc {
                    ftype: create_func_from_types(
                        &types,
                        Rc::new(RefCell::new(Type::Enum(th.clone()))),
                    ),
                    orig: th.clone(),
                    generics: map,
                }))
            }
        }
    }

    pub fn try_get_member(this: &Rc<RefCell<Self>>, name: &str) -> Option<AllObject> {
        this.borrow()
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
        f.write_str(&self.decl.name)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CreateEnumInstanceFunc {
    ftype: Rc<RefCell<Type>>,
    orig: Rc<RefCell<EnumType>>,
    generics: HashMap<String, Expr>,
}

impl CreateEnumInstanceFunc {
    pub fn name(&self) -> &str {
        unsafe { mem::transmute(self.orig.borrow().name()) }
    }

    pub fn call(&self) -> Rc<RefCell<Type>> {
        self.ftype.clone()
    }

    pub fn call_with_arg_expr(&self, arg: Expr, ctx: &mut IrContext) -> Result<AllObject, Error> {
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
                let mut generics_types = Vec::new();
                new_data.into_iter().for_each(|(name, data)| {
                    let ty = data.try_get_type().unwrap();
                    generics.insert(name, ty.clone());
                    generics_types.push(ty);
                });
                let variants = self
                    .orig
                    .borrow()
                    .variants
                    .iter()
                    .map(|v| {
                        Rc::new(EnumInstanceVariantInstance {
                            variant: v.clone(),
                            data: monomorphization(v.data.get_data(), &generics),
                        })
                    })
                    .collect();
                Ok(AllObject::EnumInstance(ctx.create_specialized_enum(
                    EnumInstance {
                        orig: self.orig.clone(),
                        generics: generics_types,
                        variants,
                    },
                )))
            }
        }
    }
}

pub fn monomorphization<'a>(
    data: impl Iterator<Item=&'a Rc<RefCell<Type>>>,
    generics: &HashMap<String, Rc<RefCell<Type>>>,
) -> Vec<Rc<RefCell<Type>>> {
    data.map(|t| match Type::get_inner_cell(t).borrow().deref() {
        Type::Generic(n) => generics.get(n.as_str()).unwrap().clone(),
        _ => t.clone(),
    })
        .collect()
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumInstanceVariantInstance {
    variant: Rc<EnumVariant>,
    data: Vec<Rc<RefCell<Type>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumInstance {
    pub orig: Rc<RefCell<EnumType>>,
    pub generics: Vec<Rc<RefCell<Type>>>,
    pub variants: Vec<Rc<EnumInstanceVariantInstance>>,
}

impl EnumInstance {
    pub fn name(&self) -> &str {
        unsafe { mem::transmute(self.orig.borrow().name()) }
    }

    pub fn span(&self) -> Span {
        self.orig.borrow().span()
    }

    pub fn call(self: &Rc<EnumInstance>) -> Rc<RefCell<Type>> {
        Rc::new(RefCell::new(Type::EnumInstance(self.clone())))
    }

    pub fn try_get_member(self: &Rc<Self>, name: &str) -> Option<AllObject> {
        self.orig
            .borrow()
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
        self.variants.iter().any(|v| variant.is_part_of(v))
    }

    pub fn is_part_of(&self, other: &EnumInstance) -> bool {
        Rc::ptr_eq(&self.orig, &other.orig)
            && self
            .generics
            .iter()
            .zip(other.generics.iter())
            .all(|(l, r)| l.borrow().is_part_of(r.borrow().deref()))
    }
}

impl Display for EnumInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        Display::fmt(&self.orig.borrow(), f)?;
        match self.generics.is_empty() {
            true => {}
            false => {
                f.write_str("<")?;
                f.write_str(
                    &self
                        .generics
                        .iter()
                        .map(|t| t.borrow().to_string())
                        .join(", "),
                );
                f.write_str(">")?
            }
        }
        Ok(())
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
    orig: Spanned<ast::EnumVariant>,
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
        ctx: &Context<'_, AllObject>,
        ir_ctx: &mut IrContext,
    ) -> Result<Rc<RefCell<Type>>, Error> {
        self.data
            .iter()
            .zip(self.variant.data.get_data())
            .map(|(expr, ty)| {
                let expr_ty = type_check_expr(expr, ctx, ir_ctx)?;
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
        self.variant == var.variant
            && self.data.iter().zip(var.data.iter()).all(|(e, ty)| {
            e.try_get_type()
                .unwrap()
                .borrow()
                .is_part_of(ty.borrow().deref())
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
