// TODO: custom ids for global and scope ctx.

use crate::arena::{Arena, Id};
use crate::common::{Find, PathToFind};
use crate::ir::objects::{Arg, DataDef, DataVariant, FunctionObject, Object, Var};
use crate::ir::types::{Concrete, Generic, Type};

#[derive(Debug, Clone)]
pub struct GlobalCtx {
    funcs: Arena<FunctionObject>,
    datas: Arena<DataDef>,
    funcs_ctxs: Vec<ScopeCtxInner>,
    need_func_ctx: bool,
}

impl GlobalCtx {
    pub fn new() -> Self {
        GlobalCtx {
            funcs: Arena::new(),
            datas: Arena::new(),
            funcs_ctxs: vec![],
            need_func_ctx: false,
        }
    }
}

impl GlobalCtx {
    pub fn alloc_func(&mut self, func: FunctionObject) -> Id<FunctionObject> {
        assert!(!self.need_func_ctx);
        self.need_func_ctx = true;
        self.funcs.alloc(func)
    }
    #[inline(always)]
    pub fn alloc_data(&mut self, d: DataDef) -> Id<DataDef> {
        self.datas.alloc(d)
    }
    pub fn insert_func_ctx(&mut self, ctx: ScopeCtxInner) {
        assert!(self.need_func_ctx);
        self.need_func_ctx = false;
        self.funcs_ctxs.push(ctx);
    }
    pub fn remove_func(&mut self) {
        assert!(self.need_func_ctx);
        self.need_func_ctx = false;
        self.funcs.pop();
    }
    #[inline(always)]
    pub fn get_data(&self, id: Id<DataDef>) -> &DataDef {
        self.datas.get(id).unwrap()
    }
    #[inline(always)]
    pub fn get_data_mut(&mut self, id: Id<DataDef>) -> &mut DataDef {
        self.datas.get_mut(id).unwrap()
    }
    #[inline(always)]
    pub fn get_func(&self, id: Id<FunctionObject>) -> &FunctionObject {
        self.funcs.get(id).unwrap()
    }
    #[inline(always)]
    pub fn get_func_mut(&mut self, id: Id<FunctionObject>) -> &mut FunctionObject {
        self.funcs.get_mut(id).unwrap()
    }
    #[inline(always)]
    pub fn get_data_variant(&self, id: Id<DataDef>, var_idx: usize) -> &DataVariant {
        &self.get_data(id).variants[var_idx]
    }
    pub fn iter_funcs<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&FunctionObject, &ScopeCtxInner)> + 'a {
        assert!(!self.need_func_ctx);
        self.funcs.iter().zip(self.funcs_ctxs.iter())
    }
}

impl Find for GlobalCtx {
    type Item = Object;

    fn find(&self, path: PathToFind) -> Option<Self::Item> {
        self.funcs
            .find(path.clone())
            .map(Object::Function)
            .or_else(|| self.datas.find(path))
    }
}

#[derive(Debug)]
pub struct ScopeCtx<'a> {
    pub global: &'a GlobalCtx,
    pub types: Arena<Type>,
    pub generics: Arena<Generic>,
    pub args: Arena<Arg>,
    pub vars: ScopedArena<Var>,
    pub scope: usize,
}

impl<'a> ScopeCtx<'a> {
    pub fn new(global: &'a GlobalCtx) -> Self {
        ScopeCtx {
            global,
            types: Arena::new(),
            generics: Arena::new(),
            vars: ScopedArena::new(),
            args: Arena::new(),
            scope: 0,
        }
    }
    pub fn from_inner(global: &'a GlobalCtx, inner: ScopeCtxInner) -> Self {
        ScopeCtx {
            global,
            types: inner.types,
            generics: inner.generics,
            vars: inner.vars,
            args: inner.args,
            scope: 0,
        }
    }
    #[inline(always)]
    pub fn get_type(&self, id: Id<Type>) -> &Type {
        self.types.get(id).unwrap()
    }
    #[inline(always)]
    pub fn get_var(&self, id: Id<Var>) -> &Var {
        self.vars.get(id)
    }
    #[inline(always)]
    pub fn get_arg(&self, id: Id<Arg>) -> &Arg {
        self.args.get(id).unwrap()
    }
    #[inline(always)]
    pub fn get_generic(&self, id: Id<Generic>) -> &Generic {
        self.generics.get(id).unwrap()
    }
    #[inline(always)]
    pub fn alloc_type(&mut self, d: Type) -> Id<Type> {
        self.types.alloc(d)
    }
    #[inline(always)]
    pub fn alloc_var(&mut self, d: Var) -> Id<Var> {
        self.vars.alloc(d)
    }
    #[inline(always)]
    pub fn alloc_arg(&mut self, d: Arg) -> Id<Arg> {
        self.args.alloc(d)
    }
    #[inline(always)]
    pub fn alloc_generic(&mut self, d: Generic) -> Id<Generic> {
        self.generics.alloc(d)
    }
    pub fn enter_scope(&mut self) {
        self.vars.enter_scope();
        self.scope += 1;
    }
    pub fn leave_scope(&mut self) {
        self.vars.drop_scope();
        self.scope -= 1;
    }
    pub fn types(&self) -> &Arena<Type> {
        &self.types
    }
    pub fn args(&self) -> &Arena<Arg> {
        &self.args
    }
    pub fn inner(self) -> ScopeCtxInner {
        ScopeCtxInner {
            types: self.types,
            generics: self.generics,
            args: self.args,
            vars: self.vars,
        }
    }
    pub fn extend_by(&mut self, other: ScopeCtx) {
        let ScopeCtxInner {
            types,
            generics,
            args,
            vars,
        } = other.inner();
        self.types.extend_by(types);
        assert_eq!(generics.len(), 0);
        assert_eq!(args.len(), 0);
        assert_eq!(vars.arena.len(), 0);
    }
}

#[derive(Debug, Clone)]
pub struct ScopeCtxInner {
    pub types: Arena<Type>,
    pub generics: Arena<Generic>,
    pub args: Arena<Arg>,
    pub vars: ScopedArena<Var>,
}

impl ScopeCtxInner {
    pub fn empty() -> Self {
        ScopeCtxInner {
            types: Arena::new(),
            generics: Arena::new(),
            args: Arena::new(),
            vars: ScopedArena::new(),
        }
    }
}

impl Find for ScopeCtx<'_> {
    type Item = Object;

    fn find(&self, path: PathToFind) -> Option<Self::Item> {
        self.vars
            .find(path.clone())
            .or_else(|| self.args.find(path.clone()).map(Object::Arg))
            .or_else(|| self.generics.find(path.clone()).map(Object::Generic))
            .or_else(|| self.global.find(path))
    }
}

impl ScopeCtx<'_> {
    pub fn alloc_bool(&mut self) -> Id<Type> {
        match self.global.find(PathToFind::name("Bool")).unwrap() {
            Object::Enum(x) => self.alloc_type(Type::Data(Concrete::base(x))),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopedArena<T> {
    arena: Arena<T>,
    size: Vec<u16>,
}

impl<T> ScopedArena<T> {
    pub fn new() -> Self {
        ScopedArena {
            arena: Arena::new(),
            size: vec![0],
        }
    }
    pub fn alloc(&mut self, value: T) -> Id<T> {
        *self.size.last_mut().unwrap() += 1;
        self.arena.alloc(value)
    }
    pub fn get(&self, id: Id<T>) -> &T {
        self.arena.get(id).unwrap()
    }
    pub fn enter_scope(&mut self) {
        self.size.push(0);
    }
    pub fn drop_scope(&mut self) {
        match self.size.as_slice() {
            [] => {
                #[cfg(debug_assertions)]
                unimplemented!("Try to drop non-existent scope");
                #[cfg(not(debug_assertions))]
                unsafe {
                    std::hint::unreachable_unchecked()
                }
            }
            [.., x] => {
                self.arena.remove_lasts(*x as usize);
                self.size.pop();
            }
        }
    }
}

impl Find for ScopedArena<Var> {
    type Item = Object;

    fn find(&self, path: PathToFind) -> Option<Object> {
        if path.has_segments() {
            return None;
        }

        self.arena.iter_with_ids().rev().find_map(|(id, x)| {
            if x.name.as_str() == path.endpoint {
                Some(Object::Var(id))
            } else {
                None
            }
        })
    }
}
