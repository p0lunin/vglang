use std::ops::Deref;
use std::fmt::{Debug, Formatter};
use crate::common::{Span, Spanned};

#[derive(PartialEq, Clone)]
pub struct Token {
    pub span: Span,
    pub ast: Ast,
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("Token").field("ast", &self.ast).finish()
    }
}

impl Deref for Token {
    type Target = Ast;

    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

impl Token {
    pub fn new(span: Span, ast: Ast) -> Self {
        Token { span, ast }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Add(Box<Token>, Box<Token>),
    Sub(Box<Token>, Box<Token>),
    Mul(Box<Token>, Box<Token>),
    Div(Box<Token>, Box<Token>),
    Pow(Box<Token>, Box<Token>),
    And(Box<Token>, Box<Token>),
    Or(Box<Token>, Box<Token>),
    Gr(Box<Token>, Box<Token>),
    Le(Box<Token>, Box<Token>),
    GrEq(Box<Token>, Box<Token>),
    LeEq(Box<Token>, Box<Token>),
    Eq(Box<Token>, Box<Token>),
    NotEq(Box<Token>, Box<Token>),
    Neg(Box<Token>),
    Parenthesis(Box<Token>),
    Int(i128),
    Double(f64),
    Ident(String),
    Val,
    Slice(Slice),
    Implication(Box<Token>, Box<Token>),
    Named(Spanned<String>, Box<Token>),
    CallFunction(Box<Token>, Box<Token>),
    Dot(Box<Token>, Box<Token>),
}

#[derive(Debug, PartialEq)]
pub enum TopLevelToken {
    Type(Type),
    FunctionDef(FunctionDef),
    FunctionImpl(FunctionImpl),
    EnumDecl(EnumDecl),
    NewLine,
    Comment,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Type(pub Spanned<String>, pub Token);

#[derive(Debug, PartialEq)]
struct Var(Spanned<String>, Token);

#[derive(Debug, PartialEq, Clone)]
pub struct Slice {
    pub first: Spanned<i128>,
    pub second: Spanned<i128>,
    pub last: Spanned<i128>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDef {
    pub name: Spanned<String>,
    pub generics: Vec<Spanned<Generic>>,
    pub type_def: Box<Token>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionImpl(
    pub Spanned<String>,
    pub Vec<Spanned<String>>,
    pub FunctionBody,
);

#[derive(Debug, PartialEq)]
pub struct FunctionBody(pub Token);

#[derive(Debug, PartialEq)]
pub struct EnumDecl {
    pub name: Spanned<String>,
    pub variants: Vec<Spanned<EnumVariant>>,
    pub generics: Vec<Spanned<Generic>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumVariant {
    pub name: Spanned<String>,
    pub kind: EnumVariantKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum EnumVariantKind {
    Unit,
    WithData(Vec<Token>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Generic {
    pub name: Spanned<String>,
}

