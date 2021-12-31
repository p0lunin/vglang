use crate::common::{Span, Spanned, BinOp};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

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
    BinOp(Box<Token>, Box<Token>, BinOp),
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
    Let {
        var: Spanned<String>,
        assign: Box<Token>,
        expr: Box<Token>,
    },
    IfThenElse {
        if_: Box<Token>,
        then: Box<Token>,
        else_: Box<Token>,
    },
    CaseExpr(Box<CaseExpr>),
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

#[derive(Debug, PartialEq, Clone)]
pub struct EnumDecl {
    pub name: Spanned<String>,
    pub variants: Vec<Spanned<EnumVariant<Token>>>,
    pub generics: Vec<Spanned<Generic>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumVariant<T> {
    pub name: Spanned<String>,
    pub datas: Vec<T>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Generic {
    pub name: Spanned<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseExpr {
    pub cond: Token,
    pub arms: Vec<CaseArm>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseArm {
    pub pat: Spanned<Pattern>,
    pub arm: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Otherwise,
    Ident(String),
    Variant(Path, Vec<Spanned<Pattern>>),
    Bind(Spanned<String>, Box<Spanned<Pattern>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Path {
    Place(String),
    Path(String, Box<Path>),
}

impl Path {
    pub fn end(&self) -> &str {
        match self {
            Path::Path(_, next) => next.end(),
            Path::Place(s) => s.as_str(),
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Path::Place(s) => f.write_str(s.as_str()),
            Path::Path(s, next) => write!(f, "{}.{}", s, next),
        }
    }
}
