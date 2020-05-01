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
    Ident(Ident),
    Val,
    Slice(Slice)
}

#[derive(Debug, PartialEq)]
pub enum TopLevelToken {
    Type(Type),
    NewLine,
    Comment,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub String);

impl Ident {
    pub fn is_val(&self) -> bool {
        self.0.as_str() == "val"
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Type(pub Spanned<Ident>, pub Token);

#[derive(Debug, PartialEq)]
struct Var(Spanned<Ident>, Token);

#[derive(Debug, PartialEq, Clone)]
pub struct Slice {
    first: Spanned<i128>,
    second: Spanned<i128>,
    last: Spanned<i128>,
}

peg::parser! { grammar lang() for str {
    pub rule parse_lang() -> Vec<Spanned<TopLevelToken>>
        = (type_declaration() / new_line() / comment())*

    rule comment() -> Spanned<TopLevelToken>
        = start:position!() "//" ([_])* new_line()? end:position!() { Spanned::new(TopLevelToken::Comment, Span::new(start, end)) }

    rule new_line() -> Spanned<TopLevelToken>
        = start:position!() "\r"? "\n" "\r"? end:position!() { Spanned::new(TopLevelToken::NewLine, Span::new(start, end)) }

    pub rule type_declaration() -> Spanned<TopLevelToken>
        = start:position!() "type" __ ident:ident() _ "=" inli(1) def:type_definition(1) end:position!() {
            Spanned::new(TopLevelToken::Type(Type(ident, def)), Span::new(start, end))
        }

    pub rule type_definition(i: usize) -> Token
        = logic(i)

    pub rule logic(i: usize) -> Token = precedence! {
        x:(@) inli(i) "|" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Or(Box::new(x), Box::new(y))) }
        --
        x:(@) inli(i) "&" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::And(Box::new(x), Box::new(y))) }
        --
        x:(@) inli(i) ">" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Gr(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "<" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Le(Box::new(x), Box::new(y))) }
        x:(@) inli(i) ">=" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::GrEq(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "<=" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::LeEq(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "==" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Eq(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "!=" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::NotEq(Box::new(x), Box::new(y))) }
        --
        d:block(i, <logic(i)>) { d }
        x:arithmetic(i) {x}
    }
    pub rule arithmetic(i: usize) -> Token = precedence! {
        x:(@) inli(i) "+" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Add(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "-" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Sub(Box::new(x), Box::new(y))) }
        start:position!() inli(i) "-" inli(i) x:@ { Token::new(Span::new(start, x.span.end), Ast::Neg(Box::new(x))) }
        --
        x:(@) inli(i) "*" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Mul(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "/" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Div(Box::new(x), Box::new(y))) }
        --
        x:@ inli(i) "^" inli(i) y:(@) { Token::new(x.span.extend(&y.span), Ast::Pow(Box::new(x), Box::new(y))) }
        --
        start:position!() "(" inli(i) v:logic(i) inli(i) ")" end:position!() {
            Token::new(Span::new(start, end), Ast::Parenthesis(Box::new(v)))
        }
        d:block(i, <logic(i)>) { d }
        x:single(i) {x}
    }

    rule single(i: usize) -> Token
        = num() /
        s:position!() "val" e:position!() { Token::new(Span::new(s, e), Ast::Val) } /
        id:ident() { Token::new(id.span, Ast::Ident(id.inner())) } /
        s:position!() slice:slice() e:position!() { Token::new(Span::new(s, e), Ast::Slice(slice)) }

    rule slice() -> Slice
        = "[" _ first:spanned_int() _ "," _ second:spanned_int() _ ".." _ last:spanned_int() _ "]" {
            Slice { first, second, last }
        }

    pub rule num() -> Token
        = d:double() / int()

    rule double() -> Token
        = l:digit() "." r:digit() {
            let (l, spanl) = l;
            let (r, spanr) = r;
            let span = spanl.extend(&spanr);
            let digit = format!("{}.{}", l, r).as_str().parse().unwrap();
            Token::new(span, Ast::Double(digit))
        }

    rule int() -> Token
        = n:digit() {
            Token::new(n.1, Ast::Int(n.0.parse().unwrap()))
        }

    rule spanned_int() -> Spanned<i128>
        = n:digit() {
            Spanned::new(n.0.parse().unwrap(), n.1)
        }

    pub rule digit() -> (&'input str, Span)
        = s:position!() n:$(['0'..='9']+) e:position!() {
            (n, Span::new(s, e))
        }
    rule ident() -> Spanned<Ident>
        = s:position!() ident:$(['a'..='z'|'A'..='Z'|'0'..='9'|'_']+) e:position!() {
            Spanned::new(Ident(String::from(ident)), Span::new(s, e))
        }

    rule block<T>(i: usize, rul: rule<T>) -> T
        = nli(i) token:rul() { token }

    rule _() = [' ']*
    rule __() = [' ']+
    // Indentation, new line, indentation
    rule inli(i: usize) = _ nli(i)?
    // New line with indentation
    rule nli(i: usize) = ("\r"? "\n" "\r"?) indentation(i)
    rule indentation(i: usize) = [' ']*<{i*4}>
}}

use crate::spanned::{Span, Spanned};
use lang::{arithmetic, num, parse_lang, type_declaration};
use std::convert::TryFrom;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;

pub fn parse(
    data: &str,
) -> Result<Vec<Spanned<TopLevelToken>>, peg::error::ParseError<peg::str::LineCol>> {
    parse_lang(data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_digit() {
        assert_eq!(
            num("1.2"),
            Ok(Token::new(Span::new(0, 3), Ast::Double(1.2)))
        );
        assert_eq!(num("1"), Ok(Token::new(Span::new(0, 1), Ast::Int(1))));
    }

    #[test]
    fn parse_expr() {
        assert_eq!(
            arithmetic("1.2+3", 0),
            Ok(Token::new(
                Span::new(0, 5),
                Ast::Add(
                    Box::new(Token::new(Span::new(0, 3), Ast::Double(1.2))),
                    Box::new(Token::new(Span::new(4, 5), Ast::Int(3)))
                )
            ))
        )
    }

    #[test]
    fn parse_indentation() {
        assert!(arithmetic("\n    1.2+3", 1).is_ok())
    }

    #[test]
    fn parse_type() {
        let data = "type I32 = Int &\n    val > -2^16 & val < 2^16-1";
        type_declaration(data).unwrap();
    }

    #[test]
    fn parse_type_def() {
        assert_eq!(
            type_declaration("type Foo = x \n    & 3 > 0"),
            Ok(Spanned::new(
                TopLevelToken::Type(Type(
                    Spanned::new(Ident("Foo".to_string()), Span::new(5, 8)),
                    Token::new(
                        Span::new(11, 25),
                        Ast::And(
                            Box::new(Token::new(
                                Span::new(11, 12),
                                Ast::Ident(Ident("x".to_string()))
                            )),
                            Box::new(Token::new(
                                Span::new(20, 25),
                                Ast::Gr(
                                    Box::new(Token::new(Span::new(20, 21), Ast::Int(3))),
                                    Box::new(Token::new(Span::new(24, 25), Ast::Int(0),))
                                )
                            ))
                        )
                    )
                )),
                Span::new(0, 25),
            ))
        )
    }
}
