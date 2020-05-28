use crate::{
    syntax::ast::*,
    common::{Spanned, Span},
};

peg::parser! { grammar lang() for str {
    pub rule parse_lang() -> Vec<Spanned<TopLevelToken>>
        = (top_level_token())*

    rule top_level_token() -> Spanned<TopLevelToken>
        = type_declaration() / new_line() / comment() / FunctionDef(1) / function_impl(1) / enum_decl()

    rule comment() -> Spanned<TopLevelToken>
        = start:position!() "/*" (all_except_end_comment())* [_] "*/" end:position!() {
            Spanned::new(TopLevelToken::Comment, Span::new(start, end))
        } /
        start:position!() "//" (all_except_new_line())* (new_line() / ![_]) end:position!() {
            Spanned::new(TopLevelToken::Comment, Span::new(start, end))
        }

    rule all_except_end_comment() = [_] !"*/"

    rule all_except_new_line() = [_] !"\n"

    rule new_line() -> Spanned<TopLevelToken>
        = start:position!() "\r"? "\n" "\r"? end:position!() { Spanned::new(TopLevelToken::NewLine, Span::new(start, end)) }

    pub rule type_declaration() -> Spanned<TopLevelToken>
        = start:position!() "type" __ ident:ident() _ "=" inli(1) def:type_definition(1) end:position!() {
            Spanned::new(TopLevelToken::Type(Type(ident, def)), Span::new(start, end))
        }

    pub rule type_definition(i: usize) -> Token
        = logic(i)

    rule FunctionDef(i: usize) -> Spanned<TopLevelToken>
        = start:position!() name:ident() _ ":" inli(i) ftype:type_definition(i) new_line() end:position!() {
            Spanned::new(TopLevelToken::FunctionDef(FunctionDef(name, Box::new(ftype))), Span::new(start, end))
        }

    rule function_impl(i: usize) -> Spanned<TopLevelToken>
        = start:position!() name:ident() __ args:(ident_space())*  _ "=" _ body:type_definition(i) end:position!() {
            Spanned::new(TopLevelToken::FunctionImpl(FunctionImpl(name, args, FunctionBody(body))), Span::new(start, end))
        }

    rule ident_space() -> Spanned<String>
        = i:ident() __ { i }

    rule types_space(i: usize) -> Token
        = t:logic(i) _ { t }

    rule enum_decl() -> Spanned<TopLevelToken>
        = s:position!() "enum" __ id:ident() gs:generics(1) vs:(block(1, <enum_variant(1)>))+ e:position!() {
            Spanned::new(TopLevelToken::EnumDecl(EnumDecl {
                name: id,
                variants: vs,
                generics: gs,
            }), Span::new(s, e))
        }

    rule generics(i: usize) -> Vec<Spanned<Generic>>
        = "<" g:spaced(<generic(i)>) ** "," ">" { g }

    rule spaced<T>(r: rule<T>) -> T
        = _ data:r() _ { data }

    rule generic(i: usize) -> Spanned<Generic>
        = s:position!() name:ident() e:position!() { Spanned::new(Generic { name }, Span::new(s, e)) }

    rule enum_variant(i: usize) -> Spanned<EnumVariant>
        = s:position!() id:ident() _ types:(types_space((i+1)))* e:position!() {
            match types.len() {
                0 => Spanned::new(EnumVariant { name: id, kind: EnumVariantKind::Unit }, Span::new(s, e)),
                _ => Spanned::new(EnumVariant { name: id, kind: EnumVariantKind::WithData(types) }, Span::new(s, e))
            }
        }

    rule logic(i: usize) -> Token = precedence! {
        x:(@) inli(i) "#" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::CallFunction(Box::new(x), Box::new(y))) }
        --
        x:(@) inli(i) "." inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Dot(Box::new(x), Box::new(y))) }
        --
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
        x:(@) inli(i) "+" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Add(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "-" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Sub(Box::new(x), Box::new(y))) }
        start:position!() inli(i) "-" inli(i) x:@ { Token::new(Span::new(start, x.span.end), Ast::Neg(Box::new(x))) }
        --
        x:(@) inli(i) "*" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Mul(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "/" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Div(Box::new(x), Box::new(y))) }
        --
        x:@ inli(i) "^" inli(i) y:(@) { Token::new(x.span.extend(&y.span), Ast::Pow(Box::new(x), Box::new(y))) }
        --
        x:@ inli(i) "->" inli(i) y:(@) { Token::new(x.span.extend(&y.span), Ast::Implication(Box::new(x), Box::new(y)))}
        --
        start:position!() "(" inli(i) v:logic(i) inli(i) ")" end:position!() {
            Token::new(Span::new(start, end), Ast::Parenthesis(Box::new(v)))
        }
        d:block(i, <logic(i)>) { d }
        x:single(i) { x }
    }

    rule logic2(i: usize) -> Token = precedence! {
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
        x:(@) inli(i) "+" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Add(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "-" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Sub(Box::new(x), Box::new(y))) }
        start:position!() inli(i) "-" inli(i) x:@ { Token::new(Span::new(start, x.span.end), Ast::Neg(Box::new(x))) }
        --
        x:(@) inli(i) "*" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Mul(Box::new(x), Box::new(y))) }
        x:(@) inli(i) "/" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Div(Box::new(x), Box::new(y))) }
        --
        x:@ inli(i) "^" inli(i) y:(@) { Token::new(x.span.extend(&y.span), Ast::Pow(Box::new(x), Box::new(y))) }
        --
        x:@ inli(i) "->" inli(i) y:(@) { Token::new(x.span.extend(&y.span), Ast::Implication(Box::new(x), Box::new(y)))}
        --
        start:position!() "(" inli(i) v:logic(i) inli(i) ")" end:position!() {
            Token::new(Span::new(start, end), Ast::Parenthesis(Box::new(v)))
        }
        d:block(i, <logic(i)>) { d }
        x:single_without_func(i) { x }
    }

    // to avoid left recursion
    rule single_without_func(i: usize) -> Token
        = num() /
        s:position!() "val" e:position!() { Token::new(Span::new(s, e), Ast::Val) } /
        id:ident() { Token::new(id.span, Ast::Ident(id.inner())) } /
        s:position!() slice:slice() e:position!() { Token::new(Span::new(s, e), Ast::Slice(slice)) } /
        s:position!() "{" _ id:ident() _ ":" _ ty:type_definition(i) "}" e:position!() { Token::new(Span::new(s, e), Ast::Named(id, Box::new(ty))) }

    rule single(i: usize) -> Token
        = num() /
        s:position!() left:logic2(i) " " right:logic2(i) e:position!() {
            Token::new(Span::new(s, e), Ast::CallFunction(Box::new(left), Box::new(right)))
        } /
        s:position!() "val" e:position!() { Token::new(Span::new(s, e), Ast::Val) } /
        id:ident() { Token::new(id.span, Ast::Ident(id.inner())) } /
        s:position!() slice:slice() e:position!() { Token::new(Span::new(s, e), Ast::Slice(slice)) } /
        s:position!() "{" _ id:ident() _ ":" _ ty:type_definition(i) "}" e:position!() { Token::new(Span::new(s, e), Ast::Named(id, Box::new(ty))) }

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
    rule ident() -> Spanned<String>
        = s:position!() ident:$(['a'..='z'|'A'..='Z'|'_'] ['a'..='z'|'A'..='Z'|'0'..='9'|'_']*) e:position!() {
            Spanned::new(String::from(ident), Span::new(s, e))
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

pub fn parse_text(
    data: &str,
) -> Result<Vec<Spanned<TopLevelToken>>, peg::error::ParseError<peg::str::LineCol>> {
    lang::parse_lang(data)
}