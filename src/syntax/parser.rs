use crate::{
    common::{Span, Spanned},
    syntax::ast::*,
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
        = statement(i)

    rule FunctionDef(i: usize) -> Spanned<TopLevelToken>
        = start:position!() name:ident() _ gs:generics(1) _ ":" inli(i) ftype:type_definition(i) new_line() end:position!() {
            Spanned::new(TopLevelToken::FunctionDef(FunctionDef {
                name,
                generics: gs,
                type_def: Box::new(ftype)
            }), Span::new(start, end))
        }

    rule function_impl(i: usize) -> Spanned<TopLevelToken>
        = start:position!() name:ident() __ args:(ident_space())*  _ "=" _ body:statement(i) end:position!() {
            Spanned::new(TopLevelToken::FunctionImpl(FunctionImpl(name, args, FunctionBody(body))), Span::new(start, end))
        }

    rule ident_space() -> Spanned<String>
        = i:ident() __ { i }

    rule types_space(i: usize) -> Token
        = t:logic(i) _ { t }

    rule enum_decl() -> Spanned<TopLevelToken>
        = s:position!() "data" __ id:ident() gs:generics(1) _ "=" _ vs:(enum_variant(1) ** "|") e:position!() {
            Spanned::new(TopLevelToken::EnumDecl(EnumDecl {
                name: id,
                variants: vs,
                generics: gs,
            }), Span::new(s, e))
        }

    rule generics(i: usize) -> Vec<Spanned<Generic>>
        = g:generics_inner(i)? {
            match g {
                Some(generics) => generics,
                None => vec![]
            }
        }

    rule generics_inner(i: usize) -> Vec<Spanned<Generic>>
        = "<" g:spaced(<generic(i)>) ** "," ">" { g }

    rule spaced<T>(r: rule<T>) -> T
        = _ data:r() _ { data }

    rule generic(i: usize) -> Spanned<Generic>
        = s:position!() name:ident() e:position!() { Spanned::new(Generic { name }, Span::new(s, e)) }

    rule enum_variant(i: usize) -> Spanned<EnumVariant<Token>>
        = s:position!() inli(i) id:ident() _ types:(types_space((i+1)))* inli(i) e:position!() {
            Spanned::new(EnumVariant { name: id, datas: types }, Span::new(s, e))
        }

    rule statement(i: usize) -> Token
        = let_stat(i) / if_then_else(i) / case_stat(i) / logic(i)

    rule case_stat(i: usize) -> Token
        = s:position!() "case" _ cond:statement(i) _ "of" inli(i) arms:case_arms(i) e:position!() {
            Token::new(Span::new(s, e), Ast::CaseExpr(Box::new(CaseExpr { cond, arms })))
        }

    rule case_arms(i: usize) -> Vec<CaseArm>
        = arm:case_arm((i+1)) _ "?>" { vec![arm] } /
          arm:case_arm((i+1)) nli(i) other:case_arms(i) {
            let mut other = other;
            other.push(arm);
            other
          }

    rule case_arm(i: usize) -> CaseArm
        = pat:SP(<pattern()>) _ "=>" _ arm:statement(i) {
            CaseArm { pat, arm }
        }

    rule pattern() -> Pattern
        =
        "_" !ident() {
            Pattern::Otherwise
        } /
        "(" _ pat:pattern() _ ")" { pat } /
        id:ident() _ "@" _ pat:SP(<pattern()>) {
            Pattern::Bind(id, Box::new(pat))
        } /
        id:ident() !(_ ident() / ".") {
            match (id.as_bytes()[0] as char).is_lowercase() {
                true => Pattern::Ident(id.inner()),
                false => Pattern::Variant(Path::Place(id.inner()), vec![])
            }
        } /
        p:path() _ pats:(SP(<pattern()>) ** "") {
            Pattern::Variant(p, pats)
        }

    rule SP<T>(rul: rule<T>) -> Spanned<T>
        = start:position!() r:rul() end:position!() {
            Spanned::new(r, Span::new(start, end))
        }

    rule path() -> Path
        = id:ident() next:next_path()? {
            match next {
                Some(path) => Path::Path(id.inner(), Box::new(path)),
                None => Path::Place(id.inner())
            }
        }

    rule next_path() -> Path = "." next:path() { next }

    rule let_stat(i: usize) -> Token
        = s:position!() "let" __ id:ident() _ "=" _ assign:statement(i) _ "in" _ expr:statement((i+1)) e:position!() {
            Token::new(Span::new(s, e), Ast::Let {
                var: id,
                assign: Box::new(assign),
                expr: Box::new(expr),
            })
        }

    rule if_then_else(i: usize) -> Token
        = s:position!() "if" __ if_:logic(i) inli((i+1)) "then" __ then:statement((i+1)) inli((i+1)) "else" __ else_:statement((i+1)) e:position!() {
            let if_ = Box::new(if_);
            let then = Box::new(then);
            let else_ = Box::new(else_);
            Token::new(Span::new(s, e), Ast::IfThenElse { if_, then, else_ })
        }

    rule logic(i: usize) -> Token = precedence! {
        x:(@) " " y:@ { Token::new(x.span.extend(&y.span), Ast::CallFunction(Box::new(x), Box::new(y))) }
        --
        x:(@) _ "." inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Dot(Box::new(x), Box::new(y))) }
        --
        x:(@) fs:position!() _ "`" id:ident() "`" end:position!() inli(i) y:@ {
            Token::new(x.span.extend(&y.span), Ast::CallFunction(
                Box::new(Token::new(x.span.extend(&Span::new(fs, end)), Ast::CallFunction(
                    Box::new(Token::new(Span::new(fs, end), Ast::Ident(id.to_string()))),
                    Box::new(x),
                ))),
                Box::new(y),
            ))
        }
        --
        x:(@) _ "|" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Or(Box::new(x), Box::new(y))) }
        --
        x:(@) _ "&" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::And(Box::new(x), Box::new(y))) }
        --
        x:(@) _ ">" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Gr(Box::new(x), Box::new(y))) }
        x:(@) _ "<" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Le(Box::new(x), Box::new(y))) }
        x:(@) _ ">=" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::GrEq(Box::new(x), Box::new(y))) }
        x:(@) _ "<=" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::LeEq(Box::new(x), Box::new(y))) }
        x:(@) _ "==" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Eq(Box::new(x), Box::new(y))) }
        x:(@) _ "!=" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::NotEq(Box::new(x), Box::new(y))) }
        --
        x:(@) _ "+" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Add(Box::new(x), Box::new(y))) }
        x:(@) _ "-" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Sub(Box::new(x), Box::new(y))) }
        start:position!() inli(i) "-" inli(i) x:@ { Token::new(Span::new(start, x.span.end), Ast::Neg(Box::new(x))) }
        --
        x:(@) _ "*" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Mul(Box::new(x), Box::new(y))) }
        x:(@) _ "/" inli(i) y:@ { Token::new(x.span.extend(&y.span), Ast::Div(Box::new(x), Box::new(y))) }
        --
        x:@ _ "^" inli(i) y:(@) { Token::new(x.span.extend(&y.span), Ast::Pow(Box::new(x), Box::new(y))) }
        --
        x:@ _ "->" inli(i) y:(@) { Token::new(x.span.extend(&y.span), Ast::Implication(Box::new(x), Box::new(y)))}
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
        s:position!() "{" _ id:ident() _ ":" _ ty:type_definition(i) _ "}" e:position!() { Token::new(Span::new(s, e), Ast::Named(id, Box::new(ty))) }

    rule single(i: usize) -> Token
        = num() /
        s:position!() "val" e:position!() { Token::new(Span::new(s, e), Ast::Val) } /
        id:ident() { Token::new(id.span, Ast::Ident(id.inner())) } /
        s:position!() slice:slice() e:position!() { Token::new(Span::new(s, e), Ast::Slice(slice)) } /
        s:position!() "{" _ id:ident() _ ":" _ ty:type_definition(i) _ "}" e:position!() { Token::new(Span::new(s, e), Ast::Named(id, Box::new(ty))) }

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
        = s:position!() !"data" !"in" !"of" ident:$(['a'..='z'|'A'..='Z'|'_'] ['a'..='z'|'A'..='Z'|'0'..='9'|'_'|'\'']*) e:position!() {
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

pub fn parse_token(data: &str) -> Result<Token, peg::error::ParseError<peg::str::LineCol>> {
    lang::type_definition(data, 1)
}
