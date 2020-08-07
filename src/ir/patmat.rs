use crate::common::Spanned;
use crate::syntax;
use crate::syntax::ast::Path;

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Otherwise,
    Ident(String),
    Variant(Path, Vec<Spanned<Pattern>>),
    Bind(String, Box<Spanned<Pattern>>),
}

impl Pattern {
    pub fn parse(ast: Spanned<syntax::ast::Pattern>) -> Spanned<Pattern> {
        let Spanned { val: ast, span } = ast;
        Spanned::new(
            match ast {
                syntax::ast::Pattern::Otherwise => Pattern::Otherwise,
                syntax::ast::Pattern::Bind(s, p) => {
                    Pattern::Bind(s.inner(), Box::new(Pattern::parse(*p)))
                }
                syntax::ast::Pattern::Variant(path, patterns) => {
                    Pattern::Variant(path, patterns.into_iter().map(Pattern::parse).collect())
                }
                syntax::ast::Pattern::Ident(i) => Pattern::Ident(i),
            },
            span,
        )
    }
}

pub fn check_exhaustive(patterns: &[Pattern]) -> bool {
    true
}
