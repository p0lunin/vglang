use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    And,
    Or,
    Gr,
    Eq,
    NotEq,
    GrOrEq,
    Le,
    LeOrEq,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => f.write_str("+"),
            BinOp::Gr => f.write_str(">"),
            BinOp::Le => f.write_str("<"),
            _ => unimplemented!(),
        }
    }
}
