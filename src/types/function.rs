use crate::error::Error;
use crate::parser::{FunctionDef, FunctionImpl};
use crate::spanned::Spanned;
use crate::types::{OneTypeKind, Type, TypeOperable};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub get_value: Rc<Spanned<Type>>,
    pub return_value: Rc<Spanned<Type>>,
}

impl Function {
    pub fn is_part_of(&self, other: &Function) -> bool {
        other.get_value.is_part_of(&self.get_value)
            && self.return_value.is_part_of(&other.return_value)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.get_value.fmt(f)?;
        f.write_str(" -> ")?;
        self.return_value.fmt(f)?;
        Ok(())
    }
}

impl TypeOperable<Function> for OneTypeKind<Function> {
    fn add(self, right: Type) -> Result<Self, String> {
        Err("+ is not allowed for `Function` value".to_owned())
    }

    fn neg(self) -> Result<Self, String> {
        Err("- is not allowed for `Function` value".to_owned())
    }

    fn and(self, right: Type) -> Result<Self, String> {
        Err("& is not allowed for `Function` value".to_owned())
    }

    fn or(self, right: Type) -> Result<Self, String> {
        Err("| is not allowed for `Function` value".to_owned())
    }

    fn mul(self, right: Type) -> Result<Self, String> {
        Err("* is not allowed for `Function` value".to_owned())
    }

    fn sub(self, right: Type) -> Result<Self, String> {
        Err("- is not allowed for `Function` value".to_owned())
    }

    fn div(self, right: Type) -> Result<Self, String> {
        Err("/ is not allowed for `Function` value".to_owned())
    }

    fn pow(self, right: Type) -> Result<Self, String> {
        Err("^ is not allowed for `Function` value".to_owned())
    }
}
