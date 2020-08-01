use crate::ir::types::Type;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub get_value: Rc<Type>,
    pub return_value: Rc<Type>,
}

impl Function {
    pub fn is_part_of(&self, other: &Function) -> bool {
        other.get_value.is_part_of(self.get_value.deref())
            && self.return_value.is_part_of(&other.return_value.deref())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "({} -> {})", self.get_value, self.return_value)
    }
}
/*
impl TypeOperable for Function {
    fn add(self, _: Type) -> Result<Self, String> {
        Err("+ is not allowed for `Function` value".to_owned())
    }

    fn neg(self) -> Result<Self, String> {
        Err("- is not allowed for `Function` value".to_owned())
    }

    fn and(self, _: Type) -> Result<Self, String> {
        Err("& is not allowed for `Function` value".to_owned())
    }

    fn or(self, _: Type) -> Result<Self, String> {
        Err("| is not allowed for `Function` value".to_owned())
    }

    fn mul(self, _: Type) -> Result<Self, String> {
        Err("* is not allowed for `Function` value".to_owned())
    }

    fn sub(self, _: Type) -> Result<Self, String> {
        Err("- is not allowed for `Function` value".to_owned())
    }

    fn div(self, _: Type) -> Result<Self, String> {
        Err("/ is not allowed for `Function` value".to_owned())
    }

    fn pow(self, _: Type) -> Result<Self, String> {
        Err("^ is not allowed for `Function` value".to_owned())
    }
}
*/
