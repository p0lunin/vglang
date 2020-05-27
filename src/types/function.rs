use crate::error::Error;
use crate::types::{OneTypeKind, Type, TypeOperable};
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub get_value: Rc<RefCell<Type>>,
    pub return_value: Rc<RefCell<Type>>,
}

impl Function {
    pub fn is_part_of(&self, other: &Function) -> bool {
        other
            .get_value
            .borrow()
            .is_part_of(self.get_value.borrow().deref())
            && self
            .return_value
            .borrow()
            .is_part_of(&other.return_value.borrow().deref())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.get_value.borrow().fmt(f)?;
        f.write_str(" -> ")?;
        self.return_value.borrow().fmt(f)?;
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
