use crate::ir::types::{TypeOperable, OneTypeKind, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeType;

impl TypeOperable<TypeType> for OneTypeKind<TypeType> {
    fn add(self, _: Type) -> Result<Self, String> {
        Err("+ is not allowed for `Type` value".to_owned())
    }

    fn neg(self) -> Result<Self, String> {
        Err("- is not allowed for `Type` value".to_owned())
    }

    fn and(self, _: Type) -> Result<Self, String> {
        Err("& is not allowed for `Type` value".to_owned())
    }

    fn or(self, _: Type) -> Result<Self, String> {
        Err("| is not allowed for `Type` value".to_owned())
    }

    fn mul(self, _: Type) -> Result<Self, String> {
        Err("* is not allowed for `Type` value".to_owned())
    }

    fn sub(self, _: Type) -> Result<Self, String> {
        Err("- is not allowed for `Type` value".to_owned())
    }

    fn div(self, _: Type) -> Result<Self, String> {
        Err("/ is not allowed for `Type` value".to_owned())
    }

    fn pow(self, _: Type) -> Result<Self, String> {
        Err("^ is not allowed for `Type` value".to_owned())
    }
}