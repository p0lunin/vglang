use crate::ir::types::{OneTypeKind, Type, TypeOperable};

#[derive(Debug, Clone, PartialEq)]
pub struct Unknown;

impl TypeOperable<Unknown> for OneTypeKind<Unknown> {
    fn add(self, _: Type) -> Result<Self, String> {
        Err("+ is not allowed for `Unknown` value".to_owned())
    }

    fn neg(self) -> Result<Self, String> {
        Err("- is not allowed for `Unknown` value".to_owned())
    }

    fn and(self, _: Type) -> Result<Self, String> {
        Err("& is not allowed for `Unknown` value".to_owned())
    }

    fn or(self, _: Type) -> Result<Self, String> {
        Err("| is not allowed for `Unknown` value".to_owned())
    }

    fn mul(self, _: Type) -> Result<Self, String> {
        Err("* is not allowed for `Unknown` value".to_owned())
    }

    fn sub(self, _: Type) -> Result<Self, String> {
        Err("- is not allowed for `Unknown` value".to_owned())
    }

    fn div(self, _: Type) -> Result<Self, String> {
        Err("/ is not allowed for `Unknown` value".to_owned())
    }

    fn pow(self, _: Type) -> Result<Self, String> {
        Err("^ is not allowed for `Unknown` value".to_owned())
    }
}
