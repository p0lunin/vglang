use crate::ir::types::Type;

pub trait TypeOperable: Sized {
    fn add(&self, right: &Type) -> Result<Self, String>;
    fn neg(&self) -> Result<Self, String>;
    fn and(&self, right: &Type) -> Result<Self, String>;
    fn or(&self, right: &Type) -> Result<Self, String>;
    fn mul(&self, right: &Type) -> Result<Self, String>;
    fn sub(&self, right: &Type) -> Result<Self, String>;
    fn div(&self, right: &Type) -> Result<Self, String>;
    fn pow(&self, right: &Type) -> Result<Self, String>;
}
