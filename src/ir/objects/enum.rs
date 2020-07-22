use crate::ir::types::Type;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct DataType {
    pub name: String,
    pub generics: Vec<Rc<Type>>,
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.generics.as_slice() {
            [] => f.write_str(self.name.as_str()),
            xs => {
                f.write_str("(")?;
                f.write_str(self.name.as_str())?;
                f.write_str(" ")?;
                xs.iter().for_each(|x| {
                    write!(f, " {}", x);
                });
                f.write_str(")")?;
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct DataDef {
    pub ty: Rc<DataType>,
    pub variants: Vec<Rc<DataVariant>>,
}

impl Display for DataDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        Display::fmt(&self.ty, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataVariant {
    pub dty: Rc<DataType>,
    pub name: String,
    pub data: EnumVariantData,
}

#[derive(Debug, PartialEq, Clone)]
pub enum EnumVariantData {
    Unit,
    WithData(Vec<Rc<Type>>),
}

impl EnumVariantData {
    pub fn get_data(&self) -> impl Iterator<Item = &Rc<Type>> {
        match self {
            EnumVariantData::Unit => [].iter(),
            EnumVariantData::WithData(d) => d.iter(),
        }
    }
}

impl Display for DataVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}.{}", self.dty, self.name)
    }
}
