pub mod base_types;
mod r#type;
mod type_operable;
mod concrete;

pub use self::{
    r#type::{Generic, Type},
    type_operable::*,
    concrete::Concrete,
};
