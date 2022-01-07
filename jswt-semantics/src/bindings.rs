use jswt_derive::FromEnumVariant;
use std::{borrow::Cow, collections::BTreeMap};

#[derive(Debug, FromEnumVariant)]
pub enum Binding {
    Class(ClassBinding),
}

#[derive(Debug)]

pub struct ClassBinding {
    
}

// A binding represents an arbitrary type definition
// these are expected to lib between semantic phases
#[derive(Debug, Default)]
pub struct BindingsTable {
    // Binding of an identifier to type
    table: BTreeMap<Cow<'static, str>, Binding>,
}

impl BindingsTable {
    pub fn lookup(&self, key: &str) -> Option<&Binding> {
        self.table.get(key.into())
    }

    pub fn define<T: Into<Cow<'static, str>>, U: Into<Binding>>(&mut self, key: T, ty: U) {
        self.table.insert(key.into(), ty.into());
    }
}
