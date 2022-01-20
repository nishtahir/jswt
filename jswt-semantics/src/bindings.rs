use jswt_derive::FromEnumVariant;
use std::{borrow::Cow, collections::BTreeMap};

#[derive(Debug, FromEnumVariant)]
pub enum Binding {
    Class(ClassBinding),
}

impl Binding {
    pub fn as_class(&self) -> Option<&ClassBinding> {
        if let Self::Class(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Default)]
pub struct ClassBinding {
    pub name: Cow<'static, str>,
    pub fields: Vec<Field>,
}

#[derive(Debug, PartialEq)]
pub struct Field {
    pub name: Cow<'static, str>,
    pub index: usize,
    pub size: usize,
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

    pub fn define<T: Into<Cow<'static, str>>, U: Into<Binding>>(&mut self, key: T, binding: U) {
        self.table.insert(key.into(), binding.into());
    }
}
