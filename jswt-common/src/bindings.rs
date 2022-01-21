use std::{borrow::Cow, collections::BTreeMap};

#[derive(Debug)]
pub enum Binding {
    Class(ClassBinding),
}

/// TODO generate enum variants for this
impl From<ClassBinding> for Binding {
    fn from(v: ClassBinding) -> Self {
        Self::Class(v)
    }
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

impl ClassBinding {
    /// Total size of fields on the class
    pub fn size(&self) -> usize {
        self.fields.iter().fold(0, |acc, f| acc + f.size)
    }
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
        self.table.get(key)
    }

    pub fn define<T: Into<Cow<'static, str>>, U: Into<Binding>>(&mut self, key: T, binding: U) {
        self.table.insert(key.into(), binding.into());
    }
}
