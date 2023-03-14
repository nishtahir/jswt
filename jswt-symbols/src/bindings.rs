use std::{borrow::Cow, collections::BTreeMap};

use jswt_common::Type;

use crate::FunctionSignature;

// Class containing all bindings that
// we've discovered in the global scope
// We're keeping these definitions out of the symbol table
// to make it easier to track all the class instances we've discovered
#[derive(Default, Debug)]
pub struct BindingsTable {
    bindings: BTreeMap<Cow<'static, str>, ClassBinding>,
}

impl BindingsTable {
    pub fn insert(&mut self, name: Cow<'static, str>, binding: ClassBinding) {
        self.bindings.insert(name, binding);
    }

    pub fn lookup(&self, name: &str) -> Option<&ClassBinding> {
        self.bindings.get(name.into())
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct ClassBinding {
    pub name: Cow<'static, str>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

impl ClassBinding {
    /// Total size of fields on the class
    pub fn size(&self) -> usize {
        self.fields.iter().fold(0, |acc, f| acc + f.size)
    }

    pub fn field(&self, name: &str) -> Option<&Field> {
        for field in &self.fields {
            if field.name == name {
                return Some(field);
            }
        }
        None
    }

    pub fn method(&self, name: &str) -> Option<&Method> {
        for method in &self.methods {
            if method.name == name {
                return Some(method);
            }
        }
        None
    }
}

#[derive(Debug, PartialEq)]
pub struct Field {
    pub name: Cow<'static, str>,
    pub index: usize,
    pub size: usize,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct Method {
    pub name: Cow<'static, str>,
    pub signature: FunctionSignature,
}
