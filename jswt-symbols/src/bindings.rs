use crate::Parameter;
use jswt_common::Type;
use std::{borrow::Cow, collections::BTreeMap};

// Class containing all bindings that
// we've discovered in the global scope
// We're keeping these definitions out of the symbol table
// to make it easier to track all the class instances we've discovered
#[derive(Default, Debug)]
pub struct SemanticBindingsTable {
    bindings: BTreeMap<Cow<'static, str>, ClassBinding>,
}

impl SemanticBindingsTable {
    pub fn insert(&mut self, name: Cow<'static, str>, binding: ClassBinding) {
        self.bindings.insert(name, binding);
    }

    pub fn lookup(&self, name: &str) -> Option<&ClassBinding> {
        self.bindings.get(name.into())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ClassBinding {
    pub name: Cow<'static, str>,
    pub constructors: Vec<Constructor>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

impl ClassBinding {
    /// Total size of fields on the class
    pub fn size(&self) -> usize {
        self.fields.iter().fold(0, |acc, f| acc + f.size)
    }

    /// Find a field with the given name on the class binding
    pub fn field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.name == name)
    }

    /// Find a method with the given name on the class binding
    pub fn method(&self, name: &str) -> Option<&Method> {
        self.methods.iter().find(|m| m.name == name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
    pub name: Cow<'static, str>,
    pub index: usize,
    pub size: usize,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {
    pub name: Cow<'static, str>,
    pub params: Vec<Parameter>,
    pub ret: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Constructor {
    pub params: Vec<Parameter>,
}
