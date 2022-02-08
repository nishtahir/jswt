use std::borrow::Cow;

use jswt_derive::FromEnumVariant;
use jswt_types::{FunctionType, ObjectType, Type};

#[derive(Debug, PartialEq, FromEnumVariant)]
pub enum Symbol {
    Type(TypeBinding),
    Function(FunctionBinding),
    Class(ClassBinding),
}

impl Symbol {
    /// Returns `true` if the symbol is [`Function`].
    ///
    /// [`Function`]: Symbol::Function
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(..))
    }

    /// Returns `true` if the symbol is [`Class`].
    ///
    /// [`Class`]: Symbol::Class
    pub fn is_class(&self) -> bool {
        matches!(self, Self::Class(..))
    }

    pub fn as_class(&self) -> Option<&ClassBinding> {
        if let Self::Class(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_type(&self) -> Type {
        match self {
            Symbol::Type(binding) => binding.ty.clone(),
            Symbol::Function(binding) => binding.as_type(),
            Symbol::Class(binding) => binding.as_type(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TypeBinding {
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct FunctionBinding {
    pub params: Vec<Type>,
    pub returns: Type,
}

impl FunctionBinding {
    fn as_type(&self) -> Type {
        Type::Function(FunctionType {
            params: self.params.clone(),
            returns: Box::new(self.returns.clone()),
        })
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct ClassBinding {
    pub name: Cow<'static, str>,
    pub fields: Vec<Field>,
}

impl ClassBinding {
    /// Total size of fields on the class
    pub fn size(&self) -> usize {
        self.fields.iter().fold(0, |acc, f| acc + f.size)
    }

    pub fn as_type(&self) -> Type {
        Type::Object(ObjectType::Reference(self.name.clone()))
    }

    pub fn field(&self, name: &str) -> Option<&Field> {
        for field in &self.fields {
            if field.name == name {
                return Some(field);
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
}
