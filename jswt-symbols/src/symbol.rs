use std::borrow::Cow;

use jswt_derive::FromEnumVariant;
use jswt_types::Type;

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
}

#[derive(Debug, PartialEq)]
pub struct Field {
    pub name: Cow<'static, str>,
    pub index: usize,
    pub size: usize,
}
