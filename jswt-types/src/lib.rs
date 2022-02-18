use std::{borrow::Cow, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Class(Cow<'static, str>),
    Array(Box<Type>),
    Function(FunctionType),
    Void,
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub returns: Box<Type>,
}

impl Type {
    pub fn is_same_as(&self, other: &Type) -> bool {
        self == other
    }

    pub fn is_assignable_to(&self, other: &Type) -> bool {
        self == other
    }

    pub fn is_comparable_to(&self, other: &Type) -> bool {
        self == other
    }

    pub fn coerce(self, other: &Type) -> Type {
        if self.is_unknown() {
            return other.clone();
        }
        return self;
    }

    /// Returns `true` if the type is [`Function`].
    ///
    /// [`Function`]: Type::Function
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(..))
    }

    /// Returns `true` if the type is [`Unknown`].
    ///
    /// [`Unknown`]: Type::Unknown
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}
