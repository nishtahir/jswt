use std::{borrow::Cow, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Object(ObjectType),
    Function(FunctionType),
    Void,
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectType {
    Array(Box<Type>),
    String,
    Reference(Cow<'static, str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    I32,
    U32,
    F32,
    Boolean,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub returns: Box<Type>,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::I32 => f.write_str("i32"),
            PrimitiveType::U32 => f.write_str("u32"),
            PrimitiveType::F32 => f.write_str("f32"),
            PrimitiveType::Boolean => f.write_str("boolean"),
        }
    }
}

impl Type {
    fn is_same_as(&self, other: &Type) -> bool {
        self == other
    }

    fn is_assignable_to(&self, other: &Type) -> bool {
        self == other
    }

    /// Returns `true` if the type is [`Function`].
    ///
    /// [`Function`]: Type::Function
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(..))
    }
}
