use std::fmt::Display;

use jswt_derive::FromEnumVariant;

#[derive(Debug, Clone, PartialEq, FromEnumVariant)]
pub enum Type {
    Primitive(PrimitiveType),
    Array(Box<Type>),
    String,
    Object,
    Function,
    Void,
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self {
            Type::Primitive(p) => p.to_string(),
            Type::Array(_) => todo!(),
            Type::String => "string".to_owned(),
            Type::Object => "object".to_owned(),
            Type::Function => "function".to_owned(),
            Type::Void => "void".to_owned(),
            Type::Unknown => "?".to_owned(),
        };
        f.write_str(&value)
    }
}

impl Type {
    /// Returns `true` if the type is [`Function`].
    ///
    /// [`Function`]: Type::Function
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function)
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::Primitive(PrimitiveType::I32) | Self::Primitive(PrimitiveType::U32)
        )
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::Boolean))
    }

    pub fn boolean() -> Self {
        Self::Primitive(PrimitiveType::Boolean)
    }

    pub fn i32() -> Self {
        Self::Primitive(PrimitiveType::I32)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrimitiveType {
    I32,
    U32,
    F32,
    Boolean,
    Unit,
    Void,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::I32 => f.write_str("i32"),
            PrimitiveType::U32 => f.write_str("u32"),
            PrimitiveType::F32 => f.write_str("f32"),
            PrimitiveType::Boolean => f.write_str("boolean"),
            PrimitiveType::Unit => f.write_str("unit"),
            PrimitiveType::Void => f.write_str("void"),
        }
    }
}
