use std::{borrow::Cow, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Object(ObjectType),
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
