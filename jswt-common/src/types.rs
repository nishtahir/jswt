use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Array(Box<Type>),
    String,
    Object,
    Function(Vec<Type>, Box<Type>),
    Void,
    Unknown,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrimitiveType {
    I32,
    U32,
    F32,
    Boolean,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self {
            Type::Primitive(p) => p.to_string(),
            Type::Array(t) => format!("array[{}]", t.to_string()),
            Type::String => "string".to_owned(),
            Type::Object => "object".to_owned(),
            Type::Function(_, _) => "function".to_owned(),
            Type::Void => "void".to_owned(),
            Type::Unknown => "?".to_owned(),
        };
        f.write_str(&value)
    }
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

pub trait Typeable {
    fn defined_type(&self) -> Type;
}

impl Type {
    pub fn array(ty: Type) -> Self {
        Self::Array(Box::new(ty))
    }

    pub fn boolean() -> Self {
        Self::Primitive(PrimitiveType::Boolean)
    }

    pub fn i32() -> Self {
        Self::Primitive(PrimitiveType::I32)
    }

    pub fn f32() -> Self {
        Self::Primitive(PrimitiveType::F32)
    }

    pub fn string() -> Self {
        Self::String
    }

    pub fn unknown() -> Self {
        Self::Unknown
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::Boolean))
    }

    pub fn is_signed_number(&self) -> bool {
        matches!(
            self,
            Self::Primitive(PrimitiveType::I32) | Self::Primitive(PrimitiveType::F32)
        )
    }

    pub fn is_real_number(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::I32))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(_, _))
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}
