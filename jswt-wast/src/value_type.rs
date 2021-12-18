use jswt_common::{PrimitiveType, Type};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValueType {
    I32,
    F32,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::I32 => f.write_str("i32"),
            ValueType::F32 => f.write_str("f32"),
        }
    }
}

impl From<Type> for ValueType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Primitive(p) => match p {
                PrimitiveType::I32 => ValueType::I32,
                PrimitiveType::U32 => todo!(),
                PrimitiveType::F32 => ValueType::F32,
                PrimitiveType::Boolean => ValueType::I32,
            },
            Type::Array(_) => todo!(),
            Type::String => ValueType::I32,
            Type::Object => ValueType::I32,
            Type::Function(_, _) => todo!(),
            Type::Void => todo!(),
            Type::Unknown => todo!(),
        }
    }
}
