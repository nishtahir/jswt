use std::borrow::Borrow;

use jswt_ast::high_level::TypeAnnotation;
use jswt_ast::high_level::*;
use jswt_common::{PrimitiveType, Type};

// Substitute for the From trait
pub trait Convert<T> {
    fn convert(_: T) -> Self;
}

impl Convert<&TypeAnnotation> for Type {
    fn convert(ty: &TypeAnnotation) -> Self {
        match ty {
            TypeAnnotation::Primary(primary) => Type::convert(primary),
        }
    }
}

impl Convert<&Box<PrimaryTypeAnnotation>> for Type {
    fn convert(pta: &Box<PrimaryTypeAnnotation>) -> Self {
        match pta.borrow() {
            PrimaryTypeAnnotation::Reference(_) => Type::Object,
            PrimaryTypeAnnotation::Primitive(primitive) => Type::convert(primitive),
            PrimaryTypeAnnotation::Array(ty) => Type::Array(Box::new(Type::convert(ty))),
        }
    }
}

impl Convert<&PrimaryTypeAnnotation> for Type {
    fn convert(pta: &PrimaryTypeAnnotation) -> Self {
        match pta {
            PrimaryTypeAnnotation::Reference(_) => Type::Object,
            PrimaryTypeAnnotation::Primitive(primitive) => Type::convert(primitive),
            PrimaryTypeAnnotation::Array(ty) => Type::Array(Box::new(Type::convert(ty))),
        }
    }
}

impl Convert<&Primitive> for Type {
    fn convert(primitive: &Primitive) -> Self {
        match primitive {
            Primitive::I32 => Type::Primitive(PrimitiveType::I32),
            Primitive::U32 => Type::Primitive(PrimitiveType::U32),
            Primitive::F32 => Type::Primitive(PrimitiveType::F32),
            Primitive::Boolean => Type::Primitive(PrimitiveType::Boolean),
        }
    }
}

impl Convert<&Literal> for Type {
    fn convert(literal: &Literal) -> Self {
        match literal {
            Literal::Array(_) => todo!(),
            Literal::String(_) => Type::String,
            // TODO support unsigned numbers
            Literal::Integer(_) => Type::Primitive(PrimitiveType::I32),
            Literal::Boolean(_) => Type::Primitive(PrimitiveType::Boolean),
            Literal::Float(_) => Type::Primitive(PrimitiveType::F32),
        }
    }
}
