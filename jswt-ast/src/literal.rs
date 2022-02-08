use jswt_common::Span;
use jswt_derive::{FromEnumVariant, Spannable};
use jswt_types::{ObjectType, PrimitiveType, Type};

use crate::SingleExpression;

#[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
pub enum Literal {
    Array(ArrayLiteral),
    String(StringLiteral),
    Integer(IntegerLiteral),
    Float(FloatingPointLiteral),
    Boolean(BooleanLiteral),
}

impl Literal {
    pub fn as_type(&self) -> Type {
        match self {
            Literal::Array(_) => todo!(),
            Literal::String(_) => Type::Object(ObjectType::String),
            Literal::Integer(_) => Type::Primitive(PrimitiveType::I32),
            Literal::Float(_) => Type::Primitive(PrimitiveType::F32),
            Literal::Boolean(_) => Type::Primitive(PrimitiveType::Boolean),
        }
    }
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct BooleanLiteral {
    pub span: Span,
    pub value: bool,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct IntegerLiteral {
    pub span: Span,
    pub value: i32,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct FloatingPointLiteral {
    pub span: Span,
    pub value: f32,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct StringLiteral {
    pub span: Span,
    pub value: &'static str,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ArrayLiteral {
    pub span: Span,
    pub elements: Vec<SingleExpression>,
}
