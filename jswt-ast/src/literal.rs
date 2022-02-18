use jswt_common::Span;
use jswt_derive::{FromEnumVariant, Spannable};
use jswt_types::Type;

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
            Literal::Array(_) => Type::Array(Box::new(Type::Class("any".into()))),
            Literal::String(_) => Type::Class("string".into()),
            Literal::Integer(_) => Type::Class("i32".into()),
            Literal::Float(_) => Type::Class("f32".into()),
            Literal::Boolean(_) => Type::Class("boolean".into()),
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
