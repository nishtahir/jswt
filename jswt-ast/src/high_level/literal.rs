use jswt_common::Type;
pub use jswt_common::{Span, Spannable};
use jswt_derive::{FromEnumVariant, Spannable, Typeable};

use crate::high_level::SingleExpression;

#[derive(Debug, PartialEq, FromEnumVariant, Spannable, Typeable)]
pub enum Literal {
    Array(ArrayLiteral),
    String(StringLiteral),
    Integer(IntegerLiteral),
    Float(FloatingPointLiteral),
    Boolean(BooleanLiteral),
}

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct BooleanLiteral {
    pub ty: Type,
    pub span: Span,
    pub value: bool,
}

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct IntegerLiteral {
    pub ty: Type,
    pub span: Span,
    pub value: i32,
}

#[derive(Debug, PartialEq, Spannable, Typeable)]

pub struct FloatingPointLiteral {
    pub ty: Type,
    pub span: Span,
    pub value: f32,
}

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct StringLiteral {
    pub ty: Type,
    pub span: Span,
    pub value: &'static str,
}

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct ArrayLiteral {
    pub ty: Type,
    pub span: Span,
    pub elements: Vec<SingleExpression>,
}
