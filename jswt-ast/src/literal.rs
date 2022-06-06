use jswt_common::{Span, Type};
use jswt_derive::{FromEnumVariant, Spannable, Typeable};

use crate::SingleExpression;

#[derive(Debug, PartialEq, FromEnumVariant, Spannable, Typeable, Clone)]
pub enum Literal {
    Array(ArrayLiteral),
    String(StringLiteral),
    Integer(IntegerLiteral),
    Float(FloatingPointLiteral),
    Boolean(BooleanLiteral),
}

#[derive(Debug, PartialEq, Spannable, Typeable, Clone)]
pub struct BooleanLiteral {
    pub span: Span,
    pub value: bool,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Spannable, Typeable, Clone)]
pub struct IntegerLiteral {
    pub span: Span,
    pub value: i32,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Spannable, Typeable, Clone)]
pub struct FloatingPointLiteral {
    pub span: Span,
    pub value: f32,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Spannable, Typeable, Clone)]
pub struct StringLiteral {
    pub span: Span,
    pub value: &'static str,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Spannable, Typeable, Clone)]
pub struct ArrayLiteral {
    pub span: Span,
    pub elements: Vec<SingleExpression>,
    pub ty: Type,
}
