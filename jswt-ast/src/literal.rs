pub use jswt_common::{Span, Spannable};
use jswt_derive::{FromEnumVariant, Spannable};

use crate::SingleExpression;

#[derive(Debug, PartialEq, FromEnumVariant, Spannable)]
pub enum Literal {
    Array(ArrayLiteral),
    String(StringLiteral),
    Integer(IntegerLiteral),
    Float(FloatingPointLiteral),
    Boolean(BooleanLiteral),
}

#[derive(Debug, PartialEq, Spannable)]
pub struct BooleanLiteral {
    pub span: Span,
    pub value: bool,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct IntegerLiteral {
    pub span: Span,
    pub value: i32,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct FloatingPointLiteral {
    pub span: Span,
    pub value: f32,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct StringLiteral {
    pub span: Span,
    pub value: &'static str,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct ArrayLiteral {
    pub span: Span,
    pub elements: Vec<SingleExpression>,
}
