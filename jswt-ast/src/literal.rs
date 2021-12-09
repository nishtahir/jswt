pub use jswt_common::{Span, Spannable};
use jswt_derive::FromEnumVariant;

use crate::SingleExpression;

#[derive(Debug, PartialEq, FromEnumVariant)]
pub enum Literal {
    Array(ArrayLiteral),
    String(StringLiteral),
    Number(NumberLiteral),
    Boolean(BooleanLiteral),
}

impl Spannable for Literal {
    fn span(&self) -> Span {
        match self {
            Literal::String(s) => s.span.to_owned(),
            Literal::Number(n) => n.span.to_owned(),
            Literal::Boolean(b) => b.span.to_owned(),
            Literal::Array(a) => a.span.to_owned(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BooleanLiteral {
    pub span: Span,
    pub value: bool,
}

#[derive(Debug, PartialEq)]

pub struct NumberLiteral {
    pub span: Span,
    pub value: i32,
}

#[derive(Debug, PartialEq)]
pub struct StringLiteral {
    pub span: Span,
    pub value: &'static str,
}

#[derive(Debug, PartialEq)]
pub struct ArrayLiteral {
    pub span: Span,
    pub elements: Vec<SingleExpression>,
}
