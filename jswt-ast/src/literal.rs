pub use jswt_common::{Span, Spannable};

#[derive(Debug, PartialEq)]
pub enum Literal {
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
        }
    }
}

impl From<StringLiteral> for Literal {
    fn from(v: StringLiteral) -> Self {
        Self::String(v)
    }
}

impl From<NumberLiteral> for Literal {
    fn from(v: NumberLiteral) -> Self {
        Self::Number(v)
    }
}

impl From<BooleanLiteral> for Literal {
    fn from(v: BooleanLiteral) -> Self {
        Self::Boolean(v)
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
