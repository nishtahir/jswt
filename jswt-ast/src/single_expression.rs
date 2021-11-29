pub use jswt_common::{Span, Spannable};

pub use crate::ident::Ident;
use crate::Literal;

#[derive(Debug, PartialEq)]
pub enum SingleExpression {
    Arguments(ArgumentsExpression),
    Bitwise(BinaryExpression),
    Multiplicative(BinaryExpression),
    Additive(BinaryExpression),
    Equality(BinaryExpression),
    Relational(BinaryExpression),
    Identifier(IdentifierExpression),
    Literal(Literal),
}

impl From<ArgumentsExpression> for SingleExpression {
    fn from(v: ArgumentsExpression) -> Self {
        Self::Arguments(v)
    }
}

impl From<IdentifierExpression> for SingleExpression {
    fn from(v: IdentifierExpression) -> Self {
        Self::Identifier(v)
    }
}

impl From<Literal> for SingleExpression {
    fn from(v: Literal) -> Self {
        Self::Literal(v)
    }
}

impl From<BinaryExpression> for SingleExpression {
    fn from(v: BinaryExpression) -> Self {
        Self::Additive(v)
    }
}

impl Spannable for SingleExpression {
    fn span(&self) -> Span {
        match self {
            SingleExpression::Multiplicative(exp) => exp.span(),
            SingleExpression::Additive(exp) => exp.span(),
            SingleExpression::Literal(exp) => exp.span(),
            SingleExpression::Identifier(exp) => exp.span(),
            SingleExpression::Arguments(exp) => exp.span(),
            SingleExpression::Equality(exp) => exp.span(),
            SingleExpression::Bitwise(exp) => exp.span(),
            SingleExpression::Relational(exp) => exp.span(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ArgumentsExpression {
    pub span: Span,
    pub ident: Box<SingleExpression>,
    pub arguments: ArgumentsList,
}

impl Spannable for ArgumentsExpression {
    fn span(&self) -> Span {
        self.span.to_owned()
    }
}

#[derive(Debug, PartialEq)]
pub struct ArgumentsList {
    pub span: Span,
    pub arguments: Vec<SingleExpression>,
}

impl Spannable for ArgumentsList {
    fn span(&self) -> Span {
        self.span.to_owned()
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpression {
    pub span: Span,
    op: UnaryOperator,
    expr: Box<SingleExpression>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpression {
    pub span: Span,
    pub left: Box<SingleExpression>,
    pub op: BinaryOperator,
    pub right: Box<SingleExpression>,
}

impl Spannable for BinaryExpression {
    fn span(&self) -> Span {
        self.span.to_owned()
    }
}

#[derive(Debug, PartialEq)]
pub struct IdentifierExpression {
    pub ident: Ident,
}

impl Spannable for IdentifierExpression {
    fn span(&self) -> Span {
        self.ident.span.to_owned()
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Plus(Span),
    Minus(Span),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Plus(Span),
    Minus(Span),
    Mult(Span),
    Div(Span),
    Equal(Span),
    NotEqual(Span),
    Greater(Span),
    GreaterEqual(Span),
    Less(Span),
    LessEqual(Span),
    And(Span),
    Or(Span),
    Assign(Span),
}
