pub use jswt_common::{Span, Spannable};
use jswt_derive::Spannable;

pub use crate::ident::Ident;
use crate::Literal;

#[derive(Debug, PartialEq)]
pub enum SingleExpression {
    Unary(UnaryExpression),
    Assignment(BinaryExpression),
    MemberIndex(MemberIndexExpression),
    Arguments(ArgumentsExpression),
    Multiplicative(BinaryExpression),
    Bitwise(BinaryExpression),
    Additive(BinaryExpression),
    Equality(BinaryExpression),
    Relational(BinaryExpression),
    Identifier(IdentifierExpression),
    Literal(Literal),
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
            SingleExpression::Assignment(exp) => exp.span(),
            SingleExpression::Unary(exp) => exp.span(),
            SingleExpression::MemberIndex(exp) => exp.span(),
        }
    }
}

#[derive(Debug, PartialEq, Spannable)]
pub struct ArgumentsExpression {
    pub span: Span,
    pub ident: Box<SingleExpression>,
    pub arguments: ArgumentsList,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct ArgumentsList {
    pub span: Span,
    pub arguments: Vec<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct MemberIndexExpression {
    pub span: Span,
    pub target: Box<SingleExpression>,
    pub index: Box<SingleExpression>,
}


#[derive(Debug, PartialEq, Spannable)]
pub struct UnaryExpression {
    pub span: Span,
    pub op: UnaryOperator,
    pub expr: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct BinaryExpression {
    pub span: Span,
    pub left: Box<SingleExpression>,
    pub op: BinaryOperator,
    pub right: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct IdentifierExpression {
    pub span: Span,
    pub ident: Ident,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Plus(Span),
    Minus(Span),
    Not(Span),
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
