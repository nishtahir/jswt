use jswt_common::Span;
use jswt_derive::Spannable;

pub use crate::ident::Ident;
use crate::Literal;

#[derive(Debug, PartialEq, Spannable)]
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
