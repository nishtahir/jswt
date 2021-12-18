use jswt_common::{Span, Type};
use jswt_derive::{Spannable, Typeable};

pub use crate::common::Ident;
use crate::high_level::Literal;

#[derive(Debug, PartialEq, Spannable, Typeable)]
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

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct ArgumentsExpression {
    pub ty: Type,
    pub span: Span,
    pub ident: Box<SingleExpression>,
    pub arguments: ArgumentsList,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct ArgumentsList {
    pub span: Span,
    pub arguments: Vec<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct MemberIndexExpression {
    pub ty: Type,
    pub span: Span,
    pub target: Box<SingleExpression>,
    pub index: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct UnaryExpression {
    pub ty: Type,
    pub span: Span,
    pub op: UnaryOperator,
    pub expr: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct BinaryExpression {
    pub ty: Type,
    pub span: Span,
    pub left: Box<SingleExpression>,
    pub op: BinaryOperator,
    pub right: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Typeable)]
pub struct IdentifierExpression {
    pub span: Span,
    pub ident: Ident,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Plus(Span),
    Minus(Span),
    Not(Span),
}

#[derive(Debug, PartialEq, Clone)]
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
