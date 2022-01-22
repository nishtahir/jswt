use crate::{ident::Identifier, Literal};

use jswt_common::Span;
use jswt_derive::Spannable;

#[derive(Debug, PartialEq, Spannable, Clone)]
pub enum SingleExpression {
    Unary(UnaryExpression),
    Assignment(BinaryExpression),
    MemberIndex(MemberIndexExpression),
    New(NewExpression),
    Arguments(ArgumentsExpression),
    Multiplicative(BinaryExpression),
    Bitwise(BinaryExpression),
    Additive(BinaryExpression),
    Equality(BinaryExpression),
    Relational(BinaryExpression),
    Identifier(IdentifierExpression),
    MemberDot(MemberDotExpression),
    This(ThisExpression),
    Literal(Literal),
}

impl SingleExpression {
    pub fn as_arguments(&self) -> Option<&ArgumentsExpression> {
        if let Self::Arguments(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_identifier_mut(&mut self) -> Option<&mut IdentifierExpression> {
        if let Self::Identifier(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct NewExpression {
    pub span: Span,
    pub expression: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct MemberDotExpression {
    pub span: Span,
    pub target: Box<SingleExpression>,
    pub expression: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ArgumentsExpression {
    pub span: Span,
    pub ident: Box<SingleExpression>,
    pub arguments: ArgumentsList,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ArgumentsList {
    pub span: Span,
    pub arguments: Vec<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ThisExpression {
    pub span: Span,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct MemberIndexExpression {
    pub span: Span,
    pub target: Box<SingleExpression>,
    pub index: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct UnaryExpression {
    pub span: Span,
    pub op: UnaryOperator,
    pub expr: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct BinaryExpression {
    pub span: Span,
    pub left: Box<SingleExpression>,
    pub op: BinaryOperator,
    pub right: Box<SingleExpression>,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct IdentifierExpression {
    pub span: Span,
    pub ident: Identifier,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Plus(Span),
    Minus(Span),
    Not(Span),
    PostIncrement(Span),
    PostDecrement(Span),
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
