use crate::token::Token;

pub trait Evaluate {
    fn evaluate(&self);
}

#[derive(Debug, PartialEq)]

pub enum Expr<'a> {
    Unary(UnaryExpr<'a>),
    Binary(BinaryExpr<'a>),
    Literal(LiteralExpr<'a>),
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr<'a> {
    op: &'a Token<'a>,
    arg: &'a Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr<'a> {
    left: &'a Expr<'a>,
    op: &'a Token<'a>,
    right: &'a Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct LiteralExpr<'a> {
    arg: &'a Token<'a>,
}

impl<'a> LiteralExpr<'a> {
    pub fn new(arg: &'a Token) -> Self {
        Self { arg }
    }
}

impl<'a> From<LiteralExpr<'a>> for Expr<'a> {
    fn from(expr: LiteralExpr<'a>) -> Self {
        Expr::Literal(expr)
    }
}
