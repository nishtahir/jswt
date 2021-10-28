use super::{literal::LiteralExpr, node::Node, operator::Operator};

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Unary(UnaryExpr<'a>),
    Binary(BinaryExpr<'a>),
    Literal(LiteralExpr<'a>),
}

impl<'a> Expr<'a> {
    pub fn string(arg: Node<'a>) -> Self {
        let expr = LiteralExpr::string(arg);
        Expr::Literal(expr)
    }

    pub fn number(arg: Node<'a>) -> Self {
        let expr = LiteralExpr::number(arg);
        Expr::Literal(expr)
    }

    pub fn boolean(arg: Node<'a>) -> Self {
        let expr = LiteralExpr::boolean(arg);
        Expr::Literal(expr)
    }

    pub fn unary(op: Operator, right: Expr<'a>) -> Self {
        Expr::Unary(UnaryExpr {
            op,
            right: Box::new(right),
        })
    }

    pub fn binary(left: Expr<'a>, op: Operator, right: Expr<'a>) -> Self {
        Expr::Binary(BinaryExpr {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr<'a> {
    op: Operator,
    right: Box<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr<'a> {
    left: Box<Expr<'a>>,
    op: Operator,
    right: Box<Expr<'a>>,
}

impl<'a> BinaryExpr<'a> {
    pub fn new(left: Box<Expr<'a>>, op: Operator, right: Box<Expr<'a>>) -> Self {
        Self { left, op, right }
    }
}
