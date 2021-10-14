use crate::node::Node;

pub trait Evaluate {
    fn evaluate(&self);
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Unary(UnaryExpr<'a>),
    Binary(BinaryExpr<'a>),
    Literal(LiteralExpr<'a>),
}

impl<'a> Expr<'a> {
    pub fn literal(arg: Node<'a>) -> Self {
        Expr::Literal(LiteralExpr { arg })
    }

    pub fn unary(op: Node<'a>, right: Expr<'a>) -> Self {
        Expr::Unary(UnaryExpr {
            op,
            right: Box::new(right),
        })
    }

    pub fn binary(left: Expr<'a>, op: Node<'a>, right: Expr<'a>) -> Self {
        Expr::Binary(BinaryExpr {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr<'a> {
    op: Node<'a>,
    right: Box<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr<'a> {
    left: Box<Expr<'a>>,
    op: Node<'a>,
    right: Box<Expr<'a>>,
}

impl<'a> BinaryExpr<'a> {
    pub fn new(left: Box<Expr<'a>>, op: Node<'a>, right: Box<Expr<'a>>) -> Self {
        Self { left, op, right }
    }
}

#[derive(Debug, PartialEq)]
pub struct LiteralExpr<'a> {
    arg: Node<'a>,
}

impl<'a> LiteralExpr<'a> {
    pub fn new(arg: Node<'a>) -> Self {
        Self { arg }
    }
}

impl<'a> From<LiteralExpr<'a>> for Expr<'a> {
    fn from(expr: LiteralExpr<'a>) -> Self {
        Expr::Literal(expr)
    }
}

#[cfg(test)]
mod test {

    use crate::token::TokenType;

    use super::*;

    #[test]
    fn test_expression() {
        let left = Expr::literal(Node::new("left", 0, TokenType::String));
        let op = Node::new("+", 4, TokenType::Plus);
        let right = Expr::literal(Node::new("right", 5, TokenType::String));

        let expr = Expr::binary(left, op, right);
    }
}
