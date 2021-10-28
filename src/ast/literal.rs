use super::node::Node;

#[derive(Debug, PartialEq)]
pub enum LiteralExpr<'a> {
    String(StringLiteralExpr<'a>),
    Number(NumberLiteralExpr<'a>),
    Boolean(BooleanLiteralExpr<'a>),
}

impl<'a> LiteralExpr<'a> {
    pub fn string(node: Node<'a>) -> Self {
        LiteralExpr::String(StringLiteralExpr { node })
    }

    pub fn number(node: Node<'a>) -> Self {
        LiteralExpr::Number(NumberLiteralExpr { node })
    }

    pub fn boolean(node: Node<'a>) -> Self {
        LiteralExpr::Boolean(BooleanLiteralExpr { node })
    }
}
#[derive(Debug, PartialEq)]

pub struct BooleanLiteralExpr<'a> {
    pub node: Node<'a>,
}

#[derive(Debug, PartialEq)]

pub struct NumberLiteralExpr<'a> {
    pub node: Node<'a>,
}

#[derive(Debug, PartialEq)]
pub struct StringLiteralExpr<'a> {
    pub node: Node<'a>,
}
