use crate::token::{Token, TokenType};

#[derive(Debug, PartialEq)]
pub struct Node<'a> {
    pub value: &'a str,
    pub offset: usize,
    pub kind: TokenType,
}

impl<'a> Node<'a> {
    pub fn new(value: &'a str, offset: usize, kind: TokenType) -> Self {
        Node {
            value,
            offset,
            kind,
        }
    }
}

impl<'a> From<&Token<'a>> for Node<'a> {
    fn from(value: &Token<'a>) -> Self {
        Node::new(value.lexme, value.offset, value.kind)
    }
}
