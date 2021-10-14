use crate::token::TokenType;

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
