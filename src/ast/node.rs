use super::span::Span;

#[derive(Debug, PartialEq)]
pub struct Node<'a> {
    pub span: Span,
    pub value: &'a str,
}

impl<'a> Node<'a> {
    pub fn new(value: &'a str, start: usize, end: usize) -> Self {
        Node {
            value,
            span: Span::new(start, end),
        }
    }
}
