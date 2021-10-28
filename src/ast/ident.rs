use super::span::Span;

#[derive(Debug, PartialEq)]

pub struct Ident<'a> {
    pub span: Span,
    pub value: &'a str,
}

impl<'a> Ident<'a> {
    pub fn new(value: &'a str, start: usize, end: usize) -> Self {
        Self {
            span: Span::new(start, end),
            value,
        }
    }
}
