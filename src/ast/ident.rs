use super::span::Span;

#[derive(Debug, PartialEq)]

pub struct Ident {
    pub span: Span,
    pub value: &'static str,
}

impl Ident {
    pub fn new(value: &'static str, start: usize, end: usize) -> Self {
        Self {
            span: Span::new(start, end),
            value,
        }
    }
}
