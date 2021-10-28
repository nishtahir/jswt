use super::{ident::Ident, span::Span};

#[derive(Debug, PartialEq)]
pub struct Type<'a> {
    pub span: Span,
    pub ident: Ident<'a>,
}

impl<'a> Type<'a> {
    pub fn new(value: &'a str, start: usize, end: usize) -> Self {
        Self {
            span: Span::new(start, end),
            ident: Ident::new(value, start, end),
        }
    }

    // TODO - This is not a very good way to deal with the problem
    // given that, 1. void may not be explicitly declared
    // We're currently passing in the next/previous token's span
    pub fn void(span: Span) -> Self {
        Self {
            ident: Ident::new("void", span.start, span.end),
            span,
        }
    }
}
