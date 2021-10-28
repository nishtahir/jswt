use super::{ident::Ident, span::Span, types::Type};

#[derive(Debug, PartialEq)]
pub struct FunctionStmt<'a> {
    // pub span: Span,
    pub ident: Ident<'a>,
    pub params: Vec<Param<'a>>,
    pub returns: Type<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Param<'a> {
    pub span: Span,
    pub ident: Ident<'a>,
    pub ty: Type<'a>,
}

impl<'a> Param<'a> {
    pub fn new(ident: Ident<'a>, ty: Type<'a>, start: usize, end: usize) -> Self {
        Self {
            span: Span::new(start, end),
            ident,
            ty,
        }
    }
}
