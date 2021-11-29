pub use jswt_common::{Span, Spannable};

#[derive(Debug, PartialEq)]
pub struct Ident {
    pub span: Span,
    pub value: &'static str,
}

impl Ident {
    pub fn new(value: &'static str, span: Span) -> Self {
        Self { span, value }
    }
}
