use jswt_common::Span;
use jswt_derive::Spannable;
use jswt_types::Type;

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct TypeAnnotation {
    pub span: Span,
    pub ty: Type,
}
