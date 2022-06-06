use std::borrow::Cow;

use jswt_common::{Span, Type};
use jswt_derive::Spannable;

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct TypeAnnotation {
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierType {
    pub name: Cow<'static, str>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayType {
    pub ident: Box<Type>,
}
