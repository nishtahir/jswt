use std::borrow::Cow;

use jswt_common::Span;
use jswt_derive::{FromEnumVariant, Spannable};

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct TypeAnnotation {
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, PartialEq, FromEnumVariant, Clone)]
pub enum Type {
    Identifier(IdentifierType),
    Array(ArrayType),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierType {
    pub name: Cow<'static, str>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayType {
    pub ident: Box<Type>,
}
