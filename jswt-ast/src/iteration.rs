use jswt_common::Span;
use jswt_derive::{FromEnumVariant, Spannable};

use crate::{SingleExpression, BlockStatement};

#[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
pub enum IterationStatement {
    While(WhileIterationElement),
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct WhileIterationElement {
    pub span: Span,
    pub expression: SingleExpression,
    pub block: BlockStatement,
}
