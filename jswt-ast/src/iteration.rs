use jswt_common::Span;
use jswt_derive::{FromEnumVariant, Spannable};

use crate::{BlockStatement, SingleExpression};

#[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
pub enum IterationStatement {
    While(WhileIterationElement),
    For(ForIterationElement),
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct WhileIterationElement {
    pub span: Span,
    pub expression: SingleExpression,
    pub block: BlockStatement,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ForIterationElement {
    pub span: Span,
    pub initializer: SingleExpression,
    pub condition: SingleExpression,
    pub update: SingleExpression,
    pub block: BlockStatement,
}
