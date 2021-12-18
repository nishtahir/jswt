use jswt_common::Span;
use jswt_derive::{FromEnumVariant, Spannable};

use crate::{SingleExpression, StatementElement};

#[derive(Debug, PartialEq, FromEnumVariant, Spannable)]
pub enum IterationStatement {
    While(WhileIterationElement),
}

#[derive(Debug, PartialEq, Spannable)]
pub struct WhileIterationElement {
    pub span: Span,
    pub expression: SingleExpression,
    pub statement: Box<StatementElement>,
}
