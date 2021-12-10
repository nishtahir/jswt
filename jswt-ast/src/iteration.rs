use jswt_common::{Span, Spannable};
use jswt_derive::{FromEnumVariant, Spannable};

use crate::{SingleExpression, StatementElement};

#[derive(Debug, PartialEq, FromEnumVariant)]
pub enum IterationStatement {
    While(WhileIterationElement),
}

#[derive(Debug, PartialEq, Spannable)]
pub struct WhileIterationElement {
    pub span: Span,
    pub expression: SingleExpression,
    pub statement: Box<StatementElement>,
}

impl Spannable for IterationStatement {
    fn span(&self) -> Span {
        todo!()
    }
}
