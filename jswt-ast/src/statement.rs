use jswt_derive::FromEnumVariant;

use crate::{
    iteration::IterationStatement, AssignableElement, SingleExpression, Span, Spannable,
    StatementList, TypeAnnotation, VariableModifier,
};

#[derive(Debug, PartialEq, FromEnumVariant)]
pub enum StatementElement {
    Block(BlockStatement),
    Empty(EmptyStatement),
    If(IfStatement),
    Iteration(IterationStatement),
    Return(ReturnStatement),
    Variable(VariableStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub span: Span,
    pub statements: StatementList,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct EmptyStatement {
    pub span: Span,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct IfStatement {
    pub span: Span,
    pub condition: SingleExpression,
    pub consequence: Box<StatementElement>,
    pub alternative: Option<Box<StatementElement>>,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub span: Span,
    pub expression: SingleExpression,
}

#[derive(Debug, PartialEq)]
pub struct VariableStatement {
    pub span: Span,
    pub modifier: VariableModifier,
    pub target: AssignableElement,
    pub expression: SingleExpression,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expression: SingleExpression,
}

impl Spannable for StatementElement {
    fn span(&self) -> Span {
        match self {
            StatementElement::Block(stmt) => stmt.span.to_owned(),
            StatementElement::Empty(stmt) => stmt.span.to_owned(),
            StatementElement::If(stmt) => stmt.span.to_owned(),
            StatementElement::Return(stmt) => stmt.span.to_owned(),
            StatementElement::Variable(stmt) => stmt.span.to_owned(),
            StatementElement::Expression(stmt) => stmt.span.to_owned(),
            StatementElement::Iteration(stmt) => stmt.span(),
        }
    }
}