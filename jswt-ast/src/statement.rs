use jswt_derive::FromEnumVariant;

use crate::{
    iteration::IterationStatement, variable::VariableModifier, AssignableElement, SingleExpression,
    Span, Spannable, StatementList, TypeAnnotation,
};

#[derive(Debug, PartialEq, FromEnumVariant, Spannable)]
pub enum StatementElement {
    Block(BlockStatement),
    Empty(EmptyStatement),
    If(IfStatement),
    Iteration(IterationStatement),
    Return(ReturnStatement),
    Variable(VariableStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq, Spannable)]
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

#[derive(Debug, PartialEq, Spannable)]
pub struct ReturnStatement {
    pub span: Span,
    pub expression: SingleExpression,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct VariableStatement {
    pub span: Span,
    pub modifier: VariableModifier,
    pub target: AssignableElement,
    pub expression: SingleExpression,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expression: SingleExpression,
}
