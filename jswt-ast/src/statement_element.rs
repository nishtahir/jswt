use crate::{
    AssignableElement, SingleExpression, Span, Spannable, StatementList, VariableModifier,
};

#[derive(Debug, PartialEq)]
pub enum StatementElement {
    Block(BlockStatement),
    Empty(EmptyStatement),
    If(IfStatement),
    Return(ReturnStatement),
    Variable(VariableStatement),
    Expression(ExpressionStatement),
}

impl From<IfStatement> for StatementElement {
    fn from(v: IfStatement) -> Self {
        Self::If(v)
    }
}

impl From<ExpressionStatement> for StatementElement {
    fn from(v: ExpressionStatement) -> Self {
        Self::Expression(v)
    }
}

impl From<ReturnStatement> for StatementElement {
    fn from(v: ReturnStatement) -> Self {
        Self::Return(v)
    }
}

impl From<VariableStatement> for StatementElement {
    fn from(v: VariableStatement) -> Self {
        Self::Variable(v)
    }
}

impl From<EmptyStatement> for StatementElement {
    fn from(v: EmptyStatement) -> Self {
        Self::Empty(v)
    }
}

impl From<BlockStatement> for StatementElement {
    fn from(v: BlockStatement) -> Self {
        Self::Block(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub span: Span,
    pub statements: StatementList,
}

#[derive(Debug, PartialEq)]
pub struct EmptyStatement {
    pub span: Span,
}

#[derive(Debug, PartialEq)]
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
        }
    }
}
